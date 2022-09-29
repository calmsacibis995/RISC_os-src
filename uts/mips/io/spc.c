/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: spc.c,v 1.23.1.9.1.2.1.2 90/11/20 15:12:02 beacker Exp $"
#include "sys/types.h"
#include "sys/buf.h"
#ifndef	DIAG
#include "sys/debug.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/am9516.h"
#include "sys/mb87030.h"
#include "sys/elog.h"
#include "sys/dvh.h"
#include "sys/m120scsi.h"
#include "sys/scsi.h"
#else	DIAG
#include "sys/cmn_err.h"
#include "sys/param.h"

#include "lit.h"
#include "regdesc.h"

#include "machine/am9516.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/mb87030.h"
#include "machine/m120scsi.h"
#include "machine/scsi.h"
#include "sysv/param.h"

#endif	DIAG

#undef DEBUGRPH
#undef SAVE_PTR		/* enable saveptr() debug prints */
#undef MULT_INITS  	/* allow multiple initiators on the scsi bus */

#define TC_ALL ((fuji->count_hi<<16)|(fuji->count_mid<<8)|fuji->count_lo)
#define NO_RST_ATN	0 /* for msg out phase don't reset the atn line */
#define	WAITLOOPS	800000
#define INACTIVE	0xff /* indicates the SCSI bus is inactive */

#define PTR_BITS_MSK    0x6	/* mask to isolate pointer bits */
#define PTR_BIT_SHT	1	/* bit shift required to justify ptr bits */

struct fuji_manager fuji_manager;
static int scsi_multi_phase = 0;
static int priority_poll = 0; /* flag to let polled commands be serviced */
static int waitid = 7;

int timeout_cnt = 0;	/* flag used to allow a panic after spctimeout code
			   fails to work (ie can't recover) */
#ifdef	DIAG
int managersize = sizeof(fuji_manager);
#endif	DIAG

/* scsi channel states
 */
#define CH_BUSY		0x00000001 /* the channel is active (not free) */
#define CH_SELECT	0x00000002 /* channel in process of selection */
#define CH_RESELECT	0x00000004 /* channel was re-selected */

/* scsi target states
 */
#define U_WAITING	0x00000001 /* we desire to issue a cmd on this unit
				    * but SCSI channel is BUSY */
#define U_SYNC		0x00000002 /* syncronous xfer supported */
#define U_PRIMED	0x00000004 /* we've issued the first command */
#define U_SYNC_DATA	0x00000010 /* primed for syncronous DATA-IN/OUT */
#define U_DISCON	0x00000020 /* target has disconnected */
#define U_POLLED	0x00000040 /* we're a polled command */
	
void 	spctimeout();

#define AMD_BASE	(*(struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400))
#define AMD_ADDR	(&AMD_BASE)
#define AMD_REG		struct am9516
#define FUJI_BASE	(*(struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400))
#define FUJI_ADDR	(&FUJI_BASE)
#define FUJI_REG	struct mb87030
#define SYSCON		(u_short *)PHYS_TO_K1(SCR)

#ifndef	DIAG
struct fuji_manager saved_spc_manager;

spcinit(flag)
int flag;
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register u_short *syscon = SYSCON;
	register struct fuji_manager* manager = &fuji_manager;
	register i, zero = 0;

#ifndef SABLE
	if (badaddr(&fuji->scsi_id, sizeof(fuji->scsi_id))) {
		cmn_err(CE_CONT,"fuji scsi chip not addressable at 0x%x\n",
							 &fuji->scsi_id);
		return(-1); /* catastrophic failure (panic) */
	}
#endif SABLE
	/* initialize syscon for fast udc reads just to make sure */
	*syscon &= ~SCR_SLOWUDCEN; wbflush(); /* enable udc 'fast' read */

	fuji->control = RESET_DISABLE; wbflush(); /* reset the SPC
							 (if not already) */
	/* initialize the FUJI mb87030 SPC (scsi protocol controller)
	 */
	fuji->scsi_id = BUSID_OUT7; wbflush(); /* SPC is highest priority */
#ifndef SABLE
	fuji->command = zero; wbflush();
#endif SABLE
	fuji->xfer_mode = zero; wbflush();
	fuji->phase_c = zero; wbflush();
	fuji->count_hi = zero; wbflush();
	fuji->count_mid = zero; wbflush();
	fuji->count_lo = zero; wbflush();
	fuji->temp = zero; wbflush();
	fuji->phse_diag.diag_c = zero; wbflush();
	fuji->control &= ~RESET_DISABLE; wbflush(); /* SPC out of reset */
	DELAY(100);	/* allow ~100us for things to settle down */
	fuji->command = RST_OUT; wbflush(); /* reset the scsi bus */
	DELAY(300);	/* allow ~300us for targets to see the reset */
	fuji->command = zero; wbflush();
	DELAY(10000);	/* allow ~10ms for targets to recover from reset */
	if (flag) {
		saved_spc_manager = *manager;
		for (i=6; i >= 0; i--) {
			manager->iopbp[i] = 0;
			manager->unit_flags[i] = 0;
		}
	}
	manager->channel_flags = 0;
	return(0);
}
#endif	DIAG
/* Fuji start routine
 */ 
spcstart(iopb, level)
register struct scsi_iopb *iopb;
register level; /* indicate whether the caller is a high-level routine */
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	u_char old_control;
	int s, t, id, j, k;

	s = splbio();
	if (manager->channel_flags & CH_BUSY) {
		/* the spc is busy, so save state */
		manager->iopbp[iopb->scsi_target] = iopb;
		manager->unit_flags[iopb->scsi_target] |= U_WAITING;
#ifndef	DIAG
		ASSERT(level == HIGHLEVEL);
#else	DIAG
		if (!(level == HIGHLEVEL)) {
			cmn_err(CE_CONT,"spc: (1) level != HIGH\n");
			GoToMonitor();
		}
#endif	DIAG
#ifndef	DIAG
		if (!(iopb->scsi_flags & POLLED)) {
			t = splclock();
			/* start timeout */
			ASSERT(iopb->scsi_timeid == 0);
			iopb->scsi_timeid =
				 timeout(spctimeout, iopb, iopb->scsi_time);
			splx(t);
		}
#endif	DIAG
		splx(s);
		return;
	/* we've got a POLLED command */
	} else if (iopb->scsi_flags & POLLED) {
polledcmd:
		id = iopb->scsi_target;
	        for (j=6; j >= 0; j--) {
			if (manager->unit_flags[j] & U_DISCON) {
				manager->iopbp[id] = iopb;
				manager->unit_flags[id] |= (U_WAITING|U_POLLED);
				priority_poll = 1; /* set priority */
				splx(s);
				return;
			}
		}
		priority_poll = 0; /* re-set priority */
		manager->unit_flags[id] &= ~(U_WAITING|U_POLLED);
		manager->active_id = id; /* our new active id */
		manager->channel_flags |= CH_BUSY;
		old_control = fuji->control;
		scsicmd(iopb); /* polled routines */
		fuji->control = old_control; wbflush();
		if (iopb->scsi_status || iopb->scsi_hwstatus)
			iopb->scsi_taskid |= ERROR;
		manager->channel_flags &= ~CH_BUSY;
		manager->active_id = INACTIVE;
		if (level == LOWLEVEL)
		    goto checkwaiting;
		splx(s);
		return;
	} else {
		/* service WAITING targets first
		 * NOTE that a high-level request won't have 
		 * waiting set!!
		 */
checkwaiting:
		k = waitid;
		waitid = 7;
		for (j=7; j > 0; j--) {
		    k--;
		    if (k < 0) k = 6;
		    if (manager->unit_flags[k] & U_WAITING) {
			if (priority_poll && 
				!(manager->unit_flags[k] & U_POLLED))
				continue; /* don't start another non-polled
					   * command! */
			/* we're no longer WAITING so clear it */
			manager->unit_flags[k] &= ~U_WAITING;
			waitid = k;
			break;
		    }
			
		}
		/* no one WAITING and a lowlevel caller so just return */
		if ((waitid == 7) && (level == LOWLEVEL)) {
			splx(s);
			return;	/* no work to do */
		} else if (waitid != 7) { /* someone was WAITING so save the
			/* HIGHLEVEL callers state and set waiting on it IF
			 * we were called from a HIGHLEVEL routine */
			if (level == HIGHLEVEL) {
			    manager->unit_flags[iopb->scsi_target] |= U_WAITING;
			    manager->iopbp[iopb->scsi_target] = iopb;
#ifndef	DIAG
			    if (!(iopb->scsi_flags & POLLED)) {
				t = splclock();
				/* start timeout */
				ASSERT(iopb->scsi_timeid == 0);
				iopb->scsi_timeid =
				      timeout(spctimeout,iopb,iopb->scsi_time);
				splx(t);
			    }
#endif	DIAG
			}
			iopb = manager->iopbp[waitid];
			if(iopb->scsi_flags & POLLED)
			    goto polledcmd;
			manager->active_id = waitid; /* our new active id */
			/* while we wait for command complete or resel int
			 * we MUST let any high level requests que up
			 */
			manager->channel_flags |= CH_BUSY;
			splx(s);
			spcselect(iopb); /* startit */
		} else { /* the call was from a HIGHLEVEL routine */
#ifndef	DIAG
			ASSERT(level == HIGHLEVEL);
#else	DIAG
			if (!(level == HIGHLEVEL)) {
				cmn_err(CE_CONT,"spc: (2) level != HIGH\n");
				GoToMonitor();
			}
#endif	DIAG
			manager->active_id = iopb->scsi_target; /* active id */
			manager->iopbp[iopb->scsi_target] = iopb;
			/* while we wait for command complete or resel int
			 * we have to let any high level requests que up
			 */
#ifndef	DIAG
			if (!(iopb->scsi_flags & POLLED)) {
			    t = splclock();
			    /* start timeout */
			    ASSERT(iopb->scsi_timeid == 0);
			    iopb->scsi_timeid =
				   timeout(spctimeout,iopb,iopb->scsi_time);
			    splx(t);
			} else
			    goto polledcmd;
#endif	DIAG
			manager->channel_flags |= CH_BUSY;
			splx(s);
			spcselect(iopb); /* startit */
		}
	}
}
/* Arbitrate for and Select a target.
 * with the spc being the sole inititiator (MULT_INIT not defined) it
 * is noted that either a target will sneak in and reselect during the
 * arbitration process yielding a reselection interrupt or the spc being
 * the highest priority scsi device will win the arbitration. In light
 * of this we can safely issue the select command and take our hands off.
 * With multiple initiators, even though spc is the highest priority,
 * it is possible another initiator will win the bus before we can get
 * the select command off. for this reason we MUST spin on scsi BUSY and
 * do a hard delay to see if we really won the arbitration.
 *
 */ 
spcselect(iopb)
register struct scsi_iopb *iopb;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	register volatile u_char *stat = &fuji->status;
	register s, t;
	int delay;

	delay = WAITLOOPS;
	while (*stat & (INIT|BUSY)) { /* SPC still connected? */
		if (iopb->scsi_flags & DISCON_RECON) {
		   if (fuji->interupts & RESELECT) {
			manager->unit_flags[iopb->scsi_target] |= U_WAITING;
			spcintr();
			return;
		   }
		}
		if (!(delay -= 1)) {
			spc_regs();
#ifndef	DIAG
			cmn_err(CE_PANIC,
				"spcselect: timed out with spc connected");
#else	DIAG
			cmn_err(CE_CONT,
				"spcselect: timed out with spc connected!\n");
			cmn_err(CE_CONT,"spc: (3) PANIC\n");
			GoToMonitor();
#endif	DIAG
		}
	}
	if (fuji->interupts & DISCONECT) /* bus free int pending? */
		fuji->interupts = DISCONECT; wbflush();
	fuji->phase_c = I_O_SEL; wbflush();
	if (iopb->scsi_flags & DISCON_RECON) {
		/* see if we were reselected */
		if (curphase() & BSY) { /* bail and try again */
			manager->unit_flags[iopb->scsi_target] |= U_WAITING;
			return;
		}
	}
	if (iopb->scsi_flags & MESSAGES) {
		/* enable reselection */
		fuji->control = ARBIT_EN|PARITY_EN|SEL_NO|RESEL_EN|INTS_EN;
	} else {
		fuji->control = ARBIT_EN|PARITY_EN|SEL_NO|RESEL_NO|INTS_EN;
	}
	wbflush();
	fuji->temp = (1 << iopb->scsi_target)|BUSID_IN7; wbflush();
#ifdef MULT_INITS
arbitrate:
#endif MULT_INITS
	fuji->count_hi  = TSEL_HI; wbflush();/* these two set SELECT time-out */
	fuji->count_mid = TSEL_LO; wbflush();/* period (~1 sec) */
	fuji->count_lo  = TWAIT; wbflush();/* "BUS FREE" delay period ~1250ns */
	manager->channel_flags |= CH_SELECT;
	if (iopb->scsi_flags & MESSAGES) {
		/* assert ATN line to request a MSGOUT phase for an ID message*/
		fuji->command = SET_ATN; wbflush();
	}
	s = splbio(); /* don't allow resel int while we issue sel */
	fuji->command = SELECT; wbflush(); /* issue the select command */
	splx(s);
#ifdef MULT_INITS
	DELAY(8);	/* hard 8us arbitration delay (should be?) */
	if (fuji->status & INIT) { /* won arbitration */
		return;
	} else {
		/* what about killing the Select command here??? 
		 * what state is spc in at this point?? */
		goto arbitrate; /* we lost the arbitration */
	}
#endif MULT_INITS
}
#ifndef	DIAG
/*
 */
void
spctimeout(iopb)
register struct scsi_iopb *iopb;
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	register u_short *syscon = SYSCON;
	int s, t, phase, i, id, orig_id;
	u_char msg;
	extern int scsiexterr;

	s = splbio();
	if (scsiexterr > 1)
		spc_regs();
	if (timeout_cnt) {
#ifndef	DIAG
		cmn_err(CE_PANIC,"spctimeout retries failed");
#endif	DIAG
	} else
		timeout_cnt++;
	*syscon &= ~SCR_SCSIHIN; wbflush(); /* set SCR_RSTSCSI to be reset */
	*syscon |= SCR_RSTSCSI; wbflush();  /* reset "scsi pipe" state machine
					    * and hold it that way */
	orig_id = iopb->scsi_target; /* save the original */
	id = manager->active_id; /* who are we working on (or INACTIVE) */
	cmn_err(CE_CONT,"\nSCSI %dL%d: COMMAND TIMEOUT! command = 0x%x\n",
		iopb->scsi_target, iopb->scsi_lun,
		iopb->cmd_blk.cdb_0.cdb_0_cmd);
	if (id == INACTIVE)
		cmn_err(CE_CONT,"the SCSI bus is inactive\n");
	else if (id != iopb->scsi_target) {
		cmn_err(CE_CONT,"the active SCSI target id is %d\n",id);
	} else cmn_err(CE_CONT,"on the active target\n");

	/* first clear ALL outstanding software timers,
	 * a target will have a timeout outstanding if
	 * it is WAITING, DISCONNECTED, or the active target. 
	 */
	for (i=6; i >= 0; i--) {
		iopb = manager->iopbp[i];
		if (i == orig_id) { /* guy that timed out doesn't need
				     * untime'ing */
			iopb->scsi_timeid = 0;
			continue;
		}
		/* remember that an 'active' target will NOT be waiting
		 * and might not be disconected */
		if ((i != id)&&(!(manager->unit_flags[i]&(U_WAITING|U_DISCON))))
			continue;
#ifndef	DIAG
		/* clear safety timeout
	 	 */
		if (!(iopb->scsi_flags & POLLED)) {
		    t = splclock();
		    ASSERT(iopb->scsi_timeid != 0);
		    untimeout(iopb->scsi_timeid);
		    iopb->scsi_timeid = 0;
		    splx(t);
		}
#endif	DIAG
	}
	cmn_err(CE_CONT,"issuing SCSI reset\n");
	spcinit(0);	/* reset and initialize spc and reset SCSI bus.
			 * UDC doesn't need a reset since an EOP or CA
			 * (end of process or chain abort) are nothing
			 * special to it. we simply reprogram it */
	manager->channel_flags |= CH_BUSY; /* since spcinit clears it */
	*syscon &= ~SCR_RSTSCSI; wbflush(); /* drop the state machine reset */
	/*
	 * now call the high-level interrupt handler as
	 * appropriate. the 'active' target is the only
	 * one that should be reported as a hardware error,
	 * the others will be silent retries
	 */
	cmn_err(CE_CONT,"reissuing commands for targets: ");
	for (i=6; i >= 0; i--) {
		if (manager->unit_flags[i] & (U_WAITING|U_DISCON)) {
			if (i == id)
				cmn_err(CE_CONT,"[%d]\n",i);
			else
				cmn_err(CE_CONT,"%d ",i);
		} else if (i == id) {
			cmn_err(CE_CONT,"[%d]\n",i);
		} else {
			manager->unit_flags[i] = 0; /* force repriming */
			continue;
		}
		manager->unit_flags[i] = 0; /* force repriming for sync/async */
		iopb = manager->iopbp[i];
		if (i == id) /* this is the bad boy */
			iopb->scsi_hwstatus = HWERROR;
		else 	iopb->scsi_hwstatus = NOERROR;
		iopb->scsi_status = NOERROR;
		iopb->scsi_taskid |= ERROR; /* force a retry for all */
		common_scsi_intr(iopb->scsi_un);
	}
	cmn_err(CE_CONT,"\n");
	manager->channel_flags &= ~CH_BUSY;
	splx(s);
	spcstart(iopb,LOWLEVEL); /* now start things off again */
}
#endif	DIAG
/*
 * Fuji SPC interrupt handler
 */ 
spcintr()
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	register struct scsi_iopb *iopb;
	int i, s, hold, id;
	int j, k;

	hold = fuji->interupts;
	id = manager->active_id; /* who are we working on */
	if (id != INACTIVE) {
	    iopb = manager->iopbp[id];
	    if (!iopb) {
		cmn_err(CE_CONT,"spurious fuji int; int= %x\n",hold);
		return;
	    }
	} else if (!(hold & (RESELECT | DISCONECT | SCSI_RESET))) {
	    cmn_err(CE_CONT,"fuji id inactive but no RESEL; int= %x\n",hold);
	    return;
	}

	/* multiple ints can be valid simultaneously
	 */
	for (i = 0; i < 8; hold >>= 1, i++) {
	  if (!hold) break;
	  if (hold & 1) {
	    switch (i) {
	    case INT_SELECT:
		cmn_err(CE_CONT,"phantom SELECT interrupt from SPC\n");
		fuji->interupts = SLECT; wbflush();
		break;
	/* a disconnected target has arbitrated for the scsi bus, won
	 * and reselected us. we'll resume that target's command
	 */
	    case INT_RESELECT:
		/*
		 * in case we were reselected while trying to select
		 */
		if (manager->channel_flags & CH_SELECT) {
			ASSERT(id != INACTIVE);
			manager->unit_flags[id] |= U_WAITING;
			manager->channel_flags &= ~CH_SELECT;
		}
		/* required for an SPC problem: ATN was set for select
		 * command when a reselect interrupt occurred.
		 * ALSO: needed for brain-damaged cdc drives that
		 * issue saveptr message after a reselect with no
		 * Data in/out phase!!
		 */
		manager->channel_flags |= CH_RESELECT;
		id = fuji->temp; /* grab the reselector's id */
		for (j = 0,k = 1; j < 7; j++, k <<= 1) 
			if (id & k) break;
		iopb = manager->iopbp[j];
		manager->active_id = j; /* new active target id */
		manager->unit_flags[j] &= ~U_DISCON; /* no longer disconnected*/
		manager->channel_flags |= CH_BUSY; /* channel is now busy */
		fuji->interupts = RESELECT; wbflush(); /* reset this int */
		phase(iopb); /* look for a new phase */
		break;
	/*
	 * the spc has detected scsi bus free phase. 
	 * BUG: the SPC issues this interrupt regardless of the 
	 * setting of the bus free interrupt enable bit in the
	 * phase control register!!! so just clear it and continue
	 */
	    case INT_DISCONECT:
		fuji->phase_c = 0; wbflush(); /* doesn't work!!! */
		fuji->interupts = DISCONECT; wbflush();
		break;
	    case INT_CMD_CMPLT:
		if (fuji->interupts & DISCONECT) { /* bus free also?? */
/*			fuji->phase_c = 0; wbflush(); */
			fuji->interupts = CMD_CMPLT|DISCONECT; wbflush();
			hold = 0;
		} else {
		    fuji->interupts = CMD_CMPLT; wbflush(); /* turn it off */
		    if (scsi_multi_phase == 1) {
			    iopb->scsi_count0 = 0;
		    } else if (scsi_multi_phase == 2) {
			    iopb->scsi_count0 = iopb->scsi_count1;
			    iopb->scsi_count1 = 0;
			    iopb->scsi_bufaddr0 = iopb->scsi_bufaddr1;
		    }
		    scsi_multi_phase = 0;
		}
		if (manager->channel_flags & CH_SELECT) /* we be selected */
			manager->channel_flags &= ~CH_SELECT;
		phase(iopb); /* look for a new phase */
		break;
	    case INT_SER_REQ: /* this code sees this int first */
		if (fuji->interupts & CMD_CMPLT) { /* cmd completed also?? */
			if (manager->channel_flags & CH_SELECT) /* selected */
				manager->channel_flags &= ~CH_SELECT;
			if (fuji->interupts & DISCONECT) { /* bus free also?? */
/*				fuji->phase_c = 0; wbflush();	*/
				fuji->interupts = CMD_CMPLT|SER_REQ|DISCONECT;
				wbflush();
			} else fuji->interupts = CMD_CMPLT|SER_REQ; wbflush();
			hold = 0;
		} else fuji->interupts = SER_REQ; wbflush(); /* turn it off */
		phase(iopb); /* look for a new phase */
		break;
	    case INT_TIME_OUT: /* selection time-out */
		fuji->interupts = TIME_OUT; wbflush(); /* turn off this int */
		manager->channel_flags &= ~CH_SELECT;
		iopb->scsi_hwstatus = SELTMO;
		iopb->scsi_taskid |= ERROR;
#ifndef	DIAG
		/* clear safety timeout and initialize */
		s = splclock();
		ASSERT(iopb->scsi_timeid != 0);
		untimeout(iopb->scsi_timeid);
	   	iopb->scsi_timeid = 0;
	   	splx(s);
#endif	DIAG
		common_scsi_intr(iopb->scsi_un);
		/* to be fair don't set ~BUSY till now */
		manager->active_id = INACTIVE;
		manager->channel_flags &= ~CH_BUSY;
		spcstart(iopb,LOWLEVEL); /* more work? */
		break;
	    case INT_HDW_ERROR: /* spc hardware error */
		cmn_err(CE_CONT,"fuji spc hardware error detected\n");
		cmn_err(CE_CONT,"SERR=%R\n",fuji->err_stat,spcerr_desc);
		fuji->interupts = HDW_ERROR; wbflush(); /* turn off this int */
		manager->channel_flags &= ~CH_SELECT;
		iopb->scsi_hwstatus = HWERROR;
		iopb->scsi_taskid |= ERROR;
		phase(iopb); /* look for a new phase */
		break;
       	    case INT_SCSI_RESET:
		cmn_err(CE_CONT,"SCSI reset detected!\n");
		fuji->interupts = SCSI_RESET; wbflush(); /* turn off this int */
		for (i = 0; i < 7; ++i)
		    manager->unit_flags[i] &= ~U_PRIMED;
		break;
	    default:
		/* never! */
		cmn_err(CE_CONT,"spcintr: default intr= 0x%x\n",hold);
		spc_regs();
		fuji->interupts = 0xff; wbflush(); /* turn off any int source */
		break;
	    }
	  }
	}
#ifdef	DIAG
	return(0);
#endif	DIAG
}
/* see if REQ is asserted; if it is see what transfer phase is requested
 * by the target and proceed with it. If REQ isn't asserted then start
 * a transfer for a bogus phase and wait for a service required int which
 * will then send us right back here again.
 */
phase(iopb)
register struct scsi_iopb *iopb;
{
    register FUJI_REG *fuji = FUJI_ADDR;
    register AMD_REG *amd = AMD_ADDR;
    register struct fuji_manager* manager = &fuji_manager;
    register u_short i, count, *syscon = SYSCON;
    register phase, tc_all;
    u_char msg, id, data_phase = DATA_OUT; /* default */
    int s;

    id = manager->active_id; /* our current active target */
newphase:
    if (((phase = curphase()) & REQ) || /* target REQ? */
	    ((manager->unit_flags[id] & U_SYNC_DATA) && 
	    (!(phase & (MSG|C_D))))) {
	switch(phase & XFER_PHSE_MSK) {
	case DATA_IN:
	    data_phase = DATA_IN;
	case DATA_OUT:
	    manager->channel_flags &= ~CH_RESELECT;
	    manager->unit_flags[id] &= ~U_SYNC_DATA;
	    if (iopb->scsi_flags & DMA_XFER) { /* use dma */
		if (iopb->scsi_count0) { /* have some ptm bytes to do */
		    scsi_multi_phase = 1;
		    ptm(iopb, data_phase);
		} else {
		    if (iopb->scsi_count1)
			scsi_multi_phase = 2;
		    dma(iopb, data_phase);
		}
	    } else if (iopb->scsi_flags & PTM_XFER) { /* use ptm */
		ptm(iopb, data_phase);
	    } else {
#ifndef	DIAG
		cmn_err(CE_PANIC,"transfer mode not set");
#else	DIAG
		cmn_err(CE_CONT,"transfer mode not set\n");
		cmn_err(CE_CONT,"spc: (5) PANIC\n");
		GoToMonitor();
#endif	DIAG
	    }
	    break;
	case COMMAND:
	    ptm(iopb,COMMAND); /* send command (ints enabled) */
	    break;
	case STATUS:
	    scsi_multi_phase = 0;
	    manager->channel_flags &= ~CH_RESELECT;
	    if (iopb->scsi_flags & DMA_XFER) { /* check dma */
		amd->ptr = COC1; wbflush();
		if (count = amd->base) {
			*syscon |= SCR_EOP9516; wbflush();
			*syscon &= ~SCR_EOP9516; wbflush();
		}
		if (count = (*syscon & (SCR_PTR1 | SCR_PTR0))>>1) {
		    /* flush "scsi pipe" state machine */
		    *syscon |= SCR_SCSIHIN; wbflush();
		    *syscon |= SCR_RSTSCSI; wbflush();
		    i = 5000;
		    while (*syscon & (SCR_PTR1 | SCR_PTR0) && i) {
			wbflush();
			i--;
		    }
		    *syscon &= ~SCR_RSTSCSI; wbflush();
		    if (!i) {
			/* reset "scsi pipe" state machine */
			*syscon &= ~SCR_SCSIHIN; wbflush();
			*syscon |= SCR_RSTSCSI; wbflush();
			*syscon &= ~SCR_RSTSCSI; wbflush();
			cmn_err(CE_CONT,
			    "SCSI unable to flush scsipipe %d bytes lost\n",
			    count);
		    }
		}
	    }
	    man_status(iopb);
	    goto newphase; /* now look for msg-in phase */
	    break;
	case MSG_OUT:
	    /* handle case of reselection with ATN set */
	    if (manager->channel_flags & CH_RESELECT) {
		msg = MSG_NOOP;
	    } else {
		if (iopb->scsi_flags & DISCON_RECON)
		    /* tell target we support discon/recon */
		    msg = IDENTIFY|DIS_REC_YES|iopb->scsi_lun;
		else 
		    msg = IDENTIFY|DIS_REC_NO|iopb->scsi_lun;
	    }
	    /* send the byte manually (no interrupts) */
	    if (manager->unit_flags[id] & U_PRIMED)
		msg_out(iopb, msg, RST_ATN);
	    else { /* see if syncronous xfer supported */
		msg_out(iopb, msg, NO);
		sync_msg(iopb);
		/* target could have gone to bus free!!
		 * in this case need to call high level
		 * program and retry command */
		if (!(curphase() & BSY)) {
		    cmn_err(CE_CONT,"bus free after sync_msg\n");
		    iopb->scsi_hwstatus = SELTMO;
		    iopb->scsi_taskid |= ERROR;
		    manager->channel_flags &= ~CH_BUSY;
#ifndef	DIAG
		    /* clear safety timeout and initialize
		     */
		    s = splclock();
		    ASSERT(iopb->scsi_timeid != 0);
		    untimeout(iopb->scsi_timeid);
		    iopb->scsi_timeid = 0;
		    splx(s);
#endif	DIAG
		    common_scsi_intr(iopb->scsi_un);
		    return;
		}
	    }
	    goto newphase; /* command phase is next */
	case MSG_IN:
	    if (manager->unit_flags[id] & U_SYNC) { /* syncronous */
		fuji->xfer_mode = iopb->scsi_syncoff | iopb->scsi_syncxfer;
	    } else
		fuji->xfer_mode = ASYNC; /* async transfer */
	    wbflush();
	    msg = msg_in(iopb);
	    /* id message is an implied restore pointers
	     * and indicates the LUN
	     */
	    if (msg & MSG_ID) { /* identify message */
		if ((msg & LUN_MASK) != iopb->scsi_lun) {
		    cmn_err(CE_CONT,"LUN mismatch: wanted %d got %d\n",
			iopb->scsi_lun,msg&LUN_MASK);	
#ifndef	DIAG
		    cmn_err(CE_PANIC," ");
#else	DIAG
		    cmn_err(CE_CONT,"spc: (6) PANIC\n");
		    GoToMonitor();
#endif	DIAG
		}
		/* syncronous data transfer possible!!! */
		if (manager->unit_flags[id] & U_SYNC)
		    manager->unit_flags[id] |= U_SYNC_DATA;
		goto newphase; /* look for next phase */
	    } else {
		switch (msg) {
		case MSG_RSTRPTR: /* restore pointers */
		    goto newphase; /* look for next phase */
		case MSG_SAVEPTR: /* save pointers */
		    /* NO saveptr() call if directly from
		     * a reselect (after id message) */
		    if (manager->channel_flags & CH_RESELECT) {
			manager->channel_flags &= ~CH_RESELECT;
		    } else if (tc_all = TC_ALL)
			saveptr(iopb, tc_all);
		    goto newphase; /* get next message byte */
		case MSG_DISCON: /* target will yield bus */
		    manager->channel_flags &= ~CH_BUSY;
		    manager->unit_flags[id] |= U_DISCON;
		    manager->active_id = INACTIVE;
		    spcstart(iopb,LOWLEVEL); /* more work? */
		    break;
		case MSG_CMDCMP: /* command complete */
#ifndef	DIAG
		    /* clear safety timeout and initialize
		     */
		    s = splclock();
		    ASSERT(iopb->scsi_timeid != 0);
		    untimeout(iopb->scsi_timeid);
		    iopb->scsi_timeid = 0;
		    splx(s);
#endif	DIAG
		    if (iopb->scsi_status || iopb->scsi_hwstatus)
			iopb->scsi_taskid |= ERROR;
		    common_scsi_intr(iopb->scsi_un);
		    /* to be fair don't set ~BUSY till now */
		    manager->active_id = INACTIVE;
		    manager->channel_flags &= ~CH_BUSY;
		    manager->unit_flags[id] &= ~U_SYNC_DATA;
		    timeout_cnt = 0;
		    spcstart(iopb,LOWLEVEL); /* more work? */
		    break;
		case MSG_REJECT: /* rejected message */
		    goto newphase;
		default:
		    cmn_err(CE_CONT,"phase(): msg-in = %d\n",msg);
#ifndef	DIAG
		    cmn_err(CE_PANIC," ");
#else	DIAG
		    cmn_err(CE_CONT,"spc: (8) PANIC\n");
		    GoToMonitor();
#endif	DIAG
		}
	    }
	    break;
	default:
	    break;
	}
    } else {
	/* if REQ isn't asserted we DON'T want to wait around 
	 * so we'll issue a  bogus ptm xfer and wait for a
	 * service required int
	 */
/*	fuji->phase_c = BFREE_INT_EN|P_BOGUS; wbflush();	*/
	fuji->phase_c = P_BOGUS; wbflush(); /* set a bogus phase */
	fuji->command = TRANSFER|PRG_XFER; wbflush();
    }
}

/*      Transfer data from SCSI to memory, or memory to SCSI
 *	using "program transfer mode"
 *
 *	Interrupts are enabled
 */
ptm(iopb, dataphase)
register struct scsi_iopb *iopb;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register AMD_REG *amd = AMD_ADDR;
	register u_char *ptr; /* memory address pointer */
	register struct fuji_manager* manager = &fuji_manager;
	register volatile u_char *s = &fuji->status; /* fuji status pointer */
	register volatile u_char *d = &fuji->data;
	register unsigned len;
	int delay, phase;
	u_short addr, id;

	id = manager->active_id; /* our current active target */
	if (dataphase == COMMAND) {
	    phase = P_CMD;
	    ptr = (u_char *)&iopb->cmd_blk; /* pointer to first cmd byte */
	    len = common_scsi_cmdlen(*ptr);
	} else {
	    len = iopb->scsi_count0;
	    ptr = (u_char*)iopb->scsi_bufaddr0;    /* memory address pointer */
	    if (dataphase == DATA_IN)
		    phase = P_DATAIN;
	    else
		    phase = P_DATAOUT;
	}
	/* preliminary to issuing the transfer command
	 */
	fuji->count_hi = HB(len); wbflush();
	fuji->count_mid = MB(len); wbflush();
	fuji->count_lo = LB(len); wbflush();
	/* enable bus free interrupts for potential
	 * catastropic target disconnects
	 */
/*	fuji->phase_c = BFREE_INT_EN|phase; wbflush(); /* set transfer phase */
	fuji->phase_c = phase; wbflush(); /* set transfer phase */
	if (dataphase == COMMAND) {
	    if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
		    /* set up phase() for a potential syncronous data phase */
		    manager->unit_flags[id] |= U_SYNC_DATA;
		    fuji->xfer_mode = iopb->scsi_syncoff | iopb->scsi_syncxfer;
	    } else
		    fuji->xfer_mode = ASYNC; /* asyncronous transfer */
	    wbflush();
	}
	fuji->command = TRANSFER|PRG_XFER; wbflush(); /* issue PTM command */
	delay = WAITLOOPS;
	while ((*s & (INIT|BUSY|XFER)) != (INIT|BUSY|XFER)) { /* SPC started? */
		if (!(delay -= 1)) {
			spc_regs();
#ifndef	DIAG
			cmn_err(CE_PANIC,
				"ptm: timed out waiting to start xfer");
#else	DIAG
			cmn_err(CE_CONT,
				"ptm: timed out waiting to start xfer\n");
			cmn_err(CE_CONT,"spc: (9) PANIC\n");
			GoToMonitor();
#endif	DIAG
			iopb->scsi_hwstatus = SCSITMO;
			return(1);
		}
	}
	if (dataphase == DATA_IN) {
	    do {
		delay = WAITLOOPS;
		while (*s & DEMPTY) { /* spin while FIFO is empty */
		    if (fuji->interupts) { /* int pending? */
			spcintr();
			return;
		    }
		    if (!(delay -= 1)) {
			cmn_err(CE_CONT,
			    "ptm(): timed out spinning on fifo empty\n");

			cmn_err(CE_CONT,"len = %d ptr= %x\n",len,ptr);
			spc_regs(); /* DEBUG for now */

			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		    }
		}
		*ptr++ = *d;	/* move a byte from SPC to memory */ 
	    } while (len -= 1);
	} else { /* data-out or command phase */
	    do {
		delay = WAITLOOPS;
		while (*s & DFULL) { /* spin while FIFO is full */
		    if (dataphase != COMMAND) {
			if (fuji->interupts) { /* int pending? */
			    spcintr();
			    return;
			}
		    }
		    if (!(delay -= 1)) {
			cmn_err(CE_CONT,
			    "ptm(): timed out spinning on fifo full\n");
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		    }
		}
		*d = *ptr++; wbflush();	/* move a byte from memory to SPC */ 
	    } while (len -= 1);
	}
	return(0);
}
/*      Transfer data from SCSI to memory, or memory to SCSI
 *	using "hardware transfer mode", using the AMD 9516.
 *	Interrupts are enabled.
 */
dma(iopb, dataphase)
register struct scsi_iopb *iopb;
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register AMD_REG *amd = AMD_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	register u_short *syscon = SYSCON;
	register u_char *s = &fuji->status; /* fuji status pointer */
	int delay, phase = P_DATAOUT; /* default */
	u_short addr, id;

	id = manager->active_id; /* our current active target */
	if (dataphase == DATA_IN)
		phase = P_DATAIN;
	/* preliminary to issuing the transfer command
	 */
	fuji->count_hi = HB(iopb->scsi_count); wbflush();
	fuji->count_mid = MB(iopb->scsi_count); wbflush();
	fuji->count_lo = LB(iopb->scsi_count); wbflush();
	/* enable bus free interrupts for potential
	 * catastropic target disconnects
	 */
/*	fuji->phase_c = BFREE_INT_EN|phase; wbflush(); /* set transfer phase */
	fuji->phase_c = phase; wbflush(); /* set transfer phase */
	if (phase == P_DATAIN) {
		/* set spc direction bit for reading */
		*syscon |= SCR_SCSIHIN; wbflush();
	} else {
		/* set spc direction bit for writing */
		*syscon &= ~SCR_SCSIHIN; wbflush();
	}
	/* now set-up the UDC */
	amd->ptr = CAR1_HI; wbflush(); /* select ch 1 chain address reg HI */
	addr = (u_short)((K1_TO_PHYS(iopb->scsi_extra) >> 8) & 0xff00);
	addr |= ONEWAIT;
	amd->base = addr; wbflush();
	amd->ptr = CAR1_LO; wbflush(); /* select ch 1 chain address reg LO */
	amd->base = (K1_TO_PHYS(iopb->scsi_extra) & 0xffff); wbflush();

	/* start the spc first for better performance (theoretically)
	 * BUT, in order for syncronous writes to work I can't do it!!!
	 */
	amd->ptr = COMMAND1; wbflush(); /* select ch 1 command reg */
	amd->base = START_CHAIN1; wbflush();
	fuji->command = TRANSFER|DMA_XFER_MODE; wbflush(); /* dma xfer cmd */
	/*
	 * hands off and wait for completion interrupt or service required
	 * interrupt in case of disconnection
	 */
}
/* 
 * message out phase (use manual transfer)
 * An interrupt should NOT occur.
 * NOTE: REQ is active and the phase noted before this call from phase()
 */
msg_out(iopb, msg, reset_atn)
register struct scsi_iopb *iopb;
u_char msg, reset_atn;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register volatile u_char *c,*d;	/* fuji cmd, data pointers */
	u_int delay;

	d = &fuji->temp;	/* data is xfer'ed via the spc temp reg */
	c = &fuji->command;	/* point to fuji command register */

	fuji->phase_c = P_MSGOUT; wbflush(); /* message in transfer phase */
	*d = msg; wbflush(); /* write temp */
	/*
	 * from the ansi scsi spec: "Normally, the initiator negates ATN
	 * while REQ is true and ACK is false during the last REQ/ACK
	 * handshake of the MESSAGE OUT phase"
	 */
	if (reset_atn)
		*c = RST_ATN; wbflush(); /* reset the attention line */
	*c = SET_ACK;  wbflush(); /* assert ack */
	delay = WAITLOOPS;
	while (curphase() & REQ) { /* wait REQ de-assert */
		if (!(delay -= 1)) {
			cmn_err(CE_CONT,
				"msg_out: timed out waiting for ~REQ\n");
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
	}
	*c = RST_ACK;  wbflush(); /* de-assert ack */
	return(0);
}
/* 
 * message in phase
 * NOTE: REQ is active and the phase noted before this call as driven by the
 * target
 */
msg_in(iopb)
register struct scsi_iopb *iopb;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register volatile u_char *c,*d;	/* fuji cmd, data pointers */
	u_char msg;
	u_int delay;

	d = &fuji->temp;	/* data is xfer'ed via the spc temp reg */
	c = &fuji->command;	/* point to fuji command register */

	fuji->phase_c = P_MSGIN; wbflush(); /* message in transfer phase */
	msg = *d; /* read temp */
	*c = SET_ACK;  wbflush(); /* assert ack */
	delay = WAITLOOPS;
	while (curphase() & REQ) { /* wait REQ de-assert */
		if (!(delay -= 1)) {
			spc_regs();
			cmn_err(CE_CONT,"msg_in: timed out waiting for ~REQ\n");
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
	}
	*c = RST_ACK;  wbflush(); /* de-assert ack */
	return(msg);
}
/* 
 * handle status phase using Req/Ack manual tranfer
 * NOTE: REQ is active and the phase noted before this call as driven by
 * the target
 */
man_status(iopb)
register struct scsi_iopb *iopb;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register struct fuji_manager* manager = &fuji_manager;
	register volatile u_char *c,*d;	/* fuji cmd, data pointers */
	u_char status;
	int delay, id;

	id = manager->active_id; /* our current active target */
	d = &fuji->temp;	/* data is xfer'ed via the spc temp reg */
	c = &fuji->command;	/* point to fuji command register */

	fuji->phase_c = P_STATUS; wbflush(); /* status transfer phase */
	iopb->scsi_status = *d;		/* read temp and move to iopb */
	*c = SET_ACK;  wbflush();	/* assert ack to say we read it */
	delay = WAITLOOPS;
	while (curphase() & REQ) { /* wait REQ de-assert*/
		if (!(delay -= 1)) {
			spc_regs();
			cmn_err(CE_CONT,
				"man_status: timed out waiting for ~REQ\n");
			iopb->scsi_hwstatus = PHASE_ERROR;
			return(1);
		}
	}
	*c = RST_ACK;  wbflush();	/* de-assert ACK */
	return(0);
}
dumpsge(ptr)
register struct scsisge *ptr;
{
	cmn_err(CE_CONT,"ptr= %x reload= %x mem_ptr[0]= %x [1]= %x\n",
		ptr,ptr->reload_word,ptr->mem_ptr[0],ptr->mem_ptr[1]);
	cmn_err(CE_CONT,
		"count= %x chanmode[0]= %x [1]= %x next[0]= %x [1]= %x\n",
		ptr->count,ptr->chanmode[0],ptr->chanmode[1],
		ptr->next_blk[0],ptr->next_blk[1]);
}
/*
 * routine to collapse and update dma chain entries for subsequent
 * resumption of dma data transfer upon reselection. We're keyed off
 * of the spc transfer count which reflects the exact number of bytes
 * NOT transferred which is compared with the count originally set.
 */
saveptr(iopb, tc_all)
register struct scsi_iopb *iopb;
register unsigned int tc_all;
{
	register FUJI_REG *fuji = FUJI_ADDR;
	register AMD_REG *amd = AMD_ADDR;
	register u_short i, pipe_count, *syscon = SYSCON;
	u_int udcaddr, hold_addr;
	u_int count = 0, udc_count;
	u_int bytes_xfered;
	struct scsisge *ptr, *nxtptr;
	u_short car;

	/* calculate the actual bytes read/written across the scsi bus.
	 * if nothing was transferred just return
	 */

	if (!(bytes_xfered = iopb->scsi_count - tc_all)) {
		cmn_err(CE_CONT,"saveptr(): null transfer\n");
		return;
	}
	/* correct our total byte count (for spc programming) */
	if (iopb->scsi_count0) {
	    iopb->scsi_count0 -= bytes_xfered; /* update official byte count */
	    iopb->scsi_bufaddr0 += bytes_xfered; /* update address */
	    scsi_multi_phase = 0;
	    return;
	}
	if (iopb->scsi_syncxfer & SYNC_XFER) {
#ifndef	DIAG
	    ASSERT(bytes_xfered%sizeof(long) == 0);
#else	DIAG
	    if (bytes_xfered%sizeof(long)) {
		cmn_err(CE_CONT,"spc: (11) xfer cnt not modulo %d\n",
			sizeof(long));
		GoToMonitor();
	    }
#endif	DIAG
	}
	if ((count = bytes_xfered & 3)) {
		/* must set up count0 and bufaddr0 */
		iopb->scsi_bufaddr += bytes_xfered;
		iopb->scsi_bufaddr0 = iopb->scsi_bufaddr;
		iopb->scsi_count0 = count;
		bytes_xfered += count;
	}
	/* update official byte count */
	iopb->scsi_count -= bytes_xfered;
	*syscon |= SCR_EOP9516; wbflush();
	*syscon &= ~SCR_EOP9516; wbflush();
	if (pipe_count = (*syscon & (SCR_PTR1 | SCR_PTR0))>>1) {
	    if ((iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE) == C0_READ) {
		    /* flush "scsi pipe" state machine */
		    *syscon |= SCR_SCSIHIN; wbflush();
		    *syscon |= SCR_RSTSCSI; wbflush();
		    i = 5000;
		    while (*syscon & (SCR_PTR1 | SCR_PTR0) && i) {
			wbflush();
			i--;
		    }
		    *syscon &= ~SCR_RSTSCSI; wbflush();
		    if (!i) {
			/* reset "scsi pipe" state machine */
			*syscon &= ~SCR_SCSIHIN; wbflush();
			*syscon |= SCR_RSTSCSI; wbflush();
			*syscon &= ~SCR_RSTSCSI; wbflush();
			cmn_err(CE_CONT,
				"SCSI unable to flush scsipipe %d bytes lost\n",
				pipe_count);
		    }
	    } else { /* we were writing */
			/* reset "scsi pipe" state machine */
			*syscon &= ~SCR_SCSIHIN; wbflush();
			*syscon |= SCR_RSTSCSI; wbflush();
			*syscon &= ~SCR_RSTSCSI; wbflush();
	    }
	}
	fuji->control |= CNTRL_RESET; wbflush(); /* reset transfer cmd */
	/* point to first scatter/gather element(sge)*/
	nxtptr = ptr = (struct scsisge *)iopb->scsi_extra;
	count = ptr->count * sizeof(short); /* byte count for this sge */
	while(bytes_xfered >= count) {
		bytes_xfered -= count; /* reduce the running count */
		/* see if our next sge has something to auto chain */
		nxtptr++; /* point to the next sge */
		if (ptr->next_blk[0] || ptr->next_blk[1]) {
			count = nxtptr->count * sizeof(short);
		} else {
#ifndef	DIAG
			cmn_err(CE_PANIC,"saveptr: null advance chain pointer");
#else	DIAG
			cmn_err(CE_CONT,
				"saveptr: null advance chain pointer\n");
			cmn_err(CE_CONT,"spc: (11) PANIC\n");
			GoToMonitor();
#endif	DIAG
		}
		if (nxtptr->reload_word == LD_CHAN_MODE) { /* no mas */
#ifndef	DIAG
			cmn_err(CE_PANIC,"saveptr: hit terminal chain entry");
#else	DIAG
			cmn_err(CE_CONT,"saveptr: hit terminal chain entry\n");
			cmn_err(CE_CONT,"spc: (13) PANIC\n");
			GoToMonitor();
#endif	DIAG
		}
		ptr = nxtptr; /* advance pointer to next chain entry */
	}
	iopb->scsi_extra = (u_int)ptr; /* reset chain ptr (could be the same) */
	ptr->count -= bytes_xfered/sizeof(short); /* adjust 'short' count */
	/*
	 * we need to recover the original address
	 * from the sge and then correct it based on bytes_xfered value.
	 */
	udcaddr = ((ptr->mem_ptr[0] << 8) & 0xff0000);
	udcaddr |= ptr->mem_ptr[1];
	if (ptr->mem_ptr[0] & UDC_A24_P)		
		udcaddr |= ADDR_24;		
	if (!(ptr->mem_ptr[0] & UDC_A25_P))		
		udcaddr |= ADDR_25;		
	udcaddr += bytes_xfered; /* adjust addr */
	/* manipulate our address into something usable by UDC */
	hold_addr = udcaddr; /* save the original */
	udcaddr = UDC_ADDR(udcaddr);
	udcaddr |= INC_ADDR; /* no soft wait states */
	if (hold_addr & ADDR_24) udcaddr |= UDC_A24;
	if (!(hold_addr & ADDR_25)) udcaddr |= UDC_A25;
	ptr->mem_ptr[0] = HS(udcaddr);
	ptr->mem_ptr[1] = LS(udcaddr);
	fuji->control &= ~CNTRL_RESET; wbflush(); /* drop the xfer reset */
}
/* routine to handle message handshake with a target to determine if 
 * syncronous transfers are supported
 * we're already in message out phase when we get here with ATN set
 * use manual transfer so we don't have to deal with interrupts
 * NOTE: unexpected target behavior can occur here!!
 */
sync_msg(iopb)
register struct scsi_iopb *iopb;
{
	register struct fuji_manager* manager = &fuji_manager;
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register struct scsi_unit *un = iopb->scsi_un;
	u_char msg;
	int sync = 0, delay, i, id;
	u_char message[5];
	static u_char deflt_message[5] =
		 {MSG_EXT,EXT_MSGLEN,MSG_SYNC_CODE,SYNC_XFER_RATE,SYNC_OFFSET};
#ifdef notdef
		 hack to force all drives to run asyncronous
		 {MSG_EXT,EXT_MSGLEN,MSG_SYNC_CODE,0,0};
#endif notdef
	if ((curphase() & XFER_PHSE_MSK) != (MSG|C_D)) {
		goto not_msg_out;
	}
	id = manager->active_id; /* our current active target */
	for (i=0; i < 5; i++) {
		delay = WAITLOOPS;
		/* wait for REQ since the msg_out routine assumes as much */
		while (!(curphase() & REQ)) {
			if (!(delay -= 1)) {
			      cmn_err(CE_CONT,
				  "timed out waiting for REQ in sync_msg\n");
			      cmn_err(CE_CONT,"after sending %d bytes\n",i);
			      goto done;
			}
		}
		/* need to handle the case of target changing to msg-in for
		 * message reject at any time, ie req asserted after phase
		 * change */
		if ((curphase() & XFER_PHSE_MSK) ==(MSG|C_D|I_O)) {
			/* reset the attention line */
			fuji->command = RST_ATN; wbflush();
			if ((msg = msg_in(iopb)) == MSG_REJECT) {
#ifdef DEBUGRPH
				cmn_err(CE_CONT,"sync_msg: got MSG_REJECT\n");
#endif DEBUGRPH
			} else
				cmn_err(CE_CONT,
					"sync_msg: got msg-in= %d\n",msg);
			goto done;
		} else if ((curphase() & XFER_PHSE_MSK) != (MSG|C_D)) {
not_msg_out:
			/* reset attention line */
			fuji->command = RST_ATN; wbflush();
			goto done;
		}
		if (i == 4)
			/* reset ATN now */
			msg_out(iopb, deflt_message[i],RST_ATN);
		else
			msg_out(iopb, deflt_message[i],NO_RST_ATN);
	}
	/* wait for target to assert MSG IN phase */
	delay = WAITLOOPS;
	while ((curphase() & XFER_PHSE_MSK) != (MSG|C_D|I_O)) {
		if (!(delay -= 1)) {
			cmn_err(CE_CONT,
				"timed out waiting for msg-in in sync_msg\n");
			goto done;
		}
	}
	for (i=0; i < 5; i++) {
		delay = WAITLOOPS;
		/* wait for REQ since _msg_out assumes as much */
		while (!(curphase() & REQ)) {
			if (!(delay -= 1)) {
			   cmn_err(CE_CONT,
				"timed out waiting for byte %d sync_msg\n",i);
			   break;
			}
		}
		message[i] = msg_in(iopb); 
		if (message[i] == MSG_REJECT) {
#ifdef DEBUGRPH
			cmn_err(CE_CONT,"got reject message\n");
#endif DEBUGRPH
			break;
		}
	}
	if (i == 5) { /* target sent entire message so he MIGHT be syncronous */
		if (message[3] != 0xff && message[3]) { /* valid? */
			if (message[3] < 0x40) i = 0x0; /* 4.00 Mb/s max */
			else if (message[3] < 0x66) i = 0x4; /* 2.67 Mb/s max */
			else if (message[3] < 0x80) i = 0x8; /* 2.00 Mb/s max */
			else if (message[3] < 0xa0) i = 0xa; /* 1.60 Mb/s max */
			iopb->scsi_syncxfer = i | SYNC_XFER;
			if (message[4] == 8) iopb->scsi_syncoff = 0;
			else iopb->scsi_syncoff = message[4] << 4;
			manager->unit_flags[id] |= U_SYNC; /* set state */
#ifdef DEBUGRPH
			cmn_err(CE_CONT,
			    "sync_msg: target says xfer= 0x%x offset= %d\n",
			    message[3],message[4]);
			cmn_err(CE_CONT,
			    "syncxfer= %x offset= %x\n",
			    iopb->scsi_syncxfer & ~SYNC_XFER,
			    iopb->scsi_syncoff); 
#endif DEBUGRPH
			un->un_dmaaddmask = 0;
			un->un_dmacntmask = 0;
		} else {
			un->un_dmastartmask = 0; /* request splitable */
		}
	}
done:
	manager->unit_flags[id] |= U_PRIMED; /* set state */
}
