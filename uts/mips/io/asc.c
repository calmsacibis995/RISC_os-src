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
#ident	"$Header: asc.c,v 1.1.1.8.1.5.1.7 90/12/20 19:22:07 beacker Exp $"

#include "sys/types.h"
#include "sys/bsd_glue.h"
#include "sys/immu.h"
#include "sys/sysmacros.h"
#include "sys/buf.h"
#ifndef	DIAG
#include "sys/debug.h"
#include "sys/param.h"
#include "sys/errno.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"	    /* dma defines */
#include "sys/ncr53c94.h"   /* ASC (Advanced SCSI Controller) defines */
#include "sys/r3030scsi.h"  /* some dma/scsi defines and interface defines
			     * to common_scsi (includes rb3125 stuff) */
#include "sys/elog.h"
#include "sys/dvh.h"
#include "sys/scsi.h"	    /* common scsi defines */
#include "sys/vmereg.h"
#else	DIAG
#include "sys/cmn_err.h"
#include "sys/param.h"

#include "lit.h"
#include "regdesc.h"

#include "machine/rambo.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "sys/rambo.h"
#include "machine/ncr53c94.h"
#include "machine/r3030scsi.h" /* some dma/scsi defines and interface defines
			         * to common_scsi (includes rb3125 stuff) */
#include "machine/scsi.h"
#include "sysv/param.h"

#endif	DIAG


#define TC_ALL 		((ncr->count_hi<<8)|ncr->count_lo)
#define INACTIVE	0xff /* indicates the SCSI bus is inactive */

extern int showconfig;
extern int scsi_id;
extern int scsi_reset;
struct ncr_manager ncr_manager;

/* the purpose of this ifdef is to isolate the code for target mode */

#define TARGET_MODE

#ifdef TARGET_MODE
struct ncr_target ncr_target;
#endif

static int scsi_multi_phase = 0;
static int priority_poll = 0; /* flag to let polled commands be serviced */
static int waitid = 8;
u_char message[5]; 	/* hold our sync message-in bytes */
int sync_msg_cnt = 0;	/* count msg-in bytes so we know when to stop */
int timeout_cnt  = 0;	/* flag used to allow a panic after asctimeout code
			   fails to work (ie can't recover) */
int ncr_rev = 0;	/* says what rev ncr chip we're using */
int dma_pending = 0;	/* count times we need to wait for normal dma */
int ptm_dma_pending = 0;/* count times we need to wait for ptm dma */
#ifdef	DIAG
int managersize = sizeof(ncr_manager);
#endif	DIAG

/* scsi channel states
 */
#define CH_BUSY		0x00000001 /* the channel is active (not free) */
#define CH_SELECT	0x00000002 /* channel in process of selection */
#define CH_SELECT_STOP	0x00000004 /* channel in process of selection-stop */
#define CH_RESELECT	0x00000008 /* channel was re-selected */

/* scsi target states
 */
#define U_WAITING	0x00000001 /* we desire to issue a cmd on this unit
				    * but SCSI channel is BUSY */
#define U_SYNC		0x00000002 /* syncronous xfer supported */
#define U_PRIMED	0x00000004 /* we've issued the first command */
#define U_STATUS_SEQ	0x00000008 /* we've initiated the cmd cmplt sequence */
#define U_PTM_ACTIVE	0x00000010 /* ncr PTM transfer is active (mult ints) */
#define U_DISCON	0x00000020 /* target has disconnected  */
#define U_PTM_READ	0x00000040 /* rambo PTM read initiated */
#define U_SYNC_DATA	0x00000080 /* primed for syncronous DATA-IN/OUT */
#define U_CMDCMP	0x00000100 /* notice when we're completed a command */
#define U_MSG_IN	0x00000200 /* we're grabbing a msg-in byte */
#define U_BUS_FREE_OK   0x00000400 /* it's ok for a discon interrupt to occur*/
#define U_RESEL_ID 	0x00000800 /* our resel id byte is in the fifo */
#define U_MSG_OUT 	0x00001000 /* handle ncr bug whereby xtra TRANSFER 
				    * command is needed to get last msg-out
				    * byte transferred */
#define U_CMD_PHSE 	0x00002000 /* we're in command phase */
#define U_DATA_PHSE 	0x00004000 /* we're in data phase */
#define U_TRANSFER_PAD 	0x00008000 /* we have to transfer pad this xfer */
#define U_POLLED 	0x00010000 /* we're a polled cmd (used for scheduling)*/
#define U_PTM_COPY	0x00020000 /* for genesis, to indicate copy after ptm */
#define U_PTM_WRITE	0x00040000 /* rambo PTM write initiated */
	
void 	asctimeout();

extern int SCSI_REG_BASE[];
extern int RAMBO_REG_BASE[];

#ifndef	DIAG
struct ncr_manager saved_ncr_manager;

ascinit(flag)
int flag;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register i;
	static ids[8] = {BUSID_OUT0,
			BUSID_OUT1,
			BUSID_OUT2,
			BUSID_OUT3,
			BUSID_OUT4,
			BUSID_OUT5,
			BUSID_OUT6,
			BUSID_OUT7};
	int myid;

	set_cpu_con(CON_SCSIRESET,CON_SCSIRESET);
#ifndef SABLE
	if (badaddr(&ncr->count_lo, sizeof(ncr->count_lo))) {
		cmn_err(CE_CONT,"NCR 53c94 scsi chip not addressable at 0x%x\n",
							 &ncr->count_lo);
		return(-1); /* catastrophic failure (panic) */
	}
	if (IS_R3030) {
	   if (badaddr(&rambo->dma_laddr_1, sizeof(rambo->dma_laddr_1))) {
		cmn_err(CE_CONT,"RAMBO chip not addressable at 0x%x\n",
						 &rambo->dma_laddr_1);
		return(-1); /* catastrophic failure (panic) */
	   }
	} else { /* Genesis */
	   if (badaddr(&dma->cmd_addr, sizeof(dma->cmd_addr))) {
		cmn_err(CE_CONT,"RAMBO chip not addressable at 0x%x\n",
						 &dma->cmd_addr);
		return(-1); /* catastrophic failure (panic) */
	   }
	}
#endif SABLE
	ncr->command = NOP; wbflush(); /* needed after hard or soft reset! */
	DELAY(100);	/* allow ~100us for things to settle down */
	ncr->command = RESET_NCR; wbflush(); /* soft reset the ASC */
	DELAY(100);	/* allow ~100us for things to settle down */
	ncr->command = NOP; wbflush(); /* needed after hard or soft reset! */

	/* initialize the NCR ncr53c94 ASC (advanced scsi controller)
	 */
	ncr->clk_conv = FACTOR_25MHZ; wbflush(); /* 25Mhz clock */
	ncr->f_o.sync_offset = ASYNC; wbflush(); /* run async as default */
	/* disable scsi reset int */
	ncr->config1 = NO_SCSI_RST_INT|PARITY_ENABLE; wbflush();
	if(scsi_reset) {
		ncr->command = RESET_SCSI; wbflush(); /* reset the scsi bus */
		DELAY(300);	/* allow ~300us for targets to see the reset */
		ncr->command = NOP; wbflush(); /* required after reset */
		DELAY(10000);	/* allow ~10ms for targets to recover from reset */
	}
 	/* enable parity checking, set ASC ID to highest priority (7)
	 * this is used for arbitration */
	if(scsi_id >= 0 && scsi_id <= 7)
		myid = ids[scsi_id];
	else {
		cmn_err(CE_WARN,"Scsi id reset to 7\n");
		scsi_id = 7;
		myid = ids[7];
	}
	ncr->config1 = PARITY_ENABLE|myid; wbflush();
/*	ncr->config2 = ????; /* only 4 bits valid on old rev ncr
			      * these 4 bits are not used currently.
			      * more bits valid on new rev part but not used */
/*	ncr->config3 = ????; /* valid on new rev ncr part, but not on old */
	if (!ncr->config3) { /* test to determine chip revision (for sync) */
		ncr_rev = 1; /* we've got a '486' rev ncr chip (or later) */
	} else {
		ncr_rev = 0; /* we've got a '469' rev ncr chip (or earlier) */
cmn_err(CE_CONT,"old rev ncr scsi chip is installed, running asyncronous\n");
	}
	ncr->i_t.timeout = MS250; wbflush(); /* set 250ms Sel timeout value */

        /* initialize RAMBO channel 1 (scsi channel)
	 */
	if (IS_R3030) {
		rambo->dma_block_1 = 0; wbflush();
		rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
		rambo->dma_mode_1  = 0; wbflush(); /* disable channel */
	} else { /* Genesis */
		/* reset the dma engine */
		dma->count_control = ~NO_DMA_RESET; wbflush();
	}
	if (flag) {
		saved_ncr_manager = *manager;
		for (i=7; i >= 0; i--) {
			manager->iopbp[i] = 0;
			manager->unit_flags[i] = 0;
		}
	}
	manager->channel_flags = 0;
	return(0);
}
#endif	DIAG
/* NCR start routine
 */ 

ascstart(iopb,level)

#ifdef TARGET_MODE
struct scsi_iopb *iopb;
int level; /* indicate whether the caller is a high-level routine */
{
	ascstart1(iopb,level,0);
}

ascstart1(iopb, level, flag)
#endif
register struct scsi_iopb *iopb;
register level; /* indicate whether the caller is a high-level routine */
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register struct ncr_manager* manager = &ncr_manager;
	int s, t, id, j, k;

	s = splbio();

#ifdef TARGET_MODE
if(flag) goto checkwaiting;
#endif

	if (manager->channel_flags & CH_BUSY) {
		/* the asc is busy, so save state */
		manager->iopbp[iopb->scsi_target] = iopb;
		manager->unit_flags[iopb->scsi_target] |= U_WAITING;
#ifndef	DIAG
		ASSERT(level == HIGHLEVEL);
		if (!(iopb->scsi_flags & POLLED)) {
			t = splclock();
			/* start timeout */
			ASSERT(iopb->scsi_timeid == 0);
			iopb->scsi_timeid =
				 timeout(asctimeout, iopb, iopb->scsi_time);
			splx(t);
		}
#endif	DIAG
		splx(s);
		return;
	/* we've got a POLLED command */
	} else if (iopb->scsi_flags & POLLED) {
polledcmd:
		/* while targets are disconnected we can't issue our polled
		 * command since we can't guarantee that the ncr won't get 
		 * reselected! We'll have to wait till there are no drives
		 * disconnected before we issue it */
		id = iopb->scsi_target;
	        for (j=7; j >= 0; j--) {
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
		scsicmdp(iopb); /* polled routines */
		if (iopb->scsi_status || iopb->scsi_hwstatus)
			iopb->scsi_taskid |= ERROR;
		manager->channel_flags &= ~CH_BUSY;
		manager->active_id = INACTIVE;
		/* clean up required! */
		ncr->command = FLUSH_NCR_FIFO; wbflush();
		ncr->count_lo = 0; wbflush();
		ncr->count_hi = 0; wbflush();
		ncr->command = NOP|DMA; wbflush(); /* force the load! */
		if (IS_R3030) {
			rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
			rambo->dma_block_1 = 0; wbflush();
		} else { /* Genesis */
			/* reset the dma engine */
			dma->count_control = ~NO_DMA_RESET;
	 	        wbflush();
		}
		ncr->command = EN_RESEL; wbflush(); /* superstition */
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
		waitid = 8;
		for (j=8; j > 0; j--) {
		    k--;
		    if (k < 0) k = 7;
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
		if ((waitid == 8) && (level == LOWLEVEL)) {
			splx(s);
			return;	/* no work to do */
		} else if (waitid != 8) { /* someone was WAITING so save the
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
				      timeout(asctimeout,iopb,iopb->scsi_time);
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
			ascselect(iopb); /* startit */
		} else { /* the call was from a HIGHLEVEL routine */
#ifndef	DIAG
			ASSERT(level == HIGHLEVEL);
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
				   timeout(asctimeout,iopb,iopb->scsi_time);
			    splx(t);
			} else
			    goto polledcmd;
#endif	DIAG
			manager->channel_flags |= CH_BUSY;
			splx(s);
			ascselect(iopb); /* startit */
		}
	}
}
/* Arbitrate for and Select a target, then send the command, all
 * in one fell swoop.
 */ 
ascselect(iopb)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register volatile u_char *fifo = &ncr->fifo;
	register u_char *ptr; /* memory address pointer */
	register len;
	register offset = 0;
	int id = iopb->scsi_target;

	iopb->scsi_count_all = iopb->scsi_count; /* for debugging use */
	/* dest_id used for arbitration along with 'my id' in config1 */
        ncr->s_d.dest_id = iopb->scsi_target; wbflush(); /* encoded low 3 bits*/

	if (!ncr_rev) /* just run async if we're using the old chip */
		manager->unit_flags[id] |= U_PRIMED;
	/* if we haven't done our extended sync message handshake
	 * here's how we initiate it. note that the command isn't
	 * loaded or sent! */
	if (!(manager->unit_flags[id] & U_PRIMED)) {
		if (iopb->scsi_flags & MESSAGES) { /* no need otherwise! */
			manager->channel_flags |= (CH_SELECT|CH_SELECT_STOP);
			*fifo = IDENTIFY|DIS_REC_YES|iopb->scsi_lun; wbflush();
			ncr->command = SELECT_ATN_STOP; wbflush();
			return;
		}
	} else {
	    	if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
		    ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
		    ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
	    	} else {
		    ncr->f_o.sync_offset = ASYNC; wbflush();
		}
	}
	ptr = (u_char *)&iopb->cmd_blk; /* pointer to first cmd byte */
	len = common_scsi_cmdlen(*ptr);
	if (iopb->scsi_flags & MESSAGES) { /* load ID message byte */
		if (iopb->scsi_flags & DISCON_RECON) {
			*fifo = IDENTIFY|DIS_REC_YES|iopb->scsi_lun; wbflush();
		} else 
			*fifo = IDENTIFY|DIS_REC_NO|iopb->scsi_lun; wbflush();
	}
	do {
		*fifo = *ptr++; wbflush();
	} while (len -= 1);
	manager->channel_flags |= CH_SELECT; /* in case of reselect */
	if (iopb->scsi_flags & MESSAGES) {
		ncr->command = SELECT_ATN; wbflush();
	} else {
		ncr->command = SELECT_NO_ATN; wbflush();
	}
}
#ifndef	DIAG
/*
 */
void
asctimeout(iopb)
register struct scsi_iopb *iopb;
{
	register struct scsisge   *sgeptr;
	register struct scsisge_g *sgeptr_g;
    	register struct scsi_unit *un;
	register struct scsi_iopb *iopb_active;
	register struct ncr_manager* manager = &ncr_manager;
	int s, t, phase, i, id, orig_id;
	u_char temp;

	s = splbio();
	if (timeout_cnt) {
#ifndef	DIAG
		cmn_err(CE_PANIC,"asctimeout retries failed");
#endif	DIAG
	} else
		timeout_cnt++;
    	un = iopb->scsi_un;
	orig_id = iopb->scsi_target; /* save the original */
	id = manager->active_id; /* who are we working on (or INACTIVE) */
	cmn_err(CE_CONT,"\nSCSI %dL%d: COMMAND TIMEOUT!\n",
		iopb->scsi_target, iopb->scsi_lun);
	if (id == INACTIVE) {
		cmn_err(CE_CONT,"the SCSI bus is inactive\n");
		iopb_active = (struct scsi_iopb*)0;
	} else if (id != iopb->scsi_target) {
		cmn_err(CE_CONT,"the active SCSI target id is %d\n",id);
		iopb_active = manager->iopbp[id];
	} else {
		cmn_err(CE_CONT,"on the active target\n");
		iopb_active = (struct scsi_iopb*)0;
	}
	if (IS_R3030) {
		sgeptr = (struct scsisge *)un->un_extra;
		for (i=0;i < MAX_SGENTRY;i++,sgeptr++) {
			dumpsge(sgeptr);
			if (!sgeptr->count) break;
		}
	} else {
		sgeptr_g = (struct scsisge_g *)un->un_extra;
		for (i=0;i < MAX_SGENTRY;i++,sgeptr_g++) {
			dumpsge_g(sgeptr_g);
			if (!sgeptr_g->control_count) break;
		}
	}
#ifdef MIPS_LOCAL
	if (iopb_active) {
cmn_err(CE_CONT,"active->scsi_extra= %x\n",iopb_active->scsi_extra);
cmn_err(CE_CONT,"active->scsi_count= %x count_all= %x\n",
			iopb_active->scsi_count,iopb_active->scsi_count_all);
cmn_err(CE_CONT,"active lbn %x %x %x\n",
		    iopb_active->cmd_blk.cdb_0.cdb_0_lba_h,
		    iopb_active->cmd_blk.cdb_0.cdb_0_lba_m,
		    iopb_active->cmd_blk.cdb_0.cdb_0_lba_l);
	}
cmn_err(CE_CONT,"un= %x un->un_flags= %x\n",un,un->un_flags);
cmn_err(CE_CONT,"iopb->scsi_extra= %x\n",iopb->scsi_extra);
cmn_err(CE_CONT,"iopb->scsi_count= %x count_all= %x\n",
			iopb->scsi_count,iopb->scsi_count_all);
cmn_err(CE_CONT,"lbn %x %x %x\n",
		    iopb->cmd_blk.cdb_0.cdb_0_lba_h,
		    iopb->cmd_blk.cdb_0.cdb_0_lba_m,
		    iopb->cmd_blk.cdb_0.cdb_0_lba_l);
	asc_regs(YES);
	if (IS_R3030)
		rambo_regs();
	else
		dma_regs();
#endif MIPS_LOCAL
	/* first clear ALL outstanding software timers,
	 * a target will have a timeout outstanding if
	 * it is WAITING, DISCONNECTED, or the active target. 
	 */
	for (i=7; i >= 0; i--) {
		if(i == scsi_id) continue;
		iopb = manager->iopbp[i];
		if (i == orig_id) { /* guy that timed out doesn't need
				     * untime'ing */
			iopb->scsi_timeid = 0;
			continue;
		}
		/* remember that an 'active' target will NOT be waiting
		 * and might not be disconected */
		temp = U_WAITING|U_DISCON;
		if ((i != id) && (!(manager->unit_flags[i] & temp)))
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
	if (IS_R3030)
		cmn_err(CE_CONT,
		"resetting ncr and rambo, then issuing SCSI reset\n");
	else /* Genesis */
		cmn_err(CE_CONT,
	  	"resetting ncr and dma engine, then issuing SCSI reset\n");
	ascinit(0);	/* reset and initialize asc and reset SCSI bus.
			 * clear rambo */
	manager->channel_flags |= CH_BUSY; /* since ascinit clears it */
	/*
	 * now call the high-level interrupt handler as
	 * appropriate. the 'active' target is the only
	 * one that should be reported as a hardware error,
	 * the others will be silent retries
	 */
	cmn_err(CE_CONT,"reissuing commands for targets: ");
	for (i=7; i >= 0; i--) {
		if(i == scsi_id) continue;
		temp = U_WAITING|U_DISCON;
		if (manager->unit_flags[i] & temp) {
			if (i == id)
				cmn_err(CE_CONT,"%d (was active) ",i);
			else
				cmn_err(CE_CONT,"%d ",i);
		} else if (i == id) {
			cmn_err(CE_CONT,"%d (was active) ",i);
		} else {
			manager->unit_flags[i] = 0; /* force repriming */
			continue;
		}
		manager->unit_flags[i] = 0;/* force repriming for sync/async*/
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
	ascstart(iopb,LOWLEVEL); /* now start things off again */
}
#endif	DIAG
/*
 * NCR asc interrupt handler
 */ 
ascintr()
{
    register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
    register volatile DMA_REG *dma = DMA_ADDR;
    register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
    register struct ncr_manager* manager = &ncr_manager;
    register struct scsi_iopb *iopb;
    register struct scsi_unit *un;
    register hold, seqn, status;
    int holdit, id, msg;
    int i, j, k, s, len;
    u_short fill_data = 0, phaser;
    u_char cmd;
    u_short tc_all;

#ifdef TARGET_MODE
	if(ncr_target.nt_state != NT_BUSFREE) {
		ncr_target_intr();
		return;
	}
#endif

    status = ncr->s_d.status;   /* read status register */
    seqn = ncr->s_p.seqn_step;  /* read sequence step register */
    seqn &= SEQN_STEP_MSK;      /* isolate relevent bits */

    /* read interrupt status register and thereby CLEAR ALL THREE! */
    while (hold = ncr->i_t.interrupts) {
	if (hold & SCSI_RESET) {
		cmn_err(CE_CONT,"SCSI reset detected!\n");
		for (i = 0; i < 8; ++i)
		    manager->unit_flags[i] &= ~U_PRIMED;
		hold &= ~SCSI_RESET;
		if (!hold) return;
	}
#ifdef TARGET_MODE
	if(hold & (SEL_W_ATN|SLECT)) {
		ncr_target_busfree(status,hold);
		return;
	}
#endif
	id = manager->active_id;    /* who are we working on */
	if (id != INACTIVE) {
		iopb = manager->iopbp[id];
		if (!iopb) { /* spurious interrupt! */
		   cmn_err(CE_CONT,"spurious ncr int; int= %x status= %x\n",
				hold, status);
		   return;
		}
		un = iopb->scsi_un;
		if (IS_R3030) {
		    if (rambo->dma_mode_1 & DMA_INTR) { /* rambo int pending? */
			rambointr();
		    }
		}
	} else if (!(hold & RESELECT)) {	
		cmn_err(CE_CONT,"stray ncr interrupt; int reg= %x status= %x\n",
				hold, status);
		if (IS_R3030) {
		    if (rambo->dma_mode_1 & DMA_INTR) { /* rambo int pending? */
			rambointr();
		    }
		}
		return; /* wait for timeout to occur so we can retry */
	}
	if (status & ASC_PAR_ERR) { /* force a retry on a parity error */
		/* The asc will assert ATN on a parity error detection. A NOP
		 * message will be sent to the target and things will continue
		 * normally BUT this statement will force a retry. If all
		 * retries fail then a hard error will occur (all this handled
		 * by the high level code).
		 */
		iopb->scsi_hwstatus = PARITY;
	}
	/* multiple interrupt reg bits can (and will) be valid simultaneously
	 */
        holdit = hold;
	for (i = 0; i < 8; hold >>= 1, i++) {
	  if (!hold) break;
	  if (hold & 1) {
	    switch (i) {
	    case INT_SELECT:
		cmn_err(CE_CONT,"phantom SELECT interrupt from ASC\n");
		break;
	    case INT_SELECT_ATN:
		cmn_err(CE_CONT,"phantom SELECT w ATN interrupt from ASC\n");
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
			manager->channel_flags &= ~(CH_SELECT|CH_SELECT_STOP);
		}
		manager->channel_flags |= CH_BUSY; /* channel is now busy */
		/* next statement required for case whereby ATN was set
		 * for select command when a reselect interrupt occurred.
		 * ALSO: needed for brain-damaged cdc drives that
		 * issue saveptr message after a reselect with no
		 * Data in/out phase!!
		 */
		manager->channel_flags |= CH_RESELECT;
		id = ncr->fifo; /* grab the reselector's id */
		for (j = 0,k = 1; j < 8; j++, k <<= 1) 
			if ((id & k) && (j != scsi_id)) break;
		iopb = manager->iopbp[j];
		manager->active_id = j; /* new active target id */
		manager->unit_flags[j] &= ~U_DISCON; /* no longer disconnected*/
		manager->unit_flags[j] |=  U_RESEL_ID; /* special case */
		hold = 0; /* CMD_CMPLT could also be set, BS could be? */
		phase(iopb,MSG_IN); /* handle msg-in byte (ID) */
		break;
	    case INT_CMD_CMPLT: /* code sees this one before SER_REQ */
		if (manager->channel_flags & CH_SELECT) { /* we be selected */
		   manager->channel_flags &= ~CH_SELECT;
		   if (!ncr_rev) { /* if we're using the old chip */
		      phaser = status & XFER_PHSE_MSK;/* determine phase*/
		      if (phaser != DATA_IN) { /* flush pad byte, ncr bug */
		   	ncr->command = FLUSH_NCR_FIFO; wbflush();
		      /* if the phase is data-in and we're syncronous no need */
		      } else if (!(manager->unit_flags[id] & U_SYNC)) {
		   	ncr->command = FLUSH_NCR_FIFO; wbflush();
		      }
		   }
/* we need to decide how to deal with this error case!
 * I have seen it and the drive responds with a check condition and we
 * retry. This may not happen though. In that case a timeout could handle
 * it. So, maybe we do nothing as now.
 */
		   j = SER_REQ|CMD_CMPLT; /* compiler problem? */
		   if (manager->channel_flags & CH_SELECT_STOP) {
		   	manager->channel_flags &= ~CH_SELECT_STOP;
		   	if ((holdit != j) || (seqn != SEQN_SEL_ATN_STOP )) {
			   cmn_err(CE_CONT,"\nSCSI %dL%d: ",
				iopb->scsi_target, iopb->scsi_lun);
			   cmn_err(CE_CONT,"on SELECT_stop int = %x\n", holdit);
			   cmn_err(CE_CONT,"and SEQstep register = %x s/b %x\n",
						seqn,SEQN_SEL_ATN_STOP);
			}
		   } else { /* normal select cmd */
		   	if ((holdit != j) || (seqn != SEQN_SEL_ATN)) {
			   cmn_err(CE_CONT,"\nSCSI %dL%d: ",
				iopb->scsi_target, iopb->scsi_lun);
			   cmn_err(CE_CONT,"on SELECT int reg = %x\n", holdit);
			   cmn_err(CE_CONT,"and SEQstep register = %x s/b %x\n",
						seqn,SEQN_SEL_ATN);
		   	}
		   }
		}
		hold = 0;    /* clear SER_REQ */
		phase(iopb,0); /* look for a new phase */
		break;
	    case INT_SER_REQ:
		/* we end up here after a transfer command completes
		 * and a REQ is seen or the phase changes and a REQ
		 * is seen. Note that for successive msg-in bytes
		 * we get this int (for the new REQ) along with a
		 * separate int for fuction complete after the transfer.
		 */ 
		if (manager->channel_flags & CH_SELECT) { /* selected */
		   manager->channel_flags &= ~(CH_SELECT|CH_SELECT_STOP);
cmn_err(CE_CONT,"ser req int on SELECTion\n");
		} else if (manager->unit_flags[id] & U_TRANSFER_PAD) {
			manager->unit_flags[id] &= ~U_TRANSFER_PAD;
			cmn_err(CE_CONT,"\nSCSI %dL%d: after transfer pad:\n",
					iopb->scsi_target, iopb->scsi_lun);
			asc_regs(NO);
			tc_all  = TC_ALL;
cmn_err(CE_CONT,"transfer padded 0x%x bytes\n",0x10000-tc_all);
cmn_err(CE_CONT,"iopb->count = %x count_all= %x\n",
				iopb->scsi_count,iopb->scsi_count_all);
			ncr->command = FLUSH_NCR_FIFO; wbflush();
			ncr->count_lo = 0; wbflush();
			ncr->count_hi = 0; wbflush();
			ncr->command = NOP|DMA; wbflush(); /* force the load! */
		} else { /* handle multi-phase transfers (not scsi phases!)
		    * We end-up here for transfer complete, which is defined
		    * to be TC==0, fifo empty, and REQ asserted (could be the
		    * same phase!!).
		    * We also end-up here for disconnects. If rambo is not
		    * empty we're screwed! This case needs to be handled!!
		    *
		    * check rambo for un-sent data if we're doing a PTM
		    * read. if so we need to fill the rambo buffer to 32 half
		    * words to force the 64 byte block transfer then copy
		    * back out to the proper spot in memory.
		    */
		   if (IS_R3030) {
		     if (manager->unit_flags[id] & U_PTM_READ) {
		        if (!(rambo->dma_mode_1 & FIFO_EMPTY)) { /* empty? */
			    len = rambo->dma_mode_1 & COUNT_MSK;
			    len = BLOCK_HW_CNT - len; /* half words to write */
			    do {
			 	rambo->dma_fifo_1 = fill_data; wbflush();
			    } while (len -= 1);
			    /* now put the data where it really belongs */
		    	    bcopy(K2_TO_K1(un->un_buf_64),
			     	iopb->scsi_bufaddr0,iopb->scsi_count0);
			    /* needed cause sometimes the target doesn't yield
			     * all the bytes that were ask for! (tape for 
			     * example). this could lead to bad things happening
			     * in the near future, like a tape command that
			     * issues a save-ptr message before the discon msg*/
			    ncr->count_lo = 0; wbflush();
			    ncr->count_hi = 0; wbflush();
			    ncr->command = NOP|DMA; wbflush(); /* force load! */
	    		    ncr->command = FLUSH_NCR_FIFO; wbflush();
			    /* sanity check to see if we really did empty it! */
		            if (!(rambo->dma_mode_1 & FIFO_EMPTY)) {
				cmn_err(CE_CONT,"\nSCSI %dL%d: on ptm_read:\n",
					iopb->scsi_target, iopb->scsi_lun);
				rambo_regs();	
				cmn_err(CE_PANIC,
					"rambo not empty after flush\n");
			    }
			} else {
			    /* remember that the ncr will pad a byte if 
			     * necessary to do the last 16-bit dma xfer!,
			     * ie for the case of 63 bytes rambo will be
			     * empty. */
			    /* now put the data where it really belongs */
		    	    bcopy(K2_TO_K1(un->un_buf_64),
			     	iopb->scsi_bufaddr0,iopb->scsi_count0);
			}
		     }
		   } else { /* Genesis */
		     if (manager->unit_flags[id] & U_PTM_READ) {
			/* if the dma engine is still running then FLUSH
			 * out the remaining half-word */
			if (!(dma->count_control & NO_RUN_ENABLE)) {
				dma->control =
				(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA);
				wbflush();
				DELAY(1);
				dma->control =
	(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA|NO_FLUSH_PIPE|NO_RUN_ENABLE);
				wbflush();
			}
		        if (manager->unit_flags[id] & U_PTM_COPY) {
				/* now put the data where it really belongs */
		    		bcopy(K2_TO_K1(un->un_buf_64),
			     		iopb->scsi_bufaddr0,iopb->scsi_count0);
		   		manager->unit_flags[id] &= ~U_PTM_COPY;
			}
			/* see comments for RAMBO code above */
			ncr->count_lo = 0; wbflush();
			ncr->count_hi = 0; wbflush();
			ncr->command = NOP|DMA; wbflush(); /* force load! */
	    		ncr->command = FLUSH_NCR_FIFO; wbflush();
		     /* this handles the ptm write case on command bytes
	              * normal multi-phase ptm writes are covered below */
		     } else if (manager->unit_flags[id] & U_PTM_WRITE) {
		   	manager->unit_flags[id] &= ~U_PTM_WRITE;
			dma->count_control = ~NO_DMA_RESET;
			wbflush();
		     }
		   }
		   /* handle case of multiple xfer's per scsi rd/wr command */
		   if (status & TC0) { /* only if transfer completed */
		       if (scsi_multi_phase == 1) {
			   iopb->scsi_count0 = 0;
			   /* handle write case - there could still be data */
			   if (IS_R3030) {
			       rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
			       rambo->dma_block_1 = 0; wbflush();
			   } else { /* Genesis */
			       /* reset the dma engine if we were writing */
		     	       if (!(manager->unit_flags[id] & U_PTM_READ)) {
			       	    dma->count_control = ~NO_DMA_RESET;
				    wbflush();
			       }
			   }
		       } else if (scsi_multi_phase == 2) {
			   iopb->scsi_count0 = iopb->scsi_count1;
			   iopb->scsi_count1 = 0;
			   iopb->scsi_bufaddr0 = iopb->scsi_bufaddr1;
		       }
		       scsi_multi_phase = 0;
		   }
		   manager->unit_flags[id] &= ~U_PTM_READ;
		}
		if (hold > 1) {
			cmn_err(CE_CONT,"on SR int; int reg= %x\n",holdit);
			hold = 0;    /* clear SER_REQ */
		}
		if (sync_msg_cnt) { /* we're handling multiple msg-in bytes!! */
			break;	/* our sync message scheme requires this since
				 * we get 2 interrupts per byte! */
		}
		phase(iopb,0); /* look for a new phase */
		break;
	    case INT_DISCONECT: /* selection time-out or target disconnects
				 * (scsi bus phase is bus free) */
		if (manager->unit_flags[id] & U_CMDCMP) { /* we're done! */
			timeout_cnt = 0;
			manager->unit_flags[id] &= ~U_CMDCMP;
			manager->active_id = INACTIVE;
			ncr->command = EN_RESEL; wbflush(); /* enable resel */
		} else if (manager->channel_flags & CH_SELECT) {
			manager->channel_flags &= ~(CH_SELECT|CH_SELECT_STOP);
			/* flush the ncr fifo (or else!) */
	    		ncr->command = FLUSH_NCR_FIFO; wbflush();
			iopb->scsi_hwstatus = SELTMO; /* selection timeout! */
			ncr->command = EN_RESEL; wbflush(); /* enable resel */
		/* the target frees the scsi bus after the disconnect message
		 * and we'll get this interrupt */
		} else if (manager->unit_flags[id] & U_BUS_FREE_OK) {
			manager->unit_flags[id] &= ~U_BUS_FREE_OK;
			manager->active_id = INACTIVE;
			ncr->command = EN_RESEL; wbflush(); /* enable resel */
			/* to be fair don't set ~BUSY till now */
			manager->channel_flags &= ~CH_BUSY;
			ascstart(iopb,LOWLEVEL); /* more work? */
			return; /* nothing else to do! */
		} else { /* catastrophic failure! */
			cmn_err(CE_CONT,"target dropped BUSY!\n");
			iopb->scsi_hwstatus = HWERROR;
			cmn_err(CE_CONT,"holdit %x seqn %x status %x\n",
						holdit,seqn,status);
			asc_regs(NO);
			if (IS_R3030)
				rambo_regs();
			else
				dma_regs();
		}
#ifndef	DIAG
		/* clear safety timeout and initialize */
		if (!(iopb->scsi_flags & POLLED)) {
			s = splclock();
			ASSERT(iopb->scsi_timeid != 0);
			untimeout(iopb->scsi_timeid);
			iopb->scsi_timeid = 0;
			splx(s);
		}
#endif	DIAG
		if (iopb->scsi_status || iopb->scsi_hwstatus)
			iopb->scsi_taskid |= ERROR;
		common_scsi_intr(iopb->scsi_un);
		manager->active_id = INACTIVE;
		/* to be fair don't set ~BUSY till now */
		manager->channel_flags &= ~(CH_BUSY|CH_RESELECT);
		ascstart(iopb,LOWLEVEL); /* more work? */
		break;
	    case INT_ILL_CMD: /* should never happen */
		cmn_err(CE_CONT,"ASC illegal command!\n");
		cmn_err(CE_CONT,"holdit %x seqn %x status %x\n",
					holdit,seqn,status);
		asc_regs(NO);
		if (IS_R3030)
			rambo_regs();
		else
			dma_regs();
		/* let command timeout handle error recovery */
		break;
#ifdef notdef
	/* handled above now */
       	    case INT_SCSI_RESET:
		cmn_err(CE_CONT,"SCSI reset detected!\n");
		for (i = 0; i < 8; ++i)
		    manager->unit_flags[i] &= ~U_PRIMED;
		break;
#endif notdef
	    }
	  }
	}
	status = ncr->s_d.status;   /* read status register */
	if (!(status & INTR))
		break;
	seqn = ncr->s_p.seqn_step;  /* read sequence step register */
        seqn &= SEQN_STEP_MSK;      /* isolate relevent bits */
    }
#ifdef	DIAG
	return(0);
#endif	DIAG
}
/*
 * RAMBO interrupt handler
 * by definition we can't get here unless the block count is 0.
 * since it's 0 we don't have to mess with the channel enable bit,
 * just make sure that the block count is the last thing written
 * since doing this will resume the transfer!
 * NOTE that the direction bit will never need to change here
 * since we're obviouly going to continue in the same direction.
 */ 
rambointr()
{
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register struct scsi_iopb *iopb;
	register struct scsisge *sgeptr, *sgeptr_nxt, *sgeptr1;
	register unsigned count;
	int id, s;
	u_char cmd, flag = 0;

	s = splall(); /* block all to get this over without interruption */

	id = manager->active_id; /* who are we working on */
	iopb = manager->iopbp[id];
	cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE;
	if (cmd == C0_WRITE) { /* we have to wait for fifo_empty!!!!!! */
		while (!(rambo->dma_mode_1 & FIFO_EMPTY)) /* fifo empty? */
			DELAY(1);
	}
	/* point to current scatter/gather element (sge)
	 * the one we just completed */
	sgeptr = sgeptr_nxt = (struct scsisge *)iopb->scsi_extra;
	count  = sgeptr->count << BLOCK_SHIFT; /* block count for this sge */
	ASSERT(count != 0);
	iopb->scsi_count -= count; /* update official byte count */
	ASSERT(iopb->scsi_count != 0); /* we shouldn't be here otherwise! */
	sgeptr_nxt++; /* point to the next sge */
	rambo->dma_laddr_1 = sgeptr_nxt->mem_ptr; wbflush(); /* new address */
	iopb->scsi_extra = (u_int)sgeptr_nxt; /* update global chain ptr */
	sgeptr1 = sgeptr_nxt++;	  /* point to the next sge */
	if (!sgeptr_nxt->count) { /* no INT unless we have to! */
		rambo->dma_mode_1  &= ~INTR_EN; wbflush();
	}
	ASSERT(sgeptr1->count != 0);
	rambo->dma_block_1 = sgeptr1->count; wbflush();/* MUST BE LAST WRITE! */
	splx(s);
}
/*
 * Genesis dma error interrupt handler
 * todo: any reasonable way to keep from PANIC'ing?
 */ 
dma_error()
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register struct scsi_iopb *iopb;
    	register struct scsi_unit *un;
	register struct scsisge_g *sgeptr_g;
	register unsigned short previpl;
	int i, id;

	id = manager->active_id; /* who are we working on (or INACTIVE) */
	if (id == INACTIVE) {
		cmn_err(CE_CONT,"no active id\n");
		goto bypass;
	}
	cmn_err(CE_CONT,"active id is %x\n",id);
	iopb = manager->iopbp[id];
	if (iopb == 0) {
		cmn_err(CE_CONT,"manager struct was cleared!\n");
		goto bypass;
	}
	un = iopb->scsi_un;
	sgeptr_g = (struct scsisge_g *)un->un_extra;
	for (i=0;i < MAX_SGENTRY;i++,sgeptr_g++) {
		dumpsge_g(sgeptr_g);
		if (!sgeptr_g->control_count) break;
	}
	cmn_err(CE_CONT,"un= %x un->un_flags= %x\n",un,un->un_flags);
	cmn_err(CE_CONT,"iopb->scsi_extra= %x\n",iopb->scsi_extra);
	cmn_err(CE_CONT,"iopb->scsi_count= %x count_all= %x\n",
				iopb->scsi_count,iopb->scsi_count_all);
	cmn_err(CE_CONT,"lbn %x %x %x\n",
		    iopb->cmd_blk.cdb_0.cdb_0_lba_h,
		    iopb->cmd_blk.cdb_0.cdb_0_lba_m,
		    iopb->cmd_blk.cdb_0.cdb_0_lba_l);
bypass:
	if (!(dma->count_control & NO_DMA_ERROR)) { /* dma error? */
		asc_regs(YES);
		dma_regs();
		/* clear the dma error state */
		dma->control =
		(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_FLUSH_PIPE|NO_RUN_ENABLE);
		wbflush();
		DELAY(1);
		dma->control =
		(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA|NO_FLUSH_PIPE|NO_RUN_ENABLE);
		wbflush();
		cmn_err(CE_PANIC,"unrecoverable dma error");
	} else { /* looks like an IMR screw-up saying we got a zero-half int */
		previpl = *(unsigned short *)PHYS_TO_K1(VME_IMR_RB3125);
		cmn_err(CE_CONT,"undesired dma int occured due to IMR = %x\n",
					previpl);
		dma_regs();
		cmn_err(CE_PANIC,"unrecoverable dma error");
	}
}
/* the ASC will only interrupt if REQ is true.
 * see what transfer phase is requested
 * by the target and proceed with it.
 */
phase(iopb,flag)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register volatile DMA_REG *dma_engine = DMA_ADDR;
	register struct ncr_manager* manager = &ncr_manager;
	register phase;
	static u_short tc_all, tc_fifo, data_phase;
	u_char msg, id, cmd;
	u_short fill_data = 0, rambo_hold;
	int i, s, len;

	id = manager->active_id; /* our current active target */
	/* determine transfer phase */
	if (flag)
	    phase = flag;    /* for illegal phase change in reselect SQ 555 */
	else
	    phase = ncr->s_d.status & XFER_PHSE_MSK;
	switch(phase) {
	case DATA_IN:
	    data_phase = DATA_IN;
	case DATA_OUT:
	    if (phase == DATA_OUT)
	    	data_phase = DATA_OUT; /* need to save this value for later! */
	    manager->unit_flags[id] &= ~U_CMD_PHSE;
	    if (iopb->scsi_flags & DMA_XFER) { /* use dma */
		if (iopb->scsi_count0) { /* we have some ptm bytes to do first*/
		    scsi_multi_phase = 1;
		    if (IS_R3030)
			ptm_rambo(iopb, data_phase); /* use rambo for ptm */
		    else
			ptm_genesis(iopb, data_phase);
		} else if (iopb->scsi_count1) { /* we have ptm bytes after dma*/
		    scsi_multi_phase = 2;
		    dma(iopb);
		} else {
		    if (manager->unit_flags[id] & U_DATA_PHSE) {
			cmn_err(CE_CONT,
				"target still in data phase, transfer pad..\n");
			manager->unit_flags[id] |= U_TRANSFER_PAD;/*set state*/
			asc_regs(YES);
			if (IS_R3030)
				rambo_regs();
			else
				dma_regs();
			/* this set-up 'should be' correct for both DATA-IN and
			 * DATA-OUT. NOTE that NO dma actually takes place!  */
			ncr->count_lo = 0; wbflush();
			ncr->count_hi = 0; wbflush(); /* set max value (64k) */
			iopb->scsi_hwstatus = HWERROR;
			ncr->command = TRANSFER_PAD|DMA; wbflush(); /* use dma*/
			return;
		    }
		    dma(iopb);
		}
	    } else if (iopb->scsi_flags & PTM_XFER) /* use ptm */
		if (IS_R3030)
			ptm_rambo(iopb, data_phase); /* use rambo for ptm */
		else
			ptm_genesis(iopb, data_phase);
	    break;
	case COMMAND: /* used after "SEL W ATN & STOP" when not PRIMED  */
	    /* this state variable is needed for "old rev" ncr scsi chips */
	    if (manager->unit_flags[id] & U_CMD_PHSE) {
		manager->unit_flags[id] &= ~U_CMD_PHSE;
		ncr->command = TRANSFER; wbflush(); /* handle ncr bug */
	    } else {
		ncr->command = 0x1B; wbflush();	/* reset attention */
		manager->unit_flags[id] |= U_PRIMED;
		if (IS_R3030)
			ptm_rambo(iopb,COMMAND); /* send command bytes */
		else
			ptm_genesis(iopb,COMMAND); /* send command bytes */
	    }
	    break;
	case STATUS:
	    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE;
	    if (IS_R3030) {
	       /* deal with a partially full rambo for variable record cases */
	       rambo_hold = rambo->dma_mode_1;
	       if (!(rambo_hold & FIFO_EMPTY)) { /* empty? */
		     len = rambo_hold & COUNT_MSK;
		     len = BLOCK_HW_CNT - len; /* half words to write */
		     do {
			rambo->dma_fifo_1 = fill_data; wbflush();
		     } while (len -= 1);
		     rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
	       }
	       /* unfortunately I have to do this cause the variable record
	        * stuff could leave this set which could lead to bad things */
	       rambo->dma_block_1 = 0; wbflush();
	    } else { /* Genesis */
	       if (cmd == C0_WRITE) { /* only reset if we were writing */
		     /* reset the dma engine */
		     dma_engine->count_control = ~NO_DMA_RESET;
		     wbflush();
	       } else { /* we were reading */
		     /* if the dma engine is still running then FLUSH
		      * out the remaining half-word! */
		     if (!(dma_engine->count_control & NO_RUN_ENABLE)) {
			   dma_engine->control =
				(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA);
			   wbflush();
			   DELAY(1);
			   dma_engine->control =
	  (NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA|NO_FLUSH_PIPE|NO_RUN_ENABLE);
			   wbflush();
		     }
	       }
	    }
	    /* must clear count for those devices that finish command phase and
	     * do an immediate save pointer without setting this up. Causes a
	     * panic in saveptr if old leftover count is bigger than total
	     * request */
	    ncr->count_lo = 0; wbflush();
	    ncr->count_hi = 0; wbflush();
	    ncr->command = NOP|DMA; wbflush(); /* force load! */
	    manager->unit_flags[id] &= ~U_DATA_PHSE;
	    manager->unit_flags[id] |= U_STATUS_SEQ;
	    ncr->command = FLUSH_NCR_FIFO; wbflush(); /* we MUST flush here! */
	    ncr->command = CMD_CMP; wbflush(); /* command complete sequence */
	    scsi_multi_phase = 0;
	    break;
	case MSG_OUT:
	    /* handle case of reselection with ATN set */
	    if (manager->channel_flags & CH_RESELECT) {
		ncr->fifo = MSG_NOOP; wbflush();
		cmn_err(CE_CONT,
		    "msg-out xfer needed on reselect; NO-OP message used!\n");
	        manager->channel_flags &= ~CH_RESELECT;
		ncr->command = TRANSFER; wbflush();
	    } else if (!(manager->unit_flags[id] & U_PRIMED)) {
		if (manager->unit_flags[id] & U_MSG_OUT) {
			manager->unit_flags[id] &= ~U_MSG_OUT;
			if (!ncr_rev) { /* if we're using the old chip */
	    		   ncr->command = TRANSFER; wbflush(); /* ncr bug! */
			} else
			   cmn_err(CE_CONT,"ncr msg-out bug with new chip?\n");
		} else
			sync_msg(iopb,MSG_OUT); /* syncronous msg handshake */
	    } else {
		if (manager->unit_flags[id] & U_MSG_OUT) {
			manager->unit_flags[id] &= ~U_MSG_OUT;
			if (!ncr_rev) { /* if we're using the old chip */
	    		   ncr->command = TRANSFER; wbflush(); /* ncr bug! */
			} else
			   cmn_err(CE_CONT,"ncr msg-out bug with new chip?\n");
		} else {
			/*
			cmn_err(CE_CONT,"non-planned msg-out, sending NO-OP\n");
 			 */
			if (!ncr_rev) /* if we're using the old chip */
	   		   manager->unit_flags[id] |= U_MSG_OUT; /* ncr bug */
			ncr->fifo = MSG_NOOP; wbflush();
			ncr->command = TRANSFER; wbflush();
		}
	    }
	    break;
	case MSG_IN:
	    if (!(manager->unit_flags[id] & U_PRIMED)) {
		if (sync_msg_cnt) { /* set by sync_msg() */
			message[sync_msg_cnt-1] = ncr->fifo;
			ncr->command = MSG_OK; wbflush(); /* let ASC drop ack */
			if (message[sync_msg_cnt-1] == MSG_REJECT) {
				manager->unit_flags[id] |= U_PRIMED;
				/* initialize for next time */
				for (i=0; i < 5; i++)
					message[i] = 0;
				sync_msg_cnt = 0;
				return;
			}
			if (sync_msg_cnt == 5) {
				sync_msg(iopb,MSG_IN); /* finish up */
				return;
			}
			ncr->command = TRANSFER; wbflush();/* next msg-in byte*/
			sync_msg_cnt++;
			return;
		} else {
			manager->unit_flags[id] &= ~U_MSG_OUT;
			/* flush msg-out bytes in case they didn't get sent!
			 * as when the tape rejects after the 1st msg-out byte
			 */
			ncr->command = FLUSH_NCR_FIFO; wbflush();
			ncr->command = TRANSFER; wbflush();/* 1st msg-in byte*/
			sync_msg_cnt++;
			return;
		}
	    } else if (manager->unit_flags[id] & U_STATUS_SEQ) {
		if (iopb->scsi_status = ncr->fifo) { /* status byte */
			/* reset dma engine in case we were in the middle of a
			 * transfer and got a check condition!!
			 */
			if (IS_R3030) {
				rambo->dma_mode_1 = FLUSH_RAMBO_FIFO; wbflush();
				rambo->dma_block_1 = 0; wbflush();
			} else { /* Genesis */
				/* reset the dma engine */
				dma_engine->count_control = ~NO_DMA_RESET;
				wbflush();
			}
		}
		manager->unit_flags[id] &= ~U_STATUS_SEQ;
		msg = ncr->fifo; /* grab message byte */
		if (msg)
			cmn_err(CE_CONT,
				"command complete message byte= 0x%x!\n",msg);
		ncr->command = MSG_OK; wbflush(); /* let ASC drop ack */
		ncr->command = FLUSH_NCR_FIFO; wbflush(); /* needed? */
	    } else if (manager->unit_flags[id] & U_MSG_IN) {
		msg = ncr->fifo; /* grab message byte */
		manager->unit_flags[id] &= ~U_MSG_IN;
		ncr->command = MSG_OK; wbflush(); /* let ASC drop ack */
	    } else if (manager->unit_flags[id] & U_RESEL_ID) {
		msg = ncr->fifo; /* grab ID message byte from reselection */
		/* set period and offset here if we arrived via a reselect cause
		 * we could have these set up for another drive with different
		 * requirements!
		 */
		if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
			ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
			ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
		} else
			ncr->f_o.sync_offset = ASYNC; wbflush();
		/* now we can let the potential syncronous data-in bytes in! */
		ncr->command = MSG_OK; wbflush(); /* let ASC drop ack */
		manager->unit_flags[id] &= ~U_RESEL_ID;
	    } else {
		manager->unit_flags[id] |= U_MSG_IN; /* set for return here! */
/* if we're writing the fifo will still pull in data (up to the max fifo amt)
   after switching to message-in for the saveptr. that means our snapshot of
   tc_all could be off by the amount in the fifo!! and really screw things up
   for saveptr(), so save the number of bytes in the fifo just in case!
   also, if the fifo's full, say after the saveptr msg, then we have nowhere to
   put the next msg-in byte (the disconnect msg), so we flush the fifo!!
 */
		tc_all  = TC_ALL; /* before xfer to get a valid count */
		tc_fifo = ncr->f_o.fifo_flags & FIFO_MASK; /* could be needed */
		ncr->command = FLUSH_NCR_FIFO; wbflush(); /* now purge it! */
		ncr->command = TRANSFER; wbflush();/* latch msg-in byte*/
		return; /* wait for interrupt, we'll still be msg-in phase*/
	    }
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
		    cmn_err(CE_CONT,"asc: (6) PANIC\n");
		    GoToMonitor();
#endif	DIAG
		}
		break; /* just wait for service required int */
	    } else {
		switch (msg) {
		case MSG_RSTRPTR: /* restore pointers */
		    break;
		case MSG_SAVEPTR: /* save pointers */
		    /* NO saveptr() call if directly from
		     * a reselect (after id message) */
		    manager->channel_flags &= ~CH_RESELECT;
		    if (tc_all) { /* do we need to saveptr() */
			if (tc_fifo) {
	    		   if (data_phase == DATA_OUT) { /*we were writing*/
				tc_all += tc_fifo; /* correct our count*/
			   }
			}
			saveptr(iopb, tc_all);
			/* needed for the case whereby the next scsi action is a
			 * command (we use the fifo!) to a target that takes the
			 * command bytes, then issues a save ptr msg, after no
			 * data transfer then disconnects! (1/4" tape)
			 */ 
			ncr->count_lo = 0; wbflush();
			ncr->count_hi = 0; wbflush();
			ncr->command = NOP|DMA; wbflush(); /* force load! */
		    }	
		    break;
		case MSG_DISCON: /* target will yield bus */
		    manager->unit_flags[id] |= (U_DISCON|U_BUS_FREE_OK);
		    manager->unit_flags[id] &= ~U_DATA_PHSE;
		    /* reset relevant dma engine! */
		    if (IS_R3030) {
			rambo->dma_block_1 = 0; wbflush();
			rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
		    } else { /* Genesis */
			/* reset the dma engine */
			dma_engine->count_control = ~NO_DMA_RESET; wbflush();
		    }
		    break;
		case MSG_CMDCMP: /* command complete */
		    manager->unit_flags[id] |= U_CMDCMP;
		    break;
		case MSG_REJECT: /* rejected message */
		    cmn_err(CE_CONT,"asc received a REJECT message\n");
		    break;
		default:
		    cmn_err(CE_CONT,"phase(): unknown msg-in (%x)\n",msg);
		}
	    }
	    break;
	default:
	    break;
	}
}
/*      Transfer data, cmd, or msg bytes to or from memory,
 *	using partial RAMBO dma. Further action will be required
 *	for reads after we get the completion interrupt.
 */
ptm_rambo(iopb, dataphase)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register volatile u_char *fifo = &ncr->fifo;
	register u_char *ptr, *ptr1; /* memory address pointer */
    	register struct scsi_unit *un = iopb->scsi_un;
	register len, offset = 0;
	int id;

	/* just in case */
	rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
	rambo->dma_block_1 = 0; wbflush();
	id = manager->active_id; /* our current active target */
	if (dataphase == COMMAND) { /* only for SEL_ATN_STOP */
		manager->unit_flags[id] |= U_CMD_PHSE;
		/* needed for a swift drive problem, zero the internal count
		 * registers */
		ncr->count_lo = 0; wbflush();
		ncr->count_hi = 0; wbflush();
		ncr->command = NOP|DMA; wbflush(); /* force the load! */
	    	if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
		    ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
		    ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
	    	} else {
		    ncr->f_o.sync_offset = ASYNC; wbflush();
		}
		ptr = (u_char *)&iopb->cmd_blk; /* pointer to first cmd byte */
		len = common_scsi_cmdlen(*ptr);
	} else { /* we're in a DATA phase */
		len = ncr->count_lo = iopb->scsi_count0;
		wbflush();
		ASSERT(len < un->un_dmaalign);
		ncr->count_hi = 0; wbflush();
		ptr = (u_char*)iopb->scsi_bufaddr0; /* memory address pointer */
	}
	if (dataphase == DATA_IN) { /* after first TRANSFER cmd */
		manager->unit_flags[id] |= U_PTM_READ; /* set state */
		rambo->dma_mode_1 = TO_MEMORY|CHANNEL_EN; wbflush();
	} else if (dataphase == COMMAND) {
		do {
			*fifo = *ptr++; wbflush();
		} while (len -= 1);
		ncr->command = TRANSFER; wbflush();
	} else { /* we're doing DATA-OUT */
		ptr1 = (u_char*)K2_TO_K1(un->un_buf_64);
		do {
			ptr1[offset++] = *ptr++; /* fill buffer */
		} while (len -= 1);
		rambo->dma_mode_1 = CHANNEL_EN; wbflush();
	}
	if (dataphase != COMMAND) {
		rambo->dma_laddr_1 = (u_long)(K2_TO_PHYS(un->un_buf_64));
		wbflush();
		rambo->dma_block_1 = 1; wbflush();
		/* is this correct? */
	    	if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
		    ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
		    ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
	    	} else {
		    ncr->f_o.sync_offset = ASYNC; wbflush();
		}
		ncr->command = TRANSFER|DMA; wbflush();
	}
}
/*      Transfer data, cmd, or msg bytes to or from memory,
 *	using the Genesis dma engine and/or ncr chip.
 *	Remember that setting PTM in the higher level code does
 *	not necessarily mean that we can't use dma.
 */
ptm_genesis(iopb, dataphase)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register struct ncr_manager* manager = &ncr_manager;
	register volatile u_char *fifo = &ncr->fifo;
	register u_char *ptr, *ptr1; /* memory address pointers */
    	register struct scsi_unit *un = iopb->scsi_un;
	u_int len, count, id, offset = 0, flag = 0;

	id = manager->active_id; /* our current active target */
	if (dataphase == COMMAND) { /* only for SEL_ATN_STOP */
		manager->unit_flags[id] |= U_CMD_PHSE|U_PTM_WRITE;
		/* needed for a swift drive problem, zero the internal count
		 * registers */
		ncr->count_lo = 0; wbflush();
		ncr->count_hi = 0; wbflush();
		ncr->command = NOP|DMA; wbflush(); /* force the load! */
	    	if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
		    ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
		    ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
	    	} else {
		    ncr->f_o.sync_offset = ASYNC; wbflush();
		}
		ptr = (u_char *)&iopb->cmd_blk; /* pointer to first cmd byte */
		len = common_scsi_cmdlen(*ptr);
	} else { /* we're in a DATA phase */
		len = iopb->scsi_count0;
		ptr = (u_char*)iopb->scsi_bufaddr0; /* memory address pointer */
	}
	if (IS_KSEG0(ptr))
	    ptr = (u_char*)(K0_TO_PHYS(ptr));
	else if (IS_KSEG1(ptr))
	    ptr = (u_char*)(K1_TO_PHYS(ptr));
	else if (IS_KSEG2(ptr))
	    ptr = (u_char*)(K2_TO_PHYS(ptr));
	while (!(dma->count_control & NO_DMA_PENDING)) {
	    DELAY(1);
	    if (flag++ == 0x200000) {
		dma_regs();
		asc_regs(YES);
		cmn_err(CE_PANIC,"dma locked up! trying to ptm transfer");
	    }
	}
	if (flag)
	    ptm_dma_pending++;
	if ((long)ptr & un->un_dmaaddmask) { /* non aligned address */
		ASSERT(len < 65); /* common_scsi should assure this! */
		ptr1 = (u_char*)K2_TO_K1(un->un_buf_64);
		if (dataphase == DATA_OUT) { /* writing */
			ptr = (u_char*)PHYS_TO_K1(ptr);
			do {
				ptr1[offset++] = *ptr++; /* fill buffer */
			} while (len -= 1);
		} else { /* reading, so set special state to copy later */
			manager->unit_flags[id] |= U_PTM_COPY; /* set state */
		}
		dma->mem_ptr = (u_int)(K1_TO_PHYS(ptr1)); wbflush();
	} else
		dma->mem_ptr = (u_int)ptr; wbflush();
	ncr->count_lo = len; wbflush();
	ncr->count_hi = 0;   wbflush();
	if (count = (len & un->un_dmacntmask)) /* roundup required? */
		len += (un->un_dmaalign - count);
	count = len << DMA_BYTE_SHIFT; /* convert and position count */
	count = ~count & 0xffff0000;
	count |=(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_FLUSH_PIPE|NO_CLR_DMA);
	if (manager->unit_flags[id] & U_SYNC) { /* we're syncronous */
	    ncr->f_o.sync_offset = iopb->scsi_syncoff;  wbflush();
	    ncr->s_p.sync_period = iopb->scsi_syncxfer; wbflush();
	} else {
	    ncr->f_o.sync_offset = ASYNC; wbflush();
	}
	if (dataphase == DATA_IN) {
		manager->unit_flags[id] |= U_PTM_READ; /* set state */
	} else {
		count |= TO_MEMORYB; /* write to a scsi device */
	}
	ncr->command = DMA|TRANSFER; wbflush(); /* dma xfer cmd */
	dma->count_control = count;  wbflush(); /* start xfer */
}
/*      Transfer data from SCSI to memory, or memory to SCSI
 *	using "hardware transfer mode", using DMA.
 */
dma(iopb)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	register struct scsisge *sgeptr, *sgeptr_nxt;
	u_int id, flag = 0;

	id = manager->active_id; /* our current active target */
	manager->unit_flags[id] |= U_DATA_PHSE;

	manager->channel_flags &= ~CH_RESELECT;
	/* a count of 0 is a 64Kb transfer, the maximum for 16 bits */
	ncr->count_hi = MB(iopb->scsi_count); wbflush();
	ncr->count_lo = LB(iopb->scsi_count); wbflush();
	/* point to current scatter/gather element (sge)
	 * and set up RAMBO
	 * NOTE that rambo's block count must be 0 coming in here,
	 * so he can't start transferring before we want him to
	 */
	if (IS_R3030) {
		sgeptr = sgeptr_nxt = (struct scsisge *)iopb->scsi_extra;
		sgeptr_nxt++; /* for our look-ahead test */
		rambo->dma_laddr_1 = sgeptr->mem_ptr; wbflush();
		if (!sgeptr_nxt->count) { /* no INT unless we have to! */
			sgeptr->chanmode &= ~INTR_EN;
		}
	} else { /* Genesis */
		     while (!(dma->count_control & NO_DMA_PENDING)) {
			   DELAY(1);
			   if (flag++ == 0x200000) {
				dma_regs();
				asc_regs(YES);
				cmn_err(CE_PANIC,"dma locked up!");
			   }
		     }
		     if (flag)
			   dma_pending++;
	}
	/* I believe the scsi chip will be slower so start it first! */
	ncr->command = DMA|TRANSFER; wbflush(); /* dma xfer cmd */
	if (IS_R3030) {
		rambo->dma_mode_1  = sgeptr->chanmode; wbflush();
		rambo->dma_block_1 = sgeptr->count;    wbflush();
	} else { /* we're Genesis */
		dma->cmd_addr = K1_TO_PHYS(iopb->scsi_extra); wbflush();
	}
	/*
	 * hands off and wait for RAMBO interrupt if enabled,
	 * or NCR completion or service required
	 * interrupt will occur instead.
 	 * Genesis uses command chaining so that no dma should occur.
	 */
}
dumpsge(ptr)
register struct scsisge *ptr;
{
	cmn_err(CE_CONT,"ptr= %x mem_ptr= %x ",ptr,ptr->mem_ptr);
	cmn_err(CE_CONT,"count= %x chanmode= %x\n",
				ptr->count,ptr->chanmode);
}
dumpsge_g(ptr)
register struct scsisge_g *ptr;
{
	cmn_err(CE_CONT,"ptr= %x mem_ptr= %x ", ptr,ptr->mem_ptr);
	cmn_err(CE_CONT,"control_count= %x\n",  ptr->control_count);
}
/*
 * routine to collapse and update dma chain entries for subsequent
 * resumption of dma data transfer upon reselection. We're keyed off
 * of the asc transfer count which reflects the exact number of bytes
 * NOT transferred which is compared with the count originally set.
 */
saveptr(iopb, tc_all)
register struct scsi_iopb *iopb;
register unsigned int tc_all;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register struct ncr_manager* manager = &ncr_manager;
	int bytes_xfered;
	u_int len, count, offset_count;
	u_short fill_data = 0;
	struct scsisge *ptr, *nxtptr;
	struct scsisge_g *ptr_g, *nxtptr_g;
    	struct scsi_unit *un;
	u_char cmd, i;
	int id = iopb->scsi_target;

	/* calculate the actual bytes read/written across the scsi bus.
	 * if nothing was transferred just return
	 */
	if (!(bytes_xfered = iopb->scsi_count - tc_all))
		return;
	ASSERT(bytes_xfered > 0);
    	un = iopb->scsi_un;
	if (iopb->scsi_count0) { /* disconnect on ptm transfer */
	    /* correct our total byte count (for asc programming) */
	    iopb->scsi_count0 -= bytes_xfered; /* update official byte count */
	    iopb->scsi_bufaddr0 += bytes_xfered; /* update address */
	    scsi_multi_phase = 0;
	    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE;
	    if (IS_R3030) {
	      if (cmd == C0_READ) {
	    	/* see if there's data in the fifo to be helped along
		 * if so, we need to fill the rambo buffer to 32 half
		 * words and force the 64 byte block transfer */
		if (len = rambo->dma_mode_1 & COUNT_MSK) {/* half-word count*/
			len = BLOCK_HW_CNT - len; /* half words to force dma*/ 
			do {
				 rambo->dma_fifo_1 = fill_data; wbflush();
			} while (len -= 1);
		}
	      } else if (!(rambo->dma_mode_1 & FIFO_EMPTY)) { /* needs flush */
		rambo->dma_mode_1  = 0; wbflush(); /* disable channel */
		rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
	      }
	      rambo->dma_block_1 = 0; wbflush();
	    } else { /* Genesis */
	      if (cmd == C0_READ) {
		dma->control =
			(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA);
		wbflush();
		DELAY(1);
		dma->control =
  (NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA|NO_FLUSH_PIPE|NO_RUN_ENABLE);
		wbflush();
	      } else { /* writing, so reset the dma engine */
	        dma->count_control = ~NO_DMA_RESET;
		wbflush();
	      }
	    }
	    return;
	}
	if (count = bytes_xfered & un->un_dmaaddmask) { /* non-blk aligned */
	    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE;
	    if ((manager->unit_flags[id] & U_SYNC) &&
			 ((cmd == C0_WRITE) || (cmd == C0_READ)))
		cmn_err(CE_PANIC,"saveptr: sync device attemped multi-phase");
	    /* must set up count0 and bufaddr0 */
	    offset_count = un->un_dmaalign - count;
	    iopb->scsi_count0 = offset_count; /* sub-blk count */
	    bytes_xfered += offset_count;
	    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd & ~CD10BYTE;
	    if (cmd == C0_READ) {
	    	/* see if there's data in the fifo to be helped along
		 * if so, we need to fill the rambo buffer to 32 half
		 * words and force the 64 byte block transfer */
		if (IS_R3030) {
		   if (len = rambo->dma_mode_1 & COUNT_MSK) {/* 1/2 word count*/
			len = BLOCK_HW_CNT - len; /* half words to force dma*/ 
			do {
				 rambo->dma_fifo_1 = fill_data; wbflush();
			} while (len -= 1);
		   }
		} else { /* Genesis */
			dma->control =
				(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA);
			wbflush();
			DELAY(1);
			dma->control =
	  (NO_CHAIN_ENABLE|NO_DMA_RESET|NO_CLR_DMA|NO_FLUSH_PIPE|NO_RUN_ENABLE);
			wbflush();
		}
	    } else { /* we're writing */
		if (IS_R3030) {
		   if (!(rambo->dma_mode_1 & FIFO_EMPTY)) { /* needs flush */
		      rambo->dma_mode_1  = 0; wbflush(); /* disable channel */
		      rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
		   }
		} else { /* Genesis */
		      /* reset the dma engine */
		      dma->count_control = ~NO_DMA_RESET; wbflush();
		}
	    }
	    if (IS_R3030) {
	    	rambo->dma_block_1 = 0; wbflush();
	    }
	}
	/* update official byte count 
	 */
	iopb->scsi_count -= bytes_xfered;
	/* point to first scatter/gather element (sge)
	 */
	if (IS_R3030) {
	   nxtptr = ptr = (struct scsisge *)iopb->scsi_extra;
	   count  = ptr->count << BLOCK_SHIFT; /* byte count for this sge */
	   while(bytes_xfered >= count) {
		bytes_xfered -= count; /* reduce the running count */
		nxtptr++; /* point to the next sge */
		if (!(count = nxtptr->count << BLOCK_SHIFT)) {
#ifdef MIPS_LOCAL
cmn_err(CE_CONT,"iopb->count= %x count_all= %x\n",
				iopb->scsi_count,iopb->scsi_count_all);
cmn_err(CE_CONT,"count= %x bytes_xfered= %x tc_all= %x\n",
					count, bytes_xfered, tc_all);
cmn_err(CE_CONT,"iopb->scsi_extra= %x\n",iopb->scsi_extra);
			ptr = (struct scsisge *)un->un_extra;
			for (i=0;i < MAX_SGENTRY;i++,ptr++) {
				dumpsge(ptr);
				if (!ptr->count) break;
			}
			asc_regs(YES);
			rambo_regs();
#endif MIPS_LOCAL
#ifndef	DIAG
			cmn_err(CE_PANIC,"saveptr: null advance chain pointer");
#else	DIAG
			cmn_err(CE_CONT,
				"saveptr: null advance chain pointer\n");
			cmn_err(CE_CONT,"asc: (11) PANIC\n");
			GoToMonitor();
#endif	DIAG
		}
		ptr = nxtptr; /* advance pointer to next chain entry */
	   }
	   iopb->scsi_extra = (u_int)ptr; /* update chain ptr(may be the same)*/
	   ptr->count -= (bytes_xfered >> BLOCK_SHIFT);/* adjust 'block' count*/
	   ptr->mem_ptr += bytes_xfered; /* adjust our dma address */
	   if (iopb->scsi_count0) { /* setup the rest of the multi-phase xfer */
		iopb->scsi_bufaddr0 = ptr->mem_ptr - offset_count;
		iopb->scsi_bufaddr0 = PHYS_TO_K1(iopb->scsi_bufaddr0);
	   }
	   nxtptr = ptr + 1;
	   if (!nxtptr->count) /* no INT unless we have to! */
		ptr->chanmode &= ~INTR_EN;
	} else { /* Genesis */
	   nxtptr_g = ptr_g = (struct scsisge_g *)iopb->scsi_extra;
	   count = ptr_g->control_count;
	   count = ~count & 0xffff0000;
	   count = count >> DMA_BYTE_SHIFT; /* sge byte count */
	   while(bytes_xfered >= count) {
		bytes_xfered -= count; /* reduce the running count */
		nxtptr_g++; /* point to the next sge */
		count = nxtptr_g->control_count;
	   	count = ~count & 0xffff0000;
		if (!(count = count >> DMA_BYTE_SHIFT)) {
#ifdef MIPS_LOCAL
cmn_err(CE_CONT,"iopb->count= %x count_all= %x\n",
				iopb->scsi_count,iopb->scsi_count_all);
cmn_err(CE_CONT,"count= %x bytes_xfered= %x tc_all= %x\n",
					count, bytes_xfered, tc_all);
cmn_err(CE_CONT,"iopb->scsi_extra= %x\n",iopb->scsi_extra);
			ptr_g = (struct scsisge_g *)un->un_extra;
			for (i=0;i < MAX_SGENTRY;i++,ptr++) {
				dumpsge_g(ptr_g);
				if (!ptr_g->control_count) break;
			}
			asc_regs(YES);
			dma_regs();
#endif MIPS_LOCAL
#ifndef	DIAG
			cmn_err(CE_PANIC,"saveptr: null advance chain pointer");
#else	DIAG
			cmn_err(CE_CONT,
				"saveptr: null advance chain pointer\n");
			cmn_err(CE_CONT,"asc: (11) PANIC\n");
			GoToMonitor();
#endif	DIAG
		}
		ptr_g = nxtptr_g; /* advance pointer to next chain entry */
	   }
	   iopb->scsi_extra = (u_int)ptr_g;/*update chain ptr(may be the same)*/
	   count -= bytes_xfered; /* count is already correct! */
	   count = count << DMA_BYTE_SHIFT; /* convert and position byte cnt */
	   count = ~count & 0xffff0000;
	   ptr_g->control_count &= 0xffff; /* mask off the old count */
	   ptr_g->control_count |= count;  /* fold in the new half-word count */
	   ptr_g->mem_ptr += bytes_xfered; /* adjust our dma address */
	   if (iopb->scsi_count0) { /* setup the rest of the multi-phase xfer */
		iopb->scsi_bufaddr0 = ptr_g->mem_ptr - offset_count;
		iopb->scsi_bufaddr0 = PHYS_TO_K1(iopb->scsi_bufaddr0);
	   }
	}
}
/* routine to handle extended message handshake with a target to
 * determine if syncronous transfers are supported.
 * initially we're already in message out phase with ATN set.
 */
sync_msg(iopb,msgout_msgin)
register struct scsi_iopb *iopb;
register msgout_msgin;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register struct scsi_unit *un = iopb->scsi_un;
	register struct ncr_manager* manager = &ncr_manager;
	int i, id;
	static u_char deflt_message[5] =
	{MSG_EXT,EXT_MSGLEN,MSG_SYNC_CODE,SYNC_XFER_RATE_ASC,SYNC_OFFSET_ASC};

	id = manager->active_id; /* our current active target */
	if (msgout_msgin == MSG_OUT) { /* handle msg-out portion first */
	   ncr->command = FLUSH_NCR_FIFO; wbflush();
	   for (i=0; i < 5; i++) {
		ncr->fifo = deflt_message[i]; wbflush();
	   }
	   ncr->command = TRANSFER; wbflush(); /* wait for BS and FC? */
	   manager->unit_flags[id] |= U_MSG_OUT; /* handle ncr bug */
	   sync_msg_cnt = 0; /* set to enable msg-in byte handling */
	} else { /* handle msg-in bytes */
	   /* the message[3] byte specifies the minimum time between
	    * leading edges of successive REQ or ACK pulses in ns X 4
	    */
	   if (message[3] != 0xff && message[3]) {  /* valid extended msg? */
		if     (message[3] <=0x32) i= 0x05; /* clks, 5.00 Mb/s max */
		else if(message[3] < 0x3c) i= 0x06; /* clks, 4.16 Mb/s max */
		else if(message[3] < 0x46) i= 0x07; /* clks, 3.57 Mb/s max */
		else if(message[3] < 0x50) i= 0x08; /* clks, 3.12 Mb/s max */
		else if(message[3] < 0x5a) i= 0x09; /* clks, 2.77 Mb/s max */
		else if(message[3] < 0x64) i= 0x0a; /* clks, 2.50 Mb/s max */
		else if(message[3] < 0x6e) i= 0x0b; /* clks, 2.27 Mb/s max */
		else if(message[3] < 0x78) i= 0x0c; /* clks, 2.08 Mb/s max */
		else if(message[3] < 0x82) i= 0x0d; /* clks, 1.92 Mb/s max */
		iopb->scsi_syncxfer = i;
		if (iopb->scsi_syncoff = message[4])   /* max offset */
			manager->unit_flags[id] |= U_SYNC; /* set state */
	    	if (showconfig) {
			if (manager->unit_flags[id] & U_SYNC) { /* set state */
			   cmn_err(CE_CONT,
			   "target %x says sync xfer= %x (%dns) offset= %d\n",
				id,message[3],message[3]*4,message[4]);
/*
			   cmn_err(CE_CONT,
			   "sync_xfer= %x sync_offset= %x\n",
				iopb->scsi_syncxfer, iopb->scsi_syncoff); 
 */
			} else
			   cmn_err(CE_CONT,
			   "target %d is asyncronous\n",id);
		}
	   } else { /* Asyncronous only supported */
	   	un->un_dmastartmask = 0; /* allow multi-phase use */
	   }
sync_done:
	   /* initialize globals for next time */
	   for (i=0; i < 5; i++)
		message[i] = 0;
	   sync_msg_cnt = 0;
	   manager->unit_flags[id] |= U_PRIMED; /* set state */
	}
}


/*	scsi target state machine:

	busfree -> (no service req) process cmd -> terminate -> disconnect
	busfree -> (ser. req) branch to msgout phase -> cmd phase ->
		 process cmd -> terminate -> disconnect

	msgout phase -> branch to itself until message is received -> next

*/

#ifdef TARGET_MODE
#undef STATIC
#define STATIC static
#define cmn_err

STATIC
ncr_target_term_phase(status)
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
        unsigned char x;
	*fifo = (u_char) status; wbflush();
        *fifo = 0; wbflush();

        ncr->command = T_TERM_SEQN; wbflush();
	ncr_target.nt_state = NT_TERMINATE_PHASE;
	ncr_target.nt_next = NT_DISCONNECT_PHASE;
}


STATIC
ncr_target_io(len,cmd)
unsigned cmd;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
	register u_char *p;
	register i;

	ncr->count_lo = len; wbflush();
	ncr->count_hi = 0; wbflush();
        ncr->f_o.sync_offset = ASYNC; wbflush();
	
	for(i = 0, p = &ncr_target.nt_buf[0]; i < len; i++)
		*fifo = *p++;
	ncr->command = (unsigned char) cmd; wbflush();
}

STATIC
ncr_target_io_cmd(cmd)
unsigned cmd;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);

	ncr->count_lo = 1; wbflush();
	ncr->count_hi = 0; wbflush();
        ncr->f_o.sync_offset = ASYNC; wbflush();
	ncr->command = (unsigned char) cmd; wbflush();
}

STATIC
ncr_target_setup_sense()
{
	register char *p = &ncr_target.nt_buf[0];
	register i;

	for(i = 0; i < 8; i++)
		*p++ = 0;
	p = &ncr_target.nt_buf[0];
	p[0] = 0xf0;
	p[2] = ncr_target.nt_sensekey;
}

STATIC
ncr_target_setup_inquiry()
{
	register char *p = &ncr_target.nt_buf[0];

	*p++ = TYPE_CPU;
	*p++ = 0;
	*p++ = 2;
	*p++ = 0;
	*p++ = 0;
}


	
STATIC
ncr_target_proc_cmd()
{
	ncr_target.nt_state = NT_DATA_PHASE;
	ncr_target.nt_next = NT_TERMINATE_PHASE;

	switch(ncr_target.nt_buf[0]) {
	case C0_INQUIRY:
		ncr_target_setup_inquiry();
		ncr_target_io(5,T_SEND_DATA);
		break;
	case C0_TESTRDY:
		ncr_target_term_phase(SCSI_OK);
		break;
	case C0_REQSENSE:
		ncr_target_setup_sense();
		ncr_target_io(8,T_SEND_DATA);
		ncr_target.nt_sensekey = NO_SENSE;
		break;
	default:
		ncr_target.nt_sensekey = ILL_REQ;
		ncr_target_term_phase(SCSI_CHECK);
	}
}

STATIC
ncr_target_busfree(status,intr_status)
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
        char busid,id;
	int i;

cmn_err(CE_CONT,"selected\n");
	ncr_manager.channel_flags | CH_BUSY;
	busid = *fifo;
	id = *fifo;
	if(intr_status & SER_REQ) {
		ncr_target.nt_blen = 0;
		ncr_target.nt_state = NT_BUSFREE;
		ncr_target.nt_next = NT_CMD_PHASE;
		ncr_target_msgout();
		return;
	}
	for(i = 0; i < 6; i++) {
		ncr_target.nt_buf[i] = *fifo;
	}
	ncr_target_proc_cmd();
}

STATIC
ncr_target_cmd_phase()
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
	register char *p = &ncr_target.nt_buf[0];

cmn_err(CE_CONT,"cmd phase\n");
	if(ncr_target.nt_state != NT_CMD_PHASE) {
		ncr_target.nt_blen = 0;
		ncr_target_io_cmd(T_REC_CMD);
		ncr_target.nt_state = NT_CMD_PHASE;
		return;
	}

	ncr_target.nt_state = NT_CMD_PHASE;
	ncr_target.nt_next = NT_TERMINATE_PHASE;
	p[ncr_target.nt_blen++] = *fifo;
	if(ncr_target.nt_blen >= 6) {
		ncr_target_proc_cmd();
	} else
		ncr_target_io_cmd(T_REC_CMD);
}

STATIC
ncr_target_setup_sync()
{
	register char *p;

	p = &ncr_target.nt_buf[0];

	p[0] = 1;
	p[1] = 3;
	p[2] = 1;
	p[3] = 0;
	p[4] = 0;
}

STATIC
ncr_target_proc_msg()
{
cmn_err(CE_CONT,"proc msg\n");
	ncr_target_setup_sync();
	ncr_target_io(5,T_SEND_MSG);
	ncr_target.nt_state = NT_NEW_PHASE;
}

STATIC
ncr_target_msgout()
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
	register char *p = &ncr_target.nt_buf[0];

cmn_err(CE_CONT,"msgout\n");
	if(ncr_target.nt_state != NT_MSGOUT_PHASE) {
		ncr_target.nt_blen = 0;
		ncr_target_io_cmd(T_REC_MSG);
		ncr_target.nt_state = NT_MSGOUT_PHASE;
		return;
	}

	ncr_target.nt_state = NT_MSGOUT_PHASE;
	p[ncr_target.nt_blen++] = *fifo;
	if(p[0] == MSG_EXT) {
		if(ncr_target.nt_blen >= 2) {
			if(p[1] > 3) {
				ncr_target_msg_reject();
				return;
			}
			if(p[1] + 2 <= ncr_target.nt_blen) {
				ncr_target_proc_msg();
				return;
			}
		}
		ncr_target_io_cmd(T_REC_MSG);
		return;
	}
	ncr_target_msg_reject();
}

STATIC
ncr_target_setup_reject()
{
	ncr_target.nt_buf[0] = MSG_REJECT;
}


STATIC
ncr_target_msg_reject()
{
	ncr_target_setup_reject();
	ncr_target_io(1,T_SEND_MSG);
	ncr_target.nt_state = NT_MSGIN_PHASE;
}

STATIC
ncr_target_intr()
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
        register volatile u_char *fifo = &ncr->fifo;
        register seqn,intr_status,status;
	register struct ncr_manager* manager = &ncr_manager;

	status = ncr->s_d.status;
	intr_status = ncr->i_t.interrupts;

cmn_err(CE_CONT,"stat %x intr %x state %d next %d\n",status,intr_status,
	ncr_target.nt_state, ncr_target.nt_next);
	/* handle exceptions first */
	if(intr_status & SCSI_RESET) {
		int i;
		ncr->command = FLUSH_NCR_FIFO; wbflush();
		ncr->command = T_DISC;wbflush();
		ncr_target.nt_state = ncr_target.nt_next = NT_BUSFREE;
		for(i = 0; i < 8; i++)
			manager->unit_flags[i] &= ~U_PRIMED;
		return;
	}
		
	if(status & (GROSS_ERR|ASC_PAR_ERR)) {
	/* for now we just disconnect the initiator */
		ncr->command = FLUSH_NCR_FIFO; wbflush();
		ncr->command = T_DISC;wbflush();
		ncr_target.nt_state = ncr_target.nt_next = NT_BUSFREE;
		return;
	}

	/* service msgout and cmd phase since we can only do one byte at a 
	time.  The state machine does not quite work because it implies
	a change of state at this point.
	*/
	switch(ncr_target.nt_state) {
	case NT_MSGOUT_PHASE:
		ncr_target_msgout();
		return;
	case NT_CMD_PHASE:
		ncr_target_cmd_phase();
		return;
	}

	/* assume a command completion */
	switch(ncr_target.nt_next) {
	case NT_CMD_PHASE:
		ncr_target_cmd_phase();
		break;
	case NT_DISCONNECT_PHASE:
		if(!(intr_status & DISCONECT)) {
			ncr->command = T_DISC; wbflush();
		}
		ncr->command = FLUSH_NCR_FIFO; wbflush();
		ncr->command = EN_RESEL; wbflush();
		ncr_target.nt_state = NT_BUSFREE;
		ncr_manager.channel_flags &= ~CH_BUSY;
		ascstart1(0,LOWLEVEL,1);
		break;
	case NT_TERMINATE_PHASE:
	default:
		ncr_target_term_phase(SCSI_OK);
		break;
	}
}
#endif
