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
#ident "$Header: asc_poll.c,v 1.13.1.5 90/12/14 12:28:24 chungc Exp $"

/*
 * asc_poll.c 		ncr53c94 device driver
 *			ASC
 * These routines will be used by stand-alone disk, tape and other drivers.
 * The desire is to keep common low-level routines out of the higher
 * level scsi drivers.
 */
#ifndef STANDALONE
#include "sys/types.h"
#include "sys/bsd_glue.h"
#include "sys/immu.h"
#include "sys/sysmacros.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/dvh.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"	    /* dma defines */
#include "sys/ncr53c94.h"   /* ASC (Advanced SCSI Controller) defines */
#include "sys/r3030scsi.h"
#include "sys/scsi.h"
#else STANDALONE
#include "prom/prom.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "machine/dvh.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/rambo.h"
#include "machine/ncr53c94.h"
#include "machine/r3030scsi.h"
#include "machine/scsi.h"
#endif STANDALONE

#define TC_ALL ((ncr->count_hi<<8)|ncr->count_lo)
#define FMT_POLL_LOOP_FACTOR	(POLL_LOOP_FACTOR / 3)

extern int SCSI_REG_BASE[];
extern int RAMBO_REG_BASE[];

#ifdef STANDALONE
int scsi_first;
_scsi_initp()
{
    register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
    register volatile RAMBO_REG *rambo  = (RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
    register volatile DMA_REG *dma = DMA_ADDR;
    char *scsi_id, *scsi_reset, sc;
    int target_id, reset_scsi;
    extern char *getenv();

    if (IS_RB3125) {
	*((long *) PHYS_TO_K1(CPU_CR_RB3125)) |= CR_SCSIRESETB;
    }
#ifndef SABLE
    if (badaddr(&ncr->count_lo, sizeof(ncr->count_lo))) {
	cmn_err(CE_CONT,"NCR 53c94 scsi chip not addressable at 0x%x\n",
						 &ncr->count_lo);
	return(-1); /* catastrophic failure (panic) */
    }
    if (IS_R3030) {
	if (badaddr(&rambo->dma_laddr_1, sizeof(rambo->dma_laddr_1))) {
		cmn_err(CE_CONT,"dma asic not addressable at 0x%x\n",
					 &rambo->dma_laddr_1);
		return(-1); /* catastrophic failure (panic) */
   	}
    } else { /* Genesis */
	if (badaddr(&dma->cmd_addr, sizeof(dma->cmd_addr))) {
		cmn_err(CE_CONT,"dma engine not addressable at 0x%x\n",
					 &dma->cmd_addr);
		return(-1); /* catastrophic failure (panic) */
   	}
    }
#endif SABLE
    if (!scsi_first) {
	ncr->command = NOP; wbflush(); /* needed after hard or soft reset! */
	DELAY(1024);	/* allow ~100us for things to settle down */
	ncr->command = RESET_NCR; wbflush(); /* soft reset the ASC */
	DELAY(1024);	/* allow ~100us for things to settle down */
	ncr->command = NOP; wbflush(); /* needed after hard or soft reset! */
	
	/* initialize the NCR ncr53c94 ASC (advanced scsi controller)
	 */
	ncr->clk_conv = FACTOR_25MHZ; wbflush(); /* 25Mhz clock */
	ncr->f_o.sync_offset = ASYNC; /* force to run async as default */
	ncr->config1 = NO_SCSI_RST_INT; wbflush();/* disable scsi reset int */
	
	/* determine if the scsi bus should be reset */
	reset_scsi = 1;	
	target_id = 7;	
        if (IS_R3030) {
            sc = (scsi_reset = getenv("scsi_reset")) ? *scsi_reset : 0;
            if (sc == '0') reset_scsi = 0; 
	    sc = (scsi_id = getenv("scsi_id")) ? *scsi_id : 0;
	    if ((sc >= '0') && (sc < '8')) target_id = sc - '0';
	    else {
		if (sc) {
		    cmn_err(CE_WARN,"Invalid scsi_id, reset to 7\n");
#ifdef PROM
		    set_nvram(NVADDR_SCSI_ID, NVLEN_SCSI_ID, "7");
#endif
		}
	    }
        }
	if(reset_scsi) {
	    ncr->command = RESET_SCSI; wbflush(); /* reset the scsi bus */
	    DELAY(4096);     /* allow ~300us for targets to see the reset */
	    ncr->command = NOP; wbflush(); /* required after reset */
	    DELAY(10000);   /* allow ~10ms for targets to recover from reset */
	}

 	/* enable parity checking, set ASC ID to highest priority (7)
	 * this is used for arbitration */
	ncr->config1 = PARITY_ENABLE|target_id; wbflush();
/*	ncr->config2 = ????; /* only 4 bits valid on current rev ncr
			      * these 4 bits are not used currently */
/*	ncr->config3 = ????; /* NO bits valid on current rev ncr */
	ncr->i_t.timeout = MS250; /* set 250ms timeout value for selection */

       /* initialize RAMBO channel 1 (scsi channel)
	*/
	if (IS_R3030) {
		rambo->dma_block_1 = 0; wbflush();
		rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
		rambo->dma_mode_1  = 0; wbflush(); /* disable channel */
	}
        scsi_first++; /* only the first time */	
    }
    return(0);
}
#endif STANDALONE


verify_scsi_id(target_id)
{
    register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
    struct scsi_unit *un;
    SCSI_INQUIRY *inq, *sinq, sinqbuf;
    int scsi_status, scsi_error, scsi_diff, scsi_count;
    int i,first,id_duplicate = 1;

	scsi_error = scsi_diff = scsi_count = 0;
	sinq = &sinqbuf;
	first = 1;	
	for(i = 0; i <= 7; i++) {
		if(i == target_id) continue;
		ncr->config1 = PARITY_ENABLE|i; wbflush();
		if(scsi_status = scsi_inquiry(target_id,0,&un)) {
		    if (scsi_status != 0x81) { 	/* not scsi inquiry timeout */
			scsi_error++;
	    	   	ncr->command = RESET_SCSI; wbflush(); /*reset scsi bus*/
	    	   	DELAY(4096);     
	    	   	ncr->command = NOP; wbflush(); 
	    	   	DELAY(10000);  
		    }
		    id_duplicate = 0;
		    break;
		}
		else {			/* detect for different responses */
    		    inq = (SCSI_INQUIRY *)K0_TO_K1(&un->un_inq);
		    scsi_count++;
		    if (!first) {
		    	if (strcmp(inq->vendor_id,sinq->vendor_id) ||
			    strcmp(inq->product_id,sinq->product_id) ||
			    strcmp(inq->revision_level,sinq->revision_level) ||
			    strcmp(inq->serial_nr,sinq->serial_nr)) 
			    scsi_diff++;
		    }
		    strcpy(sinq->vendor_id, inq->vendor_id);
		    strcpy(sinq->product_id, inq->product_id);
		    strcpy(sinq->revision_level, inq->revision_level);
		    strcpy(sinq->serial_nr, inq->serial_nr); 
		    first = 0; 
		}
	}
	ncr->config1 = PARITY_ENABLE|target_id; wbflush();
	if (id_duplicate && !scsi_diff)
		cmn_err(CE_WARN,"duplicated scsi id %d\n",target_id);
	else if (id_duplicate && (scsi_count == 7) && (target_id != 7))
		cmn_err(CE_WARN,"7 scsi devices detected: fully configured system or duplicated scsi id %d.\n", target_id);
	else if (scsi_error)
		cmn_err(CE_WARN,"scsi error, possibly due to scsi id conflict or scsi_reset.\n");
}


scsicmdp(iopb)
register struct scsi_iopb *iopb;
{
    register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
    register volatile RAMBO_REG *rambo = (RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
    register volatile DMA_REG *dma = DMA_ADDR;
    register int delay, cmd;
    register hold, seqn, status;
    u_short temp, phase = 0x1313;
    int  i, busy = 0;

    /* NOTE: for polled commands DISCONNECT/RECONNECT will NOT be allowed.
     * NCR interrupt pin assertion CAN NOT be disabled via the chip
     * registers so that these interrupts must be disabled elsewhere!
     * for pizazz it appears that it must be done at the cpu chip.
     * Arbitration is USED, since the ncr won't allow non-arbitration.
     * 
     * check for pending interrupt!
     */
    if (ncr->s_d.status & INTR) {
	    cmn_err(CE_CONT,"ncr interrupt pending prior to command start\n");
	    asc_regs(YES); /* dump ASC register state, clears pending ints */
	    cmn_err(CE_CONT,"trying to send command anyway...\n");
    }
busy_retry:
    if (IS_R3030) { /* reset Rambo to a know cleared state */
	rambo->dma_mode_1 = FLUSH_RAMBO_FIFO; wbflush(); /* flush fifo */
	rambo->dma_block_1 = 0; wbflush();
    } else { /* reset the genesis dma engine */
	dma->count_control = ~NO_DMA_RESET; wbflush();
    }
    ncr->command = FLUSH_NCR_FIFO; wbflush(); /* flush ncr fifo */
    _sel_cmd(iopb); /* handle arbitration, selection and command phases */
    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd; /* scsi command byte */
    switch (cmd) {
    case C0_REZERO:
    case C0_FORMAT:
    case C0_SPACE:
    case C0_LOAD:			/* same as C0_STARTSTOP */
    case C0_ERASE:
	/* make dots print about every second */
	delay = iopb->scsi_time * POLL_LOOP_FACTOR;
	break;
    default:
	delay = 10 * POLL_LOOP_FACTOR;	/* normal selection delay */
	break;
    }
    /*
     * POLL for 'arbitration-selection-command completion' 
     */
    while (!(ncr->s_d.status & INTR)) {
	if (cmd == C0_FORMAT && !(delay % FMT_POLL_LOOP_FACTOR))
	    cmn_err(CE_CONT,".");
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"TIMED-OUT waiting for selection interrupt\n");
	    iopb->scsi_hwstatus = SCSITMO;
	    asc_regs(YES); /* dump ASC register state */
	    if (IS_R3030)
		rambo_regs();
	    else
		dma_regs();
	    goto failed;
	}
	DELAY(POLL_LOOP_DELAY);
    }
    status = ncr->s_d.status;   /* read status register */
    seqn = ncr->s_p.seqn_step;  /* read sequence step register */
    seqn &= SEQN_STEP_MSK;      /* isolate relevent bits */
    hold = ncr->i_t.interrupts; /* read interrupt status register and
       			         * thereby clear all three registers plus
				 * the status register interrupt bit */
    temp = SER_REQ|CMD_CMPLT;
    if ((hold != temp) || (seqn != SEQN_SEL_NO_ATN)) {
	if (hold & DISCONECT) { /* selection time-out? */
	    iopb->scsi_hwstatus = SELTMO;
	} else {
	    if (hold != temp) {
	       cmn_err(CE_CONT,
	       "after SELECT, int reg = %x s/b %x\n",hold,SER_REQ|CMD_CMPLT);
		cmn_err(CE_CONT,
		"sequence reg= %x status reg= %x\n", seqn, status);
	    } else if (seqn != SEQN_SEL_NO_ATN) {
	       /* catch special case whereby the target drive asserts
		* status phase (busy) after selection. The Imprimis Swift
		* drive does this! This happens after a scsi reset!
		*/
	       if ((seqn == 2) && ((status & XFER_PHSE_MSK) == STATUS)) {
			/* status and message in phases */
			_status_msg(iopb,NO_XFER);
			if (iopb->scsi_status == SCSI_BUSY) {
		 	   DELAY(1048576);
			   if (busy++ == 0x100) {
	       			cmn_err(CE_CONT,"target continuously busy!\n");
				iopb->scsi_hwstatus = HWERROR;
				asc_regs(NO); /* dump ASC register state */
				goto failed;
			   }
			   goto busy_retry;
			}
	       }
	       cmn_err(CE_CONT,"SEQUENCE register = %x s/b %x\n",
						seqn,SEQN_SEL_NO_ATN);
	       cmn_err(CE_CONT,"STATUS register = %x\n",status);
	    }
	    iopb->scsi_hwstatus = HWERROR;
	    asc_regs(NO); /* dump ASC register state */
	}
	goto failed;
    }
    if (IS_R3030) {
	/* flush rambo if required */
	if (!(rambo->dma_mode_1 & FIFO_EMPTY))
		rambo->dma_mode_1 = FLUSH_RAMBO_FIFO; wbflush();/* flush fifo */
    } else { /* Genesis */
	dma->count_control = ~NO_DMA_RESET; wbflush();
    }
    ncr->command = FLUSH_NCR_FIFO; wbflush(); /* flush ncr fifo */
    /* data in/out phase, status and msg in phase
     */
    switch (cmd) {
    case C0_INQUIRY:
    case C0_READ:
    case C0_RECDIAG:
    case C0_REQSENSE:
    case C0_SENSEMODE:
    case C0_RDBLOCK:
    case C1_READ:
    case C1_READCAP:
    case C1_READDEF:
	phase = DATA_IN;
	break;
    case C0_FORMAT:
    case C0_MODESEL:
    case C0_REASSIGN:
    case C0_RECBUF:
    case C0_SENDDIAG:
    case C0_WRITE:
    case C1_SETLIMITS:
    case C1_WRITE:
	phase = DATA_OUT;
	break;
    }
    if ((iopb->scsi_flags & XFER_MASK) == NO_XFER) {
	(void)_status_msg(iopb,NO_XFER); /* status, msg-in phases */
    } else {
        (void)_xfer(iopb, phase, status); /* data xfer, status, msg-in phases */
    }
    if (iopb->scsi_hwstatus == 0xaa)
	iopb->scsi_hwstatus = 0;
    if (iopb->scsi_status || iopb->scsi_hwstatus) {
failed:
#ifdef STANDALONE
	if ((iopb->scsi_hwstatus != SELTMO)   && 
	    (iopb->scsi_status != SCSI_CHECK) &&
	    (iopb->scsi_status != SCSI_BUSY)) {
	    /* ONLY if we're not a unix 'polled' cmd, cause we'll let
	     * the asctimeout code handle the unix case */
	    if (IS_R3030)
	        rambo_regs(); /* allways dump cause we use for command phase */
	    else
		dma_regs();
	    scsi_first = 0; /* allow ncr chip reset to happen */
	    _scsi_initp(); /* reset the ncr and issue scsi bus reset */
	}
#endif STANDALONE
	if (iopb->scsi_status) {
	    if (iopb->scsi_status == SCSI_BUSY) {
		DELAY(1048576);
		if (busy++ < 0x100)
		    goto busy_retry;
	    }
	    return(iopb->scsi_status);
	} else
	    return(iopb->scsi_hwstatus);
    }
    return(0);
}
/* Arbitrate for and Select a target, then send the command, all
 * in one fell swoop.
 */ 
_sel_cmd(iopb)
register struct scsi_iopb *iopb;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
    	register struct scsi_unit *un = iopb->scsi_un;
	register u_char *ptr, *ptr1; /* memory address pointer */
	int len, cmd, offset = 0;
	u_int count;

	/* dest_id used for arbitration along with 'my id' in config1 */
        ncr->s_d.dest_id = iopb->scsi_target; wbflush(); /* encoded low 3 bits*/
        ncr->f_o.sync_offset = ASYNC; wbflush(); /* asyncronous only */
	cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd;  /* scsi command byte */
	ptr = &iopb->cmd_blk.cdb_0.cdb_0_cmd; /* pointer to first cmd byte */
	switch (cmd & GROUP_MASK) {
	case CD10BYTE:
		len = 10;
		break;
	case CD12BYTE:
		len = 12;
		break;
	case CD6BYTE:
	default:
		len = 6;
	}
	ncr->count_lo = len; wbflush();
	ncr->count_hi = 0;   wbflush(); /* clean up!!! */
	if (IS_R3030) {
		ptr1 = (u_char*)K2_TO_K1(un->un_buf_64);
		do {
			ptr1[offset++] = *ptr++; /* fill buffer */
		} while (len -= 1);
		rambo->dma_laddr_1 = (u_long)(K2_TO_PHYS(un->un_buf_64));
		wbflush();
		rambo->dma_block_1 = 1; wbflush();
		rambo->dma_mode_1  = CHANNEL_EN; wbflush(); /* start xfer */
	} else { /* Genesis */
		dma->mem_ptr = (u_int)K1_TO_PHYS(ptr); wbflush();
	        if (count = (len & un->un_dmacntmask))
			len += (un->un_dmaalign - count);
		count = len << DMA_BYTE_SHIFT; /* convert and position count */
		count = ~count & 0xffff0000;
		count |=
	     (TO_MEMORYB|NO_CHAIN_ENABLE|NO_DMA_RESET|NO_FLUSH_PIPE|NO_CLR_DMA);
		dma->count_control = count; wbflush();/* start xfer */
	}
	ncr->command = SELECT_NO_ATN|DMA; wbflush();
}
/*
 * handle status and msg-in phases for command completion 
 */
_status_msg(iopb,transfer)
register struct scsi_iopb *iopb;
int transfer;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
    	register hold, seqn, status;
	register int delay;
	u_char msg;

	/* we've got to handle the TRANSFER command interrupt first!
	 */
	if (transfer != NO_XFER) { /* we've got a data xfer pending */
	    delay = POLL_DELAY;
	    while (!(ncr->s_d.status & INTR)) { /* POLL for ncr interrupt */
#ifdef STANDALONE
		switch (iopb->cmd_blk.cdb_0.cdb_0_cmd) {
		case C0_SPACE:
		case C0_REWIND:
		case C0_FORMAT:
		    _scandevs(); /* scan for abort */
		    break;
		}
#endif STANDALONE
		if (!(delay -= 1)) {
	    		cmn_err(CE_CONT,"dkisSM1: timed-out polling for INT\n");
			asc_regs(YES); /* dump ASC register state */
		        if (IS_R3030)
			    rambo_regs();
		        else
			    dma_regs();
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
	    }
	    status = ncr->s_d.status;   /* read status register */
	    hold = ncr->i_t.interrupts; /* read interrupt status register */
	    if (!delay)
	    	cmn_err(CE_CONT,"status= %x int reg= %x\n",status, hold);
	    if (hold != SER_REQ) {/* we should have service required only
				     NOTE that we don't get a CMD CMPLT here */
			cmn_err(CE_CONT,"dkisSM3: int reg s/b SER_REQ only\n");
			goto error;
	    }
	    if (IS_R3030) {
	        /* flush rambo if required */
		if (!(rambo->dma_mode_1 & FIFO_EMPTY))
		    rambo->dma_mode_1  = FLUSH_RAMBO_FIFO; wbflush();
	    }
	} else { /* no data xfer, we go from command phase right to status */
	    delay = 1 * POLL_LOOP_FACTOR;
	    while ((ncr->s_d.status & XFER_PHSE_MSK) != STATUS) {
#ifdef STANDALONE
		switch (iopb->cmd_blk.cdb_0.cdb_0_cmd) {
		case C0_SPACE:
		case C0_REWIND:
		case C0_FORMAT:
		    _scandevs(); /* bounce the LEDS and scan for abort */
		    break;
		}
#endif STANDALONE
		if (!(delay -= 1)) {
	    		cmn_err(CE_CONT,"dkisSM1: timeout polling STATUS\n");
			asc_regs(YES); /* dump ASC register state */
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
		DELAY(POLL_LOOP_DELAY);
	    }
	}
	status = ncr->s_d.status;   /* phase bits are always valid! */
	if ((status & XFER_PHSE_MSK) != STATUS) { /* should have status phase */
		cmn_err(CE_CONT,"dkisSM5: scsi bus not in STATUS phase\n");
		goto error;
	}
	ncr->command = CMD_CMP; wbflush(); /* command complete sequence */
	delay = 1 * POLL_LOOP_FACTOR; /* delay time-out period */
	while (!(ncr->s_d.status & INTR)) { /* POLL for interrupt */
		if (!(delay -= 1)) {
	    		cmn_err(CE_CONT,"dkisSM6: timed-out polling ncr\n");
			asc_regs(YES); /* dump ASC register state */
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
		DELAY(POLL_LOOP_DELAY);
	}
	status = ncr->s_d.status;   /* read status register */
	hold = ncr->i_t.interrupts; /* read interrupt status register */
	if (hold != CMD_CMPLT) { /* we should have command complete only */
		cmn_err(CE_CONT,"dkisSM7: int reg s/b CMD_CMPLT only\n");
		goto error;
	}
	if ((status & XFER_PHSE_MSK) != MSG_IN) {
		cmn_err(CE_CONT,"dkisSM8: scsi bus NOT in MSG_IN phase\n");
		goto error;
	}
	iopb->scsi_status = ncr->fifo; /* grab status byte */
	if (msg = ncr->fifo)           /* grab message byte */
		cmn_err(CE_CONT,"dkisSM9: cmd cmp msg = %x s/b 0\n",msg);
	ncr->command = MSG_OK; wbflush(); /* let ASC drop ack */
	delay = 1 * POLL_LOOP_FACTOR; /* delay time-out period */
	while (!(ncr->s_d.status & INTR)) { /* POLL for interrupt */
		if (!(delay -= 1)) {
	    		cmn_err(CE_CONT,"dkisSM10: timed-out polling\n");
			asc_regs(YES); /* dump ASC register state */
			iopb->scsi_hwstatus = SCSITMO;
			return(-1);
		}
		DELAY(POLL_LOOP_DELAY);
	}
	status = ncr->s_d.status;   /* read status register */
	hold = ncr->i_t.interrupts; /* read interrupt status register */
	if (hold != DISCONECT) { /* we should be disconnected from scsi */
		cmn_err(CE_CONT,"dkisSM11: int reg s/b DISCONECT only\n");
		goto error;
	}
	if ((status & XFER_PHSE_MSK) != BUS_FREE) {
		cmn_err(CE_CONT,"dkisSM12: scsi bus not FREE\n");
		goto error;
	}
	return(0);
error:
	cmn_err(CE_CONT,"INTERRUPT register = %x\n",hold);
	cmn_err(CE_CONT,"STATUS register = %x\n",status);
	asc_regs(NO); /* dump ASC register state */
	if (IS_R3030)
		rambo_regs();
	else
		dma_regs();
	iopb->scsi_hwstatus = PHASE_ERROR;
	return(-1);
}
/*      Transfer data to/from SCSI via rambo
 */
_xfer(iopb, phase, status)
register struct scsi_iopb *iopb;
u_short phase, status;
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);
	register volatile DMA_REG *dma = DMA_ADDR;
	register u_char *ptr, *ptr1;  /* memory address pointer */
	register unsigned len; /* byte counter */
    	register struct scsi_unit *un = iopb->scsi_un;
	register struct scsisge *sgeptr, *sgeptr_nxt;
	register struct scsisge_g *sgeptr_g, *sgeptr_nxt_g;
	short fill_data = 0;   /* rambo fifo fill data */
	int ptm_datain = 0;    /* flag to indicate special action needed */
	u_int delay, offset = 0, count;

	/* is SCSI bus in the required phase? */
	if ((status & XFER_PHSE_MSK) != phase) {
		/* this is a common occurence, as after a SCSI reset we
		 * get a check condition status on any command */
		if ((status & XFER_PHSE_MSK) == STATUS) { /* status phase? */
			/* status and message in phases */
			return(_status_msg(iopb,NO_XFER));
		}
		cmn_err(CE_CONT,"_xfer(): scsi phase is %x s/b %x\n",
			status&XFER_PHSE_MSK, phase);
		iopb->scsi_hwstatus = PHASE_ERROR;
		asc_regs(YES); /* dump ASC register state */
		return(-1);
	}
	/* point to current scatter/gather element (sge)
	 */
	if (IS_R3030) {
		sgeptr = (struct scsisge *)iopb->scsi_extra;
	} else {
		sgeptr_g = (struct scsisge_g *)iopb->scsi_extra;
	}
	if ((iopb->scsi_flags & XFER_MASK) == PTM_XFER) { /* non-mod-64 */
		/* since we can't guarantee alignment on buffers used
		 * we'll use our 64-byte aligned buffer for all PIZAZZ cases */
		if ((phase == DATA_OUT) && IS_R3030) { /* write to scsi */
			len = iopb->scsi_count0; /* byte count */
			ASSERT(len <= un->un_dmaalign);
			ptr = (u_char*)iopb->scsi_bufaddr0;
			ptr = (u_char*)(PHYS_TO_K1(ptr));
			do {
				un->un_buf_64[offset++] = *ptr++;
			} while (len -= 1);
		} else /* read from scsi */
			ptm_datain = 1; /* set flag for later actions */
		if (IS_R3030) {
			rambo->dma_laddr_1 =(u_long)(K2_TO_PHYS(un->un_buf_64));
			wbflush();
		} else { /* Genesis */
			ptr = (u_char*)iopb->scsi_bufaddr0;
			if (IS_KSEG0(ptr))
			    ptr = (u_char*)(K0_TO_PHYS(ptr));
			else if (IS_KSEG1(ptr))
			    ptr = (u_char*)(K1_TO_PHYS(ptr));
			else if (IS_KSEG2(ptr))
			    ptr = (u_char*)(K2_TO_PHYS(ptr));
			dma->mem_ptr = (u_int)ptr;
			wbflush();
		}
		if (IS_R3030) {
			len = iopb->scsi_count0 >> BLOCK_SHIFT;
			if (iopb->scsi_count0 & un->un_dmacntmask) /* mod 64? */
				len++; 	/* round up to '1' block */
		} else /* Genesis */
			len = iopb->scsi_count0;
	} else { /* DMA transfer */
		if (IS_R3030) {
			len = sgeptr->count; /* number of 64 byte blocks */ 
			rambo->dma_laddr_1 = sgeptr->mem_ptr; wbflush();
		}
	}
	if (phase == DATA_IN) { /* scsi read */
		if (IS_R3030)
			rambo->dma_mode_1  = TO_MEMORY|CHANNEL_EN; wbflush();
	} else {
		if (IS_R3030)
			rambo->dma_mode_1  = CHANNEL_EN; wbflush();
	}
	if ((iopb->scsi_flags & XFER_MASK) == PTM_XFER) { /* sub-blk xfer */
		ncr->count_hi  = 0; wbflush();
		ncr->count_lo  = LB(iopb->scsi_count0); wbflush();
	} else {
		ncr->count_hi  = MB(iopb->scsi_count); wbflush();
		ncr->count_lo  = LB(iopb->scsi_count); wbflush();
	}
	ncr->command = TRANSFER|DMA; wbflush();
	if (IS_R3030) {
		rambo->dma_block_1 = len; wbflush(); /* START XFER */
	} else if ((iopb->scsi_flags & XFER_MASK) == PTM_XFER) {
		if (count = len & un->un_dmacntmask)
			/* round up to next higher word count! */
			len += (un->un_dmaalign - count);
		count = len << DMA_BYTE_SHIFT; /* convert and position count */
		count = ~count & 0xffff0000;
		count |=(NO_CHAIN_ENABLE|NO_DMA_RESET|NO_FLUSH_PIPE|NO_CLR_DMA);
		if (phase == DATA_OUT) /* scsi write */
			count |= TO_MEMORYB;
		dma->count_control = count; wbflush(); /* start xfer */
	} else { /* DMA Genesis transfer (chain, chain, chain...) */
		dma->cmd_addr = K1_TO_PHYS(iopb->scsi_extra); wbflush();
	}
	if (IS_R3030) { /* Pizazz only */
	  /* a unix polled command could have multiple scatter-gather
	   * elements. for these cases we have to poll for dma complete
	   * at rambo then restart the next sge, etc.
	   * kernel core dumps use this a lot.
	   */
	  if ((iopb->scsi_flags & XFER_MASK) == DMA_XFER) {
another:
	     sgeptr_nxt = sgeptr + 1; /* point to the next sge */
	     if (sgeptr_nxt->count) { /* do we have another element? */
		delay = POLL_DELAY;
		while (rambo->dma_block_1) { /* POLL rambo blk cnt */
			if ((ncr->s_d.status & INTR)) {
			    if (len = rambo->dma_mode_1 & COUNT_MSK) {
				len = BLOCK_HW_CNT - len;
				do {
				     rambo->dma_fifo_1 = fill_data; wbflush();
				} while (len -= 1);
				DELAY(128); /* delay a bit to allow block dma */
			    }
			    goto shortread;
			}
			DELAY(512); /* allow dma to occur! */
			if (!(delay -= 1)) {
				cmn_err(CE_CONT,
				"scsix1: time-out polling rambo blk count\n");
				asc_regs(YES);/* dump ASC register state */
				rambo_regs(); /* dump DMA CH 1 register state */
				iopb->scsi_hwstatus = SCSITMO;
				return(-1);
			}
		}
		/* this is really necessary! since rambo is too fast!!! */
		if (phase == DATA_OUT) { /* scsi write; may not be done yet */
		   delay = POLL_DELAY;
		   while (!(rambo->dma_mode_1 & FIFO_EMPTY)) { /* fifo empty? */
			DELAY(16);
			if (!(delay -= 1)) {
				cmn_err(CE_CONT,
				"scsix2: time-out polling rambo mode count\n");
				asc_regs(YES);/* dump ASC register state */
				rambo_regs(); /* dump DMA CH 1 register state */
				iopb->scsi_hwstatus = SCSITMO;
				return(-1);
			}
		   }
		}
		sgeptr = sgeptr_nxt; /* advance to next sge element */
		rambo->dma_laddr_1 = sgeptr->mem_ptr; wbflush();
		rambo->dma_block_1 = sgeptr->count;   wbflush();
		goto another; /* continue transfer */
	     }
	  }
	}
shortread:
	if (ptm_datain) { /* need to wait for ncr interrupt for this case */
		delay = POLL_DELAY;
		while (!(ncr->s_d.status & INTR)) { /* POLL for ncr interrupt */
			if (!(delay -= 1)) {
				cmn_err(CE_CONT,
				"scsix3: time-out polling ncr interrupt\n");
				asc_regs(YES); /* dump ASC register state */
				if (IS_R3030)
					rambo_regs();
				else
					dma_regs();
				iopb->scsi_hwstatus = SCSITMO;
				return(-1);
			}
		}
		/* see if there's data in the fifo to be helped along
		 * if so, we need to fill the rambo buffer to 32 half
		 * words and force the 64 byte block transfer
		 */
		if (IS_R3030) {
			if (len = rambo->dma_mode_1 & COUNT_MSK) {/* 1/2-words*/
				len = BLOCK_HW_CNT - len; /* # to force dma */ 
				do {
				      rambo->dma_fifo_1 = fill_data; wbflush();
				} while (len -= 1);
				DELAY(128); /* delay a bit to allow block dma */
			}
			offset = 0;
			len = iopb->scsi_count0;   /* byte count */
			ptr = (u_char*)iopb->scsi_bufaddr0; /* physical addr */
			ptr = (u_char*)(PHYS_TO_K1(ptr));
			ptr1 = (u_char*)(K2_TO_K1(un->un_buf_64));
			/* copy data */
			do {
				*ptr++ = ptr1[offset++];
			} while (len -= 1);
		} else { /* Genesis */
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
		}
	}
	/* for non-ptm-datain wait for the ncr interrupt in the next routine */
	/* status and message in phases */
	return(_status_msg(iopb,iopb->scsi_flags & XFER_MASK));
}
/* Print out NCR register values
 */
asc_regs(flag)
int flag; /* says whether status, sequence, and interrupt regs already read */
{
	register volatile NCR_REG *ncr = (NCR_REG*)MACHDEP(SCSI_REG_BASE);

	cmn_err(CE_CONT,"NCR asc register values:\n");
	cmn_err(CE_CONT,"TC =(%x)(%x) or %d COMMAND = 0x%x FIFO_FLAGS = 0x%x\n",
		ncr->count_hi, ncr->count_lo, TC_ALL, ncr->command,
		ncr->f_o.fifo_flags);
	if (flag) {
		cmn_err(CE_CONT,"STATUS = 0x%x SEQN_STEP = 0x%x ",
			ncr->s_d.status, ncr->s_p.seqn_step);
		/* read last since doing so clears it and the previous 2 regs
		 */
		cmn_err(CE_CONT,"INTERRUPTS = 0x%x\n",ncr->i_t.interrupts);
	}
	cmn_err(CE_CONT,"CONFIG1 = 0x%x CONFIG2 = 0x%x CONFIG3 = 0x%x\n",
		ncr->config1, ncr->config2, ncr->config3);
}

/* Print out RAMBO Channel 1 register values
 */
rambo_regs()
{
	register volatile RAMBO_REG *rambo =(RAMBO_REG*)MACHDEP(RAMBO_REG_BASE);

	cmn_err(CE_CONT,"RAMBO CH1 register values: ");
	/* 0 load memory address register (R/W) */
	/* 100 hardware register (R) */
	cmn_err(CE_CONT,"LADDR_1= %x DIAG_1= %x\n",
		rambo->dma_laddr_1,rambo->dma_diag_1);
	/* 202 fifo (R/W) */
/* don't read the fifo as this will pull a half-word out, which we don't want!
	/* 300 mode register (R/W) */
	/* 402 block count register (R/W) */
	/* 500 current address register (R) */
	cmn_err(CE_CONT,"FIFO_1= %x MODE_1= %x BLOCK_1= %x CADDR_1= %x\n\n",
		rambo->dma_fifo_1,rambo->dma_mode_1,
		rambo->dma_block_1,rambo->dma_caddr_1);
}

/* Print out Genesis dma register values
 */
dma_regs()
{
	register volatile DMA_REG *dma = DMA_ADDR;
	u_int count;

	cmn_err(CE_CONT,"Genesis DMA register values:\n");
	cmn_err(CE_CONT,"Command Address = %x Memory Address = %x\n",
		dma->cmd_addr,dma->mem_ptr);
	cmn_err(CE_CONT,"Control (cnt invalid)= %x ",dma->control&0xffff);
	count = dma->count_control;
	cmn_err(CE_CONT,"Control/Count = %x\n",count);
	count = ~count >> 16;
	cmn_err(CE_CONT,"halfword count = 0x%x or 0x%x bytes\n",count,count<<1);
}
