#ident "$Header: spc_poll.c,v 1.24 90/05/30 08:58:33 hal Exp $"
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * spc_poll.c 	MB87030/AM9516 device drivers (for stand-alone and polled use)
 *		SPC     UDC
 * These routines will be used by stand-alone disk, tape and other drivers.
 * The desire is to keep common low-level routines out of the higher
 * level scsi drivers.
 */
#ifndef STANDALONE
#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "sys/dvh.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/am9516.h"
#include "sys/mb87030.h"
#include "sys/m120scsi.h"
#include "sys/scsi.h"
#else STANDALONE
#include "sys/param.h"
#include "sys/buf.h"
#include "machine/dvh.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "mips/am9516.h"
#include "mips/mb87030.h"
#include "mips/m120scsi.h"
#include "mips/scsi.h"
#endif STANDALONE

#define BUSY_RETRIES	500
#define TC_ALL ((fuji->count_hi<<16)|(fuji->count_mid<<8)|fuji->count_lo)

static _msg();
static int _xfer();

#define AMD_BASE	(*(struct am9516 *)PHYS_TO_K1(AMD_BASE_R2400))
#define AMD_ADDR	(&AMD_BASE)
#define AMD_REG		struct am9516
#define FUJI_BASE	(*(struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400))
#define FUJI_ADDR	(&FUJI_BASE)
#define FUJI_REG	struct mb87030
#define SYSCON		(u_short *)PHYS_TO_K1(SCR)

#ifdef STANDALONE
int scsi_first;
_scsi_init()
{
    register volatile FUJI_REG *fuji = FUJI_ADDR;
    register AMD_REG *amd = AMD_ADDR;
    register zero = 0;

#ifndef SABLE
    if (badaddr(&fuji->scsi_id, sizeof(fuji->scsi_id))) {
	cmn_err(CE_CONT,"fuji scsi chip not addressable at 0x%x\n",
		&fuji->scsi_id);
	return(-1); /* catastrophic failure (panic) */
    }
    if (badaddr(&amd->base, sizeof(amd->base))) {
	cmn_err(CE_CONT,"amd udc chip not addressable at 0x%x\n", &amd->base);
	return(-1); /* catastrophic failure (panic) */
    }
#endif SABLE
    if (!scsi_first) {
       /* reset the SPC (if not already) */
       fuji->control = RESET_DISABLE; wbflush();
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

       /* initialize the UDC
	*/
       amd->ptr = COMMAND1;  wbflush(); /* select ch 1 command reg */
       amd->base = RESET_CMD; wbflush(); /* and reset it */
       DELAY(100);	/* allow ~100us for the reset to take hold */
       amd->ptr = MASTER_MODE; wbflush(); /* select reg */
       if (amd->base) {
	    cmn_err(CE_CONT,"after reset UDC mode was 0x%x it should be 0\n",
						    amd->base);
	    goto bad;
       }
       amd->ptr = MASTER_MODE; wbflush(); /* select reg */
       amd->base = CHIP_ENABLE|CHIP_WAIT_EN|CHIP_NOVEC; wbflush();
       scsi_first++; /* only the first time */	
    }
    return(0);
bad:
    return(1);
}
#endif STANDALONE
scsicmd(iopb)
register struct scsi_iopb *iopb;
{
    register volatile FUJI_REG *fuji = FUJI_ADDR;
    register delay;
    u_char status;	/* scsi status */
    u_char msg;
    int  i, cmd, phase, busy = 0;

    /* wait for scsi bus to become free 
     */
    delay = 2 * POLL_LOOP_FACTOR;
    while (curphase() & BSY) {
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"SCSI bus 'NOT FREE' going into SELECT phase\n");
	    cmn_err(CE_CONT,"SCSI bus =%R\n",
			curphase(),spcps_desc);
	    iopb->scsi_hwstatus = PHASE_ERROR;
	    goto failed;
	}
	DELAY(POLL_LOOP_DELAY);
    }
    /* NOTE: for standalones DISCONNECT/RECONNECT and INTERRUPTS
     * will NOT be allowed. A scsi reset condition detected WILL CAUSE
     * a non-maskable interrupt!!
     * Sooo.. if a target issues a reset, we'll exception out to the 
     * monitor prompt.
     * All interrupt events are VALID in the fuji/amd ints registers for
     * POLLING even though interrupt assertion is NOT enabled.
     * Arbitration is NOT USED.
     *
     * Required steps before SELECT command can be issued:
     */
busy_retry:
    fuji->interupts = 0xff;	wbflush();	/* reset any pending ints */
    fuji->control = ARBIT_EN|PARITY_EN|SEL_NO|RESEL_NO|INTS_NO; wbflush();
    fuji->phase_c = I_O_SEL; wbflush(); /* say we're INITIATOR not TARGET */
    /* set target id for bus */
    fuji->temp = (1 << iopb->scsi_target)|BUSID_IN7; wbflush();
    fuji->count_hi = TSEL_HI; wbflush(); /* these two set SELECT time-out */
    fuji->count_mid = TSEL_LO; wbflush(); /* period */
    fuji->count_lo = TWAIT; wbflush(); 	/* "BUS FREE" delay period (~1250ns) */
    fuji->command = SELECT; wbflush(); 	/* issue the select command */
    /*
     * POLL on selection phase completion 
     */
    delay = 1 * POLL_LOOP_FACTOR;
    while (!fuji->interupts) {
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"TIMED-OUT spinning on selection phase\n");
	    iopb->scsi_hwstatus = SELTMO;
	    goto failed;
	}
	DELAY(POLL_LOOP_DELAY);
    }
    if (!(fuji->interupts & CMD_CMPLT)) { /* selection phase sucessful? */
	if (fuji->interupts & TIME_OUT) {/* selection time-out? */
	    iopb->scsi_hwstatus = SELTMO;
	    fuji->interupts = TIME_OUT; wbflush(); /* reset int */
	} else
	    iopb->scsi_hwstatus = SELTMO;
	goto failed;
    }
    fuji->interupts = CMD_CMPLT; wbflush();   /* reset cmd complete event */

    /* command phase; REMEMBER that dma byte count MUST be modulo 4!!!
     * so that for our 6-byte command packet we'll use PTM
     */
    if (_xfer(iopb, P_CMD))
	goto failed;

    /* data in/out phase, status and msg in phase
     */
    cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd;	/* scsi command byte */
    switch (cmd) {
	case C0_COPY:
	case C0_FORMAT:
	case C0_MODESEL:
	case C0_REASSIGN:
	case C0_SENDDIAG:
	case C0_WRITE:
	case C1_WRITE:
	case C1_WRVERIFY:
	case C1_VERIFY:
	case C1_DAHIGH:
	case C1_DAEQUAL:
	case C1_DALOW:
	    phase = P_DATAOUT;
	    break;
	default:
	    phase = P_DATAIN;
	    break;
    }

    if ((iopb->scsi_flags & XFER_MASK) == NO_XFER) {
	(void)_man_status(iopb); /* stat, msgin phases */
    } else {
	(void)_xfer(iopb, phase);
    }
    if (iopb->scsi_hwstatus == 0xaa)
	iopb->scsi_hwstatus = 0;
    if (iopb->scsi_status || iopb->scsi_hwstatus) {
failed:
	if ((iopb->scsi_hwstatus != SELTMO) && 
	    !(iopb->scsi_status == SCSI_CHECK ||
	      iopb->scsi_status == SCSI_BUSY)) {
	    spc_regs();	/* dump SPC register state */
	    fuji->command = RST_OUT; wbflush(); /* reset the scsi bus */
	    DELAY(300);	/* allow ~300us for targets to see the reset */
	    fuji->command = 0; wbflush();
	    fuji->interupts = 0xff; wbflush();   /* reset any pending ints */
	    if ((iopb->scsi_flags & XFER_MASK) == DMA_XFER)
		udc_regs(); /* dump DMA CH 1 reg state */
	}
	if (iopb->scsi_status) {
	    if (iopb->scsi_status == SCSI_BUSY) {
		DELAY(1000);
		if (busy++ < BUSY_RETRIES)
		    goto busy_retry;
	    }
	    return(iopb->scsi_status);
	} else
	    return(iopb->scsi_hwstatus);
    }
    return(0);
}
_man_status(iopb)
register struct scsi_iopb *iopb;
{
	register volatile FUJI_REG *fuji = FUJI_ADDR;
	register u_int cmd, delay;
	u_char status;

	cmd = iopb->cmd_blk.cdb_0.cdb_0_cmd;
#ifndef SABLE
	/* is SCSI bus in STATUS phase? */
	if (cmd == C0_FORMAT)
	    delay = iopb->scsi_time;
	else
	    delay = POLL_DELAY;

	while ((curphase() & XFER_PHSE_MSK) != (C_D|I_O)) {
#ifdef STANDALONE
	    switch (cmd) {
	    case C0_FORMAT:
		cmn_err(CE_CONT,".");
		DELAY(1050000);
	    case C0_SPACE:
	    case C0_REWIND:
		    _scandevs(); /* bounce the LEDS and scan for abort */
		    break;
	    }
#endif STANDALONE
	    if (!(delay -= 1)) {
		cmn_err(CE_CONT,"_man_status: scsi bus NOT in status phase\n");
		iopb->scsi_hwstatus = PHASE_ERROR;
		return(-1);
	    }
	}
#endif SABLE
	fuji->phase_c = P_STATUS; wbflush(); /* status xfer phase*/
	delay = 1 * POLL_LOOP_FACTOR;
	while (!(curphase() & REQ)) { /* wait REQ assert*/
	    if (!(delay -= 1)) {
		cmn_err(CE_CONT,"_man_status: timed out waiting for REQ\n");
		iopb->scsi_hwstatus = SCSITMO;
		return(-1);
	    }
	    DELAY(POLL_LOOP_DELAY);
	}
	iopb->scsi_status = fuji->temp;	/* read temp and move to iopb */
	fuji->command = SET_ACK;  wbflush();/* assert ack to say we read it */
	delay = 1 * POLL_LOOP_FACTOR;
	while (curphase() & REQ) { /* wait REQ de-assert*/
	    if (!(delay -= 1)) {
		cmn_err(CE_CONT,"_man_status: timed out waiting for ~REQ\n");
		iopb->scsi_hwstatus = SCSITMO;
		return(-1);
	    }
	    DELAY(POLL_LOOP_DELAY);
	}
	fuji->command = RST_ACK;  wbflush();	/* de-assert ACK */
	if (status = _msg(iopb,MSG_IN,0,0)) {	/* cmd cmplt msg should be 0 */
	    cmn_err(CE_CONT,
		"_man_status(): msg for cmd cmplt was 0x%x should be 0\n",
		status);
	    iopb->scsi_hwstatus = PHASE_ERROR;
	    return(-1);
	}
	return(0);
}
/*      Transfer data to/from SCSI
 */
static int
_xfer(iopb, phase)
register struct scsi_iopb *iopb;
register int phase;
{
    register volatile FUJI_REG *fuji = FUJI_ADDR;
    register AMD_REG *amd = AMD_ADDR;
    register u_short *syscon = SYSCON;
    register u_char *ptr;    /* memory address pointer */
    register volatile u_char *d = &fuji->data;   /* fuji data pointer */
    register volatile u_char *s = &fuji->status; /* fuji status pointer */
    register unsigned len;   /* byte counter */
    register u_char mask;
    u_char newphase;
    u_short addr;
    u_int count, delay;

    mask = DFULL;
    if (phase == P_CMD) {	/* command transfer */
	switch (iopb->cmd_blk.cdb_0.cdb_0_cmd & GROUP_MASK) {
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
	ptr = &iopb->cmd_blk.cdb_0.cdb_0_cmd; /* pointer to first cmd byte */
	phase = COMMAND;
    } else {
	if ((iopb->scsi_flags & XFER_MASK) == DMA_XFER) {
	    len = iopb->scsi_count;
	} else {
	    len = iopb->scsi_count0;
	    ptr = (u_char *)iopb->scsi_bufaddr0;
	}
	if (phase == P_DATAOUT) {
	    phase = DATA_OUT;
	} else {
	    phase = DATA_IN;
	    mask = DEMPTY;
	}
    }
    /* is SCSI bus in the required phase? */
    delay = POLL_DELAY;
    while ((newphase = (curphase() & XFER_PHSE_MSK)) != phase) {
	if (newphase == (C_D|I_O)) {
	    if (phase == P_CMD) {
		iopb->scsi_flags &= ~XFER_MASK;		/* say no data phase */
		iopb->scsi_flags |= NO_XFER;
		return(0);			/* will do status next */
	    } else {
		/* status and message in phases */
		return(_man_status(iopb));
	    }
	}
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"_xfer(): scsi bus NOT in correct phase\n");
	    iopb->scsi_hwstatus = PHASE_ERROR;
	    return(-1);
	}
    }
    /* preliminary to issuing the transfer command
     */
    fuji->count_hi  = HB(len); wbflush();
    fuji->count_mid = MB(len); wbflush();
    fuji->count_lo  = LB(len); wbflush();
    fuji->phase_c = phase; wbflush(); /* set transfer phase */
    fuji->xfer_mode = ASYNC; /* asyncronous transfer */
    wbflush();
    count = len;
    if (phase != COMMAND && (iopb->scsi_flags & XFER_MASK) == DMA_XFER) {
	if (phase == DATA_IN) {
		/* set spc direction bit for reading */
		*syscon |= SCR_SCSIHIN; wbflush();
	} else {
		/* set spc direction bit for writing */
		*syscon &= ~SCR_SCSIHIN; wbflush();
	}
	/* now set-up the UDC for chaining */
	amd->ptr = CAR1_HI; wbflush(); /* select ch 1 chain address reg HI */
	addr = (u_short)((K1_TO_PHYS(iopb->scsi_extra) >> 8) & 0xff00);
	addr |= ONEWAIT;
	amd->base = addr; wbflush();
	amd->ptr = CAR1_LO; wbflush(); /* select ch 1 chain address reg LO */
	amd->base = (K1_TO_PHYS(iopb->scsi_extra) & 0xffff); wbflush();
	amd->ptr = COMMAND1; wbflush(); /* select ch 1 command reg */
	amd->base = START_CHAIN1; wbflush();
	fuji->command = TRANSFER|DMA_XFER_MODE; wbflush(); /* dma xfer cmd */
    } else {
	fuji->command = TRANSFER|PRG_XFER; wbflush(); /* program xfer cmd */
	delay = POLL_DELAY;
	while ((*s & (INIT|BUSY|XFER)) != (INIT|BUSY|XFER)) { /* SPC started? */
	    if (!(delay -= 1)) {
		if (phase == DATA_IN) {
		    if (fuji->interupts & SER_REQ) {
			if ((curphase() & XFER_PHSE_MSK)
				== (C_D|I_O)) {
			    fuji->interupts = 0xff; wbflush();
			    /* status and message in phases */
			    return(_man_status(iopb));
			}
		    }
		}
		cmn_err(CE_CONT,"_xfer(): timed out waiting to start xfer\n");
		iopb->scsi_hwstatus = SCSITMO;
		return(-1);
	    }
	}
	do {
	    delay = POLL_DELAY;
	    while (*s & mask) { /* spin while FIFO is full/empty */
		if (!(delay -= 1) ||
		    (fuji->interupts & SER_REQ)) {
		    if ((curphase() & XFER_PHSE_MSK)==(C_D|I_O)) {
			fuji->interupts = 0xff; wbflush();
			/* status and message in phases */
			return(_man_status(iopb));
		    }
		    cmn_err(CE_CONT,"_xfer(): timed out spinning on fifo\n");
		    iopb->scsi_hwstatus = SCSITMO;
		    return(-1);
		}
	    }
	    if (phase == DATA_IN)
		*ptr++ = *d;		/* move a byte to memory */
	    else {
		*d = *ptr++; wbflush();	/* move a byte from memory to SPC */ 
	    }
	} while (len -= 1);
    }
    delay = 1 * POLL_LOOP_FACTOR;
    while (!fuji->interupts) { /* wait for cmd complete int */
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"_xfer(): timed out waiting for any int\n");
	    iopb->scsi_hwstatus = SCSITMO;
	    return(-1);
	}
	DELAY(POLL_LOOP_DELAY);
    }
    if (!(fuji->interupts & CMD_CMPLT)) {
	if (fuji->interupts & SER_REQ) {
	    if ((curphase() & XFER_PHSE_MSK) == (C_D|I_O)) {
		fuji->interupts = 0xff; wbflush();
		/* status and message in phases */
		return(_man_status(iopb));
	    }
	}
	cmn_err(CE_CONT,"_xfer(): got int but it wasn't cmplt\n");
	cmn_err(CE_CONT,"while trying to xfer 0x%x bytes\n",count);
	iopb->scsi_hwstatus = SCSITMO;
	return(-1);
    }
    fuji->interupts = CMD_CMPLT; wbflush();   /* reset any pending ints */
    if (phase != COMMAND)
	return(_man_status(iopb)); /* status and message in phases */
    return(0);
}
/*      Transfer message data to/from MB87030 temp register from/to SCSI
 *	using  manual req/ack handshake, bypassing the am9516.
 */
static
_msg(iopb, phase, msg, flag)
register struct scsi_iopb *iopb;
register u_char phase, msg;
int flag;
{
    register volatile FUJI_REG *fuji = FUJI_ADDR;
    u_int delay;
    u_char *mp;

    if (phase == MSG_OUT)
	    mp = "OUT";
    else
	    mp = "IN";
    /* is SCSI bus in right phase? */
    delay = 1 * POLL_LOOP_FACTOR;
    while ((curphase() & XFER_PHSE_MSK) != phase) {
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"_msg(): MSG_%s phase not asserted\n",mp);
	    iopb->scsi_hwstatus = PHASE_ERROR;
	    return(-1);
	}
	DELAY(POLL_LOOP_DELAY);
    }
    fuji->phase_c = phase; wbflush(); /* set transfer phase */
    if (phase == MSG_OUT)
	fuji->temp = msg; wbflush(); /* message to be sent to TARGET */
    delay = 1 * POLL_LOOP_FACTOR;
    while (!(curphase() & REQ)) { /* wait REQ assert*/
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"_msg(): MSG_%s timed out waiting for REQ\n",mp);
	    iopb->scsi_hwstatus = SCSITMO;
	    return(-1);
	}
	DELAY(POLL_LOOP_DELAY);
    }
    if (phase == MSG_OUT) {
	if (flag)	 /* de-assert ATN now */
	    fuji->command = RST_ATN; wbflush();
    } else
	msg = fuji->temp;
    fuji->command = SET_ACK; wbflush(); /* tell target he's got it */
    delay = 1 * POLL_LOOP_FACTOR;
    while (curphase() & REQ) { /* wait REQ de-assert*/
	if (!(delay -= 1)) {
	    cmn_err(CE_CONT,"_msg(): MSG_%s timed out waiting for ~REQ\n",mp);
	    iopb->scsi_hwstatus = SCSITMO;
	    return(-1);
	}
	DELAY(POLL_LOOP_DELAY);
    }
    fuji->command = RST_ACK;  wbflush();	/* de-assert ack */
    return(phase == MSG_OUT ? 0 : msg);
}
spc_regs()
{
	register FUJI_REG *fuji = FUJI_ADDR;

	cmn_err(CE_CONT,"FUJI spc register values:\n");
	cmn_err(CE_CONT,"BDID = 0x%x ",fuji->scsi_id);
	cmn_err(CE_CONT,"SCTL=%R\n",fuji->control,spcc_desc);
	cmn_err(CE_CONT,"SCMD=%R ",fuji->command,spccmd_desc);
	cmn_err(CE_CONT,"TMOD = 0x%x\n",fuji->xfer_mode);
	cmn_err(CE_CONT,"INTS=%R ",fuji->interupts,spcints_desc);
	cmn_err(CE_CONT,"PSNS=%R\n",fuji->phse_diag.phase_s,spcps_desc);
	cmn_err(CE_CONT,"SSTS=%R ",fuji->status,spcst_desc);
	cmn_err(CE_CONT,"SERR=%R\n",fuji->err_stat,spcerr_desc);
	cmn_err(CE_CONT,"PCTL=%R\n",fuji->phase_c,spcpc_desc);
	cmn_err(CE_CONT,"TC =(%x)(%x)(%x) or %d ",fuji->count_hi,
				fuji->count_mid,fuji->count_lo,TC_ALL);
	cmn_err(CE_CONT,"MBC= 0x%x\n",fuji->byte_cnt);
}
/* Print out udc Channel 1 register values
 */
udc_regs()
{
	register AMD_REG *amd = AMD_ADDR;
	register u_short *syscon = SYSCON;
	register unsigned addr;	
	register unsigned car;	

	*syscon &= ~SCR_SLOWUDCEN; wbflush(); /* udc 'fast' read */
	cmn_err(CE_CONT,"\nAMD udc CH1 register values:\n");
	amd->ptr = MASTER_MODE; wbflush();
	cmn_err(CE_CONT,"MM=%R ",amd->base,udcmm_desc);

	amd->ptr = CARA1_HI; wbflush();
	car = amd->base;
	cmn_err(CE_CONT,"CARA1HI=%R\n",car,udc_car_desc);
	addr = ((car << 8) & 0xff0000);
	amd->ptr = CARA1_LO; wbflush();
	addr |= amd->base;
	if (car & UDC_A24_P)		
		addr |= ADDR_24;		
	if (!(car & UDC_A25_P))		
		addr |= ADDR_25;		
	cmn_err(CE_CONT,"A 'scsi write' source addr= 0x%x ",addr);

	amd->ptr = CARB1_HI; wbflush();
	car = amd->base;
	cmn_err(CE_CONT,"CARB1HI=%R\n",car,udc_car_desc);
	addr = ((car << 8) & 0xff0000);
	amd->ptr = CARB1_LO; wbflush();
	addr |= amd->base;
	if (car & UDC_A24_P)		
		addr |= ADDR_24;		
	if (!(car & UDC_A25_P))		
		addr |= ADDR_25;		
	cmn_err(CE_CONT,"B 'scsi read' destination addr= 0x%x ",addr);

	amd->ptr = CAR1_HI; wbflush();
	car = amd->base;
	cmn_err(CE_CONT,"CHAINHI=%R\n",car,udc_car_desc);
	addr = ((car << 8) & 0xff0000);
	amd->ptr = CAR1_LO; wbflush();
	addr |= amd->base;
	cmn_err(CE_CONT,"chain addr= 0x%x ",addr);

	amd->ptr = COC1; wbflush();
	cmn_err(CE_CONT,"COUNT= 0x%x\n",amd->base);
	amd->ptr = STATUS1; wbflush();
	cmn_err(CE_CONT,"STATUS=%R ",amd->base,udc_stat_desc);

	*syscon |= SCR_SLOWUDCEN; wbflush(); /* udc 'slow' read */

	amd->ptr = CMR1_HI; wbflush();
	cmn_err(CE_CONT,"MODEHI=%R\n",amd->base,udc_modehi_desc);
	amd->ptr = CMR1_LO; wbflush();
	cmn_err(CE_CONT,"MODELO=%R\n",amd->base,udc_modelo_desc);

	*syscon &= ~SCR_SLOWUDCEN; wbflush(); /* udc 'fast' read */
}

curphase()
{
    register volatile FUJI_REG *fuji = FUJI_ADDR;
    register u_char phase1, phase2;

    while ((phase1 = fuji->phse_diag.phase_s) !=
	    (phase2 = fuji->phse_diag.phase_s)) ;
    return((int)phase1);
}
