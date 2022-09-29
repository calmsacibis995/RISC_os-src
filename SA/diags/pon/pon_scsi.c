#ident "$Header: pon_scsi.c,v 1.14.1.1 90/07/18 14:32:54 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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

#define	NOREGS

#include "sys/types.h"
#include "sys/param.h"
#include "sys/buf.h"
#include "mips/cpu.h"
#include "mips/cp0.h"
#include "mips/cpu_board.h"
#include "mips/mb87030.h"
#include "mips/m120scsi.h"
#include "mips/scsi.h"
#include "pon.h"

#undef	CMD_MASK
#define	CMD_MASK		0xfd		/* usable bits of the SPC command register */
#define	PHASC_MASK		0x87		/* usable bits of the SPC phase control register */
#define	XMOD_MASK		0xfc		/* usable bits of the SPC transfer mode register */

extern char success[], failure[], skipped[], crlf[];


/*
 * Checks all writable/readable SPC registers.  Nothing else.
 */
Pon_ScsiSlave()

{
	register volatile struct mb87030 *spc;
	register int error;
	register u_char expect;
	register u_char temp;
	register u_int i;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);

	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_SCSISLAVE);
	pon_puts("SCSI Slave Test...");

#ifndef	SABLE
	SetSR(GetSR() & SR_BEV);		/* disable interrupts */
	error = 0;

	SetSCR(SCR_RSTSCSI);
	FlushWB();

	/*
	 * Initialize all writable/readable registers.
	 */
	spc->command = 0;
	FlushWB();
	spc->phase_c = 0;
	FlushWB();
	spc->xfer_mode = 0;
	FlushWB();
	spc->count_hi = 0;
	FlushWB();
	spc->count_mid = 0;
	FlushWB();
	spc->count_lo = 0;
	FlushWB();
	spc->control = 0;
	FlushWB();

	/*
	 * Check out the SCSI Bus ID register.
	 */
	for (i = 0; i < 8; i++) {
		spc->scsi_id = i;
		FlushWB();
		temp = spc->scsi_id;		/* let conflict handle the flushing */
		if (temp != (1 << i)) {
#ifdef	DEBUG
			pon_puts("SPC BID: expect ");
			pon_puthex(1 << i);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}
	}

	/*
	 * Check out command register.  Keeps reset on until the end of test.
	 */
	spc->command = RST_OUT;
	FlushWB();
	for (i = 0; i < 60000; i++) {
		;
	}

	temp = spc->command;
	if (temp != RST_OUT) {
#ifdef	DEBUG
		pon_puts("SPC CMD 1: expect ");
		pon_puthex(RST_OUT);
		pon_puts(" actual ");
		pon_puthex(temp);
		pon_puts(crlf);
#endif	DEBUG
		error++;
		goto done;
	}

	spc->command = 0;
	FlushWB();
	temp = spc->command;
	if (temp != 0) {
#ifdef	DEBUG
		pon_puts("SPC CMD 2: expect ");
		pon_puthex(0);
		pon_puts(" actual ");
		pon_puthex(temp);
		pon_puts(crlf);
#endif	DEBUG
		error++;
		goto done;
	}

	expect = 0x55 | RST_OUT;
	for (i = 0; i < 2; i++) {
		expect &= CMD_MASK;
		spc->command = expect;
		FlushWB();
		temp = spc->command;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC CMD 3: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}

		expect = (~expect | RST_OUT) & 0xff;
	}

	spc->command = RST_OUT;			/* keep reset on until end of test */
	FlushWB();

	/*
	 * Check out the phase control register.
	 */
	expect = 0x55;
	for (i = 0; i < 2; i++) {
		expect &= PHASC_MASK;
		spc->phase_c = expect;
		FlushWB();
		temp = spc->phase_c;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC PHASC: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			spc->xfer_mode = 0;
			goto done;
		}

		expect = ~expect & 0xff;
	}

	/*
	 * Check out the transfer mode register.
	 */
	expect = 0x55;
	for (i = 0; i < 2; i++) {
		expect &= XMOD_MASK;
		spc->xfer_mode = expect;
		FlushWB();
		temp = spc->xfer_mode;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC XMOD: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			spc->xfer_mode = 0;
			goto done;
		}

		expect = (~expect | RST_OUT) & 0xff;
	}

	/*
	 * Check out the transfer count registers.
	 */
	expect = 0x55;
	for (i = 0; i < 2; i++) {
		spc->count_hi = expect;
		FlushWB();
		spc->count_mid = expect;
		FlushWB();
		spc->count_lo = expect;
		FlushWB();
		spc->control = expect;
		FlushWB();

		temp = spc->count_hi;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC CNT HI: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}

		temp = spc->count_mid;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC CNT MID: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}

		temp = spc->count_lo;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC CNT LO: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}

		temp = spc->control;
		if (temp != expect) {
#ifdef	DEBUG
			pon_puts("SPC CTRL: expect ");
			pon_puthex(expect);
			pon_puts(" actual ");
			pon_puthex(temp);
			pon_puts(crlf);
#endif	DEBUG
			error++;
			goto done;
		}

		expect = ~expect & 0xff;
	}

done:
	spc->command = 0;
	FlushWB();
	spc->phase_c = 0;
	FlushWB();
	spc->xfer_mode = 0;
	FlushWB();
	spc->count_hi = 0;
	FlushWB();
	spc->count_mid = 0;
	FlushWB();
	spc->count_lo = 0;
	FlushWB();
	spc->control = 0;
	FlushWB();

	SetSCR(0);
	FlushWB();
	DELAY(131072);

#ifdef	DEBUG_C
	printf("SCSI Slave Test Errors: %d\n", error);
#endif	DEBUG_C
	if (error) {
		pon_puts(failure);
		FastFlash(PON_SCSISLAVE);
		pon_set_leds(PON_SCSISLAVE);
		SetDepend(PON_FAULT_SCSI);
		return(FAIL);
	}

#endif
	pon_puts(success);
	return(PASS);
}


struct all {
	struct scsi_iopb iopb;
	struct scsisge scsisge;
	SCSI_INQUIRY un_inquiry;
	SCSI_EXT_SENSE un_sense;
};

static struct all *all = (struct all *)PHYS_TO_K1(PON_SCRATCHMEM);

/*
 * Checks for all SCSI Devices.
 */
Pon_ScsiMaster()

{
	struct scsi_iopb *ip;
	SCSI_INQUIRY *inq;
	SCSI_EXT_SENSE *sense;
	register u_int blk;
	register u_int error;
	register u_int i;
	register u_int id;
	register u_int lun;

	ip = &all->iopb;
	inq = &all->un_inquiry;
	sense = &all->un_sense;
	if ((machine_type != BRDTYPE_R2400) && (machine_type != BRDTYPE_M180)){
		return(PASS);
	}

	pon_set_leds(PON_SCSIMASTER);
	pon_puts("SCSI Master Test...");

#ifndef	SABLE

	if (GetDepend() & (PON_FAULT_IMR | PON_FAULT_MEM | PON_FAULT_SCR | PON_FAULT_SCSI)) {
		pon_puts(skipped);
		goto norun;
	}

	SetIMR(INT_SCSI);			/* enable just SPC interrupts */
	SetSR(((GetSR() & SR_BEV) & ~(SR_IEC | SR_IMASK)) | SR_IBIT3);
						/* enable IMR interrupt for polling */

	bzero(all, sizeof(struct all));
	if (ScsiInit() == FAIL) {		/* initialize the SPC and UDC */
#ifdef	DEBUG_C
		printf("Failed SCSI initialization\n");
#endif	DEBUG_C
		return(FAIL);
	}

	error = 0;

	/*
	 * Check all SCSI Device ID's and LUN's.
	 */ 
	for (id = 0; id < 7; id++) {
	    for (lun = 0; lun < 8; lun++) {
#ifdef	DEBUG_C
		printf("ID %d, LUN %d\n", id, lun);
#endif	DEBUG_C
		ScsiCmd(id, lun, C0_TESTRDY, 0, 0, 0);

		/*
		 * Expect a check condition here, but if there isn't one,
		 * igonre it.  Not all devices may return
		 * a check condition after reset.
		 */
		if (ip->scsi_hwstatus) {
		    /*
		     * A Selection Timeout status returned means there's
		     * no device out there at the pariticular Device
		     * ID.
		     */
		    if (ip->scsi_hwstatus == SELTMO) {
#ifdef	DEBUG
			pon_puthex(id);
			pon_puts("No device responding\r\n");
#endif	DEBUG
			break;
		    }
		    else {
			error++;
#ifdef	DEBUG_C
			printf("SCSI error: %x\n", ip->scsi_hwstatus);
#endif	DEBUG_C
			break;
		    }
		}
		else if (ip->scsi_status & SCSI_CHECK) {
		    bzero(sense, sizeof(SCSI_EXT_SENSE));
		    error += ScsiCmd(id, lun, C0_REQSENSE, sense, 
				     0, sizeof(SCSI_EXT_SENSE));
		    if (error) {
#ifdef	DEBUG
			    pon_puts("Failed Request Sense\r\n");
#endif	DEBUG
			    break;
		    }

		    /*
		     * If not Unit Attention, check if Illegal Request.
		     * Illegal Request is returned for an LUN
		     * that is out of range.
		     */
		    if (sense->key != UNIT_ATN) {
#ifdef	DEBUG_C
			printf("Sense data was: %x\n", sense->key);
#endif	DEBUG_C
			if (sense->key != ILL_REQ) {
			    error++;
			}
			break;
		    }
		}

		/*
		 * Find out what type of device is at this ID and LUN.
		 * If it's a disk, then test it.  Can't do
		 * anything with devices like tape.
		 */
		bzero(inq, sizeof(SCSI_INQUIRY));
		error += ScsiCmd(id,lun,C0_INQUIRY,inq,0,sizeof(SCSI_INQUIRY));
		if (error) {
#ifdef	DEBUG
		    pon_puts("Failed Inquiry\r\n");
#endif	DEBUG
		    break;
		}

#ifdef	DEBUG_C
		printf("Device type: %x\n", inq->device_type);
#endif	DEBUG_C
	    }
	}

	SetIMR(0);
	SetSR(GetSR() & SR_BEV);
	ScsiInit();

	if (error) {
		pon_puts(failure);
		FastFlash(PON_SCSIMASTER);
		pon_set_leds(PON_SCSIMASTER);

norun:
		SetDepend(PON_FAULT_SCSI);
		return(FAIL);
	}

#endif
	pon_puts(success);
	return(PASS);
}


#define DEBUG_CRPH
#define TC_ALL	((spc->count_hi << 16) | (spc->count_mid << 8) | spc->count_lo)

#define	WAITLOOPS		1000000		/* delay */
#define CMDSIZE			6


static ScsiInit()

{
	register volatile struct mb87030 *spc;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	if (badaddr(&spc->scsi_id, sizeof(spc->scsi_id))) {
#ifdef	DEBUG_C
		printf("ScsiInit: SPC not addressable at %x\n", &spc->scsi_id);
#endif	DEBUG_C
		return(FAIL);
	}

	spc->control = 0x80; 
	FlushWB();				/* reset the SPC (if not already) */

	/*
	 * Initialize the SPC (scsi protocol controller).
	 */ 
	spc->scsi_id = BUSID_OUT7; 
	FlushWB();
	spc->command = 0; 
	FlushWB();
	spc->xfer_mode = 0; 
	FlushWB();
	spc->phase_c = 0; 
	FlushWB();
	spc->count_hi = 0; 
	FlushWB();
	spc->count_mid = 0; 
	FlushWB();
	spc->count_lo = 0; 
	FlushWB();
	spc->temp = 0; 
	FlushWB();
	spc->phse_diag.diag_c = 0; 
	FlushWB();
	spc->control &= ~RESET_DISABLE; 
	FlushWB();				/* SPC out of reset */
	DELAY(128);				/* allow ~100us for things to settle down */
	spc->command = RST_OUT; 
	FlushWB();				/* reset the scsi bus */;
	DELAY(512);				/* allow ~500us for targets to see the reset */
	spc->command = 0; 
	FlushWB();

	return(PASS);
}


static ScsiCmd(id, lun, cmd, addr, blk, cnt)

u_char id;
u_char lun;
u_char cmd;
u_int addr;
u_int blk;
u_int cnt;

{
	register struct scsi_iopb *iopbp = &all->iopb;
	register struct scsisge *sgp;
	register u_int hold_addr;
	u_char *cp;
	u_int status;

#ifdef	DEBUG_C
	printf("ScsiCmd: %x %x %x %x %x %x\n", id, lun, cmd, addr, blk, cnt);
#endif	DEBUG_C

	cp = (u_char *)&iopbp->cmd_blk;
	cp[0]=cp[1]=cp[2]=cp[3]=cp[4]=cp[5]=cp[6]=cp[7]=cp[8]=cp[9]=0;
	iopbp->scsi_target = id;
	iopbp->scsi_lun = lun;
	iopbp->scsi_status = 0;
	iopbp->scsi_hwstatus = 0;

	sgp = &all->scsisge;
	iopbp->scsi_extra = (u_int)sgp;
	if (addr) {				/* DMA transfer */
		hold_addr = K1_TO_PHYS(addr);

		sgp->mem_ptr[0] = HS(addr);
		sgp->mem_ptr[1] = LS(addr);
		if (cnt) {
			sgp->count = cnt / sizeof(short);
			iopbp->scsi_count = cnt;
			iopbp->scsi_flags = PTM_XFER;
			/* DMA is setup even though using PTM mode */
		}
		else {
#ifdef	DEBUG
			pon_puts("ScsiCmd: DMA with no count\r\n");
#endif	DEBUG
			return(FAIL);
		}

		iopbp->scsi_bufaddr = PHYS_TO_K1(hold_addr);
		sgp->next_blk[0] = 0;
		sgp->next_blk[1] = 0;
	}
	else {
		iopbp->scsi_flags = NO_XFER;	/* no data in/out phase */
	}

	/*
	 * SCSI Command Descriptor Bytes in the IOPB
	 */
	iopbp->cmd_blk.cdb_0.cdb_0_cmd = cmd;	/* SCSI Command */
	switch (cmd) {
	case C0_INQUIRY:
	case C0_REQSENSE:
		iopbp->cmd_blk.cdb_0.cdb_0_lun = iopbp->scsi_lun;
		iopbp->cmd_blk.cdb_0.cdb_0_len = cnt;
		break;

	case C0_REZERO:
	case C0_TESTRDY:
		iopbp->cmd_blk.cdb_0.cdb_0_lun = iopbp->scsi_lun;
		break;

	default:
#ifdef	DEBUG_C
		printf("Unknown command %x\n", cmd);
#endif	DEBUG_C
		return(FAIL);
	}

#ifdef	DEBUG_C
	printf("%x %x %x %x %x %x %x %x %x %x %x %x\n",
	    iopbp->scsi_count, iopbp->scsi_flags,
	    cp[0],cp[1],cp[2],cp[3],cp[4],cp[5],cp[6],cp[7],cp[8],cp[9]);
#endif	DEBUG_C

	status = ScsiStart(iopbp);
#ifdef	DEBUG_C
	printf("ScsiCmd status: %x %x %x %x\n", iopbp->cmd_blk.cdb_0.cdb_0_cmd,
		status, iopbp->scsi_status, iopbp->scsi_hwstatus);
#endif	DEBUG_C
	if ((status == PASS) || (status == SCSI_CHECK)) {
		return(PASS);
	}
	else {
		return(FAIL);
	}
}


static ScsiStart(iopbp)

register struct scsi_iopb *iopbp;

{
	register volatile struct mb87030 *spc;
	register u_int delay;
	u_char msg;
	u_char status;
	int cmd;
	int phase;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	/*
	 * Wait for SCSI Bus to become free.
	 */
	delay = WAITLOOPS;
	while (spc->phse_diag.phase_s & BSY) {
	    if (!(delay -= 1)) {
#ifdef	DEBUG_C
printf("ScsiStart: SCSI Bus NOT FREE going into SELECT phase, phase was %x\n", 
     spc->phse_diag.phase_s);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = PHASE_ERROR;
		goto failed;
	    }
	}

	spc->interupts = 0xff;			/* reset any pending interrupts */
	FlushWB();
	spc->control = ARBIT_NO | PARITY_EN | SEL_NO | RESEL_NO | INTS_EN;
	FlushWB();
	spc->phase_c = I_O_SEL; 		/* INITIATOR not TARGET */
	FlushWB();
	spc->temp = 1 << iopbp->scsi_target;	/* set target ID for bus */
	FlushWB();
	spc->count_hi = TSEL_HI;		/* these two set SELECT time-out period */
	FlushWB();
	spc->count_mid = TSEL_LO;
	FlushWB();
	spc->count_lo = TWAIT;			/* "BUS FREE" delay period (~1250ns) */
	FlushWB();
	spc->command = SELECT;			/* issue the select command */
	FlushWB();

	/*
	 * POLL on selection phase completion.
	 */
	delay = WAITLOOPS;	/* delay should be > select time-out period */
	while (!(GetCause() & SR_IBIT3)) {
	    if (!(delay -= 1)) {
#ifdef	DEBUG
		pon_puts("ScsiStart: Timeout spinning on selection phase\r\n");
#endif	DEBUG
		iopbp->scsi_hwstatus = SELTMO;
		goto failed;
	    }
	}

	if (!(spc->interupts & CMD_CMPLT)) {	/* selection phase sucessful? */
		if (spc->interupts & TIME_OUT) {/* selection time-out? */
			iopbp->scsi_hwstatus = SELTMO;
			spc->interupts = TIME_OUT;
						/* reset interrupts */
		}
		else {
			iopbp->scsi_hwstatus = SELTMO;
		}

		goto failed;
	}

	spc->interupts = CMD_CMPLT;		/* reset command complete event */
	FlushWB();

	/*
	 * DMA byte count MUST be modulo 4 so for 6-byte command 
	 * packet use PTM mode.
	 */
	if (PTM_Out(iopbp, P_CMD)) {
		goto failed;
	}

	/*
	 * Data-in/out phase, status and message-in phase.
	 */
	cmd = iopbp->cmd_blk.cdb_0.cdb_0_cmd;
	phase = P_DATAIN;

	switch (iopbp->scsi_flags & XFER_MASK) {
	case MAN_XFER:				/* manual REQ/ACK transfer */
		if (phase == P_DATAIN) {
			Man_In(iopbp, phase);
		}
		else {
			Man_Out(iopbp, phase);
		}
		break;

	case PTM_XFER:				/* program hardware transfer mode */
		if (phase == P_DATAIN) {
			PTM_In(iopbp, phase);
		}
		else {
			PTM_Out(iopbp, phase);
		}
		break;

	case NO_XFER:				/* no data-in/out phase required */
		Man_Status(iopbp);
		break;
	}

	if (iopbp->scsi_hwstatus == 0xaa) {
		iopbp->scsi_hwstatus = 0;
	}

	if (iopbp->scsi_status || iopbp->scsi_hwstatus) {
failed:
	    if ((iopbp->scsi_hwstatus != SELTMO) && 
		(iopbp->scsi_status != CHECK_CONDITION)) {
		    spc->interupts = 0xff;	/* reset any pending interrupts */
		    FlushWB();
	    }

	    if (iopbp->scsi_status) {
		    return(iopbp->scsi_status);
	    } else {
		    return(iopbp->scsi_hwstatus);
	    }
	}

	return(PASS);
}


/*
 * Transfer data from SCSI to memory via the SPC temp register using 
 * manual req/ack handshake, bypassing the AMD 9516.
 */
static Man_In(iopbp, phase)

register struct scsi_iopb *iopbp;
int phase;

{
	register volatile struct mb87030 *spc;
	register volatile u_char *c;
	register volatile u_char *d;
	register u_char *ptr; 			/* memory pointer */
	register u_int len;			/* byte counter */
	u_int delay;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	c = &spc->command;
	d = &spc->temp;
	ptr = (u_char *)iopbp->scsi_bufaddr;
	len = iopbp->scsi_count;

	/*
	 * Wait for SCSI Bus to be in data-in phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != I_O) {
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_In: Scsi Bus NOT in data-in phase\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = PHASE_ERROR;
			return(FAIL);
		}
	}

	spc->phase_c = phase; 
	FlushWB();				/* set transfer phase */
	do {
		delay = WAITLOOPS;
		while (!(spc->phse_diag.phase_s & REQ)) {
						/* wait REQ assert */
			if (!(delay -= 1)) {
				iopbp->scsi_hwstatus = SCSITMO;
				return(FAIL);
			}
		}

		*ptr++ = *d;			/* read temp and move byte to memory */
		*c = SET_ACK;  
		FlushWB();			/* assert ACK */
		delay = WAITLOOPS;
		while (spc->phse_diag.phase_s & REQ) {
						/* wait REQ de-assert */
			if (!(delay -= 1)) {
				iopbp->scsi_hwstatus = SCSITMO;
				return(FAIL);
			}
		}

		*c = RST_ACK;  
		FlushWB();			/* de-assert ACK */
	} while (len -= 1);

	if (phase == P_DATAIN) {
		Man_Status(iopbp);		/* status and message-in phases */
	}

	return(PASS);
}


/*
 * Transfer data from memory to SCSI via the SPC temp register using 
 * manual req/ack handshake, bypassing the AMD 9516.
 */
static Man_Out(iopbp, phase)

register struct scsi_iopb *iopbp;
int phase;

{
	register volatile struct mb87030 *spc;
	register volatile u_char *c;
	register volatile u_char *d;
	register u_char *ptr;			/* memory pointer */
	register u_int len;			/* byte counter */
	u_int delay;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	c = &spc->command;
	d = &spc->temp;
	ptr = (u_char *)iopbp->scsi_bufaddr;
	len = iopbp->scsi_count;

	/*
	 * Wait for SCSI Bus to be in data-out phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != ~I_O) {
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_Out: SCSI Bus NOT in data-out phase\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = PHASE_ERROR;
			return(FAIL);
		}
	}

	spc->phase_c = phase; 
	FlushWB();				/* set transfer phase */
	do {
		delay = WAITLOOPS;
		while (!(spc->phse_diag.phase_s & REQ)) {
						/* wait REQ assert*/
			if (!(delay -= 1)) {
				iopbp->scsi_hwstatus = SCSITMO;
				return(FAIL);
			}
		}

		*d = *ptr++; 
		FlushWB(); 			/* memory data to temp */
		*c = SET_ACK;  
		FlushWB();			/* assert ACK */
		delay = WAITLOOPS;
		while (spc->phse_diag.phase_s & REQ) {
						/* wait REQ de-assert*/
			if (!(delay -= 1)) {
				iopbp->scsi_hwstatus = SCSITMO;
				return(FAIL);
			}
		}

		*c = RST_ACK;  
		FlushWB();			/* de-assert ACK */
	} while (len -= 1);

	if (phase == P_DATAIN) {		/* no status, message-in phases  */
						/* if command transfer */
		Man_Status(iopbp);
	}

	return(PASS);
}


static Man_Status(iopbp)

register struct scsi_iopb *iopbp;

{
	register volatile struct mb87030 *spc;
	register volatile u_char *c;
	register volatile u_char *d;
	u_char status;
	u_int delay;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	c = &spc->command;
	d = &spc->temp;

	/*
	 * Wait for SCSI Bus to be in status phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != (C_D | I_O)) {
		if (!(delay -= 1)) {
#ifdef	DEBUG
		    pon_puts("Man_Status: SCSI Bus NOT in status phase\r\n");
#endif	DEBUG
		    iopbp->scsi_hwstatus = PHASE_ERROR;
		    return(FAIL);
		}
	}

	spc->phase_c = P_STATUS; 
	FlushWB();				/* status transfer phase*/
	delay = WAITLOOPS;
	while (!(spc->phse_diag.phase_s & REQ)) {
						/* wait REQ assert*/
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_Status: Timeout waiting for REQ\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	iopbp->scsi_status = *d;		/* read temp and move to iopb */
	*c = SET_ACK;  
	FlushWB();				/* assert ACK */
	delay = WAITLOOPS;
	while (spc->phse_diag.phase_s & REQ) {	/* wait REQ de-assert*/
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_Status: Timeout waiting for ~REQ\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	*c = RST_ACK;  
	FlushWB();				/* de-assert ACK */

	/*
	 * Wait for SCSI Bus to be in message-in phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != (MSG | C_D | I_O)) {
	    if (!(delay -= 1)) {
#ifdef	DEBUG
		pon_puts("Man_Status: SCSI Bus NOT in message-in phase\r\n");
#endif	DEBUG
		iopbp->scsi_hwstatus = PHASE_ERROR;
		return(FAIL);
	    }
	}

	spc->phase_c = P_MSGIN; 
	FlushWB();				/* message in transfer phase */
	delay = WAITLOOPS;
	while (!(spc->phse_diag.phase_s & REQ)) {
						/* wait REQ assert */
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_Status: Timeout waiting for REQ\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	status = *d;	 			/* read temp */
	*c = SET_ACK;  
	FlushWB();				/* assert ACK */
	delay = WAITLOOPS;
	while (spc->phse_diag.phase_s & REQ) {	/* wait REQ de-assert*/
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("Man_Status: Timeout waiting for ~REQ\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	*c = RST_ACK;  
	FlushWB();				/* de-assert ACK */
	if (status) {				/* command complete message should be 0 */
#ifdef	DEBUG_C
printf("Man_Status: Message-in for command complete was %x, should be 0\n", 
status);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = PHASE_ERROR;
		return(FAIL);
	}

	return(PASS);
}


/*
 * Transfer data from SCSI to memory via the SPC data register using 
 * "progammed transfer mode", bypassing the AMD 9516.
 */
static PTM_In(iopbp, phase)

register struct scsi_iopb *iopbp;
int phase;

{
	register volatile struct mb87030 *spc;
	register volatile u_char *d;
	register volatile u_char *s;
	register u_char *ptr;			/* memory address pointer */
	register u_int len;			/* byte counter */
	u_int delay;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	d = &spc->data;
	s = &spc->status;

	/*
	 * Wait for SCSI Bus to be in data-in phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != I_O) {
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("PTM_In: SCSI Bus NOT in data-in phase\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = PHASE_ERROR;
			return;
		}
	}

	/*
	 * Preliminary to issuing the transfer command.
	 */
	spc->count_hi = HB(iopbp->scsi_count); 
	FlushWB();
	spc->count_mid = MB(iopbp->scsi_count); 
	FlushWB();
	spc->count_lo = LB(iopbp->scsi_count); 
	FlushWB();
	len = iopbp->scsi_count;
	ptr = (u_char *)iopbp->scsi_bufaddr;
	spc->phase_c = phase; 
	FlushWB();				/* set transfer phase */
	spc->xfer_mode = ASYNC; 
	FlushWB();
	spc->command = TRANSFER | PRG_XFER;/* issue program transfer command */
	delay = WAITLOOPS;
	while ((*s & (INIT | BUSY | XFER)) != (INIT | BUSY | XFER)) {
						/* wait for SPC transfer to start */
		if (!(delay -= 1)) {
			if (spc->interupts & SER_REQ) {
				if ((spc->phse_diag.phase_s & XFER_PHSE_MSK) 
						== (C_D|I_O)) {
					spc->interupts = 0xff;
					FlushWB();
					Man_Status(iopbp);
					return;
				}
			}

#ifdef	DEBUG
		    pon_puts("PTM_In: Timeout waiting to start transfer\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return;
		}
	}

	do {
		delay = WAITLOOPS;
		while (*s & DEMPTY) {		/* spin if nothing to read */
			if (!(delay -= 1) || (spc->interupts & SER_REQ)) {
				if ((spc->phse_diag.phase_s & XFER_PHSE_MSK) == (C_D|I_O)) {
					spc->interupts = 0xff;
					FlushWB();
					Man_Status(iopbp);
					return;
				}

#ifdef	DEBUG
			    pon_puts("PTM_In: Timeout waiting for data\r\n");
#endif	DEBUG
				iopbp->scsi_hwstatus = SCSITMO;
				return;
			}
		}

		*ptr++ = *d;			/* move a byte to memory */

	} while (len -= 1);

	delay = WAITLOOPS;
	while (!(GetCause() & SR_IBIT3)) {	/* wait for cmd complete interrupt */
		if (!(delay -= 1)) {
#ifdef	DEBUG
		    pon_puts("PTM_In: Timeout waiting for any interrupt\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return;
		}
	}

	if (!(spc->interupts & CMD_CMPLT)) {
		if (spc->interupts & SER_REQ) {
			if ((spc->phse_diag.phase_s & XFER_PHSE_MSK) 
					== (C_D | I_O)) {
				spc->interupts = 0xff;
				FlushWB();
				Man_Status(iopbp);
				return;
			}
		}

#ifdef	DEBUG_C
printf("PTM_In: Got interrupt but it wasn't command complete while transferring %x bytes\n", iopbp->scsi_count);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = SCSITMO;
		return;
	}

	spc->interupts = CMD_CMPLT;		/* reset any pending ints */
	if (*s != XFER_CMP_ST) {		/* check the transfer completion status */
#ifdef	DEBUG
		pon_puts("PTM_In: Timeout waiting for transfer completion status\r\n");
#endif	DEBUG
#ifdef	DEBUG_C
		printf("PTM_In: Status should be %x, was %x\n", XFER_CMP_ST, *s);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = SCSITMO;
		return;
	}

	if (phase == P_DATAIN) {
		Man_Status(iopbp);
	}

	return;
}


/*
 * Transfer data from memory to SCSI via the SPC data register using 
 * "progammed transfer mode", bypassing the AMD 9516.
 */
static PTM_Out(iopbp, phase)

register struct scsi_iopb *iopbp;
int phase;

{
	register volatile struct mb87030 *spc;
	register volatile u_char *d;
	register volatile u_char *s;
	register u_char *ptr;			/* memory address pointer */
	register u_int len;			/* byte counter */
	u_int delay;
	u_int status;

	spc = (struct mb87030 *)PHYS_TO_K1(FUJI_BASE_R2400);
	d = &spc->data;
	s = &spc->status;
	if (phase == P_CMD) {			/* command transfer */
		switch (iopbp->cmd_blk.cdb_0.cdb_0_cmd & GROUP_MASK) {
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

		/* pointer to first command byte */
		ptr = (u_char *)&iopbp->cmd_blk;
		phase = C_D;
	}
	else {
		len = iopbp->scsi_count;
		ptr = (u_char *)iopbp->scsi_bufaddr;
		phase = 0;
	}

	/*
	 * Wait for SCSI Bus to be in the required phase.
	 */
	delay = WAITLOOPS;
	while ((spc->phse_diag.phase_s & XFER_PHSE_MSK) != phase) {
		if (!(delay -= 1)) {
#ifdef	DEBUG
			pon_puts("PTM_Out: SCSI Bus NOT in correct phase\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = PHASE_ERROR;
			return(FAIL);
		}
	}

	/*
	 * Preliminary to issuing the transfer command.
	 */
	spc->count_hi  = HB(len); 
	FlushWB();
	spc->count_mid = MB(len); 
	FlushWB();
	spc->count_lo  = LB(len); 
	FlushWB();
	spc->phase_c = phase; 
	FlushWB();				/* set transfer phase */
	spc->xfer_mode = ASYNC; 
	FlushWB();
	spc->command = TRANSFER|PRG_XFER; /* issue program transfer command */
	delay = WAITLOOPS;
	while ((*s & (INIT | BUSY | XFER)) != (INIT | BUSY | XFER)) {
						/* SPC started? */
		if (!(delay -= 1)) {
#ifdef	DEBUG
		    pon_puts("PTM_Out: Timeout waiting to start transfer\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	do {
		delay = WAITLOOPS;
		while (*s & DFULL) {		/* spin while FIFO is full */
			if (!(delay -= 1) || (spc->interupts & SER_REQ)) {
				if ((spc->phse_diag.phase_s & XFER_PHSE_MSK) 
						== (C_D | I_O)) {
					spc->interupts = 0xff;
					FlushWB();
					Man_Status(iopbp);
					return(FAIL);
				}

#ifdef	DEBUG
			pon_puts("PTM_Out: Timeout spinning on FIFO full\r\n");
#endif	DEBUG
				iopbp->scsi_hwstatus = SCSITMO;
				return(FAIL);
			}
		}

		*d = *ptr++; 
		FlushWB();			/* move a byte from memory to SPC */
	} while (len -= 1);

	delay = WAITLOOPS;
	while (!(GetCause() & SR_IBIT3)) {	/* wait for cmd complete interrupt */
		if (!(delay -= 1)) {
#ifdef	DEBUG
		    pon_puts("PTM_Out: Timeout waiting for any interrupt\r\n");
#endif	DEBUG
			iopbp->scsi_hwstatus = SCSITMO;
			return(FAIL);
		}
	}

	if (!(spc->interupts & CMD_CMPLT)) {
		if (spc->interupts & SER_REQ) {
			if ((spc->phse_diag.phase_s & XFER_PHSE_MSK) == (C_D | I_O)) {
				spc->interupts = 0xff;
				FlushWB();
				Man_Status(iopbp);
				return(FAIL);
			}
		}

#ifdef	DEBUG_C
printf("PTM_Out: Got interrupt but it wasn't complete while transferring %x bytes\n", iopbp->scsi_count);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = SCSITMO;
		return(FAIL);
	}

	spc->interupts = CMD_CMPLT;		/* reset any pending ints */
	if (*s != XFER_CMP_ST) {		/* check the transfer cmp status */
#ifdef	DEBUG_C
printf("PTM_Out: Status should be %x, was %x, TC %x\n",XFER_CMP_ST,*s,TC_ALL);
#endif	DEBUG_C
		iopbp->scsi_hwstatus = SCSITMO;
		return(FAIL);
	}

	if (phase == P_DATAOUT) {
		status = Man_Status(iopbp);	/* status and message-in phases */
	}

	return(PASS);
}
