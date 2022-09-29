#ident "$Header: tpscsi.c,v 1.13 90/03/27 10:26:36 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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
 * tpscsi.c -- Introl SCSI cartridge tape controller standalone driver
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/mtio.h"
#include "machine/cpu.h"
#include "mips/dkinreg.h"
#include "mipsvme/vmereg.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#undef DEBUG

#ifdef XSABLE
static struct tsdevice tpreg;
#endif SABLE

#define WAITLOOPS	1000000
#define MAXRESETS	1
#define TP_BLKSIZ       (32 * 512)
#define TP_SHIFT        5	
#define TP_MASK         0x1f	
#define TP_BLKS_PER_REC	32
/*
 * Scsi Check Condition defines
 */
#define FM		0x80	/* Indicates that a File Mark has been read */
#define EOM		0x40	/* EOM or BOT has been reached */
#define VALID		0x80	/* Indicates that the residual length field */
/* Sense Keys			 * is defined (request sense command) */
#define NO_SENSE	0x00	/* FM, EOM or status unavailavble */
#define REC_ERR		0x01	/* command completed with recovery actions */
#define NOT_RDY		0x02	/* Tape drive can't be accessed */
#define MEDIUM_ERR	0x03	/* Non-recoverable data error */
#define HW_ERR		0x04	/* non-recoverable hdw failure (parity, etc) */
#define ILL_REQ		0x05	/* cmd block contains illegal parameter */
#define UNIT_ATN	0x06	/* cartridge change or tape drive was reset */
#define DATA_PROT	0x07	/* cartridge is write-protected */
#define BLANK_CHK	0x08	/* no-data condition encountered on tape */
#define CMD_ABORT	0x0b	/* tape drive aborted the command */
#define VOL_OVFLOW	0x0d	/* physical EOM with data still in buffer */
#define KEYMASK		0x0f

static char *TapeBuf;
static char didinit;
/*
 * Standard address for Introl 300.
 * Address should be given relative to A16 Supervisor address space.
 */
static char *tpscsistd[] = {
	(char *)0x3000
};

#define	NUNITS		7
#define	NCTLRS		(sizeof(tpscsistd)/sizeof(tpscsistd[0]))

/*
 * Per controller information
 */
struct	tpscsi_softc {
	u_char	ts_open;	/* prevent multiple opens */
	u_char	ts_lastiow;	/* last operation was a write */
	int	ts_xfer;	/* # of bytes xfered by last read */
	int	ts_curfile;	/* last file read/written on tape */
	int	ts_nxtrec;	/* next record on tape to read/write */
	int	ts_resid;	/* residual byte count */
	char   *c_io;		/* ptr to Introl I/O space */
	struct int_unit {
		struct  int_iopb	un_iopb;	/* IOPB */
		struct  int_iopb*	un_iopbp;/* IOPB pointer */
		u_char	un_sense[16];	/* Last Request Sense bytes */
		struct inquiry	un_inq;	/* Last Inquiry Command bytes */
		int 	type;
	} c_un[NUNITS];
} tpscsi_softc[NCTLRS];

/* how we talk to the Introl board
 */
#define INT_STATUS(ctlr)      *((unsigned char*)(tpscsi_softc[ctlr].c_io+0x201))
#define INT_START0(ctlr,x)    *((unsigned short*)tpscsi_softc[ctlr].c_io) = x
#define INT_START1(ctlr,x)    *((unsigned short*)tpscsi_softc[ctlr].c_io+1) = x

static struct int_errors {
	unsigned char	inerr_type;
	char *inerr_name;
} interrs[] = {
	/* SCSI Status */
	{ CHECK_CONDITION, "Check Condition" },
	{ SCSI_MET, "Condition Met" },
	{ SCSI_INTER, "Intermediate" },
	{ SCSI_COMBO, "Intermediate/Condition Met/Good" },
	{ SCSI_RESV, "Reservation Conflict" },
	/* Adaptor Status */
	{ AS_SELTMO, "selection timeout error" },
	{ AS_PHASERR, "scsi phase error" },
	{ AS_PBERROR, "I/O Parameter Block Negated" },
	{ AS_PARITY, "scsi parity error unrecoverable " },
	{ AS_HWERROR, "hardware failure" },
	{ AS_SCSITMO, "scsi timeout error" },
	{ AS_VMEBUSERR, "vme bus error" },
	{ 0xAA, "timed out waiting for adaptor" },
	{ 0, "status code not in table" }
};
char *
printerr(type)
	register unsigned char type;
{
	register struct int_errors *in = interrs;
	register hold;

	while (hold = in->inerr_type) {
		if (type == hold)
			return (in->inerr_name);
		in++;
	}
	return (in->inerr_name);
}

/*
 * _tpscsiinit -- initialize driver global data
 */
_tpscsiinit()
{
	struct int_iopb *ip;
	int i, j;

	if (sizeof (struct tp_dir) > IOB_FS)
		_io_abort ("bad size in iob for tpscsi");
	bzero(tpscsi_softc, sizeof(tpscsi_softc));
	didinit = 0;

#ifdef XSABLE
	tpqicstd[0] = (struct tsdevice *)K1_TO_PHYS(&tpreg);
	tpreg.tssr = TS_NBA;
#endif SABLE
}

/*
 * _tpscsiopen -- open tape device
 */
_tpscsiopen(io)
register struct iob *io;
{
	register struct tpscsi_softc *ts;
	register struct tp_dir *tpd;
	register int unit = io->i_unit;
	register int ctlr = io->i_ctlr;
	register int part = io->i_part;
	register struct int_unit *un;
	register struct int_iopb *ip;
	u_int status, addr, i, count;
	int flag = 0, tun = 0;

	/*
	 * verify controller and unit numbers
	 */
	if( !didinit ){
	    didinit = 1;
	    TapeBuf = (char *)align_malloc(TP_BLKSIZ,4);
	}
	if (unit >= NUNITS) {
		printf("tpscsi: bad unit number %d\n", unit);
		goto bad;
	}
	if (ctlr >= (int)NCTLRS) {
		printf("tpscsi: bad controller number %d\n", ctlr);
		goto bad;
	}
	tpscsi_softc[ctlr].c_io = (char *)
			PHYS_TO_K1(VMESA16_TO_PHYS(tpscsistd[ctlr]));

#ifndef SABLE
	/*
	 * probe for controller
	 */
	if (badaddr(tpscsi_softc[ctlr].c_io, sizeof(tpscsi_softc[ctlr].c_io))) {
		printf("no Introl controller at 0x%x\n", tpscsi_softc[ctlr].c_io);
		goto bad;
	}
#endif !SABLE

	/*
	 * tape is unique open device, so refuse if already open.
	 */
	ts = &tpscsi_softc[ctlr];
	if (ts->ts_open) {
		printf("tpscsi(%d,%d,%d) in use\n", ctlr,unit,part);
		goto badio;
	}
	/*
	 * save device address for future use 
	 */
	io->i_devaddr = (unsigned)tpscsi_softc[ctlr].c_io;

	un = &tpscsi_softc[ctlr].c_un[unit];
	ip =(struct int_iopb*)(&tpscsi_softc[ctlr].c_un[unit].un_iopb);
	tpscsi_softc[ctlr].c_un[unit].un_iopbp = ip;

	if ((INT_STATUS(ctlr) & HOST_A_RDY) == 0) { /* controller dead? */
		printf("tpscsi(%d,%d,%d): Adapter Not Ready!\n",ctlr,unit,part);
		goto badio;
	}
again:
	addr = K1_TO_PHYS(&un->un_inq);
	setupiopb(C0_INQUIRY,ctlr,unit,addr,0,sizeof(struct inquiry));
	if (status = int_startit(unit, ctlr)) {
		if (status == AS_SELTMO) { /* Adaptor selection timeout */
			/* give the tape one more chance */
			if (!flag) {
				/* bounce the LEDS and scan for abort */
				_scandevs();
				DELAY(800000);	/* ~.8 second delay */
				goto again;
			} else {
				printf("tpscsi(%d,%d,%d): status; %s\n",ctlr,
					unit,part,(char *)printerr(status));
				goto badio;
			}
		} else goto badio;
	}
	printf("vendor id is '");
	for (i=0; i < 8; i++)
		printf("%c", un->un_inq.inq_vendor[i]);
	printf("' product id is '");
	for (i=0; i < 16; i++)
		printf("%c", un->un_inq.inq_product[i]);
	printf("'\nand the revision level is '");
	for (i=0; i < 4; i++)
		printf("%c", un->un_inq.inq_rev[i]);
	printf("' ");
	switch (un->un_inq.inq_type) {
		case TYPE_TAPE:
			un->type = INT_TAPE;
			printf("\n");
			break;
		default:
			printf("device is NOT a tape drive\n");
			goto badio;
	}
retryit:
	setupiopb(C0_TESTRDY,ctlr,unit,0,0,0);
	status = int_startit(unit, ctlr);
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&un->un_sense[0]);
		setupiopb(C0_REQSENSE,ctlr,unit,addr,0,16);
		if (int_startit(unit, ctlr)) {
			printf("tpscsi(%d,%d,%d): request sense ERROR\n",
						 ctlr,unit,part);
			goto badio;	/* no need to retry */
		} else if (un->un_sense[2] & UNIT_ATN) {
			if (!tun) {
#ifdef DEBUG
				printf("\nunit attention\n");
#endif DEBUG
				tun++;
				goto retryit;
			}
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
			if (flag = (un->un_sense[2] & KEYMASK))
			       printf("SENSE key %d; %s\n",flag,printkey(flag));
		}
		goto badio;
	} else if (status) {
		if (flag) printf("\n");
		printf("tpscsi(%d,%d,%d): failed open; %s\n",ctlr,unit,part,
						(char *)printerr(status));
		goto badio;
	}
	/*
	 * Rewind the tape so that it may be moved forward to the
	 * correct file.
	 */
	setupiopb(C0_REWIND,ctlr,unit,0,0,0);
	if (status = int_startit(unit, ctlr))
		goto stat;
	if (part) {
		count = part;
#ifdef DEBUG
		printf("spacing FILES with count= 0x%x\n",count);
#endif DEBUG
		setupiopb(C0_SPACE,ctlr,unit,0,count,FILES);
		if (status = int_startit(unit, ctlr))
			goto stat;
	}
	/*
	 * Read in the volume header and see if it's valid.
	 * Everytime we open a file we read in the header.
	 * This is due to the fact that multiple directories may
	 * exist on one physical tape and we don't know which
	 * one was read in last time.
	 */
	addr = K1_TO_PHYS(&TapeBuf[0]);
	setupiopb(C0_READ,ctlr,unit,addr,(TP_BLKSIZ >> DEV_BSHIFT),0);
	/* we should always read 16k here since we know that's what we wrote
	 * ie, any check condition is no good */
	if (status = int_startit(unit, ctlr)) {
		printf("Failed to read in volume header\n");
		goto stat;
	}
	ts->ts_xfer = TP_BLKSIZ; /* read the full amount to get here */
	tpd = (struct tp_dir *)io->i_fs_tape;
	bcopy (TapeBuf, tpd, sizeof (struct tp_dir));
	if (io->i_fstype != DTFS_NONE) {
		switch (io->i_fstype) {
		      case DTFS_AUTO:
				if (!is_tpd (tpd)) {
					io->i_fstype = DTFS_NONE;
					break;
				}
				else {
					/*
					 * If more than one type of volume
					 * header is ever possible we would
					 * then need to do some mapping
					 * like vh_mapfstype();
					 */
					io->i_fstype = DTFS_TPD;
					break;
				}

			case DTFS_TPD:
				if (!is_tpd (tpd)) {
					printf ("bad volume header\n");
					goto badio;
				}
		}
	}
	/*
	 * open successful
	 */
	ts->ts_curfile = 0;
	ts->ts_lastiow = 0;
	ts->ts_resid = 0;
	ts->ts_nxtrec = 0;
	return (0);
stat:
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&un->un_sense[0]);
		setupiopb(C0_REQSENSE,ctlr,unit,addr,0,16);
		if (status = int_startit(unit, ctlr)) {
			printf("request sense failed\n");
			printf("tpscsi(%d,%d,%d): %s\n",ctlr,unit,part,
						(char *)printerr(status));
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",un->un_sense[i]);
			printf("\n");
		}
	} else
		printf("tpscsi(%d,%d,%d): %s\n",ctlr,unit,part,
					(char *)printerr(status));
badio:
	io->i_errno = EIO;
	return (-1);

bad:
	io->i_errno = ENXIO;
	return (-1);
}

/*
 * _tpscsiclose -- close tape device
 */
_tpscsiclose(io)
register struct iob *io;
{
	register struct tpscsi_softc *ts;
	register ctlr = io->i_ctlr;
	register unit = io->i_unit;
	struct int_unit *isu;
	u_int status, addr, i;

	ts = &tpscsi_softc[ctlr];
	/*
	 * if tape was open for writing or last operation was a write,
	 * then write two EOF's and backspace over the last one.
	 */
	if ((io->i_flgs == F_WRITE) || ((io->i_flgs & F_WRITE) && 
	   (ts->ts_lastiow))) {
		/* TODO: are 2 FM's necessary? tpqic() uses two! */
		setupiopb(C0_WRFM,ctlr,unit,0,1,0);
		if (status = int_startit(unit, ctlr))
			goto bad;
	}
	/*
	 * rewind the tape
	 */
	setupiopb(C0_REWIND,ctlr,unit,0,0,0);
	if (status = int_startit(unit, ctlr))
		goto bad;

	ts->ts_lastiow = 0;
	ts->ts_open = 0;
	return (0);

bad:
	isu = &tpscsi_softc[ctlr].c_un[unit];
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&isu->un_sense[0]);
		setupiopb(C0_REQSENSE,ctlr,unit,addr,0,16);
		if (status = int_startit(unit, ctlr)) {
			printf("tpscsi(%d,%d,%d): %s\n",ctlr,unit,io->i_part,
					(char *)printerr(status));
		} else {
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",isu->un_sense[i]);
			printf("\n");
		}
	} else
		printf("tpscsi(%d,%d,%d): %s\n",ctlr,unit,io->i_part,
					(char *)printerr(status));
	ts->ts_lastiow = 0;
	ts->ts_open = 0;
	io->i_errno = EIO;
	return (-1);
}

/*
 * _tpscsistrategy -- perform io
 */
_tpscsistrategy(io, func)
register struct iob *io;
register int func;
{
	register struct tpscsi_softc *ts;
	register struct tp_dir *tpd;
	daddr_t blkno;
	struct int_iopb *ip;
	struct int_unit *isu;
	int repcnt, off, newblk, xfer, space = 0;
	int blks, nxtblks, status = 0;
	int ctlr = io->i_ctlr;
	int unit = io->i_unit;
	u_int count, addr, i;

	ts = &tpscsi_softc[ctlr];
	ts->ts_lastiow = 0;
	ip =(struct int_iopb*)(&tpscsi_softc[ctlr].c_un[unit].un_iopb);
	/*
	 * If the request is for block zero, func equals a read
	 * and we have a valid tape volume header just return it.
	 * (Normally called from inside the file systems open routine)
	 */
	if (func == READ) {
		tpd = (struct tp_dir *)io->i_fs_tape;
		if (io->i_bn == 0 && (tpd->td_magic == TP_MAGIC) &&
		    io->i_cc == sizeof (struct tp_dir)) {
			bcopy (tpd, io->i_ma, io->i_cc);
			return (io->i_cc);
		}
	}
	/*
	 * Normally this flag is set in the open routine.  In this
	 * environment it's not save to set it there.  If you're
	 * opening a tape with a file system and you specifiy a name
	 * to use.  You could pass the tape controllers open only to
	 * fail inside the file systems open routine. (which calls
	 * strategy). You would then
	 * be left with this flag set and no way to reopen the correct
	 * file unless you rebooted or restarted the standalone program.
	 * When you have reached this point in _tpqicstrategy I know
	 * that you have completed the open.
	 */
	ts->ts_open = 1;

	if (func == WRITE) {
		if ((io->i_bn >> TP_SHIFT) != ts->ts_nxtrec)
			return (-1);
		blks = (io->i_cc + (DEV_BSIZE-1)) >> DEV_BSHIFT;
		setupiopb(C0_WRITE,ctlr,unit,io->i_ma,blks,0);
		status = int_startit(unit, ctlr);
		ts->ts_resid = 0;
		ts->ts_nxtrec++;
	}
	else {
		newblk = io->i_bn >> TP_SHIFT;
		if (newblk != ts->ts_nxtrec) {
#ifdef DEBUG
printf("strat: newblk = %d nextrec = %d\n",newblk,ts->ts_nxtrec);
#endif DEBUG
			ts->ts_nxtrec++;
			blks = newblk*TP_BLKS_PER_REC;
			nxtblks = ts->ts_nxtrec*TP_BLKS_PER_REC;
#ifdef DEBUG
printf("strat: blks = %d nxtblks = %d\n",blks,nxtblks);
#endif DEBUG
			if (newblk > ts->ts_nxtrec) {
				count = blks - nxtblks;
				setupiopb(C0_SPACE,ctlr,unit,0,count,BLOCKS);
				status = int_startit(unit, ctlr);
			} else if (newblk < ts->ts_nxtrec) {
				count = nxtblks - blks;
				count = ~count + 1; /* two's complement */
				setupiopb(C0_SPACE,ctlr,unit,0,count,BLOCKS);
				status = int_startit(unit, ctlr);
			}
			ts->ts_nxtrec = newblk;
			if (status) {
				space = 1;
				printf("tpscsi(%d,%d%d): SPACE cmd error\n",
						ctlr,unit,io->i_part);
				goto stat;
			}
			blks = TP_BLKSIZ >> DEV_BSHIFT;
			addr = K1_TO_PHYS(&TapeBuf[0]);
			setupiopb(C0_READ,ctlr,unit,addr,blks,0);
			if (status = int_startit(unit, ctlr))
				goto stat;
			ts->ts_xfer = TP_BLKSIZ;
		}
		xfer = _min(ts->ts_xfer, io->i_cc);
		/*
		 * Using i_bn here because i_offset is valid for a file
		 * not the tape_file. It's the file systems job to worry
		 * about file offsets.
		 */
		off = (io->i_bn & TP_MASK) << DEV_BSHIFT;
		bcopy (&TapeBuf[off], io->i_ma, xfer);
		ts->ts_xfer -= xfer;
		ts->ts_resid = io->i_cc - xfer;
	}
stat:
	isu = &tpscsi_softc[ctlr].c_un[unit];
	if (status == CHECK_CONDITION) {
		addr = K1_TO_PHYS(&isu->un_sense[0]);
		setupiopb(C0_REQSENSE,ctlr,unit,addr,0,16);
		if (int_startit(unit, ctlr)) {
			ts->ts_resid = io->i_cc;
			goto badio;
		} else {
#ifdef DEBUG
			printf("SENSE DATA: ");
			for (i=0; i < 16; i++)
				printf("%x ",isu->un_sense[i]);
			printf("\n");
#endif DEBUG
			if (space) goto badio; /* SPACE cmd error? */
			if (isu->un_sense[2] & FM) { /* read a file mark? */
				if (isu->un_sense[0] & VALID) { /* residual? */
					count = isu->un_sense[6] << DEV_BSHIFT;
				} else count = 0; /* is this OK? */
				ts->ts_xfer = TP_BLKSIZ - count;
				xfer = _min(ts->ts_xfer, io->i_cc);
				off = (io->i_bn & TP_MASK) << DEV_BSHIFT;
#ifdef DEBUG
printf("count= 0x%x i_cc= 0x%x xfer= 0x%x ts->ts_resid= 0x%x\n",count,io->i_cc,xfer,ts->ts_resid);
#endif DEBUG
				bcopy (&TapeBuf[off], io->i_ma, xfer);
				ts->ts_xfer -= xfer;
				ts->ts_resid = io->i_cc - xfer;
			}
			else goto badio;
		}
	} else if (status) goto badio;
	return (io->i_cc - ts->ts_resid);

badio:
	printf("tpscsi(%d,%d,%d): %s\n",ctlr,unit,io->i_part,
					(char *)printerr(status));
	io->i_errno = EIO;
	return (-1);
}
/*
 * _tpscsiioctl -- io controls
 */
_tpscsiioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
	io->i_errno = EINVAL;
	return (-1);
}

/*
** setupiopb() - Routine to set up the IOPB for the Introl Board.
*/
setupiopb(cmd, ctlr, unit, addr, blkcount, bcount)
{
	register struct int_iopb *ip;
	register int c;

	ip = tpscsi_softc[ctlr].c_un[unit].un_iopbp;
	ip->in_addmod = VME_A32NPAMOD; /* 0x09 - 32 bit access */
	ip->in_adctl  = AC_BURST|AC_WIDTH32|AC_IOPBPRI;
	ip->in_burst  = BURSTVALUE;	/* Data Throttle Burst */
	ip->in_break  = BURSTBREAK;	/* Data Throttle Break */
	ip->in_bufh   = ((u_int)addr>>16) & 0xffff;	/* Data Buffer */
	ip->in_bufl   = ((u_int)addr) & 0xffff;		/* Data Buffer */
	if (!blkcount)
		c = bcount/4;
	else
		c = blkcount*(DEV_BSIZE/4);
	ip->in_cnth = HB(c);		/* Transfer Count */
	ip->in_cntm = MB(c);		/* Transfer Count */
	ip->in_cntl = LB(c);		/* Transfer Count */
	ip->in_adstatus = 0xAA;		/* Adapter status (for POLLING) */
	ip->in_intlv = 0;		/* forces POLLED operation */
	ip->in_scstatus = 0;		/* SCSI status */
	ip->in_target = unit ? unit:6;	/* Target ID (default is 6) */
	/*
	** SCSI Command Descriptor Bytes in the IOPB
	*/
	ip->incd_b0 = cmd;		/* SCSI Command */
	if (cmd == C0_READ || cmd == C0_WRITE || cmd == C0_WRFM ||
					cmd == C0_SPACE) {
		if (cmd == C0_WRFM)
			ip->incd_b1 = 0;
		else if (cmd == C0_SPACE)
			ip->incd_b1 = bcount; /* space code */
		else
			ip->incd_b1 = 1; /* Fixed block size */
		ip->incd_b2 = HB(blkcount);
		ip->incd_b3 = MB(blkcount);
		ip->incd_b4 = LB(blkcount);
		return;
	}
	ip->incd_b1 = (0<<5);
	ip->incd_b2 = 0;
	ip->incd_b3 = 0;
	ip->incd_b4 = bcount;  /* byte count */
	ip->incd_b5 = 0;
}
/*
** fire off command
*/
int_startit(unit, ctlr)
	register unit, ctlr;
{
	register struct int_iopb *ip;
	register int timesup = WAITLOOPS;
	u_int addr, tv, s;
#ifdef DEBUG
	u_short s1, s2;
	u_char c, d;
	dump_iopb(ctlr,unit);
#endif DEBUG
	/* conversion needed in case we're running in sash */
	ip = (struct int_iopb*)K0_TO_K1(tpscsi_softc[ctlr].c_un[unit].un_iopbp);
	addr = K1_TO_PHYS(&ip->in_task);	/* iopb address */
	while((INT_STATUS(ctlr) & HOST_A_RDY) == 0) /* will Introl take iopb? */
		printf("!");
	INT_START0(ctlr,(int)((addr>>16)&0xFFFF)); wbflush();
	INT_START1(ctlr,(int)(addr&0xFFFF));
#ifdef DEBUG
	s1 = *((unsigned short*)tpscsi_softc[ctlr].c_io);
	s2 = *((unsigned short*)tpscsi_softc[ctlr].c_io+1);
	d = INT_STATUS(ctlr);
	printf("&iopb= 0x%x%x hostrdy= 0x%x\n",s1,s2,d);
#endif DEBUG
	while (ip->in_adstatus == 0xAA && --timesup)
		_scandevs();	/* bounce the LEDS while we wait */
	if (!timesup)
		printf("tpscsi%d: command timeout occurred\n",unit);
	if (ip->in_adstatus) {
#ifdef DEBUG
		printf("in%d: Adapter ERROR status: %s\n", unit,
			(char *)printerr(ip->in_adstatus));
#endif DEBUG
		return(ip->in_adstatus);
	}
	if (ip->in_scstatus) {
#ifdef DEBUG
		printf("in%d: Scsi ERROR status: %s\n", unit,
			(char *)printerr(ip->in_scstatus));
#endif DEBUG
		return(ip->in_scstatus);
	}
	return 0;
}
dump_iopb(ctlr, unit)
{
	register struct int_iopb *ip;
	register int i, j;

	ip = tpscsi_softc[ctlr].c_un[unit].un_iopbp;
	j=(int)((ip->in_cnth<<16)|(ip->in_cntm<<8)|(ip->in_cntl));
	printf("IOPB: Task=%x BuF=%x%x Cnt=%d Adap=%x Scsi=%x",ip->in_task,
		ip->in_bufh,ip->in_bufl,j,ip->in_adstatus,ip->in_scstatus);
	printf(" AdCntrl=%x Ipl=%d Statid(vec)=%x\nBRst=%x BK=%x AM=%x TAR=%x",
		ip->in_adctl, ip->in_intlv, ip->in_statid,
		ip->in_burst, ip->in_break,ip->in_addmod,ip->in_target);
	printf(" CD(%x %x %x %x %x %x",
		ip->incd_b0, ip->incd_b1, ip->incd_b2,
	 	ip->incd_b3, ip->incd_b4, ip->incd_b5);
	j=(int)((ip->incd_b2<<16)|(ip->incd_b3<<8)|(ip->incd_b4));
	printf(" (%d)blks",j);
	if (ip->incd_b0 & CD10BYTE)
		printf(" %x %x %x %x)\n",
			ip->incd_b6, ip->incd_b7, ip->incd_b8, ip->incd_b9);
	else
		printf(")\n");
}
