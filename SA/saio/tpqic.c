#ident "$Header: tpqic.c,v 1.7 90/03/07 12:40:54 menna Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * tpqic.c -- ISI cartridge tape controller standalone driver
 */

#include "sys/errno.h"
#include "sys/param.h"
#include "sys/mtio.h"
#include "machine/cpu.h"
#include "mipsvme/tsreg.h"
#include "mipsvme/vmereg.h"
#include "saio/tpd.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

/*
 * Standard addresses for QIC tape controller registers.
 * Can exist on any 8 word boundary within address range of
 * 0xfff000 to 0xffffe0.
 * Address should be given relative to A24 Supervisor address space.
 */
#ifdef SABLE
static struct tsdevice tpreg;
#endif SABLE

#define WAITLOOPS	5000000
#define MAXRESETS	1
#define TP_BLKSIZ       (32 * 512)
#define TP_SHIFT        5	
#define TP_MASK         0x1f	
#define TP_BLKS_PER_REC 	32

#define SET_TS(cmd, mem, cnt) \
	ts->ts_command.c_cmd = TS_ACK | TS_CVC | cmd; \
	ts->ts_command.c_loba = LO16(K1_TO_PHYS(mem)) & 0xffff; \
	ts->ts_command.c_hiba = HI16(K1_TO_PHYS(mem)); \
	ts->ts_command.c_size = cnt + (cnt & 1);

static char *TapeBuf;
static char didinit;
static struct tsdevice *tpqicstd[] = {
	(struct tsdevice *)0xfff550,
	(struct tsdevice *)0xfff560
};

#define	NUNITS		1	/* only one drive per controller */
#define	NCTLRS		(sizeof(tpqicstd)/sizeof(tpqicstd[0]))

/*
 * Per controller information
 */
static struct tpqic_softc {
	u_char		ts_open;	/* prevent multiple opens */
	u_char		ts_lastiow;	/* last operation was a write */
	int		ts_xfer;	/* # of bytes xfered by last read */
	int		ts_curfile;	/* last file read/written on tape */
	int		ts_nxtrec;	/* next record on tape to read/write */
	int		ts_resid;	/* residual byte count */
	struct ts_cmd	ts_command;	/* command message buffer */
	struct ts_sts	ts_status;	/* status message buffer */
	struct ts_char	ts_chardata;	/* characteristics data */
} tpqic_softc[NCTLRS+1];

/*
 * Termination class codes
 */
static struct reg_values tpqictc_values[] = {
	/* value		name */
	{  TS_SUCC,		"successful termination" },
	{  TS_ATTN,		"attention condition" },
	{  TS_ALERT,		"tape status alert" },
	{  TS_REJECT,		"function reject" },
	{  TS_RECOV,		"recoverable error" },
	{  TS_RECNM,		"recoverable error, no tape motion" },
	{  TS_UNREC,		"unrecoverable error" },
	{  TS_FATAL,		"fatal error" },
	{  0,			NULL }
};

/*
 * Status register description
 */
static struct reg_desc tpqicsr_desc[] = {
	/* mask		shift	name			format	values */
	{  TS_SC,	0,	"SpecCond",		NULL,	NULL },
	{  TS_RMR,	0,	"RegModRefused",	NULL,	NULL },
	{  TS_NXM,	0,	"NonExistMem",		NULL,	NULL },
	{  TS_NBA,	0,	"NeedBufAddr",		NULL,	NULL },
	{  TS_SSR,	0,	"Ready",		NULL,	NULL },
	{  TS_OFL,	0,	"Offline",		NULL,	NULL },
	{  TS_TC,	0,	"TermCond",		NULL,	tpqictc_values},
	{  0,		0,	NULL,			NULL,	NULL }
};

/*
 * Error codes in extended status 0
 */
static struct reg_desc tpqicxs0_desc[] = {
	/* mask		shift	name		format	values */
	{  TS_TMK,	0,	"TapeMark",	NULL,	NULL },
	{  TS_RLS,	0,	"RcrdShort",	NULL,	NULL },
	{  TS_LET,	0,	"LogEOT",	NULL,	NULL },
	{  TS_RLL,	0,	"RcrdLong",	NULL,	NULL },
	{  TS_WLE,	0,	"WrLckErr",	NULL,	NULL },
	{  TS_NEF,	0,	"NonExecFunc",	NULL,	NULL },
	{  TS_ILC,	0,	"IllCmd",	NULL,	NULL },
	{  TS_ONL,	0,	"Online",	NULL,	NULL },
	{  TS_WLK,	0,	"WrLocked",	NULL,	NULL },
	{  TS_BOT,	0,	"BOT",		NULL,	NULL },
	{  TS_EOT,	0,	"EOT",		NULL,	NULL },
	{  0,		0,	NULL,		NULL,	NULL }
};

/*
 * Error codes in extended status 1
 */
static struct reg_desc tpqicxs1_desc[] = {
	/* mask		shift	name	format	values */
	{  TS_UNC,	0,	"DataErr",	NULL,	NULL },
 	{  0,		0,	NULL,		NULL,	NULL }
};

/*
 * Error codes in extended status 2
 */
static struct reg_desc tpqicxs2_desc[] = {
	/* mask		shift	name	format	values */
 	{  TS_OPM,	0,	"TapeMoving",	NULL,	NULL },
 	{  0,		0,	NULL,		NULL,	NULL }
};

/*
 * Error codes in extended status 3
 */
static struct reg_desc tpqicxs3_desc[] = {
	/* mask		shift	name		format	values */
	{  TS_OPI,	0,	"OpIncmplt",	NULL,	NULL },
	{  TS_REV,	0,	"Reverse",	NULL,	NULL },
	{  TS_RIB,	0,	"RvrsIntoBOT",	NULL,	NULL },
	{  0,		0,	NULL,		NULL,	NULL }
};


/*
 * _tpqicinit -- initialize driver global data
 */
_tpqicinit()
{
	if (sizeof (struct tp_dir) > IOB_FS)
		_io_abort ("bad size in iob for tpq");
	bzero(tpqic_softc, sizeof(tpqic_softc));
	didinit = 0;

#ifdef XXX
	/* This causees write bus error since tpqicstd is defined */
	tpqicstd[0] = (struct tsdevice *)0xfff550;
#endif
#ifdef SABLE
	tpqicstd[0] = (struct tsdevice *)K1_TO_PHYS(&tpreg);
	tpreg.tssr = TS_NBA;
#endif SABLE

}

/*
 * _tpqicopen11 -- open qic-11 tape device
 */
_tpqicopen11(io)
register struct iob *io;
{
	io->i_flgs |= F_QIC11;
	_tpqicopen(io);
}
/*
 * _tpqicopen24 -- open qic-24 tape device
 */
_tpqicopen24(io)
register struct iob *io;
{
	io->i_flgs |= F_QIC24;
	_tpqicopen(io);
}
/*
 * _tpqicopen -- open tape device
 */
_tpqicopen(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;
	register struct tp_dir *tpd;

	if( !didinit ){
	    didinit = 1;
	    TapeBuf = (char *)align_malloc(TP_BLKSIZ,4);
	}

	/*
	 * verify controller and unit numbers
	 * NOTE: a negative controller number means the user wants to specify
	 * a controller address directly and not use the defaults.
	 */
	if (io->i_unit >= NUNITS) {
		printf("tpqic bad unit number %d\n", io->i_unit);
		goto bad;
	}
	if (io->i_ctlr >= (int)NCTLRS) {
		printf("tpqic bad controller number %d\n", io->i_ctlr);
		goto bad;
	}
	if (io->i_ctlr >= 0) {
		tp = (struct tsdevice *)
			PHYS_TO_K1(VMESA24_TO_PHYS(tpqicstd[io->i_ctlr]));
	} else {
		tp = (struct tsdevice *)
			PHYS_TO_K1(VMESA24_TO_PHYS(-io->i_ctlr));
		io->i_ctlr = NCTLRS;
	}

#ifndef SABLE
	/*
	 * probe for controller
	 */
	if (badaddr(&tp->tssr, sizeof(tp->tssr))) {
		printf("no cartridge tape controller at 0x%x\n", &tp->tssr);
		goto bad;
	}
#endif !SABLE

	/*
	 * tape is unique open device, so refuse if already open.
	 */
	ts = &tpqic_softc[io->i_ctlr];
	if (ts->ts_open) {
		printf("tpqic(%d) in use\n", io->i_ctlr);
		goto badio;
	}

	/*
	 * save device address for future use 
	 */
	io->i_devaddr = (unsigned)tp;

	/*
	 * initialize controller
	 */
	if (tpqic_reset(io) == 0)
		goto badio;

	/*
	 * cause the extended status registers to be updated
	 */
	SET_TS(TS_SENSE, 0, 0);
	tpqic_runcmd(io);

	if (tp->tssr & TS_SC) {
		tpqic_error(io);
		goto badio;
	}

	/*
	 * check for tape on line 
	 */
	if ((ts->ts_status.s_xs0 & TS_ONL) == 0) {
		printf("tpqic(%d) not online\n", io->i_ctlr);
		goto badio;
	}

	/*
	 * Rewind the tape so that it may be moved forward to the
	 * correct file.
	 */
	SET_TS(TS_REW, 0, 0);  
	tpqic_runcmd (io);
	if (io->i_part) {
		SET_TS(TS_SFORWF, io->i_part, 0);
		tpqic_runcmd (io);
	}

	if (tp->tssr & TS_SC) {
		tpqic_error(io);
		goto badio;
	}

	/*
	 * Read in the volume header and see if it's valid.
	 * Everytime we open a file we read in the header.
	 * This is due to the fact that multiple directories may
	 * exist on one physical tape and we don't know which
	 * one was read in last time.
	 */
	tpd = (struct tp_dir *)io->i_fs_tape;
	SET_TS(TS_RCOM, TapeBuf, TP_BLKSIZ);
	tpqic_runcmd (io);
	if (tp->tssr & TS_SC) {
		tpqic_error (io);
		printf("Failed to read in volume header\n");
		goto badio;
	}

	ts->ts_xfer = TP_BLKSIZ - ts->ts_status.s_rbpcr;
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

#ifdef notdef
	/*
	 * If there is no file system on this tape file reposition tape
	 * to the beginning of this file.
	 *
	 * Note:
	 * If the controller worked as stated this would be one command
	 * instead of two.  Here's the problem.  If I issue a reverse
	 * tape mark command(TS_SREVF) it must then be followed by a
	 * read command (TS_RCOM) to clear some strange condition it gets into.
	 * On top of this I can't just check for the TS_SC bit being set
	 * to see if there was an error.  Both TS_SREVF and TS_RCOM set
	 * the TS_SC bit and a couple of others telling you that a file
	 * mark was hit.  I must ignore the error
	 * when these other bits are set and check for a real error.
	 * Plain and simply it's just to much B.S. to use the reverse
	 * tape mark command.
	 */
	if (io->i_fstype == DTFS_NONE) {
		SET_TS(TS_REW, 0, 0);
		tpqic_runcmd (io);
		if (tp->tssr & TS_SC) {
			tpqic_error (io);
			goto badio;
		}
		if (io->i_part) {
			SET_TS(TS_SFORWF, io->i_part, 0);
			tpqic_runcmd (io);
			if (tp->tssr & TS_SC) {
				tpqic_error (io);
				goto badio;
			}
		}
	}
#endif notdef
	
	/*
	 * open successful
	 */
	ts->ts_curfile = 0;
	ts->ts_lastiow = 0;
	ts->ts_resid = 0;
	ts->ts_nxtrec = 0;
	return (0);

badio:
	io->i_errno = EIO;
	return (-1);

bad:
	io->i_errno = ENXIO;
	return (-1);
}

/*
 * _tpqicclose -- close tape device
 */
_tpqicclose(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];

	/*
	 * if tape was open for writing or last operation was a write,
	 * then write two EOF's and backspace over the last one.
	 */
	if ((io->i_flgs == F_WRITE) || ((io->i_flgs & F_WRITE) && 
	   (ts->ts_lastiow))) {
		SET_TS(TS_WEOF, 1, 0);
		tpqic_runcmd(io);
		if (tp->tssr & TS_SC) {
			tpqic_error(io);
			goto bad;
		}

		SET_TS(TS_WEOF, 1, 0);
		tpqic_runcmd(io);
		if (tp->tssr & TS_SC) {
			tpqic_error(io);
			goto bad;
		}
	}

	/*
	 * rewind the tape
	 */
	ts->ts_command.c_cmd = TS_ACK | TS_CVC | TS_REW;
	tpqic_runcmd(io);
	if (tp->tssr & TS_SC) {
		tpqic_error(io);
		goto bad;
	}

	ts->ts_lastiow = 0;
	ts->ts_open = 0;
	return (0);

bad:
	ts->ts_lastiow = 0;
	ts->ts_open = 0;
	io->i_errno = EIO;
	return (-1);
}

/*
 * _tpqicstrategy -- perform io
 */
_tpqicstrategy(io, func)
register struct iob *io;
register int func;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;
	register struct tp_dir *tpd;
	daddr_t blkno;
	int repcnt, off, newblk, xfer;
	int blks, nxtblks;

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];
	ts->ts_lastiow = 0;

	/*
	 * wait for unit to become ready
	 */
	tpqic_waitready(io);
	if ((tp->tssr & TS_SSR) == 0) {
		goto badio;
	}

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
	 * fail inside the file systems open routine.  You would then
	 * be left with this flag set and no way to reopen the correct
	 * file unless you rebooted or restarted the standalone program.
	 * When you have reached this point in _tpqicstrategy I know
	 * that you have completed the open.
	 */
	ts->ts_open = 1;

	if (func == WRITE) {
		if ((io->i_bn >> TP_SHIFT) != ts->ts_nxtrec)
			return (-1);
		SET_TS(TS_WCOM, io->i_ma, io->i_cc);
		tpqic_runcmd (io);
		ts->ts_resid = 0;
		ts->ts_nxtrec++;
	}
	else {
		newblk = io->i_bn >> TP_SHIFT;
		if (newblk != ts->ts_nxtrec) {
			ts->ts_nxtrec++;
			blks =  newblk;
			nxtblks = ts->ts_nxtrec;
			if (!(io->i_flgs & F_QIC11)) {
				blks *= TP_BLKS_PER_REC;
				nxtblks *= TP_BLKS_PER_REC;
			}
			if (newblk > ts->ts_nxtrec) {
				SET_TS(TS_SFORW, blks - nxtblks, 0);
				tpqic_runcmd (io);
			}
			else if (newblk < ts->ts_nxtrec) {
				SET_TS(TS_SREV, nxtblks - blks, 0);
				tpqic_runcmd (io);
			}

			ts->ts_nxtrec = newblk;
			if (tp->tssr & TS_SC) {
				tpqic_error (io);
				return (-1);
			}
			
			SET_TS(TS_RCOM, TapeBuf, TP_BLKSIZ);
			tpqic_runcmd (io);
			ts->ts_xfer = TP_BLKSIZ - ts->ts_status.s_rbpcr;
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

	if (tp->tssr & TS_SC) {
		if (ts->ts_status.s_xs0 & TS_TMK) {
			if (io->i_flgs & F_QIC11)
				ts->ts_resid = io->i_cc;
		} else if ((ts->ts_status.s_xs0 & (TS_RLS|TS_RLL)) == 0) {
			ts->ts_resid = io->i_cc;
			tpqic_error (io);
			goto badio;
		}
	}
	return (io->i_cc - ts->ts_resid);

badio:
	io->i_errno = EIO;
	return (-1);
}

/*
 * _tpqicioctl -- io controls
 */
_tpqicioctl(io, cmd, arg)
register struct iob *io;
register int cmd;
register caddr_t arg;
{
#ifdef notdef
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;
	struct mtop *mtop;
	struct mtget *mtget;
	register int callcount;
	int fcount;
	/*
	 * these must match the values and order of the MT codes
	 */
	static int tsops[] = {
		TS_WEOF,	/* write an end-of-file record */
		TS_SFORWF,	/* forward space file */
		TS_SREVF,	/* backward space file */
		TS_SFORW,	/* forward space record */
		TS_SREV,	/* backward space record */
		TS_REW,		/* rewind */
		TS_OFFL,	/* rewind and put the drive offline */
		TS_SENSE,	/* no operation, sets status only */
		TS_RET		/* retention operation */
	};

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];

	switch (cmd) {

	case TIOCTOP:
		/*
		 * perform a tape operation 
		 */
		mtop = (struct mtop *)arg;
		switch (mtop->mt_op) {

		case MTWEOF:	/* write an end-of-file record */
			fcount = 1;
			callcount = mtop->mt_count;
			break;

		case MTFSF:	/* forward space file */
		case MTBSF:	/* backward space file */
		case MTFSR:	/* forward space record */
		case MTBSR:	/* backward space record */
			callcount = 1;
			fcount = mtop->mt_count;
			break;

		case MTREW:	/* rewind */
		case MTOFFL:	/* rewind and put the drive offline */
		case MTNOP:	/* no operation, sets status only */
		case MTRET:	/* retention operation */
			callcount = 1;
			fcount = 1;
			break;

		case MTRST:	/* reset controller */
			tpqic_reset(io);
			return (0);

		default:
			goto bad;
		}

		if ((callcount <= 0) || (fcount <= 0))
			goto bad;

		ts->ts_lastiow = 0;
		while (--callcount >= 0) {
			ts->ts_command.c_cmd = TS_ACK | TS_CVC | 
						tsops[mtop->mt_op];
			ts->ts_command.c_repcnt = fcount;
			tpqic_runcmd(io);
			if (tp->tssr & TS_SC) {
				tpqic_error(io);
				goto badio;
			}
		}

		if (tsops[mtop->mt_op] == TS_SFORW)
			ts->ts_nxtrec += fcount;
		if (tsops[mtop->mt_op] == TS_SREV)
			ts->ts_nxtrec -= fcount;
		if (tsops[mtop->mt_op] == TS_SFORWF)
			ts->ts_curfile += fcount;
		if (tsops[mtop->mt_op] == TS_SREVF)
			ts->ts_curfile -= fcount;
		break;

	case TIOCGET:
		/*
		 * provide tape status
		 */
		mtget = (struct mtget *)arg;
		mtget->mt_dsreg = 0;
		mtget->mt_erreg = ts->ts_status.s_xs0;
		mtget->mt_resid = ts->ts_resid;
		mtget->mt_type = MT_ISQIC;
		break;

	default:
		goto bad;
		break;
	}
	return (0);

badio:
	io->i_errno = EIO;
	return (-1);

bad:
#endif notdef
	io->i_errno = EINVAL;
	return (-1);
}

/*
 * tpqic_error -- print out error information
 */
tpqic_error(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];

	printf("\ntpqic(%d) error: sr = %R\n",
	    io->i_ctlr, tp->tssr, tpqicsr_desc);
	printf("extended status 0: %R\n", ts->ts_status.s_xs0, tpqicxs0_desc);
	printf("extended status 1: %R\n", ts->ts_status.s_xs1, tpqicxs1_desc);
	printf("extended status 2: %R\n", ts->ts_status.s_xs2, tpqicxs2_desc);
	printf("extended status 3: %R\n", ts->ts_status.s_xs3, tpqicxs3_desc);

	/*
	 * currently qic-2 hangs on physical end of tape
	 */
	if (ts->ts_status.s_xs0 & TS_EOT) {
		tpqic_reset(io);
	}
/* TODO: error recovery ??? */
}

/*
 * tpqic_runcmd -- cause controller to execute a command
 */
tpqic_runcmd(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];

	if ((tp->tssr & TS_SSR) == 0) {
		return;
	}
	tp->tssr = HI16(K1_TO_PHYS(&ts->ts_command)) & 0xff;
	tp->tsdb = LO16(K1_TO_PHYS(&ts->ts_command));
#ifdef SABLE
	tpqicemu();
#else
	wbflush();
#endif SABLE
	tpqic_waitready(io);

	/*
	 * currently qic-2 hangs on physical end of tape
	 */
	if (ts->ts_status.s_xs0 & TS_EOT) {
		tpqic_reset(io);
	}
}

/*
 * tpqic_reset -- reset and initialize controller 
 */
tpqic_reset(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register struct tpqic_softc *ts;

	tp = (struct tsdevice *)io->i_devaddr;
	ts = &tpqic_softc[io->i_ctlr];

	/*
	 * reset controller
	 */
	tp->tssr = TSSR_RESET;
#ifdef SABLE
	tpqicemu();
#else
	wbflush();
#endif SABLE
	tpqic_waitready(io);

	/*
	 * Inform the controller of the location and size of the
	 * status message buffer via the write characteristics
	 * command.
	 * This MUST be the first command executed after a reset.
	 */
	ts->ts_chardata.char_ladr = LO16(K1_TO_PHYS(&ts->ts_status));
	ts->ts_chardata.char_hadr = HI16(K1_TO_PHYS(&ts->ts_status)) & 0xff;
	ts->ts_chardata.char_size = sizeof(struct ts_sts);
	if (io->i_flgs & F_QIC11) { /* qic-11 with ISI headers */
		ts->ts_chardata.char_mode = (TS_ESS | TS_FMT | TS_Q11);
	} else if (io->i_flgs & F_QIC24) { /* qic-24 without ISI headers */
		ts->ts_chardata.char_mode = (TS_ESS | TS_FMT | TS_RAW);
	} else /* default is no ISI headers */
		ts->ts_chardata.char_mode = (TS_ESS | TS_RAW);
	ts->ts_command.c_cmd = TS_ACK | TS_CVC | TS_SETCHR;
	ts->ts_command.c_loba = LO16(K1_TO_PHYS(&ts->ts_chardata));
	ts->ts_command.c_hiba = HI16(K1_TO_PHYS(&ts->ts_chardata)) & 0xff;
	ts->ts_command.c_size = sizeof(struct ts_char);

	tpqic_runcmd(io);
	if ((tp->tssr & TS_SC) || (tp->tssr & TS_NBA)) {
		tpqic_error(io);
		return (0);
	}
	return (1);
}

/*
 * tpqic_waitready -- wait for controller to become ready
 */
tpqic_waitready(io)
register struct iob *io;
{
	register volatile struct tsdevice *tp;
	register int i;
	register int nresets;

	i = 0;
	nresets = 0;
	tp = (struct tsdevice *)io->i_devaddr;
	do {
		_scandevs();
		if (++i > WAITLOOPS) {
			tp->tssr = TSSR_RESET;
			if (++nresets > MAXRESETS) {
				printf("tpqic(%d) never became ready\n", 
				    io->i_ctlr);
				return;
			}
			i = 0;
		}
	} while ((tp->tssr & TS_SSR) == 0);
}

#ifdef SABLE
/*
 * When running under SABLE we fake io transfers.
 */

#define _IOBUFSIZ (10 * 512)
#define EMUSHIFT 9
#define CMDMASK 02777

static char	_fakeiobuf[_IOBUFSIZ];
static int	_fakeioptr, after_reset;
static struct ts_sts *emu_ts;

tpqicemu()
{
	register struct tsdevice *tp = &tpreg;
	register struct ts_cmd *emu_c;
	register struct ts_char *emu_char;

	if (tp->tssr & TSSR_RESET) {
		after_reset = 1;
		_fakeioptr = 0;
		tp->tssr = TS_SSR;
	}
	else {
		/* 
		 * We must be processing a command
		 */
		emu_c = (struct ts_cmd *)
		   PHYS_TO_K1((tp->tssr << 16) | tp->tsdb);
		if (after_reset) {
			emu_status (emu_c);
			after_reset = 0;
		}
		else
			emu_cmd (emu_c);
	}
	return(0);
}

emu_status (emu_c)
	register struct ts_cmd *emu_c;
{
	register struct tsdevice *tp = &tpreg;
	register struct ts_char *emu_char;

	if (emu_c->c_cmd & TS_SETCHR) {
		
		/*
		 * First reference the charateristics structure
		 */
		emu_char = (struct ts_char *)
		    PHYS_TO_K1((emu_c->c_hiba << 16) | emu_c->c_loba);
		emu_ts = (struct ts_sts *)
		    PHYS_TO_K1((emu_char->char_hadr << 16) | emu_char->char_ladr);
		emu_ts->s_xs0 = TS_ONL|TS_BOT;
		tp->tssr = TS_SSR;
	}
	else
		tp->tssr = TS_SC | TS_NBA;
}

emu_cmd (emu_c)
	register struct ts_cmd *emu_c;
{
	register struct tsdevice *tp = &tpreg;
	register int cc, cmd;

	tp->tssr = TS_SSR;
	emu_ts->s_xs0 = TS_ONL;
	cmd = emu_c->c_cmd & CMDMASK;

	if (emu_c->c_cmd & (TS_SWB|TS_IE)) {
		tp->tssr = TS_SC;
		emu_ts->s_xs0 |= TS_ILC;
		return(0);
	}

	switch (cmd) {
		case TS_WCOM:
			if ((_fakeioptr + emu_c->c_size) > _IOBUFSIZ) {
				tp->tssr = TS_SC;
				emu_ts->s_xs0 |= TS_EOT;
			}
			else {
				bcopy (PHYS_TO_K1((emu_c->c_hiba<<16) | emu_c->c_loba),
					&_fakeiobuf[_fakeioptr], 
					emu_c->c_size);
				_fakeioptr += emu_c->c_size;
			}
			break;

		case TS_RCOM:
			cc = (_fakeioptr + emu_c->c_size) <= _IOBUFSIZ ?
				emu_c->c_size :
				_IOBUFSIZ - _fakeioptr;
			emu_ts->s_rbpcr = emu_c->c_size - cc;
			bcopy (&_fakeiobuf[_fakeioptr],
				PHYS_TO_K1((emu_c->c_hiba << 16) | emu_c->c_loba),
				cc);
			_fakeioptr += cc;
			break;

		case TS_SFORW:
			if ((_fakeioptr += (emu_c->c_repcnt<<EMUSHIFT))
			     > _IOBUFSIZ) {
				_fakeioptr = _IOBUFSIZ;
				tp->tssr = TS_SC;
				emu_ts->s_xs0 |= TS_EOT;
			}
			break;
	
		case TS_SREV:
			if ((_fakeioptr -= (emu_c->c_repcnt<<EMUSHIFT)) < 0) {
				_fakeioptr = 0;
				tp->tssr = TS_SC;
				emu_ts->s_xs0 |= TS_BOT;
			}
			break;

		case TS_REW:
			_fakeioptr = 0;
			emu_ts->s_xs0 |= TS_BOT;
			break;

		case TS_WEOF:
		case TS_SENSE:
			break;	/* just ignore for now */

		default:
			/*
			 * guess what? some bird brain issue an illegal command
			 */
			tp->tssr = TS_SC;
			emu_ts->s_xs0 |= TS_ILC;
	}
}
#endif SABLE
