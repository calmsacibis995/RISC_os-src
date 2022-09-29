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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: cdsio.c,v 1.7.4.2 90/05/10 05:11:53 wje Exp $"

/*
 * Streams driver for the Central Data 3608 serial board
 * Written by: Kipp Hickman
 * (lots of code stolen from io/sduart.c)
 *
 *	X get board to probe
 *	X send a character
 *	X rcv a character
 *	o log in
 *	o get RTS to work
 *	o get DTR to work
 *	o get full blown modem to work
 *	o get break to work
 *	X set input timer
 *	o eliminate all the XXX's
 *	o write a standalone diagnostic to go with the driver (MIPS)
 *	o flag to speed up interrupts when no data is needed to be sent
 *	o fix ^S/^Q to use send-immediate
 *	o does graphics on clover need gl_portinuse/gl_softintr, etc?
 *	o make sure driver works on clover
 *	o make sure driver works on pm2
 *	o make sure driver works on ip2
 *	o test multiple boards on clover
 *	o test multiple boards on pm2/ip2
 *	o fix lboot to use multiple interrupt vectors
 *
 * XXX export SVR0, SVR3 stuff to outer makefiles
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/io/RCS/cdsio.c,v $
 * $Revision: 1.7.4.2 $
 * $Date: 90/05/10 05:11:53 $
 */
#define	INDRIVER

#if defined(PM2) || defined(IP2)
#define	wbflush()
#define	volatile

#include "cd.h"
#include "../debug/debug.h"
#include "../h/param.h"
#include "../h/systm.h"
#include "../h/user.h"
#include "../h/termio.h"
#include "../streams/stream.h"
#include "../streams/stropts.h"
#include "../streams/strids.h"
#include "../streams/stty_ld.h"
#include "../h/file.h"
#include "machine/cpureg.h"
#include "../multibus/cdsioreg.h"
#include "../multibus/mbvar.h"
#define	SIOOFFSET	4
#else
#include "sys/debug.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/fs/s5dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/edt.h"
#include "sys/file.h"
#include "sys/cdsioreg.h"
#include "sys/cmn_err.h"
#define	SIOOFFSET	6
#endif

#if defined(PM2) || defined(IP2)
/*
 * Configuration info for iris 2000 & 3000 series
 */
int	cdintr();
static	int	cdProbe();
static	struct mb_device *cddinfo[NCD];
struct	mb_driver cddriver = {
	cdProbe, (int (*)())0, (int (*)())0, (int (*)())0, cdintr,
	(char *(*)())0, "ttyc", cddinfo,
};

/* pointer to i/o port in multibus i/o space */
static	struct	iodevice *cdio[NCD];

/* pointer to memory in multibus memory space */
static	struct	device *cdmem[NCD];

/* software struct for each port */
struct	cdport cdport[NCD*CDUPC];
int	cdnumports = NCD * CDUPC;
int	cdnumboards = NCD;

#define	CDMEMBASE	0x400000	/* base multibus address to start */
#define	CDMEMSIZE	0x010000	/* size of each board */
#define	CDIPL		(5|INT_ENABLE)	/* interrupt info */
#endif

#define	TRUE	(1==1)
#define	FALSE	(1==0)

#ifdef	CD3608
/*
 * lookup tables to convert CFLAG's to write register bits
 */
static	char wr3BitsPerChar[] = { WR3_BPC_5, WR3_BPC_6, WR3_BPC_7, WR3_BPC_8 };
static	char wr5BitsPerChar[] = { WR5_BPC_5, WR5_BPC_6, WR5_BPC_7, WR5_BPC_8 };

/* macros to use lookup tables and whatnot */
#define	WR3BPC(cf)	wr3BitsPerChar[((cf) & CSIZE) >> 4]
#define	WR5BPC(cf)	wr5BitsPerChar[((cf) & CSIZE) >> 4]
#define	STOPBITS(cf)	(((cf) & CSTOPB) ? WR4_STOP_2 : WR4_STOP_1)
#define	PARITY(cf) \
	(((cf) & PARENB) \
	 ? (((cf) & PARODD) ? WR4_PARITY : WR4_EVEN|WR4_PARITY) \
	 : 0)
#define	CARRIER(cf)	(/*((cf) & CLOCAL) ? WR3_DCD_ : */0)

#define	WR3ON(cf) 	(WR3BPC(cf) | WR3_RCV | CARRIER(cf))
#define	WR3OFF(cf)	(WR3BPC(cf) | CARRIER(cf))
#define	WR4BITS(cf)	(WR4_CLK_16 | STOPBITS(cf) | PARITY(cf))
#define	WR5BITS(cf)	(WR5BPC(cf) | WR5_BIT3/* | WR5_DTR*/)
#define	WR14BITS(cf)	(WR14_BIT0 | WR14_BIT1)
#define	CARRIER_PRESENT(us)	(((us) & SCC_DCD_) == 0)

/* CBAUD to wr13 and wr12 values */
static	struct {
	char	wr13;			/* wr13 value */
	char	wr12;			/* wr12 value */
} baud[] = {
	{ 0, 0 },			/* B0 - invalid */
	{ 0x05, 0xFE },			/* B50 */
	{ 0x03, 0xFE },			/* B75 */
	{ 0x02, 0xB8 },			/* B110 */
	{ 0x02, 0x39 },			/* B134.5 */
	{ 0x01, 0xFE },			/* B150 */
	{ 0x01, 0x7E },			/* B200 */
	{ 0x00, 0xFE },			/* B300 */
	{ 0x00, 0x7E },			/* B600 */
	{ 0x00, 0x3E },			/* B1200 */
	{ 0x00, 0x29 },			/* B1800, approximately */
	{ 0x00, 0x1E },			/* B2400 */
	{ 0x00, 0x0E },			/* B4800 */
	{ 0x00, 0x06 },			/* B9600 */
	{ 0x00, 0x02 },			/* B19200 */
	{ 0x00, 0x00 },			/* B38400 */
};
#endif	/* CD3100 */

#ifdef	CD3100
static	unchar mr1BitsPerChar[] = {MR1_BPC_5, MR1_BPC_6, MR1_BPC_7, MR1_BPC_8};
static	unchar mr2Baud[] = {
	0xFF,			/* B0 --> hangup port */
	MR2_BAUD_50,		/* B50 */
	MR2_BAUD_75,		/* B75 */
	MR2_BAUD_110,		/* B110 */
	MR2_BAUD_134,		/* B134 */
	MR2_BAUD_150,		/* B150 */
	0xFF,			/* B200 -- not supported */
	MR2_BAUD_300,		/* B300 */
	MR2_BAUD_600,		/* B600 */
	MR2_BAUD_1200,		/* B1200 */
	MR2_BAUD_1800,		/* B1800 */
	MR2_BAUD_2400,		/* B2400 */
	MR2_BAUD_4800,		/* B4800 */
	MR2_BAUD_9600,		/* B9600 */
	MR2_BAUD_19200,		/* B19200 */
	0xFF,			/* B38400 -- not supported */
};

#define	STOPBITS(cf)	(((cf) & CSTOPB) ? MR1_STOP_2 : MR1_STOP_1)
#define	MR1BPC(cf)	mr1BitsPerChar[((cf) & CSIZE) >> 4]
#define	PARITY(cf) \
	(((cf) & PARENB) \
	 ? (((cf) & PARODD) ? MR1_PARITY : MR1_EVEN|MR1_PARITY) \
	 : 0)
/*#define	CARRIER(cf)		(((cf) & CLOCAL) ? CR_DTR|CR_RTS : 0)*/
#define	CARRIER(cf)	(CR_DTR | CR_RTS)

#define	MR1BITS(cf)	(STOPBITS(cf) | PARITY(cf) | MR1BPC(cf) | MR1_BIT1)
#define	MR2BITS(cf)	(MR2_BIT5 | MR2_BIT4 | mr2Baud[(cf) & CBAUD])
#define	CRBITS(cf)	(CR_NORMAL | CR_BIT2 | CARRIER(cf) | CR_BIT0)
#define	BAUDOK(cf)	(mr2Baud[(cf) & CBAUD] != (unchar)0xFF)
#define	CARRIER_PRESENT(us)	(((us) & SCC_DCD) != 0)
#endif

/* default cflags */
#define DEF_CFLAG ((ushort)(SSPEED | CS8|CREAD|HUPCL|CLOCAL))

/* stream stuff */
extern	struct stty_ld def_stty_ld;
static	struct module_info dum_info = {
	STRID_DUART,			/* module ID */
	"CD3608",			/* module name */
	0,				/* minimum packet size */
	1024,				/* maximum packet size--1 sec@9600 */
	128,				/* hi-water mark */
	16,				/* lo-water mark */
};

#define MIN_RMSG_LEN 16			/* minimum buffer size */
#define MAX_RMSG_LEN 2048		/* largest msg allowed */
#define XOFF_RMSG_LEN 256		/* send XOFF here */
#define MAX_RBUF_LEN 1024

#define	CDXMITLIMIT		10	/* chars to send per xmit */

#define	CARRPRI	STIPRI			/* sleep for carrier at this */

static int cdOpen();
static int cdRsrv(), cdClose();
static struct qinit cdrinit = {
	NULL, cdRsrv, cdOpen, cdClose, NULL, &dum_info, NULL
};

static int cdWput();
static struct qinit cdwinit = {
	cdWput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab cdsioinfo = {&cdrinit, &cdwinit, NULL, NULL};

/* macros to get at bits in dev (these know that CDUPC == 8) */
#define	UNIT(dev)	((dev) & 0x1F)
#define	PORT(dev)	((dev) & 7)
#define	BOARD(dev)	(UNIT(dev) >> 3)
#define	MODEM(dev)	((dev) & 0x40)

#ifdef	SVR3
#define	MAXCTLRS	(32 / CDUPC)
#else
#define	MAXCTLRS	NCD
#endif

static	char	cdprobed[MAXCTLRS];
static	char	cdearly[MAXCTLRS];

#ifdef	CD3608
static	char *cderrors[] = {
	"no error",
	"bad eprom checksum",
	"bad scratchpad ram",
	"bad dual-port ram",
	"bad scc addressing",
	"bad usart test",
	"receiver interrupt failed",
};
#endif
#ifdef	CD3100
static	char *cderrors[] = {
	"no error",
	"unknown error",
	"bad eprom checksum",
	"bad scratchpad ram",
	"bad dual-port ram",
	"bad bus-lock",
	"bad usart test",
	"receiver interrupt failed",
	"partial usart damage",
};
#endif
#define	CDERRS		(sizeof(cderrors) / sizeof(char *))

static	void	cdHangup(), cdStop(), cdStartOutput();
static	int	cdDelay();

/*
 * Get error message given error code - used during boot up
 */
static char *
cdErrorMsg(c)
	unchar c;
{
	if (c >= CDERRS)
		return ("Unknown error");
	else
		return (cderrors[c]);
}

/*
 * Poll the board, waiting for it to be ready for a new command
 */
static void
cdPoll(iod, unit)
	register volatile struct iodevice *iod;
	int unit;
{
	register long timo;

	timo = 100000;
	while (!(iod->hostStatusPort & HSP_READY) && --timo)
		;
	if (timo == 0) {
#ifdef	SVR0
		printf("ttyc%d: timeout while polling board\n", unit);
#else
		cmn_err(CE_CONT, "ttyc%d: timeout waiting for board\n",
				 unit);
#endif
	}
}

/*
 * Start board off on a new command
 */
static void
cdStartCmd(dp)
	register struct cdport *dp;
{
	cdPoll(dp->dp_iodevice, dp->dp_index);
	dp->dp_iodevice->cmdPort = dp->dp_bit;
	wbflush();
}

/*
 * Send a command to the given board.  Return FALSE if the command fails,
 * TRUE if it succeeds.
 */
static int
cdCmd(dp, cmd, cmdhigh, cmdlow)
	register struct cdport *dp;
	char cmd, cmdhigh, cmdlow;
{
	register volatile struct device *d;
	long timo;

	d = dp->dp_device;
	d->cmd = cmd;
	d->cmdHigh = cmdhigh;
	d->cmdLow = cmdlow;
	d->cmdStatus = 0xFF;
	cdStartCmd(dp);

	/* wait for command to complete */
	timo = 100000;
	while ((d->cmdStatus == 0xFF) && --timo) {
		DELAY(100);
	}
	if (timo == 0) {
#ifdef	SVR0
		printf("ttyc%d: timeout waiting for board\n", dp->dp_index);
#else
		cmn_err(CE_CONT, "ttyc%d: timeout waiting for board\n",
				 dp->dp_index);
#endif
		return (FALSE);
	}
	if (d->cmdStatus == 0x01) {
		printf("XXX ttyc%d: cmd failed, cmd=%x high=%x low=%x\n",
			    dp->dp_index, cmd, cmdhigh, cmdlow);
		return (FALSE);
	}
	return (TRUE);
}

/*
 * Set the port parameters.  "cf" is the new control flags.
 */
static int
cdSetParams(dp, cf, tp)
	register struct cdport *dp;
	register uint cf;
	struct termio *tp;
{
	register uint diff;
	register int s;

	s = spltty();
	diff = cf ^ dp->dp_cflag;

	if (diff &= (CBAUD|CSIZE|CSTOPB|PARENB|PARODD)) {
		if ((cf & CBAUD) == 0) {	/* hang up line if asked */
			cdHangup(dp);
		} else {
#ifdef	CD3608
			/*
			 * Disable receiver.  Setup transmitter state, then
			 * enable the receiver.  Also setup the highwater
			 * mark for the input buffer to 256 bytes, and set
			 * the delay time so that the board will interrupt
			 * no more than 30 times a second.
			 */
			if (!cdCmd(dp, CMD_SET_SCC, WR3OFF(cf), 3) ||
			    !cdCmd(dp, CMD_SET_SCC, WR4BITS(cf), 4) ||
			    !cdCmd(dp, CMD_SET_SCC, WR5BITS(cf), 5) ||
			    !cdCmd(dp, CMD_SET_BAUD, baud[cf&CBAUD].wr13,
				     baud[cf&CBAUD].wr12) ||
			    !cdCmd(dp, CMD_SET_SCC, WR14BITS(cf), 14) ||
			    !cdCmd(dp, CMD_SET_SCC, WR3ON(cf), 3) ||
			    !cdCmd(dp, CMD_SET_IDT, 60>>8, 60) ||
			    !cdCmd(dp, CMD_SET_HWM, 256>>8, 256))
#endif	/* CD3608 */
#ifdef	CD3100
			/*
			 * Setup uart mode and command registers
			 */
			if (BAUDOK(cf) &&
			    (!cdCmd(dp, CMD_SET_MR12, MR1BITS(cf),
					MR2BITS(cf)) ||
			     !cdCmd(dp, CMD_SET_CR, 0, CRBITS(cf)) ||
			     !cdCmd(dp, CMD_SET_IDT, 60>>8, 60) ||
			     !cdCmd(dp, CMD_SET_HWM, 256>>8, 256)))
#endif
			{
				return (FALSE);
			}
		}
	}
	if (tp)
		dp->dp_termio = *tp;
	dp->dp_cflag = cf;
	splx(s);
	return (TRUE);
}

/*
 * Handle TCSET's
 */
static void
cdtcset(dp,bp)
	register struct cdport *dp;
	register mblk_t *bp;
{
	register struct iocblk *iocp;
	register struct termio *tp;
	register uint cflag;

	iocp = (struct iocblk*)bp->b_rptr;
	tp = STERMIO(bp);

	cflag = tp->c_cflag;
	cflag &= ~CLOCAL;
	if (dp->dp_cflag & CLOCAL)
		cflag |= CLOCAL;

	if (!cdSetParams(dp, cflag, tp)) {
		/* oops, error */
		iocp->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
	} else {
		/* tell line discipline the results */
		tp->c_cflag = dp->dp_cflag;
		iocp->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
	}
}

/*
 * Return non-zero if the given port is active
 */
static int
cdIsOn(dp)
	register struct cdport *dp;
{
	/* activate receiver */
#ifdef	CD3608
	if (!cdCmd(dp, CMD_SET_SCC, WR3ON(dp->dp_cflag), 3))
		return (0);
#endif

	if (dp->dp_cflag & CLOCAL) {
		/* assume on for direct connects */
		dp->dp_state |= DP_DCD;			/*XXX?*/
		return (1);
	} else {
		/* examine modem status */
		if (CARRIER_PRESENT(dp->dp_device->uartStatus)) {
			dp->dp_state |= DP_DCD;
			return (1);
		} else
			return (0);
	}
}

/*
 * Stop transmitting
 */
/*ARGSUSED*/
static void
cdStop(dp)
	struct cdport *dp;
{
	/* NOP */
}

/*
 * Clear receiver input buffer
 */
void
cdReceiveClear(dp)
	struct cdport *dp;
{
	register volatile struct device *d;

	d = dp->dp_device;
	if (d->inputFillPtr != d->inputEmptyPtr) {
		/* snarf up input data */
		d->inputEmptyPtr = d->inputFillPtr;
	}
}

/*
 * flush output
 *	Interrupts must have been made safe here.
 */
static void
cdFlushOutput(dp)
	register struct cdport *dp;
{
	if ((dp->dp_state & DP_TIMEOUT) == DP_TIMEOUT) {
#ifdef	SVR0
		untimeout_id(dp->dp_tid);	/* forget stray timeout */
#else
		untimeout(dp->dp_tid);		/* forget stray timeout */
#endif
		dp->dp_state &= ~DP_TIMEOUT;
	}

	freemsg(dp->dp_wbp);
	dp->dp_wbp = NULL;
}

/*
 * flush input
 *	interrupts must be safe here
 */
static void
cdFlushInput(dp)
	register struct cdport *dp;
{
	freemsg(dp->dp_rmsg);
	dp->dp_rmsg = NULL;
	dp->dp_rmsg_len = 0;
	freemsg(dp->dp_rbp);
	dp->dp_rbp = NULL;

	qenable(dp->dp_rq);		/* turn input back on */
}

/*
 * Hangup the given line
 */
static void
cdHangup(dp)
	struct cdport *dp;
{
	cdFlushOutput(dp);			/* forget pending output */
	/* disable receiver (no more input interrupts) */
#ifdef	CD3608
	(void) cdCmd(dp, CMD_SET_SCC, WR3OFF(dp->dp_cflag), 3);
#endif
	cdStop(dp);				/* stop transmitter */
	cdReceiveClear(dp);			/* clear input buffer */
}

/*
 * open a serial port
 */
static int
cdOpen(rq, dev, flag, sflag)
	queue_t *rq;
	dev_t dev;
	int flag, sflag;
{
	extern dev_t console_dev, duart_dev;
	register struct cdport *dp;
	register queue_t *wq = WR(rq);
	register ushort port;
	int s;

	if (sflag)			/* only a simple stream driver */
		return (OPENFAIL);

	/* validate device */
	port = UNIT(dev);
	if ((port > cdnumports) || !cdprobed[BOARD(dev)]) {
		u.u_error = ENXIO;
		return (OPENFAIL);
	}

	dp = &cdport[port];

	s = spltty();
	if (!(dp->dp_state & (DP_ISOPEN|DP_WOPEN))) {	/* on the 1st open */
		register uint cflag;

		cflag = SSPEED | CS8 | HUPCL | CLOCAL;
		dp->dp_state &= ~(DP_TXSTOP|DP_LIT|DP_BLOCK
				  |DP_TX_TXON|DP_TX_TXOFF
				  |DP_FLOW);
		if (MODEM(dev)) {
			cflag &= ~CLOCAL;
			dp->dp_state |= DP_FLOW;
		}
		dp->dp_litc = CLNEXT;	/* clear everything */
		dp->dp_stopc = CSTOP;
		dp->dp_startc = CSTART;
		if (!cdSetParams(dp, cflag, &def_stty_ld.st_termio)) {
			u.u_error = EIO;
			return (OPENFAIL);
		}

		/* wait for carrier, if we are supposed to */
		if (!cdIsOn(dp) && !(dp->dp_cflag & CLOCAL) &&
			 !(flag & FNDELAY)) {
			do {
				dp->dp_state |= DP_WOPEN;
				if (sleep((caddr_t)dp, CARRPRI | PCATCH)) {
					u.u_error = EINTR;
					cdHangup(dp);
					dp->dp_state &= ~DP_WOPEN;
					splx(s);
					return (OPENFAIL);
				}
			} while (!(dp->dp_state & DP_DCD));
		}

		rq->q_ptr = (caddr_t)dp;	/* connect device to stream */
		wq->q_ptr = (caddr_t)dp;
		dp->dp_wq = wq;
		dp->dp_rq = rq;
		dp->dp_state |= DP_ISOPEN;
		dp->dp_cflag |= CREAD;

		cdReceiveClear(dp);		/* discard input */
		if (!strdrv_push(rq, "stty_ld", dev)) {
			cdHangup(dp);
			dp->dp_state &= ~(DP_ISOPEN|DP_WOPEN);
			splx(s);
			return (OPENFAIL);
		}
	} else {
		/*
		 * You cannot open two streams to the same device.  The dp
		 * structure can only point to one of them.  Therefore, you
		 * cannot open two different minor devices that are synonyms
		 * for the same device.
		 * XXX eliminate this restriction?
		 */
		if (dp->dp_rq == rq) {
			ASSERT(dp->dp_wq == wq);
			ASSERT(dp->dp_rq->q_ptr == (caddr_t)dp);
			ASSERT(dp->dp_wq->q_ptr == (caddr_t)dp);
		} else {
			u.u_error = ENOSR;	/* fail if already open */
			splx(s);
			return (OPENFAIL);
		}
	}
	splx(s);
	return (minor(dev));		/* return successfully */
}

/*
 * close a port
 */
static int
cdClose(rq)
	queue_t *rq;
{
	register struct cdport *dp;
	register int s;

	dp = (struct cdport *)rq->q_ptr;
	if (!dp) {
		/* port wasn't really open */
		return;
	}

	s = spltty();
	ASSERT(dp >= &cdport[0] && dp->dp_rq == rq);
	cdFlushInput(dp);
	cdFlushOutput(dp);
	dp->dp_rq = NULL;
	dp->dp_wq = NULL;
	dp->dp_state &= ~DP_ISOPEN;

	/* hang up line if we are supposed to */
	if ((dp->dp_cflag & HUPCL)
#ifdef	GL2
	    && !gl_portinuse(dp->dp_index)
#endif
	    )
		cdHangup(dp);
	splx(s);
}

/*
 * get a new buffer
 *	Interrupts ought to be off here.
 * XXX bleh
 */
static mblk_t *
cdGetNewbp(dp,pri)
	register struct cdport *dp;
	uint pri;
{
	register int size;
	register mblk_t *bp;
	register mblk_t *rbp;

	rbp = dp->dp_rbp;
	if (dp->dp_rmsg_len >= MAX_RMSG_LEN	/* if overflowing */
	    || (rbp != 0		/* or current buffer empty */
		&& rbp->b_rptr >= rbp->b_wptr)) {
		bp = 0;
	} else {
		size = dp->dp_rbsize;
		if (size > MAX_RBUF_LEN)	/* larger buffer */
			size = MAX_RBUF_LEN;	/* as we get behind */
		for (;;) {
			if (size < MIN_RMSG_LEN)
				size = MIN_RMSG_LEN;

			bp = allocb(size, pri);
			if (bp != 0)
				break;

			if (BPRI_HI == pri
			    && size > MIN_RMSG_LEN) {
				size >>= 2;
				continue;
			}
			break;
		}
	}

	if (rbp == 0) {			/* if we have an old buffer */
		dp->dp_rbp = bp;
	} else if ((bp != 0) ||
		   (rbp->b_wptr >= rbp->b_datap->db_lim)) {
		   /* & a new buffer */
		    /* or old buffer is full */
		str_conmsg(&dp->dp_rmsg, &dp->dp_rmsge, rbp);
		dp->dp_rmsg_len += (rbp->b_wptr - rbp->b_rptr);
		dp->dp_rbp = bp;
	}

	if (dp->dp_rmsg_len >= XOFF_RMSG_LEN
	    || !dp->dp_rbp) {
#ifdef	XXX
		if (DP_FLOW & dp->dp_state)	/* drop RTS */
			dp->dp_ca->ca_ropbc = dp->dp_rts;
#endif
		if ((dp->dp_iflag & IXOFF)	/*  do XOFF */
		    && !(dp->dp_state & DP_BLOCK)) {
			dp->dp_state |= DP_TX_TXOFF;
			dp->dp_state &= ~DP_TX_TXON;
			cdStartOutput(dp);
		}
	}

	return bp;
}

/*
 * send a bunch of 1 or more characters up the stream
 *	This should be invoked only because a message could not be sent
 *	upwards by the interrupt, and things have now drained.
 */
static int
cdRsrv(rq)
	register queue_t *rq;
{
	register mblk_t *bp;
	register struct cdport *dp;
	register int s;

	dp = (struct cdport *)rq->q_ptr;
	ASSERT(dp->dp_rq == rq);
	ASSERT(dp >= &cdport[0] && dp <= &cdport[cdnumports]);
	ASSERT(dp->dp_state & DP_ISOPEN);

	if (!canput(rq->q_next)) {	/* quit if upstream congested */
		noenable(rq);
		return;
	}
	enableok(rq);

	s = spltty();

	if ((bp = dp->dp_rbp) != NULL) {
		register int sz;
		sz = (bp->b_wptr - bp->b_rptr);
		if (sz > 0
		    && (!dp->dp_rsrv_cnt || !dp->dp_rmsg)) {
			str_conmsg(&dp->dp_rmsg, &dp->dp_rmsge, bp);
			dp->dp_rmsg_len += sz;
			dp->dp_rbp = 0;
		}
	}

	if ((bp = dp->dp_rmsg) != NULL) {
		dp->dp_rmsg = 0;
		dp->dp_rbsize = dp->dp_rmsg_len;
		dp->dp_rmsg_len = 0;
		splx(s);		/* without too much blocking, */
		putnext(rq, bp);	/* send the message */
		(void)spltty();
	}

	if (!dp->dp_rmsg) {
/* XXX		dp->dp_ca->ca_sopbc = dp->dp_rts;	/* restore RTS */
		if (dp->dp_state & DP_BLOCK) {	/* do XON */
			dp->dp_state |= DP_TX_TXON;
			cdStartOutput(dp);
		}
	}

	if (!dp->dp_rbp) {
		(void) cdGetNewbp(dp, BPRI_LO);
	}

	splx(s);
}

/*
 * 'put' function
 *	Just start the output if we like the message.
 */
static int
cdWput(wq, bp)
	queue_t *wq;
	register mblk_t *bp;
{
	register struct cdport *dp;
	register struct iocblk *iocp;
	register int s;

	dp = (struct cdport *)wq->q_ptr;
	if (!dp) {
		sdrv_error(wq,bp);	/* quit now if not open */
		return;
	}

	s = spltty();
	ASSERT(dp->dp_wq == wq);
	ASSERT(dp >= &cdport[0] && dp <= &cdport[cdnumports]);
	ASSERT(dp->dp_state & DP_ISOPEN);

	switch (bp->b_datap->db_type) {

	case M_FLUSH:
		if (*bp->b_rptr & FLUSHW) {
			cdFlushOutput(dp);
			dp->dp_state &= ~DP_TXSTOP;
			cdStartOutput(dp);	/* restart output */
		}
		if (*bp->b_rptr & FLUSHR)
			cdFlushInput(dp);
		sdrv_flush(wq,bp);
		break;

	case M_DATA:
	case M_DELAY:
		if (!(dp->dp_state & DP_DCD)
		    && !(dp->dp_cflag & CLOCAL))
			freemsg(bp);	/* discard if !local & !dcd */
		else {
			putq(wq, bp);
			cdStartOutput(dp);
		}
		break;

	case M_IOCTL:
		iocp = (struct iocblk*)bp->b_rptr;
		switch (iocp->ioc_cmd) {
		case TCXONC:
			ASSERT(iocp->ioc_count == sizeof(int));
			switch (*(int*)(bp->b_cont->b_rptr)) {
			case 0:		/* stop output */
				dp->dp_state |= DP_TXSTOP;
				cdStop(dp);
				break;
			case 1:		/* resume output */
				dp->dp_state &= ~DP_TXSTOP;
				cdStartOutput(dp);
				break;
			case 2:
/* XXX
				if (DP_FLOW & dp->dp_state)
					dp->dp_ca->ca_ropbc = dp->dp_rts;
*/
				if (!(dp->dp_state & DP_BLOCK)) {
					dp->dp_state |= DP_TX_TXOFF;
					dp->dp_state &= ~DP_TX_TXON;
				}
				break;
			case 3:
/* XXX				dp->dp_ca->ca_sopbc = dp->dp_rts; */
				if (dp->dp_state & DP_BLOCK) {
					dp->dp_state |= DP_TX_TXON;
					cdStartOutput(dp);
				}
				break;
			default:
				iocp->ioc_error = EINVAL;
				break;
			}
			bp->b_datap->db_type = M_IOCACK;
			iocp->ioc_count = 0;
			qreply(wq, bp);
			break;

		case TCSETA:
			ASSERT(iocp->ioc_count == sizeof(struct termio));
			cdtcset(dp, bp);
			qreply(wq, bp);
			break;

		case TCSETAW:
		case TCSETAF:
			ASSERT(iocp->ioc_count == sizeof(struct termio));
			putq(wq, bp);
			cdStartOutput(dp);
			break;

		case TCGETA:
			tcgeta(wq, bp, &dp->dp_termio);
			break;

		case TCSBRK:
			putq(wq, bp);
			cdStartOutput(dp);
			break;

		case FIONREAD:
			fion(RD(wq),bp, (msgdsize(dp->dp_rmsg)
					 + msgdsize(dp->dp_rbp)));
			qreply(wq, bp);
			break;

		case TCBLKMD:
			dp->dp_iflag |= IBLKMD;
			iocp->ioc_count = 0;
			bp->b_datap->db_type = M_IOCACK;
			qreply(wq, bp);
			break;

		default:
			bp->b_datap->db_type = M_IOCNAK;
			qreply(wq, bp);
			break;
		}
		break;


	default:
		sdrv_error(wq, bp);
	}

	splx(s);
}

static int
cdDelay(dp)
	register struct cdport *dp;
{
	int s;

	s = spltty();
	if ((dp->dp_state & (DP_BREAK|DP_BREAK_QUIET)) != DP_BREAK) {
		dp->dp_state &= ~(DP_TIMEOUT|DP_BREAK|DP_BREAK_QUIET);
		cdStartOutput(dp);		/* resume output */
	} else {
		dp->dp_state |= DP_BREAK_QUIET;
		dp->dp_tid = timeout(cdDelay, (caddr_t)dp, HZ/20);
	}
	splx(s);
}

/*
 * interrupt-process an IOCTL
 *	This function processes those IOCTLs that must be done by the output
 *	interrupt.
 */
static int
cdInterruptIoctl(dp, bp)
	register struct cdport *dp;
	register mblk_t *bp;
{
	register struct iocblk *iocp;
	int needsAttention;

	needsAttention = 0;
	iocp = (struct iocblk *)bp->b_rptr;
	if (iocp->ioc_cmd == TCSBRK) {
		if (*(int *)bp->b_cont->b_rptr == 0) {
			register volatile struct device *d;

			dp->dp_state |= (DP_TIMEOUT|DP_BREAK);
			dp->dp_tid = timeout(cdDelay, (caddr_t)dp, HZ/3);
			needsAttention = 1;

			/* tell board to send a break */
			d = dp->dp_device;
			d->cmd = CMD_SEND_BREAK;
			d->cmdStatus = 0xFF;
			cdStartCmd(dp);
		}
		iocp->ioc_count = 0;
		bp->b_datap->db_type = M_IOCACK;
	} else {
		cdtcset(dp, bp);
		if (iocp->ioc_cmd == TCSETAF)
			(void) putctl1(dp->dp_rq->q_next, M_FLUSH, FLUSHR);
	}
	putnext(dp->dp_rq, bp);
	return (needsAttention);
}

/*
 * Output something
 */
static int
cdOutput(dp)
	register struct cdport *dp;
{
	register unchar c;
	register mblk_t *wbp;
	register volatile unchar *obuf;
	register int xmitcount;

printf("t");
	if ((dp->dp_state & (DP_TXSTOP|DP_TIMEOUT|DP_ISOPEN)) != DP_ISOPEN) {
		cdStop(dp);
		return (0);
	}
	xmitcount = 0;
	obuf = &dp->dp_device->outputBuf0[0];
	while (xmitcount < CDXMITLIMIT) {
		if (dp->dp_state & DP_TX_TXON) {	/* send XON or XOFF */
			c = dp->dp_startc;
			dp->dp_state &= ~(DP_TX_TXON|DP_TX_TXOFF|DP_BLOCK);
		} else
		if (dp->dp_state & DP_TX_TXOFF) {
			c = dp->dp_stopc;
			dp->dp_state &= ~DP_TX_TXOFF;
			dp->dp_state |= DP_BLOCK;
		} else {
			if (!(wbp = dp->dp_wbp)) {
				wbp = getq(dp->dp_wq);	/* get another msg */
				if (!wbp)
					break;

				/*
				 * If new message is not a data message and
				 * we have already placed some characters in
				 * the output buffer, don't process new
				 * message now.
				 */
				if ((wbp->b_datap->db_type != M_DATA) &&
				    (xmitcount != 0)) {
					break;
				}
				switch (wbp->b_datap->db_type) {
				  case M_DATA:
					dp->dp_wbp = wbp;
					break;
				  case M_DELAY:
					dp->dp_state |= DP_TIMEOUT;
					dp->dp_tid =
					    timeout(cdDelay, (caddr_t)dp,
							 *(int*)wbp->b_rptr);
					freemsg(wbp);
					return (0);
				  case M_IOCTL:
					if (cdInterruptIoctl(dp, wbp))
						return (dp->dp_bit);
					continue;
				  default:
#ifdef	SVR0
					printf("ttyc%d: bad msg %d\n",
							dp->dp_index,
							wbp->b_datap->db_type);
					panic("bad streams msg");
#else
					cmn_err(CE_PANIC, "ttyc%d: bad msg %d",
							  dp->dp_index,
						      wbp->b_datap->db_type);
#endif
				}
			}
			if (wbp->b_rptr >= wbp->b_wptr) {
				ASSERT(wbp->b_datap->db_type == M_DATA);
				dp->dp_wbp = rmvb(wbp,wbp);
				freeb(wbp);
				continue;
			}
			c = *wbp->b_rptr++;
		}
#ifdef	MIPSEB
		*obuf++ = c;
#endif
#if defined(MIPSEL) || defined(PM2) || defined(IP2)
		*(obuf + (xmitcount^1)) = c;
#endif
		xmitcount++;
	}

	if (xmitcount == 0) {
		cdStop(dp);
		return (0);
	} else {
		register volatile struct device *d;

printf("x");
		/*
		 * Fill in this ports command information for the data
		 * just loaded into the output buffer.
		 */
		d = dp->dp_device;
		d->cmd = CMD_SEND_0;
		d->cmdHigh = xmitcount >> 8;
		d->cmdLow = xmitcount;
		d->cmdStatus = 0xFF;
		cdStartCmd(dp);
		dp->dp_state |= DP_XMIT;
		return (dp->dp_bit);
	}
}

/*
 * Start transmitting
 *	should be called with interrupts off
 */
static void
cdStartOutput(dp)
	struct cdport *dp;
{
	if ((dp->dp_state & (DP_TIMEOUT|DP_BREAK|DP_BREAK_QUIET|DP_XMIT)) ||
	    (cdOutput(dp) == 0))
		return;
    
#ifdef	notdef
	/* start command */
printf("s");
	cdStartCmd(dp);
#endif
}

/*
 * slow and hopefully infrequently used function to put characters
 *	somewhere where they will go up stream.
 */
static void
cdStuff(dp, c)
	register struct cdport *dp;
	unchar c;
{
	register mblk_t *bp;

	if (dp->dp_iflag & IBLKMD)
		return;

	/* get buffer if we don't have one */
	if (!(bp = dp->dp_rbp) && !(bp = cdGetNewbp(dp, BPRI_HI))) {
		dp->dp_allocb_fail++;;
		return;
	}
	*bp->b_wptr = c;
	if (++bp->b_wptr >= bp->b_datap->db_lim) {
		/* send buffer when full */
		(void) cdGetNewbp(dp, BPRI_LO);
	}
}

/*
 * Handle receive interrupt
 */
static int
cdReceiveInterrupt(dp)
	struct cdport *dp;
{
	register volatile struct device *d;
	register ushort emptyPtr;
	register mblk_t *bp;
	register unchar c;
	register unchar err;
	int bit, needSend;

printf("r");
	d = dp->dp_device;
	bit = 0;

	/* must be open to input */
	if (((dp->dp_state & (DP_ISOPEN|DP_WOPEN)) == 0)
#ifdef	XXXGL2
	    && !gl_portinuse(dp->dp_index + SIOOFFSET)
#endif
	    ) {
		cdHangup(dp);
		return (bit);
	}


	/* must be reading, to read */
	if (!(dp->dp_cflag & CREAD)
#ifdef	XXXGL2
	    && !gl_portinuse(dp->dp_index + SIOOFFSET)
#endif
	    ) {
		cdReceiveClear(dp);
		return (bit);
	}

	/* handle each input character */
	needSend = 0;
	while (d->inputFillPtr != (emptyPtr = d->inputEmptyPtr)) {
#ifdef	MIPSEB
		c = d->inputBuf[emptyPtr];
		err = d->errorBuf[emptyPtr];
		d->inputEmptyPtr = (emptyPtr + 1) & (sizeof(d->inputBuf) - 1);
#endif
#if defined(MIPSEL) || defined(PM2) || defined(IP2)
		emptyPtr = swapb(emptyPtr);
		c = d->inputBuf[emptyPtr ^ 1];
		err = d->errorBuf[emptyPtr ^ 1];
		d->inputEmptyPtr = swapb(emptyPtr + 1);
#endif

#ifdef	XXX
		/* see if graphics wants the character */
		if (gl_softintr(dp->dp_index + SIOOFFSET, c)) {
			continue;
		}
#endif

		/*
		 * start or stop output (if permitted) when we
		 * get XOFF or XON
		 */
		if (dp->dp_iflag & IXON) {
			register unchar cs = c & 0x7f;

			/* XXX */
			if ((dp->dp_state & DP_TXSTOP) &&
			    ((cs == dp->dp_startc) ||
			     ((dp->dp_iflag & IXANY) &&
			      ((cs != dp->dp_stopc) ||
			       (dp->dp_line == LDISC0))))) {

printf("^Q rcvd\n");
				/* restart output, if none pending */
				if ((dp->dp_state & DP_BUSY) == 0)
					bit |= cdOutput(dp);
				dp->dp_state &= ~DP_TXSTOP;
				if (cs == dp->dp_startc)
					continue;
			} else
			if (dp->dp_state & DP_LIT) {
				dp->dp_state &= ~DP_LIT;
			} else
			if (cs == dp->dp_stopc) {
				dp->dp_state |= DP_TXSTOP;
				cdStop(dp);		/* stop output */
				continue;
			} else
			if (cs == dp->dp_startc) {
				continue;	/* ignore extra control-Qs */
			} else
			/* just note escape */
			if ((cs == dp->dp_litc) && (LDISC0 != dp->dp_line)) {
				dp->dp_state |= DP_LIT;
			}
		}

		if (err & (ERRB_FRAMING | ERRB_OVERRUN | ERRB_PARITY)) {
			if (err & ERRB_OVERRUN)
				dp->dp_overruns++;

			/* if there was a BREAK	*/
			if ((err & ERRB_FRAMING) && (c == 0)) {
				if (dp->dp_iflag & IGNBRK)
					continue;	/* ignore it if ok */
				if (dp->dp_iflag & BRKINT) {
					(void)putctl1(dp->dp_rq->q_next,
						      M_FLUSH,FLUSHRW);
					(void)putctl1(dp->dp_rq->q_next,
						      M_PCSIG, SIGINT);
					continue;
				}
				c = '\0';
			} else
			if (IGNPAR & dp->dp_iflag)
				continue;
			else
			if (!(INPCK & dp->dp_iflag)) {
				/* ignore input parity errors if asked */
			} else
			if (err & (ERRB_PARITY|ERRB_FRAMING)) {
				if (err & ERRB_FRAMING)
					dp->dp_framingErrors++;
				if (dp->dp_iflag & PARMRK) {
					cdStuff(dp, 0377);
					cdStuff(dp, 0);
				} else
					c = '\0';
			}
		} else
		if (dp->dp_iflag & ISTRIP) {
			c &= 0x7f;
		} else
		if (c == 0377 && (dp->dp_iflag & PARMRK)) {
			cdStuff(dp, 0377);
		}

		/* get a buffer if we have none */
		if (!(bp = dp->dp_rbp) && !(bp = cdGetNewbp(dp, BPRI_HI))) {
			/* drop character if no buffer available */
			dp->dp_allocb_fail++;
			continue;
		}
		*bp->b_wptr = c;
		if (++bp->b_wptr >= bp->b_datap->db_lim) {
			/* send buffer upstream when full */
			(void) cdGetNewbp(dp, BPRI_LO);
		}
		needSend = 1;
	}
	if (needSend) {
		/* enable read service routine now that some data is ready */
		if ((dp->dp_rq != NULL) && canenable(dp->dp_rq))
			qenable(dp->dp_rq);
	}
	return (bit);
}

/*
 * Process status change interrupt
 */
static int
cdStatusInterrupt(dp)
	register struct cdport *dp;
{
printf("s");
	dp->dp_uartStatus = dp->dp_device->uartStatus;
	return (0);
}

#if defined(PM2) || defined(IP2)
int
cdintr()
{
	register int i;
	register int rv;

	rv = 0;
	for (i = 0; i < NCD; i++) {
		if (cdprobed[i])
			rv |= cdsiointr(i);
	}
	return (rv);
}
#endif

int
cdsiointr(board)
	int board;
{
	register struct cdport *dp;
	register volatile struct iodevice *iod;
	register int bit;
	register int pisf;
	int unit;
	int bits;

printf(".");
	unit = board * CDUPC;
	if (cdprobed[unit] == 0) {
		/* interrupt is too early */
		cdearly[unit]++;
		return (0);
	}

	/*
	 * Record status flag for each port that wants attention.
	 * Then clear board interrupt.
	 */
again:
	dp = &cdport[unit];
	iod = dp->dp_iodevice;
	pisf = dp->dp_device->portIntStatusFlag;
if (pisf == 0) printf("0");
#if defined(PM2) || defined(IP2)
	/*
	 * Check portIntStatusFlag and see if this interrupt was for this
	 * board.  If not, return now.
	 */
	if (pisf == 0)
		return (0);
#endif
	for (bit = 1; bit < (1 << CDUPC); bit <<= 1, dp++) {
		if ((bit & pisf) == 0) {		/* nothing doing */
if (dp->dp_device->intSourceFlag) printf("<HUH?>");
if (dp->dp_device->portIntStatusFlag) printf("<boing %x>", dp->dp_device->portIntStatusFlag);
			continue;
		}
		/* stash isf for later */
		dp->dp_isf = dp->dp_device->intSourceFlag;
	}

	/*
	 * Now start up new commands, based on interrupt status.
	 * Record in "bits" the new ports that wish attention.
	 */
	bits = 0;
	dp = &cdport[unit];
	for (bit = 1; bit < (1 << CDUPC); bit <<= 1, dp++) {
		if ((bit & pisf) == 0)			/* nothing doing */
			continue;
		if (dp->dp_isf & ISF_INPUT)		/* input interrupt */
			bits |= cdReceiveInterrupt(dp);
		else
		if (dp->dp_isf & ISF_STATUS)		/* status interrupt */
			bits |= cdStatusInterrupt(dp);
		else
		if ((dp->dp_isf & ISF_OUTPUT) ||
		    (dp->dp_device->outputBusy == 0)) {	/* output interrupt */
if (!(dp->dp_isf & ISF_OUTPUT)) printf("<!output>");
if (dp->dp_device->outputBusy) printf("<BUSY>");
			dp->dp_state &= ~DP_XMIT;
			bits |= cdOutput(dp);
		}
		dp->dp_isf = 0;
	}

	/* clear interrupt */
	cdPoll(iod, unit);
	iod->cmdPort = 0;
	wbflush();
	return (1);
}

#if defined(PM2) || defined(IP2)
/*
 * Probe for the board at "reg"
 */
static int
cdProbe(reg)
	long reg;
{
	register int i;
	register struct cdport *dp;
	register volatile struct device *d;
	register volatile struct iodevice *iod;
	register int unit;
	long timo;
	static char nextBoard;

	/* just double checking the compiler ... */
	ASSERT(sizeof(struct device) == 8192);

	cdio[nextBoard] = iod = (struct iodevice *) (MBIO_VBASE + reg ^ 1);
	iod->cmdPort = 0;			/* poke */

	/*
	 * If we get here, the board probed.  Wait for board to come up
	 * to a clean state
	 */
	timo = 100000;
	while (((iod->hostStatusPort & HSP_READY) == 0) && --timo) {
		DELAY(200);
	}
	if (timo == 0) {
		printf("\nttyc%d: board reset timed out, err=%s (%x)\n",
				  unit, cdErrorMsg(d->outputBuf0[0^1]),
				  d->outputBuf0[0^1]);
		return (CONF_DEAD);
	}
#ifdef	PM2
	/*
	 * XXX map the board in, somewhere?
	 */
#endif
#ifdef	IP2
	cdmem[nextBoard] = (struct device *)
		(SEG_MBMEM + CDMEMBASE + nextBoard*CDMEMSIZE);
#endif

	/*
	 * Get firmware revision from first port on the board.
	 */
	unit = nextBoard * CDUPC;
	dp = &cdport[unit];
	d = cdmem[nextBoard];
	for (i = 0; i < CDUPC; i++, dp++, d++) {
		dp->dp_index = unit + i;
		dp->dp_bit = 1 << i;
		dp->dp_device = d;
		dp->dp_iodevice = cdio[nextBoard];
		if ((cdCmd(dp, CMD_SET_INPUT, CDIPL, 0) == FALSE) ||
		    (cdCmd(dp, CMD_SET_OUTPUT, CDIPL, 0) == FALSE) ||
		    (cdCmd(dp, CMD_SET_STATUS, CDIPL, 0) == FALSE) ||
		    (cdCmd(dp, CMD_SET_PARITY, CDIPL, 0) == FALSE)) {
			return (CONF_DEAD);
		}
	}
	printf("(firmware revision %d) ",
		swapb(cdport[unit].dp_device->firmwareRev));
	cdprobed[nextBoard] = 1;
	if (cdearly[nextBoard]) {
		/*
		 * Call interrupt routine if we received an early interrupt
		 */
		(void) cdintr();
	}
	nextBoard++;
	return (CONF_ALIVE);
}
#endif

#ifdef	SVR3
/*
 * Probe the given central data board
 */
cdsioedtinit(e)
	struct edt *e;
{
	register struct cdport *dp;
	register volatile struct device *d;
	register volatile struct iodevice *iod;
	register int i;
	register int unit;
	register int ipl, vec;
	int board;
	long timo;

	/* just double checking the compiler ... */
	ASSERT(sizeof(struct device) == 8192);

	board = e->e_intr_info->v_unit;
	unit = board * CDUPC;
	d = (volatile struct device *) e->e_base;
	iod = (struct iodevice *) (d + 7);
	/*
	 * Probe board by writing a zero.  This also has the side effect
	 * of stopping the board diagnostics.
	 */
	if (wbadaddr(&iod->cmdPort, sizeof(iod->cmdPort))) {
		printf("ttyc%d: missing\n", unit);
		return;
	}

	/* wait for board to come up to a clean state */
	timo = 100000;
	while (((iod->hostStatusPort & HSP_READY) == 0) && --timo) {
		DELAY(100);
	}
	if (timo == 0) {
		if (showconfig)
			printf("ttyc%d: board reset timed out, err=%s (%x)\n",
					unit,
#ifdef	MIPSEB
					cdErrorMsg(d->outputBuf0[0]),
					d->outputBuf0[0]
#endif
#if defined(MIPSEL) || defined(PM2) || defined(IP2)
					cdErrorMsg(d->outputBuf0[0^1]),
					d->outputBuf0[0^1]
#endif
					);
		return;
	}

	vec = e->e_intr_info->v_vec;
	ipl = e->e_intr_info->v_brl | INT_ENABLE | INT_ROAK;

	/* initialize data structures */
	dp = &cdport[unit];
	for (i = 0; i < CDUPC; i++, dp++, d++) {
		dp->dp_device = d;
		dp->dp_iodevice = iod;
		dp->dp_bit = 1 << i;
		dp->dp_index = i + unit;

		/* program interrupt vectors and levels */
		if ((cdCmd(dp, CMD_SET_INPUT, ipl, vec) == FALSE) ||
		    (cdCmd(dp, CMD_SET_OUTPUT, ipl, vec) == FALSE) ||
		    (cdCmd(dp, CMD_SET_STATUS, ipl, vec) == FALSE) ||
		    (cdCmd(dp, CMD_SET_PARITY, ipl, vec) == FALSE)) {
			if (showconfig)
			    printf("ttyc%d: board isn't working correctly\n",
					    unit);
			return;
		}
	}
	d = (volatile struct device *) e->e_base;
	if (showconfig) {
		printf("ttyc%d: firmware revision %d, ipl %d, vec 0x%x\n",
				unit, d->firmwareRev,
				e->e_intr_info->v_brl, vec);
	}
	cdprobed[board] = 1;
	/*
	 * If interrupt fired off during probe then board was hung from a
	 * previous boot.  Handle interrupt now, clearing its state.
	 */
	if (cdearly[board]) {
		cdsiointr(board);
	}
}
#endif
