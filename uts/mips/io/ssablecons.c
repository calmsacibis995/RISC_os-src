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
#ident	"$Header: ssablecons.c,v 1.9.1.2 90/05/10 05:32:49 wje Exp $"

/* streams driver for the Sable console
 *	asynchronous receiver (synchrounous transmitters)
 */

#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/fs/s5dir.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/file.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/debug.h"
#include "sys/callo.h"
#include "sys/sablecons.h"
#include "sys/bc.h"
#include "sys/ctlspace.h"
#include "sys/ss.h"
#include "sys/sysinfo.h"

#define CN_BMAX 	2
#define CN_LMAX	1
#define CN_OUTCMAX	2048

extern int SCONS0_BASE[], SCONS1_BASE[];	/* Indexed by machine_type */

extern struct stty_ld def_stty_ld;


int	cn_act(), cn_zap(), cn_outc();

struct ss_devdep_info cndd =
        { "CN", SS_TTY, CN_BMAX, CN_LMAX, CN_OUTCMAX,
           cn_act, cn_zap, cn_outc, ss_null, ss_null,
	   ss_null
        };

struct ss_struct cnboard[CN_BMAX];
struct ss_line cnlines[CN_BMAX][CN_LMAX];



/* stream stuff */
static struct module_info cnm_info = {
	STRID_CONS,			/* module ID */
	"CONS",				/* module name */
	0,				/* minimum packet size */
	1024,				/* maximum packet size--1 sec@9600 */
	128,				/* hi-water mark */
	16,				/* lo-water mark */
};

extern struct callout calltodo;		/* first item of callout table */

static int cn_open();

static struct qinit cn_rinit = {
	NULL, ss_rsrv, cn_open, ss_close, NULL, &cnm_info, NULL
};

static struct qinit cn_winit = {
	ss_wput, NULL, NULL, NULL, NULL, &cnm_info, NULL
};

/* external definition of this streams driver */
struct streamtab cninfo = {&cn_rinit, &cn_winit, NULL, NULL};

/*
 * each port has a data structure that looks like this
 */
struct dport{
	struct scons_device *dp_scp;	/* port device addresses	    */

	int	dp_fe;			/* framing error counts   */

	struct	ss_line *dp_dpl;
};

struct dport sdports[] = {
	{ (struct scons_device *)0x0, 0, &cnlines[0][0]},
	{ (struct scons_device *)0x0, 0, &cnlines[1][0]}
};

/* turn off TX interrupts
 *	Interrupts should be made safe before coming here.
 *	(Sable console does not have TX interrupts.)
 */
#define cn_stop(dpl)	wbflush()

#ifndef cn_stop(dpl)
static int
cn_stop(dpl)
	struct ss_line *dpl;
{
	wbflush();
}
#endif cn_stop(dpl)

/* gobble any waiting input
 *	this should be called only when safe from interrupts
 */
static int
cn_rclr(dpl)
	struct ss_line *dpl;
{
	struct dport *dp = (struct dport *) dpl->ss_llocal;
	register struct scons_device *scp = dp->dp_scp;
	register u_char c;

	while (scp->sc_status & SC_STAT_RXRDY) {
		c = scp->sc_rx;
#ifdef lint
		c = c;
#endif
	}
	wbflush();
}

/* activate a port
 *	this should be called only when safe from interrupts
 */
static int				/* return !=0 if carrier present */
cn_act(dpl)
	struct ss_line *dpl;
{
	struct dport *dp = (struct dport *) dpl->ss_llocal;
	unsigned long o_status;

	o_status = dp->dp_scp->sc_status;
	if (!(o_status & SC_CMD_RXIE)) {	/* if now enabling input, */
		cn_rclr(dpl);			/* gobble old stuff */
		dp->dp_scp->sc_command = SC_CMD_RXIE;
	};
}

/* shutdown a port
 *	this should be called only when safe from interrupts
 */
static int
cn_zap(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;

	dp->dp_scp->sc_status &= ~SC_CMD_RXIE;	/* disable RX interrupts */
	/* cn_flushw(dpl); */			/* forget pending output */
	cn_stop(dpl);
	cn_rclr(dpl);
}

/* initialize the console ports
 *	This is done to allow debugging soon after booting.
 */

cninit()
{
	register struct dport *dp;

	sdports[0].dp_scp = 
		(struct scons_device *)MACHDEP(SCONS0_BASE);
	sdports[1].dp_scp = 
		(struct scons_device *)MACHDEP(SCONS1_BASE);

	for (dp = sdports; dp < &sdports[CN_BMAX]; dp++) {
		dp->dp_dpl->ss_line = 0;
		dp->dp_dpl->pss = cnboard + (dp - sdports);
		dp->dp_dpl->ss_llocal = (char *) dp;
		dp->dp_dpl->pss->ss_nlines = CN_LMAX;
		dp->dp_dpl->pss->ss_devdep = &cndd;
		dp->dp_dpl->pss->ss_addr = (caddr_t) dp->dp_scp;
		dp->dp_dpl->ss_lenable = 1;
		ss_cont_i(dp->dp_dpl, IFLAGS, &def_stty_ld.st_termio);

		if (IS_R6300)
		    *(volatile uint *)CSR_IVECTMASK |= CSR_IVECTSET_DUART;
	}
}

/* open a stream DUART port
 */
static int
cn_open(rq, dev, flag, sflag)
queue_t *rq;				/* our new read queue */
dev_t dev;
int flag;
int sflag;
{
	register ushort port;

	if (sflag)			/* only a simple stream driver */
		return(OPENFAIL);

	port = minor(dev);
	if (port >= CN_BMAX) {		/* fail if bad device # */
		return(OPENFAIL);
	}

    	return(ss_open (sdports[port].dp_dpl, rq, dev, flag, sflag, IFLAGS));
}

/* process interrupt for a single CONSOLE
 * (Sable's console device has only receive interrupts0
 */
cn_intr()
{
	register struct dport *dp;

	for (dp = sdports; dp < (sdports + CN_BMAX); dp++) {
		while (dp->dp_scp->sc_status & SC_STAT_RXRDY) {
			cn_rx(dp->dp_dpl);
		}
	}
}

/* input interrupt
 */
static int
cn_rx(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register u_char c;
	register struct scons_device *scp = dp->dp_scp;
	register mblk_t *bp;

	sysinfo.rcvint++;
	/* must be open to input
	 */
	if (!((SS_ISOPEN|SS_WOPEN) & dpl->ss_state)
	    ) {
		cn_zap(dpl);
		return;
	}

	/* must be reading, to read */
	if (!(dpl->ss_cflag & CREAD)) {
		cn_rclr(dpl);
		return;
	}

	/* process all available characters
	 */
	while (scp->sc_status & SC_STAT_RXRDY) {
		c = scp->sc_rx;

		/*
		 * Start or stop output (if permitted)
		 */
		if (! ss_startstop(dpl,(c & 0xFF)))
			continue;

		if ((dpl->ss_iflag & ISTRIP) == 0 &&
			   (c & 0xFF) == 0377 &&
			   dpl->ss_iflag & PARMRK) {
			ss_slowr(dpl,0377);
		};			

		ss_inc(dpl,c);
	}
	/*
	 *  Waiting a clock tick or three is much too long if
	 *  we're emulating under SABLE.  Reach into the callout
	 *  list and pretend we've seen enough clock ticks.
	 */
	{
		register struct callout *p1 = calltodo.c_next;
		if (p1 != 0) {
		    p1->c_time = 0;
		    timepoke();
		}
	}
}

/* 
 * This procedure outputs "len" chars pointed to by "cp" on line "cpl".
 * This procedure gets called from ss_tx.
 */
cn_outc(dpl, cs, len)
    register struct ss_line    *dpl;
    register char *cs;
    register int   len;
{
	register struct dport *dp;
	register struct scons_device *scp;
	register int i;

	dp = (struct dport *) dpl->ss_llocal;
	scp = dp->dp_scp;

    	if (len) {
		for (i = 0; i < len; i++) 
			scp->sc_txbuf = *cs++;
		wbflush();
		scp->sc_command |= SC_CMD_TXFLUSH;
		sysinfo.outch += len;
		if (dpl->ss_wbp)
			dpl->ss_wbp->b_rptr += len;
	 }
	ss_start(dpl);
}



/*
 * Print a character on console. (buffered)
 */
cnputc(c)
register int c;
{
	register struct scons_device *scp = sdports[0].dp_scp;

	scp->sc_txbuf = c;
	if (c == '\n')
		scp->sc_txbuf = '\r';
	scp->sc_command |= SC_CMD_TXFLUSH;
}

/*
 * Print a character on console. (un-buffered)
 */
cnputc_flush(c)
register int c;
{
	register struct scons_device *scp = sdports[0].dp_scp;

	if (c == '\n') {
		scp->sc_txbuf = c;
		scp->sc_tx = '\r';
	} else if (c == '\0') {
		scp->sc_command |= SC_CMD_TXFLUSH;
	} else {
		scp->sc_tx = c;
	}
}
/*
 * Read a character from the console.
 */
cngetc()
{
	register struct scons_device *scp = sdports[0].dp_scp;

	while (!(scp->sc_status & SC_STAT_RXRDY))
		continue;
	return((int)scp->sc_rx);
}


