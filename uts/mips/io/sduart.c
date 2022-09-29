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
#ident	"$Header: sduart.c,v 1.56.1.9.1.3.1.5 91/01/14 19:28:32 beacker Exp $"

/* streams driver for signetics SCN2681 (motorola SCN68681) dual
 *	asynchronous receiver/transmitters.
 *
 */

#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/fs/s5dir.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/file.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/sysinfo.h"
#include "sys/debug.h"
#include "sys/ss.h"
#include "sys/ioa.h"
#include "bsd43/sys/ioctl.h"
#ifdef R6000
#include "sys/bc.h"
#endif R6000

#ifdef R6000
#undef R2300
#endif

/* maximum # of duarts we have */
#ifdef R6000
#define NUMDUARTS	1
#define PIN28		1	/* 28 pin chip */
#undef  PIN40			/* not 40 pin chip */
#endif
#if defined(R2300)
#define	NUMDUARTS	2
#define PIN28		1	/* 28 pin chip */
#undef  PIN40			/* not 40 pin chip */
#endif

#define LN_DUARTPORTS	1		/* ports/DUART */
#define DUARTPORTS	(1<<LN_DUARTPORTS)
/* This is assumed to be (2**n)-1 */
#define	MAXPORT	   (NUMDUARTS*DUARTPORTS)	/* maximum port #. */

#define DU_BMAX 	NUMDUARTS
#define DU_LMAX		DUARTPORTS
#define DU_OUTCMAX	1

extern int DUART0[], DUART1[];			/* Lboot'able addresses */

#define UNTIMEOUT_ID untimeout
#ifdef R6000
#define WBFLUSH() { DELAY(16); }
#else
#define WBFLUSH() { wbflush(); DELAY(256); }
#endif
#ifndef KOPT_NOGL
#define MIPS_GL
#endif


#define	CARRPRI	STIPRI			/* sleep for carrier at this */

#define DEBUGGING

static volatile int dp_timeid;		/* used for timeout value */

#define SILOMAX	256			/* Software silo */
struct dp_silo{
	uchar si_status;
	uchar si_c;
};

static void dp_dispatch();		/* read chars from the silo */

extern struct stty_ld def_stty_ld;

extern int prom_mon;
extern int console_panic_enabled;

int	du_enable_line(), du_disable_line(), du_outc(), du_setline(), du_modem_control();
int	du_driver_control();

struct ss_devdep_info dudd =
        { "DU", SS_TTY, DU_BMAX, DU_LMAX, DU_OUTCMAX,
           NULL, NULL, du_outc, du_setline, du_modem_control, 
	   du_driver_control
        };

struct ss_struct duboard[DU_BMAX];
struct ss_line dulines[DU_BMAX][DU_LMAX];

/* for forcing-kernel-dump-feature */
int about_to_crash = 0;
int panic_index = 0;



/*
 * streams stuff
 */

static struct module_info dum_info = {
	STRID_DUART,			/* module ID */
	"DUART",			/* module name */
	0,				/* minimum packet size */
	1024,				/* maximum packet size */
	128,				/* high water mark */
	16,				/* low water mark */
};

static int du_open();

static struct qinit du_rinit = {
	NULL, ss_rsrv, du_open, ss_close, NULL, &dum_info, NULL
};

static struct qinit du_winit = {
	ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab duinfo = {&du_rinit, &du_winit, NULL, NULL};


#ifdef mips
#define	SCNDCPORT	0		/* port for the secondary console */
#define PDBXPORT	1		/* XXX--kernel debugging port */
#else
#define	SCNDCPORT	1		/* port for the secondary console */
#define	GFXKEYPORT	0		/* port for the graphics keyboard */
#define	DIALBPORT	3		/* port for dial box use	 */
#endif

/*
 * Each DUART has the following addresses:
 *	These two structures define the device addresses of a single DUART
 *	port.  Each port has the address of one of each.  However, each pair
 *	of ports on a single DUART have identical 'chip' address.
 *
 *	The machines differ in how they decode the chip addresses.
 */
#undef ADDRP
#undef CAT
#define CAT(str) str


#if defined(R2300) || defined(R6000)
#define ADDRP(lab,n) u_char lab, CAT(padA)n,CAT(padB)n,CAT(padC)n
#endif
#ifdef mips
#define VOL volatile
#else
#define VOL
#endif
typedef VOL struct {			/* *** Port Address ***		*/
	ADDRP(pa_mr,0);			/* Mode Register		*/
	ADDRP(pa_sr,1);			/* Status Register		*/
	ADDRP(pa_cr,2);			/* Command Register		*/
	ADDRP(pa_rhr,3);		/* RX Holding Reg		*/
} PAD;
#define	pa_csr	pa_sr			/* Clock Select Register	*/
#define	pa_thr	pa_rhr			/* TX Holding Register		*/


typedef VOL struct {			/* *** Chip Address ***		*/
	ADDRP(ca_mra,0);		/* Mode Register A		*/
	ADDRP(ca_sra,1);		/* Status Register A		*/
	ADDRP(ca_cra,2);		/* Command Register A		*/
	ADDRP(ca_rhra,3);		/* RX Holding Reg A		*/
	ADDRP(ca_ipcr,4);		/* Input Port Change Reg	*/
	ADDRP(ca_isr,5);		/* Interrupt Status Reg		*/
	ADDRP(ca_ctu,6);		/* Counter/Timer Upper		*/
	ADDRP(ca_ctl,7);		/* Counter/Timer Lower		*/

	ADDRP(ca_mrb,8);		/* Mode Register B		*/
	ADDRP(ca_srb,9);		/* Status Register B		*/
	ADDRP(ca_crb,10);		/* Command Register B		*/
	ADDRP(ca_rhrb,11);		/* RX Holding Reg B		*/
	ADDRP(ca_x1,12);		/* Reserved (interrupt vector)	*/
	ADDRP(ca_iport,13);		/* Input Port			*/
	ADDRP(ca_start,14);		/* Start Counter Command	*/
	ADDRP(ca_stop,15);		/* Stop Counter Command		*/
} CAD;
#define	ca_acr	ca_ipcr			/* Aux Control Register		*/
#define	ca_imr	ca_isr			/* Interrupt Mask Register	*/
#define	ca_ctur	ca_ctu			/* C/T Upper Register		*/
#define	ca_ctlr	ca_ctl			/* C/T Lower Register		*/
#define	ca_opcr	ca_iport		/* Output Port Conf. Reg	*/
#define	ca_sopbc ca_start		/* Set Output Port Bits Command */
#define	ca_ropbc ca_stop		/* Reset Output Port Bits Command*/
#undef ADDRP
#undef CAT

#ifdef IP4
Consult rcs to find this old code
#endif

#ifdef IP2
Consult rcs to find this old code
#endif

#ifdef PM2
Consult rcrcs to find this old code
#endif

#if defined(R2300) || defined(R6000)
#define DUINCR	0x20
#endif

/* some bit definitions for the duart */

/* mr1 */
#define	MR1_ODDPARITY	0x04		/* odd parity	*/
#define	MR1_NOPARITY	0x10		/* no parity	*/

/* mr2 */
#define	MR2_1STOP	0x07		/* 1 stop bit	  */
#define	MR2_2STOP	0x0F		/* 2 stop bits	  */
#ifdef PIN40
#define	MR2_CTS		0x10		/* CTS enable */
#endif

/* sr */
#define	SR_RXREADY	0x01		/* receiver ready	*/
#define SR_RXFULL	0x02		/* receiver full	*/
#define	SR_TXREADY	0x04		/* xmit ready		*/
#define SR_TXEMPTY	0x08		/* xmit empty		*/
#define	SR_OVERRUN	0x10		/* overrun		*/
#define	SR_PERROR	0x20		/* parity error		*/
#define	SR_FRERROR	0x40		/* frame error		*/
#define	SR_RBREAK	0x80		/* break received	*/

/* cr */
#define	CR_RESETMR	0x10		/* reset MR pointer to be MR1	*/
#define CR_DISABLE_TX	0x08		/* disable tx			*/
#define CR_DISABLE_RX	0x02		/* disable rx			*/
#define CR_ENABLE_TX	0x04		/* enable tx			*/
#define CR_ENABLE_RX	0x01		/* enable tx			*/
#define	CR_RXRESET	0x20		/* reset receiver		*/
#define	CR_TXRESET	0x30		/* reset transmitter		*/
#define	CR_ERRCLR	0x40		/* clear error bits		*/
#define	CR_STARTBREAK	0x60		/* start a break		*/
#define	CR_STOPBREAK	0x70		/* stop a break			*/

/* opcr */
#if defined(R2300) || defined(R6000)
#define OPCR_INIT0	0x00		/* no output enables		*/
#define OPCR_INIT1	0x00
#endif

/* acr */
#if defined(R2300) || defined(R6000)
#define ACR_SETUP	0xEC		/* baud set 2, timer, extern clock*/
#endif

/* isr */
#define	ISR_TXA		0x01		/* port a transmitter is ready	*/
#define	ISR_TXB		0x10		/* port b transmitter is ready	*/
#define	ISR_RXA		0x02		/* port a receiver is ready	*/
#define	ISR_RXB		0x20		/* port b receiver is ready	*/
#define	ISR_DCD		0x80		/* enable carrier detect ints	*/

/* imr */
#define	IMR_DCD		0x80		/* enable carrier detect ints	  */
#define	IMR_COUNTER	0x08		/* enable counter ints		  */
#define	IMR_TXA		ISR_TXA		/* enable port a transmitter ints */
#define	IMR_TXB		ISR_TXB		/* enable port b transmitter ints */
#define	IMR_RXA		ISR_RXA		/* enable port a receiver ints	  */
#define	IMR_RXB		ISR_RXB		/* enable port b receiver ints	  */

#if defined(R2300) || defined(R6000)
#define	IPORT_DCDA	0x00		/* dcd input for A ports--not used */
#define	IPORT_DCDB	0x04		/* dcd input bit for B ports	*/
#endif

/* oport (used with ca_ropbc and ca_sopbc) */
#ifdef PIN40
#define	OPORT_RTSA	0x01		/* rts for port a */
#define	OPORT_RTSB	0x02		/* rts for port b */
#define	OPORT_DTRA	0x10		/* dtr for port a */
#define	OPORT_DTRB	0x20		/* dtr for port b */
#endif
#ifdef PIN28
#define	OPORT_RTSA	0x00		/* rts for port a */
#define	OPORT_RTSB	0x02		/* rts for port b */
#define	OPORT_DTRA	0x00		/* dtr for port a */
#define	OPORT_DTRB	0x01		/* dtr for port b */
#endif

#ifdef XXX
/* addresses of the chips */
#ifdef R2300
#define DUART0 (0x1e008002+K1BASE)
#define DUART1 (0x1e800000+K1BASE)
#endif
#endif XXX

/*
 * duart bit patterns for various speeds
 */
#define BAUDBAD		0x01
#define	BAUD75		0x00
#define	BAUD110		0x11
#define	BAUD134		0x22
#define	BAUD150		0x33
#define	BAUD300		0x44
#define	BAUD600		0x55
#define	BAUD1200	0x66
#define	BAUD1800	0xAA
#define	BAUD2400	0x88
#define	BAUD4800	0x99
#define	BAUD9600	0xBB
#define	BAUD19200	0xCC
#define	BAUD38400	0XDD

#if defined(PM2) || defined(R2300) || defined(R6000)
#define du_speeds_38 du_speeds_no38	/* no 38kb for some */
#endif
static int du_speeds_no38[16] = {
	BAUDBAD,	BAUDBAD,	BAUD75,		BAUD110,
	BAUD134,	BAUD150,	BAUDBAD,	BAUD300,
	BAUD600,	BAUD1200,	BAUD1800,	BAUD2400,
	BAUD4800,	BAUD9600,	BAUD19200,	BAUDBAD
};



static u_char duart_imr[NUMDUARTS];	/* shadow of IMR register on chip */

/*
 * each port has a data structure that looks like this
 */
struct dport{
	CAD	*dp_ca;			/* chip device addresses	    */
	PAD	*dp_pa;			/* port device addresses	    */

	u_char	*dp_imrp;		/* pointer to shadow IMR register   */
	int	*dp_speed;		/* pointer to baud rate table	    */
	u_char	dp_tximr;		/* transmitter-ready interrupt bit  */
	u_char	dp_rximr;		/* receiver-ready interrupt bit	    */

	u_char	dp_rts;			/* RTS bit for the port		    */
	u_char	dp_dtr_rts;		/* RTS & DTR bits for the port	    */
	u_char	dp_dcd;			/* input mask for carrier (dcd)     */

	int	dp_fe;			/* framing error count   */

	struct	ss_line *dp_dpl;
	int	dp_sspeed;		/* default speed for each port */

#ifdef DEBUGGING
    int li_overflow;			/* count of silo overflows */
    int li_silomaxfull;			/* max silo count */
#endif
	struct dp_silo *li_rptr;            /* silo read pointer */
	struct dp_silo *li_wptr;            /* silo write pointer */
	struct dp_silo *li_silo_start;      /* start of silo */
	struct dp_silo *li_silo_limit;      /* limit of silo */
	struct dp_silo li_silo[SILOMAX];    /* actual silo */
};

/*
 * Addresses set up in duinit
 */
struct dport dports[] = {
	/* chip 0, port A ; this is tty0 (console) */
	{(CAD *)0, (PAD *)0, &duart_imr[0], &du_speeds_no38[0],
	IMR_TXA, IMR_RXA, OPORT_RTSA, (OPORT_RTSA|OPORT_DTRA),
	IPORT_DCDA, 0, NULL, B9600 },

	/* chip 0, port B ; this is tty1 */
	{(CAD *)0, (PAD *)DUINCR, &duart_imr[0], &du_speeds_no38[0],
	IMR_TXB, IMR_RXB, OPORT_RTSB, (OPORT_RTSB|OPORT_DTRB),
	IPORT_DCDB, 0, NULL, B9600 },

	/* chip 1, port A ; this is tty2 ; dne on R3200|M6000 */
	{(CAD*)0, (PAD*)0, &duart_imr[1], &du_speeds_38[0],
	IMR_TXA, IMR_RXA, OPORT_RTSA, (OPORT_RTSA|OPORT_DTRA),
	IPORT_DCDA, 0, NULL, B9600 },

	/* chip 1, port B ; this is tty3 ; dne on R3200|M6000 */
	{(CAD*)0, (PAD*)DUINCR, &duart_imr[1], &du_speeds_38[0],
	IMR_TXB, IMR_RXB, OPORT_RTSB, (OPORT_RTSB|OPORT_DTRB),
	IPORT_DCDB, 0, NULL, B9600 },

};


/* given one port, find the other one on the chip */
#define OTHER(dp) (((dp)->dp_dpl->ss_line & 1) ? ((dp)-1) : ((dp)+1))
#define SECONDPORT(dp) ((dp)->dp_dpl->ss_line & 1)

#define	DPUNIT(m)	(((m)&0x7E)>>1)
#define	DPLINE(m)	((m)&0x01)

/* turn off TX interrupts
 *	Interrupts should be made safe before coming here.
 */
static
du_stop(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp;
	register u_char imr;

	dp = (struct dport *) dpl->ss_llocal;
	imr = (*dp->dp_imrp & ~dp->dp_tximr);
	*dp->dp_imrp = imr;
	dp->dp_ca->ca_imr = imr; WBFLUSH();
}



/* 
 * This procedure outputs "len" chars pointed to by "cp" on line "cpl".
 * This procedure gets called from ss_tx.
 */
du_outc(dpl, cs, len)
    register struct ss_line    *dpl;
    register char *cs;
    register int   len;
{
	register struct dport *dp;
	register u_char imr;

	dp = (struct dport *) dpl->ss_llocal;

	ASSERT(len <= 1);
    	if (len) {
		imr = *dp->dp_imrp;
	    	if (!(imr & dp->dp_tximr)) {
			imr |= dp->dp_tximr;
			dp->dp_ca->ca_imr = imr; 
			WBFLUSH();
			*dp->dp_imrp = imr;
		};
        	dpl->ss_state |= SS_BUSY;
		dp->dp_pa->pa_thr = *cs; 
		WBFLUSH();
		sysinfo.outch++;
	 }
}



/* gobble any waiting input
 *	this should be called only when safe from interrupts
 */
static
du_rclr(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register PAD *pa = dp->dp_pa;
	register u_char c;

	while (pa->pa_sr & SR_RXREADY) {
		c = pa->pa_rhr;
		not_dead_code(c);
#ifdef OS_CDEBUG
		if ((dp - dports) == SCNDCPORT
		    && '\001' == c)
			debug("ring");
#endif
#ifdef lint
		c = c;
#endif
	}

	pa->pa_cr = CR_ERRCLR; WBFLUSH();	/* clear any errors */
}



/* activate a port
 *	this should be called only when safe from interrupts
 */
static int du_act_flags();

static int				/* return !=0 if carrier present */
du_enable_line(dpl)
	struct ss_line *dpl;
{
	return(du_act_flags(dpl,dpl->ss_cflag));
}


static int				/* return !=0 if carrier present */
du_act_flags(dpl,cflag)
	struct ss_line *dpl;
	int cflag;
{
	register struct dport *dp;
	register u_char imr, o_imr;

	/* Enable ints (rx and dcd are done here; do tx later in du_stop) */
	dp = (struct dport *) dpl->ss_llocal;
	o_imr = *dp->dp_imrp;
  	imr = o_imr | dp->dp_rximr;
	if ( SECONDPORT(dp))     /* always watch carrier */
		imr |= IMR_DCD;		
	*dp->dp_imrp = imr;
	dp->dp_ca->ca_imr = imr; WBFLUSH();

	dp->dp_ca->ca_sopbc = dp->dp_dtr_rts; WBFLUSH();

	if (!(o_imr & dp->dp_rximr))	/* if now enabling input, */
		du_rclr(dpl);		/* gobble old stuff */

	dpl->ss_modem_state |= (BSD43_TIOCM_LE |
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS);
	if (! SECONDPORT(dp)) {
		dpl->ss_modem_state |= (BSD43_TIOCM_CD |
					BSD43_TIOCM_DSR);
		return(1);
	}
	if (dp->dp_ca->ca_iport & dp->dp_dcd) {	/* the input is low-true */
		return(0);
	} else {
		return(1);
	}
}



/* shutdown a port
 *	this should be called only when safe from interrupts
 */
static
du_disable_line(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp;
	register u_char imr;
	register struct ss_line *o_dpl;
	register struct dport *o_dp;

	dp = (struct dport *) dpl->ss_llocal;

#ifdef IP2GL
	if (gl_portinuse(dp - dports))
		return;
#endif

	/* Turn off all interrupts (rx (done here) and tx (in du_stop) and
	 * carrier-detect (done here)) */
	imr = *dp->dp_imrp;
#ifdef OS_CDEBUG
	if (!kdebug
	    || (dp - dports) != SCNDCPORT)
#endif
		imr &= ~dp->dp_rximr;

	o_dp = OTHER(dp);		/* watch carrier-detect only if */
	o_dpl = o_dp->dp_dpl;		/* other port is open & cares */

	*dp->dp_imrp = imr;
	/* Not necessary to update chip register here, since du_stop will */
	/* update it in a moment					  */
	/* dp->dp_ca->ca_imr = imr; WBFLUSH();				  */
	du_stop(dpl);
	du_rclr(dpl);

	/* Now drop rts, to let world know we are not listening... */
	dp->dp_ca->ca_ropbc = dp->dp_rts; WBFLUSH();
	dpl->ss_modem_state &= ~(BSD43_TIOCM_LE | 
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS |
				BSD43_TIOCM_ST |
				BSD43_TIOCM_SR |
				BSD43_TIOCM_RI);
	if (SECONDPORT(dp))
		dpl->ss_modem_state &= ~(BSD43_TIOCM_CD |
					BSD43_TIOCM_DSR);
}



/*
 * This procedure sets the baud, parity, ... line parameters contained in
 * "cflag" of the line "dpl".
 */
du_setline (dpl, cflag)
    register struct ss_line    *dpl;
    int cflag;
{
	register struct dport *dp;
	register int baud;
	register u_char mr1, mr2;
	register uint diff;

	dp = (struct dport *) dpl->ss_llocal;

	baud = (cflag & CBAUD);

	if (dpl->pss->ss_bconv[baud] == BAUDBAD) {
		return(EINVAL);
	}

	if (dpl == &dulines[0][0])  /* set default speed for tty0 only*/
		dp->dp_sspeed = baud;	

	diff = cflag ^ dpl->ss_cflag;

	if (diff &= (CBAUD|CSIZE|CSTOPB|PARENB|PARODD)) {
		register PAD *pa = dp->dp_pa;

		mr2 = ((cflag & CSTOPB) ? MR2_2STOP : MR2_1STOP);
#ifdef PIN40
		if (dpl->ss_state & SS_FLOW)
			mr2 |= MR2_CTS;
#endif

		mr1 = (cflag & CSIZE) >> 4;	/* & # of data bits */

		if (!(cflag & PARENB))
			mr1 |= MR1_NOPARITY;	/* no parity */
		else if (cflag & PARODD)	/* or even parity */
			mr1 |= MR1_ODDPARITY;	/* or odd parity */

		pa->pa_cr = CR_RXRESET; WBFLUSH();
		pa->pa_cr = CR_TXRESET; WBFLUSH();
		pa->pa_cr = CR_RESETMR; WBFLUSH();/* point to mr1 */
		pa->pa_mr = mr1; WBFLUSH();
		pa->pa_mr = mr2; WBFLUSH();
		pa->pa_csr = dpl->pss->ss_bconv[cflag & CBAUD]; WBFLUSH();

		pa->pa_cr = CR_ERRCLR; WBFLUSH();
		pa->pa_cr = CR_ENABLE_TX|CR_ENABLE_RX; WBFLUSH();

		if (du_act_flags(dpl,cflag)) {	/* turn line (back) on */
			dpl->ss_modem_state |= (BSD43_TIOCM_CD |
						BSD43_TIOCM_DSR);
		} else {
			dpl->ss_modem_state &= ~(BSD43_TIOCM_CD |
						 BSD43_TIOCM_DSR);
		};
	}
	return(0);
}



du_modem_control(dpl,op,value)
	struct	ss_line	*dpl;
	int	op;
	int	value;
{
	struct	dport *dp;

	dp = (struct dport *) dpl->ss_llocal;
	switch (op) {
	case SS_MC_STARTBREAK:
		dp->dp_pa->pa_cr = CR_STARTBREAK; WBFLUSH();
		break;

	case SS_MC_STOPBREAK:
		dp->dp_pa->pa_cr = CR_STOPBREAK; WBFLUSH();
		break;

	case SS_MC_ENABLEFLOW:
		dp->dp_ca->ca_sopbc = dp->dp_rts; WBFLUSH();
		break;

	case SS_MC_DISABLEFLOW:
		dp->dp_ca->ca_ropbc = dp->dp_rts; WBFLUSH();
		break;

	case SS_MC_ENABLEDTR:
		dp->dp_ca->ca_sopbc = (dp->dp_dtr_rts ^ dp->dp_rts);
		WBFLUSH();
		break;

	case SS_MC_DISABLEDTR:
		dp->dp_ca->ca_ropbc = (dp->dp_dtr_rts ^ dp->dp_rts);
		WBFLUSH();
		break;

	default:
		break;
	}
	return(0);
}


du_drain_timeout(dpl)
	struct	ss_line *dpl;
{
	wakeup((caddr_t) dpl);
}


du_driver_control(dpl,op,value)
	struct	ss_line	*dpl;
	int	op;
	int	value;
{
	struct	dport *dp;
	int	s;

	dp = (struct dport *) dpl->ss_llocal;
	switch (op) {
	case SS_DC_TIOCOUTQ:
		return((dpl->ss_state & SS_BUSY) ? 1 : 0);
		break;

	case SS_DC_DISABLELINE:
		return(du_disable_line(dpl));
		break;

	case SS_DC_ENABLELINE:
		return(du_enable_line(dpl));
		break;
case SS_DC_STOP:
		return(du_stop(dpl));

	case SS_DC_DRAINOUTPUT:
		/* wait for the last transmit interrupt (software drained) */
		ss_wait_for_not_busy(dpl);

		/* wait for the internal UART buffer to drain */
		while (! (dp->dp_pa->pa_sr & SR_TXEMPTY)) {
			if (delay2(HZ/20,STOPRI|PCATCH))
				break;
		};
		break;

	case SS_DC_SDRVFLUSH:
		s = spltty();
		if (dpl->ss_state & SS_BUSY) {
			du_stop(dpl);
			dpl->ss_state &= ~SS_BUSY;
			if (dpl->ss_state & SS_XBUSY) {
				dpl->ss_state &= ~SS_XBUSY;
			} else if (dpl->ss_wbp) {
				dpl->ss_wbp->b_rptr += dpl->ss_lcc;
			};				
			dpl->ss_lcc = 0;
		};
		splx(s);
		break;

	default:
		break;
	}
	return(0);
}


/* initialize the duart ports
 *	This is done to allow debugging soon after booting.
 */
int	du_init_disable_reset = 0; /* set non-zero to prevent reset */

du_init()
{
	register int port;
	register struct dport	*dp;
	register CAD		*ca;
	u_char			dummy;

#ifdef R6000
	/*
	 *  XXX  BootProm versions 5.33 and earlier do not initialize the
	 *    IOC's duart interrupt vector, so we may have a parity error
	 *    in this vector.  Clear the parity error now, rather than later
	 *    when we do the full IOC/GBA initialization.
	 */
	ioa_deposit_intr_vector(
	    PHYS_TO_K1(DUART0_STR_R6300 & ~(CTL_SPACE_UNIT_BSIZE-1))
		| IOA_DUART_INT_CMD1, 0 );
#endif R6000

	/*
	 * Since we are trying to do all at run-time, initialize ports
	 */

	for (dp = dports, port = 0; port < MAXPORT; port++, ++dp) {
		dp->dp_dpl = &dulines[DPUNIT(port)][DPLINE(port)];
		dp->dp_dpl->ss_line = DPLINE(port);
		dp->dp_dpl->pss = duboard + DPUNIT(port);
		dp->dp_dpl->ss_llocal = (char *) dp;
		dp->li_rptr = dp->li_wptr = dp->li_silo_start =
			 &dp->li_silo[0];
		dp->li_silo_limit = &dp->li_silo[SILOMAX];

		if (! (SECONDPORT(dp))) {
			dp->dp_dpl->pss->ss_lines = dp->dp_dpl;
			dp->dp_dpl->pss->ss_bconv = dp->dp_speed;
			dp->dp_dpl->pss->ss_nlines = DU_LMAX;
			dp->dp_dpl->pss->ss_devdep = &dudd;
			dp->dp_dpl->pss->ss_addr = (caddr_t) dp->dp_ca;
		};
	};

	dports[0].dp_ca = (CAD *)(MACHDEP(DUART0));
	dports[0].dp_pa = (PAD *)(MACHDEP(DUART0));
	dports[0].dp_dpl->ss_lenable = 1;
	dports[1].dp_ca = (CAD *)(MACHDEP(DUART0));
	dports[1].dp_pa = (PAD *)(MACHDEP(DUART0)+DUINCR);
	dports[1].dp_dpl->ss_lenable = 1;
	if (!IS_R3200 && !IS_M6000) {	/* M2000 and M6000 have 1 duart */
		dports[2].dp_ca = (CAD *)(MACHDEP(DUART1));
		dports[2].dp_pa = (PAD *)(MACHDEP(DUART1));
		dports[2].dp_dpl->ss_lenable = 1;
		dports[3].dp_ca = (CAD *)(MACHDEP(DUART1));
		dports[3].dp_pa = (PAD *)(MACHDEP(DUART1)+DUINCR);
		dports[3].dp_dpl->ss_lenable = 1;
	}

	ca = dports[0*DUARTPORTS].dp_ca;
	ca->ca_imr = 0; WBFLUSH();
	ca->ca_opcr = OPCR_INIT0; WBFLUSH();	/* clear the DUARTS */
	ca->ca_acr = ACR_SETUP; WBFLUSH();

	if (!IS_R3200 && !IS_M6000) {	/* M2000 and M6000 have 1 duart */
		ca = dports[1*DUARTPORTS].dp_ca;
		ca->ca_imr = 0; WBFLUSH();
		{
			ca->ca_opcr = OPCR_INIT1; WBFLUSH();
			ca->ca_acr = ACR_SETUP; WBFLUSH();
				/* baud rate for IP2, clock on PM2 */
#ifdef ACR_RATE1
			ca->ca_ctu = (u_char)(ACR_RATE1 >> 8);
			ca->ca_ctl = (u_char)(ACR_RATE1 & 0xFF);
#endif
		}
		dummy = ca->ca_start;		/* start clock on 2nd DUART */
		not_dead_code(dummy);

#ifdef OPCR_INIT2
		dports[4].dp_ca = (CAD *)(MACHDEP(DUART2));
		dports[4].dp_pa = (PAD *)(MACHDEP(DUART2));
		dports[4].dp_dpl->ss_lenable = 1;
		dports[5].dp_ca = (CAD *)(MACHDEP(DUART2));
		dports[5].dp_pa = (PAD *)(MACHDEP(DUART2)+DUINCR);
		dports[5].dp_dpl->ss_lenable = 1;
		ca = dports[2*DUARTPORTS].dp_ca;
		ca->ca_imr = 0;
		ca->ca_opcr = OPCR_INIT2;
		ca->ca_acr = ACR_SETUP;
#ifdef ACR_RATE2
		ca->ca_ctu = (u_char)(ACR_RATE2 >> 8);
		ca->ca_ctl = (u_char)(ACR_RATE2 & 0xFF);
		dummy = ca->ca_start;		/* start clock on 3rd DUART */
		not_dead_code(dummy);
#endif
#endif
	}

	(void) du_baud_defs();

	if (du_init_disable_reset)
		return;
		
	for (dp = &dports[0]; dp < &dports[MAXPORT]; dp++) { 
		register PAD *pa = dp->dp_pa;

		if (! dp->dp_dpl->ss_lenable) 
			continue;
		pa->pa_cr = CR_RXRESET; WBFLUSH();
		pa->pa_cr = CR_TXRESET; WBFLUSH();
		dp->dp_ca->ca_ropbc = dp->dp_dtr_rts; WBFLUSH();
	}

#ifdef SCNDCPORT
	/* initialize the secondary console port */
	dp = dports + SCNDCPORT;
	ss_cont_i(dp->dp_dpl, ((ushort)(dp->dp_sspeed | (IFLAGS & ~CBAUD))),
		&def_stty_ld.st_termio);
#endif

#ifdef PDBXPORT
	/* initialize kernel debuggin port */
	dp = dports + PDBXPORT;
	ss_cont_i(dp->dp_dpl, ((ushort)(dp->dp_sspeed | (IFLAGS & ~CBAUD))),
		&def_stty_ld.st_termio);
#endif

#ifdef GFXKEYPORT
	/* initialize the graphics keyboard port. */
	dp = dports + GFXKEYPORT;
	dp->dp_dpl->ss_iflag = IGNBRK;
	ss_cont_i(dp->dp_dpl, B600|CS8|PARODD|PARENB|CREAD|CLOCAL,
		(struct termio *) NULL);
#endif

}

/* open a stream DUART port
 */
static int
du_open(rq, dev, flag, sflag)
queue_t *rq;				/* our new read queue */
dev_t dev;
int flag;
int sflag;
{
	register struct ss_struct *dpb;
	register struct ss_line      *dpl;    /* line of interest */
	register int line;
	int	status;
	int	was_open;
	extern dev_t		console_dev, duart_dev;
	register struct dport	*dp;
	register ushort		cflag;

	if (sflag)			/* only a simple stream driver */
		return(OPENFAIL);

	if (DPUNIT(minor(dev)) >= DU_BMAX)
		return(OPENFAIL);
	dpb = duboard + DPUNIT(minor(dev));
	line = DPLINE(minor(dev));
	if (line >= dpb->ss_nlines)
		return(OPENFAIL);
	dpl = dpb->ss_lines + line;
	dp = (struct dport *) dpl->ss_llocal;
#ifdef IP2_GL
	if (gl_portinuse(dp - dports)) {
			/* if in use by graphics */
		u.u_error = ENXIO;
		return(OPENFAIL);
	};
#endif

	cflag = dp->dp_sspeed;
	cflag |= (IFLAGS & ~CBAUD);


    	return(ss_open (dpl, rq, dev, flag, sflag, cflag));
}


#undef POLL_DUARTS


#if defined(R2300) || defined(R6000)
#define POLL_DUARTS
du_poll()
{
	/* On M2000 and Excalibur duintr(2) should NOT be called - regression */
	while (duintr(0) || ((IS_R3200 || IS_M6000)? 0 : duintr(2)) ) continue;
}
#endif


/* process interrupt for a single DUART
 */
#ifdef POLL_DUARTS
static int
#endif
duintr(cn)
register int cn;			/* 1st port of chip that woke up */
{
	register struct dport *dp, *dp1;
	struct ss_line *dpl;
	struct ss_line *dpl1;
	register u_char isr;
	register u_char worked = 0;
	extern int ioa_ctlspace_vaddr[];

	ASSERT(cn < MAXPORT);
	dp = &dports[(ushort)cn];
	dp1 = dp+1;
	dpl = dp->dp_dpl;
	dpl1 = dp1->dp_dpl;

#ifndef POLL_DUARTS
	do {
#endif
		worked = 0;
		isr = dp->dp_ca->ca_isr & *dp->dp_imrp;

		/* first process input interrupts */
		if (isr & ISR_RXA)
			worked++, du_rx(dpl);
		if (isr & ISR_RXB)
			worked++, du_rx(dpl1);

		/* then carrier-detect interrupts */
		if (isr & ISR_DCD) {
			register u_char iport;	/* get state */
			sysinfo.mdmint++;
			iport = dp->dp_ca->ca_ipcr;	/* & reset change */
			if (iport & IPORT_DCDA) {
				if (dpl->ss_modem_state & BSD43_TIOCM_CD)
					worked++, ss_coff(dpl);
			} else {
				if (!(dpl->ss_modem_state & BSD43_TIOCM_CD))
					worked++, ss_con(dpl);
			}

			if (iport & IPORT_DCDB) {
				if (dpl1->ss_modem_state & BSD43_TIOCM_CD)
					worked++, ss_coff(dpl1);
			} else {
				if (!(dpl1->ss_modem_state & BSD43_TIOCM_CD))
					worked++, ss_con(dpl1);
			}
		}

		/* finally xmit data */
		if (isr & ISR_TXA)
			worked++, du_tint(dpl);
		if (isr & ISR_TXB)
			worked++, du_tint(dpl1);
#ifdef POLL_DUARTS
		if (IS_M6000) {
		    /*  reset IOC so another Duart Interrupt can be sent  */
		    *(volatile u_int *)(ioa_ctlspace_vaddr[0]
				         | IOA_ERRORINFO_REG) = IOC_DUART_INT;
		}
		return(worked);
#else
	} while (0 != worked);

	if (IS_M6000) {
		/*  reset IOC so another Duart Interrupt can be sent  */
		*(volatile u_int *)(ioa_ctlspace_vaddr[0] | IOA_ERRORINFO_REG)
			= IOC_DUART_INT;
	}
#endif
}


/*
 *	Transmit Interrupt
 */

du_tint(dpl)
	struct	ss_line *dpl;
{
	sysinfo.xmtint++;
	if (dpl->ss_state & SS_BUSY) {
		dpl->ss_state &= ~SS_BUSY;
		if (dpl->ss_state & SS_XBUSY) {
			dpl->ss_state &= ~SS_XBUSY;
		} else if (dpl->ss_wbp)
			dpl->ss_wbp->b_rptr++;
		dpl->ss_lcc = 0;
	};
	ss_tx(dpl);
}

/* input interrupt
 */
static
du_rx(dpl)
	struct	ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register u_char c;
	register u_char sr;
	register PAD *pa = dp->dp_pa;
	register mblk_t *bp;
#ifdef DEBUGGING
	int count;
#endif
	int spltty();

	sysinfo.rcvint++;
	/* must be open to input
	 */
	if (!((SS_ISOPEN|SS_WOPEN) & dpl->ss_state)
#ifdef IP2_GL
	    && !gl_portinuse(dp - dports)
#endif
	    ) {
#ifdef SCNDCPORT
		if ((dp - dports) == SCNDCPORT) {
			du_rclr(dpl);
			return;
		};
#endif SCNDCPORT
#ifdef PDBXPORT
		if ((dp - dports) == PDBXPORT) {
			du_rclr(dpl);
			return;
		};
#endif PDBXPORT
#ifdef GFXKEYPORT
		if ((dp - dports) == GFXKEYPORT) {
			du_rclr(dpl);
			return;
		};
#endif GFXKEYPORT
		du_disable_line(dpl);
		return;
	}


	/* must be reading, to read */
	if (!(dpl->ss_cflag & CREAD)
#ifdef IP2_GL
	    && !gl_portinuse(dp - dports)
#endif
	    ) {
		du_rclr(dpl);
		return;
	}


	/* process all available characters
	 */
	while ((sr = pa->pa_sr) & SR_RXREADY) {
		c = pa->pa_rhr;
		if (sr & (SR_FRERROR|SR_PERROR|SR_OVERRUN|SR_RBREAK)) {
			pa->pa_cr = CR_ERRCLR;	/* clear an error */
			WBFLUSH(); 
		}
		
		/* if user wants to panic the kernel */
		if(console_panic_enabled && about_to_crash) 
		  if(c == 'y') /* OK ! he asked for it */
			cmn_err(CE_PANIC,
			"Console rec'd 2 consecutive ctl-T's. Dumping core.\n");
		  else  { /* ignore his answer */
			panic_index = 0;
			about_to_crash=0;
			return;
		  }

		if (ss_startstop(dpl,(c & 0xFF))) 
			dp_putsilo(dpl, sr, c);
	}
#ifdef DEBUGGING
	/* update interesting stats */
	if ((count = dp->li_wptr - dp->li_rptr) < 0)
		count += SILOMAX;
	if (count > dp->li_silomaxfull)
		dp->li_silomaxfull = count;
#endif
	if (!dp_timeid)
#ifdef SABLE
		dp_timeid = timeout_spl(dp_dispatch, 0, 0, spltty);
#else
		dp_timeid = timeout_spl(dp_dispatch, 0, HZ/20, spltty);
#endif SABLE
}

/* Called from the callout table.  read chars and associated status
   from the silo and feed them to the receive interupt routine
*/
static void
dp_dispatch()
{
	uchar c;
	uchar status;
	register struct dport	*dp;
	register struct ss_line *dpl;

	dp_timeid = 0;

	for (dp = &dports[0]; dp < &dports[MAXPORT]; dp++) { 
		for (; dp->li_rptr != dp->li_wptr;){
			dp_getsilo(dp->dp_dpl,&status, &c);
			dp_rx1(dp->dp_dpl,status,c);
		}
	}
}

dp_getsilo(dpl,sr, c)
register struct	ss_line *dpl;
register uchar *sr;
register uchar *c;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register struct dp_silo *siloptr;

	siloptr = dp->li_rptr;

	*sr = siloptr->si_status;
	*c  = siloptr->si_c;

	if (++dp->li_rptr == dp->li_silo_limit)
		dp->li_rptr = dp->li_silo_start;
}

dp_putsilo(dpl, sr, c)
register struct	ss_line *dpl;
register uchar c;
register uchar sr;
{
    register struct dport *dp = (struct dport *) dpl->ss_llocal;
    register struct dp_silo *siloptr;

    siloptr = dp->li_wptr;
    siloptr->si_status = sr;
    siloptr->si_c      = c;

    if (++dp->li_wptr == dp->li_silo_limit)
	dp->li_wptr = dp->li_silo_start;

    if (dp->li_wptr == dp->li_rptr) {
#ifdef DEBUGGING
	++dp->li_overflow;
#endif
	dp->li_wptr--;
	if (dp->li_wptr < dp->li_silo_start)
	    dp->li_wptr = dp->li_silo_start;
    }
}
static void
panic_timeout()
{

    if(about_to_crash){
	cmn_err(CE_CONT, "\nTimed-out!You will need to enter 2 more ");
	cmn_err(CE_CONT, "ctl-T's if you want to crash the kernel.\n");
	about_to_crash = 0;
	panic_index = 0;
    }
}

dp_rx1(dpl, sr, c)
register struct ss_line    *dpl;
register int c;
register int sr;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	static char panic_array[]= {0x14, 0x14};

	if(console_panic_enabled) 
	{
	 if ((dp - dports) == SCNDCPORT) { /* really console port 0! */
	   if (c == panic_array[panic_index]) {	/*check if ctl-T is hit */
	     if (panic_index == 0 ) { /* one ctl-T has been entered */
	       ctl_t_msg();
	       panic_index++;	
	     } 
	     else {
	          about_to_crash = 1;/* set flag,2 cntl-T's have been entered */
		  cmn_err(CE_CONT, "Are you sure you want to do this? ");
		  cmn_err(CE_CONT, "You have 10 secs to answer -  y or n - ");
		  timeout_spl(panic_timeout,0,HZ*10,spltty);
	     }
	   } else {
	     	  panic_index = 0;	/* not a ctl-T, clear flag */
		  about_to_crash = 0;
	   }	
	 }
	}
#ifdef OS_CDEBUG
	if (kdebug		/* if we get a control-A, debug */
	    && (dp - dports) == SCNDCPORT	/* on this port */
	    && '\001' == (c & 0x7f)) {
		debug("ring");
		return;	
	}
#endif
#if defined(IP2_GL) || defined(MIPS_GL)
	if (gl_softintr((dp - dports), c))
		return;	/* give graphic input to graphics */
#endif

	if (SS_ISOPEN & dpl->ss_state){
		/* 
		 * Errors?
		 */
		if (sr & (SR_FRERROR|SR_PERROR|SR_OVERRUN|SR_RBREAK)) {

			if (SR_OVERRUN & sr){
				dpl->ss_overflow++;	/* count overrun */
#ifdef MIPS_LOCAL
				cmn_err(CE_CONT,"sduart: line %d overflow\n", dpl->ss_line);
#endif MIPS_LOCAL
			}

			if (SR_RBREAK & sr) {	/* if there was a BREAK	*/
#ifdef mips
#ifdef SCNDCPORT
				if ((dp - dports) == SCNDCPORT)
					cntl_p();
#endif SCNDCPORT
#endif
				if (dpl->ss_iflag & IGNBRK)
					return;	/* ignore it if ok */
				if (dpl->ss_iflag & BRKINT) {
					ss_mk_sig(dpl,SIGINT);
					return;  
				}
				if (dpl->ss_iflag & PARMRK) {
					ss_slowr(dpl,0377); 
					ss_slowr(dpl,0);   
				}
				c = '\0';

			} else if (IGNPAR & dpl->ss_iflag) {
				return;  

			} else if (!(INPCK & dpl->ss_iflag)) {
				/* ignore input parity errors if asked */

			} else if ((SR_PERROR|SR_FRERROR) & sr) {
				if (SR_FRERROR & sr){
					dp->dp_fe++;
					cmn_err(CE_CONT,"sduart: line %d framing error\n", 
						dpl->ss_line);
				}
				else
					cmn_err(CE_CONT,"sduart: line %d parity error\n", 
						dpl->ss_line);
				if (dpl->ss_iflag & PARMRK) {
					ss_slowr(dpl,0377);
					ss_slowr(dpl,0);
				} else {
					c = '\0';
				}
			}


		} else if ((dpl->ss_iflag & ISTRIP) == 0 &&
			   (c & 0xFF) == 0377 &&
			   dpl->ss_iflag & PARMRK) {
			ss_slowr(dpl,0377);
		};			

		ss_inc(dpl,c);
	}
}



/* read a character from a DUART port
 *	This function is not part of the driver.  It steals characters
 *	for the kgl or for debugging.
 */
int
duxgetchar(port, timo)
short port;
register long timo;			/* cycles around check loop */
{
	register PAD *pa;
	register u_char c;
	register int s;

	ASSERT(port < MAXPORT);

	pa = dports[port].dp_pa;
	s = spltty();
#ifndef mips
	if (timo) {			/* old grot */
		while (!(pa->pa_sr & SR_RXREADY)) {
			if (!--timo) return(-1);
		}
	} else
#endif
		while (!(pa->pa_sr & SR_RXREADY)) continue;
	c = pa->pa_rhr;
	splx(s);

	return(c);
}


/* read a character from the secondary console port.
 *	Delay forever waiting for the character.
 */
dugetchar()
{
	return( duxgetchar(SCNDCPORT, 0L) );
}


/* output to a duart port
 */
duxputchar(c, port)
register short c;
register short port;
{
	register PAD *pa = dports[port].dp_pa;
	register int s;

#ifndef mips
	/* ignore commands if char is negative */
	if ( c < 0 )			/* old grot */
		return;
#endif

	s = spltty();
	while (!(pa->pa_sr & SR_TXREADY))	/* wait for xmitter ready */
		continue;

	pa->pa_thr = c;	WBFLUSH();		/* send the character */
	splx(s);
}


/* output a character to the secondary console port.
 */
duputchar(c)
register short	c;
{
	register PAD *pa = dports[SCNDCPORT].dp_pa;
	register int s;

	duxputchar(c, SCNDCPORT);
#ifdef mips
	if (c == '\n')
		duxputchar('\r', SCNDCPORT);
#endif

	/* implement XON/XOFF */
	s = spltty();
	if ((pa->pa_sr & SR_RXREADY)
	    && (pa->pa_rhr & 0x7F) == CSTOP) {
		(void)dugetchar();
	}
	splx(s);
}


/* initializes default baud rates */
static
du_baud_defs()
{
	register int		port;
	extern char		*prom_getenv();
	int xx;

	/* get baud rate for A & B ports from NVRAM by asking prom routines */

	for (port = 0; port < MAXPORT; port++) {
		if (! dports[port].dp_dpl->ss_lenable)
			continue;

		switch (port) {
#ifndef SABLE
		case 0:
			xx = atoi(prom_getenv("lbaud"));
			not_dead_code(xx);
			dports[0].dp_sspeed = map_speed(atoi(prom_getenv("lbaud")));
			break;
		case 1:
			xx = atoi(prom_getenv("rbaud"));
			not_dead_code(xx);
			dports[1].dp_sspeed = map_speed(atoi(prom_getenv("rbaud")));
			break;
#endif !SABLE
		default:
			dports[port].dp_sspeed = SSPEED;
		}
		if (dports[port].dp_sspeed == BAUDBAD)
			dports[port].dp_sspeed = SSPEED;
	}
}

static int
map_speed(speed)
int speed;
{
	switch (speed) {
		case 0:		speed = B0;		break;
		case 75:	speed = B75;		break;
		case 110:	speed = B110;		break;
		case 134:	speed = B134;		break;
		case 150:	speed = B150;		break;
		case 300:	speed = B300;		break;
		case 600:	speed = B600;		break;
		case 1200:	speed = B1200;		break;
		case 2400:	speed = B2400;		break;
		case 4800:	speed = B4800;		break;
		case 9600:	speed = B9600;		break;
		case 19200:	speed = EXTA;		break;
		case 38400:	speed = EXTB;		break;
		default:	speed = -1;		break;
	}
	return (speed);
}
