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
#ident	"$Header: scc.c,v 1.1.1.9.1.7.1.5 90/12/20 18:09:40 beacker Exp $"

/*
 * streams driver for the SCC
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
#include "sys/callo.h"
#include "sys/ss.h"
#include "sys/scc.h"
#include "bsd43/sys/ioctl.h"
#include "sys/uart_ioctl.h"
#define DEBUGGING

union wd_by {
	struct	w_b {
		unsigned char	pad[3];
		unsigned char	by;
	} w_b;
	unsigned int	w_w;
};

int scc_fifo_flag_0;
int scc_fifo_flag_1;

struct scc_chan {
	union wd_by	scc_cmd;
	union wd_by	scc_data;
};
#define	scc_cmd_b	scc_cmd.w_b.by
#define	scc_cmd_w	scc_cmd.w_w
#define	scc_data_b	scc_data.w_b.by
#define	scc_data_w	scc_data.w_w

#define SCC_BMAX 	2
#define SCC_LMAX	1
#define SCC_OUTCMAX	1
#define SCC_OUTCMAX_F	4

extern struct stty_ld def_stty_ld;

extern int SCC_REG_BASE[];
extern int prom_mon;
extern int console_panic_enabled;

extern int mono, color;

/* for forcing-kernel-dump-feature */
int about_to_crash = 0;
int panic_index = 0;


int	scc_act(), scc_outc();
int	scc_enable_line(), scc_disable_line(), scc_outc(), scc_setline();
int	scc_modem_control(), scc_driver_control();

struct ss_devdep_info sccdd =
        { "SCC", SS_TTY, SCC_BMAX, SCC_LMAX, SCC_OUTCMAX,
           NULL, NULL, scc_outc, scc_setline, scc_modem_control,
	   scc_driver_control
        };

struct ss_devdep_info sccdd_f =
        { "SCC", SS_TTY, SCC_BMAX, SCC_LMAX, SCC_OUTCMAX_F,
           NULL, NULL, scc_outc, scc_setline, scc_modem_control,
	   scc_driver_control
        };

struct ss_struct sccboard[SCC_BMAX];
struct ss_line scclines[SCC_BMAX][SCC_LMAX];

struct scc_regs {
	unsigned char	scc_reg[16];
} scc_regs[SCC_BMAX];


/* stream stuff */
static struct module_info sccm_info = {
	STRID_CONS,			/* module ID */
	"SCC",				/* module name */
	0,				/* minimum packet size */
	1024,				/* maximum packet size--1 sec@9600 */
	128,				/* hi-water mark */
	16,				/* lo-water mark */
};

extern struct callout calltodo;		/* first item of callout table */

static int scc_open();

static struct qinit scc_rinit = {
	NULL, ss_rsrv, scc_open, ss_close, NULL, &sccm_info, NULL
};

static struct qinit scc_winit = {
	ss_wput, NULL, NULL, NULL, NULL, &sccm_info, NULL
};

/* external definition of this streams driver */
struct streamtab sccinfo = {&scc_rinit, &scc_winit, NULL, NULL};

/* Software silo to stash characters at interrupt time */
#define SILOMAX	256			/* Software silo */
struct scc_silo{
	uchar si_status;
	uchar si_c;
};

static void scc_dispatch();		/* drain chars from the silo */
static volatile int scc_timeid;		/* used for timeout value */

/*
 * each port has a data structure that looks like this
 */
struct dport{
	struct scc_chan *dp_scp;	/* port device addresses	    */
	struct scc_regs	*dp_sccregs;
	int	*dp_speed;		/* pointer to the baudrate table */

	int	dp_fe;			/* framing error counts   */

	struct	ss_line *dp_dpl;
	int	dp_sspeed;
	int	dp_state;

#ifdef DEBUGGING
    int li_overflow;			/* count of silo overflows */
    int li_silomaxfull;			/* max silo count */
#endif
	struct scc_silo *li_rptr;            /* silo read pointer */
	struct scc_silo *li_wptr;            /* silo write pointer */
	struct scc_silo *li_silo_start;      /* start of silo */
	struct scc_silo *li_silo_limit;      /* limit of silo */
	struct scc_silo li_silo[SILOMAX];    /* actual silo */
};

/*
 * state definitions
 */
#define	SCC_TIMESTAMP	(1<<0)		/* return timestamps */

#define	PORT_A	0		/* base + 8 channel */
#define	PORT_B	1		/* base channel */

#define	MOUSEPORT	0

#define	SCC_UNIT(m)	((m)&0x7F)

extern int scc_speeds[];

struct dport sccports[] = {
	{ (struct scc_chan *)0,
		&scc_regs[1], &scc_speeds[0], 0, &scclines[1][0], B9600},
	{ (struct scc_chan *)0,
		&scc_regs[0], &scc_speeds[0], 0, &scclines[0][0], B9600}
};

#define MAXPORT  sizeof(sccports)/sizeof(struct dport)

#ifdef mips
#define	SCNDCPORT	0	/* port for secondary console */
#define	PDBXPORT	1	/* XXX--kernel debugging port */
#endif

/*
 * to determine the time constant:
 *      TC = ( (MHZ {the clock rate} / (16 * 2 * BR)) - 2 )
 *		for a 16x clock
 */
#define	MHZ	10000000	/* base pclock for the scc part */

#define	BCONST	16		/* use this if you wish to change the divisor*/

#define	B_CONST(speed)	( (MHZ/(2*BCONST*speed)) - 2 )

static
int scc_speeds[16] = {
	B_CONST(5),	B_CONST(50),	B_CONST(75),	B_CONST(110),
	B_CONST(134),	B_CONST(150),	B_CONST(200),	B_CONST(300),
	B_CONST(600),	B_CONST(1200),	B_CONST(1800),	B_CONST(2400),
	B_CONST(4800),	B_CONST(9600),	B_CONST(19200),	B_CONST(38400)
};


unsigned int
scc_get_reg(scp, reg)
volatile struct scc_chan	*scp;
unsigned int	reg;
{
	scp->scc_cmd_w = reg;
	wbflush();
	DELAY(1);
	return (unsigned int)scp->scc_cmd_b;
}

void
scc_put_reg(scp, reg, val)
volatile struct scc_chan	*scp;
unsigned int	reg;
unsigned int	val;
{
	scp->scc_cmd_w = reg;
	wbflush();
	DELAY(1);
	scp->scc_cmd_w = val;
	wbflush();
}

/*
 * turn off TX interrupts
 *	Interrupts should be made safe before coming here.
 */
static int
scc_stop(dpl)
	struct ss_line *dpl;
{
	struct dport *dp = (struct dport *) dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register struct scc_regs *rp = dp->dp_sccregs;
	register char	rvl;

	rvl = rp->scc_reg[SCCW_ENAB_R] &= ~SCCW_ENAB_TxIntr;
	scc_put_reg(scp, SCCW_ENAB_R, rvl);
}

/*
 * gobble any waiting input
 *	this should be called only when safe from interrupts
 */
static int
scc_rclr(dpl)
	struct ss_line *dpl;
{
	struct dport *dp = (struct dport *) dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register u_char c, ce;

	while (scp->scc_cmd_b & SCCR_STATUS_RxDataAvailable) {
		if ((ce = scc_get_reg(scp, SCCR_SPECIAL_R)) & 
		    (SCCR_SPECIAL_FramingError | SCCR_SPECIAL_ParityError |
		     SCCR_SPECIAL_OverrunError)) {
			DELAY(1);
			scp->scc_cmd_w = SCCW_CMD_CMD_ResetError;
			wbflush();
		}
		DELAY(1);
		c = scp->scc_data_b;
		DELAY(1);
#ifdef lint
		c = c;
#endif
	}
	wbflush();
}

static int scc_act_flags();

static int
scc_enable_line(dpl)
	struct ss_line *dpl;
{
	return scc_act_flags(dpl, dpl->ss_cflag);
}

/*
 * activate a port
 *	this should be called only when safe from interrupts
 */
static int				/* return !=0 if carrier present */
scc_act_flags(dpl, cflag)
	struct ss_line *dpl;
	int cflag;
{
	struct dport *dp = (struct dport *) dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register unsigned char *rp = dp->dp_sccregs->scc_reg;
	register unsigned char rvl, tvl;


	/* if now enabling input, */
	if ( !(rp[SCCW_ENAB_R] & SCCW_ENAB_RxMask)) {
		scc_rclr(dpl);			/* gobble old stuff */
	}; 
	/* When doing a setline, the interrupts get disabled, so
	   reenable them here
	*/
	rvl = rp[SCCW_EXTINTR_R] |= (SCCW_EXTINTR_LabeledDCD |
		SCCW_EXTINTR_LabeledSYNC | SCCW_EXTINTR_LabeledCTS | SCCW_EXTINTR_Break);
	scc_put_reg(scp, SCCW_EXTINTR_R, rvl);

	
	rvl = rp[SCCW_ENAB_R] |= 
		SCCW_ENAB_RxIntrAllChars | SCCW_ENAB_ExtIntr;

	scc_put_reg(scp, SCCW_ENAB_R, rvl);

	/* Make sure to raise DTR and RTS.  */
	/*
	TODO: I dont like doing
	this blindly however.  We should really check to make
	sure that we are not using RTS or DTR handshaking 
	and that we need RTS or DTR to be down for flow control
	purposes.  Some other time.
	*/

	tvl = (rp[SCCW_TXMDM_R] |= (SCCW_TXMDM_LabeledDTR |
				   SCCW_TXMDM_LabeledRTS));
	scc_put_reg(scp, SCCW_TXMDM_R, tvl);


        dpl->ss_modem_state |= (BSD43_TIOCM_LE |
                                BSD43_TIOCM_DTR |
                                BSD43_TIOCM_DSR |
                                BSD43_TIOCM_RTS);

	rvl = scp->scc_cmd_b;

	/* note negative logic on Labeled lines */
	if ( rvl & SCCR_STATUS_LabeledCTS ) {
		dpl->ss_modem_state |= BSD43_TIOCM_CTS;
	} else {
		dpl->ss_modem_state &= ~BSD43_TIOCM_CTS;
	}

	return ( rvl & SCCR_STATUS_LabeledDCD ) ? 1 : 0;
}

/* shutdown a port
 *	this should be called only when safe from interrupts
 */
static int
scc_disable_line(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register struct scc_chan *scp = dp->dp_scp;
	register unsigned char *rp = dp->dp_sccregs->scc_reg;
	register unsigned char rvl;

	/* disable RX interrupts & external event intrs */
	rvl = rp[SCCW_ENAB_R] &= ~(SCCW_ENAB_RxMask | SCCW_ENAB_ExtIntr);
	scc_put_reg(scp, SCCW_ENAB_R, rvl);

	rvl = rp[SCCW_EXTINTR_R] &= ~(SCCW_EXTINTR_LabeledDCD |
		SCCW_EXTINTR_LabeledSYNC | SCCW_EXTINTR_LabeledCTS | SCCW_EXTINTR_Break);
	scc_put_reg(scp, SCCW_EXTINTR_R, rvl);

	/* scc_flushw(dpl); */			/* forget pending output */
	scc_stop(dpl);
	scc_rclr(dpl);

        /* Now drop rts, to let world know we are not listening... */
        dpl->ss_modem_state &= ~(BSD43_TIOCM_LE |
                                BSD43_TIOCM_RTS |
                                BSD43_TIOCM_CTS |
                                BSD43_TIOCM_ST |
                                BSD43_TIOCM_SR |
                                BSD43_TIOCM_RI);

	/*
	 * don't do the disable of the ports if there is the possibility
	 * of a mouse being connected.
	 */
	if (mono | color) {
		if (dp == &sccports[MOUSEPORT]) {
			rvl = rp[SCCW_TXMDM_R];
		} else {
			rvl = rp[SCCW_TXMDM_R] &= ~(SCCW_TXMDM_LabeledDTR |
					   SCCW_TXMDM_LabeledRTS);
		}
	} else {
		rvl = rp[SCCW_TXMDM_R] &= ~(SCCW_TXMDM_LabeledDTR |
				   SCCW_TXMDM_LabeledRTS);
	}

	scc_put_reg(scp, SCCW_TXMDM_R, rvl);

#ifdef MIPS_LOCAL
        if (!prom_mon)
#endif
                dpl->ss_modem_state &= ~(BSD43_TIOCM_CD |
                                        BSD43_TIOCM_DSR);
	/* reset the state information */
	dp->dp_state = 0;
}

scc_setline(dpl, cflag)
	register struct ss_line    *dpl;
	int cflag;
{
        register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register unsigned char *rp = dp->dp_sccregs->scc_reg;
	register unsigned char rvl, tvl;
	register unsigned int bvl, baud;

	rvl = rp[SCCW_AUX_R] = SCCW_AUX_BRfromPClock;
	scc_put_reg(scp, SCCW_AUX_R, rvl);

	rvl = SCCW_MISC_CLK_16;
	rvl |= ((cflag & CSTOPB) ? SCCW_MISC_SB_2 : SCCW_MISC_SB_1);
	if (!(cflag & PARENB)) {
		rvl |= SCCW_MISC_PAR_ParityOff;
	} else {
		rvl |= ((cflag & PARODD) ? SCCW_MISC_PAR_ParityOdd :
				SCCW_MISC_PAR_ParityEven);
	}
	rp[SCCW_MISC_R] = rvl;
	scc_put_reg(scp, SCCW_MISC_R, rvl);

	baud = (cflag & CBAUD);
	bvl = dp->dp_speed[baud];

	rp[SCCW_BRLO_R] = rvl = bvl & 0xff;
	scc_put_reg(scp, SCCW_BRLO_R, rvl);
	rp[SCCW_BRHI_R] = rvl = (bvl >> 8) & 0xff;
	scc_put_reg(scp, SCCW_BRHI_R, rvl);

	rvl = rp[SCCW_CLK_R] =
		(SCCW_CLK_TRxC_BRGen | SCCW_CLK_TxC_BRGen |
		 SCCW_CLK_RxC_BRGen | SCCW_CLK_DriveTRxC);
	scc_put_reg(scp, SCCW_CLK_R, rvl);

	rvl = rp[SCCW_AUX_R] |= SCCW_AUX_BRGenEnable;
	scc_put_reg(scp, SCCW_AUX_R, rvl);

	switch (cflag & CSIZE) {
	    case CS5:
		rvl = SCCW_RCV_5Bits;
		tvl = SCCW_TXMDM_5Bits;
		break;
	    case CS6:
		rvl = SCCW_RCV_6Bits;
		tvl = SCCW_TXMDM_6Bits;
		break;
	    case CS7:
		rvl = SCCW_RCV_7Bits;
		tvl = SCCW_TXMDM_7Bits;
		break;
	    case CS8:
		rvl = SCCW_RCV_8Bits;
		tvl = SCCW_TXMDM_8Bits;
		break;
	}
	rp[SCCW_RCV_R] = rvl |= SCCW_RCV_RxEn;
	scc_put_reg(scp, SCCW_RCV_R, rvl);

	rp[SCCW_TXMDM_R] = tvl |= (SCCW_TXMDM_TxEnable);
	scc_put_reg(scp, SCCW_TXMDM_R, tvl);

	rp[SCCW_EXTINTR_R] = tvl = 0;
	scc_put_reg(scp, SCCW_EXTINTR_R, tvl);

	if (scc_act_flags(dpl,cflag)) {  /* turn line (back) on */
		dpl->ss_modem_state |= (BSD43_TIOCM_CD |
					BSD43_TIOCM_DSR);
	} else {
		dpl->ss_modem_state &= ~(BSD43_TIOCM_CD |
					BSD43_TIOCM_DSR);
	};
	return 0;
}

scc_modem_control(dpl,op,value)
        struct  ss_line *dpl;
        int     op;
        int     value;
{
	register struct dport *dp = (struct dport *)dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register unsigned char *rp = dp->dp_sccregs->scc_reg;
	register unsigned char rvl;

	switch (op) {
	case SS_MC_STARTBREAK:
		rvl = (rp[SCCW_TXMDM_R] |= SCCW_TXMDM_SendBreak);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	case SS_MC_STOPBREAK:
		rvl = (rp[SCCW_TXMDM_R] &= ~SCCW_TXMDM_SendBreak);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	case SS_MC_ENABLEFLOW:
		rvl = (rp[SCCW_TXMDM_R] |= SCCW_TXMDM_LabeledRTS);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	case SS_MC_DISABLEFLOW:
		rvl = (rp[SCCW_TXMDM_R] &= ~SCCW_TXMDM_LabeledRTS);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	case SS_MC_ENABLEDTR:
		rvl = (rp[SCCW_TXMDM_R] |= SCCW_TXMDM_LabeledDTR);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	case SS_MC_DISABLEDTR:
		rvl = (rp[SCCW_TXMDM_R] &= ~SCCW_TXMDM_LabeledDTR);
		scc_put_reg(scp, SCCW_TXMDM_R, rvl);
		break;

	default:
		break;
	}
	return 0;
}

scc_driver_control(dpl,op,value)
        struct  ss_line *dpl;
        int     op;
        int     value;
{
	struct dport	*dp = (struct dport *)dpl->ss_llocal;
	register struct iocblk *ioss;

	switch (op) {
        case SS_DC_TIOCOUTQ:
                return((dpl->ss_state & SS_BUSY) ? 1 : 0);
                break;

        case SS_DC_DISABLELINE:
                return(scc_disable_line(dpl));
                break;

        case SS_DC_ENABLELINE:
                return(scc_enable_line(dpl));
                break;

	case SS_DC_STOP:
		return(scc_stop(dpl));

	case SS_DC_M_IOCTL:
		ioss = (struct iocblk*)((mblk_t *)value)->b_rptr;
		switch ( ioss->ioc_cmd ) {
		case UTCSETTIMESTAMP:
			dp->dp_state |= SCC_TIMESTAMP;
			ioss->ioc_error = 0;
			ioss->ioc_count = 0;
			((mblk_t *)value)->b_datap->db_type = M_IOCACK;
			return(1);

		case UTCCLRTIMESTAMP:
			dp->dp_state &= ~SCC_TIMESTAMP;
			ioss->ioc_error = 0;
			ioss->ioc_count = 0;
			((mblk_t *)value)->b_datap->db_type = M_IOCACK;
			return(1);

		default:
			break;
		}

	default:
		break;
	}
	return 0;
}

/* initialize the console ports
 *	This is done to allow debugging soon after booting.
 */
#define SCC_PATTERN_0	0xc6
#define SCC_PATTERN_1	0x39

scc_init()
{
	register struct dport *dp;
	register volatile struct scc_chan *sp;
	unsigned int holding;
        extern int cpu_config;

	sccports[0].dp_scp = (struct scc_chan *)(MACHDEP(SCC_REG_BASE) + 
						sizeof(struct scc_chan));
	sccports[1].dp_scp = (struct scc_chan *)(MACHDEP(SCC_REG_BASE));

	for (dp = sccports; dp < &sccports[SCC_BMAX]; dp++) {
		dp->dp_dpl->ss_line = 0;
		dp->dp_dpl->pss = sccboard + (dp - sccports);
		dp->dp_dpl->ss_llocal = (char *) dp;
		dp->dp_dpl->pss->ss_lines = dp->dp_dpl;
		dp->dp_dpl->pss->ss_nlines = SCC_LMAX;
		dp->dp_dpl->pss->ss_devdep = &sccdd;
		dp->dp_dpl->pss->ss_addr = (caddr_t) dp->dp_scp;
		dp->dp_dpl->ss_lenable = 1;

		dp->li_rptr = dp->li_wptr = dp->li_silo_start =
			  &dp->li_silo[0];
		dp->li_silo_limit = &dp->li_silo[SILOMAX];


		/*
		 * the scc comes up with the port set to register 2
		 * so this is used to clear the pointer.
		 */
		sp = dp->dp_scp;
		holding = sp->scc_cmd_w;

		scc_put_reg(sp,15,0x01);
		scc_put_reg(sp,7,0x40);
		scc_put_reg(sp,4,SCC_PATTERN_0);
		scc_fifo_flag_0 = scc_get_reg(sp,4);
		scc_put_reg(sp,4,SCC_PATTERN_1);
		scc_fifo_flag_1 = scc_get_reg(sp,4);
		if ((scc_fifo_flag_0 == SCC_PATTERN_0) && 
		    (scc_fifo_flag_1 == SCC_PATTERN_1)) {
			dp->dp_dpl->pss->ss_devdep = &sccdd_f;
     			cpu_config |= P_NEWSCC;
		}

		/* clean out the state info */
		dp->dp_state = 0;
	}


#ifdef SCNDCPORT
        /* initialize the secondary console port */
        dp = sccports + SCNDCPORT;
	scc_put_reg(dp->dp_scp, SCCW_MIC_R, SCCW_MIC_MIE);
        ss_cont_i(dp->dp_dpl, IFLAGS,
                &def_stty_ld.st_termio);
#endif

#ifdef PDBXPORT
	/* initialize kernel debuggin port */
        dp = sccports + PDBXPORT;
	scc_put_reg(dp->dp_scp, SCCW_MIC_R, SCCW_MIC_MIE);
        ss_cont_i(dp->dp_dpl, IFLAGS,
		 &def_stty_ld.st_termio);
#endif
}

/*
 * open a stream SCC port
 */
static int
scc_open(rq, dev, flag, sflag)
queue_t *rq;				/* our new read queue */
dev_t dev;
int flag;
int sflag;
{
	register struct dport *dp;
	register struct ss_line *dpl;
	register ushort port;
	register ushort cflag;

	if (sflag)			/* only a simple stream driver */
		return(OPENFAIL);

	port = SCC_UNIT(minor(dev));
	if (port >= SCC_BMAX) {		/* fail if bad device # */
		return(OPENFAIL);
	}

	dp = &sccports[port];
	dpl = dp->dp_dpl;


#ifdef MIPS_LOCAL
	if (prom_mon) {
		if (port == PDBXPORT) {
			return(OPENFAIL);
		}
	}
#endif

    	return(ss_open (dpl, rq, dev, flag, sflag, IFLAGS));
}

/* process interrupt for a single CONSOLE
 * (Sable's console device has only receive interrupts0
 */
sccintr()
{
	register struct dport *dp, *dpb;
	register volatile struct scc_chan *rp, *rpb;
	register unsigned char int_pend;

	dp = &sccports[PORT_A];
	rp = dp->dp_scp;

	dpb = &sccports[PORT_B];
	rpb = dpb->dp_scp;
	while (int_pend = scc_get_reg(rp, SCCRA_IP_R)) {

		*((unsigned char *)0x80000500) = int_pend;

		if (int_pend & SCCRA_IP_RxA) {
			scc_rx(dp->dp_dpl);
		}
	
		if (int_pend & SCCRA_IP_RxB) {
			scc_rx(dpb->dp_dpl);
		}

		if (int_pend & SCCRA_IP_TxA) {
			rp->scc_cmd_w = SCCW_CMD_CMD_ResetTxIntr;
			scc_tint(dp->dp_dpl);
		}

		if (int_pend & SCCRA_IP_TxB) {
			rpb->scc_cmd_w = SCCW_CMD_CMD_ResetTxIntr;
			scc_tint(dpb->dp_dpl);
		}

		if (int_pend & SCCRA_IP_ExtA) {
			rp->scc_cmd_w = SCCW_CMD_CMD_ResetExtIntr;
			scc_mdm(dp->dp_dpl);
		}

		if (int_pend & SCCRA_IP_ExtB) {
			rpb->scc_cmd_w = SCCW_CMD_CMD_ResetExtIntr;
			scc_mdm(dpb->dp_dpl);
		}
	}
}

/*
 *      Transmit Interrupt
 */

scc_tint(dpl)
        struct  ss_line *dpl;
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

/* Called from the callout table.  read chars and associated status
   from the silo and feed them to the recieve interupt routine
*/
static void
scc_dispatch()
{
	uchar c;
	uchar status;
	register struct dport	*dp;
	register struct ss_line *dpl;

	scc_timeid = 0;

	for (dp = &sccports[0]; dp < &sccports[MAXPORT]; dp++) { 
		for (; dp->li_rptr != dp->li_wptr;){
			scc_getsilo(dp->dp_dpl,&status, &c);
			scc_rx1(dp->dp_dpl,status,c);
		}
	}
}

scc_getsilo(dpl,sr, c)
register struct	ss_line *dpl;
register uchar *sr;
register uchar *c;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register struct scc_silo *siloptr;

	siloptr = dp->li_rptr;

	*sr = siloptr->si_status;
	*c  = siloptr->si_c;

	if (++dp->li_rptr == dp->li_silo_limit)
		dp->li_rptr = dp->li_silo_start;
}

scc_putsilo(dpl, sr, c)
register struct	ss_line *dpl;
register uchar c;
register uchar sr;
{
    register struct dport *dp = (struct dport *) dpl->ss_llocal;
    register struct scc_silo *siloptr;

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

static int
scc_rx(dpl)
	struct ss_line *dpl;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register u_char c, ce;
	register volatile struct scc_chan *scp = dp->dp_scp;
#ifdef DEBUGGING
	int count;
#endif
	int spltty();

	sysinfo.rcvint++;
	/* must be open to input
	 */
	if (!((SS_ISOPEN|SS_WOPEN) & dpl->ss_state)) {
		scc_rclr(dpl);
		return;
	}
	/* must be reading, to read */
	if (!(dpl->ss_cflag & CREAD)) {
		scc_rclr(dpl);
		return;
	}
	/* process all available characters
	 */
	while (scp->scc_cmd_b & SCCR_STATUS_RxDataAvailable) {
		if ((ce = scc_get_reg(scp, SCCR_SPECIAL_R)) & 
		    (SCCR_SPECIAL_FramingError | SCCR_SPECIAL_ParityError |
		     SCCR_SPECIAL_OverrunError)) {
			DELAY(1);
			scp->scc_cmd_w = SCCW_CMD_CMD_ResetError;
			wbflush();
		}
		DELAY(1);
		c = scp->scc_data_b;

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
			scc_putsilo(dpl, ce, c); 

	}
#ifdef DEBUGGING
	/* update interesting stats */
	if ((count = dp->li_wptr - dp->li_rptr) < 0)
		count += SILOMAX;
	if (count > dp->li_silomaxfull)
		dp->li_silomaxfull = count;
#endif

	if (!scc_timeid)
#ifdef SABLE
		scc_timeid = timeout_spl(scc_dispatch, 0, 0, spltty);
#else
		scc_timeid = timeout_spl(scc_dispatch, 0, HZ/20, spltty);
#endif SABLE

#ifdef SABLE
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
#endif
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

/* routine called from dispatch routine to handle chars
   that were stashed in the silo at interrupt time
 */
scc_rx1(dpl, sr, c)
register struct ss_line    *dpl;
register int sr;
register int c;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	static char panic_array[]= {0x14, 0x14};

	if(console_panic_enabled) 
	{
	 if ((dp - sccports) == SCNDCPORT) { /* really console port 0! */
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

#ifdef MIPS_LOCAL
#if 0
	if (prom_mon		/* if we get a control-A, debug */
	    && (dp - sccports) == PDBXPORT	/* on this port */
	    && '\001' == (c & 0x7f)) {
		debug("ring");
		return;	
	}
#endif 0
#endif MIPS_LOCAL 
		if (!(dpl->ss_state & SS_ISOPEN))
			return;

		if (sr & SCCR_SPECIAL_OverrunError) {
			dpl->ss_overflow++;
		}
		if (sr & (SCCR_SPECIAL_FramingError | SCCR_SPECIAL_ParityError )){
			if (IGNPAR & dpl->ss_iflag)  /* ignore chars with errors */
				 return;
			if (INPCK & dpl->ss_iflag){  /* check for parity errors */
				if (sr & SCCR_SPECIAL_FramingError){
					dpl->ss_framerr++;
				}
				if (dpl->ss_iflag & PARMRK) { /* Mark them bad */
					ss_slowr(dpl,0377);
					ss_slowr(dpl,0);
				} else {
					c = '\0';
				}
			}
		}

		if ((dpl->ss_iflag & ISTRIP) == 0 &&
			   (c & 0xFF) == 0377 &&
			   dpl->ss_iflag & PARMRK) {
			ss_slowr(dpl,0377);
		};			

 		scc_ss_inc(dpl,c); 
}

/*
 * scc_mdm is called from the scc interrupt routine in response to a
 * modem signal change.
 */
scc_mdm(dpl)
	struct ss_line *dpl;
{
	register volatile struct scc_chan *scp =
			((struct dport *)dpl->ss_llocal)->dp_scp;

	sysinfo.mdmint++;

	if (!(dpl->ss_state & (SS_ISOPEN | SS_WOPEN))) 
		return;

	if (scp->scc_cmd_b & SCCR_STATUS_LabeledDCD) {
		if (!(dpl->ss_modem_state & BSD43_TIOCM_CD))
			ss_con(dpl);
	} else {
		if (dpl->ss_modem_state & BSD43_TIOCM_CD)
			ss_coff(dpl);
	}
	if (scp->scc_cmd_b & SCCR_STATUS_LabeledCTS) {
		if (!(dpl->ss_modem_state & BSD43_TIOCM_CTS))
			ss_cts_on(dpl);
	} else {
		if (dpl->ss_modem_state & BSD43_TIOCM_CTS)
			ss_cts_off(dpl);
	}
	if (scp->scc_cmd_b & SCCR_STATUS_Break){
                if (dpl->ss_iflag & IGNBRK)
                        return;         /* ignore it if ok */
                if (dpl->ss_iflag & BRKINT) {
                        ss_mk_sig(dpl,SIGINT);
                        return;
                }
		if (dpl->ss_iflag & PARMRK) {
			ss_slowr(dpl,0377);
			ss_slowr(dpl,0);
		}

 		scc_ss_inc(dpl,'\0'); 
	}
}

/* 
 * This procedure outputs "len" chars pointed to by "cp" on line "cpl".
 * This procedure gets called from ss_tx.
 */
scc_outc(dpl, cs, len)
    register struct ss_line    *dpl;
    register char *cs;
    register int   len;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
	register volatile struct scc_chan *scp = dp->dp_scp;
	register struct scc_regs *rp = dp->dp_sccregs;

    	if (len) {
		if ( !(rp->scc_reg[SCCW_ENAB_R] & SCCW_ENAB_TxIntr) ) {
			rp->scc_reg[SCCW_ENAB_R] |= SCCW_ENAB_TxIntr;
			scc_put_reg(scp, SCCW_ENAB_R,
				 rp->scc_reg[SCCW_ENAB_R]);
		}
		dpl->ss_state |= SS_BUSY;
		scp->scc_data_w = (unsigned int)*cs;
		wbflush();
		sysinfo.outch++;
	}
/*
	ss_start(dpl);
*/
}



/*
 * Print a character on console. (buffered)
 */
scc_xputc(c, scp)
register int c;
register volatile struct scc_chan *scp;
{
	register int s;

	s = spltty();
	while ( !(scp->scc_cmd_b & SCCR_STATUS_TxReady) ) {
		DELAY(1);
	}

	DELAY(1);
	scp->scc_data_w = (unsigned int)c;
	wbflush();
	splx(s);
}

sccputc(port, c)
int port;
register int c;
{
	register volatile struct scc_chan *scp;
	register int s;

	scp = sccports[port].dp_scp;
	scc_xputc(c, scp);
#ifdef mips
	if (c == '\n')
		scc_xputc('\r', scp);
#endif
        /* implement XON/XOFF */
        s = spltty();
	if (scp->scc_cmd_b & SCCR_STATUS_RxDataAvailable) {
		DELAY(1);
		if ((scp->scc_data_b & 0x7F) == CSTOP) {
                	(void)sccgetc( port );
		}
        }
        splx(s);
}

int
scc_xgetc(scp, timo)
register volatile struct scc_chan *scp;
register long timo;
{
	register u_char	c;
	register int	s;

	s = spltty();
#ifndef mips
	if (timo) {			/* old grot */
		while ( !(scp->scc_cmd_b & SCCR_STATUS_RxDataAvailable)) {
			DELAY(1);
			if ( !--timo) return -1;
		}
	} else
#endif
		while ( !(scp->scc_cmd_b & SCCR_STATUS_RxDataAvailable)) {
			DELAY(1);
		}
	DELAY(1);
	c = scp->scc_data_b;
	splx(s);

	return c;
}


/*
 * Read a character from the console.
 */
sccgetc( port )
int port;
{
	register volatile struct scc_chan *scp;

	scp = sccports[port].dp_scp;
	return scc_xgetc(scp, 0L);
}

scc_ss_inc(dpl,c)
register struct ss_line *dpl;
register char c;
{
	register struct dport *dp = (struct dport *) dpl->ss_llocal;
    	register time_t tstamp;

	if ( !(dp->dp_state & SCC_TIMESTAMP) ) {
		ss_inc(dpl, c);
	} else {
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, c);
		tstamp = lbolt;
		ss_inc(dpl, ((tstamp>>24)&0xff));
		ss_inc(dpl, ((tstamp>>16)&0xff));
		ss_inc(dpl, ((tstamp>>8)&0xff));
		ss_inc(dpl, (tstamp&0xff));
	}
}
