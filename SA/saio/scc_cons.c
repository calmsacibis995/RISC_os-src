#ident "$Header: scc_cons.c,v 1.4 90/11/14 18:58:48 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Zilog scc 8530 driver
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "machine/scc_cons.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

union wd_by {
        struct  w_b {
                unsigned char   pad[3];
                unsigned char   by;
        } w_b;
        unsigned int    w_w;
};

struct scc_chan {
        union wd_by     scc_cmd;
        union wd_by     scc_data;
};
#define scc_cmd_b       scc_cmd.w_b.by
#define scc_cmd_w       scc_cmd.w_w
#define scc_data_b      scc_data.w_b.by
#define scc_data_w      scc_data.w_w

extern int SCC_REG_BASE[];
extern char *getenv();
extern char *atob();

#define SCC_REG_XXX	((struct scc_chan *)MACHDEP(SCC_REG_BASE))
#define SCC_REG_ADDR	(&SCC_REG_XXX)
#define SCC_CHAN_A	(&(SCC_REG_XXX[1]))
#define SCC_CHAN_B	(&(SCC_REG_XXX[0]))

#define SCC_REG		volatile struct scc_reg
#define SCC_CHAN	volatile struct scc_chan
/*
 * to determine the time constant:
 *      TC = ( (MHZ {the clock rate} / (16 * 2 * BR)) - 2 )
 *              for a 16x clock
 */
#define MHZ     10000000        /* base pclock for the scc part */

#define BCONST  16              /* use this if you wish to change the divisor*/

#define B_CONST(speed)  ( (MHZ/(2*BCONST*speed)) - 2 )

static struct device_buf sccbuf[2];
static int dbaud[2];	/* current baud rate */
static void sccparam();
static int scc_baud();

#define WBFLUSH()	{ wbflush(); DELAY(32); }

unsigned int
scc_get_reg(scp, reg)
volatile struct scc_chan        *scp;
unsigned int    reg;
{
        scp->scc_cmd_w = reg;
	wbflush();
	DELAY(1);
        return (unsigned int)scp->scc_cmd_b;
}

void
scc_put_reg(scp, reg, val)
volatile struct scc_chan        *scp;
unsigned int    reg;
unsigned int    val;
{
        scp->scc_cmd_w = reg;
	wbflush();
	DELAY(1);
        scp->scc_cmd_w = val;
	wbflush();
}


_sccinit()
{
	register SCC_CHAN *scc_c = SCC_CHAN_A;
	char *abaud;

	bzero(sccbuf, sizeof(sccbuf));

	abaud = getenv("lbaud");
	if (!abaud || *atob(abaud, &dbaud[0]) || scc_baud(dbaud[0]) < 0) 
		dbaud[0] = 9600;
	sccparam(0);

	scc_put_reg(scc_c, SCCW_TXMDM_R, (SCCW_TXMDM_8Bits |
			SCCW_TXMDM_TxEnable | SCCW_CLK_DriveTRxC));
}

/*
 * _sccopen -- initialize scc
 */
_sccopen(io)
struct iob *io;
{
	register SCC_CHAN *scc_c;
	unsigned unit = io->i_ctlr;
	char *abaud;
	extern char **environ;

	if (unit > 1) {
		io->i_errno = ENXIO;
		return(-1);
	}
	io->i_flgs |= F_SCAN;

	scc_c = unit ? SCC_CHAN_B : SCC_CHAN_A;

	abaud = getenv(unit ? "rbaud" : "lbaud");
	if (!abaud || *atob(abaud, &dbaud[unit]) || scc_baud(dbaud[unit]) < 0) {
		dbaud[unit] = unit ? 1200 : 9600;
		if (environ)
			printf("\nIllegal %s value: %s\n",
			    unit ? "rbaud" : "lbaud", abaud);
	}
	/* this is the early_init case, since Rx3230 uses tty1 as error
	   port, set it to 9600 instead of 1200 */
	if (!abaud && IS_R3030) dbaud[unit] = 9600;

	sccparam (unit);
	scc_put_reg(scc_c, SCCW_MIC_R, 0);
	return(0);
}

/*
 * _sccstrategy -- perform io
 */
_sccstrategy(io, func)
register struct iob *io;
int func;
{
	register struct device_buf *db;
	register c;
	int ocnt = io->i_cc;

	if (func == READ) {
		db = &sccbuf[io->i_ctlr];
		while (io->i_cc > 0) {
			/*
			 * we don't just call scandevs() here
			 * because the uarts are extremely prone
			 * to overruns
			 */
			while (c = consgetc(io->i_ctlr))
				_ttyinput(db, c);
			if ((io->i_flgs & F_NBLOCK) == 0)
				while (CIRC_EMPTY(db))
					_scandevs();
			if (CIRC_EMPTY(db))
				return(ocnt - io->i_cc);
			*io->i_ma++ = _circ_getc(db);
			io->i_cc--;
		}
		return(ocnt);
	} else if (func == WRITE) {
		while (io->i_cc-- > 0)
			consputc(*io->i_ma++, io->i_ctlr);
		return(ocnt);
	} else
		_io_abort("scc_cons bad function");
}

_sccioctl(io, cmd, arg)
struct iob *io;
{
	register struct device_buf *db = &sccbuf[io->i_ctlr];
	register c;
	int retval = 0;

	switch (cmd) {
	case FIOCSCAN:
		while (c = consgetc(io->i_ctlr))
			_ttyinput(db, c);
		break;

	case TIOCRAW:
		if (arg)
			db->db_flags |= DB_RAW;
		else
			db->db_flags &= ~DB_RAW;
		break;

	case TIOCRAWRAW:
		if (arg)
			db->db_flags |= DB_RAWRAW;
		else
			db->db_flags &= ~DB_RAWRAW;
		break;

	case TIOCFLUSH:
		CIRC_FLUSH(db);
		break;

	case TIOCREOPEN:
		retval = _sccopen(io);
		break;

	default:
		io->i_errno = EINVAL;
		return(-1);
	}
	return(retval);
}

/*
 *	sccparam - set parameter from tty struct into scc registers
 */
static void
sccparam(unit)
int unit;
{
	register SCC_CHAN *scc_c;
	int	junk;
        int baud, rvl;
	register status;

	scc_c = unit ? SCC_CHAN_B : SCC_CHAN_A;

	baud = scc_baud(dbaud[unit]);
	if (baud < 0)
		return;

#if 0
        if (environ) {  /* don't delay if called from early_init */
                register i = 0;

		while (i++ < 0x10000 && 
				(scc_c->scc_cmd_b & SCCR_STATUS_TxReady) == 0)
                        continue;
        }
#endif
	rvl = scc_c->scc_cmd_b;			/* read to clear cmd ptr */

	scc_put_reg(scc_c, SCCW_MIC_R, 0);

	scc_put_reg(scc_c, SCCW_AUX_R, SCCW_AUX_BRfromPClock);

	scc_put_reg(scc_c, SCCW_MISC_R, (SCCW_MISC_PAR_ParityOff |
					SCCW_MISC_SB_1 | SCCW_MISC_CLK_16));

	scc_put_reg(scc_c, SCCW_RCV_R, SCCW_RCV_8Bits );

	scc_put_reg(scc_c, SCCW_TXMDM_R, SCCW_TXMDM_8Bits );

        scc_put_reg(scc_c, SCCW_BRLO_R, (baud & 0xff));

        scc_put_reg(scc_c, SCCW_BRHI_R, ((baud>>8) & 0xff));

	scc_put_reg(scc_c, SCCW_CLK_R, (SCCW_CLK_TRxC_BRGen |
				SCCW_CLK_TxC_BRGen | SCCW_CLK_RxC_BRGen |
				SCCW_CLK_DriveTRxC));

	scc_put_reg(scc_c, SCCW_AUX_R, SCCW_AUX_BRfromPClock |
					 SCCW_AUX_BRGenEnable);

	scc_put_reg(scc_c, SCCW_RCV_R, SCCW_RCV_8Bits | SCCW_RCV_RxEn);

	scc_put_reg(scc_c, SCCW_TXMDM_R, (SCCW_TXMDM_8Bits |
					SCCW_CLK_DriveTRxC |
					SCCW_TXMDM_TxEnable |
					SCCW_TXMDM_LabeledDTR |
					SCCW_TXMDM_LabeledRTS));

	/*
	 * need a delay here to let clocking for new baud rate settle
	 * (No matter what, it seems that the first char after a BREAK
	 * is garbaged!  And there doesn't seem to be any easy way to
	 * avoid reading it, this doesn't work!!)
	 */
#ifndef SABLE
	DELAY(20000);
#endif
	while (scc_c->scc_cmd_b & SCCR_STATUS_RxDataAvailable) {
		DELAY(1);			/* mdove */
		junk = scc_c->scc_data_b;
	}
	CIRC_FLUSH(&sccbuf[unit]);	/* flush all pending input */
}

static
scc_baud(brate)
int brate;
{
	int     speed;

	switch (brate) {
		case 75:
		case 110:
		case 134:
		case 150:
		case 300:
		case 600:
		case 1200:
		case 2400:
		case 4800:
		case 1800:
		case 9600:
		case 19200:
			speed = B_CONST(brate);
			break;

		default: speed = -1; break;
	}
	return (speed);
}

static cur_baud;
static int baud_table[] = {
	110,
	300,
	1200,
	2400,
	4800,
	9600,
	19200
};

#define	NBAUD	(sizeof(baud_table)/sizeof(baud_table[0]))

static
nextbaud(baud)
int baud;
{
	register int i;

	for (i = 1; i < NBAUD; i++)
		if (baud_table[i] > baud)
			return(baud_table[i]);
	return(baud_table[0]);
}

static
consgetc(unit)
{
	register SCC_CHAN *scc_c;
	register status;
	register i;
#ifdef COMPILER_FIXED
	register int junk;
#else
	static int junk;
#endif
	extern Verbose;
	int rcvdata;

	scc_c = unit ? SCC_CHAN_B : SCC_CHAN_A;
	if (!(scc_c->scc_cmd_b & SCCR_STATUS_RxDataAvailable))
		return(0);
	status = scc_get_reg(scc_c, SCCR_SPECIAL_R);
	if (status & SCCR_SPECIAL_OverrunError) {
		if (Verbose)
			_errputs("\nscc overrun\n");
		while (scc_c->scc_cmd_b & SCCR_STATUS_RxDataAvailable) {
			DELAY(1);
			junk = scc_c->scc_data_b;
		}
		scc_c->scc_cmd_b = SCCW_CMD_CMD_ResetError; WBFLUSH();
		return(0);
	}
	if (status & SCCR_SPECIAL_FramingError) {
		/*
		 * switch baud rate
		 */
		scc_c->scc_cmd_b = SCCW_CMD_CMD_ResetError; WBFLUSH();
		dbaud[unit] = nextbaud(dbaud[unit]);
		sccparam(unit);
		printf(" \n\n%s Framing error: baud rate set to %d\n\n",
		    unit ? "remote" : "local", dbaud[unit]);
		return(0);
	}
	rcvdata = scc_c->scc_data_b;
   	if (!rcvdata) {
		/*
		 * switch baud rate
		 */
		scc_c->scc_cmd_b = SCCW_CMD_CMD_ResetError; WBFLUSH();
		dbaud[unit] = nextbaud(dbaud[unit]);
		sccparam(unit);
		printf(" \n\n%s Break: baud rate set to %d\n\n",
		    unit ? "remote" : "local", dbaud[unit]);
		return(0);
	}
	return(rcvdata | 0x100);
}

static
consputc(c, unit)
char c;
{
	register SCC_CHAN *scc_c;
	register struct device_buf *db;

	db = &sccbuf[unit];
	while (CIRC_STOPPED(db))
		_scandevs();

	scc_c = unit ? SCC_CHAN_B : SCC_CHAN_A;
	while (!(scc_c->scc_cmd_b & SCCR_STATUS_TxReady)) {
		_scandevs();
	}
	DELAY(10);				/* mdove */
	scc_c->scc_data_b = c; WBFLUSH();
}

/*
 * Primitive output routines in case all else fails
 */

_scc_errputc(c)
char c;
{
	register SCC_CHAN *scc_c = SCC_CHAN_A;

	if (IS_R3030) scc_c = SCC_CHAN_B;
	while (!(scc_c->scc_cmd_b & SCCR_STATUS_TxReady)) {
		DELAY(1);
	}
	scc_c->scc_data_b = c; WBFLUSH();
	DELAY(1);
}
