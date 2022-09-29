#ident "$Header: s2681cons.c,v 1.9 90/02/28 22:08:57 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * Signetics 2681 duart driver
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "machine/s2681cons.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

/*
 * TODO: modify so this will work with more than 1 2681 uart
 */

extern int CN_REG_BASE[];

#define CN_REG_XXX	(*(struct cn_reg *)MACHDEP(CN_REG_BASE))
#define CN_REG_ADDR	(&CN_REG_XXX)
#define CN_CHAN_A	(&(CN_REG_XXX.cn_chan_a))
#define CN_CHAN_B	(&(CN_REG_XXX.cn_chan_b))

#ifdef XXX
#define	CN_REG_BASE	(*(struct cn_reg *)PHYS_TO_K1(0x1e007fff))
#define	CN_REG_ADDR	(&CN_REG_BASE)
#define CN_CHAN_A	(&(CN_REG_BASE.cn_chan_a))
#define CN_CHAN_B	(&(CN_REG_BASE.cn_chan_b))
#endif
#define CN_REG		volatile struct cn_reg
#define CN_CHAN		volatile struct cn_chan

static struct device_buf s2681buf[2];
static int dbaud[2];	/* current baud rate */
static void cnparam();
static int map_baud();

#define WBFLUSH()	{ wbflush(); DELAY(32); }

_s2681init()
{
	bzero(s2681buf, sizeof(s2681buf));
}

/*
 * _s2681open -- initialize duart
 */
_s2681open(io)
struct iob *io;
{
	register CN_REG *cnr = CN_REG_ADDR;
	register CN_CHAN *cnc;
	unsigned unit = io->i_ctlr;
	char *abaud;
	extern char *getenv();
	extern char *atob();
	extern char **environ;

	if (unit > 1) {
		io->i_errno = ENXIO;
		return(-1);
	}
	io->i_flgs |= F_SCAN;

	cnc = unit ? CN_CHAN_B : CN_CHAN_A;
	cnc->cnc_cmd = CNCMD_RXRESET; WBFLUSH();
	cnc->cnc_cmd = CNCMD_TXRESET; WBFLUSH();
	cnc->cnc_cmd = CNCMD_ERRESET; WBFLUSH();
	cnc->cnc_cmd = CNCMD_BREAKRESET; WBFLUSH();
	cnc->cnc_cmd = (CNCMD_RXENABLE|CNCMD_TXENABLE); WBFLUSH();
	cnr->cn_outport_conf = 0x0; WBFLUSH();

	abaud = getenv(unit ? "rbaud" : "lbaud");
	if (!abaud || *atob(abaud, &dbaud[unit]) || map_baud(dbaud[unit]) < 0) {
		dbaud[unit] = unit ? 1200 : 9600;
		if (environ)
			printf("\nIllegal %s value: %s\n",
			    unit ? "rbaud" : "lbaud", abaud);
	}

	cnparam (unit);
	/*
	 * Assert DTR
	 */
	if (unit) {
		cnr->cn_outport_set = CNOP_DTR_B|CNOP_RTS_B; WBFLUSH();
	}
	cnr->cn_imr = 0; WBFLUSH();
	return(0);
}

/*
 * _s2681strategy -- perform io
 */
_s2681strategy(io, func)
register struct iob *io;
int func;
{
	register struct device_buf *db;
	register c;
	int ocnt = io->i_cc;

	if (func == READ) {
		db = &s2681buf[io->i_ctlr];
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
		_io_abort("s2681cons bad function");
}

_s2681ioctl(io, cmd, arg)
struct iob *io;
{
	register struct device_buf *db = &s2681buf[io->i_ctlr];
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
		retval = _s2681open(io);
		break;

	default:
		io->i_errno = EINVAL;
		return(-1);
	}
	return(retval);
}

/*
 *	cnparam - set parameter from tty struct into 2681 registers
 */
static void
cnparam(unit)
int unit;
{
	register CN_CHAN *cnc;
#ifdef COMPILER_FIXED
	register int junk;
#else
	static int junk;
#endif
	int baud;
	u_char mr1, mr2;
	extern char **environ;

	baud = map_baud(dbaud[unit]);
	if (baud < 0)
		return;
	baud |= baud << 4;

	cnc = unit ? CN_CHAN_B : CN_CHAN_A;

	if (environ) {	/* don't delay if called from early_init */
		register i = 0;

		while (i++ < 0x10000 && (cnc->cnc_stat & CNSTAT_TXEMPTY) == 0)
			continue;
	}

	/*
	 * Set baud rate. For now will only support ACR[7]=1 range of rates.
	 * See the 2681 manual for details.
	 */
	/* build new mode word */

	mr1 = CNMR1_BITS8 | CNMR1_NOPARITY;
	mr2 = (dbaud[unit] == 110) ? CNMR2_STOP2 : CNMR2_STOP1;

	cnc->cnc_stat = baud; WBFLUSH();
	cnc->cnc_cmd = CNCMD_MRRESET; WBFLUSH(); /* point to mr1 */
	cnc->cnc_mode = mr1; WBFLUSH();
	cnc->cnc_mode = mr2; WBFLUSH();
	/*
	 * need a delay here to let clocking for new baud rate settle
	 * (No matter what, it seems that the first char after a BREAK
	 * is garbaged!  And there doesn't seem to be any easy way to
	 * avoid reading it, this doesn't work!!)
	 */
	DELAY(20000);
	while (cnc->cnc_stat & CNSTAT_RXRDY) {
		DELAY(1);			/* mdove */
		junk = cnc->cnc_data;
	}
	cnc->cnc_cmd = CNCMD_BREAKRESET; WBFLUSH();
	cnc->cnc_cmd = CNCMD_ERRESET; WBFLUSH();
	CIRC_FLUSH(&s2681buf[unit]);	/* flush all pending input */
}

static
map_baud(brate)
int brate;
{
	int     speed;

	switch (brate) {
		case 75: speed = 0; break;
		case 110: speed = 1; break;
		case 134: speed = 2; break;
		case 150: speed = 3; break;
		case 300: speed = 4; break;
		case 600: speed = 5; break;
		case 1200: speed = 6; break;
		case 2400: speed = 8; break;
		case 4800: speed = 9; break;
		case 1800: speed = 10; break;
		case 9600: speed = 11; break;
		case 19200: speed = 12; break;
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
	register CN_CHAN *cnc;
	register status;
	register i;
#ifdef COMPILER_FIXED
	register int junk;
#else
	static int junk;
#endif
	extern Verbose;

	cnc = unit ? CN_CHAN_B : CN_CHAN_A;
	status = cnc->cnc_stat;
	if (!(status & CNSTAT_RXRDY))
		return(0);
	if (status & CNSTAT_OVERRUN) {
		if (Verbose)
			_errputs("\n2681 overrun\n");
		while (cnc->cnc_stat & CNSTAT_RXRDY) {
			DELAY(1);
			junk = cnc->cnc_data;
		}
		cnc->cnc_cmd = CNCMD_ERRESET; WBFLUSH();
		return(0);
	}
	if (status & (CNSTAT_RXBREAK|CNSTAT_FRAME)) {
		/*
		 * switch baud rate
		 */
		cnc->cnc_cmd = CNCMD_BREAKRESET; WBFLUSH();
		cnc->cnc_cmd = CNCMD_ERRESET; WBFLUSH();
		dbaud[unit] = nextbaud(dbaud[unit]);
		cnparam(unit);
		printf(" \n\n%s baud rate set to %d\n\n",
		    unit ? "remote" : "local", dbaud[unit]);
		return(0);
	}
	return(cnc->cnc_data | 0x100);
}

static
consputc(c, unit)
char c;
{
	register CN_CHAN *cnc;
	register struct device_buf *db;

	db = &s2681buf[unit];
	while (CIRC_STOPPED(db))
		_scandevs();

	cnc = unit ? CN_CHAN_B : CN_CHAN_A;
	while (!(cnc->cnc_stat & CNSTAT_TXRDY)) {
		_scandevs();
	}
	DELAY(10);				/* mdove */
	cnc->cnc_data = c; WBFLUSH();
}

/*
 * Primitive output routines in case all else fails
 */

_s2681_errputc(c)
char c;
{
	register CN_CHAN *cnc = CN_CHAN_A;

	while (!(cnc->cnc_stat & CNSTAT_TXRDY)) {
		DELAY(1);
	}
	cnc->cnc_data = c; WBFLUSH();
	DELAY(1);
}
