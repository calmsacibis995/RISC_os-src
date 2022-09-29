#ident "$Header: sablecons.c,v 1.3 90/02/28 22:09:08 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * Copyright 1985 by MIPS Computer Systems, Inc.
 */

/*
 * sablecons.c -- simulated uart device for sable
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "machine/cpu.h"
#include "machine/sablecons.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

static struct scons_device *scdev[] = { SCONS0_BASE, SCONS1_BASE };
static struct device_buf sconsbuf[1];

#define	NSCONS	(sizeof(scdev)/sizeof(scdev[0]))

_sconsinit()
{
	bzero(sconsbuf, sizeof(sconsbuf));
}

_sconsopen(io)
struct iob *io;
{
	if (io->i_ctlr >= NSCONS) {
		io->i_errno = ENXIO;
		return(-1);
	}
	io->i_flgs |= F_SCAN;
	return(0);
}

_sconsstrategy(io, func)
register struct iob *io;
int func;
{
	register struct device_buf *db;
	register c;
	int ocnt = io->i_cc;

	if (func == READ) {
		db = &sconsbuf[io->i_ctlr];
		while (io->i_cc > 0) {
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
		_io_abort("sablecons bad function");
}

_sconsioctl(io, cmd, arg)
register struct iob *io;
{
	struct device_buf *db = &sconsbuf[io->i_ctlr];
	int c;

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

	case TIOCFLUSH:
		CIRC_FLUSH(db);
		break;

	default:
		return(-1);
	}
	return(0);
}

/*
 * Print a character on console. (un-buffered)
 */
static
consputc(c, ctlr)
	register int c;
	int ctlr;
{
	register struct device_buf *db;
	register volatile struct scons_device *scdevp = scdev[ctlr];

	db = &sconsbuf[0];
	while (CIRC_STOPPED(db))
		_scandevs();

	scdevp->sc_tx = c;
	scdevp->sc_command |= SC_CMD_TXFLUSH;
}

/*
 * Read a character from the console.
 */
static
consgetc(ctlr)
int ctlr;
{
	register volatile struct scons_device *scdevp = scdev[ctlr];

	if (!(scdevp->sc_status & SC_STAT_RXRDY))
		return (0);
	return((int)scdevp->sc_rx | 0x100);
}

_sable_errputc(c)
char c;
{
	register volatile struct scons_device *scdevp = SCONS0_BASE;

	scdevp->sc_tx = c;
}
