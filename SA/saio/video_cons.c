#ident "$Header: video_cons.c,v 1.5.7.2 90/12/14 12:28:52 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
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
 * Video driver
 */

#include "sys/param.h"
#include "sys/errno.h"
#include "machine/scc_cons.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/teState.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

extern char *atob();

extern TeState teState;
extern caddr_t frm_addr;
extern int color_cons, mono_cons;

int nrows;
int ncols;

struct device_buf vid_device_buf;

#define WBFLUSH()	{ wbflush(); DELAY(32); }

/* Routine to determine if to set up graphic terminal as console */
graphic_cons() {
        char c;
        char *cp;
        extern char *getenv();
        int is_color,is_kbd;

        c = (cp = getenv("console")) ? *cp : 0;
	if (c == '0' || c == '1' || c == 't' || c == 'r') return;

        color_cons = 0;
        mono_cons = 0;
	is_kbd = pkbd_check();
	switch (c) {
 	    case 'm':
                if (is_kbd) mono_cons = 1;
		break;
	    case 'v':	
	    case 'a':	
                if (is_kbd) mono_cons = 1;
	    case 'c':
                is_color = color_check();
                if (is_color && is_kbd) color_cons = 1;
		break;
	    case 'l':
	    case 'g':
	    default:
                is_color = color_check();
                if (is_color && is_kbd) color_cons = 1;
		else if (is_kbd) mono_cons = 1;
	}
}


/* this is called from prom power-up, reset or init */
/* redraw the screen and start fresh */
_videoreset()
{
        clear_GDlist();
        graphic_cons();
        if (!(mono_cons || color_cons)) return;
        init_gsparam();
        save_gsparam();
        if (mono_cons) mono_init(1);
        if (color_cons) init_r3030_c8(1);
        teReset(1);
}


/* do a limited amount of resetting */
/* continue on with whatever is on the screen */
_videoinit()
{
	char c;
	char *cp;
	extern char *getenv();

	bzero(&vid_device_buf, sizeof(vid_device_buf));

	pkbd_init();

        graphic_cons();
        if (!(mono_cons || color_cons)) return;
	restore_gsparam();
	/* sanity check of screen parameters.  The frm_addr should be
	   between 0x780000 and 0x7e0000 and at a 64-byte boundary. 
	*/
	if (!(nrows > 0 && nrows < 37 && ncols > 0 && ncols < 81 &&
		teState.row > 0 && teState.row <= nrows &&
		teState.col > 0 && teState.col <= ncols &&
		(((unsigned int)frm_addr & 0x0ff8003f) == 0x780000))) {
		_videoreset();
	}
	else {
                if (mono_cons) {
		/* clear mono screen if bus_test = 0 */
        	    c = (cp = getenv("bus_test")) ? *cp : 0;  
        	    if (c == '0') { 
		        mono_init(1);
			teReset(1);
		 	return;
		    } 
		    else
		    	mono_init(0); 
		}
                if (color_cons) init_r3030_c8(0);
		teReset(0);
 	}
}

/*
 * _videoopen -- initialize scc
 */
_videoopen(io)
struct iob *io;
{
	unsigned unit = io->i_ctlr;
	extern char **environ;

	if (unit > 1) {
		io->i_errno = ENXIO;
		return(-1);
	}
	io->i_flgs |= F_SCAN;

	return(0);
}

/*
 * _sccstrategy -- perform io
 */
_videostrategy(io, func)
register struct iob *io;
int func;
{
	register struct device_buf *db;
	register c;
	int ocnt = io->i_cc;

	if (func == READ) {
		db = &vid_device_buf;
		while (io->i_cc > 0) {
			while (c = g_getchar(0))
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
			tePutChar(*io->i_ma++);
		return(ocnt);
	} else
		_io_abort("video_cons bad function");
}

_videoioctl(io, cmd, arg)
struct iob *io;
{
	register c;
	register struct device_buf *db;
	int retval = 0;

	db = &vid_device_buf;
	switch (cmd) {
	case FIOCSCAN:
		while(c = g_getchar(0))
			_ttyinput(db, c);
		break;
	case TIOCRAW:
	case TIOCRAWRAW:
		if (arg)
			db->db_flags |= DB_RAW;
		else
			db->db_flags &= ~DB_RAW;
		break;
	case TIOCFLUSH:
		CIRC_FLUSH(db);
		break;

	case TIOCREOPEN:
		retval = _videoopen(io);
		break;

	default:
		io->i_errno = EINVAL;
		return(-1);
	}
	return(retval);
}
