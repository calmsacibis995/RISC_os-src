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
#ident	"$Header: delay.c,v 1.2.1.2 90/05/10 02:09:09 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include	"curses_inc.h"

/*
 * The following array gives the number of tens of milliseconds per
 * character for each speed as returned by gtty.  Thus since 300
 * baud returns a 7, there are 33.3 milliseconds per char at 300 baud.
 */
static	short	tmspc10[] =
		{
		    /* 0   50    75   110 134.5 150  200  300   baud */
		       0, 2000, 1333, 909, 743, 666, 500, 333,
		    /* 600 1200 1800 2400 4800 9600 19200 38400 baud */
		       166, 83,  55,  41,  20,  10,   5,    2
		};

/*
 * Insert a delay into the output stream for "delay/10" milliseconds.
 * Round up by a half a character frame, and then do the delay.
 * Too bad there are no user program accessible programmed delays.
 * Transmitting pad characters slows many terminals down and also
 * loads the system.
 */

_delay(delay, outc)
register	int	delay;
int		(*outc)();
{
    register	int	mspc10;
    register	int	pc;
    register	int	outspeed;

    /* if there is no pad character, then just return */
    if (no_pad_char)
	goto good;

    outspeed = _BR(PROGTTY);
    if (outspeed <= 0 || outspeed >= (sizeof tmspc10 / sizeof tmspc10[0]))
	return (ERR);

    mspc10 = tmspc10[outspeed];
    delay += mspc10 / 2;
    if (pad_char)
	pc = *pad_char;
    else
	pc = 0;
    for (delay /= mspc10; delay-- > 0; )
	(*outc)(pc);
good:
    return (OK);
}
