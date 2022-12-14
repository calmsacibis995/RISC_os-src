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
#ident	"$Header: ttimeout.c,v 1.2.1.3 90/05/10 02:25:25 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"
#include	<fcntl.h>

/*
 * Set the current time-out period for getting input.
 *
 * delay:	< 0 for infinite delay (blocking read)
 * 		= 0 for no delay read
 * 		> 0 to specify a period in millisecs to wait
 * 		    for input, then return '\0' if none comes
 */

void	_setblock(), _settimeout();

ttimeout(delay)
int	delay;
{
    if (cur_term->_inputfd < 0)
	return (ERR);

    if (delay < 0)
	delay = -1;

    if (cur_term->_delay == delay)
	goto good;

#ifdef	SYSV
    if (delay > 0)
    {
	if (cur_term->_delay < 0)
	    _setblock(delay);
	_settimeout(delay);
    }
    else
	if ((delay + cur_term->_delay) == -1)
	    _setblock(delay);
	else
	{
	    if (delay < 0)
		_setblock(delay);
	    _settimeout(delay);
	}
#else	/* SYSV */
    cbreak();
#endif	/* SYSV */

    cur_term->_delay = delay;
good:
    return (OK);
}

#ifdef	SYSV
/* set the terminal to nodelay (==0) or block(<0) */
static	void
_setblock(block)
int	block;
{
    int	flags = fcntl(cur_term->_inputfd, F_GETFL, 0);

    if (block < 0)
	flags &= ~O_NDELAY;
    else	
	flags |= O_NDELAY;

    (void) fcntl(cur_term->_inputfd, F_SETFL, flags);
}

/*
 * set the terminal to timeout in t milliseconds,
 * rounded up to the nearest 10th of a second.
 */
static	void
_settimeout(num)
int	num;
{
#ifdef RISCOS
    change_bsd43_bits(CBREAK,RAW);
#else RISCOS
    PROGTTY.c_lflag &= ~ICANON;
#endif RISCOS
    if (num > 0)
    {
	PROGTTY.c_cc[VMIN] = 0;
	PROGTTY.c_cc[VTIME] = (num > 25500) ? 255 : (num + 99) / 100;
	cur_term->_fl_rawmode = 3;
    }
    else
    {
	PROGTTY.c_cc[VMIN] = 1;
	PROGTTY.c_cc[VTIME] = 0;
	cur_term->_fl_rawmode = 1;
    }
    reset_prog_mode();
}
#endif	/* SYSV */
