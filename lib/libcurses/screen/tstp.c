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
#ident	"$Header: tstp.c,v 1.2.1.3 90/05/10 02:25:19 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	<signal.h>
#include	"curses_inc.h"

#ifdef RISCOS
#include <bsd43/sys/syscall.h>
#define kill(x,y) syscall(BSD43_SYS_kill,(x),(y))
#define signal sigset
#endif RISCOS

/* handle stop and start signals */

#ifdef	SIGTSTP
_tstp()
{
    SGTTY	tty;
#ifdef	DEBUG
    if (outf)
	(void) fflush(outf);
#endif	/* DEBUG */
    tty = PROGTTY;
    curscr->_attrs = A_ATTRIBUTES;
    (void) endwin();
    (void) fflush(stdout);
    /* reset signal handler so kill below stops us */
    signal(SIGTSTP, SIG_DFL);
    sigrelse(SIGTSTP);
    kill(0, SIGTSTP);
    sighold(SIGTSTP);
    (void) signal(SIGTSTP, _tstp);
    PROGTTY = tty;
    fixterm();
    /* changed ehr3 SP->doclear = 1; */
    curscr->_clear = TRUE;
    (void) wrefresh(curscr);
}
#endif	/* SIGTSTP */

_ccleanup(signo)
int	signo;
{
    (void) signal(signo, SIG_IGN);

    /*
     * Fake curses into thinking that all attributes are on so that
     * endwin will turn them off since the <BREAK> key may have interrupted
     * the sequence to turn them off.
     */

    curscr->_attrs = A_ATTRIBUTES;
    (void) endwin();
#ifdef	DEBUG
    fprintf(stderr, "signal %d caught. quitting.\n", signo);
#endif	/* DEBUG */
    if (signo == SIGQUIT)
	(void) abort();
    else
	exit(1);
}
