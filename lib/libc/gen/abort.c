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
#ident	"$Header: abort.c,v 1.8.2.2 90/05/10 01:28:02 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 *	abort() - terminate current process with dump via SIGABRT
 */

#include <signal.h>

extern int kill(), getpid();
static pass = 0;		/* counts how many times abort has been called*/

int
abort()
{
	void (*sig)();


#ifdef NOTDEF  /* ATT shipped this code, but it is not per SVID. */
	if ((sig = signal(SIGABRT,SIG_DFL)) != SIG_DFL) 
		(void) signal(SIGABRT,sig); 
	else if (++pass == 1)
#endif
		_cleanup();
	return(kill(getpid(), SIGABRT));
}
