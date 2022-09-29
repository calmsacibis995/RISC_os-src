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
#ident	"$Header: wrtmsg.c,v 1.4.2.2 90/05/09 16:28:33 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* wrtmsg(user, msg) -- write message to user's tty if logged in.
	return codes: TRUE ==> success,
		      FALSE ==> failure
*/

#include	"lp.h"
#include	"lpsched.h"


wrtmsg(user, msg)
char *user;
char *msg;
{
	char *tty, *findtty();
	FILE *f;
	int sigalrm();
	unsigned alarm(), sleep();

	if((tty = findtty(user)) == 0 || (f = fopen(tty, "w")) == NULL)
		return(FALSE);
	signal(SIGALRM, sigalrm);
	alarm(10);
	fputc(BEL, f);
	fflush(f);
	sleep(2);
	fputc(BEL, f);
	fprintf(f, "\nlp: %s\n", msg);
	alarm(0);
	fclose(f);
	return(TRUE);
}

/* sigalrm() -- catch SIGALRM */

static int
sigalrm()
{
}
