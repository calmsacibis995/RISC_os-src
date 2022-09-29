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
#ident	"$Header: rename.c,v 1.5.2.3 90/05/10 01:10:03 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include "errno.h"
# include "fatal.h"
# include "signal.h"
/*
	rename (unlink/link)
	Calls xlink() and xunlink().
*/

rename(oldname,newname)
char *oldname, *newname;
{
	extern int errno;
	void (* holdsig[3])();
	int retval;
	
	/*Ignor signals 01 02 03 */
	holdsig[0] = signal(SIGHUP,SIG_IGN);
	holdsig[1] = signal(SIGINT,SIG_IGN);
	holdsig[2] = signal(SIGQUIT,SIG_IGN);
	if (unlink(newname) < 0 && errno != ENOENT)
		retval = xunlink(newname);

	if (xlink(oldname,newname) == Fvalue)
		retval = -1;
	retval = (xunlink(oldname));
	/*re establish signals */
	signal(SIGHUP,holdsig[0]);
	signal(SIGINT,holdsig[1]);
	signal(SIGQUIT,holdsig[2]);
	return(retval);
}
