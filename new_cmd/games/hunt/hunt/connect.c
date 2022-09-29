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
#ident	"$Header: connect.c,v 1.1.2.2 90/05/10 03:11:57 wje Exp $"

/*
 *  Hunt
 *  Copyright (c) 1985 Conrad C. Huang, Gregory S. Couch, Kenneth C.R.C. Arnold
 *  San Francisco, California
 *
 *  Copyright (c) 1985 Regents of the University of California.
 *  All rights reserved.  The Berkeley software License Agreement
 *  specifies the terms and conditions for redistribution.
 */

# include	"hunt.h"
# include	<signal.h>

do_connect(name)
char	*name;
{
	static long	uid;
	extern char	*ttyname();

	uid = htonl(getuid());
	(void) write(Socket, (char *) &uid, sizeof uid);
	(void) write(Socket, name, NAMELEN);
	(void) strcpy(Buf, ttyname(fileno(stderr)));
	(void) write(Socket, Buf, NAMELEN);
# ifdef MONITOR
	(void) write(Socket, (char *) &Am_monitor, sizeof Am_monitor);
# endif MONITOR
}
