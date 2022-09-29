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
#ident	"$Header: getlogin.c,v 1.3.1.2 90/05/07 20:37:29 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getlogin.c	5.3 (Berkeley) 5/9/86";
#endif LIBC_SCCS and not lint

#include <utmp.h>

static	char UTMP[]	= "/etc/utmp";
static	struct utmp ubuf;

char *
getlogin()
{
	register int me, uf;
	register char *cp;

	if (!(me = ttyslot()))
		return(0);
	if ((uf = open(UTMP, 0)) < 0)
		return (0);
	lseek (uf, (long)(me*sizeof(ubuf)), 0);
	if (read(uf, (char *)&ubuf, sizeof (ubuf)) != sizeof (ubuf)) {
		close(uf);
		return (0);
	}
	close(uf);
	if (ubuf.ut_name[0] == '\0')
		return (0);
	if (ubuf.ut_type != USER_PROCESS) {
		return (0);
	}
	ubuf.ut_name[sizeof (ubuf.ut_name)] = ' ';
	for (cp = ubuf.ut_name; *cp++ != ' '; )
		;
	*--cp = '\0';
	return (ubuf.ut_name);
}
