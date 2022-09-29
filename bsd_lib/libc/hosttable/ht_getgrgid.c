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
#ident	"$Header: ht_getgrgid.c,v 1.2.1.2 90/05/07 20:46:02 wje Exp $"

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)getgrgid.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

#include <grp.h>

struct group *
ht_getgrgid(gid)
register gid;
{
	register struct group *p;
	struct group *ht_getgrent();

	ht_setgrent();
	while( (p = ht_getgrent()) && p->gr_gid != gid );
	ht_endgrent();
	return(p);
}
