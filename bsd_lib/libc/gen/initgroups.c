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
#ident	"$Header: initgroups.c,v 1.2.1.2.1.2 90/08/20 17:51:59 hawkes Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)initgroups.c	5.3 (Berkeley) 4/27/86";
#endif LIBC_SCCS and not lint

/*
 * initgroups
 */
#include <stdio.h>
#include <sys/param.h>
#include <grp.h>

struct group *getgrent();

initgroups(uname, agroup)
	char *uname;
	int agroup;
{
	int groups[NGROUPS], ngroups = 0;
	register struct group *grp;
	register int i,j;

	if (agroup >= 0)
		groups[ngroups++] = agroup;
	setgrent();
	while (grp = getgrent()) {
		if (grp->gr_gid == agroup)
			continue;
		for (i = 0; grp->gr_mem[i]; i++)
			if (!strcmp(grp->gr_mem[i], uname)) {
		/* check for dups
		 * this can happen when multiple VIS info sources are
		 * invoked, and there is duplication
		 */
				for (j=0; j<ngroups;j++)
				{
				    if (groups[j] == grp->gr_gid)
					break;
				}
				if ( j < ngroups)  /* found a dup, skip this*/
				    continue;
				/* flow on if no dup */
				if (ngroups == NGROUPS) {
fprintf(stderr, "initgroups: %s is in too many groups\n", uname);
					goto toomany;
				}
				groups[ngroups++] = grp->gr_gid;
			}
	}
toomany:
	endgrent();
	if (setgroups(ngroups, groups) < 0) {
		perror("setgroups");
		return (-1);
	}
	return (0);
}
