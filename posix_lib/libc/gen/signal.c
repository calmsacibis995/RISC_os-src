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
#ident	"$Header: signal.c,v 1.1.1.2 90/05/10 04:14:01 wje Exp $"

/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)signal.c	5.2 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * Almost backwards compatible signal.
 */
#include <signal.h>

void
(*signal(s, a))()
	int s; 
	void (*a)();
{
	struct sigaction osa, sa;
	static sigset_t mask[NSIG];
	static int flags[NSIG];

	sa.sa_handler = a;
	sa.sa_mask = mask[s];
	sa.sa_flags = flags[s];
	if (sigaction(s, &sa, &osa) < 0)
		return ((void (*)())-1);
	if ((memcmp(&sa.sa_mask, &osa.sa_mask, sizeof(sa.sa_mask)) != 0) || 
	    sa.sa_flags != osa.sa_flags) {
		mask[s] = sa.sa_mask = osa.sa_mask;
		flags[s] = sa.sa_flags = osa.sa_flags;
		if (sigaction(s, &sa, 0) < 0)
			return ((void (*)())-1);
	}
	return (osa.sa_handler);
}
