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
#ident	"$Header: signal.c,v 1.2.1.2 90/05/07 20:42:35 wje Exp $"

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

int (*
signal(s, a))()
	int s, (*a)();
{
	struct sigvec osv, sv;
	static int mask[NSIG];
	static int flags[NSIG];

	sv.sv_handler = a;
	sv.sv_mask = mask[s];
	sv.sv_flags = flags[s];
	if (sigvec(s, &sv, &osv) < 0)
		return (BADSIG);
	if (sv.sv_mask != osv.sv_mask || sv.sv_flags != osv.sv_flags) {
		mask[s] = sv.sv_mask = osv.sv_mask;
		flags[s] = sv.sv_flags = osv.sv_flags;
		if (sigvec(s, &sv, 0) < 0)
			return (BADSIG);
	}
	return (osv.sv_handler);
}
