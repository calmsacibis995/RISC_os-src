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
#ident	"$Header: time.c,v 1.2.1.2 90/05/07 20:44:42 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#if defined(LIBC_SCCS) && !defined(lint)
static char sccsid[] = "@(#)time.c	5.3 (Berkeley) 3/9/86";
#endif LIBC_SCCS and not lint

/*
 * Backwards compatible time call.
 */
#include <sys/types.h>
#include <sys/time.h>

long
time(t)
	time_t *t;
{
	struct timeval tt;

	if (gettimeofday(&tt, (struct timezone *)0) < 0)
		return (-1);
	if (t)
		*t = tt.tv_sec;
	return (tt.tv_sec);
}
