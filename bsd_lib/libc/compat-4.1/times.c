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
#ident	"$Header: times.c,v 1.1.2.2 90/05/07 20:23:32 wje Exp $"


#include <sys/time.h>
#include <sys/resource.h>

/*
 * Backwards compatible times.
 */
struct tms {
	int	tms_utime;		/* user time */
	int	tms_stime;		/* system time */
	int	tms_cutime;		/* user time, children */
	int	tms_cstime;		/* system time, children */
};

times(tmsp)
	register struct tms *tmsp;
{
	struct rusage ru;

	if (getrusage(RUSAGE_SELF, &ru) < 0)
		return (-1);
	tmsp->tms_utime = scale60(&ru.ru_utime);
	tmsp->tms_stime = scale60(&ru.ru_stime);
	if (getrusage(RUSAGE_CHILDREN, &ru) < 0)
		return (-1);
	tmsp->tms_cutime = scale60(&ru.ru_utime);
	tmsp->tms_cstime = scale60(&ru.ru_stime);
	return (0);
}

static
scale60(tvp)
	register struct timeval *tvp;
{

	return (tvp->tv_sec * 60 + tvp->tv_usec / 16667);
}
