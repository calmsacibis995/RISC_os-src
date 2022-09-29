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
#ident	"$Header: sysconf.c,v 1.4.1.3 90/05/10 04:14:29 wje Exp $"

#ifndef lint
#endif

#include <sys/sysmips.h>
#include <time.h>
#include <unistd.h>
#include <limits.h>
#include <errno.h>

extern	int errno;

long
sysconf(name)
	unsigned name;
{

	switch (name) {
	case _SC_ARG_MAX:
		return(sysmips(GET_ARG_MAX));

	case _SC_CHILD_MAX:
		return(sysmips(GET_CHILD_MAX));

	case _SC_CLK_TCK:
		return(sysmips(GET_CLK_TCK));

	case _SC_NGROUPS_MAX:
		return(sysmips(GET_NGROUPS_MAX));

	case _SC_OPEN_MAX:
	case _SC_STREAM_MAX:
		return(sysmips(GET_OPEN_MAX));

	case _SC_JOB_CONTROL:
		return(_POSIX_JOB_CONTROL);

	case _SC_SAVED_IDS:
		return(_POSIX_SAVED_IDS);

	case _SC_VERSION:
		return(_POSIX_VERSION);

	case _SC_TZNAME_MAX:
		return(TZNAME_MAX);

	default:
		errno = EINVAL;
		return(-1);
	}

}
