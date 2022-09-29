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
#ident	"$Header: settz.c,v 1.1.1.2 90/05/09 18:55:04 wje Exp $"
#include <sys/param.h>
#include <sys/time.h>

main()

{
	struct timeval tv;
	struct timezone tz;
	struct tm *tp;

	if (gettimeofday(&tv, &tz)) {
		perror ("settz");
		exit (1);
	}

	tp = localtime ((time_t *)&tv.tv_sec);
	tz.tz_dsttime = tp->tm_isdst;
	tz.tz_minuteswest = - tp->tm_gmtoff/60;

	if (settimeofday ((struct timeval *) NULL, &tz)) {
		perror ("settz");
		exit (2);
	}

	exit (0);
}
