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
/* $Header: time.h,v 1.7.1.3 90/05/10 04:10:29 wje Exp $ */

#ifndef	_POSIX_TIME_
#define	_POSIX_TIME_	1

#include <sysv/time.h>

#ifndef	NULL
#define NULL 	0
#endif

#ifndef	CLK_TCK
#define CLK_TCK	100
#endif 

#ifndef	_TIME_T
#define _TIME_T
typedef	long	time_t;
#endif

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

#ifndef	_CLOCK_T
#define _CLOCK_T
typedef	unsigned long 	clock_t;
#endif

extern time_t	time(), mktime();
extern void 	tzset();
extern size_t	strftime();

#endif	_POSIX_TIME_
