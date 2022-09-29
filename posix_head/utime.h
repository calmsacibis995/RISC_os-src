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
/* $Header: utime.h,v 1.1.1.2 90/05/10 04:10:43 wje Exp $ */

#ifndef	_POSIX_UTIME_
#define	_POSIX_UTIME_	1

struct	utimbuf {	/* see utime(2) */
	time_t	actime;
	time_t	modtime;
};

extern	int 	utime();

#endif	_POSIX_UTIME_
