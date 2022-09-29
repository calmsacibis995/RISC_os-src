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
/* $Header: stdio.h,v 1.2.1.6 90/05/10 04:09:23 wje Exp $ */

#ifndef	_POSIX_STDIO_
#define	_POSIX_STDIO_	1

#include <sysv/stdio.h>

#ifndef _SIZE_T
#define _SIZE_T
typedef	unsigned int	size_t;
#endif

extern int remove();
extern int rename();
extern void perror();

#endif	_POSIX_STDIO_
