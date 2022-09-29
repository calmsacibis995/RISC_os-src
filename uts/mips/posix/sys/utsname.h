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
/* $Header: utsname.h,v 1.1.1.2 90/05/10 06:04:07 wje Exp $ */

#ifndef	_POSIX_SYS_UTSNAME_
#define	_POSIX_SYS_UTSNAME_

#ifndef	KERNEL
#include <sysv/sys/utsname.h>
#endif	/* not KERNEL */

extern	int	uname();

#endif	_POSIX_SYS_UTSNAME_
