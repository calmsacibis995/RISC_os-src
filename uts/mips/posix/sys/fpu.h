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
/* $Header: fpu.h,v 1.1.1.2 90/05/10 06:02:17 wje Exp $ */

#ifndef	_POSIX_SYS_FPU_
#define	_POSIX_SYS_FPU_	1

#ifdef mips
#ifndef	KERNEL
#include <sysv/sys/fpu.h>
#endif	/* not KERNEL */
#endif mips


#endif	_POSIX_SYS_FPU_
