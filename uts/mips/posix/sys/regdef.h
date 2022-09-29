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
/* $Header: regdef.h,v 1.1.1.2 90/05/10 06:02:30 wje Exp $ */

#ifndef	_POSIX_SYS_REGDEF_
#define	_POSIX_SYS_REGDEF_	1


/*
 * Mips register definitions.
 */

#ifdef mips
#ifndef	KERNEL
#include <sysv/sys/regdef.h>
#endif	/* not KERNEL */
#endif mips


#endif	_POSIX_SYS_REGDEF_
