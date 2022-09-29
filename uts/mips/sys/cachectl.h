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
/* $Header: cachectl.h,v 1.4.4.2 90/05/10 06:06:34 wje Exp $ */

#ifndef	_SYS_CACHECTL_
#define	_SYS_CACHECTL_	1


/*
 * cachectl.h -- defines for MIPS cache control system calls
 */

/*
 * Options for cacheflush system call
 */
#define	ICACHE	0x1		/* flush i cache */
#define	DCACHE	0x2		/* flush d cache */
#define	BCACHE	(ICACHE|DCACHE)	/* flush both caches */

/*
 * Options for cachectl system call
 */
#define	CACHEABLE	0	/* make page(s) cacheable */
#define	UNCACHEABLE	1	/* make page(s) uncacheable */

#endif	_SYS_CACHECTL_
