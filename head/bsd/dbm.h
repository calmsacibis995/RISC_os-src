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
/* $Header: dbm.h,v 1.7.1.2 90/05/09 19:47:36 wje Exp $ */

#ifndef	_BSD_DBM_
#define	_BSD_DBM_	1


/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dbm.h	5.2 (Berkeley) 85/06/26
 */

#ifndef NULL
/*
 * this is lunacy, we no longer use it (and never should have
 * unconditionally defined it), but, this whole file is for
 * backwards compatability - someone may rely on this.
 */
#define	NULL	((char *) 0)
#endif

#ifdef SYSTYPE_BSD43
#include <ndbm.h>
#endif
#ifdef SYSTYPE_SYSV
#include <bsd/ndbm.h>
#endif

datum	fetch();
datum	firstkey();
datum	nextkey();
#ifdef SYSTYPE_SYSV
void	dbmclose();
#endif
#if 0
datum	makdatum();
datum	firsthash();
long	calchash();
long	hashinc();
#endif

#endif	_BSD_DBM_
