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
/* $Header: machdep.h,v 1.1.2.2 90/05/10 03:24:02 wje Exp $ */

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)machdep.h	5.1 (Berkeley) 5/29/85
 */

#define LOGFILE "/usr/games/lib/saillog"	/* has to match the makefile */

#define TIMEOUT 300				/* Sync() timeout in seconds */

/* for 4.2bsd machines */
#define blockalarm()	((void) sigblock(1 << SIGALRM-1))
#define unblockalarm()	((void) sigsetmask(sigblock(0) & ~(1 << SIGALRM-1)))

/* for 2.9bsd machines (onyx)
typedef int void;
#define blockalarm()	((void) sighold(SIGALRM))
#define unblockalarm()	((void) sigrelse(SIGALRM))
*/
