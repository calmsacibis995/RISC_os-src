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
/* $Header: kernel.h,v 1.7.1.2 90/05/10 04:52:09 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)kernel.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Global variables for the kernel
 */

long	bsd43_(rmalloc)();

/* 1.1 */
long	hostid;
char	bsd43_(hostname)[BSD43_MAXHOSTNAMELEN];
int	bsd43_(hostnamelen);
char	bsd43_(domainname)[BSD43_MAXDOMNAMELEN];
int	bsd43_(domainnamelen);

/* 1.2 */
struct	bsd43_(timeval) boottime;
struct	bsd43_(timeval) time;
struct	bsd43_(timezone) tz;			/* XXX */
int	bsd43_(hz);
int	bsd43_(phz);				/* alternate clock's frequency */
int	bsd43_(tick);
int	bsd43_(lbolt);				/* awoken once a second */
int	bsd43_(realitexpire)();

#ifdef vax
double	avenrun[3];
#endif vax

#ifdef PROFILING
extern	int bsd43_(profiling);
extern	char *bsd43_(s_lowpc);
extern	u_long bsd43_(s_textsize);
extern	unsigned int *bsd43_(kcount);
#endif PROFILING

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


