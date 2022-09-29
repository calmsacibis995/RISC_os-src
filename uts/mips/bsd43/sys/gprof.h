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
/* $Header: gprof.h,v 1.6.3.2 90/05/10 04:51:53 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)gprof.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


struct bsd43_(phdr) {
    char	*lpc;
    char	*hpc;
    int		ncnt;
};

    /*
     *	histogram counters are unsigned shorts (according to the kernel).
     */
#define	BSD43_HISTCOUNTER	unsigned short

    /*
     *	fraction of text space to allocate for histogram counters
     *	here, 1/2
     */
#define	BSD43_HISTFRACTION	2

    /*
     *	Fraction of text space to allocate for from hash buckets.
     *	The value of HASHFRACTION is based on the minimum number of bytes
     *	of separation between two subroutine call points in the object code.
     *	Given MIN_SUBR_SEPARATION bytes of separation the value of
     *	HASHFRACTION is calculated as:
     *
     *		HASHFRACTION = MIN_SUBR_SEPARATION / (2 * sizeof(short) - 1);
     *
     *	For the VAX, the shortest two call sequence is:
     *
     *		calls	$0,(r0)
     *		calls	$0,(r0)
     *
     *	which is separated by only three bytes, thus HASHFRACTION is 
     *	calculated as:
     *
     *		HASHFRACTION = 3 / (2 * 2 - 1) = 1
     *
     *	Note that the division above rounds down, thus if MIN_SUBR_FRACTION
     *	is less than three, this algorithm will not work!
     *
     *	NB: for the kernel we assert that the shortest two call sequence is:
     *
     *		calls	$0,_name
     *		calls	$0,_name
     *
     *	which is separated by seven bytes, thus HASHFRACTION is calculated as:
     *
     *		HASHFRACTION = 7 / (2 * 2 - 1) = 2
     */
#define	BSD43_HASHFRACTION	2

    /*
     *	percent of text space to allocate for tostructs
     *	with a minimum.
     */
#define BSD43_ARCDENSITY	2
#define BSD43_MINARCS		50

struct bsd43_(tostruct) {
    char		*selfpc;
    long		count;
    unsigned short	link;
};

    /*
     *	a raw arc,
     *	    with pointers to the calling site and the called site
     *	    and a count.
     */
struct bsd43_(rawarc) {
    unsigned long	raw_frompc;
    unsigned long	raw_selfpc;
    long		raw_count;
};

    /*
     *	general rounding functions.
     */
#define BSD43_ROUNDDOWN(x,y)	(((x)/(y))*(y))
#define BSD43_ROUNDUP(x,y)	((((x)+(y)-1)/(y))*(y))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ARCDENSITY BSD43_ARCDENSITY
#   define HASHFRACTION BSD43_HASHFRACTION
#   define HISTCOUNTER BSD43_HISTCOUNTER
#   define HISTFRACTION BSD43_HISTFRACTION
#   define MINARCS BSD43_MINARCS
#   define ROUNDDOWN BSD43_ROUNDDOWN
#   define ROUNDUP BSD43_ROUNDUP
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


