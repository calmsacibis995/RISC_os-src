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
/* $Header: machparam.h,v 1.7.2.2 90/05/10 04:41:58 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Machine dependent constants for mips.
 */

#define	BSD43_NBPG		4096		/* bytes/page */
#define	BSD43_PGOFSET		(BSD43_NBPG-1)	/* byte offset into page */
#define	BSD43_PGSHIFT		12		/* LOG2(NBPG) */
#define	BSD43_DBSHIFT		9		/* LOG2(Disk block size) */

#define	BSD43_CLSIZE		1
#define	BSD43_CLSIZELOG2	0

#define	BSD43_SSIZE		1		/* initial stack size/NBPG */
#define	BSD43_SINCR		1		/* increment of stack/NBPG */

#define	BSD43_UPAGES		2		/* pages of u-area */
#define	BSD43_KERNELSTACK	0xffffe000	/* Top of kernel stack */
#define	BSD43_UADDR		0xffffc000	/* address of u */
#define	BSD43_UVPN		(BSD43_UADDR>>BSD43_PGSHIFT)/* virtual page number of u */

/*
 * Some macros for units conversion
 */
/* Core clicks (4096 bytes) to segments and vice versa */
#define	bsd43_ctos(x)	(x)
#define	bsd43_stoc(x)	(x)

/* Core clicks (4096 bytes) to disk blocks */
#define	bsd43_ctod(x)	((x)<<(BSD43_PGSHIFT-BSD43_DBSHIFT))
#define	bsd43_dtoc(x)	((unsigned)(x)>>(BSD43_PGSHIFT-BSD43_DBSHIFT))
#define	bsd43_dtob(x)	((x)<<BSD43_DBSHIFT)

/* clicks to bytes */
#define	bsd43_ctob(x)	((x)<<BSD43_PGSHIFT)

/* bytes to clicks */
#define	bsd43_btoc(x)	(((unsigned)(x)+BSD43_PGOFSET)>>BSD43_PGSHIFT)

/*
 * Macros to decode processor status word.
 */
#define	BSD43_USERMODE(sr)	(((sr) & BSD43_SR_KUP) == BSD43_SR_KUP)
#define	BSD43_BASEPRI(sr)	(((sr) & BSD43_SR_IMASK) \
					== (BSD43_SR_IMASK&ipl_special_mask))

/*
 * DELAY(n) should be n microseconds, roughly.  This is a first guess.
 */
#ifdef SABLE
#define	BSD43_DELAY(n)	{ register int N = 3*(n); while (--N > 0); }
#else SABLE
#define	BSD43_DELAY(n)	{ \
	extern int bsd43_(delay_mult); \
	register int N = bsd43_(delay_mult)*(n); \
	while (--N > 0); \
	}
#endif SABLE

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BASEPRI BSD43_BASEPRI
#   define CLSIZE BSD43_CLSIZE
#   define CLSIZELOG2 BSD43_CLSIZELOG2
#   define DBSHIFT BSD43_DBSHIFT
#   define DELAY BSD43_DELAY
#   define KERNELSTACK BSD43_KERNELSTACK
#   define NBPG BSD43_NBPG
#   define PGOFSET BSD43_PGOFSET
#   define PGSHIFT BSD43_PGSHIFT
#   define SINCR BSD43_SINCR
#   define SSIZE BSD43_SSIZE
#   define UADDR BSD43_UADDR
#   define UPAGES BSD43_UPAGES
#   define USERMODE BSD43_USERMODE
#   define UVPN BSD43_UVPN
#   define btoc bsd43_btoc
#   define ctob bsd43_ctob
#   define ctod bsd43_ctod
#   define ctos bsd43_ctos
#   define dtob bsd43_dtob
#   define dtoc bsd43_dtoc
#   define stoc bsd43_stoc
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


