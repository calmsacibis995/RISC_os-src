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
/* $Header: ttychars.h,v 1.7.2.2 90/05/10 04:57:29 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttychars.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * User visible structures and constants
 * related to terminal handling.
 */
#ifndef BSD43__TTYCHARS_
#define	BSD43__TTYCHARS_
struct bsd43_(ttychars) {
	char	tc_erase;	/* erase last character */
	char	tc_kill;	/* erase entire line */
	char	tc_intrc;	/* interrupt */
	char	tc_quitc;	/* quit */
	char	tc_startc;	/* start output */
	char	tc_stopc;	/* stop output */
	char	tc_eofc;	/* end-of-file */
	char	tc_brkc;	/* input delimiter (like nl) */
	char	tc_suspc;	/* stop process signal */
	char	tc_dsuspc;	/* delayed stop process signal */
	char	tc_rprntc;	/* reprint line */
	char	tc_flushc;	/* flush output (toggles) */
	char	tc_werasc;	/* word erase */
	char	tc_lnextc;	/* literal next character */
};

#define	BSD43_CTRL(c)	(c&037)

/* default special characters */
#define	BSD43_CERASE	0177
#define	BSD43_CKILL	BSD43_CTRL('u')
#define	BSD43_CINTR	BSD43_CTRL('c')
#define	BSD43_CQUIT	034		/* FS, ^\ */
#define	BSD43_CSTART	BSD43_CTRL('q')
#define	BSD43_CSTOP	BSD43_CTRL('s')
#define	BSD43_CEOF	BSD43_CTRL('d')
#define	BSD43_CEOT	BSD43_CEOF
#define	BSD43_CBRK	0377
#define	BSD43_CSUSP	BSD43_CTRL('z')
#define	BSD43_CDSUSP	BSD43_CTRL('y')
#define	BSD43_CRPRNT	BSD43_CTRL('r')
#define	BSD43_CFLUSH	BSD43_CTRL('o')
#define	BSD43_CWERASE	BSD43_CTRL('w')
#define	BSD43_CLNEXT	BSD43_CTRL('v')
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CBRK BSD43_CBRK
#   define CDSUSP BSD43_CDSUSP
#   define CEOF BSD43_CEOF
#   define CEOT BSD43_CEOT
#   define CERASE BSD43_CERASE
#   define CFLUSH BSD43_CFLUSH
#   define CINTR BSD43_CINTR
#   define CKILL BSD43_CKILL
#   define CLNEXT BSD43_CLNEXT
#   define CQUIT BSD43_CQUIT
#   define CRPRNT BSD43_CRPRNT
#   define CSTART BSD43_CSTART
#   define CSTOP BSD43_CSTOP
#   define CSUSP BSD43_CSUSP
#   define CTRL BSD43_CTRL
#   define CWERASE BSD43_CWERASE
#   define _TTYCHARS_ BSD43__TTYCHARS_
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


