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
/* $Header: bkmac.h,v 1.7.1.2 90/05/10 04:48:39 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)bkmac.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Macro definition of bk.c/netinput().
 * This is used to replace a call to
 *		(*linesw[tp->t_line].l_rint)(c,tp);
 * with
 *
 *		if (tp->t_line == NETLDISC)
 *			BKINPUT(c, tp);
 *		else
 *			(*linesw[tp->t_line].l_rint)(c,tp);
 */
#define	BSD43_BKINPUT(c, tp) { \
	if ((tp)->bsd43_t_rec == 0) { \
		*(tp)->bsd43_t_cp++ = c; \
		if (++(tp)->bsd43_t_inbuf == 1024 || (c) == '\n') { \
			(tp)->bsd43_t_rec = 1; \
			bsd43_(wakeup)((caddr_t)&(tp)->bsd43_t_rawq); \
		} \
	} \
}

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BKINPUT BSD43_BKINPUT
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


