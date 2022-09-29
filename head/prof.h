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
/* $Header: prof.h,v 1.8.3.2 90/05/10 01:02:55 wje Exp $ */

#ifndef	_PROF_
#define	_PROF_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef MARK
#define MARK(L)	{}
#else
#undef MARK
#ifdef vax
#define MARK(L)	{\
		asm("	.data");\
		asm("	.align	4");\
		asm(".L.:");\
		asm("	.long	0");\
		asm("	.text");\
		asm("M.L:");\
		asm("	nop;nop");\
		asm("	movab	.L.,r0");\
		asm("	jsb	mcount");\
		}
#endif
#if u3b || M32 || u3b15 || u3b5 || u3b2
#define MARK(L)	{\
		asm("	.data");\
		asm("	.align	4");\
		asm(".L.:");\
		asm("	.word	0");\
		asm("	.text");\
		asm("M.L:");\
		asm("	movw	&.L.,%r0");\
		asm("	jsb	_mcount");\
		}
#endif
#ifdef pdp11
#define MARK(L)	{\
		asm("	.bss");\
		asm(".L.:");\
		asm("	.=.+2");\
		asm("	.text");\
		asm("M.L:");\
		asm("	mov	$.L.,r0");\
		asm("	jsr	pc,mcount");\
		}
#endif
#ifdef mips
/*
 * The mips compiler does not support the asm() construct so you
 * can't use MARK to jump into mcount. You will have to compile
 * with -p to get entries to mcount() at the beginning of procedures.
 */
#endif mips
#endif

#endif	_PROF_
