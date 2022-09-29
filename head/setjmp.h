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
/* $Header: setjmp.h,v 1.10.3.2 90/05/10 01:03:24 wje Exp $ */

#ifndef	_SETJMP_
#define	_SETJMP_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef _JBLEN

#if vax || M32 || u3b15 || u3b5 || u3b2
#define _JBLEN	10
#endif

#if pdp11
#define _JBLEN	3
#endif

#if u370
#define _JBLEN	4
#endif

#if u3b
#define _JBLEN	11
#endif

#if mips
/*
 * jmp_buf offsets
 * NOTE: entries between JB_ONSIGSTK and JB_V0 must match struct sigcontext.
 * See libc routines setjmp/longjmp, signal, and kernel routines
 * sendsig/sigcleanup.
 */
#define	JB_ONSIGSTK	0	/* onsigstack flag */
#define	JB_SIGMASK	1	/* signal mask */
#define	JB_SP		2	/* stack pointer */
#define	JB_PC		3	/* program counter */
#define	JB_V0		4	/* longjmp retval */
#define	JB_S0		5	/* callee saved regs.... */
#define	JB_S1		6
#define	JB_S2		7
#define	JB_S3		8
#define	JB_S4		9
#define	JB_S5		10
#define	JB_S6		11
#define	JB_S7		12
#define	JB_S8		13	/* frame pointer */
#define	JB_F20		14	/* callee save regs */
#define	JB_F21		15
#define	JB_F22		16
#define	JB_F23		17
#define	JB_F24		18
#define	JB_F25		19
#define	JB_F26		20
#define	JB_F27		21
#define	JB_F28		22
#define	JB_F29		23
#define	JB_F30		24
#define	JB_F31		25
#define JB_FPC_CSR	26	/* fp control and status register */
#define	JB_MAGIC	27

#define	JB_FP		13	/* frame pointer -- obsolete */

#define	JBMAGIC		0xacedbade

#define	NJBREGS		28
#define	_JBLEN		NJBREGS
#endif mips

#ifdef LANGUAGE_C
typedef int jmp_buf[_JBLEN];

extern int setjmp();
extern void longjmp();
#endif LANGUAGE_C

#endif

#endif	_SETJMP_
