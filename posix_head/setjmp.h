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
/* $Header: setjmp.h,v 1.8.1.2 90/05/10 04:08:59 wje Exp $ */

#ifndef	_POSIX_SETJMP_
#define	_POSIX_SETJMP_	1

#include <sysv/setjmp.h>

#define SJB_RESTOREMASK	0 		/* whether or not to restore sigmask */
#define	SJB_SIGMASK0	1		/* signal mask word 0 */
#define	SJB_SIGMASK1	2		/* signal mask word 1 */
#define	SJB_SIGMASK2	3		/* signal mask word 2 */
#define	SJB_SIGMASK3	4		/* signal mask word 3 */
#define	SJB_PC		5		/* program counter */
#define	SJB_REGS	6		/* registers */
#define	SJB_ZERO	(SJB_REGS+0)	/* register zero */
#define	SJB_MAGIC	(SJB_ZERO)	/* magic number saved at reg 0 */
#define	SJB_AT		(SJB_REGS+1)	/* AT */
#define	SJB_V0		(SJB_REGS+2)	/* function result regs */
#define	SJB_V1		(SJB_REGS+3)
#define	SJB_A0		(SJB_REGS+4)	/* argument regs */
#define	SJB_A1		(SJB_REGS+5)
#define	SJB_A2		(SJB_REGS+6)
#define	SJB_A3		(SJB_REGS+7)
#define	SJB_T0		(SJB_REGS+8)	/* caller saved regs */
#define	SJB_T1		(SJB_REGS+9)
#define	SJB_T2		(SJB_REGS+10)
#define	SJB_T3		(SJB_REGS+11)
#define	SJB_T4		(SJB_REGS+12)
#define	SJB_T5		(SJB_REGS+13)
#define	SJB_T6		(SJB_REGS+14)
#define	SJB_T7		(SJB_REGS+15)
#define	SJB_S0		(SJB_REGS+16) 	/* callee saved regs */
#define	SJB_S1		(SJB_REGS+17)
#define	SJB_S2		(SJB_REGS+18)
#define	SJB_S3		(SJB_REGS+19)
#define	SJB_S4		(SJB_REGS+20)
#define	SJB_S5		(SJB_REGS+21)
#define	SJB_S6		(SJB_REGS+22)
#define	SJB_S7		(SJB_REGS+23)
#define	SJB_T8		(SJB_REGS+24) 	/* temps */
#define	SJB_T9		(SJB_REGS+25)
#define	SJB_K0		(SJB_REGS+26) 	/* kernel regs */
#define	SJB_K1		(SJB_REGS+27)
#define	SJB_GP		(SJB_REGS+28) 	/* frame pointer */
#define	SJB_SP		(SJB_REGS+29) 	/* stack pointer */
#define	SJB_FP		(SJB_REGS+30) 	/* frame pointer */
#define	SJB_RA		(SJB_REGS+31) 	/* return address */

#define	SJB_FREGS	39		/* floating-point registers */
#define	SJB_F0		(SJB_FREGS+0) 	/* function result regs */
#define	SJB_F1		(SJB_FREGS+1)
#define	SJB_F2		(SJB_FREGS+2)
#define	SJB_F3		(SJB_FREGS+3)
#define	SJB_F4		(SJB_FREGS+4) /* caller save regs */
#define	SJB_F5		(SJB_FREGS+5)
#define	SJB_F6		(SJB_FREGS+6)
#define	SJB_F7		(SJB_FREGS+7)
#define	SJB_F8		(SJB_FREGS+8)
#define	SJB_F9		(SJB_FREGS+9)
#define	SJB_F10		(SJB_FREGS+10)
#define	SJB_F11		(SJB_FREGS+11)
#define	SJB_F12		(SJB_FREGS+12) /* argument regs */
#define	SJB_F13		(SJB_FREGS+13)
#define	SJB_F14		(SJB_FREGS+14)
#define	SJB_F15		(SJB_FREGS+15)
#define	SJB_F16		(SJB_FREGS+16) /* caller save regs */
#define	SJB_F17		(SJB_FREGS+17)
#define	SJB_F18		(SJB_FREGS+18)
#define	SJB_F19		(SJB_FREGS+19)
#define	SJB_F20		(SJB_FREGS+20) /* callee save regs */
#define	SJB_F21		(SJB_FREGS+21)
#define	SJB_F22		(SJB_FREGS+22)
#define	SJB_F23		(SJB_FREGS+23)
#define	SJB_F24		(SJB_FREGS+24)
#define	SJB_F25		(SJB_FREGS+25)
#define	SJB_F26		(SJB_FREGS+26)
#define	SJB_F27		(SJB_FREGS+27)
#define	SJB_F28		(SJB_FREGS+28)
#define	SJB_F29		(SJB_FREGS+29)
#define	SJB_F30		(SJB_FREGS+30)
#define	SJB_F31		(SJB_FREGS+31)
#define SJB_FPC_CSR	(SJB_FREGS+32) /* fp control and status register */
#define SJB_FPC_EIR	(SJB_FREGS+33) /* fp control and status register */


/*
 * WARNING: a jmp_buf must be as large as a posix_sigcontext since
 * siglongjmp uses one to perform a sigreturn
 */
#define	POSIX_SIGCONTEXT_PAD	72
#define	NSJBREGS		(SJB_RA+1+POSIX_SIGCONTEXT_PAD)

/*
 * These are not part of a sigjmpbuf, but are part of the posix_sigcontext
 */
#define	SC_MDLO		(SJB_FREGS+34)
#define	SC_MDHI		(SJB_FREGS+35)

#define	SJBMAGIC	0xacedbade

#define	_SJBLEN		NSJBREGS

#ifdef LANGUAGE_C
typedef int 	sigjmp_buf[_SJBLEN];

extern int 	sigsetjmp();
extern void 	siglongjmp();
#endif LANGUAGE_C

#endif	_POSIX_SETJMP_
