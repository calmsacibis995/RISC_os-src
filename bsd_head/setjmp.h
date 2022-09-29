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
/* $Header: setjmp.h,v 1.2.2.2 90/05/07 20:08:37 wje Exp $ */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * jmp_buf offsets
 * This should really just be a struct sigcontext, but for historical
 * reasons ....
 * NOTE: THIS MUST MATCH the initial portion of struct sigcontext,
 *	sc_onsigstk, sc_sigmask, sc_pc, sc_regs, sc_mdlo, sc_mdhi,
 *	fpregs, and fpc_csr
 * must lie at offset equal to the corresponding entries in the jmp_buf
 * since longjmp performs a sigcleanup.
 * See libc routines setjmp/longjmp/sigvec, and kernel routines
 * sendsig/sigcleanup.
 */
#define	BSD43_JB_ONSIGSTK	0		/* onsigstack flag */
#define	BSD43_JB_SIGMASK	1		/* signal mask */
#define	BSD43_JB_PC	2		/* program counter */
#define	BSD43_JB_REGS	3		/* registers */
#define	BSD43_JB_ZERO	(BSD43_JB_REGS+0)	/* register zero */
#define	BSD43_JB_MAGIC	(BSD43_JB_ZERO)	/* magic number saved at reg 0 */
#define	BSD43_JB_AT	(BSD43_JB_REGS+1)	/* AT */
#define	BSD43_JB_V0	(BSD43_JB_REGS+2)	/* function result regs */
#define	BSD43_JB_V1	(BSD43_JB_REGS+3)
#define	BSD43_JB_A0	(BSD43_JB_REGS+4)	/* argument regs */
#define	BSD43_JB_A1	(BSD43_JB_REGS+5)
#define	BSD43_JB_A2	(BSD43_JB_REGS+6)
#define	BSD43_JB_A3	(BSD43_JB_REGS+7)
#define	BSD43_JB_T0	(BSD43_JB_REGS+8)	/* caller saved regs */
#define	BSD43_JB_T1	(BSD43_JB_REGS+9)
#define	BSD43_JB_T2	(BSD43_JB_REGS+10)
#define	BSD43_JB_T3	(BSD43_JB_REGS+11)
#define	BSD43_JB_T4	(BSD43_JB_REGS+12)
#define	BSD43_JB_T5	(BSD43_JB_REGS+13)
#define	BSD43_JB_T6	(BSD43_JB_REGS+14)
#define	BSD43_JB_T7	(BSD43_JB_REGS+15)
#define	BSD43_JB_S0	(BSD43_JB_REGS+16) /* callee saved regs */
#define	BSD43_JB_S1	(BSD43_JB_REGS+17)
#define	BSD43_JB_S2	(BSD43_JB_REGS+18)
#define	BSD43_JB_S3	(BSD43_JB_REGS+19)
#define	BSD43_JB_S4	(BSD43_JB_REGS+20)
#define	BSD43_JB_S5	(BSD43_JB_REGS+21)
#define	BSD43_JB_S6	(BSD43_JB_REGS+22)
#define	BSD43_JB_S7	(BSD43_JB_REGS+23)
#define	BSD43_JB_T8	(BSD43_JB_REGS+24) /* temps */
#define	BSD43_JB_T9	(BSD43_JB_REGS+25)
#define	BSD43_JB_K0	(BSD43_JB_REGS+26) /* kernel regs */
#define	BSD43_JB_K1	(BSD43_JB_REGS+27)
#define	BSD43_JB_GP	(BSD43_JB_REGS+28) /* frame pointer */
#define	BSD43_JB_SP	(BSD43_JB_REGS+29) /* stack pointer */
#define	BSD43_JB_FP	(BSD43_JB_REGS+30) /* frame pointer */
#define	BSD43_JB_RA	(BSD43_JB_REGS+31) /* return address */

#define	BSD43_JB_FREGS	38		/* floating-point registers */
#define	BSD43_JB_F0	(BSD43_JB_FREGS+0) /* function result regs */
#define	BSD43_JB_F1	(BSD43_JB_FREGS+1)
#define	BSD43_JB_F2	(BSD43_JB_FREGS+2)
#define	BSD43_JB_F3	(BSD43_JB_FREGS+3)
#define	BSD43_JB_F4	(BSD43_JB_FREGS+4) /* caller save regs */
#define	BSD43_JB_F5	(BSD43_JB_FREGS+5)
#define	BSD43_JB_F6	(BSD43_JB_FREGS+6)
#define	BSD43_JB_F7	(BSD43_JB_FREGS+7)
#define	BSD43_JB_F8	(BSD43_JB_FREGS+8)
#define	BSD43_JB_F9	(BSD43_JB_FREGS+9)
#define	BSD43_JB_F10	(BSD43_JB_FREGS+10)
#define	BSD43_JB_F11	(BSD43_JB_FREGS+11)
#define	BSD43_JB_F12	(BSD43_JB_FREGS+12) /* argument regs */
#define	BSD43_JB_F13	(BSD43_JB_FREGS+13)
#define	BSD43_JB_F14	(BSD43_JB_FREGS+14)
#define	BSD43_JB_F15	(BSD43_JB_FREGS+15)
#define	BSD43_JB_F16	(BSD43_JB_FREGS+16) /* caller save regs */
#define	BSD43_JB_F17	(BSD43_JB_FREGS+17)
#define	BSD43_JB_F18	(BSD43_JB_FREGS+18)
#define	BSD43_JB_F19	(BSD43_JB_FREGS+19)
#define	BSD43_JB_F20	(BSD43_JB_FREGS+20) /* callee save regs */
#define	BSD43_JB_F21	(BSD43_JB_FREGS+21)
#define	BSD43_JB_F22	(BSD43_JB_FREGS+22)
#define	BSD43_JB_F23	(BSD43_JB_FREGS+23)
#define	BSD43_JB_F24	(BSD43_JB_FREGS+24)
#define	BSD43_JB_F25	(BSD43_JB_FREGS+25)
#define	BSD43_JB_F26	(BSD43_JB_FREGS+26)
#define	BSD43_JB_F27	(BSD43_JB_FREGS+27)
#define	BSD43_JB_F28	(BSD43_JB_FREGS+28)
#define	BSD43_JB_F29	(BSD43_JB_FREGS+29)
#define	BSD43_JB_F30	(BSD43_JB_FREGS+30)
#define	BSD43_JB_F31	(BSD43_JB_FREGS+31)
#define BSD43_JB_FPC_CSR	(BSD43_JB_FREGS+32) /* fp control and status register */

/*
 * WARNING: a jmp_buf must be as large as a sigcontext since
 * longjmp uses one to perform a sigreturn
 */
#define	BSD43_SIGCONTEXT_PAD	48
#define	BSD43_NJBREGS		(BSD43_JB_RA+1+BSD43_SIGCONTEXT_PAD)

/*
 * These are not part of a jmpbuf, but are part of the sigcontext
 * and are referenced from the signal trampoline code in sigvec.s
 */
#define	BSD43_SC_MDLO		(BSD43_JB_REGS+32)
#define	BSD43_SC_MDHI		(BSD43_JB_REGS+33)

#define	BSD43_JBMAGIC		0xacedbade

#ifdef LANGUAGE_C
#ifndef LOCORE
typedef	int	bsd43_(jmp_buf)[BSD43_NJBREGS];
#endif !LOCORE
#endif LANGUAGE_C

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define JBMAGIC BSD43_JBMAGIC
#   define JB_A0 BSD43_JB_A0
#   define JB_A1 BSD43_JB_A1
#   define JB_A2 BSD43_JB_A2
#   define JB_A3 BSD43_JB_A3
#   define JB_AT BSD43_JB_AT
#   define JB_F0 BSD43_JB_F0
#   define JB_F1 BSD43_JB_F1
#   define JB_F10 BSD43_JB_F10
#   define JB_F11 BSD43_JB_F11
#   define JB_F12 BSD43_JB_F12
#   define JB_F13 BSD43_JB_F13
#   define JB_F14 BSD43_JB_F14
#   define JB_F15 BSD43_JB_F15
#   define JB_F16 BSD43_JB_F16
#   define JB_F17 BSD43_JB_F17
#   define JB_F18 BSD43_JB_F18
#   define JB_F19 BSD43_JB_F19
#   define JB_F2 BSD43_JB_F2
#   define JB_F20 BSD43_JB_F20
#   define JB_F21 BSD43_JB_F21
#   define JB_F22 BSD43_JB_F22
#   define JB_F23 BSD43_JB_F23
#   define JB_F24 BSD43_JB_F24
#   define JB_F25 BSD43_JB_F25
#   define JB_F26 BSD43_JB_F26
#   define JB_F27 BSD43_JB_F27
#   define JB_F28 BSD43_JB_F28
#   define JB_F29 BSD43_JB_F29
#   define JB_F3 BSD43_JB_F3
#   define JB_F30 BSD43_JB_F30
#   define JB_F31 BSD43_JB_F31
#   define JB_F4 BSD43_JB_F4
#   define JB_F5 BSD43_JB_F5
#   define JB_F6 BSD43_JB_F6
#   define JB_F7 BSD43_JB_F7
#   define JB_F8 BSD43_JB_F8
#   define JB_F9 BSD43_JB_F9
#   define JB_FP BSD43_JB_FP
#   define JB_FPC_CSR BSD43_JB_FPC_CSR
#   define JB_FREGS BSD43_JB_FREGS
#   define JB_GP BSD43_JB_GP
#   define JB_K0 BSD43_JB_K0
#   define JB_K1 BSD43_JB_K1
#   define JB_MAGIC BSD43_JB_MAGIC
#   define JB_ONSIGSTK BSD43_JB_ONSIGSTK
#   define JB_PC BSD43_JB_PC
#   define JB_RA BSD43_JB_RA
#   define JB_REGS BSD43_JB_REGS
#   define JB_S0 BSD43_JB_S0
#   define JB_S1 BSD43_JB_S1
#   define JB_S2 BSD43_JB_S2
#   define JB_S3 BSD43_JB_S3
#   define JB_S4 BSD43_JB_S4
#   define JB_S5 BSD43_JB_S5
#   define JB_S6 BSD43_JB_S6
#   define JB_S7 BSD43_JB_S7
#   define JB_SIGMASK BSD43_JB_SIGMASK
#   define JB_SP BSD43_JB_SP
#   define JB_T0 BSD43_JB_T0
#   define JB_T1 BSD43_JB_T1
#   define JB_T2 BSD43_JB_T2
#   define JB_T3 BSD43_JB_T3
#   define JB_T4 BSD43_JB_T4
#   define JB_T5 BSD43_JB_T5
#   define JB_T6 BSD43_JB_T6
#   define JB_T7 BSD43_JB_T7
#   define JB_T8 BSD43_JB_T8
#   define JB_T9 BSD43_JB_T9
#   define JB_V0 BSD43_JB_V0
#   define JB_V1 BSD43_JB_V1
#   define JB_ZERO BSD43_JB_ZERO
#   define NJBREGS BSD43_NJBREGS
#   define SC_MDHI BSD43_SC_MDHI
#   define SC_MDLO BSD43_SC_MDLO
#   define SIGCONTEXT_PAD BSD43_SIGCONTEXT_PAD
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/
