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
/* $Header: ptrace.h,v 1.6.3.2 90/05/10 04:54:10 wje Exp $ */
/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ptrace.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef BSD43__PTRACE_
#define BSD43__PTRACE_

/*
 * ptrace request numbers
 */
#define BSD43_PT_TRACE_ME	0	/* child declares it's being traced */
#define BSD43_PT_READ_I	1	/* read word in child's I space */
#define BSD43_PT_READ_D	2	/* read word in child's D space */
#define BSD43_PT_READ_U	3	/* read registers in child's user structure */
#define BSD43_PT_WRITE_I	4	/* write word in child's I space */
#define BSD43_PT_WRITE_D	5	/* write word in child's D space */
#define BSD43_PT_WRITE_U	6	/* write registers in child's user structure */
#define BSD43_PT_CONTINUE	7	/* continue the child */
#define BSD43_PT_KILL		8	/* kill the child process */
#define BSD43_PT_STEP		9	/* single step the child */

#ifdef mips

/*
 * register number definitions for PT_READ_U's and PT_WRITE_U's
 */
#define BSD43_GPR_BASE	0			/* general purpose regs */
#define	BSD43_NGP_REGS	32			/* number of gpr's */

#define BSD43_FPR_BASE	(BSD43_GPR_BASE+BSD43_NGP_REGS)	/* fp regs */
#define	BSD43_NFP_REGS	32			/* number of fp regs */

#define	BSD43_SIG_BASE	(BSD43_FPR_BASE+BSD43_NFP_REGS)	/* sig handler addresses */
#define	BSD43_NSIG_HNDLRS	32			/* number of signal handlers */

#define BSD43_SPEC_BASE	(BSD43_SIG_BASE+BSD43_NSIG_HNDLRS)	/* base of spec purpose regs */
#define BSD43_PC		BSD43_SPEC_BASE+0		/* program counter */
#define	BSD43_CAUSE		BSD43_SPEC_BASE+1		/* cp0 cause register */
#define BSD43_MMHI		BSD43_SPEC_BASE+2		/* multiply high */
#define BSD43_MMLO		BSD43_SPEC_BASE+3		/* multiply low */
#define BSD43_FPC_CSR		BSD43_SPEC_BASE+4		/* fp csr register */
#define BSD43_FPC_EIR		BSD43_SPEC_BASE+5		/* fp eir register */
#define BSD43_TRAPCAUSE	BSD43_SPEC_BASE+6		/* multiplex SIGTRAP cause */
#define BSD43_TRAPINFO	BSD43_SPEC_BASE+7		/* associated info to SIGTRAP */
#define BSD43_NSPEC_REGS	8			/* number of spec registers */
#define BSD43_NPTRC_REGS	(BSD43_SPEC_BASE + BSD43_NSPEC_REGS)

/*
 * causes for SIGTRAP
 */
#define BSD43_CAUSEEXEC	1		/* traced process exec'd */
#define BSD43_CAUSEFORK	2		/* traced process fork'd */
#define BSD43_CAUSEWATCH	3		/* traced process hit a watchpoint */
#define BSD43_CAUSESINGLE	4		/* traced process executed 1 instr */
#define BSD43_CAUSEBREAK	5		/* traced process hit a breakpoint */
#define	BSD43_CAUSETRACEON	6		/* initial trap after PTRC_TRACEON */

#endif mips

#endif BSD43__PTRACE_

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CAUSE BSD43_CAUSE
#   define CAUSEBREAK BSD43_CAUSEBREAK
#   define CAUSEEXEC BSD43_CAUSEEXEC
#   define CAUSEFORK BSD43_CAUSEFORK
#   define CAUSESINGLE BSD43_CAUSESINGLE
#   define CAUSETRACEON BSD43_CAUSETRACEON
#   define CAUSEWATCH BSD43_CAUSEWATCH
#   define FPC_CSR BSD43_FPC_CSR
#   define FPC_EIR BSD43_FPC_EIR
#   define FPR_BASE BSD43_FPR_BASE
#   define GPR_BASE BSD43_GPR_BASE
#   define MMHI BSD43_MMHI
#   define MMLO BSD43_MMLO
#   define NFP_REGS BSD43_NFP_REGS
#   define NGP_REGS BSD43_NGP_REGS
#   define NPTRC_REGS BSD43_NPTRC_REGS
#   define NSIG_HNDLRS BSD43_NSIG_HNDLRS
#   define NSPEC_REGS BSD43_NSPEC_REGS
#   define PC BSD43_PC
#   define PT_CONTINUE BSD43_PT_CONTINUE
#   define PT_KILL BSD43_PT_KILL
#   define PT_READ_D BSD43_PT_READ_D
#   define PT_READ_I BSD43_PT_READ_I
#   define PT_READ_U BSD43_PT_READ_U
#   define PT_STEP BSD43_PT_STEP
#   define PT_TRACE_ME BSD43_PT_TRACE_ME
#   define PT_WRITE_D BSD43_PT_WRITE_D
#   define PT_WRITE_I BSD43_PT_WRITE_I
#   define PT_WRITE_U BSD43_PT_WRITE_U
#   define SIG_BASE BSD43_SIG_BASE
#   define SPEC_BASE BSD43_SPEC_BASE
#   define TRAPCAUSE BSD43_TRAPCAUSE
#   define TRAPINFO BSD43_TRAPINFO
#   define _PTRACE_ BSD43__PTRACE_
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


