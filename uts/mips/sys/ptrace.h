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
/* $Header: ptrace.h,v 1.6.4.2 90/05/10 06:32:38 wje Exp $ */

#ifndef	_SYS_PTRACE_
#define	_SYS_PTRACE_	1

#ifdef mips

/*
 * ptrace.h -- definitions for use with ptrace() system call
 */

/*
 * ptrace request numbers
 */
#define PT_TRACE_ME	0		/* enable tracing on this process */
#define PT_READ_I	1		/* read user I space */
#define PT_READ_D	2		/* read user D space */
#define PT_READ_U	3		/* read user registers */
#define PT_WRITE_I	4		/* write user I space */
#define PT_WRITE_D	5		/* write user D space */
#define PT_WRITE_U	6		/* write user registers */
#define PT_CONTINUE	7		/* continue, possibly with signal */
#define PT_KILL		8		/* terminate traced process */
#define PT_STEP		9		/* single step traced process */

/*
 * This are for backward compatibility.
 */
#define PTRC_RD_I	PT_READ_I
#define PTRC_RD_D	PT_READ_D
#define PTRC_RD_REG	PT_READ_U
#define PTRC_WR_I	PT_WRITE_I
#define PTRC_WR_D	PT_WRITE_D
#define PTRC_WR_REG	PT_WRITE_U
#define PTRC_CONTINUE	PT_CONTINUE
#define PTRC_STEP	PT_STEP
#define PTRC_TERMINATE	PT_KILL

/* mips local ptrace requests */
#define	PTRC_TRACEON	20		/* enable tracing on process */
#define	PTRC_TRACEOFF	28		/* disable tracing on process */
#define	PTRC_RD_GPRS	21		/* read all user gp registers */
#define	PTRC_RD_FPRS	22		/* read all user fp registers */
#define	PTRC_WR_GPRS	24		/* read all user gp registers */
#define	PTRC_WR_FPRS	25		/* read all user fp registers */
#define	PTRC_SET_WP	26		/* set a watchpoint */
#define	PTRC_DEL_WP	27		/* set a watchpoint */
#define	PTRC_AOUTFD	29		/* return fd for traced a.out */

/*
 * register number definitions for PT_READ_U's and PT_WRITE_U's
 */
#define GPR_BASE	0			/* general purpose regs */
#define	NGP_REGS	32			/* number of gpr's */

#define FPR_BASE	(GPR_BASE+NGP_REGS)	/* fp regs */
#define	NFP_REGS	32			/* number of fp regs */

#define	SIG_BASE	(FPR_BASE+NFP_REGS)	/* sig handler addresses */
#define	NSIG_HNDLRS	32			/* number of signal handlers */

#define SPEC_BASE	(SIG_BASE+NSIG_HNDLRS)	/* base of spec purpose regs */
#define PC		SPEC_BASE+0		/* program counter */
#define	CAUSE		SPEC_BASE+1		/* cp0 cause register */
#define MMHI		SPEC_BASE+2		/* multiply high */
#define MMLO		SPEC_BASE+3		/* multiply low */
#define FPC_CSR		SPEC_BASE+4		/* fp csr register */
#define FPC_EIR		SPEC_BASE+5		/* fp eir register */
#define TRAPCAUSE	SPEC_BASE+6		/* multiplex SIGTRAP cause */
#define TRAPINFO	SPEC_BASE+7		/* associated info to SIGTRAP */
#define NSPEC_REGS	8			/* number of spec registers */
#define NPTRC_REGS	(SPEC_BASE + NSPEC_REGS)

#define MAXWIDS		8		/* max # watchpoints per process */

/*
 * causes for SIGTRAP
 */
#define CAUSEEXEC	1		/* traced process exec'd */
#define CAUSEFORK	2		/* traced process fork'd */
#define CAUSEWATCH	3		/* traced process hit a watchpoint */
#define CAUSESINGLE	4		/* traced process executed 1 instr */
#define CAUSEBREAK	5		/* traced process hit a breakpoint */
#define	CAUSETRACEON	6		/* initial trap after PTRC_TRACEON */

#endif mips

#endif	_SYS_PTRACE_
