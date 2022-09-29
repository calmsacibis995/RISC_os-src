#ident "$Header: ptrace.h,v 1.2 90/01/23 13:47:08 huang Exp $"
/* $Copyright$ */

/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ptrace.h	7.1 (Berkeley) 6/4/86
 */

#ifndef _PTRACE_
#define _PTRACE_

/*
 * ptrace request numbers
 */
#define PT_TRACE_ME	0	/* child declares it's being traced */
#define PT_READ_I	1	/* read word in child's I space */
#define PT_READ_D	2	/* read word in child's D space */
#define PT_READ_U	3	/* read registers in child's user structure */
#define PT_WRITE_I	4	/* write word in child's I space */
#define PT_WRITE_D	5	/* write word in child's D space */
#define PT_WRITE_U	6	/* write registers in child's user structure */
#define PT_CONTINUE	7	/* continue the child */
#define PT_KILL		8	/* kill the child process */
#define PT_STEP		9	/* single step the child */

#ifdef mips

/*
 * mips specific ptrace request numbers
 */
#define	PT_TRACE_ON	20	/* enable tracing on process */
#define	PT_TRACE_OFF	28	/* disable tracing on process */
#define	PT_READ_GPRS	21	/* read all user gp registers */
#define	PT_READ_FPRS	22	/* read all user fp registers */
#define	PT_WRITE_GPRS	24	/* read all user gp registers */
#define	PT_WRITE_FPRS	25	/* read all user fp registers */
#define	PT_SET_WP	26	/* set a watchpoint */
#define	PT_DEL_WP	27	/* delete a watchpoint */
#define	PT_AOUTFD	29	/* return file descriptor for traced a.out */

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
struct watchpoint {
	int	*wp_addr;		/* starting address for watchpoint */
	int	wp_bcnt;		/* number of bytes in watchpoint */
};

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

#endif _PTRACE_
