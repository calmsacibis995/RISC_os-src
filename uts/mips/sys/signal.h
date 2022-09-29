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
/* $Header: signal.h,v 1.17.3.2 90/05/10 06:37:29 wje Exp $ */

#ifndef	_SYS_SIGNAL_
#define	_SYS_SIGNAL_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt (rubout) */
#define	SIGQUIT	3	/* quit (ASCII FS) */
#define	SIGILL	4	/* illegal instruction (not reset when caught)*/
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGIOT	6	/* IOT instruction */
#define SIGABRT 6	/* used by abort, replace SIGIOT in the  future */

#define	SIGEMT	7	/* EMT instruction */
/* 4.2 signal */
#define SIGXCPU	7	/* exceeded CPU time limit */
			/* -- overloads SIGEMT (not possible on RISC/os) */

#define	SIGFPE	8	/* floating point exception */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define	SIGSEGV	11	/* segmentation violation */
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#define	SIGALRM	14	/* alarm clock */
#define	SIGTERM	15	/* software termination signal from kill */
#define	SIGUSR1	16	/* user defined signal 1 */
#define	SIGUSR2	17	/* user defined signal 2 */
#define	SIGCLD	18	/* death of a child */
#define SIGCHLD SIGCLD	/* 4.3BSD's name */
#define	SIGPWR	19	/* power-fail restart */
/* 4.2 signal */
#define SIGXFSZ	19	/* exceeded file size limit */
			/* -- overloads SIGPWR (not possible on RISC/os) */


			/* SIGWIND and SIGPHONE only used in UNIX/PC */
/*#define SIGWIND 20*/	/* window change */
/*#define SIGPHONE 21*/	/* handset, line status change */

/* 4.2 signals */
#define	SIGSTOP	20	/* sendable stop signal not from tty */
#define	SIGTSTP	21	/* stop signal from tty */

#define SIGPOLL 22	/* pollable event occured */

/* 4.2 signals */
#define	SIGIO	23	/* input/output possible signal */
#define	SIGURG	24	/* urgent condition on IO channel */
#define	SIGWINCH 25	/* window size changes */
#define SIGVTALRM 26	/* virtual time alarm */
#define SIGPROF	27	/* profiling alarm */

/* 4.3BSD job control */
#define	SIGCONT	28	/* continue a stopped process */
#define	SIGTTIN	29	/* to readers pgrp upon background tty read */
#define	SIGTTOU	30	/* like TTIN for output if (tp->t_local&LTOSTOP) */

#define SIGLOST	31	/* resource lost (eg, record-lock) */

#define	NSIG	32	/* The valid signal number is from 1 to NSIG-1 */
#define MAXSIG	32	/* size of u_signal[], NSIG-1 <= MAXSIG*/
			/* MAXSIG is larger than we need now. */
			/* In the future, we can add more signal */
			/* number without changing user.h */

#if lint
#define SIG_ERR (void(*)())0
#else
#define SIG_ERR	(void(*)())-1
#endif
#define	SIG_DFL	(void(*)())0
#if lint
#define	SIG_IGN	(void (*)())0
#else
#define	SIG_IGN	(void (*)())1
#endif
#if lint
#define SIG_HOLD (void(*)())0
#else
#define SIG_HOLD (void(*)())2
#endif

/* The following definitions are only used by kernel */
#ifdef INKERNEL
#define SIGNO_MASK	0xff
#define SIGDEFER	0x100
#define SIGHOLD		0x200
#define SIGRELSE	0x400
#define SIGIGNORE	0x800
#define SIGPAUSE	0x1000

/* Possible save/restore interfaces encountered when issuing a signal */
#define SIG_TRAP	0
#define SIG_SYSCALL	1
#define SIG_SIGRET	2
#endif

/* Convert signal number to bit mask representation */
#define sigmask(sig)	(1L << ((sig)-1))

#define	BRK_USERBP	0	/* user bp (used by debuggers) */
#define	BRK_KERNELBP	1	/* kernel bp (used by prom) */
#define	BRK_ABORT	2	/* abort(3) uses to cause SIGIOT */
#define	BRK_BD_TAKEN	3	/* for taken bd emulation */
#define	BRK_BD_NOTTAKEN	4	/* for not taken bd emulation */
#define	BRK_SSTEPBP	5	/* user bp (used by debuggers) */
#define	BRK_OVERFLOW	6	/* overflow check */
#define	BRK_DIVZERO	7	/* divide by zero check */
#define	BRK_RANGE	8	/* range error check */
#define	BRK_STACKOVERFLOW 9	/* used by Ada */
#define	BRK_MULOVF	1023	/* multiply overflow detected */

#ifdef LANGUAGE_C
/*
 * Information pushed on stack when a signal is delivered.
 * This is used by the kernel to restore state following
 * execution of the signal handler.  It is also made available
 * to the handler to allow it to properly restore state if
 * a non-standard exit is performed.
 *
 * sc_regmask is examined by the kernel when doing sigcleanup()'s
 * and indicates which registers to restore from sc_regs
 * bit 0 == 1 indicates that all coprocessor state should be restored
 *	for each coprocessor that has been used
 * bits 1 - 31 == 1 indicate registers 1 to 31 should be restored by
 *	sigcleanup from sc_regs.
 *
 * NOTE: the libc routines setjmp/longjmp must agree with this.
 */
struct sigcontext {
	int	sc_regmask;		/* regs to restore in sigcleanup */
	int	sc_mask;		/* signal mask to restore */
	int	sc_pc;			/* pc at time of signal */
	/*
	 * General purpose registers
	 */
	int	sc_regs[32];	/* processor regs 0 to 31 */
	/*
	 * Floating point coprocessor state
	 */
	int	sc_ownedfp;	/* fp has been used */
	int	sc_fpregs[32];	/* fp regs 0 to 31 */
	int	sc_fpc_csr;	/* floating point control and status reg */
	int	sc_fpc_eir;	/* floating point exception instruction reg */
	int	sc_mdhi;	/* Multiplier hi and low regs */
	int	sc_mdlo;
	/*
	 * System coprocessor registers at time of signal
	 */
	int	sc_cause;	/* cp0 cause register */
	int	sc_badvaddr;	/* cp0 bad virtual address */
	int	sc_badpaddr;	/* cpu bd bad physical address */
	int	is_sigset;
};
#endif LANGUAGE_C

#endif	_SYS_SIGNAL_
