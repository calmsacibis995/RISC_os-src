#ident "$Header: signal.h,v 1.2 90/01/23 13:47:13 huang Exp $"
/* %Q% %M% %I% */
/* $Copyright$ */

#ifndef	NSIG
#define NSIG	32

#define	SIGHUP	1	/* hangup */
#define	SIGINT	2	/* interrupt */
#define	SIGQUIT	3	/* quit */
#define	SIGILL	4	/* illegal instruction (not reset when caught) */
#define	SIGTRAP	5	/* trace trap (not reset when caught) */
#define	SIGIOT	6	/* IOT instruction */
#define	SIGEMT	7	/* EMT instruction */
#define	SIGFPE	8	/* floating point exception */
#define	SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	SIGBUS	10	/* bus error */
#define	SIGSEGV	11	/* segmentation violation */
#define	SIGSYS	12	/* bad argument to system call */
#define	SIGPIPE	13	/* write on a pipe with no one to read it */
#define	SIGALRM	14	/* alarm clock */
#define	SIGTERM	15	/* software termination signal from kill */
#define	SIGURG	16	/* urgent condition on IO channel */
#define	SIGSTOP	17	/* sendable stop signal not from tty */
#define	SIGTSTP	18	/* stop signal from tty */
#define	SIGCONT	19	/* continue a stopped process */
#define	SIGCHLD	20	/* to parent on child stop or exit */
#define	SIGTTIN	21	/* to readers pgrp upon background tty read */
#define	SIGTTOU	22	/* like TTIN for output if (tp->t_local&LTOSTOP) */
#define	SIGIO	23	/* input/output possible signal */
#define	SIGXCPU	24	/* exceeded CPU time limit */
#define	SIGXFSZ	25	/* exceeded file size limit */
#define	SIGVTALRM 26	/* virtual time alarm */
#define	SIGPROF	27	/* profiling time alarm */

#define	BRK_USERBP	0	/* user bp (used by debuggers) */
#define	BRK_KERNELBP	1	/* kernel bp (used by prom) */
#define	BRK_ABORT	2	/* abort(3) uses to cause SIGIOT */
#define	BRK_BD_TAKEN	3	/* for taken bd emulation */
#define	BRK_BD_NOTTAKEN	4	/* for not taken bd emulation */
#define	BRK_SSTEPBP	5	/* user bp (used by debuggers) */
#define	BRK_OVERFLOW	6	/* overflow check */
#define	BRK_DIVZERO	7	/* divide by zero check */
#define	BRK_RANGE	8	/* range error check */

#ifdef LANGUAGE_C
#ifndef KERNEL
int	(*signal())();
#endif !KERNEL

/*
 * Signal vector "template" used in sigvec call.
 */
struct	sigvec {
	int	(*sv_handler)();	/* signal handler */
	int	sv_mask;		/* signal mask to apply */
	int	sv_onstack;		/* if non-zero, take on signal stack */
};

/*
 * Structure used in sigstack call.
 */
struct	sigstack {
	char	*ss_sp;			/* signal stack pointer */
	int	ss_onstack;		/* current status */
};

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
	int	sc_onstack;		/* sigstack state to restore */
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
	/*
	 * System coprocessor registers at time of signal
	 */
	int	sc_cause;	/* cp0 cause register */
	int	sc_badvaddr;	/* cp0 bad virtual address */
	int	sc_badpaddr;	/* cpu bd bad physical address */
};

#define	BADSIG		(int (*)())-1
#define	SIG_DFL		(int (*)())0
#define	SIG_IGN		(int (*)())1

#ifdef KERNEL
#define	SIG_CATCH	(int (*)())2
#define	SIG_HOLD	(int (*)())3
#endif KERNEL
#endif LANGUAGE_C
#endif !NSIG
