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
/* $Header: signal.h,v 1.8.3.2 90/05/10 04:55:18 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)signal.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef BSD43_SYS_SIGNAL_
#define BSD43_SYS_SIGNAL_

#ifndef	BSD43_NSIG
#define BSD43_NSIG	32

/*
 *	The BSD signal numbers have been rearanged from Stock BSD to
 *	match SYSV signal numbers where the signals are the same but
 *	the numbers were not.
 *
 * WARNING: 
 *	SIGXCPU and SIGXFSZ overlap with the signals SIGEMT and SIGPWR
 *	respectively.  Neither signal is sent by our kernel, but a program
 *	which tries to handle -- or send -- either signal, and which also 
 *	tries to use resource limits, could have problems.
 */
#define	BSD43_SIGHUP	1	/* hangup */
#define	BSD43_SIGINT	2	/* interrupt */
#define	BSD43_SIGQUIT	3	/* quit */
#define	BSD43_SIGILL	4	/* illegal instruction (not reset when caught)*/
#define	BSD43_SIGTRAP	5	/* trace trap (not reset when caught) */
#define	BSD43_SIGIOT	6	/* IOT instruction */
#define	BSD43_SIGABRT	BSD43_SIGIOT	/* compatibility */
#define	BSD43_SIGEMT	7	/* EMT instruction */
#define BSD43_SIGXCPU	7		/* exceeded CPU time limit */
#define	BSD43_SIGFPE	8	/* floating point exception */
#define	BSD43_SIGKILL	9	/* kill (cannot be caught or ignored) */
#define	BSD43_SIGBUS	10	/* bus error */
#define	BSD43_SIGSEGV	11	/* segmentation violation */
#define	BSD43_SIGSYS	12	/* bad argument to system call */
#define	BSD43_SIGPIPE	13	/* write on a pipe with no one to read it */
#define	BSD43_SIGALRM	14	/* alarm clock */
#define	BSD43_SIGTERM	15	/* software termination signal from kill */
#define BSD43_SIGUSR1	16	/* user defined signal 1 */
#define BSD43_SIGUSR2	17	/* user defined signal 2 */
#define	BSD43_SIGCHLD	18	/* to parent on child stop or exit */
#define	BSD43_SIGCLD	BSD43_SIGCHLD	/* compatibility */
#define BSD43_SIGXFSZ	19		/* exceeded file size limit */
#define BSD43_SIGSTOP	20	/* sendable stop signal not from tty */
#define BSD43_SIGTSTP	21	/* stop signal from tty */
/* #define SIGPOLL	22 */	/* (sysv poll() only) */
#define BSD43_SIGIO	23	/* input/output possible signal */
#define BSD43_SIGURG	24	/* urgent condition on IO channel */
#define BSD43_SIGWINCH	25	/* window size changes */
#define BSD43_SIGVTALRM	26	/* virtual time alarm */
#define BSD43_SIGPROF	27	/* profiling time alarm */

#define BSD43_SIGCONT	28	/* continue a stopped process */
#define BSD43_SIGTTIN	29	/* to readers pgrp upon background tty read */
#define BSD43_SIGTTOU	30	/* like TTIN for output if (tp->t_local&LTOSTOP) */
#define BSD43_SIGLOST	31		/* resource lost (eg, record-lock) */

/*
 * Codes for the mips break instruction.
 */
#define	BSD43_BRK_USERBP		0	/* user bp (used by debuggers) */
#define	BSD43_BRK_KERNELBP	1	/* kernel bp (used by prom) */
#define	BSD43_BRK_ABORT		2	/* no longer used */
#define	BSD43_BRK_BD_TAKEN	3	/* for taken bd emulation */
#define	BSD43_BRK_BD_NOTTAKEN	4	/* for not taken bd emulation */
#define	BSD43_BRK_SSTEPBP		5	/* user bp (used by debuggers) */
#define	BSD43_BRK_OVERFLOW	6	/* overflow check */
#define	BSD43_BRK_DIVZERO		7	/* divide by zero check */
#define	BSD43_BRK_RANGE		8	/* range error check */
#define	BSD43_BRK_STACKOVERLFOW	9	/* used by Ada */
#define	BSD43_BRK_MULOVF		1023	/* multiply overflow detected */

#ifdef LANGUAGE_C
#ifndef KERNEL
int	(*bsd43_(signal)())();
#endif !KERNEL

/*
 * Signal vector "template" used in sigvec call.
 */
struct	bsd43_(sigvec) {
	int	(*sv_handler)();	/* signal handler */
	int	sv_mask;		/* signal mask to apply */
	int	sv_flags;		/* see signal options below */
};
#define BSD43_SV_ONSTACK	0x0001	/* take signal on signal stack */
#define BSD43_SV_INTERRUPT	0x0002	/* do not restart system on signal return */
#define bsd43_sv_onstack sv_flags	/* isn't compatibility wonderful! */

/*
 * Structure used in sigstack call.
 */
struct	bsd43_(sigstack) {
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
 * WARNING: THE sigcontext MUST BE KEPT CONSISTENT WITH /usr/include/setjmp.h
 * AND THE LIBC ROUTINES setjmp() AND longjmp()
 */
struct bsd43_(sigcontext) {
	/*
	 * BEGIN REGION THAT MUST CORRESPOND WITH setjmp.h
	 * BEGIN REGION THAT MUST CORRESPOND WITH A jmp_buf
	 */
	int	sc_onstack;		/* sigstack state to restore */
	int	sc_mask;		/* signal mask to restore */
	int	sc_pc;			/* pc at time of signal */
	/*
	 * General purpose registers
	 */
	int	sc_regs[32];	/* processor regs 0 to 31 */
	int	sc_mdlo;	/* mul/div low */
	int	sc_mdhi;	/* mul/div high */
	/*
	 * Floating point coprocessor state
	 */
	int	sc_ownedfp;	/* fp has been used */
	int	sc_fpregs[32];	/* fp regs 0 to 31 */
	int	sc_fpc_csr;	/* floating point control and status reg */
	int	sc_fpc_eir;	/* floating point exception instruction reg */
	/*
	 * END OF REGION THAT MUST AGREE WITH setjmp.h
	 * END OF jmp_buf REGION
	 */
	/*
	 * System coprocessor registers at time of signal
	 */
	int	sc_cause;	/* cp0 cause register */
	int	sc_badvaddr;	/* cp0 bad virtual address */
	int	sc_badpaddr;	/* cpu bd bad physical address */
};

#define	BSD43_BADSIG	(int (*)())-1
#define	BSD43_SIG_DFL	(int (*)())0
#define	BSD43_SIG_IGN	(int (*)())1

#ifdef KERNEL
#define	BSD43_SIG_CATCH	(int (*)())2
#define	BSD43_SIG_HOLD	(int (*)())3
#endif KERNEL
#endif LANGUAGE_C
#endif !BSD43_NSIG

/*
 * Macro for converting signal number to a mask suitable for
 * sigblock().
 */
#define bsd43_sigmask(m)	(1 << ((m)-1))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BADSIG BSD43_BADSIG
#   define BRK_ABORT BSD43_BRK_ABORT
#   define BRK_BD_NOTTAKEN BSD43_BRK_BD_NOTTAKEN
#   define BRK_BD_TAKEN BSD43_BRK_BD_TAKEN
#   define BRK_DIVZERO BSD43_BRK_DIVZERO
#   define BRK_KERNELBP BSD43_BRK_KERNELBP
#   define BRK_OVERFLOW BSD43_BRK_OVERFLOW
#   define BRK_RANGE BSD43_BRK_RANGE
#   define BRK_SSTEPBP BSD43_BRK_SSTEPBP
#   define BRK_USERBP BSD43_BRK_USERBP
#   define NSIG BSD43_NSIG
#   define SIGABRT BSD43_SIGABRT
#   define SIGALRM BSD43_SIGALRM
#   define SIGBUS BSD43_SIGBUS
#   define SIGCHLD BSD43_SIGCHLD
#   define SIGCLD BSD43_SIGCLD
#   define SIGCONT BSD43_SIGCONT
#   define SIGEMT BSD43_SIGEMT
#   define SIGFPE BSD43_SIGFPE
#   define SIGHUP BSD43_SIGHUP
#   define SIGILL BSD43_SIGILL
#   define SIGINT BSD43_SIGINT
#   define SIGIO BSD43_SIGIO
#   define SIGIOT BSD43_SIGIOT
#   define SIGKILL BSD43_SIGKILL
#   define SIGLOST BSD43_SIGLOST
#   define SIGPIPE BSD43_SIGPIPE
#   define SIGPROF BSD43_SIGPROF
#   define SIGQUIT BSD43_SIGQUIT
#   define SIGSEGV BSD43_SIGSEGV
#   define SIGSTOP BSD43_SIGSTOP
#   define SIGSYS BSD43_SIGSYS
#   define SIGTERM BSD43_SIGTERM
#   define SIGTRAP BSD43_SIGTRAP
#   define SIGTSTP BSD43_SIGTSTP
#   define SIGTTIN BSD43_SIGTTIN
#   define SIGTTOU BSD43_SIGTTOU
#   define SIGURG BSD43_SIGURG
#   define SIGUSR1 BSD43_SIGUSR1
#   define SIGUSR2 BSD43_SIGUSR2
#   define SIGVTALRM BSD43_SIGVTALRM
#   define SIGWINCH BSD43_SIGWINCH
#   define SIGXCPU BSD43_SIGXCPU
#   define SIGXFSZ BSD43_SIGXFSZ
#   define SIG_DFL BSD43_SIG_DFL
#   define SIG_IGN BSD43_SIG_IGN
#   define SV_INTERRUPT BSD43_SV_INTERRUPT
#   define SV_ONSTACK BSD43_SV_ONSTACK
#   define sigmask bsd43_sigmask
#   define sv_onstack bsd43_sv_onstack
#   ifdef KERNEL
#	define SIG_CATCH BSD43_SIG_CATCH
#	define SIG_HOLD BSD43_SIG_HOLD
#   endif KERNEL
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


#endif BSD43_SYS_SIGNAL_
