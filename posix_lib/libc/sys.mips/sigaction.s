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
/* $Header: sigaction.s,v 1.4.1.2 90/05/10 04:18:10 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>
#include "sys/syscall.h"
#include <setjmp.h>

/*
 * struct sigaction {
 *	int (*sa_handler)();
 *	int sa_mask;
 *	int sa_flags;
 * };
 *
 * sigaction(sig, act, oact);
 * int sig;
 * struct sigaction *act, *oact;
 *
 * NOTE:  Implement this by passing a hidden argument to the kernel which
 * point to the sigmanager.  sendsig then passes actual signal handler
 * to sigmanager as a parameter.
 */

LEAF(sigaction)
	la	a3,_sigtramp		# hidden arg -- address of tramp code
	li	v0,SYS_sigaction
	syscall
	bne	a3,zero,err
	RET(sigaction)

err:
	j	_cerror

#define	SIG_ARG0	0
#define	SIG_ARG1	1
#define	SIG_ARG2	2
#define	SIG_ARG3	3
#define	SIG_SCTXTPTR	4

#define	SIGFRAME	(8*4)	/* should be quad word aligned */

/*
 * Sigtramp is called by the kernel as:
 * 	sigtramp(signal, code, sigcontext_ptr, sighandler)
 *
 * Sigtramp should build a frame appropriate to the language calling
 * conventions and then call the sighandler.  When the sighandler
 * returns, sigtramp does a sigcleanup system call passing the
 * address of the sigcontext struct.
 */
NESTED(_sigtramp, SIGFRAME, ra)
	/*
	 * Save process state.
	 *
	 * NOTE: sp, v0, a0, a1, a2, and a3 are saved into sigcontext by
	 * by the kernel in sendsig, on a sigreturn the kernel copies the
	 * entire state indicated by the sigcontext into the exception
	 * frame and then returns to user mode via a special exit that
	 * restore the entire process state from the exception frame
	 * (unlike the normal syscall exit which assumes that the C
	 * calling sequence alleviates the necessity of preserving
	 * certain portions of the process state)
	 */
	.set	noat
	sw	AT,SJB_AT*4(a2)
	.set	at
	sw	zero,SJB_ZERO*4(a2)	# just in case someone looks
	sw	v1,SJB_V1*4(a2)
	sw	t0,SJB_T0*4(a2)
	sw	t1,SJB_T1*4(a2)
	sw	t2,SJB_T2*4(a2)
	sw	t3,SJB_T3*4(a2)
	sw	t4,SJB_T4*4(a2)
	sw	t5,SJB_T5*4(a2)
	sw	t6,SJB_T6*4(a2)
	sw	t7,SJB_T7*4(a2)
	sw	s0,SJB_S0*4(a2)
	sw	s1,SJB_S1*4(a2)
	sw	s2,SJB_S2*4(a2)
	sw	s3,SJB_S3*4(a2)
	sw	s4,SJB_S4*4(a2)
	sw	s5,SJB_S5*4(a2)
	sw	s6,SJB_S6*4(a2)
	sw	s7,SJB_S7*4(a2)
	sw	t8,SJB_T8*4(a2)
	sw	t9,SJB_T9*4(a2)
	sw	gp,SJB_GP*4(a2)
	sw	fp,SJB_FP*4(a2)
	sw	ra,SJB_RA*4(a2)
	mflo	t0
	mfhi	t1
	sw	t0,SC_MDLO*4(a2)
	sw	t1,SC_MDHI*4(a2)
	subu	sp,SIGFRAME
	sw	a2,SIG_SCTXTPTR*4(sp)	# save address of sigcontext
	jal	a3			# call signal handler
	lw	a0,SIG_SCTXTPTR*4(sp)	# sigreturn(&sigcontext)
	/*
	 * sigreturn will restore entire user state from sigcontext
	 * struct
	 */
	li	v0,SYS_sigreturn
	syscall
	.set	at
	END(_sigtramp)
