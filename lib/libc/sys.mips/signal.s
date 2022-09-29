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
/* $Header: signal.s,v 1.14.2.2 90/05/09 19:58:07 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>
#define INKERNEL
#include <sys/signal.h>
#undef INKERNEL
#include <sys/errno.h>
#include "sys/syscall.h"

LEAF(sighold)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	or	a0,a0,SIGHOLD		/* Turn on SIGHOLD bit */
	li	v0,SYS_signal		/* Call ssig() */
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(sighold)

LEAF(sigrelse)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	or	a0,a0,SIGRELSE		/* Turn on SIGRELSE bit */
	li	v0,SYS_signal		/* Call ssig() */
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(sighold)

LEAF(sigignore)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	or	a0,a0,SIGIGNORE		/* Turn on SIGIGNORE bit */
	li	v0,SYS_signal		/* Call ssig() */
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(sigignore)

LEAF(sigpause)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	or	a0,a0,SIGPAUSE		/* Turn on SIGPAUSE bit */
	li	v0,SYS_signal		/* Call ssig() */
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(sigpause)

LEAF(sigset)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	or	a0,a0,SIGDEFER		/* Turn on SIGDEFER bit */
	la	a2,_sigtramp
	li	v0,SYS_signal
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(sigset)

LEAF(signal)
	ble	a0,0,ninvalid		/* Make sure 0 < sig < MAXSIG */
	bge	a0,MAXSIG,ninvalid
	la	a2,_sigtramp		/* No special signal bit set here */
	li	v0,SYS_signal
	syscall
	beq	a3,zero,1f
	j	_cerror
1:	RET(signal)

ninvalid:
	li	v0,EINVAL
	j	_cerror

#define	SIG_SCTXTPTR	4
#define	SIGFRAME	(8*4)

/*
 * Sigtramp is called by the kernel as:
 * 	sigtramp(signal, code, sigcontext_ptr, sighandler)
 * On entry to sigtramp the sp is equal to the sigcontext_ptr.
 * Sigtramp should build a frame appropriate to the language calling
 * conventions and then call the sighandler.  When the sighandler
 * returns, sigtramp does a sigcleanup system call passing the
 * address of the sigcontext struct.
 */
NESTED(_sigtramp, SIGFRAME, ra)
	subu	sp,SIGFRAME
	sw	a2,SIG_SCTXTPTR*4(sp)	# save address of sigcontext
	jal	a3			# call signal handler
	lw	a0,SIG_SCTXTPTR*4(sp)	# sigcleanup(&sigcontext)
	li	v0,SYS_sigreturn
	syscall
	END(_sigtramp)
