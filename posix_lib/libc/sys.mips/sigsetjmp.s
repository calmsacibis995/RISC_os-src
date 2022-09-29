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
/* $Header: sigsetjmp.s,v 1.7.1.2 90/05/10 04:18:33 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <setjmp.h>
#include <sys/syscall.h>
#include <sys.s>
#include <sys/fpu.h>
#include <sys/signal.h>
/*
 * C library -- sigsetjmp, siglongjmp
 *
 *	siglongjmp(env, val)
 * will generate a "return(val)" from
 * the last call to
 *	sigsetjmp(env, savemask)
 * by restoring registers from the stack,
 * previous signal mask (if savemask is non-zero), and doing a return.
 *
 * NOTE: THIS MUST MATCH UP WITH THE POSIX_SIGCONTEXT STRUCTURE, 
 * be sure constants in setjmp.h and signal.h are consistent!
 * 
 * Whats happening here: sigsetjmp assumes that all process state except
 * the callee saved registers and the gp has been preserved by the
 * C calling sequence; therefore, sigsetjmp only saves the signal state
 * (sigmask and the signal flag), and the state that must be preserved
 * by the callee (callee saved regs, gp, sp, fp, ra, callee save fp regs
 * and fpc_csr) into a posix_sigcontext struct.
 *
 * On a siglongjmp, the sigjmp_buf is verified to be consistent, the appropriate
 * return value is dropped into the sigcontext, and a sigreturn system
 * call is performed to restore the signal state and restore the
 * callee saved register that were saved in the sigcontext by sigsetjmp.
 */

SETJMPFRM	=	32

NESTED(sigsetjmp, SETJMPFRM, zero)
	subu	sp,SETJMPFRM
	sw	ra,SETJMPFRM-4(sp)

	beq	a1,zero,1f		# if savemask == 0 don't save sigset_t
	sw	a0,SETJMPFRM-8(sp)	# save jmp_buf ptr
	sw	a1,SETJMPFRM-12(sp)	# save savemask
	li	a0,SIG_BLOCK
	move	a1,zero
	move	a2,sp
	jal	sigprocmask		# find currently blocked signals
	lw	v1,SETJMPFRM-8(sp)
	lw	a1,0(a2)		# save all 4 words of sigset_t
	sw	a1,+SJB_SIGMASK0*4(v1)
	lw	a1,4(a2)
	sw	a1,+SJB_SIGMASK1*4(v1)
	lw	a1,8(a2)
	sw	a1,+SJB_SIGMASK2*4(v1)
	lw	a1,12(a2)
	sw	a1,+SJB_SIGMASK3*4(v1)
	lw	ra,SETJMPFRM-4(sp)
	lw	a0,SETJMPFRM-8(sp)
	lw	a1,SETJMPFRM-12(sp)
	addu	sp,SETJMPFRM
	bltz	v0,botch
	j	2f
1:
	lw	ra,SETJMPFRM-4(sp)
	addu	sp,SETJMPFRM
2:
	sw	a1,+SJB_RESTOREMASK*4(a0)
	sw	gp,+SJB_GP*4(a0)
	sw	ra,+SJB_PC*4(a0)
	sw	sp,+SJB_SP*4(a0)
	sw	fp,+SJB_FP*4(a0)
	sw	s0,+SJB_S0*4(a0)
	sw	s1,+SJB_S1*4(a0)
	sw	s2,+SJB_S2*4(a0)
	sw	s3,+SJB_S3*4(a0)
	sw	s4,+SJB_S4*4(a0)
	sw	s5,+SJB_S5*4(a0)
	sw	s6,+SJB_S6*4(a0)
	sw	s7,+SJB_S7*4(a0)
	cfc1	v0,fpc_csr
	sw	v0,+SJB_FPC_CSR*4(a0)
	s.d	$f20,+SJB_F20*4(a0)
	s.d	$f22,+SJB_F22*4(a0)
	s.d	$f24,+SJB_F24*4(a0)
	s.d	$f26,+SJB_F26*4(a0)
	s.d	$f28,+SJB_F28*4(a0)
	s.d	$f30,+SJB_F30*4(a0)
	li	v0,+SJBMAGIC
	sw	v0,+SJB_MAGIC*4(a0)
	move	v0,zero
	j	ra
	END(sigsetjmp)


LEAF(siglongjmp)
	lw	s0,+SJB_SP*4(a0)
	bgtu	sp,s0,botch		# sigjmp_buf no longer on stack
	lw	v0,+SJB_MAGIC*4(a0)
	bne	v0,+SJBMAGIC,botch	# protect the naive
	bne	a1,zero,1f 		# according to the C standard
	li	v0,1			#   return 1 if val == 0
	sw	v0,+SJB_V0*4(a0)	# let posix_sigreturn set v0
	b	2f
1:
	sw	a1,+SJB_V0*4(a0)	# let posix_sigreturn set v0
	/*
	 * sigreturn will restore signal state and all callee saved
	 * registers from sigcontext and return to next instruction
	 * after setjmp call, the C calling sequence will then restore
	 * the caller saved registers
	 */
2:
	li	v0,SYS_sigreturn	# sigreturn(&sigcontext)
	syscall

botch:
	jal	_siglongjmperr
	jal	abort
	END(siglongjmp)
