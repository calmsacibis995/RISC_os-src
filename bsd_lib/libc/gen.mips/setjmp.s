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
/* $Header: setjmp.s,v 1.1.2.2 90/05/07 20:32:26 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <setjmp.h>
#include <syscall.h>
#include <mips/fpu.h>

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * previous signal mask, and doing a return.
 *
 * NOTE: THIS MUST MATCH UP WITH SIGCONTEXT STRUCTURE, be sure constants
 * in setjmp.h and signal.h are consistent!
 * 
 * Whats happening here: setjmp assumes that all process state except
 * the callee saved registers and the gp has been preserved by the
 * C calling sequence; therefore, setjmp only saves the signal state
 * (sigmask and the signal flag), and the state that must be preserved
 * by the callee (callee saved regs, gp, sp, fp, ra, callee save fp regs
 * and fpc_csr)  into a sigcontext struct.
 *
 * On a longjmp, the jmp_buf is verified to be consistent, the appropriate
 * return value is dropped into the sigcontext, and a sigreturn system
 * call is performed to restore the signal state and restore the
 * callee saved register that were saved in the sigcontext by setjmp.
 */

SETJMPFRM	=	32

NESTED(setjmp, SETJMPFRM, zero)
	subu	sp,SETJMPFRM
	sw	ra,SETJMPFRM-4(sp)
	sw	a0,SETJMPFRM-8(sp)	# save jmp_buf ptr
	move	a0,zero
	jal	sigblock		# find current sigmask
	lw	v1,SETJMPFRM-8(sp)
	sw	v0,+JB_SIGMASK*4(v1)
	move	a0,zero
	move	a1,v1
	addu	a1,+JB_ONSIGSTK*4
	jal	sigstack
	lw	a0,SETJMPFRM-8(sp)
	lw	ra,SETJMPFRM-4(sp)
	addu	sp,SETJMPFRM
	bltz	v0,botch
	sw	ra,+JB_PC*4(a0)
	sw	gp,+JB_GP*4(a0)
	sw	sp,+JB_SP*4(a0)
	sw	fp,+JB_FP*4(a0)
	sw	s0,+JB_S0*4(a0)
	sw	s1,+JB_S1*4(a0)
	sw	s2,+JB_S2*4(a0)
	sw	s3,+JB_S3*4(a0)
	sw	s4,+JB_S4*4(a0)
	sw	s5,+JB_S5*4(a0)
	sw	s6,+JB_S6*4(a0)
	sw	s7,+JB_S7*4(a0)
	cfc1	v0,fpc_csr
	sw	v0,+JB_FPC_CSR*4(a0)
	s.d	$f20,+JB_F20*4(a0)
	s.d	$f22,+JB_F22*4(a0)
	s.d	$f24,+JB_F24*4(a0)
	s.d	$f26,+JB_F26*4(a0)
	s.d	$f28,+JB_F28*4(a0)
	s.d	$f30,+JB_F30*4(a0)
	li	v0,+JBMAGIC
	sw	v0,+JB_MAGIC*4(a0)
	move	v0,zero
	j	ra
.end setjmp

/*
 * NOTE: SVID says that longjmp(x, 0) should not cause 0 to be returned.
 * There are programs that depend on longjmp(x, 0) returning 0, so
 * don't change it.
 */

LEAF(longjmp)
	lw	s0,+JB_SP*4(a0)
	bgtu	sp,s0,botch		# jmp_buf no longer on stack
	lw	v0,+JB_MAGIC*4(a0)
	bne	v0,+JBMAGIC,botch	# protect the naive
	sw	a1,+JB_V0*4(a0)		# let sigreturn set v0
	/*
	 * sigreturn will restore signal state and all callee saved
	 * registers from sigcontext and return to next instruction
	 * after setjmp call, the C calling sequence will then restore
	 * the caller saved registers
	 */
	li	v0,SYS_sigreturn	# sigreturn(&sigcontext)
	syscall

botch:
	jal	longjmperror
	jal	abort
.end longjmp
