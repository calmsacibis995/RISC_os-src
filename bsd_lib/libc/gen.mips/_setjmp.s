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
/* $Header: _setjmp.s,v 1.1.2.2 90/05/07 20:28:50 wje Exp $ */


/*
 * C library -- _setjmp, _longjmp
 *
 *	_longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	_setjmp(a)
 * by restoring registers from the stack,
 * The previous signal state is NOT restored.
 */

#include <mips/regdef.h>
#include <mips/asm.h>
#include <setjmp.h>
#include <mips/fpu.h>

LEAF(_setjmp)
	sw	ra,4*JB_PC(a0)
	sw	sp,4*JB_SP(a0)
	sw	fp,4*JB_FP(a0)
	sw	s0,4*JB_S0(a0)
	sw	s1,4*JB_S1(a0)
	sw	s2,4*JB_S2(a0)
	sw	s3,4*JB_S3(a0)
	sw	s4,4*JB_S4(a0)
	sw	s5,4*JB_S5(a0)
	sw	s6,4*JB_S6(a0)
	sw	s7,4*JB_S7(a0)
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
.end _setjmp

/*
 * _longjmp(jmp_buf, retval)
 */
LEAF(_longjmp)
	/*
	 * NOTE: SVID says that 0 is not a valid return value. There are
	 * commands that depend on this, so don't change it.
	 */
	lw	v0,+JB_MAGIC*4(a0)
	bne	v0,+JBMAGIC,botch	# protect the naive
	lw	ra,4*JB_PC(a0)
	lw	sp,4*JB_SP(a0)
	lw	fp,4*JB_FP(a0)
	lw	s0,4*JB_S0(a0)
	lw	s1,4*JB_S1(a0)
	lw	s2,4*JB_S2(a0)
	lw	s3,4*JB_S3(a0)
	lw	s4,4*JB_S4(a0)
	lw	s5,4*JB_S5(a0)
	lw	s6,4*JB_S6(a0)
	lw	s7,4*JB_S7(a0)
	l.d	$f20,+JB_F20*4(a0)
	l.d	$f22,+JB_F22*4(a0)
	l.d	$f24,+JB_F24*4(a0)
	l.d	$f26,+JB_F26*4(a0)
	l.d	$f28,+JB_F28*4(a0)
	l.d	$f30,+JB_F30*4(a0)
	lw	v0,+JB_FPC_CSR*4(a0)
	ctc1	v0,fpc_csr
	move	v0,a1			/* return retval */
	j	ra
botch:
	jal	longjmperror
	jal	abort
.end _longjmp
