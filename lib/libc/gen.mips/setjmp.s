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
/* $Header: setjmp.s,v 1.9.2.2 90/05/10 01:26:34 wje Exp $ */

/*
 * C library -- setjmp, longjmp
 *
 *	longjmp(a,v)
 * will generate a "return(v)" from
 * the last call to
 *	setjmp(a)
 * by restoring registers from the stack,
 * The previous signal state is NOT restored.
 */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <setjmp.h>
#include <sys/fpu.h>

LEAF(setjmp)
	sw	ra,JB_PC*4(a0)
	sw	sp,JB_SP*4(a0)
	sw	fp,JB_FP*4(a0)
	sw	s0,JB_S0*4(a0)
	sw	s1,JB_S1*4(a0)
	sw	s2,JB_S2*4(a0)
	sw	s3,JB_S3*4(a0)
	sw	s4,JB_S4*4(a0)
	sw	s5,JB_S5*4(a0)
	sw	s6,JB_S6*4(a0)
	sw	s7,JB_S7*4(a0)
	cfc1	v0,fpc_csr
	sw	v0,+JB_FPC_CSR*4(a0)
	s.d	$f20,+JB_F20*4(a0)
	s.d	$f22,+JB_F22*4(a0)
	s.d	$f24,+JB_F24*4(a0)
	s.d	$f26,+JB_F26*4(a0)
	s.d	$f28,+JB_F28*4(a0)
	s.d	$f30,+JB_F30*4(a0)
	move	v0,zero
	j	ra
END()

/*
 * longjmp(jmp_buf, retval)
 */
LEAF(longjmp)
	lw	ra,JB_PC*4(a0)
	lw	sp,JB_SP*4(a0)
	lw	fp,JB_FP*4(a0)
	lw	s0,JB_S0*4(a0)
	lw	s1,JB_S1*4(a0)
	lw	s2,JB_S2*4(a0)
	lw	s3,JB_S3*4(a0)
	lw	s4,JB_S4*4(a0)
	lw	s5,JB_S5*4(a0)
	lw	s6,JB_S6*4(a0)
	lw	s7,JB_S7*4(a0)
	l.d	$f20,+JB_F20*4(a0)
	l.d	$f22,+JB_F22*4(a0)
	l.d	$f24,+JB_F24*4(a0)
	l.d	$f26,+JB_F26*4(a0)
	l.d	$f28,+JB_F28*4(a0)
	l.d	$f30,+JB_F30*4(a0)
	lw	v0,+JB_FPC_CSR*4(a0)
	ctc1	v0,fpc_csr
	move	v0,a1			/* return retval */
	bnez	v0,1f			/* unless retval==0 */
	li	v0,1			/* in which case return 1 */
1:	j	ra
END()
