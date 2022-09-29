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
/* $Header: ldexp.s,v 1.3.2.2 90/05/10 01:25:21 wje Exp $ */

/*
 * double ldexp (value, exp)
 * double value;
 * int exp;
 *
 * Ldexp returns value*2**exp, if that result is in range.
 * If underflow occurs, it returns zero.  If overflow occurs,
 * it returns a value of appropriate sign and largest
 * possible magnitude.  In case of either overflow or underflow,
 * errno is set to ERANGE.  Note that errno is not modified if
 * no error occurs.
 */

#include <regdef.h>

/*.rdata*/.sdata
infinity:
	.word 0x7ff00000

.text
.globl ldexp
.ent ldexp
ldexp:
	.frame	sp, 0, ra
.set noreorder
	mfc1	t0, $f13
	mov.d	$f0, $f12
	sll	t1, t0, 1
	srl	t1, 21
	addu	t1, a2
	ble	t1, 0, 3f
	 slt	t2, t1, 2047
	beq	t2, 0, 2f
	 sll	a2, 20
	addu	t0, a2
	mtc1	t0, $f1
1:	j	ra
	 nop
2:	/* Return infinity with appropriate sign. */
	lwc1	$f1, infinity
	bge	t0, 0, 1b
	 mtc1	$0, $f0
	j	ra
	 neg.d	$f0
3:	/* Return denorm. */
	ble	t1, -52, 5f
	 mfc1	t5, $f13	# t4,t5: copy of unscaled number
	li	t2, 0x80000000
	sll	t5, 11		# clear exponent
	ble	t1, -31, 4f
	 or	t5, t2		# turn on hidden bit
	srl	t5, 11
	mfc1	t4, $f12
	subu	t1, 1		# now shift by amount of underflow
	sll	t3, t5, t1
	srl	t2, t4, t1
	negu	t1
	srl	t4, t1
	or	t4, t3
	bge	t2, 0, 9f
	 srl	t5, t1
	/* Round up */
	addu	t4, 1
	sltu	t6, t4, 1
	sll	t2, 1
	bne	t2, 0, 9f
	 addu	t5, t6
	and	t4, -2
9:	mtc1	t4, $f0
	bge	t0, 0, 1b
	 mtc1	t5, $f1
	j	ra
	 neg.d	$f0
4:	/* Return denorm with most significant word all zero. */
	mtc1	$0, $f1
	addu	t1, 20		# now shift by amount of underflow
	sll	t2, t5, t1
	negu	t1
	bge	t2, 0, 8f
	 srl	t4, t5, t1
	/* Round up */
	addu	t4, 1
	sltu	t6, t4, 1
	sll	t2, 1
	bne	t2, 0, 8f
	 mtc1	t6, $f1
	and	t4, -2
8:	bge	t0, 0, 1b
	 mtc1	t4, $f0
	j	ra
	 neg.d	$f0
5:	/* Return zero. */
	mtc1	$0, $f0
	bge	t0, 0, 1b
	 mtc1	$0, $f1
	j	ra
	 neg.d	$f0
.set reorder
.end ldexp
