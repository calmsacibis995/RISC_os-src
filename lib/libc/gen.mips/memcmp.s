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
/* $Header: memcmp.s,v 1.3.2.2 90/05/10 01:25:38 wje Exp $ */


/* memcmp(s1, s2, n) */

#include "regdef.h"
#include "asm.h"

/*
 * memcmp(src, dst, bcount)
 */
#define	NBPW	4

LEAF(memcmp)
	and	t2,a2,(NBPW-1)
	ble	a2,zero,cmpdone
	subu	a3,a2,t2
	beq	a2,t2,bytecmp
	addu	a3,a0
1:	ulw	v0,0(a0)
	ulw	v1,0(a1)
	addu	a0,NBPW
	bne	v0,v1,2f
	addu	a1,NBPW
	bne	a0,a3,1b
	move	a2,t2
	ble	t2,zero,cmpdone

/*
 * brute force byte cmp loop
 */
bytecmp:
	addu	a3,a2,a0			# src1 endpoint; BDSLOT
1:	lbu	v0,0(a0)
	lbu	v1,0(a1)
	addu	a0,1
	addu	a1,1
	bne	v0,v1,cmpne
	bne	a0,a3,1b
cmpdone:
	move	v0,zero	
	j	ra


/*
 * This is one case where a big-endian machine is a win
 */
#ifdef MIPSEB
2:	sltu	v0,v1,v0
	sll	v0,1
	subu	v0,1
	j	ra
#endif
#ifdef MIPSEL
	.set noreorder
2:	subu	t0,v0,v1
	xor	t0,v0
	xor	t0,v1
	sll	t1,t0,23
	bne	t1,$0,3f
	sll	t1,t0,15
	bne	t1,$0,3f
	sll	t1,t0,7
	bne	t1,$0,3f
	sltu	v0,v1,v0
	sll	v0,1
	j	ra
	subu	v0,1
3:	j	ra
	move	v0,t1
	.set	reorder	
#endif
cmpne:	subu	v0,v0,v1
	j	ra

.end memcmp
