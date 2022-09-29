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
/* $Header: strcmp.s,v 1.6.2.2 90/05/10 01:26:57 wje Exp $ */

/* This function is an assembly-code replacement for
   the libc function "strcmp."  */
/* Libc currently has a mips-specific C version that uses 7 instructions/byte.
   (It claims to use 6 cycles/byte, but is wrong!)
   This function uses an unrolled loop, which uses 5 instructions per byte.

   Under some circumstances more characters are read than are
   required for determining the collating order, but it
   never reads beyond the end of either string.

   There are one caveat to consider: this function is written
   in assembler code, and as such, cannot be merged
   using the U-code loader. */

/* Craig Hansen - 6-June-86 */

#include <asm.h>
#include <regdef.h>

	.text	

LEAF(strcmp)

	.set	noreorder
	lbu	t0,0(a0)
1:	lbu	t1,0(a1)
	beq	t0,0,2f
	addi	a0,2
	bne	t0,t1,3f
	lbu	t2,-1(a0)	# ok to load since -2(a0)!=0
	lbu	t1,1(a1)
	beq	t2,0,2f
	addi	a1,2
	beq	t2,t1,1b
	lbu	t0,0(a0)	# ok to load since -1(a0) != 0
	j	ra
	subu	v0,t2,t1	
2:	j	ra
	subu	v0,zero,t1
3:	j	ra
	subu	v0,t0,t1
	.end	strcmp
