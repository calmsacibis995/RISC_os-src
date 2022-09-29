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
/* $Header: execute_br.s,v 1.4.2.3 90/05/10 01:24:14 wje Exp $ */

#include "regdef.h"
#include "asm.h"
#include "sys/cachectl.h"

/*
 * execute_branch is passed a branch instruction and returns a non-zero
 * value if the branch is taken and a zero value otherwise.  This routine
 * is only intended to be used for branch on co-processor condition
 * instructions that can't be emulated.
 *
 * The branch instruction is executed in data space with a modified offset.
 * The code executed in data space looks like this:
 *	branch --------
 *	nop	      |
 *	j	a0    |
 *	j	a1 <---
 * Where a0 gets loaded with is the address of this routine's text label to
 * handle non-taken branches and a1 with the label for taken branches.
 */

#define	FRAME_SIZE	8
#define	LOCAL_SIZE	0
#define	RA_OFFSET	FRAME_SIZE-LOCAL_SIZE-4*1

NESTED(execute_branch, FRAME_SIZE, ra)
	.mask	M_RA, -(LOCAL_SIZE+4)

	subu	sp,FRAME_SIZE
	sw	ra,RA_OFFSET(sp)

	srl	a0,16		# build the branch with a modified offset
	sll	a0,16
	or	a0,8>>2	
	
	la	a1,execute_area	# store the instructions in the execute_area
	sw	a0,0(a1)	# the "branch with modified offset"
	sw	zero,4(a1)	# the "nop"
	lw	a0,1f
	sw	a0,8(a1)	# the "jr a0"
	lw	a0,2f
	sw	a0,12(a1)	# the "jr a1"

	la	a0,execute_area	# flush the icache for the execute_area
	li	a1,32
	li	a2,ICACHE
	jal	cacheflush

	la	a0,taken	# prepare to jump to the execute area
	la	a1,nottaken
	la	a2,execute_area
	.set	noreorder
	j	a2
	nop
	nop
	nop
	nop
	nop
	.set	reorder
taken:
	li	v0,1
	lw	ra,RA_OFFSET(sp)
	addu	sp,FRAME_SIZE
	j	ra
nottaken:
	move	v0,zero
	lw	ra,RA_OFFSET(sp)
	addu	sp,FRAME_SIZE
	j	ra
	END(execute_branch)

1:	j	a0
2:	j	a1

	.data
execute_area:
	.word	0
	.word	0
	.word	0
	.word	0

	.word	0
	.word	0
	.word	0
	.word	0
