#ident "$Header: swapiso.s,v 1.5 90/02/28 21:38:54 chungc Exp $"
/* $Copyright$ */

/* ------------------------------------------------------------------ */
/* | Copyright Unpublished, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "machine/cp0.h"
#include "machine/cpu_board.h"

#ifndef R3030
#define	CACHE_LIMIT	0x80010000	/* 64k */
#else
#define	CACHE_LIMIT	0x80008000	/* 32K */
#endif

		.text
LEAF(swapisolate)
	.set	noreorder
	mfc0	a0,C0_SR		# save SR


	#
	# copy data cache to memory at 3 meg
	#
	li	v1,0xa0300000		# memory location for storage
	li	t1,0x80000000		# cache address
	li	t2,CACHE_LIMIT		# termination address (64k)

3:
	beq	t2,t1,4f
	nop
	nop
	nop
 	mfc0	t0,C0_SR		# save current sr
 	or	v0,t0,SR_ISC		# isolate cache
 	mtc0	v0,C0_SR
	nop
	nop
lw	k0,0(t1)			# grab cache instruction
	nop
	nop
 	mtc0	t0,C0_SR		# unisolate cache
	nop
	nop
sw	k0,0(v1)			# write intruction to memory
	addu	t1,4
	addu	v1,4
	b	3b
	nop

4:
	li	t1,0x80000000		# cache address
	li	t2,CACHE_LIMIT		# termination address (64k)
 # memory pointer (v1) is already pointing at correct location
	nop
	nop
	or	v0,a0,SR_SWC		# swap caches
	mtc0	v0,C0_SR
	nop
	nop
    #
    # copy instruction cache to memory at 3 meg + 64k
    #

6:
	beq	t2,t1,5f
	nop
	nop
	nop
 	mfc0	t0,C0_SR		# save current sr
 	or	v0,t0,SR_ISC		# isolate cache
 	mtc0	v0,C0_SR
	nop
	nop
lw	k0,0(t1)			# grab cache instruction
	nop
	nop
 	mtc0	t0,C0_SR		# unisolate cache
	nop
	nop
sw	k0,0(v1)			# write intruction to memory
	addu	t1,4
	addu	v1,4
	b	6b
	nop

5:
	mtc0	a0,C0_SR		# restore SR
	.set	reorder
	j	ra
	END(swapisolate)
