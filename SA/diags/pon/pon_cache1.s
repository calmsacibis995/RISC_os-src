#ident "$Header: pon_cache1.s,v 1.11.1.1 90/07/18 14:29:40 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	Verifies that KSEG0 is cached, KSEG1 is uncached, caches misses read
 *	memory and drop a new entry in the cache, cache hits do not read
 *	memory and retain the entry in the cache, writes write-through to
 *	memory, and finally, that the valid bit logic is basically correct.
 *
 *	NOTE: Expects SP to contain the machine type ID.
 *	      This routine expects to be executed before a stack is
 *	      established.  Therefore, it used the S registers freely.
 *
 *	RETURNS: A 0 if the memory has passed the test.
 */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "machine/cp0.h"
#include "pon.h"

#define	OFFSET		1024
#define	KSEG0		+(K0BASE+OFFSET)
#define	KSEG1		+(K1BASE+OFFSET)

		.extern	init_cache
		.extern	success
		.extern	failure
		.extern	skipped

		.text

LEAF(Pon_Cache1)

		move	s0,ra			# save our return address
#ifndef	R3030
		li	a0,PON_CACHE1		# just begun
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

		jal	GetDepend

		and	v0,PON_FAULT_ICACHE
		beq	v0,zero,run

		la	a0,skipped
		jal	pon_puts

		b	norun
run:
		jal	init_cache

		li	v0,KSEG1		# initialize parity in block
		sw	zero,0(v0)		# memory == 0's
		sw	zero,4(v0)
		sw	zero,8(v0)
		sw	zero,12(v0)
		li	v0,-1
		sw	v0,KSEG0		# write-through 1's and drop-in
		lw	v1,KSEG1		# check if it made it to mem
		bne	v0,v1,1f		# nope

		sw	zero,KSEG1		# write behind cache
		lw	v1,KSEG0		# check for NO cache drop-in
		bne	v0,v1,1f		# nope

		.set noreorder
		mfc0	a2,C0_SR		# save current SR
		nop
		.set reorder
		or	a1,a2,SR_ISC		# isolate cache
		.set noreorder
		mtc0	a1,C0_SR
		.set reorder

		sb	zero,KSEG0		# flush line - works w/R3000
		nop
		.set noreorder
		mtc0	a2,C0_SR
		.set reorder

		lw	v1,KSEG0		# cause a read drop-in
		bne	v1,zero,1f		# Fubar!

		sw	v0,KSEG1		# write behind cache
		lw	v1,KSEG1		# check kseg1 uncached
		bne	v1,v0,1f		# nope

		lw	v1,KSEG0		# verify hit after rd drop-in
		bne	v1,zero,1f

		lw	v1,KSEG0		# verify hit after hit
		bne	v1,zero,1f

 # Successful!
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s0

 # Failed!
1:
		la	a0,failure
		jal	pon_puts

		li	a0,PON_CACHE1
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_CACHE1
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030
norun:
		li	a0,PON_FAULT_ICACHE
		jal	SetDepend

		li	v0,1
		j	s0

END(Pon_Cache1)


LEAF(size_cache)

#ifndef	R3030
		.set noreorder
		mfc0	t9,C0_SR		# save current sr
		nop
		.set reorder
		or	v0,t9,SR_ISC		# isolate cache
		.set noreorder
		mtc0	v0,C0_SR
		.set reorder

		/*
		 * Clear cache size boundries to known state.
		 */
		li	v0,MINCACHE
1:
		sw	zero,K0BASE(v0)
		sll	v0,1
		ble	v0,MAXCACHE,1b

		li	v0,-1
		sw	v0,K0BASE(zero)		# store marker in cache
		li	v0,MINCACHE		# MIN cache size
		b	2f
1:
		sll	v0,1			# cache size * 2
		bgt	v0,MAXCACHE,3f		# cache too big
2:
		lw	v1,K0BASE(v0)		# Look for marker
		beq	v1,zero,1b		# not found

		.set noreorder
		mtc0	t9,C0_SR		# restore sr
		.set reorder
		j	ra			# v0 has cachesize
3:
		.set noreorder
		mtc0	t9,C0_SR		# restore sr
		.set reorder
		li	v0,-1			# indicate error
#else	!R3030
		li	v0,32*1024
#endif	!R3030
		j	ra			# and return

END(size_cache)

		.data

begintest:
#ifndef	R3030
		.asciiz "Cache Test #1..."
#else	!R3030
		.asciiz "KSeg0/KSeg1 Cache Test..."
#endif	!R3030
