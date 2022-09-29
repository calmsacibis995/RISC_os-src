#ident "$Header: pon_cache2.s,v 1.11.1.1 90/07/18 14:29:48 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	Verifies that I-cache gets load on instruction fetches, that the
 *	I-cache is utilized when valid, and that instructions can execute
 *	at the rate of 1 instruction per clock when I-cache is valid.
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
#include "machine/cpu_board.h"
#include "machine/cpu.h"

		.extern	init_cache
		.extern	success
		.extern	failure
		.extern	skipped

		.text

LEAF(Pon_Cache2)

		move	s4,ra			# save our return address
#ifndef	R3030
		li	a0,PON_CACHE2		# just begun
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
		.set noreorder
		mfc0	s3,C0_SR		# save the SR
		.set reorder
		li	s2,SR_BEV
		jal	init_cache

#ifndef	R3030
/* If running on a M180 copy the code to cached memory space and jump to it */

		bne	sp,BRDTYPE_M180,3f
#endif	!R3030
		jal	invalidate_cache
		li	a0,PON_MEMTESTED|K0BASE  # destination address
		la	a1,1f		# start address
		la	a2,2f		# end address
		jal	move_code
		j	a0

/*
 * Run with instructions cached, then swap caches and see if cache drop-in
 * occurred.
 */
#ifndef	R3030
3:
		la	v0,1f-K0SIZE
		j	v0			# run in cached mode
#endif	!R3030
1:
		.set	noreorder
		li	v0,2
		li	v1,3
		li	a0,4
		li	a1,5

		la	a2,2f
		j	a2			# run uncached
		nop
2:
		lw	v0,1b			# should be contents of icache
		lw	v1,1b+4
		lw	a0,1b+8
		lw	a1,1b+12
		nop
		nop
		.set	reorder

		li	a2,SR_ISC|SR_SWC|SR_BEV
		.set noreorder
		mtc0	a2,C0_SR		# swap and isolate caches
		.set reorder
		nop
		nop
#ifndef	R3030
		bne	sp,BRDTYPE_M180,10f
#endif	!R3030
		li	a2,PON_MEMTESTED|K0BASE  # cached address of instruction
#ifndef	R3030
		b	11f
10:
		la	a2,1b-K0SIZE		# cachable address of instruction
11:
#endif	!R3030
		lw	a3,0(a2)
		.set noreorder
		mfc0	s0,C0_SR
		nop
		.set reorder
		and	s0,SR_CM
		li	s1,1			# set error code just in case
		bne	s0,zero,error		# no drop into icache

		li	s1,2			# set error code just in case
		bne	a3,v0,error		# didn't match instruction

		lw	a3,4(a2)
		.set noreorder
		mfc0	s0,C0_SR
		nop
		.set reorder
		and	s0,SR_CM
		li	s1,3			# set error code just in case
		bne	s0,zero,error		# no drop into icache

		li	s1,4			# set error code just in case
		bne	a3,v1,error		# didn't match instruction

		lw	a3,8(a2)
		.set noreorder
		mfc0	s0,C0_SR
		nop
		.set reorder
		and	s0,SR_CM
		li	s1,5			# set error code just in case
		bne	s0,zero,error		# no drop into icache

		li	s1,6			# set error code just in case
		bne	a3,a0,error		# didn't match instruction

		lw	a3,12(a2)
		.set noreorder
		mfc0	s0,C0_SR
		nop
		.set reorder
		and	s0,SR_CM
		li	s1,7			# set error code just in case
		bne	s0,zero,error		# no drop into icache

		li	s1,8			# set error code just in case
		bne	a3,a1,error		# didn't match instruction


/*
 * Load up i cache with instructions, then swap back caches and see
 * if they get executed.
 */
#ifndef	R3030
		bne	sp,BRDTYPE_M180,8f
#endif	!R3030
		li	a2,PON_MEMTESTED|K0BASE  # destination address
#ifndef	R3030
		b	2f
8:
		la	a2,3f-K0SIZE
2:
#endif	!R3030
		.set	noreorder
		sw	v0,0(a2)		# store li v0,2
		sw	v1,4(a2)		# store li v1,3
		sw	a0,8(a2)		# store li a0,4
		sw	a1,12(a2)		# store li a1,5
		sb	zero,16(a2)		# flush line - works w/R3000
		sb	zero,20(a2)
		move	v0,zero
		move	v1,zero
		move	a0,zero
		move	a1,zero
		nop
		nop
		.set	reorder

		.set noreorder
		mtc0	s2,C0_SR		# unswap and un-isolate caches
		.set reorder
		nop				# but keep BEV set
		nop

#ifndef	R3030
		bne	sp,BRDTYPE_M180,7f
#endif	!R3030
		li	a0,PON_MEMTESTED|K1BASE  # destination address
		la	a1,3f		# start address
		la	a2,4f		# end address
		jal	move_code
		la	a0,PON_MEMTESTED|K0BASE
		j	a0
#ifndef	R3030
7:
		la	a2,3f-K0SIZE		# run cached
		j	a2
#endif	!R3030
3:
		.set	noreorder
		nop				# should be li v0,2 in cache
		nop				# should be li v1,3 in cache
		nop				# should be li a0,4 in cache
		nop				# should be li a1,5 in cache

		la	a2,4f			# run uncached
		j	a2
		nop
		.set	reorder
4:
		li	s1,9			# set error code just in case
		bne	v0,2,error

		li	s1,10			# set error code just in case
		bne	v1,3,error

		li	s1,11			# set error code just in case
		bne	a0,4,error

		li	s1,12			# set error code just in case
		bne	a1,5,error
6:

/*
 * Execute loop in cache and insure it runs at one instruction/cycle.
 */
#ifndef SABLE
#ifndef	R3030
		bne	sp,BRDTYPE_M180,9f
#endif	!R3030
		jal	invalidate_cache
		li	a0,PON_MEMTESTED|K0BASE  # destination address
		la	a1,5f		# start address
		la	a2,7f		# end address
		jal	move_code
		j	a0
#ifndef	R3030
9:
		la	a2,5f-K0SIZE		# run cached
		j	a2
#endif	!R3030
5:
		li	v0,2			# loop count

		.set	noreorder
6:
		mfc0	a3,C0_RAND		# starting count
		nop
		nop
		nop
		nop
		mfc0	s0,C0_RAND		# ending count
		nop
		nop
		.set	reorder

		subu	v0,1
		bne	v0,zero,6b

		la	a2,7f
		j	a2			# run uncached
7:
		srl	a3,TLBRAND_RANDSHIFT
		srl	s0,TLBRAND_RANDSHIFT
		ble	s0,a3,8f

		subu	a3,s0
		and	a3,NTLBENTRIES-1
		subu	a3,NWIREDENTRIES
		b	9f
8:
		subu	a3,s0
9:
		li	s1,13			# set error code just in case
		bne	a3,5,error

		.set noreorder
		mtc0	s3,C0_SR		# restore SR
		.set reorder
#endif

 # Successful!
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s4

 # Failed!
error:
		.set noreorder
		mtc0	s3,C0_SR		# restore SR
		.set reorder
		la	a0,failure
		jal	pon_puts

		li	a0,PON_CACHE2
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_CACHE2
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030
norun:
		li	a0,PON_FAULT_ICACHE
		jal	SetDepend

		li	v0,1
		j	s4

END(Pon_Cache2)

		.data

begintest:
#ifndef	R3030
		.asciiz "Cache Test #2..."
#else	!R3030
		.asciiz "Instruction Cache Functionality Test..."
#endif	!R3030
