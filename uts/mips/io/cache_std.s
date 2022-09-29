/* Copyright (C) 1989, MIPS Computer Systems */

#ident	"$Header: cache_std.s,v 1.5.1.2.1.2 90/07/11 18:29:32 hawkes Exp $"

#include "../ml/assym.s"

#define PGSHIFT		BPTSHFT

/*
 * clean_cache(addr, len)
 *
 * invalidate all caches for range of addr to addr+len-1
 * MUST NOT DESTROY a0 and a1
 */

CLEANFRM=	(2*4)+4		# 2 args, ra
NESTED(clean_cache, CLEANFRM, zero)
	subu	sp,CLEANFRM
	sw	ra,CLEANFRM-4(sp)
	# NOTE don't need to save args because the clean routines
	# both PROMISE not to destroy them

	jal	clean_icache		# clean the addrs in the icache

	jal	clean_dcache		# clean the addrs in the dcache

	lw	ra,CLEANFRM-4(sp)
	addu	sp,CLEANFRM
	j	ra
	END(clean_cache)



.globl	dcache_errcnt
.lcomm	dcache_errcnt,4	# cache clean argument error count
.globl	icache_errcnt
.lcomm	icache_errcnt,4	# cache clean argument error count

/*
 * clean_icache(addr, len)
 * Invalidate cache for range of addr to addr+len-1.
 * The address should be a kseg0 address.
 * It is essential that this routine not cause any exceptions
 * since they won't be handled well with the cache isolated
 *
 * To limit the time that the system has all interrupts disabled,
 * the invalidation is done in "chunks" and the CPU is returned
 * to the entry state with cache operational and the status
 * register restored (thus, perhpaps, interrupts enabled) between chunks.
 * 
 * MUST NOT DESTROY a0 and a1
 */

#define	I_MAXBLK	4096	/* max bytes to clean w/out interrupts */

LEAF(clean_icache)
	.set noreorder

	mfc0	t3,C0_SR		# save SR
	beq	a1,zero,6f		# if size==0, return without action
	 lw	t1,icache_size
	bltz	a1,80f			# negative size, bad argument
	 addu	t4,a1,64-1		# round req_len up to multiple of 64
	and	t4,-64			# so can use equality test in loop
	sltu	t0,t1,t4		# is cache < req_length
	bnez	t0,10f			# if cache is smaller than req_len
	 li	t6,K0BASE		# to range check addr
	move	t1,t4			# clean only cache size
10:
	sltu	t0,a0,t6		# is base addr < kseg0 base ?
	bnez	t0,80f  		# kuseg addr, bad address
1:	 addu	t1,a0			# end address

	li	t7,K1BASE
	sltu	t7,t7,t1		# is K1BASE < ending address
	bnez	t7,80f			# yes, bad range requested

	move	t0,a0			# local copy for base addr
	addu	t6,a0,I_MAXBLK		# initial intr disable target addr
	la	t2,55f

2:
	/*
	 * Invariants:
	 * t0 -- the next address to be invalidated
	 * t1 -- the final limit (beyond) invalidation address
	 * t2 -- cached text addr at end of loop
 	 * t3 -- saved Status Register
	 * t4 -- free
	 * t5 -- free
	 * t6 -- t0 + invalidation-chunk-size
	 * t7 -- free
	 *
	 */

	mtc0	zero,C0_SR		# disable interrupts
	nop

	bltu	t6,t1,3f		
	 nop
	move	t6,t1			# final end closer than current loop max
3:
	/*
	* Invariant
	* t6 --  contains min(final_end_addr, loop_max_end_addr)
	*	That is, the current intermediate address to signal
	*	temporary exit from the invalidation loop or
	*	the final address to exit the loop.
	*/

	/* CHIP BUG (R3000, R2000A) dodge
	* We don't want the write buffer to be full when we do
	* a store in isolated mode.
	* We can't tell that, but we can see if it is empty.
	* This is the important part of "wbflush".
	* there must be 4 cycles without a store
	* before this test is started.
	* The code preceeding this takes more than 4 cycles.
	*/
99:
	bc0f	99b			# write buffer is not empty if false
	 nop
	la	t5,90f			# get kseg0 code address
	or	t5,K1BASE		# convert to kseg1 address
	j	t5			# execute from uncached addrs
	 li	t7,SR_ISC|SR_SWC	# disable intr, isolate and swap caches

	/*
	 * flush text cache
	 */
90:	mtc0	t7,C0_SR		# this instr must execute uncached
	la	t4,4f			# load kseg0 (cached) addr
	j	t4			# execute loop in cached addrs
	 nop

4:	sb	zero,0(t0)
	sb	zero,4(t0)
	sb	zero,8(t0)
	sb	zero,12(t0)
	sb	zero,16(t0)
	sb	zero,20(t0)
	sb	zero,24(t0)
	sb	zero,28(t0)
	sb	zero,32(t0)
	sb	zero,36(t0)
	sb	zero,40(t0)
	sb	zero,44(t0)
	sb	zero,48(t0)
	sb	zero,52(t0)
	sb	zero,56(t0)
	addu	t0,64
	bne	t0,t6,4b
	 sb	zero,-4(t0)

	la	t5,5f
	or	t5,K1BASE
	j	t5			# run uncached
	 nop

5:
	mtc0	zero,C0_SR		# unswap and unisolate (intr disabled)
	nop
	mtc0	t3,C0_SR		# restore entry SR contents
	nop

	j	t2			# jump to 55: in cached space
	 nop
55:
	bne	t6,t1,2b		# need to go another loop if not done
	 addu	t6,t0,I_MAXBLK		# new loop max ending address
6:
	j	ra
	 nop				# will be un-isolated on jr target

	/* error in arguments
	 * we get here if there is some problem with the arguments.
	 * For a debug system, this should panic.
	 * For a production system, the robust thing to do is
	 * clean the entire cache.
	 * Fill the local start, end, and count and branch to the 
	 * location where the work gets done.
	 */
80:
	lw	t1,icache_size
	 lw	t4,icache_errcnt
	  li	t0,K0BASE
	addu	t1,t0
	addiu	t4,1
	sw	t4,icache_errcnt
	j	2b
	 nop
	.set	reorder
	END(clean_icache)

/*
 * clean_dcache(addr, len)
 * Invalidate cache for range of addr to addr+len-1.
 * The address should be a kseg0 address.
 * It is essential that this routine not cause any exceptions
 * since they won't be handled well with the cache isolated
 *
 * To limit the time that the system has all interrupts disabled,
 * the invalidation is done in "chunks" and the CPU is returned
 * to the entry state with cache operational and the status
 * register restored (thus, perhpaps, interrupts enabled) between chunks.
 *
 * MUST NOT DESTROY a0 and a1
 */

#define	D_MAXBLK	4096	/* max bytes to clean w/out interrupts */

LEAF(clean_dcache)
	.set noreorder

	mfc0	t3,C0_SR		# save SR
	beq	a1,zero,6f		# if size==0, return without action
	 lw	t1,dcache_size
	bltz	a1,80f			# negative size, bad argument
	 addu	t4,a1,64-1		# round req_len up to multiple of 64
	and	t7,a0,3			# need to add byte offset of addr
	addu	t4,t7			#      to len of flush
	and	t4,-64			# so can use equality test in loop
	sltu	t0,t1,t4		# is cache < req_length
	bnez	t0,10f			# if cache is smaller than req_len
	 li	t6,K0BASE		# to range check addr
	move	t1,t4			# clean only cache size
10:
	sltu	t0,a0,t6		# is base addr < kseg0 base ?
	bnez	t0,80f  		# kuseg addr, bad address
1:	 addu	t1,a0			# end address

	li	t7,K1BASE
	sltu	t7,t7,t1		# is K1BASE < ending address
	bnez	t7,80f			# yes, bad range requested

	move	t0,a0			# local copy for base addr
	addu	t6,a0,D_MAXBLK		# initial intr disable target addr

2:
	/*
	 * Invariants:
	 * t0 -- the next address to be invalidated
	 * t1 -- the final limit (beyond) invalidation address
 	 * t3 -- saved Status Register
	 * t6 -- t0 + invalidation-chunk-size
	 */

	mtc0	zero,C0_SR		# disable interrupts
	nop

	bltu	t6,t1,3f		
	 nop
	move	t6,t1			# final end closer than current loop max
3:
	/*
	* Invariant
	* t6 --  contains min(final_end_addr, loop_max_end_addr)
	*	That is, the current intermediate address to signal
	*	temporary exit from the invalidation loop or
	*	the final address to exit the loop.
	*/

	/* CHIP BUG (R3000, R2000A) dodge
	* We don't want the write buffer to be full when we do
	* a store in isolated mode.
	* We can't tell that, but we can see if it is empty.
	* This is the important part of "wbflush".
	* there must be 4 cycles without a store
	* before this test is started.
	* The code preceeding this takes more than 4 cycles.
	*/
99:
	bc0f	99b			# write buffer is not empty if false
	 li	t7,SR_ISC		# disable interrupts, isolate data cache
	mtc0	t7,C0_SR
	nop				# having this stopped system hangs

4:	sb	zero,0(t0)
	sb	zero,4(t0)
	sb	zero,8(t0)
	sb	zero,12(t0)
	sb	zero,16(t0)
	sb	zero,20(t0)
	sb	zero,24(t0)
	sb	zero,28(t0)
	sb	zero,32(t0)
	sb	zero,36(t0)
	sb	zero,40(t0)
	sb	zero,44(t0)
	sb	zero,48(t0)
	sb	zero,52(t0)
	sb	zero,56(t0)
	addu	t0,64
	bne	t0,t6,4b
	 sb	zero,-4(t0)


5:
	mtc0	zero,C0_SR		# un-isolate
	nop
	mtc0	t3,C0_SR		# restore entry SR contents
	nop

	bne	t6,t1,2b		# need to go another loop if not done
	 addu	t6,t0,D_MAXBLK		# new loop max ending address
6:
	j	ra
	 nop				# will be un-isolated on jr target

	/* error in arguments
	 * we get here if there is some problem with the arguments.
	 * For a debug system, this should panic.
	 * For a production system, the robust thing to do is
	 * clean the entire cache.
	 * Fill the local start, end, and count and branch to the 
	 * location where the work gets done.
	 */
80:
	lw	t1,dcache_size
	 lw	t4,dcache_errcnt
	  li	t0,K0BASE
	addu	t1,t0
	addiu	t4,1
	sw	t4,dcache_errcnt
	j	2b
	 nop
	.set	reorder
	END(clean_dcache)

/*
 * flush_cache()
 *
 * invalidate entire cache for both instruction and data caches.
 */

FLUSHFRM=	4		# ra
NESTED(flush_cache, FLUSHFRM, zero)
	subu	sp,FLUSHFRM
	sw	ra,FLUSHFRM-4(sp)

	li	a0,K0BASE		# base address for clean
	lw	a1,icache_size		# clean the whole icache
	jal	clean_icache

	li	a0,K0BASE		# base address for clean
	lw	a1,dcache_size		# clean the whole dcache
	jal	clean_dcache

	lw	ra,FLUSHFRM-4(sp)
	addu	sp,FLUSHFRM
	j	ra

	END(flush_cache)

/*
 * page_iflush(addr)
 * flush one page of i cache, addr is assumed to be in kseg0
 */
PIFLUSHFRM=		4+4	# 1 arg, ra
NESTED(page_iflush, PIFLUSHFRM, zero)
	subu	sp,PIFLUSHFRM
	sw	ra,PIFLUSHFRM-4(sp)
					# address is already in a0
	li	a1,NBPP			# length

	jal	clean_icache

#ifdef CACHETRICKS
	/* NOTE
	 * This code was extracted from a routine that did the
	 * cache invalidation itself.  It has not been examined.
	 */
	lw	v0,icachemask
	srl	t1,a0,BPTSHFT
	and	t1,v0
	sll	t1,1			# cachecnt index
	lhu	t0,icachecnt(t1)
	addu	t0,1
	sh	t0,icachecnt(t1)
#endif CACHETRICKS

	lw	ra,PIFLUSHFRM-4(sp)
	addu	sp,PIFLUSHFRM
	j	ra
	
	END(page_iflush)

/*
 * page_dflush(addr)
 * flush one page of i cache, addr is assumed to be in kseg0
 */
PDFLUSHFRM=		4+4	# 1 arg, ra
NESTED(page_dflush, PDFLUSHFRM, zero)
	subu	sp,PDFLUSHFRM
	sw	ra,PDFLUSHFRM-4(sp)
					# address is already in a0
	li	a1,NBPP			# length

	jal	clean_dcache

#ifdef CACHETRICKS
	/* NOTE
	 * This code was extracted from a routine that did the
	 * cache invalidation itself.  It has not been examined.
	 */
	lw	v0,dcachemask
	srl	t1,a0,BPTSHFT
	and	t1,v0
	sll	t1,1			# cachecnt index
	lhu	t0,dcachecnt(t1)
	addu	t0,1
	sh	t0,dcachecnt(t1)
#endif CACHETRICKS

	lw	ra,PDFLUSHFRM-4(sp)
	addu	sp,PDFLUSHFRM
	j	ra

	END(page_dflush)
	
/*
 * Config_cache() -- determine sizes of i and d caches
 * Sizes stored in globals dcache_size and icache_size
 */
CONFIGFRM=	(4*4)+4+4		# 4 arg saves, ra, and a saved register
NESTED(config_cache, CONFIGFRM, zero)
	.set	noreorder
	subu	sp,CONFIGFRM
	sw	s0,CONFIGFRM-8(sp)	# save s0 on stack
	sw	ra,CONFIGFRM-4(sp)
	mfc0	s0,C0_SR		# save SR
	mtc0	zero,C0_SR		# disable interrupts
	la	v0,1f
	or	v0,K1BASE
	j	v0			# run uncached
	nop
	.set	reorder

1:	jal	size_cache
	sw	v0,dcache_size
	.set	noreorder
	nop
	nop
	li	v0,SR_SWC		# swap caches
	mtc0	v0,C0_SR
	nop
	nop
	.set	reorder
	jal	size_cache
	sw	v0,icache_size
	.set	noreorder
	nop
	nop
	mtc0	zero,C0_SR		# swap back caches
	nop
	nop
	.set	reorder
	la	t0,1f
	j	t0			# back to cached mode

	.set	noreorder
	nop				# BDSLOT
1:	mtc0	s0,C0_SR		# restore SR
	nop				# give the caches some slack
	lw	ra,CONFIGFRM-4(sp)
	lw	s0,CONFIGFRM-8(sp)	# restore old s0
	addu	sp,CONFIGFRM
	j	ra
	nop
	.set	reorder
	END(config_cache)

/*
 * size_cache()
 * Discover and return the size of the cache presently configured
 * as the data cache.  This is called, after the appropriate setup,
 * to size both the instruction and data caches.
 * To be specific, DO NOT change the swap-cache status bit.
 * This routine MUST be called with interrupts disabled.
 * 
 */
LEAF(size_cache)
	.set	noreorder

	/* CHIP BUG (R3000, R2000A) dodge
	* We don't want the write buffer to be full when we do
	* a store in isolated mode.
	* We can't tell that, but we can see if it is empty.
	* This is the important part of "wbflush".
	*/
99:
	nop; nop; nop; nop;		# guarantee 4 cycles before test
	bc0f	99b			# write buffer is not empty if false
	 nop
	mfc0	t0,C0_SR		# save current sr
	 nop				# "load" delay slot.
	or	v0,t0,SR_ISC		# isolate current data cache
	mtc0	v0,C0_SR
	.set	reorder
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

2:	lw	v1,K0BASE(v0)		# Look for marker
	bne	v1,zero,3f		# found marker
	sll	v0,1			# cache size * 2
	ble	v0,MAXCACHE,2b		# keep looking
	move	v0,zero			# must be no cache

	.set	noreorder
3:	mtc0	t0,C0_SR		# restore sr
	.set	reorder
	j	ra
	END(size_cache)


/*
 * invalidate_virt_dcache(addr, len)
 * invalidate virtual d-cache lines for range addr to addr+len-1
 *
 * this architecture uses a physical d-cache, so no virtual invalidate is
 * necessary
 */
LEAF(invalidate_virt_dcache)
	j	ra			# just exit
	END(invalidate_virt_dcache)


/*
 * invalidate_virt_icache(addr, len)
 * invalidate virtual i-cache lines for range addr to addr+len-1
 *
 * this architecture uses a physical i-cache, so no virtual invalidate is
 * necessary
 */
LEAF(invalidate_virt_icache)
	j	ra			# just exit
	END(invalidate_virt_icache)


/*
 * invalidate_virt_scache(addr, len)
 * invalidate virtual s-cache lines for range addr to addr+len-1
 *
 * this architecture doesn't have an s-cache, so no virtual invalidate is
 * necessary
 */
LEAF(invalidate_virt_scache)
	j	ra			# just exit
	END(invalidate_virt_scache)


/*
 * invalidate_scache(addr, len)
 * invalidate virtual and physical tags of s-cache lines for range addr to
 * addr+len-1
 *
 * this architecture doesn't have an s-cache, so no invalidate is necessary
 */
LEAF(invalidate_scache)
	j	ra			# just exit
	END(invalidate_scache)


/*
 * writeback_cache (addr, len)
 *
 * this architecture uses a write-through cache, so no writeback is necessary
 */
LEAF(writeback_cache)
	j	ra			# just exit
	END(writeback_cache)
