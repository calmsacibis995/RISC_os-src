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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: cache_r6000.s,v 1.3.1.4.1.1.1.3 90/11/12 17:45:26 beacker Exp $ */

#include "../ml/assym.s"

#ifdef R6000_BUG_FLUSH_PAUSE
#define FLUSH_PAUSE_COUNT 10
#endif R6000_BUG_FLUSH_PAUSE

#ifndef R6000_BUG_FLUSH_FLAGS
LBSS(writeback_cache_inner_addr,4)
#endif !R6000_BUG_FLUSH_FLAGS
LBSS(invalidate_scache_inner_addr,4)
LBSS(flush_cache_inner_addr,4)

	.set	reorder
/*
 * clean_cache(addr, len)
 * flush cache for range of addr to addr+len-1
 */
LEAF(clean_cache)
	j	ra
	END(clean_cache)

/*
 * clean_icache(addr, len)
 * clean_dcache(addr, len)
 * Flush i- and d-cache for range of addr to addr+len-1, for a K0 addr.
 * Since the R6000 has virtual i- and d-caches, these routines do nothing.
 */
LEAF(clean_icache)
	j	ra
	END(clean_icache)

LEAF(clean_dcache)
	j	ra
	END(clean_dcache)



/*
 * invalidate_virt_dcache(addr, len)
 * invalidate virtual d-cache lines for range addr to addr+len-1
 */
LEAF(invalidate_virt_dcache)
	blez	a1,9f			# if length <=0, then exit immediately
	lw	t3,dcache_size
	.set	noreorder
	mfc0	t8,C0_SR		# disable interrupts, and
	li	t7,SR_MM_MODE		#  enable MemMgmt Mode
	mtc0	t7,C0_SR
	.set	reorder
	subu	t2,t3,1			# form mask from d-cache byte size
	and	t0,a0,t2		# and form effective beginning address
	sltu	t4,a1,t3		# if length >= d-cache size ...
	bne	t4,zero,1f
	move	a1,t3			#  ... then adjust length and address
	move	t0,zero			#      to invalidate entire d-cache
	
1:	add	t1,t0,a1		# ending address, plus 1
	sub	t1,16*R6000_DLINE_BSIZE	#  readjusted for loop control
	.set	noreorder
2:	inval	1+ 0*R6000_DLINE_BSIZE(t0)	# invalidate 16 lines at a time,
	inval	1+ 1*R6000_DLINE_BSIZE(t0)	#  trade off excess inval's
	inval	1+ 2*R6000_DLINE_BSIZE(t0)	#   against efficient looping
	inval	1+ 3*R6000_DLINE_BSIZE(t0)
	inval	1+ 4*R6000_DLINE_BSIZE(t0)
	inval	1+ 5*R6000_DLINE_BSIZE(t0)
	inval	1+ 6*R6000_DLINE_BSIZE(t0)
	inval	1+ 7*R6000_DLINE_BSIZE(t0)
	inval	1+ 8*R6000_DLINE_BSIZE(t0)
	inval	1+ 9*R6000_DLINE_BSIZE(t0)
	inval	1+10*R6000_DLINE_BSIZE(t0)
	inval	1+11*R6000_DLINE_BSIZE(t0)
	inval	1+12*R6000_DLINE_BSIZE(t0)
	inval	1+13*R6000_DLINE_BSIZE(t0)
	inval	1+14*R6000_DLINE_BSIZE(t0)
	inval	1+15*R6000_DLINE_BSIZE(t0)
	slt	t2,t0,t1		# t2 <-- keep going?
	bne	t2,zero,2b
	addi	t0,16*R6000_DLINE_BSIZE	# (BDSLOT)  increment invalidate addr
	
	mtc0	t8,C0_SR		# restore SR, give time to settle
9:	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(invalidate_virt_dcache)


/*
 * invalidate_virt_icache(addr, len)
 * invalidate virtual i-cache lines for range addr to addr+len-1
 */
LEAF(invalidate_virt_icache)
	blez	a1,9f			# if length <=0, then exit immediately
	lw	t3,icache_size
	.set	noreorder
	mfc0	t8,C0_SR		# (BDSLOT) save SR
	li	t7,SR_MM_MODE		# disable interrupts and enable MM Mode
	mtc0	t7,C0_SR		# (BDSLOT) actually reset SR here
	.set	reorder
	subu	t2,t3,1			# form mask from i-cache byte size
	and	t0,a0,t2		# and form effective beginning address
	sltu	t4,a1,t3		# if length >= i-cache size ...
	bne	t4,zero,1f
	move	a1,t3			#  ... then adjust length and address
	move	t0,zero			#      to invalidate entire i-cache
	
1:	add	t1,t0,a1		# ending address, plus 1
	sub	t1,16*R6000_ILINE_BSIZE	#  readjusted for loop control
	.set	noreorder
2:	inval	 0*R6000_ILINE_BSIZE(t0) 	# invalidate 16 lines at a time,
	inval	 1*R6000_ILINE_BSIZE(t0)	#  trade off excess inval's
	inval	 2*R6000_ILINE_BSIZE(t0)	#  against efficient looping
	inval	 3*R6000_ILINE_BSIZE(t0)
	inval	 4*R6000_ILINE_BSIZE(t0)
	inval	 5*R6000_ILINE_BSIZE(t0)
	inval	 6*R6000_ILINE_BSIZE(t0)
	inval	 7*R6000_ILINE_BSIZE(t0)
	inval	 8*R6000_ILINE_BSIZE(t0)
	inval	 9*R6000_ILINE_BSIZE(t0)
	inval	10*R6000_ILINE_BSIZE(t0)
	inval	11*R6000_ILINE_BSIZE(t0)
	inval	12*R6000_ILINE_BSIZE(t0)
	inval	13*R6000_ILINE_BSIZE(t0)
	inval	14*R6000_ILINE_BSIZE(t0)
	inval	15*R6000_ILINE_BSIZE(t0)
	slt	t2,t0,t1		# t2 <-- keep going?
	bne	t2,zero,2b
	addi	t0,16*R6000_ILINE_BSIZE	# (BDSLOT)  increment invalidate addr

	mtc0	t8,C0_SR		# restore SR, give time to settle
9:	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(invalidate_virt_icache)


/*
 * invalidate_virt_scache (addr, len)
 *
 * Invalidate s-cache lines (which invalidates the only virtual tags, not the
 * physical tags or data) for the K0 physical address range addr to addr+len.
 * Presume that we do not cross a page boundary.
 * For efficiency we examine the pertinent range of physical tags for a match
 * on the pfn, and invalidate only those data lines which match.
 */
LEAF(invalidate_virt_scache)
	/*
	 *  A bug catcher:  verify don't cross a page boundary
	 */
	sll	t0,a0,3			# strip off upper three bits of addr
	srl	t0,3
	add	t1,t0,a1
	sub	t1,1			# last byte
	srl	t0,BPTSHFT		# phys page addr of first byte
	srl	t1,BPTSHFT		#  and last byte
	beq	t0,t1,1f		# are they the same?
	PANIC("invalidate_virt_scache(): crosses page boundary")
1:
	.set	noreorder
	/*
	 *  form the effective address of the physical tags for
	 *  that part of the data cache which might hold this page
	 *	t0  addr of first ptag for LCACHE
	 *	t1  addr of first cache line for INVALidate
	 *	t2  addr of last  cache line for INVALidate
	 *	t3  pfn from "addr"
	 */
	mfc0	t8,C0_SR		# save SR
	li	t7,SR_MM_MODE		# disable interrupts
	mtc0	t7,C0_SR		#  and enable MM Mode
	li	t0,R6000_SLINE_BSIZE	# a1 <-- max( a1, line_size )
	slt	t4,a1,t0
	beq	t4,zero,1f
	nop
	move	a1,t0
1:	sll	t3,a0,3			# shift off upper 3 bits of K0 vaddr
	srl	t1,t3,3			#   and shift back
	srl	t3,t1,BPTSHFT		#   shift again to form the pfn
	li	t4,0xdfffffff		# tentative INVAL address mask
	li	t7,0xf			# pfn mask
	and	t6,t3,t7		# low-order 4 bits of pfn will be 12:9
	bne	t6,t7,1f		#  except 0xf gets mapped to 0x7
	add	t2,t1,a1		# (BDSLOT) addr+1 of last line for INVAL
	li	t6,7
	li	t4,0xdffdffff		# correct INVAL address mask
1:	ori	t0,t6,0x1f0		# "ptags" bits will be bits 17:13
	sll	t0,9			#   and shift into proper position
	srl	t1,R6000_SLINE_SHIFT	# start to align INVAL addr
	andi	t6,t1,0x7f		# isolate cache line index
	sll	t6,2			#   and shift into proper position
	or	t0,t6			#   and merge to form complete addr
	sll	t1,R6000_SLINE_SHIFT	# align INVAL addr on line boundary
	subu	t2,R6000_SLINE_BSIZE	# set up ending addr for loop control
	and	t1,t4			# adjust INVAL address to avoid upper
	and	t2,t4			#  1/16 of s-cache, and convert to K0

2: 	lcache	t4,0(t0)		# side-0 ptag
	nop				# (LDSLOT-1)
	nop				# (LDSLOT-2)
   	lcache	t5,1(t0)		# side-1 ptag
	srl	t4,PTE_PNUMSHFT		# r-justify pfn for side-0
	bne	t4,t3,3f		# matches side-0?
	srl	t5,PTE_PNUMSHFT		# (BDSLOT) r-justify pfn for side-1
	inval	(t1)			# match!  invalidate side-0
	/* why would we match on both sides? well, play safe and ... */
3:	bne	t5,t3,4f		# matches side-1?
	addi	t0,4			# (BDSLOT) address of next ptag
	inval	1(t1)			# match!  invalidate side-1
4:	slt	t4,t1,t2		# all done?
	bne	t4,zero,2b		# loop back if not done with page
	addi	t1,R6000_SLINE_BSIZE	# (BDSLOT) address for next invalidate

	mtc0	t8,C0_SR		# restore SR, give time to settle
	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(invalidate_virt_scache)


/*
 * invalidate_scache (addr, len)
 *
 * Invalidate virtual and physical tags for the s-cache lines for the K0
 * physical address range addr to addr+len, and write-back dirty cache lines
 * to main memory.
 * Presume that we do not cross a page boundary.
 * For efficiency we examine the pertinent range of physical tags for a match
 * on the pfn, and invalidate only those data lines which match.
 */
LEAF(invalidate_scache)
#ifdef R6000_BUG_FLUSH_FLAGS
	/*
	 *  CPU bug:  the FLUSH instruction clears the line-dirty flag,
	 *  but not each per-word-dirty flag.  That means if after a FLUSH
	 *  the line is dirtied again, then the next FLUSH will rewrite the
	 *  same previously-dirty words to memory, possibly overwriting new
	 *  values.  The simplest workaround is to FLUSH the line back to
	 *  memory, and completely invalidate the line.  This is exactly what
	 *  is done by invalidate_scache().
	 */
XLEAF(writeback_cache)
#endif R6000_BUG_FLUSH_FLAGS
#ifdef DEBUG
	/*
	 *  A bug catcher:  verify K0/K1 address
	 */
	li	t0,0xc0000000		# high-order 2 bits of vaddr
	and	t1,a0,t0		# if vaddr begins with
	li	t2,0x80000000		#   binary 100 or 101 (K0 or K1)
	beq	t1,t2,1f		#    then we're ok, else exit 
	j	ra			# (do it this way so we can set a bp)
1:
#endif DEBUG
	/*
	 *  A bug catcher:  verify don't cross a page boundary
	 */
	sll	t0,a0,3			# strip off upper three bits of addr
	srl	t0,3
	add	t1,t0,a1
	sub	t1,1			# last byte
	srl	t0,BPTSHFT		# phys page addr of first byte
	srl	t1,BPTSHFT		#  and last byte
	beq	t0,t1,1f		# are they the same?
	PANIC("invalidate_scache(): crosses page boundary")
1:
	.set	noreorder
	/*
	 *  form the effective address of the physical tags for
	 *  that part of the data cache which might hold this page
	 *	t0  addr of first ptag for LCACHE
	 *	t1  addr of first cache line for INVALidate
	 *	t2  addr of last  cache line for INVALidate
	 *	t3  pfn from "addr"
	 */
	mfc0	t8,C0_SR		# save SR
	li	t7,SR_MM_MODE		# disable interrupts
	mtc0	t7,C0_SR		#  and enable MM Mode
	li	t0,R6000_SLINE_BSIZE	# a1 <-- max( a1, line_size )
	slt	t4,a1,t0
	beq	t4,zero,1f
	nop
	move	a1,t0
1:	move	t1,a0			# leave "addr" arg untouched for debug
	sll	t3,t1,3			# shift off K0/K1 prefix bits...
	srl	t3,BPTSHFT+3		#   ...and shift to get true pfn
	li	t4,0xdfffffff		# tentative mask for FLUSH/INVAL address
	li	t7,0xf			# pfn mask
	and	t6,t3,t7		# low-order 4 bits of pfn will be 12:9
	bne	t6,t7,1f		#  except 0xf gets mapped to 0x7
	add	t2,t1,a1		# (BDSLOT) addr+1 of last line for FLUSH
	li	t6,7
	li	t4,0xdffdffff		# correct FLUSH/INVAL address mask
1:	ori	t0,t6,0x1f0		# "ptags" bits will be bits 17:13
	sll	t0,9			#   and shift into proper position
	srl	t1,R6000_SLINE_SHIFT	# start to align FLUSH/INVAL addr
	andi	t6,t1,0x7f		# isolate cache line index
	sll	t6,2			#   and shift into proper position
	or	t0,t6			#   and merge to form complete addr
	sll	t1,R6000_SLINE_SHIFT	# align FLUSH addr on line boundary
	and	t1,t4			# adjust addr to avoid using upper 1/16
	and	t2,t4			#   of s-cache, and convert to K0
	li	t7,0xfffffc00		# invalid ptag value

#ifdef R6000_BUG_FLUSH
	lw	t6,invalidate_scache_inner_addr
	nop
	j	t6			# jump off to inner loop, which
	nop				# (BDSLOT) will restore SR and exit

invalidate_scache_inner_start:
#endif R6000_BUG_FLUSH
2: 	lcache	t4,0(t0)		# side-0 ptag
	nop				# (LDSLOT-1)
   	lcache	t5,1(t0)		# (LDSLOT-2) side-1 ptag
	srl	t4,PTE_PNUMSHFT		# r-justify pfn for side-0
	bne	t4,t3,3f		# matches side-0?
	srl	t5,PTE_PNUMSHFT		# (BDSLOT) r-justify pfn for side-1
#ifdef R6000_BUG_FLUSH
	nop				# align Flush/Scache to live in the
	nop				#  same I-cache line
#endif R6000_BUG_FLUSH
	flush	(t1)			# match!  flush side-0
	inval	(t1)			#  and invalidate
	scache	t7,(t0)			#  and clobber ptag
#ifdef R6000_BUG_FLUSH_PAUSE
	li	t4,FLUSH_PAUSE_COUNT	# give Memory a breather,
6:					#  so an IOC can get through
	bne	t4,zero,6b
	sub	t4,1			# (BDSLOT)
#endif R6000_BUG_FLUSH_PAUSE
	/* why would we match on both sides? well, play safe and ... */
3:	bne	t5,t3,4f		# matches side-1?
	nop				# (BDSLOT)
#ifdef R6000_BUG_FLUSH
	/* the next FLUSH must be followed by an SCACHE
	 * in the same I-cache line!
	 */
#endif R6000_BUG_FLUSH
	flush	1(t1)			# match!  flush side-1
	inval	1(t1)			#  and invalidate
	scache	t7,1(t0)		#   and clobber ptag
#ifdef R6000_BUG_FLUSH_PAUSE
	li	t4,FLUSH_PAUSE_COUNT	# give Memory a breather,
6:					#  so an IOC can get through
	bne	t4,zero,6b
	sub	t4,1			# (BDSLOT)
#endif R6000_BUG_FLUSH_PAUSE
4:	addi	t1,R6000_SLINE_BSIZE	# address for next flush/inval
	slt	t4,t1,t2		# all done?
	bne	t4,zero,2b		# loop back if not done with page
	addi	t0,4			# (BDSLOT) address of next ptag

	mtc0	t8,C0_SR		# restore SR, give time to settle
	j	ra
	nop				# (BDSLOT)
#ifdef R6000_BUG_FLUSH
invalidate_scache_inner_end:
#endif R6000_BUG_FLUSH
	.set	reorder
	END(invalidate_scache)


#ifndef R6000_BUG_FLUSH_FLAGS
/*
 * writeback_cache (addr, len)
 *
 * Writeback dirty s-cache lines for the K0 address range addr to addr+len.
 * Presume that we do not cross a page boundary.
 * For efficiency we examine the pertinent range of physical tags for a match
 * on the pfn, and flush only those data lines which match.
 */
LEAF(writeback_cache)
#ifdef DEBUG
	/*
	 *  A bug catcher:  verify K0/K1 address
	 */
	li	t0,0xc0000000		# high-order 2 bits of vaddr
	and	t1,a0,t0		# if vaddr begins with
	li	t2,0x80000000		#   binary 100 or 101 (K0 or K1)
	beq	t1,t2,1f		#    then we're ok, else exit 
	j	ra			# (do it this way so we can set a bp)
1:
#endif DEBUG
	/*
	 *  A bug catcher:  verify don't cross a page boundary
	 */
	sll	t0,a0,3			# strip off upper three bits of addr
	srl	t0,3
	add	t1,t0,a1
	sub	t1,1			# last byte
	srl	t0,BPTSHFT		# phys page addr of first byte
	srl	t1,BPTSHFT		#  and last byte
	beq	t0,t1,1f		# are they the same?
	PANIC("writeback_cache(): crosses page boundary")
1:
	.set	noreorder
	/*
	 *  form the effective address of the physical tags for
	 *  that part of the data cache which might hold this page
	 *	t0  addr of first ptag for LCACHE
	 *	t1  addr of first cache line for FLUSH
	 *	t2  addr of last  cache line for FLUSH
	 *	t3  pfn from "addr"
	 */
	mfc0	t8,C0_SR		# save SR
	li	t7,SR_MM_MODE		# disable interrupts
	mtc0	t7,C0_SR		#  and enable MM Mode
	li	t0,R6000_SLINE_BSIZE	# a1 <-- max( a1, line_size )
	slt	t4,a1,t0
	beq	t4,zero,1f
	nop
	move	a1,t0
1:	move	t1,a0			# leave "addr" arg untouched for debug
	sll	t3,t1,3			# shift off K0/K1 prefix bits...
	srl	t3,BPTSHFT+3		#   ...and shift to get true pfn
	li	t4,0xdfffffff		# tentative mask for FLUSH address
	li	t7,0xf			# pfn mask
	and	t6,t3,t7		# low-order 4 bits of pfn will be 12:9
	bne	t6,t7,1f		#  except 0xf gets mapped to 0x7
	add	t2,t1,a1		# (BDSLOT) addr+1 of last line for FLUSH
	li	t6,7
	li	t4,0xdffdffff		# correct FLUSH address mask
1:	ori	t0,t6,0x1f0		# "ptags" bits will be bits 17:13
	sll	t0,9			#   and shift into proper position
	srl	t1,R6000_SLINE_SHIFT	# start to align FLUSH addr
	andi	t6,t1,0x7f		# isolate cache line index
	sll	t6,2			#   and shift into proper position
	or	t0,t6			#   and merge to form complete addr
	sll	t1,R6000_SLINE_SHIFT	# align FLUSH addr on line boundary
	subu	t2,R6000_SLINE_BSIZE	# set up ending addr for loop control
	and	t1,t4			# adjust addr to avoid using upper 1/16
	and	t2,t4			#   of s-cache, and convert to K0

#ifdef R6000_BUG_FLUSH
	lw	t6,writeback_cache_inner_addr
	li	t7,0x3fffc		# benign S-cache address
	j	t6			# jump off to inner loop, which
	nop				# (BDSLOT) will restore SR and exit

writeback_cache_inner_start:
#endif R6000_BUG_FLUSH
2: 	lcache	t4,0(t0)		# side-0 ptag
	nop				# (LDSLOT-1)
   	lcache	t5,1(t0)		# (LDSLOT-2) side-1 ptag
	srl	t4,PTE_PNUMSHFT		# r-justify pfn for side-0
	bne	t4,t3,3f		# matches side-0?
	srl	t5,PTE_PNUMSHFT		# (BDSLOT) r-justify pfn for side-1
	flush	(t1)			# match!  flush side-0
#ifdef R6000_BUG_FLUSH
	scache	zero,(t7)		# SAME I-CACHE LINE AS FLUSH
#endif R6000_BUG_FLUSH
#ifdef R6000_BUG_FLUSH_PAUSE
	li	t4,FLUSH_PAUSE_COUNT	# give Memory a breather,
6:					#  so an IOC can get through
	bne	t4,zero,6b
	sub	t4,1			# (BDSLOT)
#endif R6000_BUG_FLUSH_PAUSE
3:	bne	t5,t3,4f		# matches side-1?
	addi	t0,4			# (BDSLOT) address of next ptag
	flush	1(t1)			# match!  flush side-1
#ifdef R6000_BUG_FLUSH
	scache	zero,(t7)		# SAME I-CACHE LINE AS FLUSH
#endif R6000_BUG_FLUSH
#ifdef R6000_BUG_FLUSH_PAUSE
	li	t4,FLUSH_PAUSE_COUNT	# give Memory a breather,
6:					#  so an IOC can get through
	bne	t4,zero,6b
	sub	t4,1			# (BDSLOT)
#endif R6000_BUG_FLUSH_PAUSE
4:	slt	t4,t1,t2		# all done?
	bne	t4,zero,2b		# loop back if not done with page
	addi	t1,R6000_SLINE_BSIZE	# (BDSLOT) address for next flush

	mtc0	t8,C0_SR		# restore SR, give time to settle
	j	ra
	nop				# (BDSLOT)
#ifdef R6000_BUG_FLUSH
writeback_cache_inner_end:
#endif R6000_BUG_FLUSH
	.set	reorder
	END(writeback_cache)
#endif !R6000_BUG_FLUSH_FLAGS


/*
 * flush_cache()
 * flush entire cache
 */
LEAF(flush_cache)
	.set	noreorder
	mfc0	t8,C0_SR		# disable interrupts, and
	li	t7,SR_MM_MODE		#  enable MemMgmt Mode
	mtc0	t7,C0_SR		# actually update SR here

	/*
	 *  Invalidate virtual d-cache
	 */
	lw	t1,dcache_size
	move	t0,zero			# init starting address
	subu	t1,16*R6000_DLINE_BSIZE	#  and ending address
1:	inval	1+ 0*R6000_DLINE_BSIZE(t0)	# invalidate 16 lines at a time,
	inval	1+ 1*R6000_DLINE_BSIZE(t0)	#  trading off excess invals
	inval	1+ 2*R6000_DLINE_BSIZE(t0)	#   against efficient looping
	inval	1+ 3*R6000_DLINE_BSIZE(t0)
	inval	1+ 4*R6000_DLINE_BSIZE(t0)
	inval	1+ 5*R6000_DLINE_BSIZE(t0)
	inval	1+ 6*R6000_DLINE_BSIZE(t0)
	inval	1+ 7*R6000_DLINE_BSIZE(t0)
	inval	1+ 8*R6000_DLINE_BSIZE(t0)
	inval	1+ 9*R6000_DLINE_BSIZE(t0)
	inval	1+10*R6000_DLINE_BSIZE(t0)
	inval	1+11*R6000_DLINE_BSIZE(t0)
	inval	1+12*R6000_DLINE_BSIZE(t0)
	inval	1+13*R6000_DLINE_BSIZE(t0)
	inval	1+14*R6000_DLINE_BSIZE(t0)
	inval	1+15*R6000_DLINE_BSIZE(t0)
	bne	t0,t1,1b		# all done?
	addi	t0,16*R6000_DLINE_BSIZE	# (BDSLOT)  increment invalidate addr

	/*
	 *  Invalidate virtual i-cache
	 */
	lw	t1,icache_size
	move	t0,zero			# init starting address
	subu	t1,16*R6000_ILINE_BSIZE	#  and ending address
2:	inval	 0*R6000_ILINE_BSIZE(t0) 	# invalidate 16 lines at a time,
	inval	 1*R6000_ILINE_BSIZE(t0)	#  trading excess invalidations
	inval	 2*R6000_ILINE_BSIZE(t0)	#  against efficient looping
	inval	 3*R6000_ILINE_BSIZE(t0)
	inval	 4*R6000_ILINE_BSIZE(t0)
	inval	 5*R6000_ILINE_BSIZE(t0)
	inval	 6*R6000_ILINE_BSIZE(t0)
	inval	 7*R6000_ILINE_BSIZE(t0)
	inval	 8*R6000_ILINE_BSIZE(t0)
	inval	 9*R6000_ILINE_BSIZE(t0)
	inval	10*R6000_ILINE_BSIZE(t0)
	inval	11*R6000_ILINE_BSIZE(t0)
	inval	12*R6000_ILINE_BSIZE(t0)
	inval	13*R6000_ILINE_BSIZE(t0)
	inval	14*R6000_ILINE_BSIZE(t0)
	inval	15*R6000_ILINE_BSIZE(t0)
	bne	t0,t1,2b		# all done?
	addi	t0,16*R6000_ILINE_BSIZE	# (BDSLOT)  increment invalidate addr

	/*
	 *  Invalidate virtual and physical tags of s-cache (and write-back)
	 */
	li	t0,0x3e000		# init starting address for ptags
	li	t1,K0BASE		# init starting address for flush/inval
	add	t2,t1,0x3c000		#  and ending address
	li	t7,0xfffffc00		# invalid ptag value
#ifdef R6000_BUG_FLUSH_PAUSE
	li	t3,7			# init 'pause' counter
#endif R6000_BUG_FLUSH_PAUSE
#ifdef R6000_BUG_FLUSH
	lw	t6,flush_cache_inner_addr
	nop
	j	t6			# jump off to inner loop, which
	nop				# (BDSLOT) will restore SR and exit

flush_cache_inner_start:
#endif R6000_BUG_FLUSH
3:	flush	0(t1)			# flush side-0
	inval	0(t1)			#  and invalidate
	scache	t7,0(t0)		#   and clobber ptag
	flush	1(t1)			# flush side-1
	inval	1(t1)			#  and invalidate
	scache	t7,1(t0)		#   and clobber ptag
	addi	t1,R6000_SLINE_BSIZE	# address for next flush/inval
#ifdef R6000_BUG_FLUSH_PAUSE
	bne	t3,zero,5f		# time for a pause?
	sub	t3,1			# (BDSLOT) decrement pause counter

	li	t3,FLUSH_PAUSE_COUNT	# give Memory a breather,
4:					#  so an IOC can get through
	bne	t3,zero,4b
	sub	t3,1			# (BDSLOT)

	li	t3,7			# reinit pause counter
5:
#endif R6000_BUG_FLUSH_PAUSE
	bne	t1,t2,3b		# all done?
	addi	t0,4			# (BDSLOT) address of next ptag

	mtc0	t8,C0_SR		# restore SR, give time to settle
	j	ra
	nop				# (BDSLOT)
#ifdef R6000_BUG_FLUSH
flush_cache_inner_end:
#endif R6000_BUG_FLUSH
	.set	reorder
	END(flush_cache)

/*
 * page_iflush(addr)
 * Flush i- and d-cache for the specified page (expressed as a K0 address).
 * Since the R6000 has virtual i- and d-caches, these routines do nothing.
 * The caller is expected to flush the virtual primary caches and the
 * secondary physical cache, as appropriate.
 */
LEAF(page_iflush)
	j	ra	
	END(page_iflush)

LEAF(page_dflush)
	j	ra
	END(page_dflush)

/*
 * Config_cache() -- determine sizes of i and d caches, and s-cache
 * Sizes stored in globals dcache_size, icache_size,
 * and scache_size (PER SIDE).
 */
#ifdef R6000_BUG_FLUSH
CONFIG_CACHE_FRM=	(4*4)+4
NESTED(config_cache, CONFIG_CACHE_FRM, zero)
	subu	sp,CONFIG_CACHE_FRM
	sw	ra,CONFIG_CACHE_FRM-4(sp)
#else
LEAF(config_cache)
#endif R6000_BUG_FLUSH

	li	v0,16*1024		# TODO get value from IdProm
	sw	v0,dcache_size

	li	v0,64*1024		# TODO get value from IdProm
	sw	v0,icache_size

	li	v0,256*1024		# TODO get value from IdProm
	sw	v0,scache_size

#ifdef R6000_BUG_FLUSH
	li	a1,0x800000a0
#ifdef R6000_BUG_FLUSH_FLAGS
	sw	a1,invalidate_scache_inner_addr	# target address
#else
	sw	a1,writeback_cache_inner_addr	# target address
	la	a0,writeback_cache_inner_start	# source address
	la	a2,writeback_cache_inner_end
	subu	a2,a0				# num bytes to move
	add	t0,a1,a2
	add	t0,R6000_ILINE_BSIZE		# figure out the addr of
	and	t0,~(R6000_ILINE_BSIZE-1)	#  the next "inner loop"
	sw	t0,invalidate_scache_inner_addr
	jal	bcopy

	lw	a1,invalidate_scache_inner_addr
#endif R6000_BUG_FLUSH_FLAGS
	la	a0,invalidate_scache_inner_start
	la	a2,invalidate_scache_inner_end
	subu	a2,a0
	add	t0,a1,a2
	add	t0,R6000_ILINE_BSIZE		# figure out the addr of
	and	t0,~(R6000_ILINE_BSIZE-1)	#  the next "inner loop"
	sw	t0,flush_cache_inner_addr
	jal	bcopy

	lw	a1,flush_cache_inner_addr
	la	a0,flush_cache_inner_start
	la	a2,flush_cache_inner_end
	subu	a2,a0
	jal	bcopy
	
	lw	ra,CONFIG_CACHE_FRM-4(sp)
	addu	sp,CONFIG_CACHE_FRM
#endif R6000_BUG_FLUSH

	j	ra
	END(config_cache)
