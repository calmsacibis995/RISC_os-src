#ident "$Header: pon_subr_6000.s,v 1.5.7.1 90/07/18 14:33:23 huang Exp $"
/* $Copyright$ */

#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"


#define DLINE_BSIZE	  8
#define	ILINE_BSIZE	 32
#define SLINE_BSIZE	128

/*
 *  init_cache()
 *
 *  Put cache related state to known values clear PE,
 *  insure all cache lines have correct parity.
 *
 *  The s-cache has random (i.e., riddled with parity errors) values in
 *  its vtags and data.
 *
 *  First, use SCACHE to write a zero to every "data" word (to clear parity and
 *  clear the dirty bits) and every "tlb" word (to clear parity and to put an
 *  "invalid" tlb entry everywhere).
 *
 *  Second, use SCACHE to write 0xfffffc00 to every ptag (to clear
 *  parity and to put a never-matching value into the ptags, forcing
 *  future cache-misses).
 *
 *  Third, use INVAL to invalidate every "data" line and every "tlb" line.
 *
 *  Finally, INVALidate every line of the i-cache and d-cache.
 */
LEAF(init_cache)
	.set	noreorder
	mfc0	t8,C0_SR		# save SR
	li	t0,SR_MM_MODE|SR_BEV	# enable MM_MODE, disable interrupts
	mtc0	t0,C0_SR

	/*  init ptag words  */
	li	t0,0x3e000		# first SCACHE ptag address
	li	t1,0x40000-16		# ending SCACHE address
	li	t2,0xfffffc00		# non-matching ptag value
1:	scache	t2,0(t0)		# init side-0 word
	scache	t2,1(t0)		#  and side-1 word
	scache	t2,4(t0)
	scache	t2,5(t0)
	scache	t2,8(t0)
	scache	t2,9(t0)
	scache	t2,12(t0)
	scache	t2,13(t0)
	bne	t0,t1,1b		# all done with ptags?
	addi	t0,16			# (BDSLOT) increment address

	/*  init data/tlb words  */
	li	t0,0			# first SCACHE data/tlb address
reset_virt_cache_alt_entry:		# t0 points to base of tlbs
	li	t1,0x3e000-16		# ending SCACHE address
1:	scache	zero,0(t0)		# init side-0 word
	scache	zero,1(t0)		#  and side-1 word
	scache	zero,4(t0)
	scache	zero,5(t0)
	scache	zero,8(t0)
	scache	zero,9(t0)
	scache	zero,12(t0)
	scache	zero,13(t0)
	bne	t0,t1,1b		# all done with data/tlbs?
	addi	t0,16			# (BDSLOT) increment address

	/*  invalidate data/tlb lines  */
	li	t0,0			# first SCACHE data/tlb address
	li	t1,0x3e000-4*SLINE_BSIZE	# ending SCACHE address
1:	inval	0(t0)			# init side-0 line
	inval	1(t0)			#  and side-1 line
	inval	SLINE_BSIZE+0(t0)
	inval	SLINE_BSIZE+1(t0)
	inval	2*SLINE_BSIZE+0(t0)
	inval	2*SLINE_BSIZE+1(t0)
	inval	3*SLINE_BSIZE+0(t0)
	inval	3*SLINE_BSIZE+1(t0)
	bne	t0,t1,1b		# all done with data/tlbs?
	addi	t0,4*SLINE_BSIZE	# (BDSLOT) increment address

reset_cache_alt_entry:
	/*
	 *  Invalidate the primary caches.  We keep things simple and do them
	 *  one cache at a time.
	 *  XXX hack in the icache and dcache sizes
	 */
	move	t0,zero
	add	t1,t0,16*1024		# last byte to invalidate, plus one
	subu	t1,8*DLINE_BSIZE	# form loop-ending address
1:	inval	0*DLINE_BSIZE+1(t0)	# invalidate d-cache line
	inval	1*DLINE_BSIZE+1(t0)
	inval	2*DLINE_BSIZE+1(t0)
	inval	3*DLINE_BSIZE+1(t0)
	inval	4*DLINE_BSIZE+1(t0)
	inval	5*DLINE_BSIZE+1(t0)
	inval	6*DLINE_BSIZE+1(t0)
	inval	7*DLINE_BSIZE+1(t0)
	bne	t0,t1,1b		# all done?
	addi	t0,8*DLINE_BSIZE	# (BDSLOT) increment target address

	move	t0,zero
	add	t1,t0,64*1024		# last byte to invalidate, plus one
	subu	t1,8*ILINE_BSIZE	# form loop-ending address
1:	inval	0*ILINE_BSIZE(t0)	# invalidate d-cache line
	inval	1*ILINE_BSIZE(t0)
	inval	2*ILINE_BSIZE(t0)
	inval	3*ILINE_BSIZE(t0)
	inval	4*ILINE_BSIZE(t0)
	inval	5*ILINE_BSIZE(t0)
	inval	6*ILINE_BSIZE(t0)
	inval	7*ILINE_BSIZE(t0)
	bne	t0,t1,1b		# all done?
	addi	t0,8*ILINE_BSIZE	# (BDSLOT) increment target address
	
	j	ra
	mtc0	t8,C0_SR		# (BDSLOT) restore SR
	.set	reorder
	END(init_cache)

/*
 *  reset_cache()
 *
 *  Whereas init_cache() completely initializes the caches, including tlbs,
 *  to eliminate parity errors and to cleanse everything, reset_cache()
 *  only performs a cleanse of the data areas (including ptags).
 */
LEAF(reset_cache)
	.set	noreorder
	mfc0	t8,C0_SR		# save SR
	li	t0,SR_MM_MODE|SR_BEV	# enable MM_MODE, disable interrupts
	mtc0	t0,C0_SR		#

	/*
	 *  Now start from the base of the s-cache and SCACHE one word per line
	 *  (to clear each line's dirty flag) and Invalidate each line, for the
	 *  entire data section of the s-cache (excluding ptags and tlbs),
	 *  and write a non-matching value into each ptag.
	 */
	move	t0,zero			# first SCACHE data word address
	li	t1,0x3c000		# ending SCACHE address
	li	t3,0xfffffc00		# generic non-matching ptag value
	li	t4,0x3e000		# first ptag address
1:	scache	zero,0(t0)		# clear "dirty" for s-cache side-0
	inval	0(t0)			# and invalidate side-0
	scache	zero,1(t0)		# and do the same for side-1
	inval	1(t0)
	scache	t3,0(t4)		# write side-0 ptag
	scache	t3,1(t4)		#   and size-1 ptag
	addi	t0,SLINE_BSIZE		# increment data address
	bne	t0,t1,1b		# all done with s-cache?
	addi	t4,4			# (BDSLOT) increment ptag address
	.set	reorder

	j	reset_cache_alt_entry	# invalidate primary caches
	END(reset_cache)

/* XXX why does R2000/3000 need this routine? */
LEAF(invalidate_cache)
	j	reset_cache
	END(invalidate_cache)

/*
 *  flushall_tlb()
 *	Flushes all tlb entries.  Because the R6000 has virtual tags and
 *	has virtual-to-physical mapping information implicit in those tags,
 *	we must really invalidate the vtags of all caches, in addition to
 *	writing zeroes to all tlb entries.  To make this all simple, we
 *	just call init_cache() at the appropriate alternate entrypoint
 *	which does all this.
 *	Also, resets the PID (C0_PID) to zero.
 */
LEAF(flushall_tlb)
	.set	noreorder
	mfc0	t8,C0_SR		# save SR
	li	t0,SR_MM_MODE|SR_BEV	# enable MM_MODE, disable interrupts
	mtc0	t0,C0_SR		#
	mtc0	zero,C0_PID		# get back to a known PID
	.set	reorder

	li	t0,0x3c000		# base address of tlb entries
	j	reset_virt_cache_alt_entry
	END(flushall_tlb)

/*
 *  get_membrd_size(ctlspace)
 *	Returns size in bytes of Memory board addressed by "ctlspace", or
 *	zero if not a Memory board.
 */
LEAF(get_membrd_size)
	move	v0,zero			# initialize return value
#ifdef R6000_BUG_IDPROM
	add	t0,a0,SBC_CTLMISC	# base of interesting IdProm info
	lw	v1,(t0)			# Board Type
	and	v0,v1,BrdIsMem
	beq	v0,zero,9f
#ifdef SABLE
	add	t0,a0,SBC_IDPROM+4
	lb	v0,ID_MEMSIZE_OFF(t0)
#else
	li	v0,5			# XXX 32MB
#endif SABLE
#else
	add	t0,a0,SBC_IDPROM+4	# base of interesting IdProm info
	lw	t1,(t0)			# Board Type
	bne	t1,BRDTYPE_R6350,9f	# exit if not Memory
	lb	v0,ID_MEMSIZE_OFF(t0)
#endif R6000_BUG_IDPROM
	li	t1,1
	sll	v0,t1,v0		# shift to get Mbytes
	sll	v0,20			# shift to get bytes
9:	j	ra
	END(get_membrd_size)


LEAF(FlushWB)
	j	ra
END(FlushWB)
