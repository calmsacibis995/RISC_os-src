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
/* $Header: tlb_r6000.s,v 1.7.1.8.1.5.1.2 90/11/06 12:38:17 beacker Exp $ */

#ifdef R6000

#include "../ml/assym.s"

	BSS(tlb_lrf,64)			/* least-recently-filled table */
					/* one byte per line */
	BSS(tlb_shadow,4*64*2)		/* 64 lines, 2-way interleaved */

#define SCACHE_UAREA_ADDR 0xfe03dffc	/* effective SCACHE addr of U-area */

#ifdef R6000_BUG_PID
/*
 *	NOTE:
 *	There is a bug in the R6020 SBC which necessitated rework on the
 *	CPU board, the net effect of which produces a problem with
 *		mtc0  Rx,C0_PID
 *	under obscure circumstances, leaving a correct PID value in the
 *	CPU onchip PID register and an incorrect PID value in the offchip
 *	register.  For instance:  if this instruction lies in the last word of
 *	an S-cache line, and the next line gets a shared hit.
 *	The simplest workaround is to issue two such instructions back-to-back.
 *	Another glitch:  because of the PID mismatch, an external parity error
 *	might be declared, so we disable these MachineChecks around the mtc0.
 *	Because interrupts are disabled, we can use $k0 and $k1.
 *	Another glitch:  the back-to-pack write-PID sequence doesn't completely
 *	fix the problem in CPU rev3.1, as it might leave the 2nd mtc0's S-cache
 *	tag with bad parity.  We correct for this by invalidating this
 *	instruction's address, forcing an I-cache miss and a shared-hit in
 *	the S-cache, which rewrites that tag.
 */
#define WRITE_PID(newPID,saveERROR,scratch,saveSR)	\
			mfc0	saveERROR,C0_ERROR;	\
			li	scratch,C0_ERROR_IMASK;	\
			mfc0	saveSR,C0_SR;		\
			mtc0	scratch,C0_ERROR;	\
			li	scratch,SR_MM_MODE;	\
			mtc0	scratch,C0_SR;		\
			mtc0	newPID,C0_PID;		\
80:			mtc0	newPID,C0_PID;		\
			la	scratch,80b;		\
			inval	(scratch);		\
			or	saveERROR,C0_ERROR_EXT;	\
			mtc0	saveERROR,C0_ERROR;	\
			mtc0	saveSR,C0_SR

#else R6000_BUG_PID
#define	WRITE_PID(newPID,tmp0,tmp1,tmp2)	mtc0	newPID,C0_PID
#endif R6000_BUG_PID

/*
 *  Deal with tlbmisses in KUSEG
 *
 *  A Jump to this UTLBmiss ("User TLBmiss") handler is moved at startup to
 *  the Utlbmiss vector 0x80000000.  The R-series CPU architecture specifies
 *  that the General Exception entrypoint is 0x80000080, which allows room for
 *  at most 32 instructions.  Since this utlbmiss handler is longer than this,
 *  we would have to break it into two parts anyway (one living at 0x80000000,
 *  and the other living elsewhere in the kernel), so we keep it in one piece
 *  and stuff the Jump to it at the 0x80000000 entrypoint.
 *
 */

VECTOR(utlbmiss,0);
	.set	noreorder
	j	utlbmiss_r6000
	nop				# don't worry about filling BDSLOT now
	.set	reorder
	END(utlbmiss);
EXPORT(eutlbmiss)	

NESTED(utlbmiss_r6000, 0, k1)
	.set	noreorder
	.set	noat
	move	k0,AT
	.set	at
	sw	k0,utlbmiss_save_AT
	sw	t0,utlbmiss_save_t0
	sw	t1,utlbmiss_save_t1
	sw	t2,utlbmiss_save_t2
	sw	t3,utlbmiss_save_t3
	sw	t4,utlbmiss_save_t4
	sw	t5,utlbmiss_save_t5
	sw	t6,utlbmiss_save_t6
	sw	t7,utlbmiss_save_t7
	mfc0	k0,C0_BADVADDR		# get faulting address
	mfc0	t0,C0_PID		# get current PID
	.set	reorder
	/*
	 * The tlb cache has 64 lines of 32 ptes.
	 * The tlb tag is the virtual address of the 32 page block
	 * in the tlb cache or'd with the PID.
	 */
	srl	t6,k0,PNUMSHFT+5	# get virtual block address
	and	t7,t6,0x3f		# extract tlb index
	sll	t0,24			# shift PID to upper byte
	or	t6,t0			#  and merge to form tlb tag

	/*
	 * Find the ste for the faulting address.
	 * Note KSTEBASE is -8k, so it can be used as
	 * an offset.
	 */
	srl	t0,k0,SNUMSHFT		# get segment number
	sll	t0,2			# adjust for word index
	lw	t5,KSTEBASE(t0)		# fetch ste

	/*
	 * Now add the page offset in the segment.
	 * Each tlb line maps 512k, so there are 4 tlb lines per
	 * segment. Each pde is 8 bytes, so the pde blocks to be
	 * loaded are 256 bytes long.
	 */
	and	t1,t7,0x3		# figure tlb line in segment
	sll	t1,8			# * 256 is pde offset
	add	t0,t5,t1		# add to ste to get pde address

	/*
	 * Form scache address to write the tlb entries:
	 * 31:25	from 31:25 of byte address, unchanged
	 * 24:18	zero
	 * 17:14	all ones
	 * 13:13	zero (==tlb, 1==ptag)
	 * 12:2		cache word index, from 24:14 of byte address
	 * 1:1		zero
	 * 0:0		s-cache side to write tlb in
	 *		THIS MUST BE IN THE INSTRUCTION OFFSET
	 *
	 * Note that since we want the 1st address in the tlb line,
	 * bits 7:2 will be zero in the address formed below.
	 */
	srl	t1,k0,25		# right justify bits 31:25
	sll	t1,12			#  shift back to clear lsb's
	or	t1,0x1e			#  merge what will be bits 17:13
	sll	t1,13			#  and shift into proper position
	srl	t2,k0,12		# put 24:14 in 12:2
	and	t2,0x1f80		#  and off bits we don't need
	or	t1,t2			# merge to form scache address

	/*
	 * The scache address is now correct except for bit 0.
	 * (the scache side)
	 *
	 * Use TLB Least Recently Filled Table to determine which side
	 *  to clobber.
	 * This table has one byte per tlb line (64 bytes) and indicates
	 *  which side to use next. The value is then toggled for the
	 *  next [u]tlbmiss.
	 * Of course, a "Least Recently Used" strategy would be better,
	 *  but we don't have a simple way of knowing which side the
	 *  hardware mostrecently referenced.
	 * KLUDGE:  don't perturb the last tlb line of side 0, as that's
	 *          where the u-area tlb is kept.
	 */
	la	t2,tlb_lrf		# tlb_lrf base address
	add	t2,t7			# form correct entry address
	li	t4,0x3f			# last entry number
	li	t3,1			# use side 1 if last entry
	beq	t7,t4,1f		# brach if last entry
	lb	t3,(t2)			# fetch tlb_lrf entry
	xor	t4,t3,1			# toggle it
	sb	t4,(t2)			# store it back
1:

	/*
	 * Update tlb_shadow.
	 * It is a two dimensional array of tlb tags, indexed by
	 * tlb index and side.
	 */
	la	t2,tlb_shadow		# tlb_shadow base address
	sll	t7,3			# rows are 2 words wide
	add	t2,t7			#  so add tlb_index*8
	sll	t3,2			# entries are 1 word wide
	add	t2,t3			#  so add side*4
	sw	t6,(t2)			# store tlb tag

	/*
	 * First zero the line so if we take a tlb miss fetching
	 * the ptes, we have a consistant tlb.
	 */
	.set	noreorder
	mfc0	t7,C0_SR		# save current SR
	li	t6,SR_MM_MODE		# enable MM mode
	mtc0	t6,C0_SR
	move	t2,t1			# save scache addr
	bne	zero,t3,2f		# decide which line to zero
	li	k0,1			# (BDSLOT) loop twice

1:
	scache	zero,0(t1)
	scache	zero,4(t1)
	scache	zero,8(t1)
	scache	zero,12(t1)
	scache	zero,16(t1)
	scache	zero,20(t1)
	scache	zero,24(t1)
	scache	zero,28(t1)
	scache	zero,32(t1)
	scache	zero,36(t1)
	scache	zero,40(t1)
	scache	zero,44(t1)
	scache	zero,48(t1)
	scache	zero,52(t1)
	scache	zero,56(t1)
	scache	zero,60(t1)
	add	t1,64
	bnel	zero,k0,1b
	sub	k0,1			# BDSLOT
	b	3f
	move	t1,t2			# (BDSLOT) restore scache addr

2:
	scache	zero,1(t1)
	scache	zero,5(t1)
	scache	zero,9(t1)
	scache	zero,13(t1)
	scache	zero,17(t1)
	scache	zero,21(t1)
	scache	zero,25(t1)
	scache	zero,29(t1)
	scache	zero,33(t1)
	scache	zero,37(t1)
	scache	zero,41(t1)
	scache	zero,45(t1)
	scache	zero,49(t1)
	scache	zero,53(t1)
	scache	zero,57(t1)
	scache	zero,61(t1)
	add	t1,64
	bnel	zero,k0,2b
	sub	k0,1			# BDSLOT

	move	t1,t2			# restore scache addr
3:
	/*
	 * If the original ste was zero, let the invalid tlb entry
	 * written above cause a general exception.
	 */ 
	beq	zero,t5,utlb_out
	nop

	/*
	 * Now fill the tlb line.
	 * Ptes are mixed with dbds, so skip every other word when
	 * loading them. At present, the loads are from k0. But this
	 * may change in the future, so toggle MM mode anyway.
	 */
	bne	zero,t3,2f
	li	k0,7			# (BDSLOT) loop 8 times

1:
	mtc0	t7,C0_SR		# disable MM
	nop
	lw	t2,0(t0)
	lw	t3,8(t0)
	lw	t4,16(t0)
	lw	t5,24(t0)
	mtc0	t6,C0_SR		# enable MM
	nop
	scache	t2,0(t1)
	scache	t3,4(t1)
	scache	t4,8(t1)
	scache	t5,12(t1)
	add	t0,32			# adjust pte address
	add	t1,16			# adjust scache address
	bnel	zero,k0,1b
	sub	k0,1			# BDSLOT
	b	3f
	mtc0	t7,C0_SR		# restore SR

2:
	mtc0	t7,C0_SR		# disable MM
	nop
	lw	t2,0(t0)
	lw	t3,8(t0)
	lw	t4,16(t0)
	lw	t5,24(t0)
	mtc0	t6,C0_SR		# enable MM
	nop
	scache	t2,1(t1)
	scache	t3,5(t1)
	scache	t4,9(t1)
	scache	t5,13(t1)
	add	t0,32			# adjust pte address
	add	t1,16			# adjust scache address
	bnel	zero,k0,2b
	sub	k0,1			# BDSLOT

EXPORT(eutlbmiss_r6000)	
	/*
	 * Beyond this point, any tlbmiss is *not* a double-tlbmiss.
	 */
utlb_out:
	mtc0	t7,C0_SR		# restore SR
3:
	lw	t0,utlbmiss_save_t0
	lw	t1,utlbmiss_save_t1
	lw	t2,utlbmiss_save_t2
	lw	t3,utlbmiss_save_t3
	lw	t4,utlbmiss_save_t4
	lw	t5,utlbmiss_save_t5
	lw	t6,utlbmiss_save_t6
	lw	t7,utlbmiss_save_t7
	lw	k0,utlbmiss_save_AT
	mfc0	k1,C0_EPC
	.set	noat
	move	AT,k0
	j	k1
	c0	C0_RFE
	.set	at
	.set	reorder
	END(utlbmiss_r6000)

/*
 * unmaptlb(rpid, vpage): unmap a single tlb entry
 *
 *	Probe for RPID/VPAGE in TLB.  If it doesn't exist, all done.
 *	If it does, zero the entry to mark it as invalid.
 */
LEAF(unmaptlb)
	.set	noreorder
	mfc0	t6,C0_SR		# save SR
	li	t5,SR_MM_MODE		# disable interrupts
	mtc0	t5,C0_SR		#  and turn on MM Mode
	nop				# give it time to settle
	mfc0	t7,C0_PID		# save current PID
	nop				#  (wait a bit...for offchip PID?)
	WRITE_PID(a0,k0,k1,v0)		# reset PID
	.set	reorder

	/*
	 *  Because each tlb line of 32 entries shares the identical
	 *  "Global" characteristic, we enforce these rules:
	 *	00000000..7fffffff	non-Global
	 *	c0000000..(UADDR-1)	Global
	 *	UADDR..ffffffff		(UADDR upward) non-Global
	 */
	sll	t2,a1,PNUMSHFT		# convert vpage to byte address
	li	t0,UADDR
	sltu	t8,t2,t0		# address < UADDR?
	beq	t8,zero,1f		#   ...no, upper K2 is PID-specific
	li	t0,K0BASE
	sltu	t8,t0,t2		# K0BASE < address?
	/*
	 *  now t8 ==1 for Global, ==0 for PID-specific
	 */
1:
	/*
	 *  Compute address of the tlb entry
	 */
	srl	t0,a1,11		# r-justify most signif 7 bits
	sll	t0,12			#  shift 'em back to clear lsb's
	ori	t0,0x1e			#  merge what will be bits 17:13
	sll	t0,13			#  and shift back into proper position
	sll	t1,a1,2			# move low-order 11 bits of vpage ...
	andi	t1,0x1ffc		#  ... to 12:2 and clear lsbits
	add	t0,t1			# effective addr for tlb line

	/*
	 *  Form the addr/pid of what we're looking for
	 */
	srl	t9,a1,5			# r-justify page addr of tlb line
	and	t9,0x1fff		#  clear upper bits (a1 signed?)
	move	t5,zero			# tentative:  Global
	bne	t8,zero,2f		# branch if Global
	move	t5,a0			# if PID-specific and PID arg nonzero,
	bne	a0,zero,2f		#  then use PID arg,
	move	t5,t7			#  else use current PID
2:	sll	t5,24			# shift PID/zero into position
	or	t9,t5			#  and merge with "page addr" (/32)

	la	t1,tlb_shadow
	and	t2,t9,0x3f		# tlb line index
	sll	t5,t2,3			#  becomes byte offset to doubleword
	add	t1,t5			# addr of doubleword entry

	lw	t3,(t1)			# what's at side-0
	lw	t4,4(t1)		#	and side-1
	beq	t3,t9,unm_0
	bne	t4,t9,u_exit		# no hit, so exit

	.set	noreorder
	/*
	 *  Side-1
	 */
	bne	t8,zero,1f		# Global?  Or PID-specific?
	nop
	scache	zero,1(t0)		# clobber side-1, PID-specific
	b	u_exit			# exit
	nop				# (BDSLOT...don't write PID here)

1:	scache	zero,3(t0)		# clobber side-1, Global
	b	u_exit			# exit
	nop				# (BDSLOT...don't write PID here)

	/*
	 *  Side-0
	 */
unm_0:
	bne	t8,zero,1f		# Global?  Or PID-specific?
	nop
	scache	zero,(t0)		# clobber side-0, PID-specific
	b	u_exit			# exit
	nop				# (BDSLOT...don't write PID here)

1:	scache	zero,2(t0)		# clobber side-0, Global
	nop				# (don't be changing PID right away)

u_exit:	WRITE_PID(t7,k0,k1,v0)		# reset PID
	mtc0	t6,C0_SR		# restore SR
	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(unmaptlb)


/*
 * invaltlb(i): Invalidate the ith ITLB entry.
 * called whenever a specific TLB entry needs to be invalidated.
 */
LEAF(invaltlb)
	j	ra
	END(invaltlb)

/*
 * tlbwired(indx, tlbpid, vaddr, pte) -- setup wired TLB entry
 * a0 -- indx -- tlb entry index
 * a1 -- tlbpid -- context number to use (0-255)
 * a2 -- vaddr -- virtual address (could have offset bits)
 * a3 -- pte -- contents of pte
 */
LEAF(tlbwired)
	/*
	 *  Simply call tlbdropin with the same arguments, left-shifted by one.
	 */
	move	a0,a1
	move	a1,a2
	move	a2,a3	
	/*
	 *  And we fall into tlbdropin()
	 */
/*
 * tlbdropin(tlbpid, vaddr, pte) -- random tlb drop-in
 * a0 -- tlbpid -- tlbcontext number to use (0-255)
 * a1 -- vaddr -- virtual address to map. Can contain offset bits
 * a2 -- pte -- contents of pte
 *
 * t7  save C0_PID
 * t6  save C0_SR
 */
EXPORT(tlbdropin)
#ifdef PERFECT_COLORING
	srl	t0,a1,PNUMSHFT		# r-justify vpn
	srl	t1,a2,PTE_PNUMSHFT	# r-justify pfn
	and	t0,0xF
	and	t1,0xF
	beq	t0,t1,1f		# correctly colored?
	andi	t0,a2,2			# pte valid?
	beq	t0,zero,1f
	andi	t0,a2,8			# yes...noncached?
	bne	t0,zero,1f
	PANIC("tlbdropin: incorrect coloring")
1:
#endif PERFECT_COLORING
	bgez	a0,1f
	PANIC("tlbdropin: PID < 0")
1:
	.set	noreorder
	mfc0	t6,C0_SR		# save SR
	li	t5,SR_MM_MODE		# disable interrupts
	mtc0	t5,C0_SR		#  and enable MM Mode
	andi	t8,a2,PG_G		# isolate Global bit in pte
	mfc0	t7,C0_PID		# save current PID
	.set	reorder

	/*
	 *  Because each tlb line of 32 entries shares the identical
	 *  "Global" characteristic, we enforce these rules:
	 *	00000000..7fffffff	non-Global
	 *	c0000000..(UADDR-1)	Global
	 *	UADDR..ffffffff		(UADDR upward) non-Global
	 */
	li	t0,UADDR
	sltu	t1,a1,t0		# address < UADDR?
	beq	t1,zero,non_Global	#   ...no, upper K2 is PID-specific
	li	t0,K0BASE
	sltu	t1,a1,t0		# address < K0BASE?
	bne	t1,zero,non_Global	#   ...yes, Kuseg is PID-specific

	/*
	 *  The virtual address is in the Global range.  Verify that the pte
	 *  says Global.
	 */
	bne	t8,zero,pid_ok		# debug: verify pte says Global
	.set	noreorder
	mtc0	t6,C0_SR		# restore SR (esp. SR_MM_MODE)
	.set	reorder
	PANIC("tlbdropin: erroneous pte non-Global")

non_Global:
	/*
	 *  The virtual address is in the non-Global range.  Verify that the
	 *  pte says non-Global.
	 */
	beq	t8,zero,pid_ok		# debug:  verify pte says non-Global
	.set	noreorder
	mtc0	t6,C0_SR		# restore SR (esp. SR_MM_MODE)
	.set	reorder
	PANIC("tlbdropin: erroneous pte Global")

pid_ok:	/*
	 *  KLUDGE:  if we're writing anything to the u-area tlb line, then
	 *	     always use side-0 for the u-area line, side-1 for other.
	 *  Otherwise, read the appropriate TLB entry for each side to 
	 *   determine if we have an existing matching line.
	 *   If we do have a match, then stuff the new entry.
	 *   If we don't have a match, then pick an s-cache side, clobber all
	 *    32 TLB entries in the line, and stuff the new entry.
	 */

	.set	noreorder
	WRITE_PID(a0,k0,k1,v0)		# reset PID 

	srl	t9,a1,PNUMSHFT+5	# r-justify virtual page addr of line
	and	t2,t9,0x3f		# tlb line index
	bne	t8,zero,1f		# if Global, then form address/PID
	move	t0,zero			# (BDSLOT) using a zero PID,
	sll	t0,a0,24		# else use the real new PID
1:	or	t9,t0			#  and form address/PID identifier
	.set	reorder

	srl	t0,a1,25		# r-justify most signif 7 vaddr bits
	sll	t0,12			# shift most signif vpn bits back
	ori	t0,0x1e			#  merge what will be bits 17:13
	sll	t0,13			#  and shift back into proper position
	srl	t1,a1,PNUMSHFT-2	# move bits 24:14 ...
	andi	t1,0x1ffc		#  ... to 12:2 and clear 2 lsbits
	add	t0,t1			# effective addr for s-cache line

	la	t1,tlb_shadow		# form the address of the doubleword
	sll	t5,t2,3			#  which describes the address/PID
	add	t1,t5			#   for the target tlb line
	la	t4,tlb_lrf		# form the address of the
	add	t4,t2			#  TLB Least Recently Filled entry

	/*
	 *  The u-area KLUDGE -- causes tlb thrashing on the u-area
	 *  line, but we'll live with that for now
	 */
	bne	t2,0x3f,not_u		# u-area line == 0x3f?
	li	t5,SCACHE_UAREA_ADDR & ~0x7f # yes...eff addr of u-area tlb line
	and	t3,t0,t5
	bne	t3,t5,u_line_1		# is that what we're writing?

	/*
	 *  Using u-area line, side 0.  Does what's now there match the vpn
	 *  and pid of what we want to write, or do we need to rewrite the
	 *  entire tlb line?
	 */
	lw	t3,(t1)			# load address/PID of what's there
	beq	t3,t9,hit_0		# hits side-0, so write one word
	li	t2,0			#  otherwise
	b	ok_u			#   rewrite entire line

u_line_1:
	/*
	 *  Using u-area line, side 1.  Does what's now there match the vpn
	 *  and pid of what we want to write, or do we need to rewrite the
	 *  entire tlb line?
	 */
	lw	t3,4(t1)		# load address/PID of what's there
	beq	t3,t9,hit_1		# hits side-1, so write one word
	li	t2,1			#  otherwise
	b	ok_u			#   rewrite entire line

not_u:	lw	t3,(t1)			# load address/PID of what's at side-0
	lw	t2,4(t1)		#  and at side-1
	beq	t3,t9,hit_0		# ...branch if matches on side-0
	beq	t2,t9,hit_1		# 		       or side-1

	/*
	 *  Neither tlb side matches.  Pick the Least Recently Filled side
	 *  to clobber, update the LRF table, zero the entire line, and 
	 *  finally stuff the single new tlb entry.
	 *  At this point:  t0  effective address for the upcoming SCACHE
	 *		    t1  address of the LRF table entry
	 */

	lb	t2,(t4)			# least recently filled side to use
ok_u:	xori	t3,t2,1			#  toggle the LRF value
	sb	t3,(t4)			#  and store back into LRF table
	and	t4,t0,0xffffff80	# addr of first entry to zero
	add	t5,t4,24*4		# addr of loop end
	bgtz	t2,clr_1		# branch if use side-1...

	sw	t9,(t1)			# remember what's at this line, side-0
	.set	noreorder
1:	scache	zero, 0(t4)		# invalidate (zero) entire tlb line
	scache	zero, 4(t4)
	scache	zero, 8(t4)
	scache	zero,12(t4)
	scache	zero,16(t4)
	scache	zero,20(t4)
	scache	zero,24(t4)
	scache	zero,28(t4)
	bne	t4,t5,1b		# done with line?
	addi	t4,8*4			# (BDSLOT)  increment address

	b	dropin_0
	nop				# (BDSLOT)

hit_0:	/*  matches side-0 tlb line  */
	li	t5,1			# update the TLB LRF to use side-1 next
	sb	t5,(t4)
dropin_0:
	bne	t8,zero,g_0		# Global?
	nop				# (BDSLOT)
 	scache	a2,(t0)			# stuff tlb as PID
	b	exit			# all done
	nop				# (BDSLOT)

g_0:	scache	a2,2(t0)		# stuff tlb as Global
	b	exit			# all done
	nop				# (BDSLOT)

clr_1:	sw	t9,4(t1)		# remember what's at this line, side-1
1:	scache	zero, 1(t4)		# invalidate (zero) entire tlb line
	scache	zero, 5(t4)
	scache	zero, 9(t4)
	scache	zero,13(t4)
	scache	zero,17(t4)
	scache	zero,21(t4)
	scache	zero,25(t4)
	scache	zero,29(t4)
	bne	t4,t5,1b		# done with line?
	addi	t4,8*4			# (BDSLOT) increment address

	b	dropin_1
	nop				# (BDSLOT)

hit_1:	/*  matches side-1 tlb line  */
	sb	zero,(t4)		# update tlb lrf to use side-0 next
dropin_1:
	bne	t8,zero,g_1		# Global?
	nop				# (BDSLOT)
	scache	a2,1(t0)		# stuff tlb as PID
	b	exit			# all done
	nop				# (BDSLOT)

g_1:	scache	a2,3(t0)		# stuff tlb as Global
	nop				# (don't be changing PID right away)

exit:	WRITE_PID(t7,k0,k1,v0)		# reset PID 
	mtc0	t6,C0_SR		# restore SR
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(tlbdropin)

/*
 * flush entire tlb -- invalidate both primary caches, the data portion of
 * the secondary cache, and the tlb portion of the secondary cache.  Also,
 * reset the Least Recently Filled table to start using side-0.
 *
 * The secondary cache is divided up into 32 8KB "chunks".  We invalidate the
 * first 31 "chunks", and leave the last "chunk" -- the one containing the
 * ptags -- alone.
 *
 * Finally, we restore the u-area tlb entry.
 */
LEAF(flush_tlb)
 	.set	noreorder
	mfc0	t7,C0_SR		# save current SR
	li	t6,SR_MM_MODE		# disable interrupts
	mtc0	t6,C0_SR		#  and enable MM Mode

	/*
	 *   Invalidate i-cache and part of s-cache side-0
	 */
	lw	t1,icache_size		# init ending address
	li	t0,0			# init starting address
	subu	t1,8*R6000_ILINE_BSIZE	# form ending address for loop
2:	inval	0*R6000_ILINE_BSIZE(t0)	# invalidate 8 lines of i-cache
	inval	1*R6000_ILINE_BSIZE(t0)	#      and   2 lines of s-cache side-0
	inval	2*R6000_ILINE_BSIZE(t0)
	inval	3*R6000_ILINE_BSIZE(t0)
	inval	4*R6000_ILINE_BSIZE(t0)
	inval	5*R6000_ILINE_BSIZE(t0)
	inval	6*R6000_ILINE_BSIZE(t0)
	inval	7*R6000_ILINE_BSIZE(t0)
	bne	t0,t1,2b		# all done?
	addi	t0,8*R6000_ILINE_BSIZE	# (BDSLOT) increment address

	/*
	 *  Invalidate rest of s-cache side-0
	 */
	lw	t1,scache_size		# init ending address
	li	t2,0x3e000		# (LDSLOT) mask ending address
	subu	t1,1			# 
	and	t1,t2			#  to leave ptags "chunk" alone
	subu	t1,8*R6000_SLINE_BSIZE	# form ending address for loop
	move	t2,t1			#  and save for when we do side-1
3:	inval	0*R6000_SLINE_BSIZE(t0)	# invalidate 8 lines of s-cache side-0
	inval	1*R6000_SLINE_BSIZE(t0)
	inval	2*R6000_SLINE_BSIZE(t0)
	inval	3*R6000_SLINE_BSIZE(t0)
	inval	4*R6000_SLINE_BSIZE(t0)
	inval	5*R6000_SLINE_BSIZE(t0)
	inval	6*R6000_SLINE_BSIZE(t0)
	inval	7*R6000_SLINE_BSIZE(t0)
	bne	t0,t1,3b		# all done?
	addi	t0,8*R6000_SLINE_BSIZE	# (BDSLOT) increment address

	/*
	 *  Invalidate d-cache and part of s-cache side-1
	 */
	lw	t1,dcache_size		# init ending address
	li	t0,0			# init starting address
	subu	t1,8*R6000_DLINE_BSIZE	# form ending address for loop
4:	inval	0*R6000_DLINE_BSIZE+1(t0) # invalidate 8 lines of d-cache
	inval	1*R6000_DLINE_BSIZE+1(t0) #  and maybe 1 line of s-cache side-1
	inval	2*R6000_DLINE_BSIZE+1(t0)
	inval	3*R6000_DLINE_BSIZE+1(t0)
	inval	4*R6000_DLINE_BSIZE+1(t0)
	inval	5*R6000_DLINE_BSIZE+1(t0)
	inval	6*R6000_DLINE_BSIZE+1(t0)
	inval	7*R6000_DLINE_BSIZE+1(t0)
	bne	t0,t1,4b		# all done?
	addi	t0,8*R6000_DLINE_BSIZE	# (BDSLOT) increment address

	/*
	 *  Invalidate rest of s-cache side-1
	 */
5:	inval	0*R6000_SLINE_BSIZE+1(t0) # invalidate 8 lines of s-cache side-1
	inval	1*R6000_SLINE_BSIZE+1(t0)
	inval	2*R6000_SLINE_BSIZE+1(t0)
	inval	3*R6000_SLINE_BSIZE+1(t0)
	inval	4*R6000_SLINE_BSIZE+1(t0)
	inval	5*R6000_SLINE_BSIZE+1(t0)
	inval	6*R6000_SLINE_BSIZE+1(t0)
	inval	7*R6000_SLINE_BSIZE+1(t0)
	bne	t0,t2,5b		# all done?
	addi	t0,8*R6000_SLINE_BSIZE	# (BDSLOT) increment address

	/*
	 *  Restore the u-area tlb
	 *
	 *  NOTE:  we depend upon the fact here that the U-area is 0xFFFFC000
	 *	   and it's locked into side-0
	 */
	li	t0,SCACHE_UAREA_ADDR
	lcache	t1,(t0)
	nop				# (LDSLOT 1)
	nop				# (LDSLOT 2)
	scache	t1,(t0)			# rewrite, to rewrite virtual tag
	nop				# (give it time to settle)
	.set	reorder

	/*
	 *  Clear the TLB shadow table -- except leave u-area entry alone
	 */
	la	a0,tlb_shadow
	add	a1,a0,4*2*64-32		# loop-ending address
	add	t1,a0,4*2*63		# addr of u-area entry
	lw	t0,(t1)			# remember current u-area entry
	.set	noreorder
1:	sw	zero,(a0)
	sw	zero,4(a0)
	sw	zero,8(a0)
	sw	zero,12(a0)
	sw	zero,16(a0)
	sw	zero,20(a0)
	sw	zero,24(a0)
	sw	zero,28(a0)
	bne	a0,a1,1b
	addi	a0,32
	.set	reorder
	sw	t0,(t1)			# restore current u-area entry

	.set	noreorder
	mtc0	t7,C0_SR		# restore SR
	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(flush_tlb)

#ifdef R6000_DEBUG_SCACHE
BSS(scache_snapshot_made,4)
BSS(scache_side0_image,256*1024)
BSS(scache_side1_image,256*1024)

LEAF(make_scache_snapshot)
	lw	t0,scache_snapshot_made
	bne	t0,zero,2f
	.set	noreorder
	mfc0	v0,C0_SR
	li	t4,SR_MM_MODE
	mtc0	t4,C0_SR
	.set	reorder
	li	t0,0
	li	t1,0x40000
	li	v1,K1BASE
	la	t2,scache_side0_image
	or	t2,v1			# store using K1 in order to avoid
	la	t3,scache_side1_image	#  blowing away the contents of the
	or	t3,v1			#   S-cache while saving its contents
	.set	noreorder
1:	lcache	t5,(t0)			# save s-cache state
	lcache	t6,1(t0)
	addi	t0,4
	sw	t5,(t2)
	addi	t2,4
	sw	t6,(t3)
	bne	t0,t1,1b
	addi	t3,4			# (BDSLOT)

	mtc0	v0,C0_SR

2:	.set	reorder
	lw	t0,scache_snapshot_made
	addi	t0,1
	sw	t0,scache_snapshot_made
	j	ra
	END(make_scache_snapshot)
#endif R6000_DEBUG_SCACHE

/*
 * set_tlbpid( new_pid )
 * a0 -- new_pid -- tlbcontext number to use (0-255)
 *
 * The PID value for the currently executing process has changed, so we
 * need to change C0_PID, then rewrite the tlb entry for the u-area.
 *
 * We could use tlbdropin(), but this is not particularly efficient:  not only
 * does that routine muck around a bit before getting to the SCACHE, but it
 * also finds it necessary to clear all 32 words of the U-area tlb line.  That
 * really isn't necessary in this case, since all we're doing is rewriting the
 * same U-area tlb entry to get a new PID in the virtual tag.
 *
 * NOTE:  we are assuming that the U-area is 0xFFFFC000, and that the tlb entry
 *	  is locked into side-0.
 */
LEAF(set_tlbpid)
	.set	noreorder
	mfc0	t7,C0_SR
	li	t0,SR_MM_MODE		# enable MM mode,
	mtc0	t0,C0_SR		#  and disable interrupts
	nop				# buffer changing PID with nop's
	WRITE_PID(a0,k0,k1,v0)		# reset PID 
	li	t0,SCACHE_UAREA_ADDR
	lcache	t1,(t0)			# current U-area tlb entry
	nop				# (LDSLOT 1)
	nop				# (LDSLOT 2)
	scache	t1,(t0)			# rewrite with new PID
	nop				# (give it time to settle)
	nop
	la	t0,tlb_shadow		# update the PID in the
	sb	a0,4*63*2(t0)		#  tlb_shadow[] array
	mtc0	t7,C0_SR		# restore SR
	j	ra
	nop				# (BDSLOT)
	.set	reorder
	END(set_tlbpid)

#ifdef R6000_DEBUG_UAREA
/*
 *  Verify that the U-area tlb entry is alive and well, else PANIC.
 */
LEAF(verify_uarea_tlb)
	.set	noreorder
	mfc0	a1,C0_SR
	li	a2,SR_MM_MODE
	mtc0	a2,C0_SR		# enable MM Mode

	lw	k0,last_uarea_pte
	nop
	beq	k0,zero,6f		# if no Uarea, then exit
	
	li	a3,SCACHE_UAREA_ADDR	# eff addr of u-area tlb entry
	lcache	a2,(a3)			# retrieve the tlb entry
	nop
	/*
	 *  Is the tlb entry what we expect?
	 */
	beq	k0,a2,1f		# tlb entries match?
	nop
	/* NO MATCH! */
	scache	k0,(a3)			# restuff a valid uarea tlb
	mfc0	k1,C0_PID
	nop
	mtc0	k1,C0_PID
	mtc0	k1,C0_PID
	mtc0	a1,C0_SR		# restore SR
	.set	reorder
	move	a3,a2
	move	a2,k0
	PANIC("Invalid U-area tlb entry:  expect %x  actual %x")
	.set	noreorder

1:	/*
	 *  Is the virtual tag correct?
	 */
	mfc0	k1,C0_SR
	nop
	and	k1,SR_CM0
	beq	k1,zero,6f		# expect not-Miss
	nop
	/* MISS */
	scache	k0,(a3)			# restuff a valid uarea tlb
	mfc0	k1,C0_PID
	nop
	mtc0	k1,C0_PID
	mtc0	k1,C0_PID
	mtc0	a1,C0_SR		# restore SR
	.set	reorder
	PANIC("U-area tlb entry gets vtag mismatch")
	.set	noreorder

6:	j	ra
	mtc0	a1,C0_SR		# (BDSLOT) restore SR

	END(verify_uarea_tlb)
#endif R6000_DEBUG_UAREA

#endif R6000
