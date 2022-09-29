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
/* $Header: usercopy.s,v 1.28.1.11.1.1.1.4 90/12/20 18:22:55 beacker Exp $ */

/* set to use as2.10 */
#define	NOMOVE	noreorder
#define	MOVE	reorder
#define	NOP	nop

/*
 * copyin(user_src, kernel_dst, bcount)
 *
 * NOTE:  unlike badaddr() and friends, this routine sets "nofault" without
 *  disabling interrupts (because the underlying bcopy() may be time-consuming).
 *  The danger here is that bcopy() may be interrupted, and the interrupting
 *  routine may do something which gets us into trap() (e.g., bus error, address
 *  error), and then into k_trap().  If this happens, then k_trap() will notice
 *  a nonzero "nofault", and will change the EPC in the stack frame to be
 *  cerror(), then graceful exit for copyin/copyout() errors.  But we're now
 *  one level deeper on the stack!  This will be very bad news when cerror()
 *  pops a bogus ra off the stack and tries to exit.
 *  We normally don't see this problem because of the low probability that we
 *  get interrupted out of bcopy() *and* the interrupting routine generates
 *  a trap()-like exception.
 */
COPYIOFRM=	(4*4)+4			# 4 arg saves plus ra
NESTED(copyin, COPYIOFRM, zero)
	subu	sp,COPYIOFRM
	sw	ra,COPYIOFRM-4(sp)
	bltz	a0,1f
2:	li	v0,NF_COPYIO
	.set	NOMOVE
	sw	v0,nofault
	jal	bcopy
	NOP
	sw	zero,nofault
	.set	MOVE
	move	v0,zero
	lw	ra,COPYIOFRM-4(sp)
	addu	sp,COPYIOFRM
	j	ra

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	cerror
	END(copyin)

/*
 * copyout(kernel_src, user_dst, bcount)
 *
 * NOTE:  see the comments for copyin().
 */
NESTED(copyout, COPYIOFRM, zero)
	subu	sp,COPYIOFRM
	sw	ra,COPYIOFRM-4(sp)
	bltz	a1,1f
2:	li	v0,NF_COPYIO
	.set	NOMOVE
	sw	v0,nofault
	jal	bcopy
	NOP
	sw	zero,nofault
	.set	MOVE
	move	v0,zero
	lw	ra,COPYIOFRM-4(sp)
	addu	sp,COPYIOFRM
	j	ra

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	cerror
	END(copyout)

NESTED(cerror, COPYIOFRM, zero)
	li	v0,-1
	lw	ra,COPYIOFRM-4(sp)
	addu	sp,COPYIOFRM
	j	ra
	END(cerror)

/*
 * bcopy(src, dst, bcount)
 *
 * Preface to the second edition (including M/120 and M/2000) by Charlie Price:
 * Memory copy becomes more complicated with the multi-word-refill
 * cache on the M/2000 and the fast memory system on the M/120.
 * Substituting measurement for intuition showed that a change in
 * the block copy algorithm was a good idea for ALL machines.
 * The main change was from blocks of 4 lw and 4 sw
 * (matched to write-buffer depth) to interleaved lw/sw instructions.
 * Increasing the block size from 32 to 64 bytes is a win on all
 * systems for large block transfers.  For the M/2000 it works
 * well for cache-line aligned transfers from cached space --
 * an especially important case with disk buffers put in cached space.
 * The calculations of the first edition are all for M-boxes (M-500!)
 * and not for M/120 or M/2000.
 * Test results of large block copies in MBytes/sec.
 *                                                M/1000   M/120   M/2000
 * - original kernel algorithm (32-byte blkcopy)    3.60    6.86    19.5
 * - 32-byte loop with interleaved lw/sw            3.73    7.30    20.1
 * - 64-byte loop with interleaved lw/sw            3.77    7.44    21.0
 *
 * There is lots of opportunity to make this faster (especially for
 * multi-line-refill caches).
 * Future *measurements* should be made to determine the value of
 * such performance work.
 *
 * Preface to the first edition (M-boxes):
 * NOTE: the optimal copy here is somewhat different than for the user-level
 * equivalents (bcopy in 4.2, memcpy in V), because:
 * 1) it frequently acts on uncached data, especially since copying from
 * (uncached) disk buffers into user pgms is high runner.
 * This means one must be careful with lwl/lwr/lb - don't expect cache help.
 * 2) the distribution of usage is very different: there are a large number
 * of bcopies for small, aligned structures (like for ioctl, for example),
 * a reasonable number of randomly-sized copies for user I/O, and many
 * bcopies of large (page-size) blocks for stdio; the latter must be
 * well-tuned, hence the use of 32-byte loops.
 * 3) this is much more frequently-used code inside the kernel than outside
 *
 * [NOTE: following for early MIPS M-boxes -- probably M/500]
 * Overall copy-loop speeds, by amount of loop-unrolling: assumptions:
 * a) low icache miss rate (this code gets used a bunch)
 * b) large transfers, especially, will be word-alignable.
 * c) Copying speeds (steady state, 0% I-cache-miss, 100% D-cache Miss):
 * d) 100% D-Cache Miss (but cacheable, so that lwl/lwr/lb work well)
 *	Config	Bytes/	Cycles/	Speed (VAX/780 = 1)
 *		Loop	Word
 *	08V11	1	35	0.71X	(8MHz, VME, 1-Deep WB, 1-way ILV)
 *		4	15	1.67X
 *		8/16	13.5	1.85X
 *		32/up	13.25	1.89X
 *	08MM44	1	26	0.96X	(8MHz, MEM, 4-Deep WB, 4-way ILV)
 *		4	9	2.78X
 *		8	7.5	3.33X
 *		16	6.75	3.70X
 *		32	6.375	3.92X	(diminishing returns thereafter)
 *
 * MINCOPY is minimum number of byte that its worthwhile to try and
 * align copy into word transactions.  Calculations below are for 8 bytes:
 * Estimating MINCOPY (C = Cacheable, NC = Noncacheable):
 * Assumes 100% D-cache miss on first reference, then 0% (100%) for C (NC):
 * (Warning: these are gross numbers, and the code has changed slightly):
 *	Case		08V11			08M44
 *	MINCOPY		C	NC		C	NC
 *	9 (1 byte loop)	75	133		57	93
 *	8 (complex logic)
 *	Aligned		51	51		40	40
 *	Alignable,
 *	worst (1+4+3)	69	96		53	80
 *	Unalignable	66	93		60	72
 * MINCOPY should be lower for lower cache miss rates, lower cache miss
 * penalties, better alignment properties, or if src and dst alias in
 * cache. For this particular case, it seems very important to minimize the
 * number of lb/sb pairs: a) frequent non-cacheable references are used,
 * b) when i-cache miss rate approaches zero, even the 4-deep WB can't
 * put successive sb's together in any useful way, so few references are saved.
 * To summarize, even as low as 8 bytes, avoiding the single-byte loop seems
 * worthwhile; some assumptions are probably optimistic, so there is not quite
 * as much disadvantage.  However, the optimal number is almost certainly in
 * the range 7-12.
 *
 *	a0	src addr
 *	a1	dst addr
 *	a2	length remaining
 */
#define	MINCOPY	8

LEAF(bcopy)
/*
 * Someday maybe call ovbopy explicitly in code.
 * For now just check here.
 */
	bgeu	a0,a1,1f		# src >= dst, no overlap error
	addu	v0,a0,a2		# src endpoint + 1
	bgeu	a1,v0,1f		# dst >= src endpoint+1, no overlap err
	j	ovbcopy			# handle overlap; returns when done
1:
	xor	v0,a0,a1		# bash src & dst for align chk; BDSLOT
	blt	a2,MINCOPY,bytecopy	# too short, just byte copy
	and	v0,NBPW-1		# low-order bits for align chk
	subu	v1,zero,a0		# -src; BDSLOT
	bne	v0,zero,unaligncopy	# src and dst not alignable
/*
 * src and dst can be simultaneously word aligned
 */
	and	v1,NBPW-1		# number of bytes til aligned
	subu	a2,v1			# bcount -= alignment
	beq	v1,zero,blkcopy		# already aligned
#ifdef MIPSEB
	lwl	v0,0(a0)		# copy unaligned portion
	swl	v0,0(a1)
#endif
#ifdef MIPSEL
	lwr	v0,0(a0)
	swr	v0,0(a1)
#endif
	addu	a0,v1			# src += alignment
	addu	a1,v1			# dst += alignment

/*
 * 64 byte block, aligned copy loop (for big reads/writes)
 * This interleaved lw/sw is better (by measurement) than a blocky
 * 4*lw/4*sw strategy.
 */
blkcopy:
	and	a3,a2,~63		# total space in 64 byte chunks
	subu	a2,a3			# count after by-64 byte loop done
	beq	a3,zero,wordcopy	# less than 64 bytes to copy
	addu	a3,a0			# source endpoint
#ifdef R6000
	/*
	 *  The R6000 cache implementation is substantially more efficient
	 *  for doubleword accesses than for singleword:  singleword causes a
	 *  D-cache miss on the 2nd word in the line, then the subsequent store
	 *  replaces that word.  A doubleword store just fill the whole D-cache
	 *  line and writes-through to the S-cache.  A doubleword load pulls
	 *  both words out of the S-cache, one cycle apart.
	 *  Keeping this in mind, for a relatively large number of bytes we
	 *  go though some effort to set up for doubleword accesses (using CP1).
	 *  Since the source and target must be cached, we can do this for K0,
	 *  but not for K1, and we go to the trouble of examining the pte for a
	 *  K2 address.  For now, we won't go to the trouble of doing Ku pte
	 *  lookups.
	 *
	 *  If the source/target address isn't doubleword-aligned, we just use
	 *  the normal singleword algorithm.  This is a future enhancement,
	 *  but most interesting uses of bcopy() use doubleword-aligned 
	 *  addresses.
	 */
	lw	t2,nofault		# if expecting some kind of trap
	bne	t2,zero,nonfp_copy	#  then use the normal algorithm
	addi	t0,a0,1024		# if length is short,
	sltu	t1,a3,t0		#  then use the normal algorithm
	bne	t1,zero,nonfp_copy
	lw	t2,fptype_word		# if there's no FPC,
	beq	t2,zero,nonfp_copy	#  then use the normal algorithm

	move	v0,zero			# tentatively indicate that source
	move	v1,zero			#  and target need singleword accesses

	andi	t2,a0,0x7		# if source addr not aligned on
#ifdef R6000_BUG_LWC1_NONCACHED
	bne	t2,zero,nonfp_copy	#  doubleword, then use non-FP algorithm
#else
	bne	t2,zero,check_target	#  doubleword, then use singleword
#endif
	li	t1,K0BASE
	sltu	t0,a0,t1		# if source is a Ku address
	bne	t0,zero,check_target	#  then use singleword
	li	t2,K1BASE
	sltu	v0,a0,t2		# if source is a K0 address
	bne	v0,zero,check_target	#  then use the doubleword algorithm
	li	t0,K2BASE
	sltu	t2,a0,t0		# if source is a K1 address
#ifdef R6000_BUG_LWC1_NONCACHED
	bne	t2,zero,nonfp_copy	#  then use non-FP algorithm
#else
	bne	t2,zero,check_target	#  then use singleword
#endif
	lw	t1,kptbl		#  else check K2 pte for "noncached"
	sub	t2,a0,t0
	srl	t2,PNUMSHFT		#    page number offset in K2
	sll	t2,3			#    byte offset of pte
	add	t1,t2			#    address of pte
	lw	t0,(t1)			#    pte
	li	t2,PG_N
	and	t2,t0			# and if K2 page is noncached,
#ifdef R6000_BUG_LWC1_NONCACHED
	bne	t2,zero,nonfp_copy	#  then use the non-FP algorithm
#else
	bne	t2,zero,check_target	#  then use singleword
#endif
	/* else source can use doubleword */
	li	v0,1			# indicate that source can use LDC1
check_target:
	andi	t2,a1,0x7		# if target addr not aligned on
	bne	t2,zero,check_both	#  doubleword, then use singleword
	li	t1,K0BASE
	sltu	t0,a1,t1		# if target is a Ku address
	bne	t0,zero,check_both	#  then use singleword
	li	t2,K1BASE
	sltu	v1,a1,t2		# if target is a K0 address
	bne	v1,zero,check_both	#  then use the doubleword algorithm
	li	t0,K2BASE
	sltu	t2,a1,t0		# if source is a K1 address
	bne	t2,zero,check_both	#  then use singleword
	lw	t1,kptbl		#  else check K2 pte for "noncached"
	sub	t2,a1,t0
	srl	t2,PNUMSHFT		#    page number offset in K2
	sll	t2,3			#    byte offset of pte
	add	t1,t2			#    address of pte
	lw	t0,(t1)			#    pte
	li	t2,PG_N
	and	t2,t0			# and if K2 page is noncached,
	bne	t2,zero,check_both	#  then use singleword
	/* else target can use doubleword */
	li	v1,1			# indicate that target can use LDC1
check_both:
	or	t0,v0,v1		# if neither source nor target can use
	beq	t0,zero,nonfp_copy	#  doubleword, then use normal algorithm
	.set	noreorder
	mfc0	t8,C0_SR
	li	t0,SR_CU1
	MTC0(t0)			# enable Cp1, disable interrupts
	cfc1	t9,fpc_parity		# remember fp parity error reg contents
	li	t0,(FPPARITY_IRF | FPPARITY_IIB)
	ctc1	t0,fpc_parity		# disable fp parity errors
	nop
	nop
	mfc1	t0,$f0			# save Cp1 registers
	mfc1	t1,$f1
	mfc1	t2,$f2
	mfc1	t3,$f3
	mfc1	t4,$f4
	mfc1	t5,$f5
	mfc1	t6,$f6
	mfc1	t7,$f7

	/* main loop */
6:	bne	v0,zero,7f
	nop
	lwc1	$f1,0(a0)		# load singlewords
	lwc1	$f0,4(a0)
	lwc1	$f3,8(a0)
	lwc1	$f2,12(a0)
	lwc1	$f5,16(a0)
	lwc1	$f4,20(a0)
	lwc1	$f7,24(a0)
	b	8f
	lwc1	$f6,28(a0)		# (BDSLOT)

7:	ldc1	$f0,0(a0)		# load doublewords
	ldc1	$f2,8(a0)
	ldc1	$f4,16(a0)
	ldc1	$f6,24(a0)

8:	bne	v1,zero,9f
	nop
	swc1	$f1,0(a1)		# store singlewords
	swc1	$f0,4(a1)
	swc1	$f3,8(a1)
	swc1	$f2,12(a1)
	swc1	$f5,16(a1)
	swc1	$f4,20(a1)
	swc1	$f7,24(a1)
	b	10f
	swc1	$f6,28(a1)		# (BDSLOT)

9:	sdc1	$f0,0(a1)		# store doublewords
	sdc1	$f2,8(a1)
	sdc1	$f4,16(a1)
	sdc1	$f6,24(a1)

10:	addu	a0,32
	bne	a0,a3,6b
	addu	a1,32			# (BDSLOT)

	mtc1	t0,$f0			# restore Cp1 registers
	mtc1	t1,$f1
	mtc1	t2,$f2
	mtc1	t3,$f3
	mtc1	t4,$f4
	mtc1	t5,$f5
	mtc1	t6,$f6
	mtc1	t7,$f7
	ctc1	t9,fpc_parity		# restore fp parity error state
	MTC0(t8)			# (BDSLOT) disable Cp1
	b	wordcopy
	nop

nonfp_copy:
	/*
	 * We're not using the FP and its doubleword instructions, so
	 * we instead use a typical load-store algorithm.  We could use
	 * the R2000/3000 algorithm, below, but that one is optimized for
	 * the Write Buffers; and, in fact, that algorithm can occasionally
	 * be worst-case for the R6000 because of tlbslice misses.  In the
	 * end, the most straightforward algorithm is the optimum one:
	 * do a bunch of Loads, then do a bunch of Stores.
	 */
	lw	t0,0(a0)
	lw	t1,4(a0)
	lw	t2,8(a0)
	lw	t3,12(a0)
	lw	t4,16(a0)
	lw	t5,20(a0)
	lw	t6,24(a0)
	lw	t7,28(a0)
	sw	t0,0(a1)
	sw	t1,4(a1)
	sw	t2,8(a1)
	sw	t3,12(a1)
	sw	t4,16(a1)
	sw	t5,20(a1)
	sw	t6,24(a1)
	sw	t7,28(a1)
	lw	t0,32(a0)
	lw	t1,36(a0)
	lw	t2,40(a0)
	lw	t3,44(a0)
	lw	t4,48(a0)
	lw	t5,52(a0)
	lw	t6,56(a0)
	lw	t7,60(a0)
	sw	t0,32(a1)
	sw	t1,36(a1)
	sw	t2,40(a1)
	sw	t3,44(a1)
	sw	t4,48(a1)
	sw	t5,52(a1)
	sw	t6,56(a1)
	sw	t7,60(a1)
	addu	a0,64
	bne	a0,a3,nonfp_copy
	 addu	a1,64			# dst+= 64; fills BD slot

	j	wordcopy
	nop
	.set	reorder
#endif R6000

	/*
	* This unrolled loop is fastest when all of the
	* writes go to memory without any intervening
	* loads from memory.  If you manage this, the
	* memory systems retire the writes to the same
	* DRAM page without re-selecting chips
	* (for existing products, every other cycles).
	* The R3200 normally has a 16-word line size so
	* a 16-word loop works.
	* The R3030 has the same faster write mode,
	* but has an 8-word line size.
	* To increase the R3030 copy speed by ~7%,
	* cache-miss in both lines by reading the
	* correct two words at the front of the loop
	* before starting the writes.
	* This is slightly faster for the M/180 as well,
	* and doesn't change the R3200 speed.
	*/
	.set noreorder
1:	lw	v0,0(a0)
	lw	t2,32(a0)	# 1st wd of 2nd cache line if 8-wd lines.
	lw	v1,4(a0)
	sw	v0,0(a1)
	lw	t0,8(a0)
	sw	v1,4(a1)
	lw	t1,12(a0)
	sw	t0,8(a1)
	lw	v0,16(a0)
	sw	t1,12(a1)
	lw	v1,20(a0)
	sw	v0,16(a1)
	lw	t0,24(a0)
	sw	v1,20(a1)
	lw	t1,28(a0)
	sw	t0,24(a1)
	#	lw for offset 32 done at top of loop
	sw	t1,28(a1)
	lw	v1,36(a0)
	sw	t2,32(a1)
	lw	t0,40(a0)
	sw	v1,36(a1)
	lw	t1,44(a0)
	sw	t0,40(a1)
	lw	v0,48(a0)
	sw	t1,44(a1)
	lw	v1,52(a0)
	sw	v0,48(a1)
	lw	t0,56(a0)
	sw	v1,52(a1)
	lw	t1,60(a0)
	sw	t0,56(a1)
	sw	t1,60(a1)
	addu	a0,64			# src+= 64; here to ease loop end
	bne	a0,a3,1b
	 addu	a1,64			# dst+= 64; fills BD slot
	.set reorder

/*
 * word copy loop
 */
wordcopy:
	and	a3,a2,~(NBPW-1)		# word chunks
	subu	a2,a3			# count after by word loop
	beq	a3,zero,bytecopy	# less than a word to copy
	addu	a3,a0			# source endpoint
1:	lw	v0,0(a0)
	addu	a0,NBPW
	sw	v0,0(a1)
	addu	a1,NBPW			# dst += 4; BD slot
	bne	a0,a3,1b
	b	bytecopy

/*
 * deal with simultaneously unalignable copy by aligning dst
 */
unaligncopy:
	subu	a3,zero,a1		# calc byte cnt to get dst aligned
	and	a3,NBPW-1		# alignment = 0..3
	subu	a2,a3			# bcount -= alignment
	beq	a3,zero,partaligncopy	# already aligned
#ifdef MIPSEB
	lwl	v0,0(a0)		# get whole word
	lwr	v0,3(a0)		# for sure
	swl	v0,0(a1)		# store left piece (1-3 bytes)
#endif
#ifdef MIPSEL
	lwr	v0,0(a0)		# get whole word
	lwl	v0,3(a0)		# for sure
	swr	v0,0(a1)		# store right piece (1-3 bytes)
#endif
	addu	a0,a3			# src += alignment (will fill LD slot)
	addu	a1,a3			# dst += alignment

/*
 * src unaligned, dst aligned loop
 * NOTE: if MINCOPY >= 7, will always do 1 loop iteration or more
 * if we get here at all
 */
partaligncopy:
	and	a3,a2,~(NBPW-1)		# space in word chunks
	subu	a2,a3			# count after by word loop
#if MINCOPY < 7
	beq	a3,zero,bytecopy	# less than a word to copy
#endif
	addu	a3,a0			# source endpoint
1:
#ifdef MIPSEB
	lwl	v0,0(a0)
	lwr	v0,3(a0)
#endif
#ifdef MIPSEL
	lwr	v0,0(a0)
	lwl	v0,3(a0)
#endif
	addu	a0,NBPW
	sw	v0,0(a1)
	addu	a1,NBPW
	bne	a0,a3,1b


/*
 * brute force byte copy loop, for bcount < MINCOPY + tail of unaligned dst
 * note that lwl, lwr, swr CANNOT be used for tail, since the lwr might
 * cross page boundary and give spurious address exception
 */
bytecopy:
	addu	a3,a2,a0		# source endpoint; BDSLOT
	ble	a2,zero,copydone	# nothing left to copy, or bad length
1:	lb	v0,0(a0)
	addu	a0,1
	sb	v0,0(a1)
	addu	a1,1			# BDSLOT: incr dst address
	bne	a0,a3,1b
copydone:
	j	ra
	END(bcopy)

/*
 * bzero(dst, bcount)
 * Zero block of memory
 *
 * Calculating MINZERO, assuming 50% cache-miss on non-loop code:
 * Overhead =~ 18 instructions => 63 (81) cycles
 * Byte zero =~ 16 (24) cycles/word for 08M44 (08V11)
 * Word zero =~ 3 (6) cycles/word for 08M44 (08V11)
 * If I-cache-miss nears 0, MINZERO ==> 4 bytes; otherwise, times are:
 * breakeven (MEM) = 63 / (16 - 3) =~ 5 words
 * breakeven (VME) = 81 / (24 - 6)  =~ 4.5 words
 * Since the overhead is pessimistic (worst-case alignment), and many calls
 * will be for well-aligned data, and since Word-zeroing at least leaves
 * the zero in the cache, we shade these values (18-20) down to 12
 */
#define	MINZERO	12
LEAF(bzero)
XLEAF(blkclr)
	subu	v1,zero,a0		# number of bytes til aligned
	blt	a1,MINZERO,bytezero
	and	v1,NBPW-1
	subu	a1,v1
	beq	v1,zero,blkzero		# already aligned
#ifdef MIPSEB
	swl	zero,0(a0)
#endif
#ifdef	MIPSEL
	swr	zero,0(a0)
#endif
	addu	a0,v1

/*
 * zero 32 byte, aligned block
 */
blkzero:
	and	a3,a1,~31		# 32 byte chunks
	subu	a1,a3
	beq	a3,zero,wordzero
	addu	a3,a0			# dst endpoint
#ifdef R6000
	/*
	 *  The R6000 cache implementation is substantially more efficient
	 *  for doubleword stores than for singleword stores for a mass of
	 *  back-to-back stores:  a singleword store causes a D-cache miss on
	 *  the second word in the D-cache line, then the subsequent store
	 *  replaces that word.  A doubleword store just fill the whole D-cache
	 *  line and writes-through to the S-cache.
	 *  Keeping this in mind, for a relatively large number of bytes we
	 *  go though some effort to set up for doubleword stores (using CP1).
	 *  Since the target must be cached, we can do this for K0, but not
	 *  for K1, and we go to the trouble of examining the pte for a K2
	 *  address.  Since the kernel does few, if any, bzero() stores to
	 *  Ku space, we won't bother to add the pte lookup for Ku addresses.
	 *
	 *  If the target address isn't doubleword-aligned, we'll just use
	 *  the normal singleword algorithm.  This is a future enhancement,
	 *  but most interesting uses of bzero() are to doubleword-aligned
	 *  targets.
	 */
	lw	t2,nofault		# if expecting some kind of trap
	bne	t2,zero,1f		#  then use the normal algorithm
	addi	t0,a0,512		# if length is short,
	sltu	t1,a3,t0		#  then use the normal algorithm
	bne	t1,zero,1f
	andi	t2,a0,0x7		# if starting addr not aligned on
	bne	t2,zero,1f		#  doubleword, then use normal algorithm
	lw	t0,fptype_word		# if there's no FPC,
	beq	t0,zero,1f		#  then use the normal algorithm
	li	t1,K0BASE
	sltu	t0,a0,t1		# if we're zeroing a Ku address
	bne	t0,zero,1f		#  then use the normal algorithm
	li	t2,K1BASE
	sltu	t1,a0,t2		# if we're zeroing a K0 address
	bne	t1,zero,2f		#  then use the doubleword algorithm
	li	t0,K2BASE
	sltu	t2,a0,t0		# if we're zeroing a K1 address
	bne	t2,zero,1f		#  then use the normal algorithm
	lw	t1,kptbl		#  else check K2 pte for "noncached"
	sub	t2,a0,t0
	srl	t2,PNUMSHFT		#    page number offset in K2
	sll	t2,3			#    byte offset of pte
	add	t1,t2			#    address of pte
	lw	t0,(t1)			#    pte
	li	t2,PG_N
	and	t2,t0			# and if K2 page is noncached,
	bne	t2,zero,1f		#  then use the normal algorithm
2:
	.set	noreorder
	mfc0	t3,C0_SR
	li	t0,SR_CU1
	MTC0(t0)			# enable Cp1, disable interrupts
	cfc1	t9,fpc_parity		# remember fp parity error reg state
	li	t0,(FPPARITY_IRF | FPPARITY_IIB)
	ctc1	t0,fpc_parity		# disable fp parity errors
	nop
	nop
	mfc1	t0,$f0			# save Cp1 registers
	mfc1	t1,$f1
	mtc1	zero,$f0		# set up Cp1 registers with zeroes
	mtc1	zero,$f1
	subu	a3,32			# set up for end-of-loop test
6:	sdc1	$f0,0(a0)		# zero doubleword
	sdc1	$f0,8(a0)
	sdc1	$f0,16(a0)
	sdc1	$f0,24(a0)
	bne	a0,a3,6b
	addu	a0,32			# (BDSLOT)

	mtc1	t0,$f0			# restore Cp1 registers
	mtc1	t1,$f1
	ctc1	t9,fpc_parity		# restore fp parity error state
	MTC0(t3)			# (BDSLOT) disable Cp1
	b	wordzero
	nop
	.set	reorder
#endif R6000
1:	sw	zero,0(a0)
	sw	zero,4(a0)
	sw	zero,8(a0)
	sw	zero,12(a0)
	addu	a0,32
	sw	zero,-16(a0)
	sw	zero,-12(a0)
	sw	zero,-8(a0)
	sw	zero,-4(a0)
	bne	a0,a3,1b

wordzero:
	and	a3,a1,~(NBPW-1)		# word chunks
	subu	a1,a3
	beq	a3,zero,bytezero
	addu	a3,a0			# dst endpoint
1:	addu	a0,NBPW
	sw	zero,-NBPW(a0)
	bne	a0,a3,1b

bytezero:
	ble	a1,zero,zerodone
	addu	a1,a0			# dst endpoint
1:	addu	a0,1
	sb	zero,-1(a0)
	bne	a0,a1,1b
zerodone:
	j	ra
	END(bzero)

/*
 * bcmp(src, dst, bcount)
 *
 * MINCMP is minimum number of byte that its worthwhile to try and
 * align cmp into word transactions
 *
 * Calculating MINCMP
 * Overhead =~ 15 instructions => 90 cycles
 * Byte cmp =~ 38 cycles/word
 * Word cmp =~ 17 cycles/word
 * Breakeven =~ 16 bytes
 */
#define	MINCMP	16

LEAF(bcmp)
	xor	v0,a0,a1
	blt	a2,MINCMP,bytecmp	# too short, just byte cmp
	and	v0,NBPW-1
	subu	t8,zero,a0		# number of bytes til aligned
	bne	v0,zero,unalgncmp	# src and dst not alignable
/*
 * src and dst can be simultaneously word aligned
 */
	and	t8,NBPW-1
	subu	a2,t8
	beq	t8,zero,wordcmp		# already aligned
	move	a1,a0
#ifdef MIPSEB
	lwl	v0,0(a0)		# cmp unaligned portion
	lwl	v1,0(a1)
#endif
#ifdef MIPSEL
	lwr	v0,0(a0)
	lwr	v1,0(a1)
#endif
	addu	a0,t8
	addu	a1,t8
	bne	v0,v1,cmpne

/*
 * word cmp loop
 */
wordcmp:
	and	a3,a2,~(NBPW-1)
	subu	a2,a3
	beq	a3,zero,bytecmp
	addu	a3,a0				# src1 endpoint
1:	lw	v0,0(a0)
	lw	v1,0(a1)
	addu	a0,NBPW				# 1st BDSLOT
	addu	a1,NBPW				# 2nd BDSLOT (asm doesn't move)
	bne	v0,v1,cmpne
	bne	a0,a3,1b			# at least one more word
	b	bytecmp

/*
 * deal with simultaneously unalignable cmp by aligning one src
 */
unalgncmp:
	subu	a3,zero,a1		# calc byte cnt to get src2 aligned
	and	a3,NBPW-1
	subu	a2,a3
	beq	a3,zero,partaligncmp	# already aligned
	addu	a3,a0			# src1 endpoint
1:	lbu	v0,0(a0)
	lbu	v1,0(a1)
	addu	a0,1
	addu	a1,1
	bne	v0,v1,cmpne
	bne	a0,a3,1b

/*
 * src unaligned, dst aligned loop
 */
partaligncmp:
	and	a3,a2,~(NBPW-1)
	subu	a2,a3
	beq	a3,zero,bytecmp
	addu	a3,a0
1:
#ifdef MIPSEB
	lwl	v0,0(a0)
	lwr	v0,3(a0)
#endif
#ifdef MIPSEL
	lwr	v0,0(a0)
	lwl	v0,3(a0)
#endif
	lw	v1,0(a1)
	addu	a0,NBPW
	addu	a1,NBPW
	bne	v0,v1,cmpne
	bne	a0,a3,1b

/*
 * brute force byte cmp loop
 */
bytecmp:
	addu	a3,a2,a0			# src1 endpoint; BDSLOT
	ble	a2,zero,cmpdone
1:	lbu	v0,0(a0)
	lbu	v1,0(a1)
	addu	a0,1
	addu	a1,1
	bne	v0,v1,cmpne
	bne	a0,a3,1b
cmpdone:
	move	v0,zero	
	j	ra

cmpne:
	li	v0,1
	j	ra
	END(bcmp)

/*
 * addupc(pc, &u.u_prof, ticks)
 */
LEAF(addupc)
	lw	v1,PR_OFF-PR_BASE(a1)	# base of profile region
	subu	a0,v1			# corrected pc
	bltz	a0,1f			# below of profile region
	lw	v0,PR_SCALE-PR_BASE(a1)	# fixed point scale factor
	bne	v0,2,2f			# if scale == 2, only use 1st bucket
	li	v0,0
	b	3f
2:	multu	v0,a0
	mflo	v0			# shift 64 bit result right 16
	srl	v0,16
	mfhi	v1
	sll	v1,16
	or	v0,v1
	addu	v0,1			# round-up to even
	and	v0,~1
	lw	v1,PR_SIZE-PR_BASE(a1)
	bgeu	v0,v1,1f		# above profile region
3:	lw	v1,(a1)			# base of profile buckets
	addu	v0,v1
	bltz	v0,adderr		# outside kuseg

	li	v1,NF_ADDUPC
	.set	NOMOVE
	sw	v1,nofault
	lh	v1,0(v0)		# add ticks to bucket
	NOP
	addu	v1,a2
	sh	v1,0(v0)
	sw	zero,nofault
	.set	MOVE

1:	j	ra
	END(addupc)

LEAF(adderr)
	sw	zero,PR_SCALE-PR_BASE(a1)
	j	ra
	END(adderr)

LEAF(fubyte)
XLEAF(fuibyte)
	.set	noreorder
	bltz	a0,1f
2:	li	v0,NF_FSUMEM		# BDSLOT
	sw	v0,nofault
	lbu	v0,0(a0)
	sw	zero,nofault		# LDSLOT
	j	ra
	nop
	.set	reorder

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	uerror
	END(fubyte)

LEAF(subyte)
XLEAF(suibyte)
	.set	noreorder
	bltz	a0,1f
2:	li	v0,NF_FSUMEM		# BDSLOT
	sw	v0,nofault
	sb	a1,0(a0)
	sw	zero,nofault
	move	v0,zero
	j	ra
	nop
	.set	reorder

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	uerror
	END(subyte)

LEAF(fuword)
XLEAF(fuiword)
	.set	noreorder
	bltz	a0,1f
2:	li	v0,NF_FSUMEM		# BDSLOT
	sw	v0,nofault
	lw	v0,0(a0)
	sw	zero,nofault		# LDSLOT
	j	ra
	nop
	.set	reorder

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	uerror
	END(fuword)
	

LEAF(suword)
XLEAF(suiword)
	.set	noreorder
	bltz	a0,1f
2:	li	v0,NF_FSUMEM		# BDSLOT
	sw	v0,nofault
	sw	a1,0(a0)
	sw	zero,nofault
	move	v0,zero
	j	ra
	nop
	.set	reorder

1:	lb	v0,UP_U_KSEGFLG
	bnez	v0,2b
	j	uerror
	END(suword)

LEAF(uerror)
	li	v0,-1			# error return
	j	ra
	END(uerror)

#ifdef TODO
use this useracc instead of the one in os/probe.c
check business of copy-on-write and locks and counts

/*
 * useracc(addr, bcnt, rw)
 * verify user access to virtual addresses addr .. addr+bcnt-1
 * if rw is 0, write access is verified, if rw is 1, read access
 */
LEAF(useracc)
	beq	a1,zero,3f		# nothing to check

	.set	noreorder
	bltz	a0,11f
12:	li	v0,NF_USERACC
	sw	v0,nofault
	.set	reorder

	addu	t0,a0,a1		# end + 1
	and	a0,~POFFMASK		# back-up to start of page
	bne	a2,zero,2f		# verify read access
	/*
	 * verify write access
	 */
1:	lw	v0,0(a0)
	addu	a0,NBPP
	sw	v0,-NBPP(a0)
	bltu	a0,t0,1b

	.set	noreorder
	sw	zero,nofault
	j	ra
	li	v0,1			# BDSLOT
	.set	reorder

	/*
	 * verify read access
	 */
2:	lw	v0,0(a0)
	addu	a0,NBPP
	bltu	a0,t0,2b

	.set	noreorder
3:	sw	zero,nofault
	j	ra
	li	v0,1			# BDSLOT
	.set	reorder

11:	lb	v0,UP_U_KSEGFLG
	bnez	v0,12b
	j	uaerror
	END(useracc)

LEAF(uaerror)
	move	v0,zero
	j	ra
	END(uaerror)

#endif TODO

/*
 * !!!!!!!!!!!!
 * Implement fustring/sustring
 */

LEAF(strlen)
	move	v0,a0		# save beginning pointer
1:	lb	v1,0(a0)	# look at byte
	addu	a0,1		# advance current pointer
	bne	v1,zero,1b	# check for null byte
	subu	v0,a0,v0	# byte count including null byte
	subu	v0,1		# exclude null byte
	j	ra
	END(strlen)
	

/*
 * The following routines uload_word(), uload_half(), uloaduhalf(),
 * ustore_word() and ustore_half() load and store unaligned items.
 * The "addr" parameter is the address at which the reference is to be
 * made.  For load routines the value is returned indirectly through
 * the "pword" parameter.  For store routines the "value" pramameter
 * is stored.  All routines indicate an error by returning a non-zero
 * value.  If no error occurs a zero is returned.
 */

/*
 * NOTE:  see the comments to copyin() about nested interrupts.
 */

/*
 * int uload_word(addr, pword)
 * u_int addr, *pword;
 */
LEAF(uload_word)
#ifdef ASSERTIONS
	lw	v0,nofault
	beq	v0,zero,8f
	PANIC("recursive nofault")
8:
#endif ASSERTIONS
	li	v0,NF_FIXADE
	.set NOMOVE
	sw	v0,nofault
	ulw	v1,0(a0)
	sw	zero,nofault
	.set MOVE
	sw	v1,0(a1)
	move	v0,zero
	j	ra
	END(uload_word)

/*
 * int uload_half(addr, pword)
 * u_int addr, *pword;
 */
LEAF(uload_half)
#ifdef ASSERTIONS
	lw	v0,nofault
	beq	v0,zero,8f
	PANIC("recursive nofault")
8:
#endif ASSERTIONS
	li	v0,NF_FIXADE
	.set NOMOVE
	sw	v0,nofault
	ulh	v1,0(a0)
	sw	zero,nofault
	.set MOVE
	sw	v1,0(a1)
	move	v0,zero
	j	ra
	END(uload_half)

/*
 * int uload_uhalf(addr, pword)
 * u_int addr, *pword;
 */
LEAF(uload_uhalf)
#ifdef ASSERTIONS
	lw	v0,nofault
	beq	v0,zero,8f
	PANIC("recursive nofault")
8:
#endif ASSERTIONS
	li	v0,NF_FIXADE
	.set NOMOVE
	sw	v0,nofault
	ulhu	v1,0(a0)
	sw	zero,nofault
	.set MOVE
	sw	v1,0(a1)
	move	v0,zero
	j	ra
	END(uload_uhalf)

/*
 * ustore_word(addr, value)
 * u_int addr, value;
 */
LEAF(ustore_word)
#ifdef ASSERTIONS
	lw	v0,nofault
	beq	v0,zero,8f
	PANIC("recursive nofault")
8:
#endif ASSERTIONS
	li	v0,NF_FIXADE
	.set NOMOVE
	sw	v0,nofault
	usw	a1,0(a0)
	sw	zero,nofault
	.set MOVE
	move	v0,zero
	j	ra
	END(ustore_word)

/*
 * ustore_half(addr, value)
 * u_int addr, value;
 */
LEAF(ustore_half)
#ifdef ASSERTIONS
	lw	v0,nofault
	beq	v0,zero,8f
	PANIC("recursive nofault")
8:
#endif ASSERTIONS
	li	v0,NF_FIXADE
	.set NOMOVE
	sw	v0,nofault
	ush	a1,0(a0)
	sw	zero,nofault
	.set MOVE
	move	v0,zero
	j	ra
	END(ustore_half)

LEAF(fixade_error)
	move	v0,gp
	j	ra
	END(fixade_error)

/*
 * This is a dummy routine used to defeat the optimizer.
 * If it is called with variables as arguments, it fools the
 * optimizer into thinking that the variables are used.
 */
LEAF(not_dead_code)
	j	ra
	END(notdeadcode)

#ifdef PROFILING
/*
 * mcount.s -- profiling routines for invocation and basic block counting
 *
 * WARNING: THIS MODULE MUST BE COMPILED -p0
 *
 *	PROFTYPE == 1	=>	only pc sampling, no counting
 *	PROFTYPE == 2	=>	invocation counting
 *	PROFTYPE == 3	=>	basic block counting
 *	PROFTYPE == 4	=>	gprof style counting
 *
 * Calls to these routines are automatically generated by the mips assembler
 * and appear as the following code sequence:
 *		.set	noreorder
 *		.set	noat
 *		move	AT,ra		# save current return address
 *		jal	_mcount		# call profiling counting routine
 *		subu	sp,8		# BDSLOT: alloc stack space for 2 words
 *		# stack space must be free'd by _mcount
 *		.set	reorder
 *		.set	at
 *
 * NB:  The compiler will no longer accept the -p2 or -p3 flags.  You must
 *      build the files you want to mcount with -Wb,-p2 for invocation counts
 *      or -Wb,-p3 for bb counts. _mcount requires startup.c to create
 *      buffer space for it.
 */

LEAF(_mcount)
	.set	noreorder
	.set	noat
	sw	AT,4(sp)	# save caller's return address
	lw	AT,_mcountoff	# offset from start of text to data
	sw	fp,0(sp)	# save temp register
	.set	noat
	beqz	AT,$bailout	# no memory for counters
	li	fp, 0x1fffffff
	and	fp, ra, fp
	li	AT, 0x21000
	sub	fp,AT		# counter is at _mcountoff + (return
	#sub	fp, ra, 0x80020000
	lw	AT, _mcountoff
	srl	fp,3		# address) / 2; aligned to word
	sll	fp,2
	addu	AT,fp
	lw	fp,0(AT)	# get counter
	nop
	addu	fp,1
	sw	fp,0(AT)
$bailout:
	lw	AT,4(sp)	# reload callers return address
	lw	fp,0(sp)	# reload temp register
	addu	sp,8		# fixup stack
	j	ra		# return
	move	ra,AT		# BDSLOT: restore caller's return address
	.set	reorder
	.set	at
	END(_mcount)
#endif PROFILING
