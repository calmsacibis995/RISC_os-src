#ident "$Header: libasm.s,v 1.34 90/10/26 15:28:20 hawkes Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * libasm.s -- standalone libc'ish assembler code
 */

#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/ioa.h"
#include "machine/bc.h"
#include "saio/setjmp.h"
#include "prom/entrypt.h"

	BSS(_icache_size, 4)		# bytes of icache
	BSS(_dcache_size, 4)		# bytes of dcache
	BSS(_scache_size, 4)		# bytes of scache
	LBSS(wbflush_tmp, 4)

	.extern	machine_type
	.globl	mbyte_size_mask
	.globl	mbyte_size_mask_end

/*
 * setjmp(jmp_buf) -- save current context for non-local goto's
 * return 0
 */
LEAF(setjmp)
	sw	ra,JB_PC*4(a0)
	sw	sp,JB_SP*4(a0)
	sw	fp,JB_FP*4(a0)
	sw	s0,JB_S0*4(a0)
	sw	s1,JB_S1*4(a0)
	sw	s2,JB_S2*4(a0)
	sw	s3,JB_S3*4(a0)
	sw	s4,JB_S4*4(a0)
	sw	s5,JB_S5*4(a0)
	sw	s6,JB_S6*4(a0)
	sw	s7,JB_S7*4(a0)
	move	v0,zero
	j	ra
	END(setjmp)


/*
 * longjmp(jmp_buf, rval)
 */
LEAF(longjmp)
	lw	ra,JB_PC*4(a0)
	lw	sp,JB_SP*4(a0)
	lw	fp,JB_FP*4(a0)
	lw	s0,JB_S0*4(a0)
	lw	s1,JB_S1*4(a0)
	lw	s2,JB_S2*4(a0)
	lw	s3,JB_S3*4(a0)
	lw	s4,JB_S4*4(a0)
	lw	s5,JB_S5*4(a0)
	lw	s6,JB_S6*4(a0)
	lw	s7,JB_S7*4(a0)
	move	v0,a1
	j	ra
	END(longjmp)

/*
 * sa_spl() -- reestablish desired sa lib status register
 * clear any pending write bus error interrupts
 * returns current sr
 */
LEAF(sa_spl)
	move	a3,ra
	jal	get_machine_type
	move	ra,a3
	beq	v0,BRDTYPE_R3030,51f		#for now
	bne	v0,BRDTYPE_R6300,52f
	/* R6000 */
51:
	.set	noreorder
	mfc0	v0,C0_SR
	.set	reorder
	move	v1,zero				# disable interrupts
	b	56f
52:	beq	v0,BRDTYPE_M180,53f
	bne	v0,BRDTYPE_R2400,54f
	/* M120 */
53:	lw	zero,FAR|K1BASE
	b	55f
	/* Mbox */
54:	lw	zero,SBE_ADDR|K1BASE
	beq	v0,BRDTYPE_RB3125,57f
	bne	v0,BRDTYPE_R3200,55f
57:
	sw	zero,SBE_ADDR|K1BASE
	.set noreorder
55:	nop
	nop
	nop
	mfc0	v0,C0_SR
#ifdef XMIPS
	lw	zero,SBE_ADDR+K1BASE
#endif
	li	v1,(SR_IMASK7|SR_IEC)
56:	mtc0	v1,C0_SR
	j	ra
	nop
	.set reorder
	END(sa_spl)

#define	NBPW	4

/*
 * bcopy(src, dst, bcount)
 *
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
 * 32 byte block, aligned copy loop (for big reads/writes)
 */
blkcopy:
	and	a3,a2,~31		# total space in 32 byte chunks
	subu	a2,a3			# count after by-32 byte loop done
	beq	a3,zero,wordcopy	# less than 32 bytes to copy
	addu	a3,a0			# source endpoint
1:	lw	v0,0(a0)
	lw	v1,4(a0)
	lw	t0,8(a0)
	lw	t1,12(a0)
	sw	v0,0(a1)
	sw	v1,4(a1)
	sw	t0,8(a1)
	sw	t1,12(a1)
	addu	a0,32			# src+= 32; here to ease loop end
	lw	v0,-16(a0)
	lw	v1,-12(a0)
	lw	t0,-8(a0)
	lw	t1,-4(a0)
	sw	v0,16(a1)
	sw	v1,20(a1)
	sw	t0,24(a1)
	sw	t1,28(a1)
	addu	a1,32			# dst+= 32; fills BD slot
	bne	a0,a3,1b

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
	move	v0,v1
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

/* This function is an assembly-code replacement for the libc function
 * strcpy.  It uses the MIPS special instructions "lwl", "lwr", "swl",
 * and "swr", which handle unaligned words.

 * The standard C version of this function is a 5-instruction loop,
 * working one byte at a time:

 * Copy string s2 to s1.  s1 must be large enough.
 * return s1
 *	char *strcpy(s1, s2)
 *	register char *s1, *s2;
 *	{
 *		register char *os1;
 *		os1 = s1;
 *		while (*s1++ = *s2++);
 *		return(os1);
 *	}

 * A better C version is 4 cycles/byte. Loop is unrolled once.
 * char *
 * strcpy(s1, s2)
 * register char *s1, *s2;
 * {
 * 	register char *os1 = s1;
 * 	while (1) {
 * 		register unsigned c;
 * 		c = s2[0];
 * 		s2 += 2;
 * 		s1[0] = c;
 * 		if (c == 0) break;
 * 		c = s2[1-2];
 * 		s1 += 2;
 * 		s1[1-2] = c;
 * 		if (c == 0) break;
 * 	}
 * 	return(os1);
 * }

 * This function starts with an unrolled loop, which uses 5
 * instructions per byte (including the store bytes at the end) for
 * the first few bytes.

 * After filling a word, the first word or portion of a word is saved
 * using a "swl" instruction. If the start of destination string is at
 * a word boundary, this leaves the result valid in the cache. Because
 * this replaces up to 4 store byte instructions, we are still near 3
 * instructions per byte, but there is only one write.
   
 * The inner loop moves 4 bytes in 16 cycles, an average of 4 cycles
 * per byte.  This is 1 cycle faster than the standard C code, the
 * same speed as the unrolled version, and it also leaves the result
 * valid in the cache.
   
 * Finally, when a zero byte is found, the end of the string is stored
 * using store byte instructions.  This adds one instruction per byte
 * for as much as three bytes, but elminates the up to four cycles of
 * overhead we counted before.
   
 * The end result is that this function is never slower than the C
 * function, is faster by up to 30% in instruction count, uses up to
 * 75% fewer writes, and leaves most of the result valid in the cache.
   
 * There are one caveat to consider: this function is written in
 * assembler code, and as such, cannot be merged using the U-code
 * loader. */

/* Craig Hansen - 3-September-86 */

#ifdef MIPSEB
#    define LWS lwl
#    define LWB lwr
#    define SWS swl
#    define SWB swr
#else
#    define LWS lwr
#    define LWB lwl
#    define SWS swr
#    define SWB swl
#endif

LEAF(strcpy)
.set noreorder
	## a0/ destination
	## a1/ source
	move	v0, a0		# a copy of destination address is returned
	## start up first word
	## adjust pointers so that a0 points to next word
	## t7 = a1 adjusted by same amount minus one
	## t0,t1,t2,t3 are filled with 4 consecutive bytes
	## t4 is filled with the same 4 bytes in a single word
	lb	t0, 0(a1)
	ori	t5, a0, 3	# get an early start
	beq	t0, 0, $doch0
	sub	t6, t5, a0	# number of char in 1st word of dest - 1
	lb	t1, 1(a1)
	add	t7, a1, t6	# offset starting point for source string
	beq	t1, 0, $doch1
	nop
	lb	t2, 2(a1)
	nop
	beq	t2, 0, $doch2
	LWS	t4, 0(a1)	# safe: always in same word as 0(a1)
#ifdef R6000_BUG_LWL
	LWB	t4, 3(a1)	# fill out word
	lb	t3, 3(a1)
	nop			# for non-6000
#else
	lb	t3, 3(a1)
	LWB	t4, 3(a1)	# fill out word
#endif R6000_BUG_LWL
	beq	t3, 0, $doch3
	SWS	t4, 0(a0)	# store entire or part word
	addi	a0, t5, 1-4	# adjust destination ptr

	## inner loop
1:	lb	t0, 1(t7)
	addi	t7, 4
	beq	t0, 0, $doch0
	addi	a0, 4
	lb	t1, 1+1-4(t7)
	nop
	beq	t1, 0, $doch1
	nop
	lb	t2, 2+1-4(t7)
	nop
	beq	t2, 0, $doch2
	LWS	t4, 0+1-4(t7)
#ifdef R6000_BUG_LWL
	LWB	t4, 3+1-4(t7)
	lb	t3, 3+1-4(t7)
	nop			# for non-6000
#else
	lb	t3, 3+1-4(t7)
	LWB	t4, 3+1-4(t7)
#endif R6000_BUG_LWL
	bne	t3, 0, 1b
	sw	t4, 0(a0)
	j	ra
	nop

	## store four bytes using swl/swr
$doch3:	j	ra
	SWB	t4, 3(a0)
	## store up to three bytes, a byte at a time.
$doch2:	sb	t2, 2(a0)
$doch1:	sb	t1, 1(a0)
$doch0:	j	ra
	sb	t0, 0(a0)

	END(strcpy)


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
	END(strcmp)

/*
 * Config_cache() -- determine sizes of i and d caches
 * Sizes stored in globals _dcache_size and _icache_size
 *
 * MUST NOT USE t4, see initmem()
 */
CONFIGFRM=	(4*4)+4+4		# 4 argsaves, ra, and a regsave
NESTED(config_cache, CONFIGFRM, zero)
	subu	sp,CONFIGFRM
	sw	ra,CONFIGFRM-4(sp)	# save return address
	sw	s0,4*4(sp)		# save s0 in first regsave slot
	.set	noreorder
	mfc0	s0,C0_SR		# save SR
	nop
	mtc0	zero,C0_SR		# disable interrupts
	.set	reorder
	lw	v0,machine_type
	beq	v0,BRDTYPE_R3030,4f
	bne	v0,BRDTYPE_R6300,2f
	/*
	 *  Excalibur -- XXX hack the values for now
	 */
	li	v0,16*1024
	sw	v0,_dcache_size
	li	v0,64*1024
	sw	v0,_icache_size
	li	v0,512*1024
	sw	v0,_scache_size
	b	3f			# go exit

4:
	/*
	 *  Pizazz -- XXX hack the values for now
	 */
	li	v0,32*1024
	sw	v0,_dcache_size
	li	v0,32*1024
	sw	v0,_icache_size
	b	3f			# go exit

2:	sw	zero,_scache_size	# no Scache
	la	v0,1f
	or	v0,K1BASE
	j	v0			# run uncached

1:	jal	_size_cache
	sw	v0,_dcache_size
	li	v0,SR_SWC		# swap caches
	.set	noreorder
	mtc0	v0,C0_SR
	jal	_size_cache
	nop
	sw	v0,_icache_size
	mtc0	zero,C0_SR		# swap back caches
	la	t0,3f
	j	t0			# back to cached mode
	nop

3:	mtc0	s0,C0_SR		# restore SR
	lw	ra,CONFIGFRM-4(sp)	# restore ra
	.set	reorder
	lw	s0,4*4(sp)		# restore s0
	addu	sp,CONFIGFRM		# pop stack
	j	ra
	END(config_cache)

/*
 * _size_cache()
 * Discover and return the size of the cache presently configured
 * as the data cache.  This is called, after the appropriate setup,
 * to size both the instruction and data caches.
 * To be specific, DO NOT change the swap-cache status bit.
 * This routine MUST be called with interrupts disabled.
 * 
 */
LEAF(_size_cache)
	.set	noreorder
			/* CHIP BUG dodge
	 		* We don't want the write buffer to be full when we do
	 		* a store in isolated mode.
			* We can't tell that, but we can see if it is empty.
	 		* This is the important part of "wbflush".
	 		*/
99:
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
	END(_size_cache)

/*
 * flush_cache()
 *
 * invalidate entire cache for both instruction and data caches.
 */

FLUSHFRM=	4		# ra
NESTED(flush_cache, FLUSHFRM, zero)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,flush_cache_r6000

	subu	sp,FLUSHFRM
	sw	ra,FLUSHFRM-4(sp)

	li	a0,K0BASE		# base address for clean
	lw	a1,_icache_size		# clean the whole icache
	jal	clean_icache

	li	a0,K0BASE		# base address for clean
	lw	a1,_dcache_size		# clean the whole dcache
	jal	clean_dcache

	lw	ra,FLUSHFRM-4(sp)
	addu	sp,FLUSHFRM
	j	ra

	END(flush_cache)

#define R6000_DLINE_BSIZE	  8
#define R6000_ILINE_BSIZE	 32
#define R6000_SLINE_BSIZE	128	
#define R6000_SLINE_SHIFT	  7
#define R6000_SCACHE_DATA_BSIZE	(240*1024)	/* don't count tlb/ptag */
#define R6000_PAGE_BSIZE	16384
#define R6000_PAGE_OFFSET	(R6000_PAGE_BSIZE-1)
#define R6000_PAGE_SHIFT	14		/* byte addr to page addr */

LEAF(flush_cache_r6000)
	li	a0,K0BASE
	li	a1,R6000_SCACHE_DATA_BSIZE
	j	clear_cache_r6000
	END(flush_cache_r6000)

/*
 * clear_cache(addr, len)
 *
 * invalidate all caches for range of addr to addr+len-1
 * MUST NOT DESTROY a0 and a1
 */

CLEARFRM=	(2*4)+4		# 2 args, ra
NESTED(clear_cache, CLEARFRM, zero)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,clear_cache_r6000

	subu	sp,CLEARFRM
	sw	ra,CLEARFRM-4(sp)
	# NOTE don't need to save args because the clean routines
	# both PROMISE not to destroy them

	jal	clean_icache		# clean the addrs in the icache

	jal	clean_dcache		# clean the addrs in the dcache

	lw	ra,CLEARFRM-4(sp)
	addu	sp,CLEARFRM
	j	ra
	END(clear_cache)

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
 * MUST NOT DESTROY a0 and a1
 * (rewrite courtesy of Earl Killian)
 */
LEAF(clean_icache)
	.set noreorder

	mfc0	t3,C0_SR		# save SR
	beq	a1,zero,6f		# if size==0, return without action
	 lw	t1,_icache_size
	bltz	a1,80f			# negative size, bad argument
	 addu	t4,a1,63		# round req_len up to multiple of 64
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

2:
	/*
	 * Invariants:
	 * t0 contains the starting invalidation address
	 * t1 contains the limit (beyond) invalidation address
 	 * t3 - saved Status Register
	 */

	mtc0	zero,C0_SR		# disable interrupts
	nop
			/* CHIP BUG dodge
	 		* We don't want the write buffer to be full when we do
	 		* a store in isolated mode.
			* We can't tell that, but we can see if it is empty.
	 		* This is the important part of "wbflush".
	 		*/
99:
	bc0f	99b			# write buffer is not empty if false
	 nop
	la	t7,3f			# get kseg0 code address
	or	t7,K1BASE		# convert to kseg1 address
	j	t7			# execute from uncached addrs
	 li	t6,SR_ISC|SR_SWC	# disable intr, isolate and swap caches

	/*
	 * flush text cache
	 */
3:	mtc0	t6,C0_SR		# this instr must execute uncached
	la	t7,4f			# load kseg0 (cached) addr
	j	t7			# execute loop in cached addrs
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
	bne	t0,t1,4b
	 sb	zero,-4(t0)

	la	t7,5f
	or	t7,K1BASE
	j	t7			# run uncached
	 nop

5:
	mtc0	zero,C0_SR		# unswap and unisolate (intr disabled)
	nop
	mtc0	t3,C0_SR		# restore entry SR contents
	nop
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
	lw	t1,_icache_size
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
 * MUST NOT DESTROY a0 and a1
 */
LEAF(clean_dcache)
	.set noreorder

	mfc0	t3,C0_SR		# save SR
	beq	a1,zero,6f		# if size==0, return without action
	 lw	t1,_dcache_size
	bltz	a1,80f			# negative size, bad argument
	 addu	t4,a1,63		# round req_len up to multiple of 64
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

2:
	/*
	 * Invariants:
	 * t0 contains the starting invalidation address
	 * t1 contains the limit (beyond) invalidation address
 	 * t3 - saved Status Register
	 */

	mtc0	zero,C0_SR		# disable interrupts
	nop

			/* CHIP BUG dodge
	 		* We don't want the write buffer to be full when we do
	 		* a store in isolated mode.
			* We can't tell that, but we can see if it is empty.
	 		* This is the important part of "wbflush".
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
	bne	t0,t1,4b
	 sb	zero,-4(t0)

5:
	mtc0	zero,C0_SR		# un-isolate
	nop
	mtc0	t3,C0_SR		# restore entry SR contents
	nop
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
	lw	t1,_dcache_size
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
 * _clear_cache_r6000(base_addr, byte_count)
 * flush and invalidate portion of r6000 cache
 *
 * This routine is a wrapper for _clear_cache_r6000 and handles the case
 * where the portion of the cache to be cleared crosses one or more page
 * boundaries.
 */

#if !PROM || !R3030
CCFRM=		(4*4)+4			# 4 argsaves, ra
NESTED(clear_cache_r6000, CCFRM, zero )
	.set	noreorder
  	subu	sp,CCFRM
	
  	beq	a1,zero,9f
  	sw	ra,CCFRM-4(sp)
1:
	/*	bcount = min(NBPC-(dmaaddr & (NBPC-1)),dmasize);	*/
  	li	t9,R6000_PAGE_BSIZE
  	andi	t8,a0,R6000_PAGE_OFFSET	# t8 = (dmaaddr & (NBPC-1))
   	subu	t0,t9,t8
	
	/* t2 = min( t0, a1 );	*/
  	slt	t1,t0,a1
  	beq	t1,zero,5f
  	move	t2,a1
  	move	t2,t0
5:  
  	subu	a1,a1,t2		# dmasize -= bcount 
  	addu	t1,a0,t2		# next dmaaddr = dmaaddr + bcount */
	sw	a1,CCFRM+4(sp)		# save remaining dmasize */
	sw	t1,CCFRM(sp)		# save next dmaaddr */
	
  	jal	_clear_cache_r6000
	move	a1,t2			# a1 = bcount */

	/*	dmasize -= bcount;	*/ 
 	lw	a1,CCFRM+4(sp)		# a1 = dmasize */
  	lw	a0,CCFRM(sp)		# a0 = dmaaddr */

	/*	dmaaddr += bcount;	*/
  	bne	a1,zero,1b
	nop

/*  return to caller */
9:
  	lw	ra,CCFRM-4(sp)
	nop
  	j	ra
  	addu	sp,CCFRM

	.set	reorder
	END(clear_cache_r6000)


/*
 * __clear_cache_r6000(base_addr, byte_count)
 * flush portion of r6000 cache
 *
 *  Assume "base_addr" is a K0 or K1 direct-mapped address.  This ensures that
 *  the proper s-cache section is cleared for a partial clear_cache.  But in
 *  case a mapped address (Ku or K2) is passed, we always convert the address
 *  to a K0 address.
 *
 *  This code doesn't have to be efficient, just effective.  The kernel
 *  has specific routines to manage each primary cache and the secondary
 *  cache, but our desire here in the SA arena is for simplicity.  We want
 *  minimal changes in the drivers and other callers of clear_cache, and we
 *  need the Prom "clear_cache" entrypoint to work for the R6000, too.
 *
 *  In that light, we invalidate appropriate pieces of both primary virtual
 *  caches (i.e. from page-offset-for-N-bytes for each possible page in the
 *  caches, to catch potential virtual aliases), and the appropriate
 *  piece of the secondary cache is written-back to physical memory, its
 *  virtual tags invalidated, and its physical tags zapped.
 */
LEAF(_clear_cache_r6000)
	.set	noreorder
	mfc0	t8,C0_SR		# disable interrupts, and
	li	t7,SR_MM_MODE		#  enable MemMgt Mode
	mtc0	t7,C0_SR

	/*
	 *  don't trust the base_addr -- always form a K0 address
	 */
	sll	a0,3			# shift off K* bits
	srl	a0,3			# shift back
	or	a0,K0BASE		# and form a K0 address

	/*
	 *  invalidate d-cache
	 */
	lw	t3,_dcache_size
	li	t2,R6000_PAGE_OFFSET	# form mask of addr page offset
	and	t0,a0,t2		#  and form effective beginning addr
	move	t9,t0			#  and save in t9
	srl	t7,t3,R6000_PAGE_SHIFT	# num pages of cache
	subu	t7,1			#  minus one
	move	t5,a1			# proposed clear length
	sltu	t4,a1,t3		# if length >= cache size ...
	bne	t4,zero,1f
	nop
	move	t5,t3			# ... then adjust length and addr
	move	t0,zero			#      to invalidate entire cache
	move	t7,zero			#      as one big "page"
1:	add	t1,t0,t5		# ending address, plus 1
	subu	t1,8*R6000_DLINE_BSIZE	# readjusted for loop control
2:	inval	1+0*R6000_DLINE_BSIZE(t0)	# inval 8 lines at a time
	inval	1+1*R6000_DLINE_BSIZE(t0)
	inval	1+2*R6000_DLINE_BSIZE(t0)
	inval	1+3*R6000_DLINE_BSIZE(t0)
	inval	1+4*R6000_DLINE_BSIZE(t0)
	inval	1+5*R6000_DLINE_BSIZE(t0)
	inval	1+6*R6000_DLINE_BSIZE(t0)
	inval	1+7*R6000_DLINE_BSIZE(t0)
	slt	t2,t0,t1		# t2 <-- keep going?
	bne	t2,zero,2b
	addi	t0,8*R6000_DLINE_BSIZE	# (BDSLOT) incr inval addr
	beq	t7,zero,3f		# completely done with d-cache?
	subu	t7,1			# (BDSLOT) decr page count
	add	t9,R6000_PAGE_BSIZE	# invalidate another piece
	b	1b			#  of d-cache, one page higher
	move	t0,t9			# (BDSLOT)
3:
	/*
	 *  invalidate i-cache
	 */
	lw	t3,_icache_size
	li	t2,R6000_PAGE_OFFSET	# form mask of addr page offset
	and	t0,a0,t2		#  and form effective beginning addr
	move	t9,t0			#  and save in t9
	srl	t7,t3,R6000_PAGE_SHIFT	# num pages of cache
	subu	t7,1			#  minus one
	move	t5,a1			# proposed clear length
	sltu	t4,a1,t3		# if length >= cache size ...
	bne	t4,zero,1f
	nop
	move	t5,t3			# ... then adjust length and addr
	move	t0,zero			#      to invalidate entire cache
	move	t7,zero			#      as one big "page"
1:	add	t1,t0,t5		# ending address, plus 1
	subu	t1,8*R6000_ILINE_BSIZE	# readjusted for loop control
2:	inval	0*R6000_ILINE_BSIZE(t0)	# inval 8 lines at a time
	inval	1*R6000_ILINE_BSIZE(t0)
	inval	2*R6000_ILINE_BSIZE(t0)
	inval	3*R6000_ILINE_BSIZE(t0)
	inval	4*R6000_ILINE_BSIZE(t0)
	inval	5*R6000_ILINE_BSIZE(t0)
	inval	6*R6000_ILINE_BSIZE(t0)
	inval	7*R6000_ILINE_BSIZE(t0)
	slt	t2,t0,t1		# t2 <-- keep going?
	bne	t2,zero,2b
	addi	t0,8*R6000_ILINE_BSIZE	# (BDSLOT) incr inval addr
	beq	t7,zero,3f		# completely done with cache?
	subu	t7,1			# (BDSLOT) decr page count
	add	t9,R6000_PAGE_BSIZE	# invalidate another piece
	b	1b			#  of d-cache, one page higher
	move	t0,t9			# (BDSLOT)
3:
	/*
	 *  invalidate s-cache
	 */
	li	t5,R6000_SLINE_BSIZE	
	slt	t4,a1,t5		# do max( clear_length, line_size )
	beq	t4,zero,1f
	nop
	move	a1,t5
1:	move	t1,a0			# beginning addr for invalidate
	sll	t3,t1,3			# shift off K0/K1 prefix bits...
	srl	t3,R6000_PAGE_SHIFT+3	# ...and shift to get true pfn
	li	t4,0xffffffff		# tentative mask for FLUSH/INVAL addrs
	li	t7,0xf			# pfn mask
	and	t6,t3,t7		# low-order 4 bits of pfn will be 12:9
	bne	t6,t7,1f		#  except 0xF gets mapped to 0x7
	add	t2,t1,a1		# (BDSLOT) addr+1 of last FLUSH
	li	t6,7
	li	t4,0xfffdffff		# correct FLUSH/INVAL address mask
1:	ori	t0,t6,0x1f0		# "ptags" bits will be 17:13
	sll	t0,9			#  and shift into proper position
	srl	t1,R6000_SLINE_SHIFT	# start to align FLUSH addr
	andi	t6,t1,0x7f		# isolate cache line index
	sll	t6,2			#  and shift into proper position
	or	t0,t6			#  and merge to form complete addr
	sll	t1,R6000_SLINE_SHIFT	# align FLUSH addr on line boundary
	subu	t2,R6000_SLINE_BSIZE	# set up ending addr for loop control
	and	t1,t4			# adjust addr to avoid using upper 1/16
	and	t2,t4			#  of s-cache
	li	t7,0xfffffc00		# invalid ptag value
2:	flush	(t1)			# flush side-0
	inval	(t1)			#  and invalidate vtag
	scache	t7,(t0)			#  and invalidate ptag
	flush	1(t1)			# flush side-1
	inval	1(t1)			#  and invalidate vtag
	scache	t7,1(t0)		#  and invalidate ptag
	addi	t0,4			# address of next ptag
	slt	t4,t1,t2		# all done?
	bne	t4,zero,2b		# loop back if not done
	addi	t1,R6000_SLINE_BSIZE	# (BDSLOT) incr addr for next FLUSH

#ifdef R6000_BUG_FLUSH
	li	t0,0x3fffc
	scache	zero,(t0)
#endif R6000_BUG_FLUSH
	j	ra
	mtc0	t8,C0_SR		# (BDSLOT) restore SR
	.set	reorder
	END(_clear_cache_r6000)

/*
 *  get_ioa_ctlspace_addr (k1_addr)
 *
 *  given a K1 address, returns the appropriate IOA CtlSpace address in v0
 *  (or zero, if k1_addr isn't in IOA GBA space), and the GBA number in v1
 *
 *  uses only v0..v1, t0..t2, and does not modify a0
 */
LEAF(get_ioa_ctlspace_addr)
	move	v0,zero			# init null return value
	li	t1,IOA3
	sltu	t2,a0,t1		# below the lowest IOA?
	bne	t2,zero,1f		#  yes, in Memory space
	sub	t0,a0,t1		# byte addr, relative to IOA3 base
	srl	t0,22			# r-justify signif addr bits
	andi	t0,0x1c			#  and form a simple index
	andi	v1,t0,0x4		# extract GBA number
	srl	v1,2			#  and right-justify it
	la	t1,ioa_ctl_index_tbl	# use simple index to map
	add	t0,t1			#
	lw	t1,(t0)			#    to another index value
	la	t2,ioa_ctlspace_vaddr
	add	t2,t1
	lw	v0,(t2)			# addr of CtlSpace of IOA
1:	j	ra
	END(get_ioa_ctlspace_addr)

	.data
ioa_ctl_index_tbl:
	.word	8,8, 4,4, 0,0, 12,12
	/*      IOA3 IOA2 IOA1 zero	*/

#else

CCFRM=		(4*4)+4			# 4 argsaves, ra
NESTED(clear_cache_r6000, CCFRM, zero )
	nop
	END(clear_cache_r6000)

#endif	!PROM || !R3030

	.text
/*
 *  decode_brd_address (brd_address)
 *
 *  Takes an SBC BrdAddress and returns a base address (in Mbytes) and a
 *  size (in Mbytes), in v0 and v1, respectively.
 *
 *  Uses registers v0, v1, a0, and t0..t3.
 */
LEAF(decode_brd_address)
	/*  separate out the pairs of bits  */
	move	v0,zero			# init even bits
	move	t1,zero			# init odd bits
	li	t2,16			# init loop counter
	li	t3,0			# init shift counter
1:	andi	t0,a0,1			# isolate even bit
	sll	t0,t3			#  shift left as appropriate
	or	v0,t0			#   and merge
	srl	a0,1
	andi	t0,a0,1			# isolate odd bit
	sll	t0,t3			#  shift left as appropriate
	or	t1,t0			#  and merge
	srl	a0,1
	addi	t3,1			# bump shift counter
	sub	t2,1			# keep looping until done
	bne	t2,zero,1b		#   with all 16 2-bit pairs

	and	t0,v0,t1		# if any bits are common,
	bne	t0,zero,2f		#  then Board responds to nothing
	or	t0,v0,t1		# merge all bits
	la	t2,mbyte_size_mask	# table starting address
	la	t1,mbyte_size_mask_end	# beyond end of table
	li	v1,1			# init size
1:	lw	t3,(t2)
	beq	t3,t0,3f		# found size mask?
	sll	v1,1			#  no, bump size by power of two
	addi	t2,4			# next table address
	bne	t2,t1,1b		# if not beyond end, then loop back
	/*  fell through:  we were fed a weird board address!  */
2:	move	v0,zero			# indicate that board responds
	move	v1,zero			#  to address zero, size zero
	j	ra

3:	srl	v1,1			# readjust size downward for final value
	j	ra
	END(decode_brd_address)

/*
 *  decode_brd_size (brd_address)
 *
 *  Takes an SBC ControlSpace address and returns a size in bytes (in v0).
 *
 *  Uses registers v0, v1, a0, a1, and t0..t3.
 *  NOTE:  we expect that decode_brd_address() does NOT use a1.
 */
LEAF(decode_brd_size)
	move	a1,ra			# save return address
	add	t0,a0,SBC_BRDADDR
	lw	a0,(t0)			# read the BoardAddress
	jal	decode_brd_address	
	sll	v0,v1,20		# convert Mbytes to bytes
	j	a1			# exit
	END(decode_brd_size)

mbyte_size_mask:	.word	0x0000		/*   0 MB */
			.word   0xffff		/*   1 MB */
			.word	0xfffe		/*   2 MB */
			.word	0xfffc		/*   4 MB */
			.word	0xfff8		/*   8 MB */
			.word	0xfff0		/*  16 MB */
			.word	0xffe0		/*  32 MB */
			.word	0xffc0		/*  64 MB */
			.word	0xff80		/* 128 MB */
			.word	0xff00		/* 256 MB */
			.word	0xfe00		/* 512 MB */
mbyte_size_mask_end:




/*
 * _cksum1(addr, len, prevcksum)
 *
 * Calculates a 16 bit ones-complement checksum.
 * Note that for a big-endian machine, this routine always adds even
 * address bytes to the high order 8 bits of the 16 bit checksum and
 * odd address bytes are added to the low order 8 bits of the 16 bit checksum.
 * For little-endian machines, this routine always adds even address bytes
 * to the low order 8 bits of the 16 bit checksum and the odd address bytes
 * to the high order 8 bits of the 16 bit checksum.
 */
LEAF(_cksum1)
	move	v0,a2		# copy previous checksum
	beq	a1,zero,4f	# count exhausted
	and	v1,a0,1
	beq	v1,zero,2f	# already on a halfword boundry
	lbu	t8,0(a0)
	addu	a0,1
#ifdef MIPSEL
	sll	t8,8
#endif MIPSEL
	addu	v0,t8
	subu	a1,1
	b	2f

1:	lhu	t8,0(a0)
	addu	a0,2
	addu	v0,t8
	subu	a1,2
2:	bge	a1,2,1b
	beq	a1,zero,3f	# no trailing byte
	lbu	t8,0(a0)
#ifdef MIPSEB
	sll	t8,8
#endif MIPSEB
	addu	v0,t8
3:	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff
4:	j	ra
	END(_cksum1)

/*
 * nuxi_s and nuxi_l -- byte swap short and long
 */
LEAF(nuxi_s)			# a0 = ??ab
	srl	v0,a0,8		# v0 = 0??a
	and	v0,0xff		# v0 = 000a
	sll	v1,a0,8		# v1 = ?ab0
	or	v0,v1		# v0 = ?aba
	and	v0,0xffff	# v0 = 00ba
	j	ra
	END(nuxi_s)

LEAF(nuxi_l)			# a0 = abcd
	sll	v0,a0,24	# v0 = d000
	srl	v1,a0,24	# v1 = 000a
	or	v0,v0,v1	# v0 = d00a
	and	v1,a0,0xff00	# v1 = 00c0
	sll	v1,v1,8		# v1 = 0c00
	or	v0,v0,v1	# v0 = dc0a
	srl	v1,a0,8		# v1 = 0abc
	and	v1,v1,0xff00	# v1 = 00b0
	or	v0,v0,v1	# v0 = dcba
	j	ra
	END(nuxi_l)

/*
 * clearpage(dst_ppn,bcount)
 *
 *	Performance
 *	Config	Cycles/	Speed vs VAX
 *		4K Page	
 *	08V11	6,144	1.09X
 *	08M44	1,229	5.46X	(could be made faster by unroll to 64)
 */
/* #define	PGSHIFT	12		*/
/* #define	NBPG	(4*1024)	*/
LEAF(clearpage)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,3f
	beq	v0,BRDTYPE_R3030,3f

	la      v0,3f
	la      v1,5f
	.set    noreorder
	mfc0    t0,C0_SR                # get status register
	li      t1,SR_SWC               # swap cache bit
	or      t1,t0
	mtc0    t1,C0_SR                # swap caches
	li      t1,K0SIZE               # size of cached addresses
	move    t2,a0                   # get starting memory address
	subu    t2,t1                   # convert to cached address
	move    t6,t2                   # save address
	move    t3,v0                   # get starting PROM address
1:
	lw      t4,(t3)                 # get word from PROM
	addu    t3,4                    # bump PROM pointer
	sw      t4,(t2)                 # store in memory
	lw      t5,(t2)                 # read from memory
	addu    t2,4                    # bump memory pointer
	bne     t4,t5,2f                # compare failed - don't do cached
	bltu    t3,v1,1b                # do all prom words
	nop
	move    v0,t6                   # get instruction address
2:
	mtc0    t0,C0_SR                # un-swap caches
	nop
	nop
	.set    reorder
	j       v0

3:
	addu	t0,a0,a1
	subu	t0,32			# dst on last pass of loop
4:	sw	zero,0(a0)
	sw	zero,4(a0)
	sw	zero,8(a0)
	sw	zero,12(a0)
	sw	zero,16(a0)
	sw	zero,20(a0)
	sw	zero,24(a0)
	sw	zero,28(a0)
	.set	noreorder
	bne	a0,t0,4b
	addu	a0,32			# BDSLOT: inc dst, NOTE after test
	.set	reorder

	j	ra
5:

	END(clearpage)

/*
 * wbflush() -- spin until write buffer empty
 */
LEAF(wbflush)
#ifndef	M2000WB
	.set	noreorder
	.set	noat
	nop
	lui	AT,0xa000
	lw	zero,(AT)
1:
	bc0f	1b
	nop
	.set	at
	.set	reorder
#else	M2000WB
	la	v0,wbflush_tmp
	or	v0,K1BASE
	sw	zero,(v0)
	lw	zero,(v0)
#endif	M2000WB
	j	ra
	END(wbflush)

#define	CPU_IMP_R6000	3

LEAF(get_machine_type)
	/*
	 *	uses v0,v1,a0
	 *		returns machine_type in v0
	 */
	.set	noreorder
	mfc0	v0,C0_PRID		# what kind of CPU is this?
	nop				# (LDSLOT)
	.set	reorder
	srl	v0,8			# right-justify "implementation" field
	bne	v0,CPU_IMP_R6000,1f	# R6000 CPU?

	/*
	 * This is an R6000 CPU.  Use the explicit BRDTYPE.
	 */
	li	v0,BRDTYPE_R6300
	j	ra

1:
#ifndef SABLE
	/*
	 * There are no proms on the R2000 side of the Jupiter workstation.
	 * The first page of prom space is remapped to zero.  By writting
	 * a pattern to some offset in the first page and reading that
	 * pattern back in prom space we know that this system is a
	 * Jupiter workstation.
	 */
	li	v1,RESTART_ADDR-4
	li	v0,RESTART_MAGIC
	sw	v0,0(v1)
	or	v1,BOOTPROM
	lw	a0,0(v1)
	bne	a0,RESTART_MAGIC,3f
	li	v0,BRDTYPE_I2000
	j	ra
3:
	/*
	 * The idea here is that on Intrepid, BOOTPROM and BOOTPROM +
	 * BOOTPROM_SIZE point to mirror images of the boot prom.
	 * If it fails, then we are on either a M-series or M2000
	 */

	li	v0,ID_SN5_OFF-1			# max bytes in IDPROM
1:
	#
	#	check if data in IDPROM matches
	#
	lbu	v1,(K1BASE|BOOTPROM)(v0)		
	lbu	a0,K1BASE|(BOOTPROM+BOOTPROM_SIZE)(v0)
	bne	v1,a0,2f
	#
	#	check if passed max bytes in IDPROM
	sub	v0,1
	bgt	v0,zero,1b
	#
	# everything matched therefore it's intrepid or R3030
	#
	la	a0,K1BASE|IDPROM_R2400+3 # IDPROM of M120/M180
	li	v0,4			# check if boardtype = 4 
	lbu	v1,0(a0)
	bne	v0,v1,5f		# if not, then R3030
	#
	#	check if IDPROM checksum OK
	#
	li	v0,32			# max bytes in IDPROM
	li	v1,0			# initialize checksum
	.set	noat
1:
	lbu	AT,0(a0)
	addu	v1,AT
	#	check if passed max bytes in IDPROM
	addu	a0,4
	sub	v0,1
	bgt	v0,zero,1b
	.set	at

	and	v1,0xff
	beq	v1,zero,1f		# if checksum OK, then Intrepid
5:	li	v0,BRDTYPE_R3030
	j	ra
1:
        lhu     v0,SCR|K1BASE
        andi    v0,SCR_KEY0
        bne     v0,zero,4f
        li      v0,BRDTYPE_M180
        j       ra
4:	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra

2:		
	# something mismatched hence Mbox
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
#ifdef	R3030
	lw	v0,machine_type
	j	ra
#else
#ifdef INTREPID
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#endif INTREPID
#endif	R3030
#endif SABLE
	END(get_machine_type)


LEAF(warmreset)
	move	a3, ra
	jal	get_machine_type
	beq	v0,BRDTYPE_R3200,1f
	beq	v0,BRDTYPE_RB3125,1f
	li	v0,PROM_REBOOT
	j	v0		# I guess just do a reboot
	j	2f
1:
	li	v0,CON_SWWARMRESET
	sb	v0,CPU_CON_M2000|K1BASE	# set reset bit in config register
2:	# SHOULDN'T GET HERE
	j	exit
	END(warmreset)
