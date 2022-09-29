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
/* $Header: bcopy.s,v 1.2.1.6 90/05/17 15:50:57 wje Exp $ */


#include "regdef.h"
#include "asm.h"

#define	NBPW	4

/*
 * bcopy(src, dst, bcount)
 *
 * MINCOPY is minimum number of byte that its worthwhile to try and
 * align copy into word transactions.
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
 * Someday maybe call ovbopy explicitely in code.
 * For now just check here.
 */
        bgeu    a0,a1,1f                # src >= dst, no overlap error
        addu    v0,a0,a2                # src endpoint + 1
        bgeu    a1,v0,1f                # dst >= src endpoint+1, no overlap err
        j       ovbcopy                 # handle overlap; returns when done
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
 * This is coded to work optimally for 8-word or 16-word line sizes
 * with page-mode DRAMs and write chaining.
 */
blkcopy:
	and	a3,a2,~63		# total space in 64 byte chunks
	subu	a2,a3			# count after by-64 byte loop done
	beq	a3,zero,wordcopy	# less than 64 bytes to copy
	addu	a3,a0			# source endpoint
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
.end bcopy
