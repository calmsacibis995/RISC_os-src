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
/* $Header: bzero.s,v 1.5.2.2 90/05/10 01:14:00 wje Exp $ */


#include <sys/regdef.h>
#include <sys/asm.h>

#define	NBPW	4

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
.end bzero