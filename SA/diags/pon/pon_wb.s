#ident "$Header: pon_wb.s,v 1.9.5.1 90/07/18 14:34:11 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restrictive Rights Legend                        |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#  $ */

/*
 *	Functional Description:
 *
 *	Generates write buffer conflict signal.  Mass quantities of writes
 *	followed by a read should do it.  Checks all combinations of word,
 *	halfword, byte and tribyte writes and reads.
 *
 *	If there were no cache test errors, this test runs cached.  It makes
 *	two passes, one to fill the cache and the next to run from cache.
 *
 *	If there were cache test errors, this test runs uncached and makes
 *	only one pass.
 *
 *	NOTE: Expects SP to contain the machine type ID.
 *	      This routine expects to be run before any C-code routines.
 *	      Therefore, it makes use of the S registers.
 */

#include "mips/asm.h"
#include "mips/regdef.h"
#include "mips/mach_ops.h"
#include "mips/cp0.h"
#include "mips/cpu_board.h"
#include "pon.h"

#define	PAT			0x12345678

#define	BYTE3_MASK		0xff
#define	BYTE2_MASK		0xff00
#define	BYTE1_MASK		0xff0000
#define	BYTE0_MASK		0xff000000

#define	HALF1_MASK		0xffff
#define	HALF0_MASK		0xffff0000

#define	LEFT_MASK		0xffffff
#define	RIGHT_MASK		0xffffff00

#define	BYTE3(pat)		(pat & BYTE3_MASK)
#define	BYTE2(pat)		((pat & BYTE2_MASK) >> 8)
#define	BYTE1(pat)		((pat & BYTE1_MASK) >> 16)
#define	BYTE0(pat)		((pat & BYTE0_MASK) >> 24)

#define	HALF1(pat)		(pat & HALF1_MASK)
#define	HALF0(pat)		((pat & HALF0_MASK) >> 16)

#define	LEFT(pat)		((pat << 8) & RIGHT_MASK)
#define	RIGHT(pat)		((pat >> 8) & LEFT_MASK)

#define	MIX1(mask, shr)		((~PAT & ~mask) | ((PAT >> shr) & mask))

#define	MIX0(mask, shl)		((~PAT & ~mask) | ((PAT << shl) & mask))

#define	WBFILL \
		.set	noreorder; \
		sw	v1,0(s4); \
		sw	v1,0(s5); \
		sw	v1,0(s6); \
		sw	v1,0(s7)

		.extern	success
		.extern	failure
		.extern	skipped
		.text

LEAF(Pon_WB)

		move	s1,ra			# save return address
#ifndef	R3030
		li	a0,PON_WB		# just begun
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

		jal	SizeMemory

		beq	v0,zero,error		# no memory found

		jal	GetDepend

		and	v0,PON_FAULT_CACHE
		beq	v0,zero,cached

		li	a3,1			# make 1 pass
		b	run
cached:
		li	a3,2			# make 2 passes
#ifndef	R3030
		bne	sp,BRDTYPE_M180,1f
#endif	!R3030
		jal	invalidate_cache
		li	a0,PON_MEMTESTED|K1BASE
		la	a1,run
		la	a2,2f
		jal	move_code
		j	v0
		
1:
		la	s0,run-K0SIZE		# run cached I-fetches
		j	s0
run:
		.set	noreorder
		la	s4,(PON_SCRATCHMEM + 0x30000) | K1BASE
		la	s5,(PON_SCRATCHMEM + 0x30004) | K1BASE
		la	s6,(PON_SCRATCHMEM + 0x30008) | K1BASE
		la	s7,(PON_SCRATCHMEM + 0x3000c) | K1BASE
		li	s0,PAT			# write data
		li	v1,~PAT			# background write data
		.set	reorder
start:
	/*
	 *  Word write.
	 */
		WBFILL
		sw	s0,0(s4)		# word write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s0,s3,error

		li	s2,HALF0(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,HALF1(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,BYTE0(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,BYTE1(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,BYTE2(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,BYTE3(PAT)		# expected data
		WBFILL
		sw	s0,0(s4)		# word write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,LEFT(PAT)		# expected data
		move	s3,zero
		WBFILL
		sw	s0,0(s4)		# word write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

		li	s2,RIGHT(PAT)		# expected data
		move	s3,zero
		WBFILL
		sw	s0,0(s4)		# word write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	s2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s0,s3,error

	/*
	 *  Halfword 0 write.
	 */
#define	PAT_H0		MIX0(HALF0_MASK, 16)

		li	s2,PAT_H0		# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_H0)	# expected data
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_H0)		# expected data
		move	s3,zero
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_H0)	# expected data
		move	s3,zero
		WBFILL
		sh	s0,0(s4)		# halfword 0 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Halfword 1 write.
	 */
#define	PAT_H1		MIX1(HALF1_MASK, 0)

		li	s2,PAT_H1		# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_H1)	# expected data
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_H1)		# expected data
		move	s3,zero
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_H1)	# expected data
		move	s3,zero
		WBFILL
		sh	s0,2(s4)		# halfword 1 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Byte 0 write.
	 */
#define	PAT_B0		MIX0(BYTE0_MASK, 24)

		li	s2,PAT_B0
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_B0)	# expected data
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_B0)		# expected data
		move	s3,zero
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_B0)	# expected data
		move	s3,zero
		WBFILL
		sb	s0,0(s4)		# byte 0 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Byte 1 write.
	 */
#define	PAT_B1		MIX0(BYTE1_MASK, 16)

		li	s2,PAT_B1		# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_B1)	# expected data
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_B1)		# expected data
		move	s3,zero
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_B1)	# expected data
		move	s3,zero
		WBFILL
		sb	s0,1(s4)		# byte 1 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Byte 2 write.
	 */
#define	PAT_B2		MIX0(BYTE2_MASK, 8)

		li	s2,PAT_B2		# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_B2)	# expected data
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_B2)		# expected data
		move	s3,zero
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_B2)	# expected data
		move	s3,zero
		WBFILL
		sb	s0,2(s4)		# byte 2 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Byte 3 write.
	 */
#define	PAT_B3		MIX0(BYTE3_MASK, 0)

		li	s2,PAT_B3		# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_B3)	# expected data
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_B3)		# expected data
		move	s3,zero
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_B3)	# expected data
		move	s3,zero
		WBFILL
		sb	s0,3(s4)		# byte 3 write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Left tribyte write.
	 */
#define	PAT_L		MIX1(LEFT_MASK, 8)

		li	s2,PAT_L		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_L)		# expected data
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_L)		# expected data
		move	s3,zero
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_L)		# expected data
		move	s3,zero
		WBFILL
		swl	s0,1(s4)		# left tribyte write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

	/*
	 *  Right tribyte write.
	 */
#define	PAT_R		MIX0(RIGHT_MASK, 8)

		li	s2,PAT_R		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lw	s3,0(s4)		# word read
		nop
		.set	reorder

		bne	s2,s3,error

		li	t2,HALF0(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lhu	s3,0(s4)		# halfword 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,HALF1(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lhu	s3,2(s4)		# halfword 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE0(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lbu	s3,0(s4)		# byte 0 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE1(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lbu	s3,1(s4)		# byte 1 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE2(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lbu	s3,2(s4)		# byte 2 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,BYTE3(PAT_R)		# expected data
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lbu	s3,3(s4)		# byte 3 read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,LEFT(PAT_R)		# expected data
		move	s3,zero
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lwl	s3,1(s4)		# left tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error

		li	t2,RIGHT(PAT_R)		# expected data
		move	s3,zero
		WBFILL
		swr	s0,2(s4)		# right tribyte write
		lwr	s3,2(s4)		# right tribyte read
		nop
		.set	reorder

		bne	t2,s3,error

		lw	s3,0(s4)		# word read check
		nop
		bne	s2,s3,error
end:
		sub	a3,1			# decrement pass count
		bne	a3,zero,run		# next pass if not 0

		la	a2,2f
		j	a2			# run uncached
2:
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s1
error:
		la	a0,failure
		jal	pon_puts

		li	a0,PON_WB
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_WB
		jal	pon_set_leds
#endif	!R3030

		li	a0,PON_FAULT_WB
		jal	SetDepend

		li	v0,1
		j	s1

END(Pon_WB)

		.data

begintest:
		.asciiz	"Write Buffer Test..."
