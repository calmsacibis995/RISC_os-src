#ident "$Header: pon_iblk_refill.s,v 1.10.3.1 90/07/18 14:31:39 huang Exp $"
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
 *	R3000 instruction block refill function.
 *	A 0 is returned upon success.
 *
 *	Functional Description:
 *
 *	Verifies that the R3000 on the R3200 CPU board is doing block refill of
 *	the defined block size for the instruction cache.  The block size is
 *	jumper configurable on the R3200 but there is no means for this program
 *	to read that information, so the block size this program determines via
 *	testing is checked against the R3200 default size.
 *
 *	The R3000 can do block refills in sizes of 4, 8, 16 and 32 words.  This
 *	program verifies that refills occur at the block size defined by
 *	IBLKSIZE.
 *
 *	User Information:
 *
 *	Requires working main memory at physical address 1024.
 */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "pon.h"

		.extern	paddress
		.extern	pexpect
		.extern	pactual
		.extern	pxor
		.extern	crlf

#define PRINT(x, y, z) \
		la	a0,failure; \
		jal	pon_puts; \
		nop; \
		la	a0,paddress; \
		jal	pon_puts; \
		nop; \
		move	a0,x; \
		jal	pon_puthex; \
		nop; \
		la	a0,pexpect; \
		jal	pon_puts; \
		nop; \
		move	a0,y; \
		jal	pon_puthex; \
		nop; \
		la	a0,pactual; \
		jal	pon_puts; \
		nop; \
		move	a0,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,pxor; \
		jal	pon_puts; \
		nop; \
		xor	a0,y,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,crlf; \
		jal	pon_puts; \
		nop

		.extern	init_cache
		.extern	invalidate_cache
		.extern	error_data
		.extern	success
		.extern failure
		.extern	skipped

#define	OFFSET			1024
#define	KSEG0			+(K0BASE+OFFSET)
#define	KSEG1			+(K1BASE+OFFSET)

#define	IBLKSIZEM180		4
#define	IBLKSIZEM20		8
#define	IBLKSIZE		16
#define	MINBLKSIZE		4
#define	MAXBLKSIZE		32

#define	JUMP_S4			0x02800008	/* jr s4 */
#define	NOP			0x00000000	/* nop */

		.text

LEAF(Pon_Iblk)

	/*
	 * If the processor type is not a R3000, skip this test.
	 */
		move	a3,ra
		beq	sp,BRDTYPE_R3200,1f
		beq	sp,BRDTYPE_RB3125,1f
		beq	sp,BRDTYPE_M180,1f
		beq	sp,BRDTYPE_R3030,1f
		b	norun
1:
#ifndef	R3030
		li	a0,PON_IBLK
		jal	pon_set_leds
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

		jal	GetDepend

		and	v0,PON_FAULT_CACHE
		beq	v0,zero,run

		la      a0,skipped
		jal	pon_puts

		b	norun
run:
		jal	init_cache
		move	s0,zero			# block index
loop:
	/*
	 * Initialize memory with patterns counting from 1 through 3 times the
	 * maximum block size.
	 */
		jal	invalidate_cache

		.set noreorder
		mtc0	zero,C0_SR
		.set reorder

		li	s2,MAXBLKSIZE*3
		li	s3,1
		move	s4,zero
1:
		sw	s3,KSEG1(s4)
		add	s3,1
		add	s4,4
		subu	s2,1
		bgtz	s2,1b

		li	s2,JUMP_S4
		li	s3,NOP
		sw	s2,KSEG1+(4*MAXBLKSIZE)(s0)
		sw	s3,KSEG1+(4*MAXBLKSIZE)+4(s0)
		la	s4,return		# return address
		la	s2,KSEG0+(4*MAXBLKSIZE)(s0)
		bne	sp,BRDTYPE_M180,1f
1:
		.set	noreorder
		j	s2			# jump to cached space
return:
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		nop
		li	s2,SR_ISC|SR_SWC
		mtc0	s2,C0_SR		# isolate and swap caches
		nop
		nop
		.set	reorder

	/*
	 * Verify that area preceeding the block refill has no cache hits.
	 */
		li	s2,KSEG0		# starting address
		li	s3,1			# count
1:
		lw	s4,(s2)			# get data
		nop
		.set noreorder
		mfc0	s5,C0_SR		# get status
		nop
		.set reorder
		and	s5,SR_CM
		beq	s5,zero,Fail		# cache miss

		add	s2,4			# next address
		add	s3,1			# count
		bne	s3,MAXBLKSIZE,1b

	/*
	 * Determine block refill size by checking for cache hits.
	 */
		li	s2,KSEG0+(4*MAXBLKSIZE)	# starting address
		li	s3,MAXBLKSIZE+1		# starting expected pattern
		la	s6,KSEG0+(4*MAXBLKSIZE)(s0)
		la	s7,KSEG0+(4*MAXBLKSIZE)+4(s0)
		move	s1,zero			# initial count
1:
		lw	s4,(s2)			# get data
		.set noreorder
		mfc0	s5,C0_SR		# get status
		nop
		.set reorder
		and	s5,SR_CM
		bne	s5,zero,2f		# cache miss

		add	s1,1
		bne	s2,s6,5f		# check if at jump instruction

		li	a2,JUMP_S4		# expected data
		beq	a2,s4,3f		# check jump instruction opcode

		b	Fail
5:
		bne	s2,s7,6f		# check if at nop instruction

		li	a2,NOP			# expected data
		beq	a2,s4,3f		# check nop instruction opcode

		b	Fail
6:
		bne	s3,s4,Fail		# check cache hit data
3:
		add	s2,4			# next address
		add	s3,1			# next pattern
		ble	s1,2*MAXBLKSIZE,1b

		b	Fail
2:
		li	a1,IBLKSIZE
		beq	sp,BRDTYPE_R3200,5f
		beq	sp,BRDTYPE_RB3125,5f

		li	a1,IBLKSIZEM180
		beq	sp,BRDTYPE_M180,5f

		li	a1,IBLKSIZEM20
5:
		bne	a1,s1,Fail
4:

	/*
	 * Verify that area following the block refill has no cache hits.
	 */
		add	s2,4			# starting address
		add	s3,1			# starting expected pattern
1:
		lw	s4,(s2)			# get data
		.set noreorder
		mfc0	s5,C0_SR		# get status
		nop
		.set reorder
		and	s5,SR_CM
		beqz	s5,Fail			# cache miss
2:
		add	s2,4			# next address
		add	s3,1			# count
		ble	s3,3*MAXBLKSIZE,1b

	/*
	 * Setup for next pass to check if all words within a block refill size
	 * cause the entire block to refill.
	 */
		.set noreorder
		mfc0	s2,C0_SR
		nop
		.set reorder
		and	s2,~SR_ISC
		.set noreorder
		mtc0	s2,C0_SR		# un-isolate caches
		nop
		nop
		.set reorder

		li	a1,IBLKSIZE
		beq	sp,BRDTYPE_R3200,5f
		beq	sp,BRDTYPE_RB3125,5f

		li	a1,IBLKSIZEM180
		beq	sp,BRDTYPE_M180,5f

		li	a1,IBLKSIZEM20
5:
		add	s0,4
		blt	s0,a1,loop
Success:
		la	a0,success
		jal	pon_puts
norun:
		move	v0,zero
		j	done
Fail:
		.set noreorder
		mfc0	s0,C0_SR
		nop
		.set reorder
		and	s0,~SR_ISC
		.set noreorder
		mtc0	s0,C0_SR		# un-isolate caches
		nop
		.set reorder

#ifdef	DEBUG
		PRINT(s2,s3,s4)
#endif	DEBUG

		la	a0,failure
		jal	pon_puts

		li	a0,PON_IBLK
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_IBLK
		jal	pon_set_leds
#endif	!R3030

		li	a0,PON_FAULT_ICACHE
		jal	SetDepend

		li	v0,1
done:
		j	a3

END(Pon_Iblk)

		.data

begintest:
		.asciiz "Instruction Cache Block Refill Test..."
