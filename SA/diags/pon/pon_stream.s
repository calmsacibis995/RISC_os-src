rcsid:	.asciiz	"$Header: pon_stream.s,v 1.4.7.1 90/07/18 14:33:08 huang Exp $"
/* $Copyright$ */

/*
 *	R3000 instruction streaming function.
 *	A 0 is returned upon success.
 *
 *	Functional Description:
 *
 *	Verifies that the R3000 on the R3200 CPU board is doing instruction
 *	streaming on cache misses for the default instruction cache block size.
 *	This is checked by reading the TLB RANDOM register just before and near
 *	the end of the instruction streaming block.
 *
 *	Execution of all instructions within the streaming block are verified.
 *
 *	With streaming enabled, the cycle count should be 12 + n, where n is
 *	the instruction cache block size.  With streaming disabled, the cycle
 *	count should be 12 + (2 * n).
 *
 *	User Information:
 *
 *	Requires working main memory at physical address 1024.
 *	Expected block size is 16.
 */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "pon.h"

#define PRINT(x, y, z) \
		la	a0,failure; \
		jal	pon_puts; \
		nop; \
		la	a0,address; \
		jal	pon_puts; \
		nop; \
		move	a0,x; \
		jal	pon_puthex; \
		nop; \
		la	a0,expect; \
		jal	pon_puts; \
		nop; \
		move	a0,y; \
		jal	pon_puthex; \
		nop; \
		la	a0,actual; \
		jal	pon_puts; \
		nop; \
		move	a0,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,xormsg; \
		jal	pon_puts; \
		nop; \
		xor	a0,y,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,crlf; \
		jal	pon_puts; \
		nop

		.globl	init_cache
		.globl	error_data
		.globl	invalidate_cache
		.extern	success
		.extern	failure
		.extern	skipped
		.extern crlf

#undef	DEBUG

#define	OFFSET			1024
#define	KSEG0			+(K0BASE+OFFSET)
#define	KSEG1			+(K1BASE+OFFSET)

#define	STREAM_CYCLES		28		/* 12 fixup cycles + 16 cycles */
						/*   for the instruction block */

		.align	12

DEPTH=	4*1
NESTED(Pon_Stream, DEPTH, zero)

	/*
	 * If the processor type is not a R3000, skip this test.
	 */

		move 	s7,ra
		lw	s0,machine_type
		bne	s0,BRDTYPE_R3200,skipit

		subu	sp,DEPTH
		sw	ra,DEPTH-4(sp)

		li	a0,PON_STREAM
		jal	pon_set_leds
		
	 	la	a0,begintest
		jal	pon_puts

		jal	GetDepend
		and	v0,PON_FAULT_CACHE
		beq	v0,zero,run


		la	a0,skipped
		jal	pon_puts
		b	norun
run:
		jal	init_cache

		jal	invalidate_cache

	/*
	 * Move the instruction streaming code to uncached memory.
	 */
		la	t0,2f
		li	t1,KSEG1
		la	t3,3f
1:
		lw	t2,(t0)
		sw	t2,(t1)
		add	t0,4
		add	t1,4
		blt	t0,t3,1b

		b	3f
2:
		.set	noreorder
#ifndef	DEBUG
		add	t2,0x0001		# 1 - start of streaming code
#endif	DEBUG
		add	t2,0x0002		# 2
		add	t2,0x0004		# 3
		add	t2,0x0008		# 4
		add	t2,0x0010		# 5
		add	t2,0x0020		# 6
		add	t2,0x0040		# 7
		add	t2,0x0080		# 8
		add	t2,0x0100		# 9
		add	t2,0x0200		# 10
		add	t2,0x0400		# 11
		add	t2,0x0800		# 12
		add	t2,0x1000		# 13
		add	t2,0x2000		# 14
		mfc0	t1,C0_RAND		# 15 - ending count
		nop
		add	t2,0x4000		# 16
#ifdef	DEBUG
		swl	zero,5(t6)		# debug trigger, tribyte write
#endif	DEBUG
		j	t3
		nop
		.set	reorder
3:
		li	t1,0xdeadbeef		# initialize ending cycle
		move	t2,zero			# initialize count
		la	t3,4f			# return address

		.set	noreorder
#ifdef	DEBUG
		li	t6,0xa1000000		# debug only
#endif	DEBUG
		li	t4,KSEG0
		j	t4			# jump to instruction streaming block
		mfc0	t0,C0_RAND		# starting count
		nop
4:
		.set	reorder

	/*
	 * Check computation result.
	 */
#ifndef	DEBUG
		li	a1,0x7fff
#else	DEBUG
		li	a1,0x7ffe
#endif	DEBUG
		bne	a1,t2,Fail

1:

	/*
	 * Check ending cycle count.
	 */
		li	a1,0xdeadbeef
		beq	a1,t1,Fail
1:

	/*
	 * Check cycle count.
	 */
		move	t4,t0
		move	t5,t1
		srl	t0,TLBRAND_RANDSHIFT
		srl	t1,TLBRAND_RANDSHIFT
		ble	t1,t0,8f

		subu	t0,t1
		and	t0,NTLBENTRIES-1
		subu	t0,NWIREDENTRIES
		b	9f
8:
		subu	t0,t1
9:
		li	a1,STREAM_CYCLES
		bne	a1,t0,Fail

Sucess:
		mfc0	s1,C0_SR
		nop
		la 	a0,success
		jal	pon_puts	
norun:
		move	v0,zero
		j	done
Fail:
		mfc0	s0,C0_SR
		nop
		la	a0,failure
		jal	pon_puts
		li	a0,PON_STREAM
		jal	FastFlash
		li	a0,PON_STREAM
		jal	pon_set_leds
		li	v0,1 
done:
		mfc0	s0,C0_SR
		nop
		and	s0,~SR_ISC
		mtc0	s0,C0_SR		# un-isolate caches
		nop
		lw	ra,DEPTH-4(sp)
		addu	sp,DEPTH
		j	ra
skipit:
		move    v0,zero
		j 	ra


END(IStreaming)

		.data


begintest:
        	.asciiz "Instruction streaming test..."
address:
		.asciiz "ERROR; Address: "
expect:
		.asciiz "  Expected: "
actual:
		.asciiz	"  Actual: "
xormsg:
		.asciiz "  Xor: "
