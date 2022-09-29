#ident "$Header: pon_cache4.s,v 1.9.1.1 90/07/18 14:30:06 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	Tests instruction cache data for addressing and stuck-at faults.
 *
 *	NOTE: Expects SP to contain the machine type ID.
 *	      This routine expects to be executed before a stack is
 *	      established.  Therefore, it used the S registers freely.
 *
 *	RETURNS: A 0 if the memory has passed the test.
 */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "machine/cp0.h"
#include "pon.h"

#define PRINT(x, y, z)	\
		la	a0,paddress; \
		jal	pon_puts; \
		nop; \
		move	a0,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,pexpect; \
		jal	pon_puts; \
		nop; \
		move	a0,x; \
		jal	pon_puthex; \
		nop; \
		la	a0,pactual; \
		jal	pon_puts; \
		nop; \
		move	a0,y; \
		jal	pon_puthex; \
		nop; \
		la	a0,pxor; \
		jal	pon_puts; \
		nop; \
		xor	a0,x,y; \
		jal	pon_puthex; \
		nop; \
		la	a0,crlf; \
		jal	pon_puts; \
		nop

		.extern	init_cache
		.extern	size_cache
		.extern	success
		.extern	failure
		.extern	skipped
		.extern	crlf
		.extern	paddress
		.extern	pexpect
		.extern	pactual
		.extern	pxor

		.text

LEAF(Pon_Cache4)

		move	s5,ra
#ifndef	R3030
		li	a0,PON_CACHE4		# just begun
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

		jal	GetDepend

		and	v0,PON_FAULT_ICACHE
		beq	v0,zero,run

		la	a0,skipped
		jal	pon_puts

		b	norun
run:
		.set noreorder
		mfc0	s1,C0_SR		# save the SR
		.set reorder
		jal	init_cache

		nop
		nop
		li	v0,SR_ISC|SR_SWC|SR_BEV	# isolate and swap caches
		.set noreorder
		mtc0	v0,C0_SR
		.set reorder
		nop
		nop
		jal	size_cache

		bltz	v0,2f			# size_cache error

		move	a1,v0			# cache size returned
		li	s0,-1			# init to all one's
		move	a0,zero			# first address
		move	s4,a0			# save first address (0)
		move	s3,a1			# last address + 4

		/*
		 * Start of mats+ algorithm performs mats+ test for addresses
		 * from firstaddr up to but not including lastaddr+1.  Fill
		 * from firstaddr+4 to lastaddr with 0's.
		 */
		addu	a0,4
1:
		sw	zero,K0BASE(a0)
		addu	a0,4
		blt	a0,s3,1b

		/*
		 * Fill firstaddr with 1's.
		 */
		move	a0,s4
		sw	s0,K0BASE(a0)
		addu	a0,4

		/*
		 * From firstaddr+4 to lastaddr read and verify 0, then write
		 * 1's.
		 */
1:
		lw	v0,K0BASE(a0);
		bne	v0,zero,2f

		sw	s0,K0BASE(a0)
		addu	a0,4
		blt	a0,s3,1b

		/*
		 * From firstaddr to lastaddr-4 read and verify 1's, then write
		 * 0's.
		 */
		move	a0,s4			# reload saved start address
		subu	s2,s3,4			# lastaddr
1:
		lw	v0,K0BASE(a0);
		bne	v0,s0,3f

		sw	zero,K0BASE(a0)
		addu	a0,4
		blt	a0,s2,1b

		/*
		 * Verify lastaddr is 1's (don't need to write).
		 */
		lw	v0,K0BASE(a0);
		bne	v0,s0,3f

		/*
		 * Verify firstaddr is 0's.
		 */
		move	a0,s4
		lw	v0,K0BASE(a0);
		bne	v0,zero,2f

		move	v0,zero			# indicate success

 # Successful!
		.set noreorder
		mtc0	s1,C0_SR		# restore SR
		.set reorder

		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s5			# and return

 # Failed!
2:
		move	s0,zero
		/* and fall into the other Failed case */
 # Failed!
3:
		.set noreorder
		mtc0	s1,C0_SR		# restore SR
		.set reorder

		move	s1,v0			# save actual data
		move	s2,a0			# save address

		la	a0,failure
		jal	pon_puts

		PRINT(s0, s1, s2)

		li	a0,PON_CACHE4
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_CACHE4
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030
norun:
		li	a0,PON_FAULT_ICACHE
		jal	SetDepend

		li	v0,1
		j	s5			# and return

END(Pon_Cache4)

		.data

begintest:
		.asciiz "Instruction Cache MATS+ Test..."
