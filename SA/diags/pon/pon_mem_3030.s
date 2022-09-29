#ident "$Header: pon_mem_3030.s,v 1.2.1.1 90/07/18 14:32:16 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 *	Functional Description:
 *
 *	Parity Toggling Test
 *
 *	This test writes data patterns which will cause both even and odd
 *	parity to be set in memory.  All words in both memory banks will have
 *	the parity bit toggled.  After each write pass, the data is read back
 *	cached to speed up the test.
 *
 *	This test does not verify the data.  It only checks for parity problems
 *	which will be signaled by a data bus error.
 *
 *	Knaizuk Hartmann Memory Test
 *
 *	This algorithm is used to perform a fast but non-ehaustive memory test.
 *	It will test a memory subsystem for stuck-at faults in both the address
 *	lines as well as the data locations.  Wired or memory array behavior
 *	and non-creative decoder design are assumed.  It makes only 4n memory
 *	accesses where n is the number of locations to be tested.  This
 *	algorithm trades completeness for speed.  No free lunch around here.
 *	Hence this algorithm is not able to isolate pattern sensitivity or
 *	intermittent errors.  C'est la vie.  This algorithm is excellent when a
 *	quick memory test is needed.
 *
 *	The algorithm breaks up the memory to be tested into 3 partitions.
 *	Partion 0 consists of memory locations 0, 3, 6, ... partition 1
 *	consists of memory locations 1, 4, 7, ... partition 2 consists of
 *	locations 2, 5, 8, ...  The partitions are filled with either an all
 *	ones pattern or an all zeroes pattern.  By varying the order in which
 *	the partitions are filled and then checked, this algorithm manages to
 *	check all combinations of possible stuck at faults.  If you don't
 *	believe me, you can find a rigorous mathematical proof (set notation
 *	and every thing) in a correspondence called "An Optimal Algorithm for
 *	Testing Stuck-at Faults in Random Access Memories" in the November 1977
 *	issue of IEEE Transactions on Computing, volume C-26 #11 page 1141.
 *
 *	The partitions and their patterns:
 *		0	1	2
 *		-	-	-
 *			0	0	set to zeroes
 *		1			set to ones
 *			v		verify still zeroes
 *			1		set to ones
 *				v	verify still zeroes
 *		v	v		verify still ones
 *		0			set to zeroes
 *		v			verify still zeroes
 *				1	set to ones
 *				v	verify still ones
 *
 *	NOTE: Expects SP to contain the machine type ID.
 *	      This routine expects to be run before any C-code routines.
 *	      Therefore, it makes use of the S registers.
 *
 *	RETURNS: A 0 if the memory has passed the test.
 */

#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/param.h"
#include "pon.h"
#include "prom.h"

#define MEM_4MBIT	0x4
#define MEM_1MBIT	0xFFFFFFFB
#define MEM_8MBYTE	K1BASE|0x800000

#define PRINT(x, y, z) \
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

#undef	DELAY
#define	DELAY(x) \
		li	a2,+(x); \
1: ; \
		subu	a2,1; \
		bne	zero,a2,1b

		.extern	crlf
		.extern	failure
		.extern	success

		.text

LEAF(Pon_Memory)

		move	s7,ra			# save return address
		li	s0,0			# initialize error flag

		la	a0,begintest
		jal	pon_puts

		jal	SizeMemory

		bne	v0,zero,1f		# found memory

		la	a0,nomem
		jal	pon_puts
2:
		jal	FastFlash

		b	2b			# just loop forever
1:
		move	gp,v0			# save memory size

	/*
	 * Setup the exception handler for any bus errors.
	 */
		la	fp,1f
		li	v0,SR_BEV
		.set	noreorder
		mtc0	v0,C0_SR		# state unknown on reset
		nop
		mtc0	zero,C0_CAUSE		# clear software interrupts
		.set	reorder

		b	2f
1:
		li	s0,1
		li	a0,PON_FAULT_FAULT
		jal	SetDepend

		la	a0,end
		j	a0
2:
		li	a0,SYS_CREG|K1BASE
		li	a1,CR_ENA_BUZZER_B|CR_CLR_ERR_REG
		sw	a1,(a0)			# clear system error register

		li	a1,CR_ENA_BUZZER_B|CR_ENA_PAR_CHK
		sw	a1,(a0)			# enable parity checking

		li	s2,0x5a5a5a5a		# even number of bits set
		li	s3,0xa7a7a7a7		# odd number of bits set

	/*
	 * Try to use one of the caches to speed this test up.
	 */
		jal	GetDepend

		and	v1,v0,PON_FAULT_ICACHE
		bne	v1,zero,1f		# i-cache failed, check d-cache

		li	s1,SR_BEV|SR_SWC|SR_ISC	# use i-cache, isolated
		b	3f
1:
		and	v1,v0,PON_FAULT_DCACHE
		bne	v1,zero,start		# both caches bad, run uncached

		li	s1,SR_BEV|SR_ISC	# use d-cache, isolated
3:
		jal	invalidate_cache

		la	a0,start
		la	a1,start-K0SIZE
		la	a2,end
		li	a3,SR_BEV
1:
		.set	noreorder
		lw	v1,(a0)			# get word from PROM
		addu	a0,4			# bump PROM pointer
		mtc0	s1,C0_SR		# isolate cache
		nop
		sw	v1,(a1)			# store word in cache
		nop
		lw	v0,(a1)			# read from cache
		nop
		mtc0	a3,C0_SR		# un-isolate cache
		bne	v0,v1,start		# compare failed - don't run cached
		nop

		and	s1,~SR_ISC		# select cache, un-isolated
		mtc0	s1,C0_SR
		.set	reorder

		la	v0,start-K0SIZE		# run cached I-fetches
		j	v0
start:

	/*
	 * Make sure each of the two memory bank gets both even and odd
	 * data.
	 */
		li	s5,K1BASE		# starting address
		or	s6,s5,gp		# ending address
1:
		sw	s2,(s5)			# even in bank 0
		sw	s2,4(s5)		# even in bank 1
		sw	s3,8(s5)		# odd in bank 0
		sw	s3,12(s5)		# odd in bank 1
		addu	s5,4*4
		blt	s5,s6,1b

	/*
	 * Speed up the test by doing block fetches.
	 */
		la	a0,invalidate_cache
		jal	ra,a0

		li	s5,K0BASE
		or	s6,s5,gp		# ending address
1:
		lw	zero,(s5)
		addu	s5,4*8
		blt	s5,s6,1b

	/*
	 * Swap the even and odd data around.
	 */
		li	s5,K1BASE		# starting address
		or	s6,s5,gp		# ending address
1:
		sw	s3,(s5)			# odd in bank 0
		sw	s3,4(s5)		# odd in bank 1
		sw	s2,8(s5)		# even in bank 0
		sw	s2,12(s5)		# even in bank 1
		addu	s5,4*4
		blt	s5,s6,1b

		la	a0,invalidate_cache
		jal	ra,a0

		li	s5,K0BASE
		or	s6,s5,gp		# ending address
1:
		lw	zero,(s5)
		addu	s5,4*8
		blt	s5,s6,1b

		la	v0,end			# run cached I-fetches
		j	v0
end:
		li	a0,SYS_CREG|K1BASE
		li	a1,CR_ENA_BUZZER_B|CR_CLR_ERR_REG
		sw	a1,(a0)			# clear system error register
		li	a1,CR_ENA_BUZZER_B
		sw	a1,(a0)			# disable parity checking
		li	a0,0x84000000		# purge FIFO, disable channel,
						#   ... clear error
		sw	a0,K1BASE|DMA_MODE1	# disable RAMBO channel 1
		sw	a0,K1BASE|DMA_MODE2	# disable RAMBO channel 2

	/*
	 * The chicken and egg problem with write buffers and memory.  Can't
	 * test one without the other working.
	 */
		jal	GetDepend

		and	a3,v0,PON_FAULT_CACHE	# cache failure flag

		li	s5,K1BASE|PON_LOMEMEND	# cached starting address
		or	s6,gp,K1BASE		# ending address
#ifdef	TEST
		li	s5,K1BASE+0x000000	# starting address
		li	s6,K1BASE+0x800000	# ending address
#endif	TEST

#ifdef	DEBUG
		la	a0,memstart_msg		# display start address message
		jal	pon_puts

		move	a0,s5
		jal	pon_puthex		# display start address

		la	a0,memend_msg		# display end address message
		jal	pon_puts

		move	a0,s6			# end address found
		jal	pon_puthex		# display end address

		la	a0,crlf
		jal	pon_puts
#endif	DEBUG

		sub	s6,8			# a1 has last address + 4,
						#   one word < last address
						#   to avoid bounds check
		bne	a3,zero,4f		# previous cache error

		li	a0,PON_LOMEMEND
		sub	s6,a0
		jal	invalidate_cache
		li	a0,PON_MEMTESTED|K0BASE
		la	a1,4f
		la	a2,endcopy
		jal	move_code
		j	v0

		la	v0,4f-K0SIZE		# run cached I-fetches
		j	v0

 #  Set partitions 1 and 2 to 0's.
4:
		move	s3,zero			# set expected data
		addu	s4,s5,4			# init addr ptr to partition 1
$32:
		sw	s3,0(s4)		# write partition 1 data
		sw	s3,4(s4)		# write partition 2 data
		addu	s4,s4,12		# bump addr 3 words to part'n 1
		bleu	s4,s6,$32		# done yet?

 #  Set partition 0 to ones.
		li	s3,-1			# set expected data
		move	s4,s5			# re-initialize pointer
$35:
		sw	s3,0(s4)		# write partition 0 data
		addu	s4,s4,12		# bump ptr address up 3 words
						#   to partition 0 again
		bleu	s4,s6,$35		# done yet?

 #  Verify partition 1 is still 0's.
		move	s3,zero			# set expected data
		addu	s4,s5,4			# re-init ptr to partition 1
$37:
		lw	s2,0(s4)		# read data
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 1 again
		bleu	s4,s6,$37		# done yet?

 # Set partition 1 to ones.
		li	s3,-1			# set expected data
		addu	s4,s5,4			# set address pointer to
						#   partition 1 start
$40:
		sw	s3,0(s4)		# write ones
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 1 again
		bleu	s4,s6,$40		# done yet?

 #  Verify that partition 2 is zeroes
		move	s3,zero			# set expected data
		addu	s4,s5,8			# re-init addr ptr to part 2
$42:
		lw	s2,0(s4)		# read location
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 2 again
		bleu	s4,s6,$42		# done yet?

 #  Verify that partitions 0 and 1 are still ones.
		li	s3,-1			# set expected data
		subu	s4,s4,4			# drop addr ptr back to part 1
		addu	s6,s6,4			# point reference addr
						#   to "last" address
		bgtu	s4,s6,$131		# is part 1 addr > "last"?

		sw	s3,0(s4)		# no! then write ones
						#   to partition 1 address
$131:
		subu	s6,s6,4			# put reference address back
		move	s4,s5			# re-init addr ptr to part 0
$45:
		lw	s2,0(s4)		# read partition 0 data
		bne	s2,s3,error		# still 1's?

		addu	s4,s4,4			# bump addr to partition 1
		lw	s2,0(s4)		# read partition 1 data
		bne	s2,s3,error		# still 1's?

		addu	s4,s4,8			# bump addr ptr 2 words to
						#   partition 0 again
		bleu	s4,s6,$45		# done yet?

 #  Set partition 0 to zeroes.
		move	s3,zero			# set expected data
		move	s4,s5			# re-init ptr to partition 0
$49:
		sw	s3,0(s4)		# write partition 0 data
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 0 again
		bleu	s4,s6,$49		# done yet?

 # Check partition 0 for zeroes.
		move	s4,s5			# re-init addr ptr to part 0
$51:
		lw	s2,0(s4)		# read partition 0 data
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 0 again
		bleu	s4,s6,$51		# done yet?
$53:
 # Set partition 2 to 1's
		li	s3,-1			# set data pattern
		addu	s4,s5,8			# re-init addr ptr to part 2
$54:
		sw	s3,0(s4)		# write partition 2 data
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 2 again
		bleu	s4,s6,$54		# done yet?

 #  Check partition 2 for ones
		addu	s4,s5,8			# re-init addr ptr to part 2
$56:
		lw	s2,0(s4)		# read partition 2 data
		bne	s2,s3,error		# is it still 1's?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#   partition 2 again
		bleu	s4,s6,$56		# done yet?

 #  Set partitions 1 and 2 to 0's for the Operating System.
		move	s3,zero			# set expected data
		addu	s4,s5,4			# init addr ptr to partition 1
$58:
		sw	s3,0(s4)		# write partition 1 data
		sw	s3,4(s4)		# write partition 2 data
		addu	s4,s4,12		# bump addr 3 words to part 1
		bleu	s4,s6,$58		# done yet?

		addu	s4,s6,4			# put last addr back where
						#   it should be
		li	s3,0x55555555		# set data pattern
		sw	s3,0(s4)		# write it
		lw	s2,0(s4)		# read it
		bne	s2,s3,error		# correct?

		li	s3,0xAAAAAAAA		# set data pattern
		sw	s3,0(s4)		# write it
		lw	s2,0(s4)		# read it
		beq	s2,s3,1f		# correct?
error:
		la	a0,error1		# revert back to
		j	a0			#   noncached i-fetches

1:
		sw	zero,0(s4)		# set last location to zero
endcopy:
		la	a0,1f			# revert back to
		j	a0			#   noncached i-fetches
1:
		beq	s0,zero,pass		# check is parity test failed

		la	a0,failure
		jal	pon_puts

		jal	FastFlash

		li	v0,1
		j	s7
pass:
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s7
 # Failed!
error1:
		la	a0,failure
		jal	pon_puts

		PRINT(s4, s3, s2)		# print address, expected,
						#   actual, and Xor
error2:
		jal	FastFlash
norun:
		li	a0,PON_FAULT_MEM
		jal	SetDepend

		li	v0,1
		j	s7

Pon_Memory_end:

END(Pon_Memory)


LEAF(SizeMemory)

		move	t9,ra

		jal	GetKCnfR		# get keyboard register
		move	a0,v0
		ori	a0,a0,MEM_4MBIT		# set 4MBit type
		jal	SetKCnfR		# write to keyboard register
		DELAY(200)

		li	t0,K1BASE		# starting address, bank 0
		li	t1,0x55aaff00		# pattern
		sw	t1,0(t0)		# write to it
		li	t2,MEM_8MBYTE		# 8 MByte address, bank 0
		li	t3,0xa5a5f0f0		# pattern
		sw	t3,0(t2)		# write to it

		lw	t4,0(t0)		# read back starting address
		bne	t4,t1,1f		# not equal, 1MBit (overwrite)

		lw	t4,0(t2)		# read back 8Mbyte address
		bne	t4,t3,2f		# pattern match, 4 MBit

#ifdef DEBUG
		la	a0,memtp_4_msg		# print MemType
		jal	pon_puts
#endif DEBUG

		/*
		 * Size memory, 4 Mbit.
		 */
		li	t0,(K1BASE | 0x2000000)	# starting address of a bank
		li	t1,0x55aaff00		# pattern
		li	t2,0x800000		# memory increment (8 MB)
		li	t3,0x2000000		# starting memory size
		li	t5,K1BASE		# tmp memory location
		li	t6,0x5a5a5a5a		# tmp pattern
4:
		sw	t1,0(t0)		# write pattern
		jal	FlushWB
		sw	t6,0(t5)		# write tmp pattern to tmp loc
		jal	FlushWB
		lw	t4,0(t0)		# read it back
		jal	FlushWB
		bne	t4,t1,5f		# keep going if equal
		addu	t3,t3,t2		# increment memory size by 8 MB
		addu	t7,t0,t2		# next bank
		sw	t6,0(t7)		# write tmp pattern to tmp loc
		jal	FlushWB
		lw	t4,0(t0)		# read it back
		bne	t4,t1,5f		# keep going if equal
		addu	t0,t0,t2		# next bank
		b	4b
5:
		move	v0,t3			# store memory size
		b	3f
1:
		bne	t4,t3,2f		# not equal, something wrong

		/*
		 * Set 1 Mbit.
		 */
		jal	GetKCnfR		# get keyboard register
		move	a0,v0
		and	a0,a0,MEM_1MBIT		# set 1MBit type
		jal	SetKCnfR		# write to keyboard register
		DELAY(200)

#ifdef DEBUG
		la	a0,memtp_1_msg		# print MemType
		jal	pon_puts
#endif DEBUG

		/*
		 * Size memory.
		 */
		li	t0,(K1BASE|0x800000)	# starting address of a bank
		li	t1,0x55aaff00		# pattern
		li	t2,0x800000		# memory increment (8 MB)
		li	t3,0x800000		# starting memory size
		li	t5,K1BASE		# tmp memory location
		li	t6,0x5a5a5a5a		# tmp pattern
6:
		sw	t1,0(t0)		# write pattern
		jal	FlushWB
		sw	t6,0(t5)		# write tmp pattern to tmp loc
		jal	FlushWB
		lw	t4,0(t0)		# read it back
		jal	FlushWB
		bne	t4,t1,7f		# keep going if equal
		addu	t0,t0,t2		# next bank
		addu	t3,t3,t2		# increment memory size by 8 MB
		b	6b
7:
		move	v0,t3			# store memory size
		b	3f
2:
		move	v0,zero			# memory size has no meaning
3:
#ifdef DEBUG
		move	a0,v0
		jal	pon_puthex
		la	a0,crlf
		jal	pon_puts
#endif DEBUG

		j	t9

END(SizeMemory)

		.data

begintest:
		.asciiz "Memory Tests..."

#ifdef DEBUG
memtp_1_msg:	.asciiz "MemType: 1 MBit.     "

memtp_4_msg:	.asciiz "MemType: 4 MBit.     "

mem_err_msg:	.asciiz "Compare error in probing MemType."

kybd_err_msg:	.asciiz "Keyboard self-test failed."

memstart_msg:
		.asciiz "Start Address: "
memend_msg:
		.asciiz " End Address: "
#endif	DEBUG

EXPORT(paddress)
		.asciiz "  Address: "
EXPORT(pexpect)
		.asciiz "  Expected: "
EXPORT(pactual)
		.asciiz "  Actual: "
EXPORT(pxor)
		.asciiz "  Xor: "
EXPORT(nomem)
		.asciiz "NO Memory Found\r\n"
