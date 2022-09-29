#ident "$Header: pon_mem.s,v 1.14.1.1 90/07/18 14:32:11 huang Exp $"
/* $Copyright: 
# |-----------------------------------------------------------|
# | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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

#undef	TEST
#undef	DEBUG

#define	INCR		0x100000

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

#undef	DELAY
#define	DELAY(x) \
		li	a2,+(x); \
1: ; \
		subu	a2,1; \
		bne	zero,a2,1b

		.extern failure
		.extern success
		.text

LEAF(Pon_Memory)

		move	s7,ra			# save return address
		li	a0,PON_MEMTEST		# just begun
		jal	pon_set_leds		# write to the CPU LEDS

		la	a0,begintest
		jal	pon_puts

	/*
	 * The chicken and egg problem with write buffers and memory.  Can't
	 * test one without the other working.
	 */
#ifndef	TEST
		jal	SizeMemory
#ifdef SABLE
		li	v0,0x4000		# for Sable, test only 16KB
#endif SABLE
		bne	v0,zero,3f		# found memory

		la	a0,nomem
		jal	pon_puts

		li	a0,LMEM_NOMEM_PATTERN	# setup error code
		sb	a0,0(v0)		# and write the diag location
2:
		li	a0,LMEM_NOMEM_PATTERN
		jal	FastFlash

		b	2b			# and just loop forever
3:
		move	a2,v0			# save memory size value
		jal	GetDepend
		and	a3,v0,PON_FAULT_CACHE	# cache failure flag
		move	v0,a2			# restore mem size val in v0
		bne	a3,zero,4f		# don't cache data

		bne	sp,BRDTYPE_M180,5f
		li	s5,K1BASE + PON_LOMEMEND # cached starting address
		b	6f
4:

		li	s5,K1BASE		# uncached starting address
		b	6f
5:
		li	s5,K1BASE		# cached starting address

6:
		or	s6,s5,v0		# ending address
#else	TEST
		li	s5,K0BASE+0x100000	# starting address
		li	s6,K0BASE+0x200000	# ending address
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
		bne	sp,BRDTYPE_M180,1f
		li	a0,PON_LOMEMEND
		sub	s6,a0
		jal	invalidate_cache
		li	a0,PON_MEMTESTED|K0BASE
		la	a1,4f
		la	a2,pass
		jal	move_code
		j	v0

1:		bne	sp,BRDTYPE_R6300,2f
		la	a0,4f			# copy the basic algorithm to
		la	a1,Pon_Memory_end	#  s-cache for cached i-fetches
		jal	copy_r6000_to_cacheable

2:		la	v0,4f-K0SIZE		# run cached I-fetches
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
						#  to partition 0 again
		bleu	s4,s6,$35		# done yet?

 #  Verify partition 1 is still 0's.
		move	s3,zero			# set expected data
		addu	s4,s5,4			# re-init ptr to partition 1
$37:
		lw	s2,0(s4)		# read data
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 1 again
		bleu	s4,s6,$37		# done yet?

 # Set partition 1 to ones.
		li	s3,-1			# set expected data
		addu	s4,s5,4			# set address pointer to
						#  partition 1 start
$40:
		sw	s3,0(s4)		# write ones
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 1 again
		bleu	s4,s6,$40		# done yet?

 #  Verify that partition 2 is zeroes
		move	s3,zero			# set expected data
		addu	s4,s5,8			# re-init addr ptr to part 2
$42:
		lw	s2,0(s4)		# read location
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 2 again
		bleu	s4,s6,$42		# done yet?

 #  Verify that partitions 0 and 1 are still ones.
		li	s3,-1			# set expected data
		subu	s4,s4,4			# drop addr ptr back to part 1
		addu	s6,s6,4			# point reference addr
						#  to "last" address
		bgtu	s4,s6,$131		# is part 1 addr > "last"?

		sw	s3,0(s4)		# no! then write ones
						#  to partition 1 address
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
						#  partition 0 again
		bleu	s4,s6,$45		# done yet?

 #  Set partition 0 to zeroes.
		move	s3,zero			# set expected data
		move	s4,s5			# re-init ptr to partition 0
$49:
		sw	s3,0(s4)		# write partition 0 data
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 0 again
		bleu	s4,s6,$49		# done yet?

 # Check partition 0 for zeroes.
		move	s4,s5			# re-init addr ptr to part 0
$51:
		lw	s2,0(s4)		# read partition 0 data
		bne	s2,s3,error		# is it still 0?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 0 again
		bleu	s4,s6,$51		# done yet?
$53:
 # Set partition 2 to 1's
		li	s3,-1			# set data pattern
		addu	s4,s5,8			# re-init addr ptr to part 2
$54:
		sw	s3,0(s4)		# write partition 2 data
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 2 again
		bleu	s4,s6,$54		# done yet?

 #  Check partition 2 for ones
		addu	s4,s5,8			# re-init addr ptr to part 2
$56:
		lw	s2,0(s4)		# read partition 2 data
		bne	s2,s3,error		# is it still 1's?
		addu	s4,s4,12		# bump addr ptr 3 words to
						#  partition 2 again
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
						#  it should be
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
		j	a0			#  noncached i-fetches


 # Successful!
1:
		sw	zero,0(s4)		# set last location to zero
pass:
		la	a0,1f			# revert back to
		j	a0			#  noncached i-fetches
1:
		la	a0,success
		jal	pon_puts

		li	v0,BRDTYPE_R6300
		beq	v0,sp,1f
		.set noreorder
		mtc0	zero,C0_INX
		mtc0	zero,C0_TLBHI
		.set reorder
1:
		move	v0,zero
		j	s7
 # Failed!
error1:
		PRINT(s4, s3, s2)		# print address, expected,
						#  actual, and Xor
		li	a0,PON_MEMTEST
		jal	FastFlash

		li	a0,PON_MEMTEST
		jal	pon_set_leds		# write to the CPU LEDS

norun:
		li	a0,PON_FAULT_MEM
		jal	SetDepend

		li	v0,BRDTYPE_R6300
		beq	v0,sp,1f
		.set noreorder
		mtc0	zero,C0_INX
		mtc0	zero,C0_TLBHI
		.set reorder
1:
		li	v0,1
		j	s7

END(Pon_Memory)
Pon_Memory_end:


LEAF(SizeMemory)

		.set noreorder
		mfc0	v0,C0_SR
		nop
		and	v0,SR_BEV		# disable everything except BEV
		mtc0	v0,C0_SR

		mtc0	zero,C0_CAUSE		# clean-up the cause register
		.set reorder
		li	v1,BRDTYPE_R2400
		beq	v1,sp,2f		# if INTREPID
		li	v1,BRDTYPE_M180
		bne	v1,sp,1f		# if not INTREPID
2:
		li	a0,FAR|K1BASE		# INTREPID
		b	$85
1:
		li	v1,BRDTYPE_R6300
		bne	v1,sp,1f		# if not R6000

		/*
		 *  R6000  --  be clever and just look at CtlSpace
		 *
		 *  We must be *very* careful about not trashing registers
		 *  used by our caller(s), who expect this to be a LEAF.
		 *  We call find_ctlspace_r6000(), and that calls
		 *  decode_brd_address(), as well as bev_general() and
		 *  __r6000_slot_unoccupied().  These routines use the t*
		 *  temporaries, a0..a2, and s2, which seems empirically
		 *  to be ok (the latter one is the most vulnerable).
		 */
		move	sp,ra			# save return address
		move	a3,zero			# cumulative size of zero
6:
		move	a0,a3			# next base address
		jal	find_ctlspace_r6000
		beq	v0,zero,7f		# nothing more there?
		add	a3,v1			# cumulative size
		li	t0,384			# 384 MBytes max
		bne	a3,t0,6b		# keep searching if <384MB
		
7:		sll	v0,a3,20		# return cum size in bytes
		move	ra,sp			# restore return address
		li	sp,BRDTYPE_R6300	#  and BrdType
		b	2f
1:
		li	a0,SBE_ADDR|K1BASE	# M-SERIES
$85:
		lw	zero,0(a0)		# clear pending write err ints
		sw	zero,0(a0)

		li	v1,BRDTYPE_R3200
                beq     v1,sp,3f

                li      v1,BRDTYPE_RB3125
		bne	v1,sp,4f

3:
		.set noreorder
		mtc0	zero,C0_INX
	 	mfc0	v0,C0_TLBHI
		nop
		.set reorder
		sll	v0,6
		b	2f
4:
		DELAY(10)
		.set noreorder
		mfc0	v1,C0_CAUSE
		nop
		.set reorder
		and	v1,CAUSE_IP8
		bne	v1,zero,$85		# more wbe's were in write buff

		li	v0,K1BASE		# starting address
$86:
		sw	zero,(v0)		# write 0
		DELAY(512)			# delay at least 512 cycles for
						#  VME bus write
		.set noreorder
		mfc0	v1,C0_CAUSE
		nop
		.set reorder
		and	v1,CAUSE_IP8
		bne	v1,zero,1f		# write bus err: NO MEMORY HERE

		addu	v0,INCR			# bump up to next chunk of mem
		b	$86
1:
		lw	zero,(a0)		# clear pending write err ints
		sw	zero,(a0)
2:
		and	v0,0x1fffffff		# remove mapping bits
		j	ra

END(SizeMemory)

		.data

begintest:
		.asciiz "Memory Test..."
hexdigit:
		.ascii  "0123456789ABCDEF"

#ifdef	DEBUG
memstart_msg:
		.asciiz "Start Address: "
memend_msg:
		.asciiz " End Address: "
#endif	DEBUG

EXPORT(paddress)
		.asciiz "ERROR\r\n  Address: "
EXPORT(pexpect)
		.asciiz "  Expected: "
EXPORT(pactual)
		.asciiz "  Actual: "
EXPORT(pxor)
		.asciiz "  Xor: "
EXPORT(nomem)
		.asciiz "NO Memory Found\r\n"
