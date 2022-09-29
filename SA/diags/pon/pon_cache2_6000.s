#ident "$Header: pon_cache2_6000.s,v 1.3.7.1 90/07/18 14:29:53 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	Exercises the i-cache/s-cache interface by writing a 2*sizeof(i-cache)
 *	series of  addi  instructions, terminated by  jr, through the d-cache,
 *	then executing these instructions through the i-cache.  This is done
 *	for physical pages on both sides of the s-cache.
 *
 *	Next, an attempt is made to exercise most of the instruction bits
 *	in each cache word using a series of sizeof(i-cache)  addiu
 *	instructions.  All sixteen bits of the Immediate field, all five bits
 *	of the RS field, and all five bits of the RT field are used.
 *
 *	Finally, a new sequence of instructions is stuffed into the s-cache
 *	which, when executed, both reset the s-cache words behind the i-cache
 *	line being executed and bump a counter.  This sequence loops several
 *	times with the presumption that the original instructions are retained
 *	in the i-cache after the first primary cache miss.  The final count is
 *	checked.  Note:  we can't drop back to uncached i-fetches, as this
 *	causes line zero of the i-cache to be invalidated (because that's where
 *	the CPU puts each uncached instruction) and causes various other virtual
 *	i-cache tags to be rewritten (because the CPU rewrites the vtag first,
 *	before it notices that the i-fetch is really uncached).
 *
 *	NOTE: This routine expects to be executed before a stack is
 *	      established.  Therefore, it uses the S registers freely.
 *
 *	RETURNS: A 0 if the test is successful, a 1 is unsuccessful.
 */

#define	Ra_save			s0
#define Addr			s1
#define Actual_1		s2
#define Expect_1		s3
#define Icache_Bsize		s4
#define Scache_Side_Bsize	s6		/* per side, entire s-cache */
#define Subtest_Msg		s7

#define Tmp0			v0
#define Tmp1			v1

#define	Addiu_Immediate		t2
#define Addiu_RT		t3
#define Addr_End		t4


#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "pon.h"

	.globl	reset_cache
	.globl	_Pon_Init_Ptags
	.extern	success
	.extern failure
	.extern skipped

	.extern	pexpect
	.extern	pactual
	.extern	crlf

#define	NBPP_SHIFT		14		/* shift bytes to pages */
#define KILO_SHIFT		10		/* mult/div by 1024 */

#define RS_SHIFT		21
#define RT_SHIFT		16

#define ICACHE_LINE_WSIZE	 8		/* words per line */
#define ICACHE_LINE_BSIZE	32		/* bytes per line */

	.text

LEAF(Pon_Cache2)
	move	Ra_save,ra		# save our return address

	li	a0,PON_CACHE2		# just begun
	jal	pon_set_leds		# write to the CPU LEDS

  	la	a0,begintest
  	jal	pon_puts

	jal	GetDepend

	and	v0,PON_FAULT_CACHE
	beq	v0,zero,run

  	la	a0,skipped
  	jal	pon_puts

	b	norun
run:
	jal	reset_cache

	.set	noreorder
	mfc0	Tmp0,C0_SR		# set MM_MODE bit in status register
	li	Tmp1,SR_MM_MODE
	or	Tmp0,Tmp1
	mtc0	Tmp0,C0_SR
	.set	reorder

	/*
	 *  Determine the cache sizes from the IdProm
	 */

	.set	noreorder
	li	Tmp0,CSR_IDPROM+4
	lb	Icache_Bsize,ID_ICACHE_OFF(Tmp0)
	li	Tmp1,1					# (LDSLOT)
	lb	Icache_Bsize,ID_ICACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Icache_Bsize,6				# 64KB
/* XXX end hack */
	sll	Icache_Bsize,Tmp1,Icache_Bsize	# power of 2 shift for Kbytes
	sll	Icache_Bsize,KILO_SHIFT		#  and finally for bytes

	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# power of 2 Kbytes
	li	Tmp1,1					# (LDSLOT)
	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Scache_Side_Bsize,9
/* XXX end hack */
	sll	Scache_Side_Bsize,Tmp1,Scache_Side_Bsize
	sll	Scache_Side_Bsize,KILO_SHIFT-1		# and for bytes on side
	.set	reorder

	/*
	 *  Initialize the ptags for the next series of tests:
	 *	side-0 for physical 0..SIDE_DATA_SIZE,
	 *	side-1 for physical SIDE_SIZE+1..SIDE_SIZE+SIDE_DATA_SIZE
	 */
	li	a0,0			# base pfn
	li	a1,0			# side-0
	li	a3,0xF			# init this many pages
1:	jal	_Pon_Init_Ptags		# init ptags for this pfn
	addi	a0,1			# increment pfn
	sub	a3,1			# decr page count
	bne	a3,zero,1b		# done with all pages on side-0?

	srl	a0,Scache_Side_Bsize,NBPP_SHIFT	# base pfn
	li	a1,1			# side-1
	li	a3,0xF			# init this many pages
2:	jal	_Pon_Init_Ptags		# init ptags for this pfn
	addi	a0,1			# increment pfn
	sub	a3,1			# decr page count
	bne	a3,zero,2b		# done with all 15 pages on side-1?
	
	/*
	 *  TEST ONE	read test of i-cache
	 */

	/*
	 *  Write a series of  addi  instructions for 2*sizeof(icache)-8 bytes
	 *  to both portions of K0 address space that the ptags are set up for,
	 *  terminated by a  jr  and another  addi  in the delay slot.
	 *  Branch to the beginning of the  addi  instructions, and verify the
	 *  terminal count.
	 */
	la	Tmp0,addi_instruction	
	lw	Expect_1,(Tmp0)		# the 'addi' instruction
	li	Addr,K0BASE		# init starting address
	sll	Addr_End,Icache_Bsize,1	# 2*sizeof(icache)
	sub	Addr_End,16		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
1:	sw	Expect_1,(Addr)		# store through d-cache
	sw	Expect_1,4(Addr)	# unroll the loop a bit
	sw	Expect_1,8(Addr)
	sw	Expect_1,12(Addr)
	bne	Addr,Addr_End,1b	# done?
	addi	Addr,16			# (BDSLOT) incr target addr
	.set	reorder

	la	Tmp0,jr_instruction
	lw	Expect_1,(Tmp0)		# the 'jr' instruction
	sw	Expect_1,-8(Addr)	#  as the terminator

	/*  now try the side-1 address  */
	la	Tmp0,addi_instruction	
	lw	Expect_1,(Tmp0)		# the 'addi' instruction
	add	Addr,Scache_Side_Bsize,K0BASE # init starting address
	sll	Addr_End,Icache_Bsize,1	# 2*sizeof(icache)
	sub	Addr_End,16		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
2:	sw	Expect_1,(Addr)		# store through d-cache
	sw	Expect_1,4(Addr)	# unroll the loop a bit
	sw	Expect_1,8(Addr)
	sw	Expect_1,12(Addr)
	bne	Addr,Addr_End,2b	# done?
	addi	Addr,16			# (BDSLOT) incr target addr
	.set	reorder

	la	Tmp0,jr_instruction
	lw	Expect_1,(Tmp0)		# the 'jr' instruction
	sw	Expect_1,-8(Addr)	#  as the terminator

	/*
	 *  Now execute the side-0 instructions, and verify the 'addi' count.
	 */
	move	Actual_1,zero		# initialize count
	srl	Expect_1,Icache_Bsize,1 # num instructions in 2*(i-cache)
	sub	Expect_1,1		# num 'addi':  expected final count
	la	ra,3f			# return address
	li	Addr,K0BASE		# starting address
	j	Addr
3:
	la	Subtest_Msg,ptest_execute0	# subtest msg
	bne	Actual_1,Expect_1,failed

	/*
	 *  And execute the side-1 instructions, and verify the 'addi' count.
	 */
	move	Actual_1,zero		# initialize count
	srl	Expect_1,Icache_Bsize,1 # num instructions in 2*(i-cache)
	sub	Expect_1,1		# num 'addi':  expected final count
	la	ra,4f			# return address
	add	Addr,Scache_Side_Bsize,K0BASE # starting address
	j	Addr
4:
	la	Subtest_Msg,ptest_execute1	# subtest msg
	bne	Actual_1,Expect_1,failed


	/*
	 *  TEST TWO	icache pattern test
	 */

	/*
	 *  Write a series of  addiu  instructions for sizeof(icache) bytes
	 *  to the base K0 address space that the ptags are set up for,
	 *  preceeded by a  li RT,0  to initialize the count, and terminated by
	 *  a  jr  and a  move v0,RT  in the delay slot to return the sum.
	 *  Branch to the first instruction, and verify the terminal count.
	 */
	la	Subtest_Msg,ptest_pattern
	li	Addiu_Immediate,1	# initialize
	li	Addiu_RT,1		# initialize
1:
	li	Addr,K0BASE		# init starting address
	la	Tmp0,li_instruction	
	lw	Tmp0,(Tmp0)		# the prototype 'li' instruction
	sll	Tmp1,Addiu_RT,RT_SHIFT
	or	Tmp0,Tmp1
	sw	Tmp0,(Addr)		# stuff the 'li' into the first word
	addi	Addr,4			#  and bump the target addr
	la	Tmp0,addiu_instruction	
	lw	Tmp0,(Tmp0)		# the prototype 'addiu' instruction
	and	Tmp1,Addiu_Immediate,0xffff	# in case negative Immediate
	or	Tmp0,Tmp1
	sll	Tmp1,Addiu_RT,RS_SHIFT
	or	Tmp0,Tmp1
	sll	Tmp1,Addiu_RT,RT_SHIFT
	or	Tmp0,Tmp1
	add	Addr_End,Addr,Icache_Bsize # sizeof(icache)
	sub	Addr_End,16		#  back up for loop-ending offset
	.set	noreorder
2:	sw	Tmp0,(Addr)		# store through d-cache
	sw	Tmp0,4(Addr)		# unroll the loop a bit
	sw	Tmp0,8(Addr)
	sw	Tmp0,12(Addr)
	inval	(Addr)			# Invalidate i-cache line
	bne	Addr,Addr_End,2b	# done?
	addi	Addr,16			# (BDSLOT) incr target addr
	.set	reorder

	inval	(Addr)			# Invalidate i-cache line
	la	Tmp0,jr_instruction
	lw	Tmp0,(Tmp0)		# the 'jr' instruction
	sw	Tmp0,(Addr)		#  as the terminator

	la	Tmp1,move_instruction
	lw	Tmp0,(Tmp1)		# the 'move' instruction
	sll	Tmp1,Addiu_RT,RS_SHIFT
	or	Tmp0,Tmp1
	sw	Tmp0,4(Addr)		#  in the bdslot, to return the result

	/*
	 *  Execute the instructions, and verify the 'addiu' count.
	 */
	
	la	ra,3f			# return address
	li	Addr,K0BASE		# starting address
	srl	v1,Icache_Bsize,2	# num addiu instructions
	mult	v1,Addiu_Immediate
	j	Addr
3:
	move	Actual_1,v0		# for error msg
	mflo	Expect_1
	bne	v0,Expect_1,failed

	and	Tmp0,Addiu_Immediate,0x8000
	bne	Tmp0,zero,4f		# all done with Immediate values?
	sll	Addiu_Immediate,1
#ifdef SABLE
	sll	Addiu_Immediate,4	# save Sable time
#endif
	and	Tmp0,Addiu_Immediate,0x8000
	beq	Tmp0,zero,1b		# not exhausted Immediate values >0
	li	Addiu_Immediate,-1	# use -1 instead of 0xffff8000
	b	1b

4:	bne	Addiu_RT,1,5f
	li	Addiu_RT,3		# use v1 instead of v0
	b	1b
5:	bne	Addiu_RT,3,6f
	li	Addiu_RT,4		# after v1, use a0 (==$4)
	b	1b
6:	bne	Addiu_RT,8,7f
	li	Addiu_RT,26		# use k0 instead of s0
	b	1b
7:	beq	Addiu_RT,26,8f		# if k0, then all done
	sll	Addiu_RT,1		# otherwise, bump RS and keep going
	b	1b
8:

	
	/*
	 *  TEST THREE	icache retention test
	 *
	 *  Stuff a sequence of instructions in the s-cache which will execute
	 *  cached and will clobber themselves in the s-cache and will bump a
	 *  count.
	 *
	 *  The test loops several times, and expects to suffer i-cache misses
	 *  only on the first pass.  On subsequent passes the instructions
	 *  should remain in the i-cache.  If an i-cache miss does occur, the
	 *  new instructions in the s-cache will cause an immediate termination
	 *  of the loop and the incrementing count will be incorrect.
	 */

	li	Addr,K0BASE		# start executing from K0 base
	add	Addr_End,Addr,Icache_Bsize
/* XXX start hack:  some I-caches are only 16KB */
	li	Icache_Bsize,16384
	add	Addr_End,Addr,Icache_Bsize
/* XXX end hack */
	sub	Addr_End,32		# and ending address
1:	/*
	 *  Through the d-cache into the s-cache, fill i-cache line images
	 *  with the typical pattern.  Do this for all but the last line.
	 *
	 *  The typical i-cache line looks like:
	 *	sw	Tmp0,0(Addr)	# where Tmp0 is   jr ra
	 *	sw	Tmp0,8(Addr)
	 *	sw	Tmp0,16(Addr)
	 *	sw	Tmp0,24(Addr)
	 *	addi	Actual_1,1
	 *	addi	Addr,32
	 *	nop
	 *	nop
	 */
	inval	0(Addr)			# invalidate i-cache line
	la	Tmp0,sw_instruction	
	lw	Tmp0,(Tmp0)		# the 'SW' instruction
	sw	Tmp0,0(Addr)		# store through d-cache
	addi	Tmp0,8			# bump the offset within the SW
	sw	Tmp0,4(Addr)		#  and stuff 2nd SW
	addi	Tmp0,8			# bump the offset within the SW
	sw	Tmp0,8(Addr)		#  and stuff 3rd SW
	addi	Tmp0,8			# bump the offset within the SW
	sw	Tmp0,12(Addr)		#  and stuff 4th SW

	la	Tmp0,addi_instruction
	lw	Tmp0,(Tmp0)		# the 'ADDI' instruction for the
	sw	Tmp0,16(Addr)		#  validity check

	la	Tmp0,addi_Addr_instruction
	lw	Tmp0,(Tmp0)		# the 'ADDI' instruction for the
	sw	Tmp0,20(Addr)		#  Address increment

	sw	zero,24(Addr)		# and the NOPs
	sw	zero,28,(Addr)

	addi	Addr,32
	bne	Addr,Addr_End,1b	# done?

	/*
	 *  The last i-cache line looks like:
	 *	sw	Tmp0,0(Addr)	# where Tmp0 is   jr ra
	 *	sub	Tmp1,1		# where Tmp1 is the outer loop counter
	 *	beq	Tmp1,zero,.+4	
	 *	addi	Actual_1,1
	 *	jr	Expect_1	# branch back to K0BASE
	 *	move	Addr,Expect_1	# where Expect_1 is K0BASE
	 *	jr	ra		# exit loop
	 *	nop
	 */
	inval	0(Addr)			# invalidate i-cache line
	la	Tmp1,last_line_instructions
	lw	Tmp0,0(Tmp1)
	sw	Tmp0,0(Addr)		# store through d-cache
	lw	Tmp0,4(Tmp1)
	sw	Tmp0,4(Addr)
	lw	Tmp0,8(Tmp1)
	sw	Tmp0,8(Addr)
	lw	Tmp0,12(Tmp1)
	sw	Tmp0,12(Addr)
	lw	Tmp0,16(Tmp1)
	sw	Tmp0,16(Addr)
	lw	Tmp0,20(Tmp1)
	sw	Tmp0,20(Addr)
	lw	Tmp0,24(Tmp1)
	sw	Tmp0,24(Addr)
	lw	Tmp0,28(Tmp1)
	sw	Tmp0,28(Addr)

	/*
	 *  Now prepare to start the loop:
	 *	Addr		K0BASE
	 *	Expect_1	K0BASE
	 *	Actual_1	zero
	 *	Tmp0		JR RA  instruction
	 *	Tmp1		four (outer loop counter)
	 *	ra		return address
	 */
	li	Addr,K0BASE
	move	Expect_1,Addr
	li	Actual_1,0
	la	Tmp0,jr_instruction
	lw	Tmp0,(Tmp0)
	li	Tmp1,4

	la	ra,2f
	j	Expect_1		# start the loop!

2:	/*
	 *  We have exited the loop.  Did we go through the
	 *  expected number of times?
	 */
	srl	Expect_1,Icache_Bsize,5	# number of lines in i-cache
	sll	Expect_1,2		# and four times through the loop
	la	Subtest_Msg,ptest_retention
	bne	Actual_1,Expect_1,failed


 # Successful!
	.set	noreorder
	mfc0	Tmp0,C0_SR
	li	Tmp1,~SR_MM_MODE
	and	Tmp0,Tmp1		# remove MM_MODE
	mtc0	Tmp0,C0_SR		#  and restore SR
	.set	reorder
  	la	a0,success
  	jal	pon_puts

	move	v0,zero
	j	Ra_save

failed:
	.set	noreorder
	mfc0	Tmp0,C0_SR
	li	Tmp1,~SR_MM_MODE
	and	Tmp0,Tmp1		# remove MM_MODE
	mtc0	Tmp0,C0_SR		#  and restore SR
	.set	reorder

	move	a0,Subtest_Msg	# subtest msg
	jal	pon_puts

	la	a0,failure
   	jal	pon_puts

	la	a0,ptest_instr
	jal	pon_puts
	li	Tmp0,K0BASE
	lw	a0,8(Tmp0)		# 'addiu' instruction
	jal	pon_puthex
	la	a0,pexpect
	jal	pon_puts
	move	a0,Expect_1
	jal	pon_puthex
	la	a0,pactual
	jal	pon_puts
	move	a0,Actual_1
	jal	pon_puthex
	la	a0,crlf
	jal	pon_puts

	li	a0,PON_CACHE2
	jal	FastFlash

	li	a0,PON_CACHE2
	jal	pon_set_leds		# write to the CPU LEDS
norun:
	li	a0,PON_FAULT_CACHE
	jal	SetDepend

	li	v0,1
	j	Ra_save	


addi_instruction:
	addi	Actual_1,1
addi_Addr_instruction:
	addi	Addr,32
addiu_instruction:
	addiu	$0,$0,0			# fill in all fields at runtime
li_instruction:
	li	$0,0			# fill in target register at runtime
sw_instruction:
	sw	Tmp0,(Addr)		# fill in offset at runtime
jr_instruction:
	j	ra
move_instruction:
	move	v0,$0			# fill in source register at runtime

last_line_instructions:
	.set	noreorder
	.set	noat
	sw	Tmp0,0(Addr)	# where Tmp0 is   jr ra
	sub	Tmp1,1		# where Tmp1 is the outer loop counter
	beq	Tmp1,zero,1f
	addi	Actual_1,1
	j	Expect_1	# branch back to K0BASE
	move	Addr,Expect_1	# where Expect_1 is K0BASE
1:	j	ra		# exit loop
	nop
	.set	reorder
	.set	at

	END(Pon_Cache2)

	.data

begintest:		.asciiz "Icache Array Test..."
ptest_retention:	.asciiz	"Retention Pass "
ptest_execute0:		.asciiz	"Execute Side-0 Pass "
ptest_execute1:		.asciiz	"Execute Side-1 Pass "
ptest_pattern:		.asciiz	"Pattern Pass "
ptest_instr:		.asciiz	"  Instruction: "
