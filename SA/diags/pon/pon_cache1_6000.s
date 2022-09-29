#ident "$Header: pon_cache1_6000.s,v 1.3.7.1 90/07/18 14:29:45 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	Performs a simple write/read verification of all s-cache locations
 *	using scache/lcache with various patterns.  Make the patterns different
 *	for each side of the s-cache.
 *
 *	Exercises the d-cache/s-cache interface by writing s-cache locations
 *	using scache to write data and ptags, then reading this data through
 *	the d-cache.  The data values are a form of address-in-address.
 *
 *	Further exercises the d-cache/s-cache interface by doing a write/read
 *	verification of 2*sizeof(d-cache) locations which hit on s-cache
 *	locations, using those previously setup ptags on both s-cache sides.
 *	Verifies that d-cache stores get to s-cache locations, and that d-cache
 *	loads can read those s-cache locations.  Various data patterns are used.
 *
 *	Further exercises the d-cache/s-cache interface by doing address-in-
 *	address write/read.  The final d-cache span of data is re-read to
 *	confirm that the data remains in the d-cache (the s-cache data is
 *	altered using scache prior to re-reading through the d-cache).  This
 *	last test is called a Retention Pass.
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
#define Dcache_Bsize		s5
#define Scache_Side_Bsize	s6		/* per side, entire s-cache */
#define Subtest_Msg		s7

#define Tmp0			v0
#define Tmp1			v1

#define	Addiu_Immediate		t2
#define Addiu_RT		t3
#define Addr_End		t4
#define DAddr			t5
#define SAddr			t6
#define Actual_2		t7
#define Expect_2		t8
#define Pattern_Number		t9


#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "pon.h"

	.globl	init_cache
	.extern	success
	.extern failure
	.extern skipped

	.extern	pexpect
	.extern	pactual
	.extern	pxor
	.extern	crlf

#define PRINT(x, y, z)	\
	blez	z,98f; \
	la	a0,paddress_scache; \
	b	99f; \
98:	la	a0,paddress_virt; \
99:	jal	pon_puts; \
	move	a0,z; \
	jal	pon_puthex; \
	la	a0,pexpect; \
	jal	pon_puts; \
	move	a0,x; \
	jal	pon_puthex; \
	la	a0,pactual; \
	jal	pon_puts; \
	move	a0,y; \
	jal	pon_puthex; \
	la	a0,pxor; \
	jal	pon_puts; \
	xor	a0,x,y; \
	jal	pon_puthex; \
	la	a0,crlf; \
	jal	pon_puts; \
	nop


#ifdef SABLE
#define PATTERN_COUNT_UPB	1
#else
#define PATTERN_COUNT_UPB	4
#endif !SABLE
#define	NBPP_SHIFT		14		/* shift bytes to pages */
#define SLINE_BSIZE		128		/* 128 bytes/line */
#define SCACHE_SIDE_DATA_BSIZE	(240*1024)	/* lower 15/16'ths of cache */
#define KILO_SHIFT		10		/* mult/div by 1024 */

	.text

LEAF(Pon_Cache1)
	move	Ra_save,ra		# save our return address

	li	a0,PON_CACHE1		# just begun
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
	jal	init_cache

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
	lb	Dcache_Bsize,ID_DCACHE_OFF(Tmp0)
	li	Tmp1,1					# (LDSLOT)
	lb	Dcache_Bsize,ID_DCACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Dcache_Bsize,4
/* XXX end hack */
	sll	Dcache_Bsize,Tmp1,Dcache_Bsize	# power of 2 shift for Kbytes
	sll	Dcache_Bsize,KILO_SHIFT		#  and for bytes

	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# power of 2 Kbytes
	li	Tmp1,1					# (LDSLOT)
	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Scache_Side_Bsize,9
/* XXX end hack */
	sll	Scache_Side_Bsize,Tmp1,Scache_Side_Bsize
	sll	Scache_Side_Bsize,KILO_SHIFT-1		# and for bytes per side
	.set	reorder


	/*
	 *  TEST ONE 	scache/lcache write-read of s-cache
	 */

  	la	a0,begintest_scache
  	jal	pon_puts

	move	Pattern_Number,zero	# init pattern number

	/*
	 *  Try a new pattern
	 */
1:	la	Tmp0,patterns		# base of patterns table
	sll	Tmp1,Pattern_Number,2	#  byte offset to pattern pair
	addi	Pattern_Number,1	# bump pattern number
	add	Tmp0,Tmp1		#   and address of the pair
	lw	Expect_1,0(Tmp0)	# next pattern to use
	lw	Expect_2,4(Tmp0)

	/*
	 *  Write the pattern to each word
	 */
	move	Addr,zero		# init starting address
	sub	Addr_End,Scache_Side_Bsize,16	#  and ending address
#ifdef SABLE
	sub	Addr_End,(192*1024)	# cut back to 1/4 size for Sable testing
#endif
	.set	noreorder
2:	scache	Expect_1,0(Addr)	# write pattern to word on each side
	scache	Expect_2,1(Addr)
	scache	Expect_1,4(Addr)	# unroll the loop a bit
	scache	Expect_2,5(Addr)
	scache	Expect_1,8(Addr)
	scache	Expect_2,9(Addr)
	scache	Expect_1,12(Addr)
	scache	Expect_2,13(Addr)
	bne	Addr,Addr_End,2b
	addi	Addr,16			# (BDSLOT)

	/*
	 *  Read each word and verify the pattern
	 */
	move	Subtest_Msg,zero	# no additional subtest err msg
	move	Addr,zero		# init starting address
	sub	Addr_End,Scache_Side_Bsize,4	#  and ending address
#ifdef SABLE
	sub	Addr_End,(192*1024)	# cut back to 1/4 size for Sable testing
#endif
	/*  first try reading with nothing in the Load Delay slot  */
3:	lcache	Actual_1,0(Addr)	# read pattern to word on each side
	nop				# (LDSLOT)
	bne	Actual_1,Expect_1,failed #  and compare to expected values
	nop				# (BDSLOT)
	lcache	Actual_2,1(Addr)
	nop				# (LDSLOT) 
	bne	Actual_2,Expect_2,failed_1
	nop				# (BDSLOT)

	/* now try another pair, with side-1 in the Load Delay slot */
	addi	Addr,4
	lcache	Actual_1,0(Addr)	# read pattern to word on each side
	lcache	Actual_2,1(Addr)
	bne	Actual_1,Expect_1,failed #  and compare to expected values
	nop				# (BDSLOT)
	bne	Actual_2,Expect_2,failed_1
	nop				# (BDSLOT)

	/* now try with side-0 in the Load Delay slot */
	addi	Addr,4
	lcache	Actual_2,1(Addr)	# read pair
	lcache	Actual_1,0(Addr)
	bne	Actual_2,Expect_2,failed_1 #  and compare to expected values
	nop				# (BDSLOT)
	bne	Actual_1,Expect_1,failed
	nop				# (BDSLOT)

	/* and a final pair, with side-1 in the Load Delay slot again */
	addi	Addr,4
	lcache	Actual_1,0(Addr)	# read pattern to word on each side
	lcache	Actual_2,1(Addr)
	bne	Actual_1,Expect_1,failed #  and compare to expected values
	nop				# (BDSLOT)
	bne	Actual_2,Expect_2,failed_1
	nop				# (BDSLOT)

	bne	Addr,Addr_End,3b	# all done with s-cache?
	addi	Addr,4			# (BDSLOT)
	.set	reorder

	/*
	 *  Do another pattern?
	 */
	bne	Pattern_Number,PATTERN_COUNT_UPB,1b

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
	 *  TEST TWO	address-at-address read test of d-cache
	 */

  	la	a0,begintest_dcache
  	jal	pon_puts

	/*
	 *  Write its address in each of the "data" words of the s-cache
	 */
	move	Addr,zero		# init starting address
	li	Addr_End,SCACHE_SIDE_DATA_BSIZE	#  and ending address
	add	Expect_2,Addr,Scache_Side_Bsize	# and init 2nd address
						#  (avoid 0xF-maps-to-0x7)
	.set	noreorder
1:	scache	Addr,0(Addr)		# write 1st addr to side-0
	scache	Expect_2,1(Addr)	#   and 2nd addr to side-1
	addi	Addr,4
	addi	Expect_2,4
	scache	Addr,0(Addr)		# unroll the loop a bit
	scache	Expect_2,1(Addr)
	addi	Addr,4
	addi	Expect_2,4
	scache	Addr,0(Addr)
	scache	Expect_2,1(Addr)
	addi	Addr,4
	addi	Expect_2,4
	scache	Addr,0(Addr)
	scache	Expect_2,1(Addr)
	addi	Addr,4			# increment 1st addr
	bne	Addr,Addr_End,1b
	addi	Expect_2,4		# (BDSLOT) and increment 2nd addr

	/*
	 *  Now access this s-cache data through the d-cache, verifying
	 *  the address-in-address for each word.
	 */
	la	Subtest_Msg,ptest_read	# for subtest error msg
	li	Addr,K0BASE		# init starting address
	move	Expect_1,zero		# init expected value
	li	Expect_2,4		#  for a pair of words
	add	Addr_End,Addr,SCACHE_SIDE_DATA_BSIZE # and ending address
#ifdef SABLE
	sub	Addr_End,(192*1024)	# speed up for Sable
#endif
2:	lw	Actual_1,(Addr)		# load 1st word
	lw	Actual_2,4(Addr)	# (LDSLOT) load 2nd word
	bne	Expect_1,Actual_1,failed # 1st word ok?
	nop				# (BDSLOT)
	bne	Expect_2,Actual_2,failed_4	# 2nd word ok?
	addi	Expect_1,8		# (BDSLOT) incr 1st word expected val
	addi	Addr,8			# incr address
	bne	Addr,Addr_End,2b	# done?
	addi	Expect_2,8		# (BDSLOT) incr 2nd word expected val
	
	/*  and try side-1  */
	move	Expect_1,Scache_Side_Bsize	# init expected value
	add	Addr,Expect_1,K0BASE	# init starting address
	add	Expect_2,Expect_1,4	# for a pair of words
	add	Addr_End,Addr,SCACHE_SIDE_DATA_BSIZE	# and ending address
#ifdef SABLE
	sub	Addr_End,(192*1024)	# speed up for Sable
#endif
3:	lw	Actual_1,(Addr)		# load 1st word
	lw	Actual_2,4(Addr)	# (LDSLOT) load 2nd word
	bne	Expect_1,Actual_1,failed # 1st word ok?
	nop				# (BDSLOT)
	bne	Expect_2,Actual_2,failed_4	# 2nd word ok?
	addi	Expect_1,8		# (BDSLOT) incr 1st word expected val
	addi	Addr,8			# incr address
	bne	Addr,Addr_End,3b	# done?
	addi	Expect_2,8		# (BDSLOT) incr 2nd word expected val
	.set	reorder


	/*
	 *  TEST THREE	write/read test of d-cache using various patterns
	 */

	move	Pattern_Number,zero	# init pattern number

	/*
	 *  Try a new pattern
	 */
1:	la	Tmp0,patterns		# base of patterns table
	sll	Tmp1,Pattern_Number,2	#  byte offset to pattern pair
	addi	Pattern_Number,1	# bump pattern number
	add	Tmp0,Tmp1		#   and address of the pair
	lw	Expect_1,0(Tmp0)	# next pattern to use
	lw	Expect_2,4(Tmp0)

	/*
	 *  Write pair of data values for 2*sizeof(dcache) bytes to both
	 *  portions of K0 address space that the ptags are set up for.
	 *  Use Lcache instructions to verify that the written data actually
	 *  gets out to the s-cache, and use LoadWord instructions to read
	 *  data through the d-cache.
	 */
	li	Addr,K0BASE		# init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,16		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
2:	sw	Expect_1,(Addr)	# store through d-cache
	sw	Expect_2,4(Addr)	# unroll the loop a bit
	sw	Expect_1,8(Addr)
	sw	Expect_2,12(Addr)
	bne	Addr,Addr_End,2b	# done?
	addi	Addr,16			# (BDSLOT) incr target addr

	/*  now try the side-1 address, and swap the data words  */
	add	Addr,Scache_Side_Bsize,K0BASE # init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,16		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
3:	sw	Expect_2,(Addr)		# store through d-cache
	sw	Expect_1,4(Addr)	# unroll the loop a bit
	sw	Expect_2,8(Addr)
	sw	Expect_1,12(Addr)
	bne	Addr,Addr_End,3b	# done?
	addi	Addr,16			# (BDSLOT) incr target addr

	/*
	 *  Now access this s-cache data directly using Lcache, and through
	 *  the d-cache using LoadWord, and verify the values in each word.
	 */
	la	Subtest_Msg,ptest_write_read	# for subtest error msg
	li	DAddr,K0BASE		# init starting d-cache address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	add	Addr_End,DAddr		#   and form ending address
#ifdef SABLE
	sub	Addr_End,Dcache_Bsize	# save Sable time
#endif
	move	SAddr,zero		# init starting s-cache address
4:	lcache	Actual_1,0(SAddr)	# read s-cache directly
	move	Addr,SAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_1,Expect_1,failed	# data ok?
	nop				# (BDSLOT)
	lw	Actual_1,(DAddr)	# side-0 data through d-cache
	move	Addr,DAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_1,Expect_1,failed	# data ok?
	addi	DAddr,4			# (BDSLOT) incr d-cache address
	addi	SAddr,4			# incr s-cache address
	/*  now read the 2nd word of the pair  */
	move	Tmp0,Expect_1		# save the word #1 pattern
	lcache	Actual_1,0(SAddr)	# read s-cache directly
	move	Addr,SAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_1,Expect_2,failed	# data ok?
	move	Expect_1,Expect_2	# (BDSLOT) expected val for err msg
	lw	Actual_1,(DAddr)	# side-0 data through d-cache
	move	Addr,DAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_1,Expect_1,failed	# data ok?
	addi	DAddr,4			# (BDSLOT) incr d-cache address
	move	Expect_1,Tmp0		# restore Expect_1
	bne	DAddr,Addr_End,4b	# done?
	addi	SAddr,4			# (BDSLOT) incr s-cache address

	/*  now do the side-1 addresses  */
	add	DAddr,Scache_Side_Bsize,K0BASE # init starting d-cache address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	add	Addr_End,DAddr		#   and form ending address
	move	SAddr,zero		# init starting s-cache address
5:	lcache	Actual_2,1(SAddr)	# read s-cache directly
	move	Addr,SAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_2,Expect_2,failed_1	# data ok?
	move	Tmp0,Expect_1		# (BDSLOT) save Expect_1 
	lw	Actual_1,(DAddr)	# side-0 data through d-cache
	move	Addr,DAddr		# (LDSLOT) set up addr for err msg
	move	Expect_1,Expect_2	# expected value for err msg
	bne	Actual_1,Expect_2,failed	# data ok?
	addi	DAddr,4			# (BDSLOT) incr d-cache address
	addi	SAddr,4			# incr s-cache address
	move	Expect_1,Tmp0		# restore Expect_1
	/*  now read the 2nd word of the pair  */
	lcache	Actual_2,1(SAddr)	# read s-cache directly
	move	Addr,SAddr		# (LDSLOT) set up addr for err msg
	move	Tmp0,Expect_2		# save Expect_2
	bne	Actual_2,Expect_1,failed_1	# data ok?
	move	Expect_2,Expect_1	# (BDSLOT) expected value for err msg
	lw	Actual_1,(DAddr)	# side-0 data through d-cache
	move	Addr,DAddr		# (LDSLOT) set up addr for err msg
	bne	Actual_1,Expect_1,failed	# data ok?
	addi	DAddr,4			# (BDSLOT) incr d-cache address
	move	Expect_2,Tmp0		# restore Expect_2
	bne	DAddr,Addr_End,5b	# done?
	addi	SAddr,4			# (BDSLOT) incr s-cache address
	.set	reorder

	/*
	 *  Do another pattern?
	 */
	bne	Pattern_Number,PATTERN_COUNT_UPB,1b


	/*
	 *  TEST FOUR	write/read address-in-address test of d-cache
	 */

	li	Addr,K0BASE		# init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,4		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
1:	sw	Addr,(Addr)		# write address-in-address
	addi	Addr,4
	sw	Addr,(Addr)		# unroll the loop a bit
	addi	Addr,4
	sw	Addr,(Addr)
	addi	Addr,4
	sw	Addr,(Addr)
	bne	Addr,Addr_End,1b
	addi	Addr,4			# (BDSLOT)
	.set	reorder

	/*  now fill side-1 addresses  */
	add	Addr,Scache_Side_Bsize,K0BASE	# init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,4		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
2:	sw	Addr,(Addr)		# write address-in-address
	addi	Addr,4
	sw	Addr,(Addr)		# unroll the loop a bit
	addi	Addr,4
	sw	Addr,(Addr)
	addi	Addr,4
	sw	Addr,(Addr)
	bne	Addr,Addr_End,2b
	addi	Addr,4			# (BDSLOT)

	/*
	 *  Read the d-cache locations, and verify address-in-address
	 */

	/*  start with side-1 this time  */
	add	Addr,Scache_Side_Bsize,K0BASE	# init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,4		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
3:	lw	Actual_1,(Addr)		# read address-in-address
	lw	Actual_2,4(Addr)	#  for two words at a time
	bne	Actual_1,Addr,failed
	move	Expect_1,Addr		# (BDSLOT) set up for err msg
	addi	Addr,4
	move	Expect_1,Addr		# set up for err msg
	bne	Actual_2,Addr,failed
	move	Actual_1,Actual_2	# (BDSLOT) fix up for err msg
	bne	Addr,Addr_End,3b
	addi	Addr,4			# (BDSLOT)
	.set	reorder

	/*  now read side-0 addresses  */
	li	Addr,K0BASE		# init starting address
	sll	Addr_End,Dcache_Bsize,1	# 2*sizeof(dcache)
	sub	Addr_End,4		#  back up for loop-ending offset
	add	Addr_End,Addr		#   and form ending address
	.set	noreorder
4:	lw	Actual_1,(Addr)		# read address-in-address
	lw	Actual_2,4(Addr)	#  for two words at a time
	bne	Actual_1,Addr,failed
	move	Expect_1,Addr		# (BDSLOT) set up for err msg
	addi	Addr,4
	move	Expect_1,Addr		# set up for err msg
	bne	Actual_2,Addr,failed
	move	Actual_1,Actual_2	# (BDSLOT) fix up for err msg
	bne	Addr,Addr_End,4b
	addi	Addr,4			# (BDSLOT)
	.set	reorder


	/*
	 *  TEST FIVE	d-cache retention
	 *
	 *  Clobber what's behind the d-cache in the s-cache, and re-read.
	 */
	move	Addr,Dcache_Bsize	# init starting address
	add	Addr_End,Addr,Dcache_Bsize
	sub	Addr_End,16		#  and back off for loop-ending addr
	.set	noreorder
1:	scache	zero,0(Addr)		# write 1st addr to side-0
	scache	zero,4(Addr)		# unroll the loop a bit
	scache	zero,8(Addr)
	scache	zero,12(Addr)
	bne	Addr,Addr_End,1b
	addi	Addr,16			# (BDSLOT) and increment addr

	/*  now read side-0 addresses  */
	la	Subtest_Msg,ptest_retention
	add	Addr,Dcache_Bsize,K0BASE # init starting address
	add	Addr_End,Addr,Dcache_Bsize # sizeof(dcache)
	sub	Addr_End,4		#  back up for loop-ending offset
	.set	noreorder
2:	lw	Actual_1,(Addr)		# read address-in-address
	lw	Actual_2,4(Addr)	#  for two words at a time
	bne	Actual_1,Addr,failed
	move	Expect_1,Addr		# (BDSLOT) set up for err msg
	addi	Addr,4
	move	Expect_1,Addr		# set up for err msg
	bne	Actual_2,Addr,failed
	move	Actual_1,Actual_2	# (BDSLOT) fix up for err msg
	bne	Addr,Addr_End,2b
	addi	Addr,4			# (BDSLOT)
	.set	reorder


	/*
	 *  TEST SIX	avoidance of upper 1/16'th of S-cache
	 *
	 *  Confirms that accesses to what would otherwise index into
 	 *  the upper 1/16'th of the S-cache will instead index into
	 *  the 7/16'th of the S-cache.
	 */
	li	a0,0xF			# base pfn
	li	a1,0			# side-0
	jal	_Pon_Init_Ptags		# init ptags
	li	a0,0x1F			# base pfn
	li	a1,1			# side-0
	jal	_Pon_Init_Ptags		# init ptags

	la	Subtest_Msg,ptest_remap_scache

	li	Addr,K0BASE+(0xF << NBPP_SHIFT)
	sw	Addr,(Addr)		# store through D-cache
	move	Expect_1,Addr		# set up for error message
	li	Addr,(0x7 << NBPP_SHIFT)
	lcache	Actual_1,0(Addr)	# read directly from S-cache
	bne	Actual_1,Expect_1,failed

	li	Addr,K0BASE+(0x1F << NBPP_SHIFT)
	sw	Addr,(Addr)		# store through D-cache
	move	Expect_1,Addr		# set up for error message
	li	Addr,(0x7 << NBPP_SHIFT)
	lcache	Actual_1,1(Addr)	# read directly from S-cache
	addi	Addr,1			# set up for error message
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


failed_4:
	/*  failed on 2nd word, so make the address/data good for err msg  */
	move	Expect_1,Expect_2
	move	Actual_1,Actual_2
	addi	Addr,4
	b	failed

failed_1:
	/*  failed on side-1, so make the address/data good for err msg  */
	move	Expect_1,Expect_2
	move	Actual_1,Actual_2
	addi	Addr,1
	b	failed

failed:
	.set	noreorder
	mfc0	Tmp0,C0_SR
	li	Tmp1,~SR_MM_MODE
	and	Tmp0,Tmp1		# remove MM_MODE
	mtc0	Tmp0,C0_SR		#  and restore SR
	.set	reorder

	beq	Subtest_Msg,zero,1f	# print any subtest msg?
	move	a0,Subtest_Msg
	jal	pon_puts
1:

	la	a0,failure
   	jal	pon_puts

	PRINT(Expect_1,Actual_1,Addr)

	li	a0,PON_CACHE1
	jal	FastFlash

	li	a0,PON_CACHE1
	jal	pon_set_leds		# write to the CPU LEDS
norun:
	li	a0,PON_FAULT_CACHE
	jal	SetDepend

	li	v0,1
	j	Ra_save	

	END(Pon_Cache1)


#define PTAGS_BASE		(248*1024)
#define PTAGS_PER_PAGE		128		/* 16K / 128 bytes */
#define	PTAGS_PER_PAGE_SHIFT	7
#define	PTAG_PFN_SHIFT		10		/* form ptag out of pfn */

LEAF(_Pon_Init_Ptags)
/*
 *  _Pon_Init_Ptags (pfn, side)
 *
 *  Initialize the ptags for the specified pfn on the specified s-cache side
 *  for an entire page.
 *
 *  Uses v0, v1, t0.  Does not modify a0..a3.
 */
	and	v0,a0,0xF		# isolate which section of s-cache
	bne	v0,0xF,1f		# if F, then remap to 7
	li	v0,0x7
1:
	sll	v0,PTAGS_PER_PAGE_SHIFT+2 # byte offset of 1st ptag
	add	v0,PTAGS_BASE			# init beginning
	add	v1,v0,(PTAGS_PER_PAGE <<2)-16	# and ending ptags address
	sll	t0,a0,PTAG_PFN_SHIFT	# ptag value
	bne	a1,zero,3f		# side-1?
	.set	noreorder
2:	scache	t0,0(v0)		# write side-0
	scache	t0,4(v0)		# unroll the loop a bit
	scache	t0,8(v0)
	scache	t0,12(v0)
	bne	v0,v1,2b		# done with ptags for one page?
	addi	v0,16			# (BDSLOT) incr addr of next ptags
	j	ra			# exit
	nop				# (BDSLOT)
3:	scache	t0,1(v0)		# write side-0
	scache	t0,5(v0)		# unroll the loop a bit
	scache	t0,9(v0)
	scache	t0,13(v0)
	bne	v0,v1,3b		# done with ptags for one page?
	addi	v0,16			# (BDSLOT) incr addr of next ptags
	j	ra			# exit
	nop				# (BDSLOT)
	.set	reorder
	END(_Pon_Init_Ptags)

	.data

begintest:		.asciiz "Cache Array Tests:  "
begintest_scache:	.asciiz "Scache..."
begintest_dcache:	.asciiz "Dcache..."
ptest_read:		.asciiz	"Read Pass "
ptest_write_read:	.asciiz	"Write/Read Pass "
ptest_retention:	.asciiz	"Retention Pass "
ptest_remap_scache:	.asciiz	"Remap Scache Pass "
paddress_scache:	.asciiz	"  Scache addr: "
paddress_virt:		.asciiz	"  Virtual addr: "

patterns:
	.word	0x00000000, 0xffffffff
	.word	0xffffffff, 0x00000000
	.word	0xaaaaaaaa, 0x55555555
	.word	0x55555555, 0xaaaaaaaa
