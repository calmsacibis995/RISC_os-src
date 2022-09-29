/* network checksum functions
 *
 * $Header: cksum.s,v 1.2.3.1 89/11/28 10:15:44 wje Exp $
 */


#include "sys/asm.h"
#include "sys/reg.h"
#include "sys/regdef.h"


/*
 * in_checksum(addr, len, prevcksum)
 *
 * Calculates a 16 bit ones-complement checksum.
 * Note this routine always adds even
 * address bytes to the high order 8 bits of the 16 bit checksum and
 * odd address bytes are added to the low order 8 bits of the 16 bit checksum.
 *
 * NB: Main loop costs 4 instr's per word, 1 lw and 3 math instrs.  Unrolled
 *     lhu loop also costs 4 instr's per word, but 2 are loads.
 */
/*
    a0 = addr
    a1 = count
    a2 = sum
*/
LEAF(in_checksum)
	move	v0,a2		# copy previous checksum
	beq	a1,zero,9f	# count exhausted
	and	v1,a0,3		# already on a word boundary ?
	beq	v1,zero,4f	# yes, but don't need to wrap carries
1:	and	v1,a0,3		# already on a word boundary ?
	beq	v1,zero,3f	# yes
	and	v1,a0,1
	beq	v1,zero,2f	# already on a halfword boundry ?
	lbu	t8,0(a0)	# leading byte
	addu	a0,1		# inc addr
#ifdef MIPSEL
	sll     t8,8
#endif MIPSEL
	addu	v0,t8		# cksum
	subu	a1,1		# dec count
	b	1b

2:	blt	a1, 2, 7f
	lhu	t8,0(a0)	# load halfword
	addu	a0,2		# inc addr
	addu	v0,t8		# cksum
	subu	a1,2		# dec count

3:	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff

4:	blt	a1, 32, 7f	# enough to worry about?
5:	lw	t0, 0(a0)	# on word boundary, begin inner loop
	lw	t1, 4(a0)
	lw	t2, 8(a0)
	lw	t3, 12(a0)
	lw	t4, 16(a0)
	lw	t5, 20(a0)
	lw	t6, 24(a0)
	lw	t7, 28(a0)
	addu	v0, t0
	sltu	v1, v0, t0
	addu	v0, v1
	addu	v0, t1
	sltu	v1, v0, t1
	addu	v0, v1
	addu	v0, t2
	sltu	v1, v0, t2
	addu	v0, v1
	addu	v0, t3
	sltu	v1, v0, t3
	addu	v0, v1
	addu	v0, t4
	sltu	v1, v0, t4
	addu	v0, v1
	addu	v0, t5
	sltu	v1, v0, t5
	addu	v0, v1
	addu	v0, t6
	sltu	v1, v0, t6
	addu	v0, v1
	addu	v0, t7
	sltu	v1, v0, t7
	addu	v0, v1
	addu	a0, 32
	subu	a1, 32
	bge	a1, 32, 5b
	srl	v1, v0, 16
	and	v0, 0xffff
	addu	v0, v1
	srl	v1, v0, 16
	and	v0, 0xffff
	addu	v0, v1
	beq	a1, zero, 9f
	beq	a1, 1, 7f

6:	lhu	t8,0(a0)
	addu	a0,2
	addu	v0,t8
	subu	a1,2
7:	bge	a1,2,6b
	beq	a1,zero,8f	# no trailing byte
	lbu	t8,0(a0)
#ifdef MIPSEB
	sll	t8,8
#endif MIPSEB
	addu	v0,t8
8:	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff
9:	j	ra
	END(in_checksum)

/*
 *	The XNS checksummer does an add-and-cycle checksum.  Odd byte
 *	lengths are dealt with by postpending a garbage byte, which is
 *	carried with the packet forever, and is not assumed to be
 *	zero.  Thus, the algorithm is load a half-word, and add it to
 *	the current checksum.  Then, shift the whole mess left one bit,
 *	and iterate.  All math is ones-complement on  16 bits, so when
 *	we are done, we must  fold back all the carry bits that are in
 *	the high 16 bits of our register.  The caller is required to
 *	half-word align the packet, since we can't easily, and to
 *	postpend the garbage byte if necessary.
 */
LEAF(ns_checksum)
	move	v0,a2		# copy previous checksum
	move	t0, a1		# save count
	beq	a1,zero,3f	# count exhausted
	and	v1,a0,1
	beq	v1,zero,2f	# already on a halfword boundry
	li	v0, 0177777	# error code
	b	3f

1:	lhu	t8,0(a0)
	addu	a0,2
	addu	v0,t8
	subu	a1,2

2:	bge	a1,2,1b
	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff
	sll	t0, 1		# divide count by two it - is round
	rol	v0, t0		# now do 32 bit rotate...
	and	v1, v0, 0xffff	# and fold it back down to 16 bits
	and	v0, 0xffff0000
	or	v0, v1		# done, now have 16 bit checksum

3:	j	ra
	END(ns_checksum)
