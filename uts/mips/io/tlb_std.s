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
/* $Header: tlb_std.s,v 1.4.1.2 90/05/10 05:35:24 wje Exp $ */

#include "../ml/assym.s"

/*
 * NOTE: These routines are coded conservatively in regards to nop's
 * and m[tf]c0 operations.  Some of the nop's may be able to be removed
 * after consulting with the chip group.  Also note: the routines are
 * coded noreorder to avoid the reorganizer moving C0 instructions around
 * that it doesn't realize are order dependent.
 */

/* NOTE:
 *
 *  The follwoing variables are placed right before the entry point for
 *  the exception vector.  Hopefully, the utlbmiss handler is not too
 *  big to overlap these variables.
 *
 *	utlbmisscnt at	E_VEC-8
 *	kstackflag  at  E_VEC-4
 */
	ABS(utlbmisscnt, E_VEC-8)

/*
 * Deal with tlbmisses in KUSEG (original which works with 3.0 chip bug)
 */
EXPORT(utlbmiss)
NESTED(utlb_3bug, 0, k1)		# Copied down to 0x80000000
	.set	noreorder
 	.set	noat
	mfc0	k0,C0_BADVADDR		# due to 3.0 chip bug
	mfc0	k1,C0_EPC
	bltz	k0,1f			# due to 3.0 chip bug
#ifdef CHIPBUG
	nop			# can't put mfc0 in slot due to mfc0 hack
#endif
	mfc0	k0,C0_CTXT
#ifndef CHIPBUG
	nop				# mfc0 delay slot.
#endif
	sll	k0,1			# compute address of pte
	lw	k0,0(k0)
	nop
	mtc0	k0,C0_TLBLO
	nop
	c0	C0_WRITER
1:	j	k1
	c0	C0_RFE
EXPORT(eutlb_3bug)
	nop			# extra NOPs for largetst utlbmiss handler
	nop
	nop
	nop
	nop
	nop
EXPORT(eutlbmiss)
 	.set	at
	.set	reorder
	END(utlb_3bug)


/* Profiled utlbmiss handler for 3.0 chip bug
 */
NESTED(utlb_3bugp, 0, k1)		# Copied down to 0x80000000
	.set	noreorder
 	.set	noat
	/*
	 * The following code assumes that the 16 bit offset on lw
	 * does not have sign bit set.
	 */
	lui	k0, utlbmisscnt>>16
	lw	k1, +utlbmisscnt&0xffff(k0)
	nop
	addiu	k1, k1,1
	sw	k1, +utlbmisscnt&0xffff(k0)

	mfc0	k0,C0_BADVADDR		# due to 3.0 chip bug
	mfc0	k1,C0_EPC
	bltz	k0,1f			# due to 3.0 chip bug
#ifdef CHIPBUG
	nop			# can't put mfc0 in slot due to mfc0 hack
#endif
	mfc0	k0,C0_CTXT
#ifndef CHIPBUG
	nop				# mfc0 delay slot.
#endif
	sll	k0,1			# compute address of pte
	lw	k0,0(k0)
	nop
	mtc0	k0,C0_TLBLO
	nop
	c0	C0_WRITER
1:	j	k1
	c0	C0_RFE
EXPORT(eutlb_3bugp)
 	.set	at
	.set	reorder
	END(utlbmiss_3bug)

/* Fast (optimized) utlbmiss handler which does not support 3.0 chip bug.
 */
NESTED(utlb_fast, 0, k1)		# Copied down to 0x80000000
	.set	noreorder
 	.set	noat
	mfc0	k0,C0_CTXT
	mfc0	k1,C0_EPC
	sll	k0,1			# compute address of pte
	lw	k0,0(k0)
	nop
	mtc0	k0,C0_TLBLO
	nop
	c0	C0_WRITER
	j	k1
	c0	C0_RFE
EXPORT(eutlb_fast)
 	.set	at
	.set	reorder
	END(utlb_fast)

/* Profiled utlbmiss handler which does not support 3.0 chip bug
 * and keeps a count of #calls to utlbmiss handler.  This handler
 * can be installed via a call to sysmips(UTLBMISS,..) system call.
 */
NESTED(utlb_fastp, 0, k1)		# Copied down to 0x80000000
	.set	noreorder
 	.set	noat
	/*
	 * The following code assumes that the 16 bit offset on lw
	 * does not have sign bit set.
	 */
	lui	k0, utlbmisscnt>>16
	lw	k1, +utlbmisscnt&0xffff(k0)
	nop
	addiu	k1, k1,1
	sw	k1, +utlbmisscnt&0xffff(k0)

	mfc0	k0,C0_CTXT
	mfc0	k1,C0_EPC
	sll	k0,1			# compute address of pte
	lw	k0,0(k0)
	nop
	mtc0	k0,C0_TLBLO
	nop
	c0	C0_WRITER
	j	k1
	c0	C0_RFE
				# utlbmisscnt is here!
				# kstackflag is here!
EXPORT(eutlb_fastp)
 	.set	at
	.set	reorder
	END(utlbmiss_fastp)


/*
 * unmaptlb(rpid, vpage): unmap a single tlb entry in the ITLB.
 *
 *	Probe for RPID/VPAGE in TLB.  If it doesn't exist, all done.
 *	If it does, mark the entries as invalid.
 *	Interrupts must be disabled because interrupts can generate
 *	a tlbmiss which will destroy the C0 registers.
 */
LEAF(unmaptlb)
	.set	noreorder
	mfc0	t0,C0_SR		# save SR
	mfc0	t1,C0_TLBHI		# save TLBHI (for TLBPID)
	mtc0	zero,C0_SR		# no interrupts
	sll	a1,TLBHI_VPNSHIFT	# shift vpn
	sll	a0,TLBHI_PIDSHIFT	# shift tlbpid
	or	t2,a1,a0		# tlbhi value to look for
	mtc0	t2,C0_TLBHI		# put args into tlbhi
	nop				# let tlbhi get through pipe
	c0	C0_PROBE		# probe for address
	mfc0	t3,C0_INX		# see what happened
	move	v0,zero			# LDSLOT
	bltz	t3,1f			# probe failed
	nop

#ifdef PROBE_BUG
	move	t4,t3			# start with original index "hint"
					# this should be the lower bounds.
6:
	c0	C0_READI		# read an entry.
	mfc0	t5,C0_TLBHI
	nop
	beq	t5,t2,2f		# test for pid/vpn match
	nop
	and	t5,TLBHI_VPNMASK	# isolate vpn
	bne	t5,a1,3f		# test vpn match for global pages
	nop
	mfc0	t5,C0_TLBLO		# vpn match, check global bit
	nop
	and	t5,TLBLO_G
	bne	t5,zero,2f
	nop
					# did not find it, try another slot
3:
	li	t5,TLBINX_INXMASK<<TLBINX_INXSHIFT
	beq	t4,t5,4f
	nop
	add	t4,1<<TLBINX_INXSHIFT
5:	mtc0	t4,C0_INX
	b	6b
	nop
	/*
	 * Bad news if we get to here. The entire tlb has been searched
	 * without finding the entry that match on the probe instruction.
	 * Return -1 to the caller and let him panic. To dangerous to restore
	 * the sr.
	 */
4:	li	v0,-1			# searched entire tlb, panic time.
	j	ra			# let caller panic
	nop
2:
	sne	v0,t3,t4		# found the match
#endif	PROBE_BUG

	li	t2,K0BASE&TLBHI_VPNMASK	# BDSLOT
	mtc0	t2,C0_TLBHI		# invalidate entry
	mtc0	zero,C0_TLBLO		# cosmetic
	nop
	c0	C0_WRITEI
1:	mtc0	t1,C0_TLBHI		# restore old TLBHI
	mtc0	t0,C0_SR		# restore sr and return
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(unmaptlb)

/*
 * unmodtlb(rpid, vpage): Clear the dirty/writeable bit for the ITLB.
 */
LEAF(unmodtlb)
	.set	noreorder
	mfc0	t0,C0_SR		# disable interrupts
	mfc0	t1,C0_TLBHI		# save TLBHI
	mtc0	zero,C0_SR
	sll	a1,TLBHI_VPNSHIFT	# construct new TLBHI
	sll	a0,TLBHI_PIDSHIFT	# shift tlbpid
	or	t2,a0,a1
	mtc0	t2,C0_TLBHI		# move to C0 for probe
	nop
	c0	C0_PROBE		# probe for address
	mfc0	t3,C0_INX
	move	v0,zero			# LDSLOT
	bltz	t3,1f			# probe failed
	nop				# BDSLOT

#ifdef PROBE_BUG
	move	t4,t3			# start with original index "hint"
6:
	c0	C0_READI		# read an entry.
	mfc0	t5,C0_TLBHI
	nop
	beq	t5,t2,2f		# test for pid/vpn match
	nop
	and	t5,TLBHI_VPNMASK	# isolate vpn
	bne	t5,a1,3f		# test vpn match for global pages
	nop
	mfc0	t5,C0_TLBLO		# vpn match, check global bit
	nop
	and	t5,TLBLO_G
	bne	t5,zero,2f
	nop
					# did not find it, try another slot
3:
	li	t5,TLBINX_INXMASK<<TLBINX_INXSHIFT
	beq	t4,t5,4f
	nop
	add	t4,1<<TLBINX_INXSHIFT
5:	mtc0	t4,C0_INX
	b	6b
	nop
	/*
	 * Bad news if we get to here. The entire tlb has been searched
	 * without finding the entry that match on the probe instruction.
	 * Return -1 to the caller and let him panic. To dangerous to restore
	 * the sr.
	 */
4:	li	v0,-1			# searched entire tlb, panic time.
	j	ra			# let caller panic
	nop
2:
	sne	v0,t3,t4		# found the match
#endif	PROBE_BUG
	c0	C0_READI		# load entry in TLBLO/TLBHI
	mfc0	t2,C0_TLBLO
	nop
	and	t2,~TLBLO_D		# reset modified bit
	mtc0	t2,C0_TLBLO		# unmod entry
	nop
	c0	C0_WRITEI
1:	mtc0	t1,C0_TLBHI		# restore old TLBHI
	mtc0	t0,C0_SR		# restore sr
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(unmodtlb)

/*
 * invaltlb(i): Invalidate the ith ITLB entry.
 * called whenever a specific TLB entry needs to be invalidated.
 */
LEAF(invaltlb)
	.set	noreorder
	li	t2,K0BASE&TLBHI_VPNMASK
	mfc0	t0,C0_TLBHI		# save current TLBHI
	mfc0	v0,C0_SR		# save SR and disable interrupts
	mtc0	zero,C0_SR
	mtc0	t2,C0_TLBHI		# invalidate entry
	mtc0	zero,C0_TLBLO
	sll	a0,TLBINX_INXSHIFT
	mtc0	a0,C0_INX
	nop
	c0	C0_WRITEI
	mtc0	t0,C0_TLBHI
	mtc0	v0,C0_SR
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(invaltlb)

/*
 * tlbwired(indx, tlbpid, vaddr, pte) -- setup wired ITLB entry
 * a0 -- indx -- tlb entry index
 * a1 -- tlbpid -- context number to use (0-63)
 * a2 -- vaddr -- virtual address (could have offset bits)
 * a3 -- pte -- contents of pte
 */
LEAF(tlbwired)
	.set	noreorder
	sll	a0,TLBINX_INXSHIFT
	mfc0	t1,C0_TLBHI		# save current TLBPID
	mfc0	v0,C0_SR		# save SR and disable interrupts
	mtc0	zero,C0_SR
	sll	a1,TLBHI_PIDSHIFT	# line up pid bits
	and	a2,TLBHI_VPNMASK	# chop offset bits
	or	a1,a2			# formatted tlbhi entry
	mtc0	a1,C0_TLBHI		# set VPN and TLBPID
#if NOCACHE==1
	or	a3,PG_N
#endif
	mtc0	a3,C0_TLBLO		# set PPN and access bits
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_WRITEI		# drop it in
	mtc0	t1,C0_TLBHI		# restore TLBPID
	mtc0	v0,C0_SR		# restore SR
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(tlbwired)

/*
 * tlbdropin(tlbpid, vaddr, pte) -- random tlb drop-in
 * a0 -- tlbpid -- tlbcontext number to use (0-63)
 * a1 -- vaddr -- virtual address to map. Can contain offset bits
 * a2 -- pte -- contents of pte
 *
 * Probes first to ensure that no other tlb entry exists with this pid
 * and vpn.
 */
LEAF(tlbdropin)
	.set	noreorder
	mfc0	t0,C0_SR		# save SR and disable interrupts
	mfc0	t1,C0_TLBHI		# save current pid
	mtc0	zero,C0_SR
	sll	a0,TLBHI_PIDSHIFT	# align pid bits for entryhi
	and	a1,TLBHI_VPNMASK	# chop any offset bits from vaddr
	or	t2,a0,a1		# vpn/pid ready for entryhi
	mtc0	t2,C0_TLBHI		# vpn and pid of new entry
#if NOCACHE==1
	or	a2,PG_N
#endif
	mtc0	a2,C0_TLBLO		# pte for new entry
	nop
	c0	C0_PROBE		# probe for stale value
	mfc0	t3,C0_INX	
	move	v0,zero			# LDSLOT
	bltz	t3,1f			# not found
	nop				# BDSLOT

#ifdef PROBE_BUG
	move	t4,t3			# start with original index "hint"
6:
	c0	C0_READI		# read an entry.
	mfc0	t5,C0_TLBHI
	nop
	beq	t5,t2,2f		# test for pid/vpn match
	nop
	and	t5,TLBHI_VPNMASK	# isolate vpn
	bne	t5,a1,3f		# test vpn match for global pages
	nop
	mfc0	t5,C0_TLBLO		# vpn match, check global bit
	nop
	and	t5,TLBLO_G
	bne	t5,zero,2f
	nop
					# did not find it, try another slot
3:
	li	t5,TLBINX_INXMASK<<TLBINX_INXSHIFT
	beq	t4,t5,4f
	nop
	add	t4,1<<TLBINX_INXSHIFT
5:	mtc0	t4,C0_INX
	b	6b
	nop
	/*
	 * Bad news if we get to here. The entire tlb has been searched
	 * without finding the entry that match on the probe instruction.
	 * Return -1 to the caller and let him panic. To dangerous to restore
	 * the sr.
	 */
4:	li	v0,-1			# searched entire tlb, panic time.
	j	ra			# let caller panic
	nop
2:
	sne	v0,t3,t4		# found the match
	mtc0	t2,C0_TLBHI
	mtc0	a2,C0_TLBLO
	nop
#endif	PROBE_BUG
	c0	C0_WRITEI		# re-use slot
	b	2f
	nop				# BDSLOT

1:	c0	C0_WRITER		# use random slot
2:	mtc0	t1,C0_TLBHI		# restore TLBPID
	mtc0	t0,C0_SR		# restore SR
	j	ra
	nop
	.set	reorder
	END(tlbdropin)

/*
 * flush entire non-wired tlb
 */
LEAF(flush_tlb)
	.set	noreorder
	li	t0,K0BASE&TLBHI_VPNMASK	# set up to invalidate entries
	li	v0,TLBRANDOMBASE	# first entry to invalidate
	li	v1,TLBRANDOMBASE+NRANDOMENTRIES	# last entry plus one
	mfc0	t1,C0_TLBHI		# save pid
	mfc0	t2,C0_SR
	mtc0	zero,C0_SR		# interrupts off
	mtc0	t0,C0_TLBHI
	mtc0	zero,C0_TLBLO
	nop
	sll	t0,v0,TLBINX_INXSHIFT
1:
	mtc0	t0,C0_INX		# set index
	addu	v0,1			# bump to next entry
	c0	C0_WRITEI		# invalidate
	bne	v0,v1,1b		# more to do
	sll	t0,v0,TLBINX_INXSHIFT	# BDSLOT

	mtc0	t1,C0_TLBHI
	nop
	mtc0	t2,C0_SR
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(flush_tlb)

/*
 * set_tlbpid -- set current tlbpid and change all the pid fields
 * in the wired tlb entries to the new pid.
 * a0 -- tlbpid -- tlbcontext number to use (0-63)
 */
LEAF(set_tlbpid)
	.set	noreorder
	mfc0	t0,C0_SR
	mtc0	zero,C0_SR		# interrupts off
	sll	a0,TLBHI_PIDSHIFT	# line up pid bits
	li	t1,NWIREDENTRIES-1
1:
	sll	t2,t1,TLBINX_INXSHIFT
	mtc0	t2,C0_INX
	nop
	c0	C0_READI		# read current tlb entry
	mfc0	t2,C0_TLBHI
	and	t2,~TLBHI_PIDMASK	# take down current pid bits
	or	t2,a0			# assert new pid
	mtc0	t2,C0_TLBHI		# write back new value
	nop
	c0	C0_WRITEI
	bne	t1,zero,1b
	sub	t1,1			# BDSLOT

	mtc0	t0,C0_SR
	j	ra
	nop				# BDSLOT
	.set	reorder
	END(set_tlbpid)
