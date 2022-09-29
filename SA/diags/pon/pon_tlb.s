#ident "$Header: pon_tlb.s,v 1.8.5.1 90/07/18 14:33:43 huang Exp $"
/* $Copyright: |
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

#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/param.h"


/*
 * Reestablish desired libsa status register, clear any pending write bus
 * error interrupts.  Returns current SR.
 */
LEAF(ta_spl)

	move	v1,zero
1:
	.set noreorder
	mfc0	v0,C0_SR
	nop
	.set reorder
	sub	v1,1
	beq	v1,zero,1b

#ifndef	R3030
	lw	v1,machine_type
	beq	v1,BRDTYPE_R2400,1f	# if INTREPID
	beq	v1,BRDTYPE_M180,1f	# if M180

	lw	zero,SBE_ADDR|K1BASE	# M-SERIES
	sw	zero,SBE_ADDR|K1BASE
	b	2f
1:
	lw	zero,FAR|K1BASE		# INTREPID or M180
2:
	li	v1,SR_IMASK7|SR_IEC
	.set noreorder
	mtc0	v1,C0_SR
	nop
	.set reorder
#endif	!R3030
	j	ra

END(ta_spl)


/*
 * Invalidate the ith ITLB entry.  Called whenever a specific TLB entry needs
 * to be invalidated.
 */
LEAF(pon_invaltlb) # HERE

	li	a2,K0BASE&TLBHI_VPNMASK
	move	v1,zero
1:
	.set noreorder
	mfc0	a1,C0_TLBHI		# save current TLBHI
	nop
	mfc0	v0,C0_SR		# save SR and disable interrupts
	nop
	beq	v1,zero,1b
	sub	v1,1

	mtc0	zero,C0_SR
	nop
	mtc0	a2,C0_TLBHI		# invalidate entry
	nop
	mtc0	zero,C0_TLBLO
	nop
	sll	a0,TLBINX_INXSHIFT
	mtc0	a0,C0_INX
	nop
	c0	C0_WRITEI
	nop
	mtc0	a1,C0_TLBHI
	nop
	mtc0	v0,C0_SR
	nop
	.set reorder
	j	ra
	nop				# BDSLOT

END(pon_invaltlb)


/*
 * Setup wired ITLB entry
 *
 * tlbwired(indx, tlbhi, pte)
 * 	a0 -- indx -- TLB entry index
 *	a1 -- tlbhi -- virtual page number and PID
 *	a2 -- pte -- contents of PTE
 */
LEAF(tlbwired)

	sll	a0,TLBINX_INXSHIFT
	move	v1,zero
1:
	.set noreorder
	mfc0	a3,C0_TLBHI		# save current TLBPID
	nop
	mfc0	v0,C0_SR		# save SR and disable interrupts
	nop
	beq	v1,zero,1b
	sub	v1,1

	mtc0	zero,C0_SR
	nop
	mtc0	a1,C0_TLBHI		# set VPN and TLBPID
	nop
	mtc0	a2,C0_TLBLO		# set PPN and access bits
	nop
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_WRITEI		# drop it in
	nop
	mtc0	a3,C0_TLBHI		# restore TLBPID
	nop
	mtc0	v0,C0_SR		# restore SR
	nop
	.set reorder
	j	ra
	nop				# BDSLOT

END(tlbwired)


/*
 * Set current TLBPID.
 * NOTE: this assumes that argument is already appropriately positioned to
 * drop into TLBHI.
 */
LEAF(set_tlbpid)

	.set	noreorder
	mtc0	a0,C0_TLBHI
	nop				# BDSLOT
	j	ra
	nop				# BDSLOT
	.set	reorder

END(set_tlbpid)


/*
 * Setup wired ITLB entry.
 *
 * write_indexed_lo(indx, data)
 *	a0 -- indx -- TLB entry index
 *	a1 -- data -- data to drop into TLBLO
 */
LEAF(write_indexed_lo)

	.set	noreorder
	sll	a0,TLBINX_INXSHIFT
	mtc0	a1,C0_TLBLO		# set PPN and access bits
	nop
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_WRITEI		# drop it in
	nop
	j	ra
	nop				# BDSLOT
	.set	reorder

END(write_indexed_lo)


/*
 * Setup wired ITLB entry.
 *
 * write_indexed_hi(indx, data)
 *	a0 -- indx -- TLB entry index
 *	a1 -- data -- data to drop into TLBHI
 */
LEAF(write_indexed_hi)

	.set	noreorder
	sll	a0,TLBINX_INXSHIFT
	mtc0	a1,C0_TLBHI		# set PPN and access bits
	nop
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_WRITEI		# drop it in
	nop
	j	ra
	nop				# BDSLOT
	.set	reorder

END(write_indexed_hi)


/*
 * Returns contents of wired ITLB entry.
 *
 * read_indexed_hi(indx)
 *	a0 -- indx -- TLB entry index
 */
LEAF(read_indexed_hi)

	.set	noreorder
	sll	a0,TLBINX_INXSHIFT
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_READI		# read into hi/lo pair
	nop
	move	v1,zero
1:
	mfc0	v0,C0_TLBHI		# return TLBHI
	nop
	beq	v1,zero,1b
	sub	v1,1

	j	ra
	nop				# BDSLOT
	.set	reorder

END(read_indexed_hi)


/*
 * Returns contents of wired ITLB entry.
 *
 * read_indexed_lo(indx)
 *	a0 -- indx -- TLB entry index
 */
LEAF(read_indexed_lo)

	.set	noreorder
	sll	a0,TLBINX_INXSHIFT
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_READI		# read into hi/lo pair
	nop
	move	v1,zero
1:
	mfc0	v0,C0_TLBLO		# return TLBLO
	nop
	beq	v1,zero,1b
	sub	v1,1

	j	ra
	nop				# BDSLOT
	.set	reorder

END(read_indexed_lo)


/*
 * Probes for a single TLB entry in the ITLB.  Probe for RPID/VPAGE in TLB.
 * If it doesn't exist, all done.  Returns index of matching entry or -1 if
 * not match.
 */
LEAF(matchtlb)

	.set	noreorder
	move	v1,zero
1:
	mfc0	a3,C0_TLBHI		# save TLBHI (for TLBPID)
	nop
	sll	a2,a1,TLBHI_VPNSHIFT	# construct TLBHI
	sll	a0,TLBHI_PIDSHIFT	# shift TLBPID
	or	a2,a0
	mfc0	v0,C0_SR		# disable interrupts.
	nop
	beq	v1,zero,1b
	sub	v1,1

	mtc0	zero,C0_SR
	nop
	mtc0	a2,C0_TLBHI
	nop				# let TLBHI get through pipe
	c0	C0_PROBE		# probe for address
	nop
	move	v1,zero
1:
	mfc0	a2,C0_INX
	nop
	beq	v1,zero,1b
	sub	v1,1

	mtc0	a3,C0_TLBHI		# restore old TLBHI
	nop
	mtc0	v0,C0_SR		# restore sr and return
	nop
	j	ra
	move	v0,a2			# BDSLOT
	.set	reorder

END(matchtlb)


LEAF(get_sr)

	move	v1,zero
1:
	.set noreorder
	mfc0	v0,C0_SR		# return status register
	nop
	.set reorder
	sub	v1,1
	beq	v1,zero,1b

	j	ra

END(get_sr)


LEAF(clear_SR_PE)

	move	v1,zero
1:
	.set noreorder
	mfc0	v0,C0_SR		# return status register
	nop
	.set reorder
	sub	v1,1
	beq	v1,zero,1b

	and	v0,SR_PE
	.set noreorder
	mtc0	v0,C0_SR
	nop
	.set reorder
	j	ra

END(clear_SR_PE)


LEAF(get_cache_sp)

	and	sp,0x1fffffff
	add	sp,K0BASE
	j	ra

END(get_cache_sp)
