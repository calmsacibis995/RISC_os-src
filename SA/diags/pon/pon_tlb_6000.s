#ident "$Header: pon_tlb_6000.s,v 1.3.7.1 90/07/18 14:33:48 huang Exp $"
/* $Copyright$ */

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
	.set noreorder
	mfc0	v0,C0_SR
	nop				# (LDSLOT, to be safe)
	mtc0	zero,C0_SR		# disable interrupts, disable BEV
	j	ra
	nop				# (BDSLOT)
	.set reorder
END(ta_spl)


/*
 * Invalidate the TLB entry for virtual address 'vaddr'.
 *	a0 -- side  -- side of S-cache for tlb, 0 or 1
 *	a1 -- vaddr -- virtual address
 */
LEAF(pon_invaltlb)
	.set	noreorder
	mfc0	t7,C0_SR		# save current SR
	li	t0,SR_MM_MODE		# and enable MM_MODE
	or	t0,t7
	mtc0	t0,C0_SR

	srl	t0,a1,25		# r-justify most signif 7 vaddr bits
	sll	t0,12			#  shift back to clear lower bits
	ori	t0,0x1e			# merge what will be bits 17:13
	sll	t0,13			#  and shift back into position
	srl	t1,a1,VPNSHIFT_R6000-2	# move bits 24:14 ...
	andi	t1,0x1ffc		#  ... to 12:2 and clear 2 lsbits
	add	t0,t1			# effective addr for S-cache line

	bne	a0,zero,1f		# side 0 or side 1?
	nop				# (BDSLOT)
	scache	zero,0(t0)		# set side-0 to not-Valid
	inval	0(t0)			#   and invalidate the line
	b	9f
	nop				# (BDSLOT)

1:	scache	zero,1(t0)		# set side-1 to not-Valid
	inval	1(t0)			#   and invalidate the line

9:	j	ra
	mtc0	t7,C0_SR		# (BDSLOT) restore SR
	.set	reorder
END(pon_invaltlb)


/*
 * Setup TLB entry
 *
 * tlbwired(vaddr, pte)
 *	a0 -- side -- side of S-cache for tlb, 0 or 1
 *	a1 -- vaddr -- virtual address
 *	a2 -- pte -- contents of PTE
 */
LEAF(tlbwired)
	.set	noreorder
	mfc0	t7,C0_SR		# save current SR
	li	t0,SR_MM_MODE		# and enable MM_MODE
	or	t0,t7
	mtc0	t0,C0_SR

	srl	t0,a1,25		# r-justify most signif 7 vaddr bits
	sll	t0,12			#  shift back to clear lower bits
	ori	t0,0x1e			# merge what will be bits 17:13
	sll	t0,13			#  and shift back into position
	srl	t1,a1,VPNSHIFT_R6000-2	# move bits 24:14 ...
	andi	t1,0x1ffc		#  ... to 12:2 and clear 2 lsbits
	add	t0,t1			# effective addr for S-cache line
	and	t2,a2,TLB_G_R6000	# isolate Global bit

	bne	a0,zero,5f		# side-0 or side-1?
	nop				# (BDSLOT)

	bne	t2,zero,1f		# Global?
	nop				# (BDSLOT)
	scache	a2,0(t0)		# stuff non-Global TLB entry into side-0
	b	9f
	nop				# (BDSLOT)
1:	scache	a2,2(t0)		# stuff Global TLB entry into side-0
	b	9f
	nop				# (BDSLOT)

5:	bne	t2,zero,6f		# Global?
	nop				# (BDSLOT)
	scache	a2,1(t0)		# stuff non-Global TLB entry into side-1
	b	9f
	nop				# (BDSLOT)
6:	scache	a2,3(t0)		# stuff Global TLB entry into side-1

9:	j	ra
	mtc0	t7,C0_SR		# (BDSLOT) restore SR
	.set	reorder
END(tlbwired)


/*
 * Get current TLBPID.
 */
LEAF(get_tlbpid)
	.set	noreorder
	mfc0	v0,C0_PID		
	j	ra
	nop				# (BDSLOT)
	.set	reorder
END(get_tlbpid)


/*
 * Set current TLBPID.
 */
LEAF(set_tlbpid)
	.set	noreorder
	j	ra
	mtc0	a0,C0_PID		# (BDSLOT)
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
	j	ra
END(write_indexed_lo)


/*
 * Probes for a single TLB entry in the ITLB.  Probe for RPID/VPAGE in TLB.
 * If it doesn't exist, all done.  Returns index of matching entry or -1 if
 * not match.
 */
LEAF(matchtlb)
	j	ra
END(matchtlb)


LEAF(get_sr)
	.set noreorder
	mfc0	v0,C0_SR		# return status register
	j	ra
	nop				# (BDSLOT)
	.set reorder
END(get_sr)
