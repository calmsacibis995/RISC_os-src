#ident "$Header: tlb.s,v 1.5 90/01/11 14:51:11 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * tlb.s -- dbgmon tlb code
 */

#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"

	.text

/*
 * get_tlblo(index) -- return entry low contents of tlb entry "index"
 */
LEAF(get_tlblo)
	.set	noreorder
	mfc0	t0,C0_SR		# save sr
	mtc0	zero,C0_SR		# disable interrupts
	mfc0	t1,C0_TLBHI		# save current pid
	sll	a0,TLBINX_INXSHIFT	# position index
	mtc0	a0,C0_INX		# drop it in C0 register
	nop
	c0	C0_READI		# read entry to entry hi/lo
	nop
	mfc0	v0,C0_TLBLO		# to return value
	mtc0	t1,C0_TLBHI		# restore current pid
	j	ra
	mtc0	t0,C0_SR		# (BDSLOT) restore sr
	.set	reorder
	END(get_tlblo)

/*
 * get_tlb_6000(index, side)-- return the contents of the tlb entry "index"
 * for the side that has been asked for (0 or 1)
 */

LEAF(get_tlb_6000)
	.set	noreorder
	mfc0	t6,C0_SR		# save SR
	li	t5,SR_MM_MODE		# disable interrupts
	mtc0	t5,C0_SR		# and turn on MM mode
	nop

	/* start forming address for reading the tlb */
	/* most significant 7 bits don't matter      */

	ori	t0,$0,0xf		# make sure you access reserved area
	sll	t0,12			# hit the tlbs, make space for index
	or 	t0,a0			# or in the index
	sll	t0,2			# form effective address

	beq	a1,zero,1f		# set 0 or set 1?
	nop				# BDSLOT
	lcache	v0,1(t0)		# read side 1
	nop				# (DSLOT #1)
	nop				# (DSLOT #2)
	nop				# (DSLOT #3)
	b 	exit			# get out
	nop				# BDSLOT
1:	lcache	v0,0(t0)		# read side 0
	nop				# (DSLOT #1)
	nop				# (DSLOT #2)
	nop				# (DSLOT #3)
exit:	j	ra			# return
	mtc0	t6,C0_SR		# (BDSLOT) restore SR
	.set 	reorder
	END(get_tlb_6000)


 /*
 * get_tlbhi(index) -- return entry high contents of tlb entry "index"
 */
LEAF(get_tlbhi)
	.set	noreorder
	mfc0	t0,C0_SR		# save sr
	mtc0	zero,C0_SR		# disable interrupts
	mfc0	t1,C0_TLBHI		# save current pid
	sll	a0,TLBINX_INXSHIFT	# position index
	mtc0	a0,C0_INX		# drop it in C0 register
	nop
	c0	C0_READI		# read entry to entry hi/lo
	nop
	mfc0	v0,C0_TLBHI		# to return value
	mtc0	t1,C0_TLBHI		# restore current pid
	j	ra
	mtc0	t0,C0_SR		# (BDSLOT) restore sr
	.set	reorder
	END(get_tlbhi)

/*
 * get_ptag(index,side)-- return the contents of the ptag entry "index"
 * for the side that has been asked for (0 or 1)
 */

LEAF(get_ptag)
	.set	noreorder
	mfc0	t6,C0_SR		# save SR
	li	t5,SR_MM_MODE		# disable interrupts
	mtc0	t5,C0_SR		# and turn on MM mode
	nop

	/* start forming address for reading the ptag*/
	/* most significant 7 bits don't matter      */

	ori	t0,$0,0x1f		# make sure you access PTAG area
	sll	t0,11			# make space for index
	or 	t0,a0			# or in the index
	sll	t0,2			# form effective address

	beq	a1,zero,1f		# set 0 or set 1?
	nop				# BDSLOT
	lcache	v0,1(t0)		# read side 1
	nop				# (DSLOT #1)
	nop				# (DSLOT #2)
	nop				# (DSLOT #3)
	b 	ptexit			# get out
	nop				# BDSLOT
1:	lcache	v0,0(t0)		# read side 0
	nop				# (DSLOT #1)
	nop				# (DSLOT #2)
	nop				# (DSLOT #3)
ptexit:	j 	ra			# return
	mtc0	t6,C0_SR		# (BDSLOT) restore SR
	.set 	reorder
	END(get_ptag)

/*
 * get_tlbpid() -- return current tlb pid
 */
LEAF(get_tlbpid)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,1f
	.set	noreorder
	mfc0	v0,C0_TLBHI		# to return value
	nop
	.set	reorder
	and	v0,TLBHI_PIDMASK
	srl	v0,TLBHI_PIDSHIFT 
	j	ra

	.set	noreorder
1:	j	ra
	mfc0	v0,C0_PID		# (BDSLOT) R6000 PID
	.set	reorder
	END(get_tlbpid)

/*
 * set_tlbpid() -- set current tlb pid
 */
LEAF(set_tlbpid)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,1f
	sll	a0,TLBHI_PIDSHIFT
	and	a0,TLBHI_PIDMASK
	.set	noreorder
	j	ra
	mtc0	a0,C0_TLBHI		# (BDSLOT) set new pid

1:	j	ra
	mtc0	a0,C0_PID		# (BDSLOT) R6000 PID
	.set	reorder
	END(set_tlbpid)

/*
 * probe_tlb(address, pid) -- return index for tlb probe of address:pid
 * for the 6000 returned value will indicate the side where address was
 * mapped.
 */
LEAF(probe_tlb)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,1f
	.set	noreorder
	mfc0	t0,C0_SR		# save sr
	mtc0	zero,C0_SR		# disable interrupts
	mfc0	t1,C0_TLBHI		# save current pid
	and	a0,TLBHI_VPNMASK	# construct tlbhi for probe
	sll	a1,TLBHI_PIDSHIFT
	and	a1,TLBHI_PIDMASK
	or	a0,a1
	mtc0	a0,C0_TLBHI
	nop
	c0	C0_PROBE		# probe entry to entry hi/lo
	nop
	mfc0	v0,C0_INX
	nop
	sra	v0,TLBINX_INXSHIFT	# position index
	mtc0	t1,C0_TLBHI		# restore current pid
	j	ra
	mtc0	t0,C0_SR		# (BDSLOT) restore sr
	.set	reorder

1:	.set	noreorder
	mfc0 	t6,C0_SR		# save SR
	li	t5,SR_MM_MODE		# disable interrupts
	mtc0	t5,C0_SR		# and turn on MM mode.
	nop

	mfc0	t5,C0_PID		# save current PID
	nop
	mtc0	a1,C0_PID		# set the specified PID
	nop

	srl	t0,a0,25		# right justify ms 7 b's
	sll	t0,12			# clear ls 12 b's
	ori	t0,0x1e			# to access tlb reserved area
	sll	t0,13			# make space for tlb index
	srl	t1,a0,12		# isolate VPN <24..14> and 
	andi	t1,0x1ffc		# form bits <12..0>
	add 	t0,t1			# add to form effective address
	
	lcache  t2,(t0)                 # read side-0
	li      t3,SR_CM0               # (DSLOT-1) set up side-0 mask
	li	t1,TLB_V_R6000		# (DSLOT-2) set up valid bit mask
	nop                             # (DSLOT-3)
	mfc0    t8,C0_SR                # fetch SR for CM0 state
	nop
	lcache  t4,1(t0)                # read side-1
	li	t7,SR_CM1		# (DSLOT-1) set up side 1 mask
        and     t8,t3                   # (DSLOT-2) isolate CM0 bit
	nop                             # (DSLOT-3)
	mfc0    t9,C0_SR                # fetch SR for CM1 state
	nop
	and     t9,t7                   # (BDSLOT) isolate CM1 bit

	bne	t8,zero,side1		# try side 1 if no hit on side 0
	nop				# (BDSLOT)
	and	t2,t1			# isolate valid bit
	beq	t2,zero,fail		# valid bit set?
	nop				# (BDSLOT)
	li	v0,0			# return side 0
	b	pass			# get out successfully
	nop				# (BDSLOT)
side1:  bne	t9,zero,fail		# exit if not found
	nop				# (BDSLOT)
	and 	t4,t1			# isolate valid bit
	beq	t4,zero,fail		# hit but is the valid bit set?
	nop				# (BDSLOT)
	li	v0,1			# return side 1
	b	pass			# exit successfully
	nop				# (BDSLOT)
fail:	li	v0,-1			# - not found
pass:	mtc0	t5,C0_PID		# (BDSLOT) restore PID
	j	ra			# return
	mtc0	t6,C0_SR		# (BDSLOT) restore SR
	.set reorder
	END(probe_tlb)

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
	j	ra
	mtc0	v0,C0_SR		# (BDSLOT) restore sr
	.set	reorder

	END(invaltlb)


/*
 * tlbwired(indx, tlbhi, pte) -- setup wired ITLB entry
 * a0 -- indx -- tlb entry index , -- side for the 6000s
 * a1 -- tlbhi -- virtual page number and pid
 * a2 -- pte -- contents of pte
 * 
 * t6 -- save SR
 * t5 -- save current PID
 */
LEAF(tlbwired)
	.set	noreorder
	mfc0	v0,C0_SR		# save SR and disable interrupts
	mtc0	zero,C0_SR
	lw	v1,machine_type
	beq	v1,BRDTYPE_R6300,6f
	sll	a0,TLBINX_INXSHIFT
	mfc0	t1,C0_TLBHI		# save current TLBPID
	mtc0	a1,C0_TLBHI		# set VPN and TLBPID
	mtc0	a2,C0_TLBLO		# set PPN and access bits
	mtc0	a0,C0_INX		# set INDEX to wired entry
	nop
	c0	C0_WRITEI		# drop it in
	mtc0	t1,C0_TLBHI		# restore TLBPID
1:	j	ra
	mtc0	v0,C0_SR		# (BDSLOT) restore SR
	.set	reorder

6:	.set 	noreorder
	mfc0    t6,C0_SR                # save SR
	li      t5,SR_MM_MODE           # disable interrupts
	mtc0    t5,C0_SR                #  and turn on MM Mode
	nop                             # give it time to settle
	mfc0    t5,C0_PID               # save current PID
	andi	t4,a1,PIDMASK_R6000	# get the pid from a1
	mtc0 	t4,C0_PID		# set the specified PID
	nop

	/* (KUSEG) and (K2SEG addresses above KPTEBASE) are marked global */
	/* The rest are PID specific				          */
	
	srl	t2,a1,VPNSHIFT_R6000	# get the VPN from a1
	sll	t2,VPNSHIFT_R6000	# clear 14 lsb's
	li	t0, KPTEBASE		# 
	sltu	t8,t2,t0		# vaddr < KPTEBASE ?
	beq	t8,zero,f_addr		# no it's PID specific
	li      t0,K0BASE               # (BDSLOT)
	sltu    t8,t0,t2                # K0BASE < address?

	/* PID specific - t8 = 0; Global - t8 = 1	*/

f_addr:	srl	t0,a1,25		# right justify ms 7 b's
	sll     t0,12                   # clear ls 12 b's
	ori     t0,0x1e                 # to access tlb reserved area
	sll     t0,13                   # make space for tlb index
	srl     t1,a1,12                # isolate VPN <24..14> and
	andi    t1,0x1ffc               # form bits <12..0>
	add     t0,t1                   # add to form effective address

	bne	a0,zero,side_1		# map side 1
	nop				# (BDSLOT)
	beq	t8,zero,pd_0		# global or pid specific?
	nop				# (BDSLOT)
	scache	a2,2(t0)		# write tlb in set 0 - global set
	b	w_exit			# exit
	mtc0	t5,C0_PID		# BDSLOT - restore PID
pd_0:	scache  a2,(t0) 		# write tlb in set 0 - global off
	b	w_exit			# exit
	mtc0	t5,C0_PID		# BDSLOT - restore PID
side_1:	beq	t8,zero,pd_1		# global or pid specific?
	nop				# (BDSLOT)
	scache	a2,3(t0)		# write tlb in set 1 - global set
	b	w_exit			# exit
	mtc0	t5,C0_PID		# BDSLOT - restore PID
pd_1:	scache	a2,1(t0)		# write tlb in set 1 - global off

w_exit: j 	ra			# return;
	mtc0	t6,C0_SR		# restore SR.

	END(tlbwired)
