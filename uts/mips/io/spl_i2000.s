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
/* $Header: spl_i2000.s,v 1.7.1.2 90/05/10 05:31:09 wje Exp $ */
#include	"../ml/assym.s"

	
/*
 * Primitives
 */ 

/*
 * Interrupts: (8, 1,2 soft ints, 3-8 hard ints)
 * 8	Bus error/timeout/sec/ded/vert
 * 7	clock.
 * 6	interproc interrupt (reserved)
 * 5	scsi devs, floppy
 * 4	uart, kbd, lance
 * 3	lp
 * 2	softnet
 * 1	softclock
 */

	.globl	iplmask
	.globl	ipl_special_mask
	.data
ipl_special_bits:
	.word	0	# bits to OR into status register
	.text

#define IPLMASK(x) iplmask+((x-1)*4)

/*
 * spl0: Don't block against anything.
 */
LEAF(spl0)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	a0,ipl_special_mask
	li	v1,SR_IEC|SR_IMASK0
	and	v1,a0		# turn off any masked bits
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(spl0)

/*
 * splsoftclock:block against clock software interrupts (was spl1, 
 *	level 1 softint).
 */
LEAF(splsoftclock)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(1)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splsoftclock)

/*
 * splnet: block against network software interrupts (level 2 softint).
 */
LEAF(splnet)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(2)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splnet)

LEAF(spltty)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(5)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(spltty)

LEAF(splbio)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(6)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splbio)

LEAF(splclock)
XLEAF(splall)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(7)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splclock)

/* TODO splhi should leave clock on if not off already */
/* DO NOT DO THIS, unless you also change clock.c to also use splall() instead
 *	of splhi()--vjs */
LEAF(splhi)
XLEAF(splhigh)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(8)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splhi)

/*
 * splimp: block against network device interrupts, check first to make
 * sure not already spl'ed higher
 */
LEAF(splimp)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,IPLMASK(5)
	nop
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR
	.set	reorder
	END(splimp)

/*
 * splx(ipl) -- restore previously saved ipl
 */
LEAF(splx)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	v1,ipl_special_mask
	lw	a1,ipl_special_bits
	and	a0,v1		# turn off any masked bits
	or	a0,a1		# turn on any special bits
	j	ra
	mtc0	a0,C0_SR
	.set	reorder
	END(splx)

#ifdef BSD42
/*
 * setsoftnet() - make software network interrupt request
 */
EXPORT(setsoftnet)
	li	a0, CAUSE_SW2
	j	siron

/*
 * acksoftnet() - acknowledge software network interrupt
 */
EXPORT(acksoftnet)
	li	a0, CAUSE_SW2
	j	siroff

/*
 * setsoftclock() - make software clock interrupt request
 */
EXPORT(setsoftclock)
	li	a0, CAUSE_SW1
	j	siron

/*
 * acksoftclock() - acknowledge software clock interrupt
 */
EXPORT(acksoftclock)
	li	a0, CAUSE_SW1
	j	siroff
#endif BSD42

/*
 * siron(level) -- make software interrupt request
 */
LEAF(siron)
	.set	 noreorder
	mfc0	v0,C0_SR
	nop				# LDSLOT  just to be careful
	mtc0	zero,C0_SR		# disable all interrupts
	nop				# wait a couple of cycles
	nop				#  to ensure disable takes effect
	mfc0	v1,C0_CAUSE
	nop				# BDSLOT
	or	v1,a0
	mtc0	v1,C0_CAUSE
	lw	a2,ipl_special_mask
	lw	a1,ipl_special_bits
	and	v0,a2		# turn off any masked bits
	or	v0,a1		# turn on any special bits
	j	ra
	mtc0	v0,C0_SR		# BDSLOT
	.set	reorder
	END(siron)

/*
 * siroff(level) -- acknowledge software interrupt request
 */
LEAF(siroff)
	.set	 noreorder
	mfc0	v0,C0_SR
	nop				# LDSLOT  just to be careful
	mtc0	zero,C0_SR		# disable all interrupts
	nop				# wait a couple of cycles
	nop				#  to ensure disable takes effect
	mfc0	v1,C0_CAUSE
	not	a0			# LDSLOT
	and	v1,a0
	mtc0	v1,C0_CAUSE
	lw	a2,ipl_special_mask
	lw	a1,ipl_special_bits
	and	v0,a2		# turn off any masked bits
	or	v0,a1		# turn on any special bits
	j	ra
	mtc0	v0,C0_SR		# BDSLOT
	.set	reorder
	END(siroff)

/*
 * irq5on: Allow hardware level 5 interrupt
 */

LEAF(irq5on)
	.set	noreorder
	mfc0	v0,C0_SR
	nop				# LDSLOT  just to be careful
	mtc0	zero,C0_SR		# disable all interrupts
	lw	v1,ipl_special_mask	#  to ensure disable takes effect
	li	a0,SR_IBIT8		# wait a couple of cycles
	or	v1,a0
	sw	v1,ipl_special_mask
	or	a0,v0
	j	ra
	mtc0	a0,C0_SR	# BDSLOT
	.set	reorder
	END(irq5on)

/*
 * irq5off: Disallow hardware level 5 interrupt
 */

LEAF(irq5off)
	.set	noreorder
	mfc0	v0,C0_SR
	nop
	mtc0	zero,C0_SR		# disable all interrupts
	lw	v1,ipl_special_mask	#  to ensure disable takes effect
	li	a0,~SR_IBIT8		# wait a couple of cycles
	and	v1,a0
	sw	v1,ipl_special_mask
	and	a0,v0
	j	ra
	mtc0	a0,C0_SR	# BDSLOT
	.set	reorder
	END(irq5off)

/*
 * irq5x: Restore interrupt for irq5 code.
 */

LEAF(irq5x)
	.set	noreorder
	mfc0	v0,C0_SR
	j	ra
	mtc0	a0,C0_SR
	.set	reorder
	END(irq5x)
