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
/* $Header: spl_std.s,v 1.4.1.2 90/05/10 05:32:13 wje Exp $ */
#include	"../ml/assym.s"


/*
 * Primitives
 */ 

/*
 * Interrupts: (8, 1,2 soft ints, 3-8 hard ints)
 * 8	Bus error/timeout/sec/ded
 * 7	profiling clock.
 * 6	interproc interrupt (reserved)
 * 5	sched clock.
 * 4	uart.
 * 3	vectored devices
 * 2	softnet
 * 1	softclock
 */

/*
 * spl0: Don't block against anything.
 */
LEAF(spl0)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(spl0)

/*
 * block against clock software interrupts (level 1 softint, was spl1).
 */
LEAF(splsoftclock)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK1
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
	li	v1,SR_IEC|SR_IMASK2
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splnet)

/*
 * splbio: block against disk and tape device interrupts. all are vme
 */
LEAF(splbio)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK3
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splbio)

/*
 * block against device interrupts. (level 3 hardint, was spl5).
 */
LEAF(spltty)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK4
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(spltty)

/*
 * block against sched clock interrupts. (level 5 hardint, was spl6).
 */
LEAF(splclock)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK6
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
	li	v1,SR_IEC|SR_IMASK8
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR
	.set	reorder
	END(splhi)

/*
 * block against all interrupts (was spl7).
 */
LEAF(splall)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK8
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splall)

/*
 * splimp: block against network device interrupts, check first to make
 * sure not already spl'ed higher
 */
LEAF(splimp)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK4
	and	v1,v0
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splimp)

/*
 * splx(ipl) -- restore previously saved ipl
 */
LEAF(splx)
	.set	noreorder
	mfc0	v0,C0_SR
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
	j	ra
	mtc0	v0,C0_SR		# BDSLOT
	.set	reorder
	END(siroff)
