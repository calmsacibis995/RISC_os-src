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
/* $Header: spl_r6000.s,v 1.5.1.5 90/05/10 05:32:05 wje Exp $ */

#include "../ml/assym.s"

/*
 * Interrupts: (4, 1,2 soft ints, 3 external, 4 cp1 )
 * 4	cp1			always enabled
 * 3	external		always enabled (ints controlled by SBC IntMask)
 * 2	softnet			never enabled (function moved to SBC IntVector)
 * 1	softclock		never enabled (function moved to SBC IntVector)
 *
 * The R6000 has two software and two hardware interrupt bits in the Status
 * Register, and a 32-bit InterruptVector in the System Bus Chip (SBC).  We
 * consolidate all interrupts except for Cp1 into the SBC InterruptVector.
 * These 32 interrupts are deemed "external" and are controlled as a group by
 * the SR "external interrupt" bit mask.  As a general rule, we want to leave
 * this SR bit turned on to enable external interrupts, and to control the
 * interrupts completely by the SBC InterruptMask.
 *
 * To be safe, when we enable some interrupt level, we always enable the SR
 * "external interrupt" and "interrupts enabled" bits (and the "cp1 interrupt",
 * too, because this is innocuous to the kernel).  We really only need to
 * enable the "external interrupt" bit once and it stays on, but we don't want
 * to take the chance that some rogue algorithm turns off those SR bits.
 * We still probably need to turn on SR_IEC, anyway.
 */

/*
 * spl0: Don't block against anything.
 */
LEAF(spl0)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			# current SBC mask
	li	t0,CSR_IVECTMASK_NONE
	sw	t0,(v1)			# enable all SBC ints
	ori	t1,SR_IEC|SR_IMASK2	# enable SR interrupts
	j	ra
	mtc0	t1,C0_SR		# (BDSLOT)
	.set	reorder
	END(spl0)

/*
 * spl1: block against clock software interrupts (level 1 softint).
 */
LEAF(splsoftclock)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	li	t0,CSR_IVECTMASK_SW1
	and	t0,v0			/* reset SBC mask with the lessor */
	sw	t0,(v1)			/*  of what's there and SW1	  */
	ori	t1,SR_IEC		/* enable SR interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
	.set	reorder
	END(splsoftclock)

/*
 * splnet: block against network software interrupts (level 2 softint).
 */
LEAF(splnet)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	li	t0,CSR_IVECTMASK_SW2
	and	t0,v0			/* reset SBC mask with the lessor */
	sw	t0,(v1)			/*  of what's there and SW2	  */
	ori	t1,SR_IEC		/* enable SR interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
	.set	reorder
	END(splnet)

/*
 * splbio: block against disk and tape device interrupts. all are vme
 */
LEAF(splbio)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	li	t0,CSR_IVECTMASK_GBA
	and	t0,v0			/* reset SBC mask with the lessor */
	sw	t0,(v1)			/*  of what's there and GBA mask  */
	ori	t1,SR_IEC		/* enable SR interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
	.set	reorder
	END(splbio)

/*
 * spl5: block against device interrupts.
 */
LEAF(spltty)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	li	t0,CSR_IVECTMASK_IO
	and	t0,v0			/* reset SBC mask with the lessor */
	sw	t0,(v1)			/*  of what's there and IO mask   */
	ori	t1,SR_IEC		/* enable general interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
	.set	reorder
	END(spltty)

/*
 * block against all interrupts (was spl7).
 */
LEAF(splall)
XLEAF(splhi)
XLEAF(splhigh)
XLEAF(splclock)
	.set	noreorder
	mfc0	t0,C0_SR
	mtc0	zero,C0_SR		/* firmly disable interrupts */
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	sw	zero,(v1)		/* (BDSLOT) disable all via SBC mask */
	j	ra
	mtc0	t0,C0_SR		/* (BDSLOT) restore SR */
	.set	reorder
	END(splall)

/*
 * splimp: block against network device interrupts, check first to make
 * sure not already spl'ed higher
 */
LEAF(splimp)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	li	t0,CSR_IVECTMASK_IO
	and	t0,v0			/* reset SBC with the lessor    */
	sw	t0,(v1)			/*  of what's there and IO mask */
	ori	t1,SR_IEC		/* enable general interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
	.set	reorder
	END(splimp)

/*
 * splx(ipl) -- restore previously saved ipl
 *
 * ??? Do we really need to reset the SR here, too?  Who or what is going to
 * ??? turn off those bits between an spl*() and this splx()?  Be safe....
 */
LEAF(splx)
EXPORT(ssplx)
	.set	noreorder
	mfc0	t1,C0_SR
	li	v1,CSR_IVECTMASK
	lw	v0,(v1)			/* current SBC mask */
	sw	a0,(v1)			/* (BDSLOT) enable all */
	ori	t1,SR_IEC		/* enable general interrupts */
	j	ra
	mtc0	t1,C0_SR		/* (BDSLOT) */
EXPORT(esplx)
	.set	reorder
	END(splx)

/*
 * is_spl_basepri( saved_ivectmask ) 
 *	returns nonzero if Vector Mask is all-enabled
 */
LEAF(is_spl_basepri)
	li	v0,1			/* default TRUE */
	li	v1,CSR_IVECTMASK_NONE
	beq	v1,a0,1f		/* all ints enabled? */
	li	v0,0			/* no, return FALSE */
1:	j	ra
	END(is_spl_basepri)

/*
 * setsoftnet() - make software network interrupt request
 */
EXPORT(setsoftnet)
	li	a0, CSR_IVECTSET_SW2
	j	siron

/*
 * acksoftnet() - acknowledge software network interrupt
 */
EXPORT(acksoftnet)
	li	a0, CSR_IVECTSET_SW2
	j	siroff

/*
 * setsoftclock() - make software clock interrupt request
 */
EXPORT(setsoftclock)
	li	a0, CSR_IVECTSET_SW1
	/* falls into siron() */
/*
 * siron(level) -- make software interrupt request
 */
LEAF(siron)
	sw	a0,CSR_IVECTSET(zero)	# turn on SW bit
	j	ra
	END(siron)

/*
 * acksoftclock() - acknowledge software clock interrupt
 */
EXPORT(acksoftclock)
	li	a0, CSR_IVECTSET_SW1
	/* falls into siroff() */
/*
 * siroff(level) -- acknowledge software interrupt request
 */
LEAF(siroff)
#ifdef R6000_BUG_SBC_TIMER
	j	r6000_bug_ivectclr	# use special code to clear interrupt
#endif R6000_BUG_SBC_TIMER
	sw	a0,CSR_IVECTCLR(zero)	# clear SW bit
	j	ra
	END(siroff)

	
#ifdef R6000_BUG_SBC_TIMER

/*
 * r6000_bug_ivectclr -- special routine used to clear SBC interrupts.
 *                       Needed since SBC may clear timer interrupt which
 *                       occurs at same time we do the clear.  This code
 *                       will restore the pending timer interrupt.
 */
 
LEAF(r6000_bug_ivectclr)
	.set	noreorder
	sw	a0,CSR_IVECTCLR(zero)	# clear SBC interrupt bit
	mfc0	t0,C0_SR
	mtc0	zero,C0_SR		/* firmly disable interrupts */
	nop				/* May not need this, but */
	nop				/*   let's make sure ints are OFF */
	lw	v1,CSR_COUNT
	lw	v0,CSR_COMPARE
	nop
	subu	v0,v0,v1		/* v0 = CSR_COMPARE - CSR_COUNT */
	lw	v1,cpu_hz_tick		/* # cpu cyles within one time tick */
	nop
	sltu	v0,v1,v0
	beq	v0,zero,1f
	nop
/*
 * If we are NOT within one timer tick before the compare then the
 * timer interrupt should be set (it may already be set, but resetting it
 * can't cause any problems).
 */
	
	li	v0,CSR_IVECTSET_TIMER
	sw	v0,CSR_IVECTSET

1:	
	j	ra
	mtc0	t0,C0_SR		/* (BDSLOT) restore SR */
	.set	reorder
	
	END(r6000_bug_ivectclr)
#endif R6000_BUG_SBC_TIMER	
