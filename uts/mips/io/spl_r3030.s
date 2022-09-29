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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: spl_r3030.s,v 1.1.1.3.1.1.1.2 90/11/02 17:56:37 beacker Exp $ */

/*
 * Interrupts for pizzaz
 *
 * These routines are use to enable and disable the interrupts.
 * Since on the R3000 all interrupts have the same priority
 * we can select which to do first. We can also mask out
 * specific interrupts and so ignore them; this allows us to
 * implement polling for specific devices when we wish.
 */

#include	"../ml/assym.s"

/*
 * DOINTDEBUG does a call/resume analysis of the interrupt levels.
 * each time an interrupt is disable or enabled. If the time is too long then
 * we print out the caller/returner's ra's.
 */
#ifdef	DOINTDEBUG
				.globl	intdebug
#define	INTDEBUG(old, new) add	sp,-68; \
			sw	a0,12(sp); \
			sw	a1,16(sp); \
			sw	a2,20(sp); \
			sw	a3,24(sp); \
			sw	t0,28(sp); \
			sw	t1,32(sp); \
			sw	t2,36(sp); \
			sw	t3,40(sp); \
			sw	t4,44(sp); \
			sw	t5,48(sp); \
			sw	t6,52(sp); \
			sw	t7,56(sp); \
			sw	t8,60(sp); \
			sw	t9,64(sp); \
			sw	old,4(sp); \
			sw	new,0(sp); \
			sw	ra,8(sp); \
			lw	a0,4(sp); \
			lw	a1,0(sp); \
			lw	a2,8(sp); \
			jal	intdebug; \
			add	sp,-20; \
			add	sp,20; \
			lw	new,0(sp); \
			lw	old,4(sp); \
			lw	ra,8(sp); \
			lw	a0,12(sp); \
			lw	a1,16(sp); \
			lw	a2,20(sp); \
			lw	a3,24(sp); \
			lw	t0,28(sp); \
			lw	t1,32(sp); \
			lw	t2,36(sp); \
			lw	t3,40(sp); \
			lw	t4,44(sp); \
			lw	t5,48(sp); \
			lw	t6,52(sp); \
			lw	t7,56(sp); \
			lw	t8,60(sp); \
			lw	t9,64(sp); \
			add	sp,68
#else
#define	INTDEBUG(old, new)
#endif

/*
 * each of the INT_LEVEL defines defines the bit for that interrupt level.
 * to calculate it we do the xor of the level and the level one higher.
 * which gives the bits that change.
 */

#define	INT_LEVEL1	(SR_IMASK0 ^ SR_IMASK1)
#define	INT_LEVEL2	(SR_IMASK1 ^ SR_IMASK2)
#define	INT_LEVEL3	(SR_IMASK2 ^ SR_IMASK3)
#define	INT_LEVEL4	(SR_IMASK3 ^ SR_IMASK4)
#define	INT_LEVEL5	(SR_IMASK4 ^ SR_IMASK5)
#define	INT_LEVEL6	(SR_IMASK5 ^ SR_IMASK6)
#define	INT_LEVEL7	(SR_IMASK6 ^ SR_IMASK7)
#define	INT_LEVEL8	(SR_IMASK7 ^ SR_IMASK8)

/*
 * Interrupt levels ---
 *
 *		Priority (8 is highest, 1 lowest)???
 *
 * Hardware
 * (vid)	8 - parity error (DMA channels 1 and 2)
 *		    Lance parity error
 *		    Video interrupt
 *	
 * (floppy)	7 - Floppy disk
 *	
 * (fpu)	6 - FPU exception
 *	
 * (clock)	5 - Timer (RAMBO timer)
 *	
 * (scsi)	4 - DMA channel 1 (Scsi transfer completed).
 *		    SCSI
 *	
 * (tty)	3 - Ethernet
 *		    SCC duart (serial lines)
 *		    Keyboard
 *		    Slot 0, AT bus.
 *	
 * Software
 * (snet)	2 - Soft network interrupt
 *	
 * (sclock)	1 - Soft clock interrupt
 *
 *
 * Use spl<name> or spl<number> to set the priority level
 * to that of the requested device, this also blocks signals
 * from equal level devices.
 *
 * In preference though, use iil<name> or iil<number> to ignore
 * an interrupt from a particular level, allowing interrupts from
 * other levels.
 * To renenable the interrupts, use illx(returned value from ill?())
 * or ail<name>/ail<number> to enable an interrupt of that level.
 */

/*
 * functions defined;
 *
 * typedef long intmask;
 *		32 bits, the C0 STATUS register
 *
 * intmask spl0()
 * intmask splnone()
 *		allow all interrupts
 *
 * intmask spl1()
 * intmask splsclock()
 *		allow all except soft clock interrupts (software #1)
 *
 * intmask spl2()
 * intmask splsnet()
 *		ignore all soft interrupts
 *
 * intmask spl3()
 * intmask spltty()
 *		ignore also terminal I/O interrupts, the ethernet
 *		controller and RAT interrupt (The digi board?)
 *
 * intmask spl4()
 * intmask splscsi()
 *		ignore also scsi interrupts, both Rambo and NEC
 *
 * intmask spl5()
 * intmask splclock()
 *		ignore also interrupts from the RAMBO timer
 *
 * intmask spl6()
 * intmask splfpu()
 *		ignore also FPU interrupts
 *
 * intmask spl7()
 * intmask splfloppy()
 *		ignore also Floppy interrupts
 *
 * intmask spl8()
 * intmask splhi()
 * intmask splvid()
 * intmask splhigh()
 *		ignore all interrupts
 *
 * void splx(oldmask)
 * intmask oldmask;
 *		restore interrupt enable/disable flags
 *
 * void iil1()
 * void iilsclock()
 *		ignore except soft clock interrupts
 *
 * void iil2()
 * void iilsnet()
 *		ignore network soft interrupts
 *
 * void iil3()
 * void iiltty()
 *		ignore terminal I/O interrupts, the ethernet
 *		controller and RAT interrupt (The digi board?)
 *
 * void iil4()
 * void iilscsi()
 *		ignore scsi interrupts, both Rambo and NEC
 *
 * void iil5()
 * void iilclock()
 *		ignore interrupts from the RAMBO timer
 *
 * void iil6()
 * void iilfpu()
 *		ignore FPU interrupts
 *
 * void iil7()
 * void iilfloppy()
 *		ignore Floppy interrupts
 *
 * void iil8()
 * void iilvid()
 *		ignore video and parity interrupts
 *
 *
 * void ail1()
 * void ailsclock()
 *		allow except soft clock interrupts
 *
 * void ail2()
 * void ailsnet()
 *		allow network soft interrupts
 *
 * void ail3()
 * void ailtty()
 *		allow terminal I/O interrupts, the ethernet
 *		controller and RAT interrupt (The digi board?)
 *
 * void ail4()
 * void ailscsi()
 *		allow scsi interrupts, both Rambo and NEC
 *
 * void ail5()
 * void ailclock()
 *		allow interrupts from the RAMBO timer
 *
 * void ail6()
 * void ailfpu()
 *		allow FPU interrupts
 *
 * void ail7()
 * void ailfloppy()
 *		allow Floppy interrupts
 *
 * void ail8()
 * void ailvid()
 *		allow video and parity interrupts
 */
	.globl	ipl_special_mask

LEAF(splnone)
XLEAF(spl0)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	a0,ipl_special_mask
	li	v1,SR_IEC|SR_IMASK0
	and	v1,a0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splnone)

LEAF(splsclock)
XLEAF(spl1)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK1
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splsclock)

LEAF(splsnet)
XLEAF(splnet)
XLEAF(spl2)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK2
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splsnet)

LEAF(spltty)
XLEAF(splimp)
XLEAF(spl3)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK3
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(spltty)

LEAF(splscsi)
XLEAF(splbio)
XLEAF(spl4)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK4
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splscsi)

LEAF(splclock)
XLEAF(spl5)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK5
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splclock)

LEAF(splfpu)
XLEAF(spl6)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK6
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splfpu)

LEAF(splfloppy)
XLEAF(spl7)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK7
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splfloppy)

LEAF(splhi)
XLEAF(splvid)
XLEAF(splall)
XLEAF(spl8)
XLEAF(splhigh)
	.set	noreorder
	mfc0	v0,C0_SR
	li	v1,SR_IEC|SR_IMASK8
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(splhi)

/*
 * splx(ipl) -- restore previously saved ipl
 */
LEAF(splx)
XLEAF(iilx)
XLEAF(ailx)
	.set	noreorder
	lw	v1,ipl_special_mask
	mfc0	v0,C0_SR
	and	v1,a0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR
	.set	reorder
	END(splx)

/*
 * ignore interrupt level - returns the old value of the
 * interrupt mask
 */
LEAF(iilsclock)
XLEAF(iil1)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL1
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilsclock)

LEAF(iilsnet)
XLEAF(iil2)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL2
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilsnet)

LEAF(iiltty)
XLEAF(iil3)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL3
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iiltty)

LEAF(iilscsi)
XLEAF(iil4)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL4
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilscsi)

LEAF(iilclock)
XLEAF(iil5)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL5
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilclock)

LEAF(iilfpu)
XLEAF(iil6)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL6
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilfpu)

LEAF(iilfloppy)
XLEAF(iil7)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL7
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilfloppy)

LEAF(iilvid)
XLEAF(iil8)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,~INT_LEVEL8
	and	t1,v1
	sw	t1,ipl_special_mask
	and	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(iilhi)
/*
 * allow interrupt levels.
 * This just turns on the bit. ail's can be
 * overridden by spls and iils.
 */
LEAF(ailsclock)
XLEAF(ail1)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL1
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailsclock)

LEAF(ailsnet)
XLEAF(ail2)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL2
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailsnet)

LEAF(ailtty)
XLEAF(ail3)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL3
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailtty)

LEAF(ailscsi)
XLEAF(ail4)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL4
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailscsi)

LEAF(ailclock)
XLEAF(ail5)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL5
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailclock)

LEAF(ailfpu)
XLEAF(ail6)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL6
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailfpu)

LEAF(ailfloppy)
XLEAF(ail7)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL7
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailfloppy)

LEAF(ailvid)
XLEAF(ail8)
	.set	noreorder
	mfc0	v0,C0_SR
	lw	t1,ipl_special_mask
	li	v1,INT_LEVEL8
	or	t1,v1
	sw	t1,ipl_special_mask
	or	v1,v0
	INTDEBUG(v0, v1)
	j	ra
	mtc0	v1,C0_SR	# BDSLOT
	.set	reorder
	END(ailhi)

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
	mtc0	v0,C0_SR
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
/*
 * rambodelay - this routine will convert a delay from microseconds
 * into rambo ticks. For the Pizazz this conversion is equivalent to
 * a multiplication by 6.25. We will do this in integers to save time
 * since we ignore the fraction part anyway.
 */
LEAF(rambodelay)
	.set	noreorder
	addu	v0,a0,a0	# v0 = 2 * delay
	addu	v0,v0,a0	# v0 = 3 * delay
	addu	v0,v0,v0	# v0 = 6 * delay
	srl	a0,a0,2		# a0 = .25 * delay
	j	ra
	addu	v0,v0,a0	# v0 = 6.25 * delay	(delay slot)
	.set	reorder
	END(rambodelay)
