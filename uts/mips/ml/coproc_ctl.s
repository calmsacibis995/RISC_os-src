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
/* $Header: coproc_ctl.s,v 1.7.1.5.1.3 90/07/17 13:00:24 hawkes Exp $ */

/*
 * This file contains routines to get and set the coprocessor point control
 * registers.
 */

#ifdef notdef
#include "../mips/regdef.h"
#include "../mips/fpu.h"
#include "../mips/cpu.h"
#include "../mips/asm.h"
#endif notdef

/*
 * get_fpc_csr returns the fpc_csr.
 */
LEAF(get_fpc_csr)
	.set	noreorder
	mfc0	v1,C0_SR
	li	a0,SR_CU1
	mtc0	a0,C0_SR
	nop
	nop				# before we can really use cp1
	cfc1	v0,fpc_csr
	nop
	nop
	mtc0	v1,C0_SR
	.set	reorder
	j	ra
	END(get_fpc_csr)

/*
 * set_fpc_csr sets the fpc_csr and returns the old fpc_csr.
 */
LEAF(set_fpc_csr)
	.set	noreorder
	mfc0	v1,C0_SR
	li	a1,SR_CU1
	mtc0	a1,C0_SR
	nop				# before we can really use cp1
	nop
	cfc1	v0,fpc_csr
	ctc1	a0,fpc_csr
	mtc0	v1,C0_SR
	.set	reorder
	j	ra
	END(set_fpc_csr)

/*
 * get_fpc_eir returns the fpc_eir.
 */
LEAF(get_fpc_eir)
	.set	noreorder
	mfc0	v1,C0_SR
	li	a0,SR_CU1
	mtc0	a0,C0_SR
	nop				# before we can really use cp1
	nop				# remove when compiled with 0.24
	cfc1	v0,fpc_eir
	mtc0	v1,C0_SR
	.set	reorder
	j	ra
	END(get_fpc_eir)

/*
 * get_fpc_parity returns the fpc_parity.
 */
LEAF(get_fpc_parity)
	.set	noreorder
	mfc0	v1,C0_SR
	li	a0,SR_CU1
	mtc0	a0,C0_SR
	nop				# before we can really use cp1
	nop				# takes two cycles
	cfc1	v0,fpc_parity
	mtc0	v1,C0_SR
	.set	reorder
	j	ra
	END(get_fpc_parity)

/*
 * set_fpc_parity resets the fpc_parity.
 */
LEAF(set_fpc_parity)
	.set	noreorder
	mfc0	v1,C0_SR
	li	a1,SR_CU1
	mtc0	a1,C0_SR
	nop				# before we can really use cp1
	nop				# takes two cycles
	ctc1	a0,fpc_parity
	nop				# don't turn off Cp1 right away...
	mtc0	v1,C0_SR
	.set	reorder
	j	ra
	END(set_fpc_parity)

/*
 * get_cpu_irr -- returns cpu revision id word
 * returns zero for cpu chips without a PRID register
 */
LEAF(get_cpu_irr)
	.set	noreorder
	mfc0	a1,C0_SR		# save sr
	nop
	nop
	mtc0	zero,C0_SR		# interrupts off
	move	v0,zero			# return zero for chips w/o PRID reg
	li	a0,NF_REVID		# set-up nofault handling
	sw	a0,nofault
	li	a0,SR_BEV		# chips w/o PRID don't have BEV
	mtc0	a0,C0_SR
	nop
	nop
	mfc0	a0,C0_SR
	nop
	nop
	and	a0,SR_BEV
	beq	a0,zero,1f		# no BEV, so no PRID
	nop
	mfc0	v0,C0_PRID
1:
	sw	zero,nofault
	mtc0	a1,C0_SR
	j	ra
	nop
	.set	reorder
	END(get_cpu_irr)

/*
 * get_fpc_irr -- returns fp chip revision id
 * NOTE: should not be called if no fp chip is present
 */
LEAF(get_fpc_irr)
	.set	noreorder
	mfc0	a1,C0_SR		# save sr
	mtc0	zero,C0_SR		#  and disable interrupts
	lw	a2,machine_type
	li	v0,SR_CU1
	mtc0	v0,C0_SR		# set fp usable
	li	a0,NF_REVID
	bne	a2,BRDTYPE_R6300,1f	# if this is R6000...
	sw	a0,nofault		# (BDSLOT) prepare for nofault exception
	mfc0	v1,C0_ERROR		# ...then save Cp0 ERROR register
	li	v0,C0_ERROR_IMASK | C0_ERROR_MASK
	mtc0	v0,C0_ERROR		# disable Machine Check exceptions
1:
	cfc1	v0,fpc_irr		# get revision id
	bne	a2,BRDTYPE_R6300,1f	# if this is R6000...
	sw	zero,nofault		# (BDSLOT)
	mfc0	a0,C0_ERROR		# ...then check if parity error
	or	v1,C0_ERROR_MASK	# restore Error Reg
	mtc0	v1,C0_ERROR		#  and clear any errors
	and	a0,C0_ERROR_MASK
	bne	a0,zero,1f		# if Cp1 access caused a parity error
	move	v0,zero			# (BDSLOT) then pretend we read a zero

	ctc1	zero,fpc_parity		# clear FP parity errs
	nop
	mtc1	zero,$f0		# clear FP data registers
	mtc1	zero,$f1		#  in case we didn't run PONs
	mtc1	zero,$f2
	mtc1	zero,$f3
	mtc1	zero,$f4
	mtc1	zero,$f5
	mtc1	zero,$f6
	mtc1	zero,$f7
	mtc1	zero,$f8
	mtc1	zero,$f9
	mtc1	zero,$f10
	mtc1	zero,$f11
	mtc1	zero,$f12
	mtc1	zero,$f13
	mtc1	zero,$f14
	mtc1	zero,$f15
	mtc1	zero,$f16
	mtc1	zero,$f17
	mtc1	zero,$f18
	mtc1	zero,$f19
	mtc1	zero,$f20
	mtc1	zero,$f21
	mtc1	zero,$f22
	mtc1	zero,$f23
	mtc1	zero,$f24
	mtc1	zero,$f25
	mtc1	zero,$f26
	mtc1	zero,$f27
	mtc1	zero,$f28
	mtc1	zero,$f29
	mtc1	zero,$f30
	mtc1	zero,$f31
	cfc1	v0,fpc_irr		# get revision id again, to return
	ctc1	zero,fpc_parity		# clear FP parity errs, enable detect
1:
	mtc0	a1,C0_SR
	j	ra
	nop
	.set	reorder
	END(get_fpc_irr)

/*
 * set_fpc_led -- set floating point board leds
 */
LEAF(set_fpc_led)
#ifndef R6000
	.set	noreorder
	mfc0	v1,C0_SR
	li	a1,SR_CU1
	mtc0	a1,C0_SR
	nop				# before we can really use cp1
	nop
	ctc1	a0,fpc_led		# set leds
	mtc0	v1,C0_SR
	.set	reorder
#endif !R6000
	j	ra
	END(set_fpc_led)

LEAF(reviderror)
	.set	noreorder
	mtc0	a1,C0_SR		# restore sr
	move	v0,zero
	.set	reorder
	j	ra
	END(reviderror)
