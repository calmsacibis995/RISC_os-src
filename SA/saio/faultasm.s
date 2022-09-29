#ident "$Header: faultasm.s,v 1.18 90/08/21 09:44:18 alexp Exp $"
/* $Copyright$ */
/*	%Q%	%I%	%M%	*/

/*
 * faultasm.s -- standalone io library fault handling code
 */

#include "sys/signal.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/asm.h"
#include "machine/ioa.h"
#include "prom/entrypt.h"
#include "saio/saio.h"
#include "saio/saioctl.h"

#define CPU_IMP_R6300		3
#define IOC_DBE_RETRY_UPB	256


	.text
/*
 * breakpoint -- client fielded a breakpoint it doesn't want to handle
 *
 * kernel (and other clients) should restore all registers except at and k0
 * k0 should be set to "at" at time of breakpoint (the state of k0 can't be
 * saved, unfortunately), then the client should jump here by loading
 * "at" with the address of "breakpoint" and jumping via at
 */
LEAF(breakpoint)
	sw	k0,_at_save
	sw	v0,_v0_save
	li	v0,EXCEPT_BRKPT
	j	exception
	END(breakpoint)

/*
 * exception vector code
 * hook_exceptions() sets-up E_VEC to jump here
 */
LEAF(exceptnorm)
	.set	noat
	move	k0,AT
	.set	at
	sw	k0,_at_save
	sw	v0,_v0_save
	/*
	 *  If this is a DBE from an M/6000 GBA, then perform some number
	 *  of retries before declaring this as a real DBE.
	 */
	.set	noreorder
	mfc0	v0,C0_PRID		# (BDSLOT) what kind of CPU is this?
	li	k0,CPU_IMP_R6300
	srl	v0,8			# right-justify "implementation" field
	bne	v0,k0,4f		# if not R6000, then skip 
	lw	v0,nofault		# (BDSLOT) if expecting a DBE, then
	mfc0	k0,C0_CAUSE		# (LDSLOT)
	bne	v0,zero,3f		#  handle this "nofault" case normally
	li	v0,EXC_DBE		# (BDSLOT)
	andi	k0,CAUSE_EXCMASK
	bne	v0,k0,3f		# only interested in DBE
	mfc0	v0,C0_BADVADDR		# (BDSLOT)
	li	k0,PROM_RESET
	.set	reorder
	sltu	k0,v0,k0		# if BadVaddr above top of CtlSpace
	beq	k0,zero,3f
	li	k0,IOA3
	sltu	k0,v0,k0		# ...or BadVaddr below IOA3
	bne	k0,zero,3f		#   then it's not a DBE for IOC or GBA
	/*  now we know it's a DBE for a GBA access 
	 */
	lw	k0,ioc_retry_last_badvaddr
	beq	k0,v0,1f		# if this is a new badvaddr...
	sw	zero,ioc_dbe_retry_count  # then start a new retry count
	sw	v0,ioc_retry_last_badvaddr # and remember this BadVaddr
1:
	.set	noreorder
	mfc0	v0,C0_EPC		# if this is a new EPC...
	lw	k0,ioc_retry_last_epc
	nop				# (LDSLOT)
	.set	reorder
	beq	k0,v0,2f		# ... then start a new retry count
	sw	zero,ioc_dbe_retry_count
	sw	v0,ioc_retry_last_epc	# and remember this EPC
2:
	lw	v0,ioc_dbe_retry_count	# if now at max retry count...
	li	k0,IOC_DBE_RETRY_UPB	# (LDSLOT)
	beq	v0,k0,3f		# ...then declare a real DBE
	addi	v0,1			# otherwise, increment count
	sw	v0,ioc_dbe_retry_count
	/*
	 *  restore registers VERY CAREFULLY, and retry the access
	 */
	lw	v0,_v0_save
	lw	k0,_at_save
	.set	noat
	move	AT,k0			# now we can't do another "lw"
	.set	noreorder
	.set	at			#  because that uses AT again
	mfc0	k0,C0_EPC		# retry faulting instruction
	nop				# (LDSLOT)
	j	k0			# reexecute the instruction
	c0	C0_RFE			# (BDSLOT)
	.set	reorder
	
3:	sw	zero,ioc_dbe_retry_count
	sw	zero,ioc_retry_last_epc
	sw	zero,ioc_retry_last_badvaddr
4:
#ifndef PROM
	/*
	 * Check if this is a "kernel breakpoint" that is to be handled
	 * by the debug monitor
	 */
	.set	noreorder
	mfc0	v0,C0_CAUSE
	nop
	and	k0,v0,CAUSE_EXCMASK
	bne	k0,+EXC_BREAK,2f	# not even a break inst
	nop
	mfc0	k0,C0_EPC
	.set	reorder
	and	v0,CAUSE_BD
	beq	v0,zero,1f		# not in branch delay slot
	addu	k0,4			# advance to bd slot
1:
	lw	k0,0(k0)		# fetch faulting instruction
	lw	v0,kernel_bp		# bp inst used by dbgmon
	bne	v0,k0,2f		# not dbgmon's break inst

	.set	noat
	lw	AT,+RB_BPADDR		# address of breakpoint handler
	beq	AT,zero,2f		# none declared
	lw	v0,_v0_save		# restore v0
	lw	k0,_at_save		# save AT in k0
	j	AT			# enter breakpoint handler
	.set	at
2:
#endif !PROM
	li	v0,EXCEPT_NORM
	j	exception
	END(exceptnorm)

/*
 * utlb miss
 * hook_exceptions() sets-up UT_VEC to jump here
 */
LEAF(exceptutlb)
	.set	noat
	move	k0,AT
	.set	at
	sw	k0,_at_save
	sw	v0,_v0_save
	li	v0,EXCEPT_UTLB

/*
 * common exception handling code
 */
exception:
	/*
	 * Save various registers so we can print informative messages
	 * for faults (whether on normal stack or fault stack)
	 */
	.set	noreorder
	sw	v0,_exc_save
	mfc0	v0,C0_EPC
	nop
	sw	v0,_epc_save
	mfc0	v0,C0_SR
	nop
	sw	v0,_sr_save
	mfc0	v0,C0_BADVADDR
	nop
	sw	v0,_badvaddr_save
	mfc0	v0,C0_CAUSE
	sw	sp,_sp_save
	sw	v0,_cause_save
	.set	reorder
	lw	sp,_fault_sp		# use "fault" stack
	/*
	 * Only save registers if on regular stack
	 * then change mode to fault mode
	 */
	lw	v0,_stack_mode
	sw	v0,_mode_save
	beq	v0,MODE_FAULT,nosave	# was in fault mode
	li	v0,MODE_FAULT
	sw	v0,_stack_mode		# now in fault mode
	lw	v0,_epc_save
	sw	v0,_regs+R_EPC*4
	lw	v0,_sr_save
	sw	v0,_regs+R_SR*4
	lw	v0,_at_save
	sw	v0,_regs+R_AT*4
	lw	v0,_v0_save
	sw	v0,_regs+R_V0*4
	lw	v0,_exc_save
	sw	v0,_regs+R_EXCTYPE*4
	lw	v0,_badvaddr_save
	sw	v0,_regs+R_BADVADDR*4
	lw	v0,_cause_save
	sw	v0,_regs+R_CAUSE*4
	lw	v0,_sp_save
	sw	v0,_regs+R_SP*4
	sw	zero,_regs+R_ZERO*4	# we don't trust anything
	sw	v1,_regs+R_V1*4
	sw	a0,_regs+R_A0*4
	sw	a1,_regs+R_A1*4
	sw	a2,_regs+R_A2*4
	sw	a3,_regs+R_A3*4
	sw	t0,_regs+R_T0*4
	sw	t1,_regs+R_T1*4
	sw	t2,_regs+R_T2*4
	sw	t3,_regs+R_T3*4
	sw	t4,_regs+R_T4*4
	sw	t5,_regs+R_T5*4
	sw	t6,_regs+R_T6*4
	sw	t7,_regs+R_T7*4
	sw	s0,_regs+R_S0*4
	sw	s1,_regs+R_S1*4
	sw	s2,_regs+R_S2*4
	sw	s3,_regs+R_S3*4
	sw	s4,_regs+R_S4*4
	sw	s5,_regs+R_S5*4
	sw	s6,_regs+R_S6*4
	sw	s7,_regs+R_S7*4
	sw	t8,_regs+R_T8*4
	sw	t9,_regs+R_T9*4
	li	k0,0xbad00bad		# make it obvious we can't save this
	sw	k0,_regs+R_K0*4
	sw	k1,_regs+R_K1*4
	sw	fp,_regs+R_FP*4
	sw	gp,_regs+R_GP*4
	sw	ra,_regs+R_RA*4
	mflo	v0
	sw	v0,_regs+R_MDLO*4
	mfhi	v0
	sw	v0,_regs+R_MDHI*4
	.set	noreorder
	mfc0	v0,C0_PRID		# what kind of CPU is this?
	li	v1,CPU_IMP_R6300
	srl	v0,8			# right-justify "implementation" field
	beq	v0,v1,1f		# if R6000, then skip some CP0 regs
	nop
	mfc0	v0,C0_INX
	mfc0	v1,C0_RAND		# save just to see it change
	sw	v0,_regs+R_INX*4
	sw	v1,_regs+R_RAND*4
	mfc0	v0,C0_TLBLO
	mfc0	v1,C0_TLBHI
	sw	v0,_regs+R_TLBLO*4
	sw	v1,_regs+R_TLBHI*4
	mfc0	v0,C0_CTXT
	b	2f
	sw	v0,_regs+R_CTXT*4	# (BDSLOT)
1:
	mfc0	v0,C0_PID
	mfc0	v1,C0_ERROR
	sw	v0,_regs+R_PID*4
	sw	v1,_error_save
	.set	reorder
2:
	lw	v0,_sr_save
	and	v1,v0,SR_CU1
	beq	v1,zero,nosave
	/*
	 *  The SR Cp1 Useable bit is on, so we tentatively assume that
	 *  Cp1 exists and we can access it.  This might be a bad assumption.
	 *  Use a local flag to remember that we're about to access Cp1.  If
	 *  the flag is on, then we got an exception while accessing Cp1, and
	 *  we don't access it again!
	 */
	lw	v1,_cp1_access		# if we got an exception while trying
	beq	v1,zero,3f		#  to access Cp1, then don't do again!

	lw	v0,_regs+R_SR*4		# bad news!  recursive exception!
	and	v0,~SR_CU1		# turn off SR_CU1 in the savearea
	sw	v0,_regs+R_SR*4		#  so _resume() won't touch Cp1 again
	sw	zero,_cp1_access	# and prepare for the next exception
	b	nosave
3:
	.set	noreorder
	li	v1,1			# remember that we're about to
	sw	v1,_cp1_access		#  muck with Cp1
	cfc1	v0,$0			# can we access Cp1 [Revision Id] ?
	swc1	$f0,_regs+R_F0*4
	.set	reorder
	swc1	$f1,_regs+R_F1*4
	swc1	$f2,_regs+R_F2*4
	swc1	$f3,_regs+R_F3*4
	swc1	$f4,_regs+R_F4*4
	swc1	$f5,_regs+R_F5*4
	swc1	$f6,_regs+R_F6*4
	swc1	$f7,_regs+R_F7*4
	swc1	$f8,_regs+R_F8*4
	swc1	$f9,_regs+R_F9*4
	swc1	$f10,_regs+R_F10*4
	swc1	$f11,_regs+R_F11*4
	swc1	$f12,_regs+R_F12*4
	swc1	$f13,_regs+R_F13*4
	swc1	$f14,_regs+R_F14*4
	swc1	$f15,_regs+R_F15*4
	swc1	$f16,_regs+R_F16*4
	swc1	$f17,_regs+R_F17*4
	swc1	$f18,_regs+R_F18*4
	swc1	$f19,_regs+R_F19*4
	swc1	$f20,_regs+R_F20*4
	swc1	$f21,_regs+R_F21*4
	swc1	$f22,_regs+R_F22*4
	swc1	$f23,_regs+R_F23*4
	swc1	$f24,_regs+R_F24*4
	swc1	$f25,_regs+R_F25*4
	swc1	$f26,_regs+R_F26*4
	swc1	$f27,_regs+R_F27*4
	swc1	$f28,_regs+R_F28*4
	swc1	$f29,_regs+R_F29*4
	swc1	$f30,_regs+R_F30*4
	swc1	$f31,_regs+R_F31*4
	.set	noreorder
	cfc1	v0,$30
	cfc1	v1,$31
	sw	v0,_regs+R_C1_EIR*4
	sw	v1,_regs+R_C1_SR*4
	sw	zero,_cp1_access	# turn off flag
	.set	reorder
nosave:
	j	_exception_handler
	END(exceptutlb)

/*
 * _resume -- resume execution of mainline code
 */
LEAF(_resume)
	lw	v0,_regs+R_SR*4
	and	v0,SR_CU1
	beq	v0,zero,1f	
	.set	noreorder
	mtc0	v0,C0_SR
	nop
	nop					/* need TWO cycles here */
	.set	reorder
	lwc1	$f0,_regs+R_F0*4
	lwc1	$f1,_regs+R_F1*4
	lwc1	$f2,_regs+R_F2*4
	lwc1	$f3,_regs+R_F3*4
	lwc1	$f4,_regs+R_F4*4
	lwc1	$f5,_regs+R_F5*4
	lwc1	$f6,_regs+R_F6*4
	lwc1	$f7,_regs+R_F7*4
	lwc1	$f8,_regs+R_F8*4
	lwc1	$f9,_regs+R_F9*4
	lwc1	$f10,_regs+R_F10*4
	lwc1	$f11,_regs+R_F11*4
	lwc1	$f12,_regs+R_F12*4
	lwc1	$f13,_regs+R_F13*4
	lwc1	$f14,_regs+R_F14*4
	lwc1	$f15,_regs+R_F15*4
	lwc1	$f16,_regs+R_F16*4
	lwc1	$f17,_regs+R_F17*4
	lwc1	$f18,_regs+R_F18*4
	lwc1	$f19,_regs+R_F19*4
	lwc1	$f20,_regs+R_F20*4
	lwc1	$f21,_regs+R_F21*4
	lwc1	$f22,_regs+R_F22*4
	lwc1	$f23,_regs+R_F23*4
	lwc1	$f24,_regs+R_F24*4
	lwc1	$f25,_regs+R_F25*4
	lwc1	$f26,_regs+R_F26*4
	lwc1	$f27,_regs+R_F27*4
	lwc1	$f28,_regs+R_F28*4
	lwc1	$f29,_regs+R_F29*4
	lwc1	$f30,_regs+R_F30*4
	lwc1	$f31,_regs+R_F31*4
	.set	noreorder
	lw	v0,_regs+R_C1_EIR*4
	lw	v1,_regs+R_C1_SR*4
	ctc1	v0,$30
	ctc1	v1,$31
	.set	reorder
1:
	lw	a0,_regs+R_A0*4
	lw	a1,_regs+R_A1*4
	lw	a2,_regs+R_A2*4
	lw	a3,_regs+R_A3*4
	lw	t0,_regs+R_T0*4
	lw	t1,_regs+R_T1*4
	lw	t2,_regs+R_T2*4
	lw	t3,_regs+R_T3*4
	lw	t4,_regs+R_T4*4
	lw	t5,_regs+R_T5*4
	lw	t6,_regs+R_T6*4
	lw	t7,_regs+R_T7*4
	lw	s0,_regs+R_S0*4
	lw	s1,_regs+R_S1*4
	lw	s2,_regs+R_S2*4
	lw	s3,_regs+R_S3*4
	lw	s4,_regs+R_S4*4
	lw	s5,_regs+R_S5*4
	lw	s6,_regs+R_S6*4
	lw	s7,_regs+R_S7*4
	lw	t8,_regs+R_T8*4
	lw	t9,_regs+R_T9*4
	lw	k1,_regs+R_K1*4
	lw	gp,_regs+R_GP*4
	lw	fp,_regs+R_FP*4
	lw	ra,_regs+R_RA*4
	lw	v0,_regs+R_MDLO*4
	mtlo	v0
	lw	v1,_regs+R_MDHI*4
	mthi	v1
	.set	noreorder
	mfc0	v1,C0_PRID		# what kind of CPU is this?
	li	v0,CPU_IMP_R6300
	srl	v1,8			# right-justify "implementation" field
	beq	v1,v0,6f		# if R6000, then skip some CP0 regs
	nop
	lw	v0,_regs+R_INX*4
	lw	v1,_regs+R_TLBLO*4
	mtc0	v0,C0_INX
	mtc0	v1,C0_TLBLO
	lw	v0,_regs+R_TLBHI*4
	lw	v1,_regs+R_CTXT*4
	mtc0	v0,C0_TLBHI
	b	2f
	mtc0	v1,C0_CTXT		# (BDSLOT)

1:
	lw	v1,_regs+R_PID*4
	nop
	mtc0	v1,C0_PID
	b	2f
/*
 * R6000 is special due to bugs in mtc0 xx,C0_pid instruction.  The workaround
 * depends upon whether the code is being executed cached or non-cached.
*/
6:
	lw	v1,_regs+R_PID*4	# load correct value for PID
	nop

	jal	6f			# determine if cached/non-cached
	li	v0,0x0a
6:	srl	ra,ra,28
	beq	ra,v0,7f		# branch to non-cache case (0xaxxxxxxx)
	li	v0,0x0b
	beq	ra,v0,7f		# branch to non-cache case (0xbxxxxxxx)
	nop



/*
 * cached execution
 *
 *      There is a bug in the R6020 SBC which necessitated rework on the
 *      CPU board, the net effect of which produces a problem with
 *              mtc0  Rx,C0_PID
 *      under obscure circumstances, leaving a correct PID value in the
 *      CPU onchip PID register and an incorrect PID value in the offchip
 *      register.  For instance:  if this instruction lies in the last word of
 *      an S-cache line, and the next line gets a shared hit.
 *      The simplest workaround is to issue two such instructions back-to-back.
 *      Another glitch:  because of the PID mismatch, an external parity error
 *      might be declared, so we disable these MachineChecks around the mtc0.
 *      Because interrupts are disabled, we can use $k0 and $k1.
 *      Another glitch:  the back-to-pack write-PID sequence doesn't completely
 *      fix the problem in CPU rev3.1, as it might leave the 2nd mtc0's S-cache
 *      tag with bad parity.  We correct for this by invalidating this
 *      instruction's address, forcing an I-cache miss and a shared-hit in
 *      the S-cache, which rewrites that tag.
 */
 	mfc0    k0,C0_ERROR
	li      k1,C0_ERROR_IMASK
	mtc0    k1,C0_ERROR
        nop
	li	k1,SR_MM_MODE		# need MM_MODE set to do an inval
	mtc0	k1,C0_SR		# correct C0_SR restored below
	mtc0    v1,C0_PID
6:      mtc0    v1,C0_PID
        la      k1,6b
        inval   (k1)
        or      k0,C0_ERROR_EXT
        mtc0    k0,C0_ERROR

	b 	8f			# continue
/*
 * non-cached execution -- "mtc0 v1,C0_PID" works OK in branch delay
 *			  slot if executed non-cached (problem in CPU 3.1)
 */

7:	j    	8f
	mtc0	v1,C0_PID
	nop
8:	lw	ra,_regs+R_RA*4		# need to restore the ra

/*
 * Common code for all platforms restarts here.
 */

2:
	lw	v0,_regs+R_CAUSE*4
	lw	v1,_regs+R_SR*4
	mtc0	v0,C0_CAUSE		# for software interrupts
	and	v1,~(SR_KUC|SR_IEC)	# not ready for these yet!
	mtc0	v1,C0_SR
	lw	sp,_regs+R_SP*4
	.set	reorder
	lw	v0,_regs+R_V0*4
	lw	v1,_regs+R_V1*4
	li	k0,MODE_NORMAL
	sw	k0,_stack_mode		# returning to normal stack
	lw	k0,_regs+R_EPC*4
	.set	noat
	.set	noreorder
	lw	AT,_regs+R_AT*4
	j	k0
	rfe
	.set	reorder
	.set	at
	END(_resume)

/*
 * the following jumps are copied by hook_exceptions to locations E_VEC
 * and UT_VEC
 *
 * NOTE: these must be jump register since they change 256MB text pages
 */
	.set	noreorder
	.set	noat		# must be set so la doesn't use at

LEAF(_j_exceptnorm)
	la	k0,exceptnorm
	j	k0
	nop
	END(_j_exceptnorm)

LEAF(_j_exceptutlb)
	la	k0,exceptutlb
	j	k0
	nop
	END(_j_exceptutlb)

	.set	at
	.set	reorder

LEAF(exec_brkpt)
	break	BRK_KERNELBP		# drop into debug monitor
	j	ra
	END(exec_brkpt)

kernel_bp:
	break	BRK_KERNELBP

/*
 *  reset_ioc_retry_count
 *	For the M/6000, in cases where we retry accesses to an IOC which
 *	generate a DBE, we sometimes fail to recognize that a DBE is a new
 *	access, not one of the retries.  In such cases the kernel may report
 *	an erroneous DBE panic.  If we can recognize appropriate spots in
 *	drivers where such DBEs are seen (e.g., in a common routine which
 *	polls a VME board), then we should call this routine after such an
 *	access, and the next DBE will start the retry sequence.
 */
LEAF(reset_ioc_retry_count)
	sw	zero,ioc_dbe_retry_count
	sw	zero,ioc_retry_last_epc
	sw	zero,ioc_retry_last_badvaddr
	j	ra
	END(reset_ioc_retry_count)


/*
 * exception state
 */
	BSS(_epc_save,4)		# epc at time of exception
	BSS(_at_save,4)			# at at time of exception
	BSS(_v0_save,4)			# v0 at time of exception
	BSS(_exc_save,4)		# exc at time of exception
	BSS(_badvaddr_save,4)		# badvaddr at time of exception 
	BSS(_cause_save,4)		# cause reg at time of exception
	BSS(_error_save,4)		# error reg at time of exception
	BSS(_sp_save,4)			# sp at time of exception
	BSS(_mode_save,4)		# prom mode at time of exception
	BSS(_sr_save,4)			# sr at time of exceptions
	BSS(_fault_sp,4)		# stack for nofault handling
	BSS(_stack_mode,4)		# running regular or fault stack
	BSS(_fp_test_on,4)		# flag for fpu exception handling
	BSS(_cp1_access,4)		# flag for cp1 exception handling
	BSS(Lstore,4)
	BSS(ioc_dbe_retry_count,4)	# for retrying IOC DBE
	BSS(ioc_retry_last_epc,4)	#       ditto
	BSS(ioc_retry_last_badvaddr,4)	#       ditto
	BSS(_regs,NREGS*4)		# client register contents
