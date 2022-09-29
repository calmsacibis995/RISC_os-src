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
/* $Header: fp_intr.s,v 1.10.1.4.1.1.1.2 90/10/16 09:58:17 beacker Exp $ */
/*
 * fp_intr.s -- floating pointer interrupt handler
 */

/*
 * Register setup before calling an interrupt handler (from intr() in 
 * mips/trap.c through the table of routines c0vec_tbl[]):
 *	a0 -- exception frame pointer
 *
 * This routine is called from a 'C' routine thus the registers are
 * handled accordingly.
 *
 * To get to here there must be some form of floating-point coprocessor
 * hardware to generate the interrupt.
 *
 * The pointer to the proc structure which currently owns the floating point
 * unit is in fpowner.  And the current values of the floating-point registers
 * are in the floating-point registers in the coprocessor.
 *
 * To exit this routine any modified value of a floating-point register
 * is just left in that register and then a return to the caller is done.
 */

/*
 * Floating-point coprocessor interrupt handler
 */
#define	FRAME_SIZE	24
#define	LOCAL_SIZE	8
#define	A0_OFFSET	FRAME_SIZE+4*0
#define	A1_OFFSET	FRAME_SIZE+4*1
#define	A2_OFFSET	FRAME_SIZE+4*2
#define	A3_OFFSET	FRAME_SIZE+4*3
#define	RA_OFFSET	FRAME_SIZE-LOCAL_SIZE+4*0
#define	V0_OFFSET	FRAME_SIZE-LOCAL_SIZE+4*1

NESTED(fp_intr, FRAME_SIZE, ra)
	subu	sp,FRAME_SIZE
	sw	ra,RA_OFFSET(sp)
	sw	a0,A0_OFFSET(sp)	# save exception frame pointer
	.mask	0x80000002, -LOCAL_SIZE

	lw	a3,fpowner		# current coproc 1 (fp) owner
	beq	a3,zero,strayfp_intr	# coproc 1 not currently owned

	/*
	 * If the p_fp is P_FP_SIGINTR1 then a SIGFPE is generated on every
	 * floating-point interrupt before each instruction and is then set to
	 * P_FP_SIGINTR2.  If p_fp is P_FP_SIGINTR2 then no SIGFPE is generated
	 * but p_fp is set to P_FP_SIGINTR1.
	 */
	lw	a1,P_FP(a3)
	beq	a1,zero,2f
	bne	a1,P_FP_SIGINTR1,1f
	# We are in state 1, so change to state2 and generate a SIGFPE
	li	a1,P_FP_SIGINTR2
	sw	a1,P_FP(a3)
	b	12f
1:	# We are in state 2, so change to state1 and don't generate a SIGFPE
 	li	a1,P_FP_SIGINTR1
	sw	a1,P_FP(a3)
2:

	.set 	noreorder
	mfc0	a1,C0_SR	# enable coproc 1 for the kernel
	nop
	or	a1,SR_CU1		
	mtc0	a1,C0_SR
	.set	reorder

	lw	a2,fptype_word		# delay slot
	lw	a3,EF_EPC*4(a0)		# load the epc into a3

	# Check the fp implementation so to know where to get the instruction
	# and then load it into register a1 where softfp() expects it.
	bne	a2,IRR_IMP_R2360,2f

	# For board implementations the instruction is in the fpc_eir
	# floating-point control register.
	sw	a3,V0_OFFSET(sp)	# save the resulting pc (board case)

#ifdef XPRBUG
	lw	a0,xpr_flags
	and	a0,XPR_FPINTR
	beq	a0,zero,1f
	subu	sp,10*4
	la	a0,9f
	.set	noreorder
	move	a1,a3
	cfc1	a2,fpc_eir
	cfc1	a3,fpc_csr
	.set	reorder
	jal	xprintf
	nop
	MSG("epc = 0x%x eir = 0x%x csr = 0x%x\n")
	addu	sp,10*4
	lw	a0,A0_OFFSET(sp)
	lw	a2,fptype_word
1:
#endif XPRBUG

	.set	noreorder
	cfc1	a1,fpc_eir
	b	4f
	nop
	.set	reorder
2:
	# For chip implementations the floating-point instruction that caused
	# the interrupt is at the address of the epc as modified by the branch
	# delay bit in the cause register.

	lw	v0,EF_CAUSE*4(a0)	# load the cause register into v0
	lw	a1,0(a3)		# load the instr at the epc into a1
	bltz	v0,3f			# check the branch delay bit
	
	# This is not in a branch delay slot (branch delay bit not set) so
	# calculate the resulting pc (epc+4) into v0 and continue to softfp().
	addu	v0,a3,4
	sw	v0,V0_OFFSET(sp)	# save the resulting pc
	b	4f
3:
	# This is in a branch delay slot so the branch will have to be emulated
	# to get the resulting pc (done by calling emulate_branch() ).
	# The arguments to emulate_branch are:
	#	a0 -- ef (exception frame)
	#	a1 -- the branch instruction
	#	a2 -- the floating-point control and status register
	#
	.set	noreorder
	cfc1	a2,fpc_csr		# get value of fpc_csr
	jal	emulate_branch		# emulate the branch
	nop
	.set 	reorder
	bne	v0,1,emulate_ok
	sw      v0,V0_OFFSET(sp)        # save the resulting pc
	lw      a0,A0_OFFSET(sp)        # restore exception frame pointer
	lw      a3,EF_EPC*4(a0)         # load the epc into a3
	lw      a2,fptype_word          # restore a2 with fptype_word value

	lw      a0,fpowner
	lw      a1,u+U_PROCP
	bne     a0,a1,illinstsig        # not current process
	sw      gp,u+PCB_RESCHED        # force resched

illinstsig:
	li      a1,SIGILL
	b	send_signal

emulate_ok:
	sw	v0,V0_OFFSET(sp)	# save the resulting pc
	lw	a0,A0_OFFSET(sp)	# restore exception frame pointer
	lw	a3,EF_EPC*4(a0)		# load the epc into a3
	lw	a2,fptype_word		# restore a2 with fptype_word value

	# Now load the floating-point instruction in the branch delay slot
	# to be emulated by softfp().
	lw	a1,4(a3)
4:
	/*
	 * Check to see if the instruction to be emulated is a floating-point
	 * instruction.  If it is not then this interrupt must have been caused
	 * by writing to the fpc_csr a value which will cause an interrupt.
	 * It is possible however that when writing to the fpc_csr the
	 * instruction that is to be "emulated" when the interrupt is handled
	 * looks like a floating-point instruction and will incorrectly be
	 * emulated and a SIGFPE will not be sent.  This is the user's problem
	 * because he shouldn't write a value into the fpc_csr which should
	 * cause an interrupt.
	 */
	srl	a3,a1,OPCODE_SHIFT
	beq	a3,OPCODE_C1,10f

	/*
	 * Setup and call psignal() to send a SIGFPE to the current process.
	 * If the owner of the floating-point unit is the current process then
	 * u.u_pcb.pcb_resched is set to force entry into trap so the signal
	 * will be posted before returning to user mode.
	 */
12:
	lw	a0,fpowner
	lw	a1,u+U_PROCP
	bne	a0,a1,11f		# not current process
	sw	gp,u+PCB_RESCHED	# force resched
11:
	li	a1,SIGFPE

send_signal:
	jal	psignal

	# We must clear the coprocessor interrupt without losing fp
	# state; we do this by calling checkfp which will unload
	# the fp to the pcb and clear the fp csr.  A signal is
	# pending, sendsig will clear the csr in the pcb after
	# saving the fp state from the pcb into the sigcontext and
	# before calling the signal handler
	lw	a0,fpowner
	move	a1,zero
	jal	checkfp

	b	8f

10:
	# For now all instructions that cause an interrupt are just handed
	# off to softfp() to emulate it and come up with correct result.
	# The arguments to softfp() are:
	#	a0 -- ef (exception frame)
	#	a1 -- floating-point instruction
	#	a2 -- fptype_word
	#
	# What might have be done is for all exceptions for which the trapped
	# result is the same as the untrapped result is: turn off the enables,
	# re-excute the instruction, restore the enables and then post a SIGFPE.

	jal	softfp
	beq	v0,zero,5f	# no signal posted

	# We must clear the coprocessor interrupt without losing fp
	# state, we do this by calling checkfp which will unload
	# the fp to the pcb and clear the fp csr.  A signal is
	# pending, sendsig will clear the csr in the pcb after
	# saving the fp state from the pcb into the sigcontext and
	# before calling the signal handler
	lw	a0,fpowner
	move	a1,zero
	jal	checkfp
	b	8f

5:
#ifdef XPRBUG
	lw	a0,xpr_flags
	and	a0,XPR_INTR
	beq	a0,zero,6f
	subu	sp,10*4
	.set	noreorder
	la	a0,9f
	cfc1	a1,fpc_csr
	jal	xprintf
	nop	
	MSG("exit fp_intr csr = 0x%x")
	.set	reorder
	addu	sp,10*4
6:
#endif XPRBUG
#ifdef ASSERTIONS
	/*
	 * If going back to user code without posting a signal there must
	 * not be any exceptions which could cause an interrupt.
	 */
	.set	noreorder
	cfc1	a0,fpc_csr
	nop
	and	a1,a0,CSR_EXCEPT	# isolate the exception bits
	and	a0,CSR_ENABLE 		# isolate the enable bits 
	or	a0,(UNIMP_EXC >> 5)	# fake an enable for unimplemented
	sll	a0,5			# align both bit sets
	and	a0,a1			# check for coresponding bits
	beq	a0,zero,7f		# if not then ok
	nop
	.set	reorder
	PANIC("fp_intr csr exceptions")
7:
#endif ASSERTIONS

	# Enable fp and hardclock interrupts to allow time to be charged
	# to kernel instead of user. This is done since fp_intr is
	# higher priority than hardclock. Ok because kernel never does
	# floating point.
	.set	noreorder
	lw	a0,fptype_word		# get revision id
	li	v1,SR_IEC|SR_IMASK4	# (LDSLOT) tentatively, not an R6010...
	bne	a0,IRR_IMP_R6010,1f
	li	a1,CSR_IVECTSET_TIMER	# (BDSLOT)
	li	a0,CSR_IVECTMASK
	sw	a1,(a0)			# enable just SBC timer interrupts
	li	v1,SR_IEC|SR_IMASK2	# R6010 uses different bits
1:
#ifdef	DOINTDEBUG
	/*
	 * this is defined in locore.s
	 */
	MTC0(v1)
#else	/* DOINTDEBUG */
	mtc0	v1,C0_SR
#endif	/* DOINTDEBUG */
	.set	reorder

	# The instruction was emulated by softfp() without a signal being
	# posted so now change the epc to the target pc.  This is a nop if
	# this is the board because we stored the epc in V0_OFFSET when we
	# started.
	lw	a0,A0_OFFSET(sp)	# restore the exception frame pointer
	lw	v0,V0_OFFSET(sp)	# get the resulting pc
	sw	v0,EF_EPC*4(a0)		# store the resulting pc in the epc

8:
	lw	ra,RA_OFFSET(sp)
	addu	sp,FRAME_SIZE
	j	ra

/*
 * At this point there has been an interrupt from the floating-point
 * coprocessor but no process was using it.
 */
strayfp_intr:
	.set	noreorder
	mfc0	a2,C0_SR	# enable coproc 1 for the kernel
	nop
	or	a3,a2,SR_CU1		
	mtc0	a3,C0_SR
	nop			# before we can really use cp1
	nop
	ctc1	zero,fpc_csr	# clear interrupt
	mtc0	a2,C0_SR
	.set	reorder

	PRINTF("stray fp interrupt\012")
	lw	ra,RA_OFFSET(sp)
	addu	sp,FRAME_SIZE
	j	ra

	.end	fp_intr

LEAF(clear_fp_unit)
	.set	noreorder
	mfc0	a2,C0_SR	# enable coproc 1 for the kernel
	nop
	or	a3,a2,SR_CU1		
	mtc0	a3,C0_SR
	nop
	nop			# before we can really use cp1
	ctc1	zero,fpc_csr	# clear interrupt
	mtc0	a2,C0_SR
	j	ra
	nop
	.set	reorder
	.end clear_fp_unit

#undef	FRAME_SIZE
#undef	LOCAL_SIZE
#undef	A0_OFFSET
#undef	A1_OFFSET
#undef	A2_OFFSET
#undef	A3_OFFSET
#undef	RA_OFFSET
#undef	V0_OFFSET
