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
/* $Header: process.s,v 1.11.1.4.1.3 90/08/20 17:59:36 hawkes Exp $ */


/*
 * stackdepth()
 * return the number of bytes from the current stack pointer to the
 * top of the stack
 */
LEAF(stackdepth)
	li	v0,KERNELSTACK
	subu	v0,sp
	j	ra
	END(stackdepth)

/*
 * save()
 * save process context in u area pcb - return 0
 * NOTE: pcb must be first in u area struct!
 */
LEAF(save)
	SAVE_PIXIE_STATE(u+U_SAV_PIX)

	/*
	 * Save C linkage registers
	 */
	sw	ra,u+PCB_PC*4
	sw	sp,u+PCB_SP*4
	sw	fp,u+PCB_FP*4
	/*
	 * ?????
	 * SHOULD SAVE FP EXCEPTION INST REGISTER like:
	 * if (fpowner)
	 *	if (cp1_present)
	 *		swc1	FP_EXCEPT,u_PCB_FPEXCEPT*4
	 */
	/*
	 * Save callee saved registers, all other live registers should have
	 * been saved on call to save() via C calling conventions
	 */
	.set	noreorder
	sw	s0,u+PCB_S0*4
	sw	s1,u+PCB_S1*4
	sw	s2,u+PCB_S2*4
	sw	s3,u+PCB_S3*4
	sw	s4,u+PCB_S4*4
	sw	s5,u+PCB_S5*4
	mfc0	v0,C0_SR
	sw	s6,u+PCB_S6*4
	sw	v0,u+PCB_SR*4
	sw	s7,u+PCB_S7*4
	j	ra
	move	v0,zero			# BDSLOT: return 0
	.set	reorder
	END(save)

/*
 * resume(p)
 * restore state for process p
 * Map u area, then restore context from pcb
 * return 1
 * NOTE: pcb must be first in u area!
 */
LEAF(resume)
	/*
	 * resume(p) -- restore context of process p
	 * NOTE: resume should ONLY be called from swtch()!
	 */
	lw	s1,machine_type
	lw	t0,P_TLBPID(a0)		# Load new TLBPID
	.set	noreorder
	bne	s1,BRDTYPE_R6300,1f
	mtc0	zero,C0_SR		# (BDSLOT) disable interrupts
	nop				# R6000: make sure ints are off...
	WRITE_PID(t0,k0,k1,v0)		# ... then reset PID
	b	3f
	nop				# (BDSLOT)

1:	sll	t0,TLBHI_PIDSHIFT	# line up pid bits
	bne	s1,BRDTYPE_R3030,3f
	/*
	 * for the r3030 we are about to change the user process area.
	 * If we entered through a system call then add the system time
	 * to the current running process.
	 */
	lw	s1,u+R_LEVEL
	nop
	addiu	s1,-1
	bne	s1,zero,3f	/* we didn't come from a syscall */
	lw	s1,rambo_at_eend
	lw	s2,CLOCK_COUNT
	lw	s3,u+R_STIME
	subu	s2,s1
	addu	s3,s2
	sw	s3,u+R_STIME	
	.set	reorder

	/*
	 * Restore all 8 wired tlb entries here.
	 * Virtual addresses are stored in proc table entry in tlbhi_tbl.
	 * pde's (tlblo part) are in p_ubptbl.
	 * 2 wired entries are used for u structure and kernel stack.
	 * The other 6 entries are used to map page tables as needed.
	 * The tfault routine drops in entries on double tlb misses.
	 * 
	 */
3:	la	s1,P_UBPTBL(a0)		# pointer to tlblo entries
	li	s3,(TLBWIREDBASE<<TLBINX_INXSHIFT) # base of wired entries
	la	s2,P_TLBHI_TBL(a0)	# pointer to tlbhi entries
#ifdef R6000
	move	a0,t0			# setup arg1=PID
	lw	a2,(s1)			# get pte of u-area
	li	s4,0x80000000		# (LDSLOT) 1st two bits of K0/K1 addr
	li	s5,0xc0000000		#	   mask for first two bits
	sw	a2,last_uarea_pte	# remember in case get exception
					#  and have to stuff it in again
	/*
	 *  We could branch around the redundant lw, but that takes two
	 *  cycles (branch and delay slot).  Just fall through for one cycle.
	 */
#endif R6000

1:
	.set	noreorder
	lw	a2,(s1)			# get pte (tlblo)
	lw	a1,(s2)			# get virtual address (tlbhi)
	addi	s1,8			# bump pointer to pte's
#ifdef R6000
	addi	s2,4			# bump pointer to virtual addresses
	and	t0,a1,s5		# don't bother to stuff a tlb entry ...
	beq	t0,s4,2f		#  ... if virtual addr is K0/K1
	nop				# (BDSLOT)
	
	jal	tlbdropin		# stuff tlb entry
	nop				# (BDSLOT)
2:
#else
	or	a1,t0			# merge in tlbpid
	mtc0	a2,C0_TLBLO		# set pfn and access bits
	mtc0	a1,C0_TLBHI		# set virtual page number and tlbpid
	mtc0	s3,C0_INX		# set index to wired entry
	addi	s2,4			# bump pointer to virtual addresses
	c0	C0_WRITEI		# write entry
#endif !R6000
	addi	s3,+((1)<<TLBINX_INXSHIFT)	# bump index
	bne	s3,+((TLBWIREDBASE+NWIREDENTRIES)<<TLBINX_INXSHIFT),1b
	nop
	.set	reorder

	/*
	 * ?????
	 * SHOULD RELOAD FP EXCEPTION INST REGISTER
	 * if (fpowner)
	 *	if (cp1_present) 
	 *		lwc1	FP_EXCEPT,u+PCB_FPEXCEPT
	 */

	/*
	 * Reload callee saved registers, all other live registers
	 * should be reloaded from stack via C calling conventions.
	 */
	REST_PIXIE_STATE(u+U_SAV_PIX)
	.set	noreorder
	lw	s7,machine_type
	lw	ra,u+PCB_PC*4
	lw	sp,u+PCB_SP*4
	lw	fp,u+PCB_FP*4
	lw	s0,u+PCB_S0*4
	lw	s1,u+PCB_S1*4
	lw	s2,u+PCB_S2*4
	lw	s3,u+PCB_S3*4
	lw	s4,u+PCB_S4*4
	lw	s5,u+PCB_S5*4
	lw	s6,u+PCB_S6*4
	/*
	 * we have restored the old process. If it was in a system call then we resume
	 * the counting of the system time for the process.
	 */
	addiu	s7,-BRDTYPE_R3030
	bne	s7,zero,1f
	lw	v0,u+PCB_SR*4		# BDSLOT
	lw	s7,u+R_LEVEL
	nop
	addiu	s7,-1
	bne	zero,s7,1f
	lw	s7,CLOCK_COUNT
	nop
	sw	s7,rambo_at_eend
1:	lw	s7,u+PCB_S7*4
	mtc0	v0,C0_SR
	j	ra
	li	v0,1			# BDSLOT: return non-zero
	.set	reorder
	END(resume)


/*
 * setjmp(jmp_buf) -- save current context for non-local goto's
 * return 0
 */
LEAF(setjmp)
	SAVE_PIXIE_STATE(u+U_JMP_PIX)

	.set	noreorder
	sw	ra,JB_PC*4(a0)
	sw	sp,JB_SP*4(a0)
	sw	fp,JB_FP*4(a0)
	sw	s0,JB_S0*4(a0)
	sw	s1,JB_S1*4(a0)
	sw	s2,JB_S2*4(a0)
	sw	s3,JB_S3*4(a0)
	sw	s4,JB_S4*4(a0)
	sw	s5,JB_S5*4(a0)
	mfc0	v0,C0_SR
	sw	s6,JB_S6*4(a0)
	sw	v0,JB_SR*4(a0)
	sw	s7,JB_S7*4(a0)
	j	ra
	move	v0,zero			# BDSLOT: return 0
	.set	reorder
	END(setjmp)


/*
 * longjmp(jmp_buf)
 */
LEAF(longjmp)
	REST_PIXIE_STATE(u+U_JMP_PIX)

	.set	noreorder
	lw	ra,JB_PC*4(a0)
	lw	sp,JB_SP*4(a0)
	lw	fp,JB_FP*4(a0)
	lw	s0,JB_S0*4(a0)
	lw	s1,JB_S1*4(a0)
	lw	s2,JB_S2*4(a0)
	lw	s3,JB_S3*4(a0)
	lw	s4,JB_S4*4(a0)
	lw	s5,JB_S5*4(a0)
	lw	v0,JB_SR*4(a0)
	lw	s6,JB_S6*4(a0)
	mtc0	v0,C0_SR
	lw	s7,JB_S7*4(a0)
	j	ra
	li	v0,1			# BDSLOT: return non-zero 
	.set	reorder
	END(longjmp)
