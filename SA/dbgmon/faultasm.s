#ident "$Header: faultasm.s,v 1.8 90/08/10 10:34:21 hawkes Exp $"
/*	%Q%	%I%	%M%
 * faultasm.s -- dbgmon fault handling code
 */
/* $Copyright$ */

#include "sys/signal.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/ctlspace.h"
#include "saio/saioctl.h"
#include "dbgmon/dbgmon.h"

#define RAMBO_COUNT     ((RAMBO_BASE + 0x0c00) | 0xA0000000)

	.text
/*
 * breakpoint -- client fielded a breakpoint it doesn't want to handle
 *
 * kernel (and other clients) should restore all registers except AT and k0
 * k0 should be set to AT at time of breakpoint (the state of k0 can't be
 * saved, unfortunately), then the client should jump here by loading
 * AT with the address of "breakpoint" and jumping via AT
 */
LEAF(breakpoint)
	.set	noat
	la	AT,_gp_save
	sw	gp,0(AT)
	la	gp,_gp
	.set	at
	sw	k0,_at_save
	sw	v0,_v0_save
	li	v0,EXCEPT_BRKPT
	j	exception
	END(breakpoint)

/*
 * exception vector code
 * hook_exceptions() sets-up E_VEC to jump here
 */
exceptnorm:
	.set	noat
	move	k0,AT
	la	AT,_gp_save
	sw	gp,0(AT)
	la	gp,_gp
	.set	at
	sw	k0,_at_save
	sw	v0,_v0_save
	li	v0,EXCEPT_NORM
	j	exception

/*
 * utlb miss
 * hook_exceptions() sets-up UT_VEC to jump here
 */
exceptutlb:
	.set	noat
	move	k0,AT
	la	AT,_gp_save
	sw	gp,0(AT)
	la	gp,_gp
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
	 * for faults (whether in monitor or client mode)
	 */
	.set	noreorder
	sw	v0,_exc_save
	nop
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
	 * Only save registers if in client mode
	 * then change mode to prom mode
	 */
	lw	v0,_prom_mode
	sw	v0,_mode_save
	beq	v0,MODE_DBGMON,nosave	# was in prom mode
	li	v0,MODE_DBGMON
	sw	v0,_prom_mode		# now in prom mode
	lw	v0,_gp_save
	sw	v0,_regs+R_GP*4
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
	sw	ra,_regs+R_RA*4
	mflo	v0
	sw	v0,_regs+R_MDLO*4
	mfhi	v0
	sw	v0,_regs+R_MDHI*4
	lw	v1,machine_type
	li	v0,BRDTYPE_R6300
	beq	v0,v1,1f		# don't save R2000/3000 CP0 regs
	.set	noreorder
	mfc0	v0,C0_INX
	mfc0	v1,C0_RAND		# save just to see it change
	sw	v0,_regs+R_INX*4
	sw	v1,_regs+R_RAND*4
	mfc0	v0,C0_TLBLO
	mfc0	v1,C0_TLBHI
	sw	v0,_regs+R_TLBLO*4
	sw	v1,_regs+R_TLBHI*4
	mfc0	v0,C0_CTXT
	lw	v1,machine_type
	sw	v0,_regs+R_CTXT*4	# (BDSLOT)
	li	v0,BRDTYPE_R3030
	bne	v0,v1,2f
	nop
	.set	reorder
	lw	v0,RAMBO_COUNT(zero)	# for the Rx3030 save
	sw	v0,_sbc_count_save	# the clock count
	b	2f
	.set	noreorder
1:					# R6000 comes here
	mfc0	v0,C0_ERROR
	mfc0	v1,C0_PID
	sw	v0,_error_save
	sw	v0,_regs+R_ERROR*4
	sw	v1,_regs+R_PID*4
	lw	v0,CSR_COUNT(zero)	# save the current SBC Count
	lw	v1,CSR_IVECTSET(zero)	# and remember if the IntVector
	sw	v0,_sbc_count_save	#  timer interrupt bit was on
	and	v1,CSR_IVECTSET_TIMER	#   at the time we hit the bpoint
	sw	v1,_sbc_intvec_timer_save
	.set	reorder
2:
	jal	_remove_brkpts		# reinstall original code
	jal	_save_vectors		# save user vectors
nosave:
	j	_exception_handler

/*
 * _resume -- resume execution of client code
 */
LEAF(_resume)
	subu	sp,(4*4)		# small frame
	jal	_restore_vectors	# restore user vectors
	jal	_install_brkpts		# save code and insert BRKPTS
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
	lw	v0,machine_type
	li	v1,BRDTYPE_R6300
	beq	v0,v1,1f		# don't restore R2000/3000 CP0 regs
	.set	noreorder
	lw	v0,_regs+R_INX*4
	lw	v1,_regs+R_TLBLO*4
	mtc0	v0,C0_INX
	mtc0	v1,C0_TLBLO
	lw	v0,_regs+R_TLBHI*4
	lw	v1,_regs+R_CTXT*4
	mtc0	v0,C0_TLBHI
	mtc0	v1,C0_CTXT
	lw	v0,machine_type
	li	v1,BRDTYPE_R3030
	bne	v0,v1,2f
	.set	reorder
	lw	v0,_sbc_count_save	# restore count for RX3030 board
	sw	v0,RAMBO_COUNT(zero)
	b	2f
	.set	noreorder
1:
	/* R6000 */
	lw	v1,_sbc_intvec_timer_save
	li	v0,CSR_IVECTSET_TIMER	# clear the IntVector
	sw	v0,CSR_IVECTCLR(zero)	#  timer interrupt bit
	lw	v0,_sbc_count_save
	sw	v1,CSR_IVECTSET(zero)	#  and restore the original timer int
	sw	v0,CSR_COUNT(zero)	# restore the original SBC Count
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
	li	k0,MODE_CLIENT
	sw	k0,_prom_mode		# entering client mode
	lw	k0,_regs+R_EPC*4
	.set	noat
	lw	AT,_regs+R_AT*4
	.set	noreorder
	j	k0
	rfe
	.set	reorder
	.set	at
	END(_resume)

/*
 * invoke(procedure, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8)
 * interface for call command to client code
 * copies arguments to new frame and sets up gp for client
 */
INVOKEFRM=(8*4)+4+4		# 8 argsaves, ra save, gp save
NESTED(invoke, INVOKEFRM, zero)
	subu	sp,INVOKEFRM
	sw	ra,INVOKEFRM-4(sp)
	sw	gp,INVOKEFRM-8(sp)
	move	v0,a0
	move	a0,a1
	move	a1,a2
	move	a2,a3
	lw	a3,INVOKEFRM+(4*4)(sp)
	lw	v1,INVOKEFRM+(5*4)(sp)
	sw	v1,4*4(sp)
	lw	v1,INVOKEFRM+(6*4)(sp)
	sw	v1,5*4(sp)
	lw	v1,INVOKEFRM+(7*4)(sp)
	sw	v1,6*4(sp)
	lw	v1,INVOKEFRM+(8*4)(sp)
	sw	v1,7*4(sp)
	lw	gp,_regs+R_GP*4
	jal	v0
	lw	gp,INVOKEFRM-8(sp)
	lw	ra,INVOKEFRM-4(sp)
	addu	sp,INVOKEFRM
	j	ra
	END(invoke)

/*
 * sa_spl() -- reestablish desired sa lib status register
 * clear any pending write bus error interrupts
 * returns current sr
 */
LEAF(sa_spl)
	.set	noreorder
	lw	v1,machine_type
	mfc0	v0,C0_SR
	bne	v1,BRDTYPE_R6300,1f
	li	v1,(SR_IMASK7|SR_IEC)	# (BDSLOT) (for non-6000)
	j	ra
	mtc0	zero,C0_SR		# (BDSLOT) for 6000, disable interrupts

1:
#ifdef MIPS
	lw	zero,SBE_ADDR+K1BASE
#endif
	j	ra
	mtc0	v1,C0_SR		# (BDSLOT) enable only BusErr
	.set	reorder
	END(sa_spl)

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

LEAF(_kernel_bp)
	break	BRK_KERNELBP		# so C code knows brkpt inst
	END(_kernel_bp)


/*
 * exception state
 */
	BSS(_gp_save,4)			# gp at time of exception
	BSS(_epc_save,4)		# epc at time of exception
	BSS(_at_save,4)			# AT at time of exception
	BSS(_v0_save,4)			# v0 at time of exception
	BSS(_exc_save,4)		# exc at time of exception
	BSS(_badvaddr_save,4)		# badvaddr at time of exception 
	BSS(_cause_save,4)		# cause reg at time of exception
	BSS(_sp_save,4)			# sp at time of exception
	BSS(_mode_save,4)		# prom mode at time of exception
	BSS(_sr_save,4)			# sr at time of exceptions
	BSS(_fault_sp,4)		# stack for nofault handling
	BSS(_prom_mode,4)		# running client or dbgmon code
	BSS(_error_save,4)		# error reg at time of exceptions
	BSS(_sbc_count_save,4)		# R6000 SBC Count register
	BSS(_sbc_intvec_timer_save,4)	#  and IntVector timer int bit
	BSS(_regs,NREGS*4)		# client register contents
