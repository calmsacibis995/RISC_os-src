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
/* $Header: locore.s,v 1.52.1.18.1.6.1.4 90/11/15 13:54:23 beacker Exp $ */

#include "sys/cpu_board.h"
#include "sys/boot.h"

#ifdef PIXIE

#define SAVE_PIXIE_STATE(base)	\
	la	t0,pixie;	\
	li	t1,(47*4);	\
	lw	t4,base;	\
99:	addu	t3,t1,t0;	\
	lw	t2,128(t3);	\
	addu	t5,t4,t1;	\
	sw	t2,(t5);	\
	subu	t1,4;		\
	bge	t1,zero,99b

#define REST_PIXIE_STATE(base)	\
	la	t0,pixie;	\
	li	t1,(47*4);	\
	lw	t4,base;	\
99:	addu	t3,t1,t0;	\
	addu	t5,t4,t1;	\
	lw	t2,(t5);	\
	sw	t2,128(t3);	\
	subu	t1,4;		\
	bge	t1,zero,99b

#define EXC_SAVE_PIXIE_STATE	\
	la	a0,pixie;	\
	li	a1,(47*4);	\
99:	addu	k0,a1,a0;	\
	lw	a2,128(k0);	\
	addu	k1,a1,sp;	\
	sw	a2,EFSZ(k1);	\
	subu	a1,4;		\
	bge	a1,zero,99b

#define EXC_REST_PIXIE_STATE	\
	la	a0,pixie;	\
	li	a1,(47*4);	\
99:	addu	k0,a1,a0;	\
	addu	k1,a1,sp;	\
	lw	a2,EFSZ(k1);	\
	sw	a2,128(k0);	\
	subu	a1,4;		\
	bge	a1,zero,99b
#else

#define SAVE_PIXIE_STATE(base)
#define REST_PIXIE_STATE(base)

#define EXC_SAVE_PIXIE_STATE
#define EXC_REST_PIXIE_STATE

#endif PIXIE

#ifdef R6000
#undef FAST_KTLBMISS_HANDLER
#endif R6000
/*
 * User stack location definition 
 */
	ABS(userstack, USERSTACK);
/*
 * Kernel entry point table
 */
STARTFRM=	(4*4)+4+4		/* 4 argsaves, old fp, and old sp */
NESTED(start, STARTFRM, zero)
EXPORT(eprol)				/* to satisfy Prof of Sable output */
	j	_realstart		/* kernel entry point */
	j	_coredump		/* dump core to config'ed dump dev */
	j	_xprdump		/* dump trace buffer to console */
	j	_xprtail		/* dump tail of trace buffer */
	j	_msgdump		/* dump msg buffer to console */

/*
 * kstackflag -- if zero, currently on user stack, if non-zero on kernel stk
 */
#ifdef R6000
#define IOC_DBE_RETRY_UPB	2048
#define IOC_DBE_RETRY_DELAY	10
	/*
	 *  Several data structures are kept in locore:
 	 *	800003d8   last u-area pte (4 bytes)
 	 */
	ABS(last_uarea_pte,	0x800003d8)
	BSS(ioc_retry_stats,		4)	/* cumulative retry count */
	BSS(utlbmiss_save_AT,	4)
	BSS(utlbmiss_save_t0,	4)
	BSS(utlbmiss_save_t1,	4)
	BSS(utlbmiss_save_t2,	4)
	BSS(utlbmiss_save_t3,	4)
	BSS(utlbmiss_save_t4,	4)
	BSS(utlbmiss_save_t5,	4)
	BSS(utlbmiss_save_t6,	4)
	BSS(utlbmiss_save_t7,	4)
#endif R6000
	ABS(kstackflag, E_VEC-4)
	BSS(remote_bp_encountered,	4)	/* went to dbgmon */
	BSS(ioc_retry_last_epc,		4)
	BSS(ioc_retry_last_badvaddr,	4)
	BSS(ioc_retry_count,		4)
	BSS(ioc_retry_count_max,	4)
	BSS(ioc_retry_delay,		4)
/*
 * we need to save the rambo counter at certain times, most
 * particuarly when we are going into user mode, when we are
 * coming out of user mode, and we also need to know how
 * long we spent in user mode!
 *
 * ###### IMPORTANT ###### IMPORTANT ###### IMPORTANT ######
 *
 * It MUST be ensured that the following absolute addresses have
 * bit 15 as a 0. Since the accesses to these addresses are
 * constructed manually for stores (as we do not have the AT
 * register available) we assume this to be so. If this changes, and
 * one of the addresses gets a 1 at bit 15 some references to that
 * variable WILL fail.
 *
 *	800003e0	Save area for $at during ktlbmiss processing
 *	800003e4	Save area for $k1 during ktlbmiss processing
 *	800003e8	Save area for rambo count on exception entry
 *	800003ec	Save area for rambo count whilst in user mode 
 *			(or a syscall)
 */
	ABS(multiuser,		0x800003d8)	/* if multiuser or not */
	ABS(badco,		0x800003dc)	/* used for debugging */
	ABS(exception_at_save,	0x800003e0)
	ABS(exception_k1_save,	0x800003e4)
	ABS(rambo_clock_save,	0x800003e8)
	ABS(rambo_at_eend,	0x800003ec)	/* is also used in trap.c */
	ABS(CLOCK_COUNT,	0xbc000c00)	/* rambo count register */
	.globl	rambo_clock_delay
/*
 *	If DOINTDEBUG is defined we will preserve all register values, 
 * and call intdebug with the old and new values for the SR.  We also 
 * pass in the return address which will allow us to check for held off 
 * interrupts.
 */
#ifdef	DOINTDEBUG
	.globl	intdebug
#define	MTC0(new)	add	sp,-68; \
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
			sw	new,0(sp); \
			sw	ra,8(sp); \
			mfc0	a0,C0_SR; \
			lw	a1,0(sp); \
			lw	a2,8(sp); \
			/*jal	intdebug;*/ \
			add	sp,-20; \
			add	sp,20; \
			lw	new,0(sp); \
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
			add	sp,68; \
			mtc0	new,C0_SR
#else
#define	MTC0(new)	mtc0	new,C0_SR
#endif	/* DOINTDEBUG */

#ifdef R6000_BUG_PID
/*
 *	NOTE:
 *	There is a bug in the R6020 SBC which necessitated rework on the
 *	CPU board, the net effect of which produces a problem with
 *		mtc0  Rx,C0_PID
 *	under obscure circumstances, leaving a correct PID value in the
 *	CPU onchip PID register and an incorrect PID value in the offchip
 *	register.  For instance:  if this instruction lies in the last word of
 *	an S-cache line, and the next line gets a shared hit.
 *	The simplest workaround is to issue two such instructions back-to-back.
 *	Another glitch:  because of the PID mismatch, an external parity error
 *	might be declared, so we disable these MachineChecks around the mtc0.
 *	Because interrupts are disabled, we can use $k0 and $k1.
 *	Another glitch:  the back-to-pack write-PID sequence doesn't completely
 *	fix the problem in CPU rev3.1, as it might leave the 2nd mtc0's S-cache
 *	tag with bad parity.  We correct for this by invalidating this
 *	instruction's address, forcing an I-cache miss and a shared-hit in
 *	the S-cache, which rewrites that tag.
 */
#define WRITE_PID(newPID,saveERROR,scratch,saveSR)	\
			mfc0	saveERROR,C0_ERROR;	\
			li	scratch,C0_ERROR_IMASK;	\
			mfc0	saveSR,C0_SR;		\
			mtc0	scratch,C0_ERROR;	\
			li	scratch,SR_MM_MODE;	\
			mtc0	scratch,C0_SR;		\
			mtc0	newPID,C0_PID;		\
80:			mtc0	newPID,C0_PID;		\
			la	scratch,80b;		\
			inval	(scratch);		\
			or	saveERROR,C0_ERROR_EXT;	\
			mtc0	saveERROR,C0_ERROR;	\
			mtc0	saveSR,C0_SR

#else R6000_BUG_PID
#define	WRITE_PID(newPID,tmp0,tmp1,tmp2)	mtc0	newPID,C0_PID
#endif R6000_BUG_PID

/*
 * Kernel initialization
 */
	.globl	ipl_special_mask
_realstart:
	/*
	 * Now on prom stack; a0, a1, and a2 contain argc, argv, and environ
	 * from boot.
	 */
	la	sp,start-STARTFRM	# leave room for arg saves
#ifdef notdef
	 TODO stack needs to be fixed!
	subu	sp,STARTFRM		# expand frame for arg saves
#endif notdef
#ifdef DBGMON
	sw	a0,_argc
	sw	a1,_argv
	sw	a2,_envirn
	sw	zero,multiuser		# zero this until we are multiuser
	sw	zero,kstackflag		# zero this for early exceptions
	la	a3,dbg_start		# location dbgmon will jump to
	jal	_check_dbg
dbg_start:	
	lw	a0,_argc
	lw	a1,_argv
	lw	a2,_envirn
#endif
	sw	zero,STARTFRM-4(sp)	# zero old ra for debuggers
	sw	zero,STARTFRM-8(sp)	# zero old fp for debuggers (???)
	.set	noreorder
#ifdef	R6000
	li	v0,0xff			# allow all C0_PID bits to change
	mtc0	v0,C0_PIDMASK
	mtc0	zero,C0_SR		# clear all SR bits (in case PROM code
					#  leaves some useless bits on)
#ifdef R6000_BUG_PID
	li	v0,C0_ERROR_IMASK	# inhibit all parity errors
	mtc0	v0,C0_ERROR
	nop
#endif R6000_BUG_PID
	WRITE_PID(zero,k0,k1,v0)	# initialize PID
#ifdef R6000_BUG_PID
	/*
	 *  CPU rev3.1:  mtc0 Rx,C0_PID  executed uncached creates an
	 *  inconsistent onchip/offchip PID register, leading to
	 *  parity errors in the S-cache.
	 */
	li	v0,SR_MM_MODE
	mtc0	v0,C0_SR		# enable MM-MODE
	la	v0,1f
	or	v0,K1BASE		# execute loop uncached
	li	t0,0
	li	t1,0x3fffc
	j	v0
	nop
1:	lcache	t2,(t0)			# clear parity error
	lcache	t3,1(t0)
	nop
	scache	t2,(t0)
	scache	t3,1(t0)
	bne	t0,t1,1b
	addi	t0,4
	mtc0	zero,C0_SR		# disable MM-MODE
	la	v0,1f
	j	v0			# resume to execute cached
	nop
1:
#endif R6000_BUG_PID
	li	v0,C0_ERROR_IMASK|C0_ERROR_MASK	# clear all Cp0 ERROR conditions
	mtc0	v0,C0_ERROR			#  and inhibit all errors
	li	v0,last_uarea_pte
	sw	zero,(v0)		# clear memory -- used as flag, too
#else  !R6000
	mtc0	zero,C0_TLBHI
	li	v0,-((-KPTEBASE)>>1)	# set up for 16 byte pte's
	mtc0	v0,C0_CTXT
#endif !R6000
	.set	reorder
	la	gp,_gp
	sw	gp,kstackflag		# running on kernel stack
	jal	mlsetup			# called mlsetup(argc, argv, environ)
	la	sp,KERNELSTACK-STARTFRM	# leave room for arg saves, etc
	sw	sp,multiuser		# non zero, now multiuser
	jal	main			# called main(argc, argv, environ)
	/*
	 * On return from main, should be set up and running as tlbpid 1.  The
	 * icode should already be copied into the user data space starting
	 * at USRDATA.  Initial register values don't matter, the GP won't
	 * be referenced.  The following should enter user mode for the first
	 * time.
	 */

	.set	noreorder
	li	v0,SR_IMASK0|SR_IEP|SR_KUP # disable IEc, enable IEp, prev umode
	lw	k1,ipl_special_mask	# mask defined in trap.c (intr)
	nop
	and	v0,v0,k1
	MTC0(v0)
#ifdef R6000
	li	k1,CSR_IVECTMASK_NONE
	sw	k1,CSR_IVECTMASK(zero)	# setup SBC Mask to enable interrupts
#endif R6000
	li	k1,USRDATA		# jump to first word of text
	sw	zero,kstackflag		# switching to user stack
	j	k1			# geronimo!  Off to /etc/init!
	c0	C0_RFE			# BDSLOT
	.set	reorder
	END(start)

#ifdef DBGMON

/*
 * Local storage for arguments
 */
LBSS(_argc, 4)
LBSS(_argv, 4)
LBSS(_envirn, 4)
#endif DBGMON


/*
 * Misc. kernel entry points
 */
EXPORT(_coredump)
	jal	dumpsys
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif R6000_BUG_PID
	la	v0,prom_restart
	j	v0

EXPORT(_xprdump)
	jal	xprdump
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif R6000_BUG_PID
	la	v0,prom_restart
	j	v0

EXPORT(_xprtail)
	jal	xprtail
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif R6000_BUG_PID
	la	v0,prom_restart
	j	v0

EXPORT(_msgdump)
	jal	msgdump
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif R6000_BUG_PID
	la	v0,prom_restart
	j	v0

/*
 * do a dump and then perform power-up sequence
 */
EXPORT(doadump)
	jal	dumpsys
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif R6000_BUG_PID
	la	v0,prom_reboot
	j	v0

/*
 * General exception entry point.
 */
#ifdef ASM_FIXED
#define	M_EXCEPT	+(M_SP|M_GP|M_AT|M_K1|M_A0|M_A1|M_A2|M_A3|M_S0|M_RA)
#define	M_TFISAVE	+(M_V0|M_V1|M_T0|M_T1|M_T2|M_T3|M_T4|M_T5|M_T6|\
			M_T7|M_T8|M_T9|M_FP)
#else
#define	M_EXCEPT	0xb80100f3
#define	M_TFISAVE	0x4300ff0d
#define	M_EXCSAVE	0xfb01ffff
#define	M_TRAPSAVE	0xfbffffff
#define	M_SYSCALLSAVE	0xf80100ff
#endif

VECTOR(exception_hook, M_EXCEPT)
#ifdef R6000
	.set	noreorder
	/*
	 *  To narrow a fatal timing window, temporarily inhibit MachineCheck
	 *  exceptions.  If we're not taking a MachineCheck exception, we'll
	 *  enable them again a little further along in the exception handler.
	 *  (Writing zero to the error bits leaves them untouched.)
	 *  If we are taking a MachineCheck exception now, then we'll enable
	 *  them in trap() after we log the error.
	 */
	li	k0,C0_ERROR_IMASK
	mtc0	k0,C0_ERROR
	.set	reorder
#endif R6000
	j	exception
	EXPORT(eexception_hook)
	END(exception_hook)

#ifdef FAST_KTLBMISS_HANDLER
#ifndef K2BASE
#define K2BASE 0xc0000000
#endif K2BASE
#ifndef EXC_RMISS
#define EXC_RMISS	8
#endif EXC_RMISS
#ifndef EXC_WMISS
#define EXC_WMISS	12
#endif EXC_WMISS
#ifndef PG_VR
#define PG_VR	0x0200
#endif PG_VR
	.globl	syssegsz
	.globl	kptbl
	.globl	kvmiss
#endif FAST_KTLBMISS_HANDLER

/*
 * The Rx3030 machine now has a different exception entry routine,
 * exception_r3030 instead - Rex
 */

VECTOR(exception, M_EXCEPT)	# Copied down to 0x80000080
	.set	noreorder
 	.set	noat
#ifdef FAST_KTLBMISS_HANDLER
	/*
	 * Inline implementation of a fast kernel tlbmiss
	 */
	move	k0,AT
	.set	at
	sw	k0,exception_at_save
	mfc0	k0,C0_CAUSE
	sw	k1,exception_k1_save
	and	k0,CAUSE_EXCMASK	# isolate exception cause
	beq	k0,EXC_RMISS,1f		# we can only handle TLB r/w misses
	nop
	bne	k0,EXC_WMISS,longway
1:
	mfc0	k0,C0_BADVADDR		# pick up pte address and fill BDSLOT
	nop
	.set	reorder
	bgt	k0,zero,longway		# if KUSEG(badvaddr), goto longway
	sub	k0,K2BASE
	blt	k0,zero,longway		# if ! K2SEG(badvaddr), goto longway
	and	k0,(~POFFMASK)
	srl	k0,(PNUMSHFT - 3)
	lw	k1,syssegsz
	sll	k1,3
	bge	k0,k1,longway		# outside kptbl
	lw	k1,kptbl
	addu	k0,k1			# address of pte in kptbl
	.set	noreorder
	.set	noat
	lw	k0,0(k0)		# load desired pte
	mfc0	k1,C0_EPC		# get ready for rfe and fill load delay
	mtc0	k0,C0_TLBLO		# drop pte in tlb
	and	k0,PG_VR		# check valid bit and fill load delay
	c0	C0_WRITER		# random drop in into tlb
	beq	k0,zero,longway		# panic if not valid
	nop
	.set	at
	.set	reorder
	lw	k0,kvmiss
	addu	k0,1
	sw	k0,kvmiss
	lw	k0,exception_at_save
	.set	noreorder
	.set 	noat
	move	AT,k0
	j	k1
	c0	C0_RFE
	nop

	.set	at
longway:
	lw	k0,exception_at_save
	lw	k1,exception_k1_save
	.set	noat
	move	AT,k0
#endif FAST_KTLBMISS_HANDLER
	/*
	 * WARNING!!!!
	 * Address calculation here assumes that 16 bit offset on lw
	 * does NOT have sign bit set!
	 */
	lui	k0,kstackflag>>16
	lw	k0,+kstackflag&0xffff(k0)
	nop
	beq	k0,zero,1f
	move	k0,AT			# BDSLOT: save at
 	.set	at
	.set	reorder
	srl	sp,2			# paranoia:  make sure sp is
	sll	sp,2			#  word-aligned, else infinite loop!
	sw	sp,EF_SP*4-EF_SIZE(sp)
	subu	sp,EF_SIZE
	b	2f

	/*
	 * Came from user mode or utlbmiss, initialize kernel stack
	 */
1:	sw	sp,KERNELSTACK-EF_SIZE+EF_SP*4
	la	sp,KERNELSTACK-EF_SIZE
	sw	gp,EF_GP*4(sp)
	la	gp,_gp
	sw	gp,kstackflag		# now on kernel stack (gp always != 0)
	/*
	 * This instruction stream can be cleaned up somewhat for write stalls,
	 * but for now, left as is so its readable when debugging
	 */
2:	sw	k0,EF_AT*4(sp)
	sw	k1,EF_K1*4(sp)		# in case we came from utlbmiss
	.set	noreorder
#ifdef R6000
	/*
	 *  If we're servicing a MachineCheck exception, then leave the
	 *  Cp0 ERROR register alone:  it has been reset to Inhibit all errors.
	 *  Otherwise, we want to restore the ERROR register to the state of
	 *  the Ignore bits at exception entry...except we want to be careful
	 *  to not enable a MachineCheck which has arisen since the beginning
	 *  of the exception handler!
	 */
	mfc0	k0,C0_CAUSE
	li	k1,EXC_CHECK		# (LDSLOT)
	andi	k0,CAUSE_EXCMASK	# we're here now because of a
	beq	k0,k1,9f		#  MachineCheck, so handle it normally
	lw	k1,cp0_error_imask	# (BDSLOT)
	mfc0	k0,C0_ERROR		# (LDSLOT) the current ERROR value
	srl	k1,16			# right-justify the Inhibit bits
	nor	k1,zero,k1		# errors which are not Inhibited
	and	k0,k1
	and	k0,C0_ERROR_MASK	# if a MachineCheck error has recently
	lw	k1,cp0_error_imask
	bne	k0,zero,1f		#  arisen, then
	nop				# (BDSLOT) don't reset Inhibit bits
	mtc0	k1,C0_ERROR		# otherwise, restore Inhibit to normal
1:	
	/*
	 *  Do software retries of BusErrors which result from an IOC
	 *  deadlock condition.  With uplevel SBC we can look at its Error
	 *  register to distinguish "deadlock DBE" from other DBE conditions.
	 *  We also retry IBE from Boot Prom ifetches.
	 */
	mfc0	k0,C0_CAUSE
	li	k1,EXC_DBE		# (LDSLOT)
	andi	k0,CAUSE_EXCMASK
	beq	k0,k1,1f
	li	k1,EXC_IBE
	bne	k0,k1,9f
	/*  this is an IBE  */
	nop
	mfc0	k0,C0_EPC		# BadVaddr for IBE is the EPC
	b	2f
	nop
1:
	/*  this is a DBE */
#ifdef SABLE
	lw	k1,nofault
	li	k0,NF_BADADDR
	beq	k1,k0,9f		# if doing badaddr(), then don't retry
#endif SABLE
	mfc0	k0,C0_BADVADDR		# (BDSLOT)
	.set	reorder
2:
	li	k1,PROM_RESET+(512*1024) # if BadVaddr is below PROM top
	sltu	k1,k0,k1
	beq	k1,zero,9f
	li	k1,IOA3			# ...and if BadVaddr is above IOA3-GBA0
	sltu	k1,k0,k1		#  then it's 
	bne	k1,zero,9f		# ...then it's a retryable DBE or IBE
	/*  now we know it's IOC or Gba space access  */
	li	k0,CSR_ERRREG
	lw	k1,(k0)			# if Bus Chip error register
	li	k0,SBCerrBadType
	and	k0,k1			#  says it's a type-5 error
	li	k1,SBCerrBadType5
	beq	k0,k1,8f		#   then always retry it
	.set	noreorder
	nop				# (BDSLOT)
	mfc0	k0,C0_BADVADDR
	.set	reorder
	lw	k1,ioc_retry_last_badvaddr
	beq	k0,k1,1f		# if this is a new BadVaddr
	sw	zero,ioc_retry_count	#  then start a new retry series
	sw	k0,ioc_retry_last_badvaddr
1:	.set	noreorder
	mfc0	k0,C0_EPC
	lw	k1,ioc_retry_last_epc
	nop				# (LDSLOT)
	.set	reorder
	beq	k0,k1,1f		# if this is a new EPC
	sw	zero,ioc_retry_count	#  then start a new retry series
	sw	k0,ioc_retry_last_epc
1:
	lw	k0,ioc_retry_count
	lw	k1,ioc_retry_count_max	# if now at max retry count,
	bne	k1,zero,1f
	li	k1,IOC_DBE_RETRY_DELAY	# setup default values for retry delay
	sw	k1,ioc_retry_delay
	li	k1,IOC_DBE_RETRY_UPB	#   and max retries the first time here
	sw	k1,ioc_retry_count_max
1:
	beq	k0,k1,9f		#  then declare a real DBE/IBE
	addi	k0,1
	sw	k0,ioc_retry_count	# otherwise, increment retry count
8:	lw	k1,ioc_retry_stats
	addi	k1,1			# increment count of the number of times
	sw	k1,ioc_retry_stats	#  this happens, just for statistics
	/*  delay a bit  */
	lw	k1,ioc_retry_delay
	.set	noreorder
1:
	bne	k1,zero,1b
	sub	k1,1			# (BDSLOT)
	/*  restart the access  */
	sw	zero,CSR_ERRREG(zero)	# clear the SBC error register
	.set	noat
	lw	AT,EF_AT*4(sp)		# restore AT and sp ... gp is ok,
	.set	at			#  since we know we came from kernel
	mfc0	k0,C0_EPC
	lw	sp,EF_SP*4(sp)		# (LDSLOT)
	j	k0			# reexecute the instruction
	c0	C0_RFE
	
9:
	lw	k1,CSR_IVECTMASK(zero)	# save entry SBC IntVectorMask
	nop				# (LDSLOT)
	sw	k1,EF_IVECTMASK*4(sp)
	mfc0	k0,C0_ERROR		# current Cp0 ERROR register has
	lw	k1,cp0_error_imask	#  been temporarily reset to Inhibit
	andi	k0,C0_ERROR_MASK	# (LDSLOT) all errors, so figure out
	or	k0,k1			#   what it *should* look like
	sw	k0,cp0_error_reg	#    and remember that
#endif R6000
	sw	a0,EF_A0*4(sp)
	mfc0	a0,C0_EPC
	sw	a1,EF_A1*4(sp)
	sw	a2,EF_A2*4(sp)
	sw	a3,EF_A3*4(sp)
	mfc0	a3,C0_CAUSE
	sw	s0,EF_S0*4(sp)
#ifdef PIXIE
	sw	s4,EF_S4*4(sp)
	sw	fp,EF_FP*4(sp)
#endif PIXIE
	sw	a3,EF_CAUSE*4(sp)
	sw	ra,EF_RA*4(sp)
	mfc0	s0,C0_SR
	sw	a0,EF_EPC*4(sp)
	.set	reorder
	sw	a0,EF_SAVEDPC*4(sp)	# to stop stack trace 

	EXC_SAVE_PIXIE_STATE

#ifdef R6000
	li	a0,SR_MM_MODE
	and	a0,s0
	.set	noreorder
	beq	a0,zero,1f
	nop
	and	a0,s0,~SR_MM_MODE
	MTC0(a0)			# turn off MM_MODE in SR
	and	a1,a3,CAUSE_EXCMASK
	beq	a1,0x3c,1f		# if MachineCheck, then keep going
	nop
	.set	reorder
	move	a2,a3
	lw	a3,EF_EPC*4(sp)
	PANIC("exception with SR_MM_MODE set!  Cause: %x  Epc: %x \n")
1:
#endif R6000

	/*
	 * Check for cache parity errors (r2000/r3000 only)
	 * If the error bit is set, clear it and increment a counter.
	 * Register assumptions:
	 *	at -- available for the assembler (saved above)
	 *	a0 -- already saved and can be used
	 *	s0 -- status register at time of exception
	 */
	.globl	cpe_count
	.lcomm	cpe_count,4	# cache parity error count in bss
#ifndef R6000
	.set	noreorder
	sll	a0,s0,31-20	# is this set in the saved sr?
	bgez	a0,3f		# if sign bit not set, not error
	MTC0(s0)		# BDslot:  this will clear SR_PE if it is set
	lw	a0,cpe_count
	nop
	addiu	a0,1		# increment the cache parity error counter
	sw	a0,cpe_count
3:
	.set	reorder
#endif !R6000

	/*
	 * Dispatch to appropriate exception handler
	 * Register setup:
	 *	s0 -- SR register at time of exception
	 *	a0 -- exception frame pointer
	 *	a1 -- cause code
	 *	a3 -- cause register
	 */
	and	a1,a3,CAUSE_EXCMASK
	lw	a2,causevec(a1)
	move	a0,sp
	.set	noreorder
	j	a2
	sw	s0,EF_SR*4(sp)
	.set	reorder
	EXPORT(eexception)
	END(exception)

/*
 * This is the Rx3030 exception entry vector
 */

VECTOR(exception_r3030, M_EXCEPT)	# Copied down to 0x80000080
	.set	noreorder
 	.set	noat
#ifdef FAST_KTLBMISS_HANDLER
	/*
	 * Inline implementation of a fast kernel tlbmiss
	 */
	move	k0,AT
	.set	at
	sw	k0,exception_at_save
	mfc0	k0,C0_CAUSE
	sw	k1,exception_k1_save
	sw	k0,badco
	and	k0,CAUSE_EXCMASK	# isolate exception cause
	lw	k1,CLOCK_COUNT
	beq	k0,EXC_RMISS,1f		# we can only handle TLB r/w misses
	nop
	bne	k0,EXC_WMISS,longway_r3030
1:
/*
 * the branch delay slot will be taken up by the first half of the
 * next instruction. We only want to complete the save if we are
 * falling through to the fast TLB miss routine.
 */
	sw	k1,rambo_clock_save	# BDSLOT
	mfc0	k0,C0_BADVADDR		# pick up pte address and fill BDSLOT
	nop
	.set	reorder
	bgt	k0,zero,longway_r3030	# if KUSEG(badvaddr), goto longway_r3030
	sub	k0,K2BASE
	blt	k0,zero,longway_r3030	# if ! K2SEG(badvaddr), goto longway_r3030
	and	k0,(~POFFMASK)
	srl	k0,(PNUMSHFT - 3)
/*
 * here we corrupt k1, so any further jumps to longway_r3030 have to go through
 * longway0_r3030 which will (if we are using RAMBO) restore k1 and then fall
 * through to longway_r3030
 */
	lw	k1,syssegsz
	sll	k1,3
	bge	k0,k1,longway0_r3030	# outside kptbl
	lw	k1,kptbl
	addu	k0,k1			# address of pte in kptbl
	.set	noreorder
	.set	noat
	lw	k0,0(k0)		# load desired pte
	mfc0	k1,C0_EPC		# get ready for rfe and fill load delay
	mtc0	k0,C0_TLBLO		# drop pte in tlb
	and	k0,PG_VR		# check valid bit and fill load delay
	c0	C0_WRITER		# random drop in into tlb
	beq	k0,zero,longway0_r3030	# panic if not valid
	nop
	.set	at
	.set	reorder
	lw	k0,kvmiss
	addu	k0,1
	sw	k0,kvmiss
	.set	noreorder
	.set 	noat
	lw	AT,exception_at_save
	j	k1
	c0	C0_RFE
	nop

	.set	at
longway0_r3030:
	lw	k1,rambo_clock_save
longway_r3030:
	.set	noat
	lw	AT,exception_at_save
#else FAST_KTLBMISS_HANDLER
	/*
	 * WARNING!!!!
	 * Address calculation here assumes that 16 bit offset on lw
	 * does NOT have sign bit set! This currently correct, as
	 * exception_k1_save has an absolute value (0x800003e4) - Rex
	 */
	lui	k0,exception_k1_save>>16
	sw	k1,+exception_k1_save&0xffff(k0)
	lw	k1,CLOCK_COUNT
	lui	k0,rambo_clock_save>>16
	sw	k1,+rambo_clock_save&0xffff(k0)
#endif FAST_KTLBMISS_HANDLER
/*
 * here we have - k1 rambo clock count (old stored in exception_k1_save)
 * 		- AT as was in program
 *		- k0 rubbish
 */
	/*
	 * WARNING!!!!
	 * Address calculation here assumes that 16 bit offset on lw
	 * does NOT have sign bit set!
	 */
	lui	k0,kstackflag>>16
	lw	k0,+kstackflag&0xffff(k0)
	nop
	beq	k0,zero,1f		# if kstackflag == 0 then we were not in kernel
	move	k0,AT			# BDSLOT: save at
/*
 *	Came from kernel space
 */
	lw	AT,multiuser
	nop
	beq	zero,AT,2f
	.set	at
	bgtu	sp,KERNELSTACK-8192+EF_SIZE,2f
	nop
	la	sp,KERNELSTACK-EF_SIZE
	mfc0	a3,C0_EPC
	mfc0	a2,C0_CAUSE
	PANIC("Kernel stack underflow Cause: 0x%x  EPC: 0x%x")
2:	sw	sp,EF_SP*4-EF_SIZE(sp)
	subu	sp,EF_SIZE
	/*
	 * This instruction stream can be cleaned up somewhat for write stalls,
	 * but for now, left as is so its readable when debugging
	 */
	.set	noat
	lw	AT,multiuser		# if we are not multiuser
	nop				# don't touch the u area
	beq	AT,zero,2f		# AT ALL
	nop

/*
 * k1 contains the current rambo time. If we are taking an interrupt
 * from a system call we sum up the time from when we started system
 * call processing (this is stored in rambo_at_eend) to the current
 * time (stored in k1) and add this to the accumulated system time
 * (stored in R_STIME). This code here co-operates with code in
 * syscall() and exception_exit() to implement the following scheme:
 *
 *				 exception_r3030	exception_exit
 *	R_LEVEL  2	syscall	 +______________________+
 *		 1	+________|			|_______+syscall exit
 *		 0 _____|					|________
 *
 * on entry to syscall we set R_LEVEL to 1 and start the timer. If
 * no interrupts or exceptions occur then we sum the U_STIME and
 * clear R_LEVEL at the end of syscall.  If an exception/interrupt
 * occurs during this time we will stop the count, and sum the time
 * onto U_STIME. If we take an interrupt from a place other than
 * syscall then we set the counter to two immediately. The counter
 * gets incremented in exception_r3030 and decremented in
 * exception_exit.  We use a two level counter scheme. We have
 * U_UTIME (U_STIME) for the current number of user (system) clock
 * ticks that have been used. This used to be incremented in clock.c
 * but we now do it here. We also have R_UTIME (R_STIME) which is
 * the current number of rambo ticks IN ADDITION to the number of
 * clock ticks. We add the number of rambo ticks to R_UTIME (R_STIME)
 * and if it is bigger than a clock tick we add a clock tick to
 * the clock tick counters.  
 */

	lw	AT,u+R_LEVEL
	nop
	addiu	AT,-1
	bne	zero,AT,3f
	nop
	li	AT,2
	sw	AT,u+R_LEVEL
	lw	AT,rambo_at_eend
	nop
	subu	k1,AT
	lw	AT,u+R_STIME
	nop
	addu	AT,k1
	lw	k1,rambo_clock_delay
	sw	AT,u+R_STIME
/*
 * we want to branch here, but we are already using AT (and k1 is the temp)
 *	bltu	AT,k1,2f		# if (AT < k1) goto 2 forward
 */
	sltu	k1,AT,k1
	bne	k1,zero,2f
	lw	k1,rambo_clock_delay	# BDslot (partially)
	nop
	subu	AT,k1			# BDslot
	sw	AT,u+R_STIME
	nop
	lw	AT,u+U_STIME
	nop
	addiu	AT,1
	b	2f
	sw	AT,u+U_STIME		# BDslot
3:	/*
	 * check to see if R_LEVEL is greater than 0, if so then we are
	 * in a nested interrupt.
	 */
	blez	AT,2f			# not from a syscall
	addiu	AT,1
	b	2f
	sw	AT,u+R_LEVEL		# and save away
	/*
	 * Came from user mode or utlbmiss, initialize kernel stack
	 */
/*
 * if the cumulated rambo user time is greater than one tick then add a tick
 * to the accumulated user time and subtract a ticks' worth from the rambo_time
 */
1:	lw	AT,multiuser		# can we do this yet?
	sw	sp,KERNELSTACK-EF_SIZE+EF_SP*4
	beq	AT,zero,1f		# not multiuser so don't account
	lw	sp,rambo_at_eend	# get old rambo count
	lw	AT,u+R_UTIME		# get current rambo usertime
	subu	k1,sp			# sub from new count
	addu	AT,k1,AT		# add in what we used
	lw	k1,rambo_clock_delay
	sw	AT,u+R_UTIME		# and we'll save it back
/*
 * same problem as above, we have to branch, but have no spare registers.
 * here we burn k1, as we can restore it from rambo_clock_delay
 *	bltu	AT,k1,1f		# if (AT < k1) goto 1 forward
 */
	sltu	k1,AT,k1
	bne	k1,zero,1f
	lw	k1,rambo_clock_delay	# BDslot (partially)
	nop
	subu	k1,AT,k1
	sw	k1,u+R_UTIME
	lw	k1,u+U_UTIME
	nop
	addiu	k1,1
	sw	k1,u+U_UTIME
	.set	at
	.set	reorder
1:	la	sp,KERNELSTACK-EF_SIZE
	sw	gp,EF_GP*4(sp)
	la	gp,_gp
	sw	gp,kstackflag		# now on kernel stack (gp always != 0)
/*
 * merge again
 */
2:
	lw	k1,exception_k1_save
	sw	k0,EF_AT*4(sp)
	sw	k1,EF_K1*4(sp)		# in case we came from utlbmiss
	.set	noreorder
	sw	a0,EF_A0*4(sp)
	mfc0	a0,C0_EPC
	sw	a1,EF_A1*4(sp)
	sw	a2,EF_A2*4(sp)
	sw	a3,EF_A3*4(sp)
	mfc0	a3,C0_CAUSE
	sw	s0,EF_S0*4(sp)
#ifdef PIXIE
	sw	s4,EF_S4*4(sp)
	sw	fp,EF_FP*4(sp)
#endif PIXIE
	sw	a3,EF_CAUSE*4(sp)
	sw	ra,EF_RA*4(sp)
	mfc0	s0,C0_SR
	sw	a0,EF_EPC*4(sp)
	.set	reorder
	sw	a0,EF_SAVEDPC*4(sp)	# to stop stack trace 

	EXC_SAVE_PIXIE_STATE

	/*
	 * Check for cache parity errors (r2000/r3000 only)
	 * If the error bit is set, clear it and increment a counter.
	 * Register assumptions:
	 *	at -- available for the assembler (saved above)
	 *	a0 -- already saved and can be used
	 *	s0 -- status register at time of exception
	 */
	.set	noreorder
	sll	a0,s0,31-20	# is this set in the saved sr?
	bgez	a0,3f		# if sign bit not set, not error
	mtc0	s0,C0_SR	# BDslot:  this will clear SR_PE if it is set
	lw	a0,cpe_count
	nop
	addiu	a0,1		# increment the cache parity error counter
	sw	a0,cpe_count
3:
	.set	reorder

	/*
	 * Dispatch to appropriate exception handler
	 * Register setup:
	 *	s0 -- SR register at time of exception
	 *	a0 -- exception frame pointer
	 *	a1 -- cause code
	 *	a3 -- cause register
	 */
	and	a1,a3,CAUSE_EXCMASK
	lw	a2,causevec(a1)
	move	a0,sp
	.set	noreorder
	j	a2
	sw	s0,EF_SR*4(sp)
	.set	reorder
	EXPORT(eexception_r3030)
	END(exception_r3030)

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
	sw	zero,ioc_retry_count
	sw	zero,ioc_retry_last_epc
	sw	zero,ioc_retry_last_badvaddr
	j	ra
	END(reset_ioc_retry_count)

/*
 * VEC_int -- interrupt handler
 */
#ifdef ASM_FIXED
VECTOR(VEC_int, M_EXCEPT|M_TFISAVE)
#else
VECTOR(VEC_int, M_EXCSAVE)
#endif
	.set	noreorder
	li	k0,SR_IEC|SR_IMASK8	# enable, but mask all interrupts
	MTC0(k0)
	.set	reorder
	jal	tfi_save
	/*
	 * If this is a floating-point interrupt then we may need "all" the
	 * user's register values in case we need to emulate a branch
	 * instruction if we are in a branch delay slot.
	 */
	andi	a2,a3,CAUSE_IP_FPINTR
	beq	a2,zero,1f
	sw	s1,EF_S1*4(sp)
	sw	s2,EF_S2*4(sp)
	sw	s3,EF_S3*4(sp)
#ifndef PIXIE
	sw	s4,EF_S4*4(sp)
#endif
	sw	s5,EF_S5*4(sp)
	sw	s6,EF_S6*4(sp)
	sw	s7,EF_S7*4(sp)
1:
	move	a2,s0			# sr is arg3
	jal	intr			# intr(ef_ptr, code, sr, cause)
	jal	tfi_restore
	b	exception_exit
	END(VEC_int)

/*
 * TLB mod.
 * Could enable interrupts here if we were so inclined....
 */
#ifdef ASM_FIXED
VECTOR(VEC_tlbmod, M_EXCEPT|M_TFISAVE)
#else
VECTOR(VEC_tlbmod, M_EXCSAVE)
#endif
	.set	noreorder
	mfc0	a2,C0_BADVADDR		# arg3 is bad vaddr
	nop
	sw	a2,EF_BADVADDR*4(sp)	# save in case of trap (ugh!)
	.set	reorder
	jal	tfi_save
	jal	tlbmod			# tlbmod(ef_ptr, code, vaddr, cause)
	la	ra,exception_exit	# fake jal with less nops
					# if we had reloc-reloc, 1 cycle
	beq	v0,zero,tfi_restore	# zero if legal to modify
	.set	noreorder
	nop				# BDSLOT
	or	a0,s0,SR_IEC		# enable interrupts
	MTC0(a0)
	move	a1,v0			# move software exception code
	move	a0,sp			# restore ep since tlbmod can trash
	.set	reorder
	lw	a3,EF_CAUSE*4(sp)	# restore cause since tlbmod can trash
	b	soft_trap		# and handle as trap
	END(VEC_tlbmod)

/*
 * TLB miss. 
 * Handles TLBMiss Read and TLBMiss Write
 * Could enable interrupts here if we were so inclined....
 */
#ifdef ASM_FIXED
VECTOR(VEC_tlbmiss, M_EXCEPT|M_TFISAVE)
#else
VECTOR(VEC_tlbmiss, M_EXCSAVE)
#endif
#ifdef R6000
	/*
	 *  If we came from utlbmiss, then the original t0..t7 were saved
	 *  in the utlbmiss savearea.  Retrieve them now, so tfi_save can
	 *  save them on the stack where they belong.
	 */
	lw	a2,EF_EPC*4(sp)		# exception PC
	la	k0,utlbmiss_r6000	# is EPC > start of utlbmiss ?
	sltu	k1,a2,k0
	bne	k1,zero,2f
	la	k0,eutlbmiss_r6000
	sltu	k1,a2,k0		# is EPC < end of utlbmiss ?
	beq	k1,zero,2f		# !=0 means "utlbmiss pending"
	lw	k0,utlbmiss_save_AT
	lw	t0,utlbmiss_save_t0	# restore t0..t7
	lw	t1,utlbmiss_save_t1
	lw	t2,utlbmiss_save_t2
	lw	t3,utlbmiss_save_t3
	lw	t4,utlbmiss_save_t4
	lw	t5,utlbmiss_save_t5
	lw	t6,utlbmiss_save_t6
	lw	t7,utlbmiss_save_t7
	sw	k0,EF_AT*4(sp)		# and re-save the AT register
2:
#endif R6000
	.set	noreorder
	jal	tfi_save		# now save v0..v1, t0..t9, etc.
	nop				# (BDSLOT)
	mfc0	a2,C0_BADVADDR		# arg3 is bad vaddr
	nop
	.set	reorder
	sw	a2,EF_BADVADDR*4(sp)	# save in case of trap (ugh!)
	jal	tlbmiss			# tlbmiss(ef_ptr, code, vaddr, cause)
	lw	s0,EF_SR*4(sp)		# tlbmiss can alter return SR
	beq	v0,zero,1f		# zero if accessable
	.set	noreorder
	or	a0,s0,SR_IEC		# enable interrupts
	MTC0(a0)
	move	a1,v0			# software exception code
	move	a0,sp			# restore ep since tlbmiss can trash
	.set	reorder
	lw	a3,EF_CAUSE*4(sp)	# restore cause since tlbmiss can trash
	b	soft_trap		# handle as trap

1:	la	ra,exception_exit	# 2 cycles, but 1 fills delay slot
	b	tfi_restore
	END(VEC_tlbmiss)

/*
 * VEC_dblword_noncached
 * Handles EXC_DBL_NC, an attempt to perform a doubleword load or store to
 * uncached space.
 */
VECTOR(VEC_dblword_noncached, M_EXCEPT)
	.set	noreorder
	mfc0	a2,C0_BADVADDR
	b	VEC_trap
	sw	a2,EF_BADVADDR*4(sp)	# (BDSLOT)
	.set	reorder
	END(VEC_dblword_noncached)

/*
 * VEC_addrerr
 * Handles AdrErrRead, AdrErrWrite
 */
VECTOR(VEC_addrerr, M_EXCEPT)
	.set	noreorder
	mfc0	a2,C0_BADVADDR
	b	VEC_trap
	sw	a2,EF_BADVADDR*4(sp)	# (BDSLOT)
	.set	reorder
	END(VEC_addrerr)

/*
 * VEC_ibe
 * Handles Instruction Bus Errors
 */
VECTOR(VEC_ibe, M_EXCEPT)
#ifdef R6000
	jal	load_pid_sync
#endif R6000
	.set	noreorder
	mfc0	ra,C0_CAUSE
	mfc0	a2,C0_EPC
	.set	reorder
	bgez	ra,1f		# BD bit not set
	addu	a2,4		# point at BD slot
	sw	a2,EF_BADVADDR*4(sp) # ibe's occur at pc
1:	b	VEC_trap
	END(VEC_ibe)

/*
 * VEC_dbe
 * Handles Data Bus Errors
 */
VECTOR(VEC_dbe, M_EXCEPT)
#ifdef R6000
	jal	load_pid_sync
#endif R6000
	.set	noreorder
	mfc0	a2,C0_BADVADDR
	b	VEC_trap
	sw	a2,EF_BADVADDR*4(sp)	# (BDSLOT)
	/*
	 * trap will calculate appropriate badvaddr if user fault
	 */
	.set	reorder
	END(VEC_dbe)

/*
 * TRAP
 * Illegal Instruction, and Overflow.
 * Also handles software exceptions raised by tlbmod and tlbmiss,
 * NOTE: tlbmod and tlbmiss replace the original exception code with
 * an appropriate software exception code.
 */
#define	M_TRAP		+(M_S1|M_S2|M_S3|M_S4|M_S5|M_S6|M_S7)
#ifdef ASM_FIXED
VECTOR(VEC_trap, M_EXCEPT|M_TFISAVE|M_TRAP)
#else
VECTOR(VEC_trap, M_TRAPSAVE)
#endif
	lw	a2,machine_type		# save far/fid
	beq	a2,BRDTYPE_M180,87f
	bne	a2,BRDTYPE_R2400,86f
87:	lhu	a2,FID|K1BASE
	sh	a2,saved_fid
	lw	a2,FAR|K1BASE
	sw	a2,saved_far
	b	84f
86:	bne	a2,BRDTYPE_R6300,84f
	beq	a1,EXC_DBE,1f
	bne	a1,EXC_IBE,84f
1:
	/* R6000 DBE or IBE exception  --  need to reset IOA state? */
	jal	tfi_save		# let us use some registers
	move	t9,a0			# save a0
	.set	noreorder
	mfc0	a0,C0_BADVADDR		# valid on R6000
	.set	reorder
	jal	get_ioa_ctlspace_addr	# get CtlSpace vaddr of this IOA
	move	a0,t9			# restore a0
	beq	v0,zero,85f		# is this really an IOA-GBA address?
	.set	noreorder
	li	t2,IOC_BUSY_RESET
	sw	t2,IOA_ERRORINFO_REG(v0)# clear BUSY state
85:	or	a2,s0,SR_IEC		# enable interrupts
#ifdef	DOINTDEBUG
	MTC0(a2)
	j	soft_trap
	nop
#else	/* DOINTDEBUG */
	j	soft_trap
	mtc0	a2,C0_SR		# (BDSLOT)
#endif	/* DOINTDEBUG */
	.set	reorder
84:
	.set	noreorder
	or	a2,s0,SR_IEC		# enable interrupts
	MTC0(a2)
	.set	reorder
	jal	tfi_save
soft_trap:				# (from tlbmod / tlbmiss)
	/*
	 * Save rest of state for debuggers
	 * ENTRY CONDITIONS: interrupts enabled, a1 contains software
	 * exception code
	 */
	sw	s1,EF_S1*4(sp)
	sw	s2,EF_S2*4(sp)
	sw	s3,EF_S3*4(sp)
	move	a2,s0
#ifndef PIXIE
	sw	s4,EF_S4*4(sp)
#endif
	sw	s5,EF_S5*4(sp)
	sw	s6,EF_S6*4(sp)
	sw	s7,EF_S7*4(sp)
	jal	trap			# trap(ef_ptr, code, sr, cause)
trap_return:
	lw	s1,EF_S1*4(sp)
	lw	s2,EF_S2*4(sp)
	lw	s3,EF_S3*4(sp)
#ifndef PIXIE
	lw	s4,EF_S4*4(sp)
#endif
	lw	s5,EF_S5*4(sp)
	lw	s6,EF_S6*4(sp)
	lw	s7,EF_S7*4(sp)
	jal	tfi_restore
	b	exception_exit
	END(VEC_trap)

/*
 * VEC_nofault -- handles nofault exceptions early on in system initialization
 * before VEC_trap is usable.
 */
#ifdef ASM_FIXED
VECTOR(VEC_nofault, M_EXCEPT|M_TFISAVE)
#else
VECTOR(VEC_nofault, M_EXCSAVE)
#endif
	jal	tfi_save
	move	a2,s0
	jal	trap_nofault		# trap_nofault(ef_ptr, code, sr, cause)
	jal	tfi_restore
	b	exception_exit
	END(VEC_nofault)

/*
 * Syscall
 * NOTE: v0, and, v1 must get restored on exit from syscall!!
 */
#define	M_SYSCALL	+(M_V0|M_V1|M_FP)
#ifdef ASM_FIXED
VECTOR(VEC_syscall, M_EXCEPT|M_SYSCALL)
#else
VECTOR(VEC_syscall, M_SYSCALLSAVE)
#endif
	.set	noreorder
	or	a1,s0,SR_IEC		# enable interrupts
	MTC0(a1)
	.set	reorder
	sw	v0,EF_V0*4(sp)		# u_rval1
	sw	v1,EF_V1*4(sp)		# u_rval2
#ifndef PIXIE
	sw	fp,EF_FP*4(sp)		# for debuggers
#endif
	/*
	 * Save rest of state for signal handlers
	 * Callee save regs are all that matters because that's all that
	 * matters accross a procedure call boundary.
	 * Signals return through trap interface so it will restore
	 * callee save regs on return from signal.
	 */
	sw	s1,EF_S1*4(sp)
	sw	s2,EF_S2*4(sp)
	sw	s3,EF_S3*4(sp)
	move	a1,v0			# arg2 -- syscall number
#ifndef PIXIE
	sw	s4,EF_S4*4(sp)
#endif
	sw	s5,EF_S5*4(sp)
	sw	s6,EF_S6*4(sp)
	sw	s7,EF_S7*4(sp)
	move	a2,s0			# arg3 -- sr
	jal	syscall			# syscall(ef_ptr, sysnum, sr, cause)
	bne	v0,zero,trap_return	# If syscall() returns non-zero, then
					# return through the trap machanism.
	lw	v0,EF_V0*4(sp)		# u_rval1
	lw	v1,EF_V1*4(sp)		# u_rval2
#ifndef PIXIE
	lw	fp,EF_FP*4(sp)		# for debuggers
#endif
	b	exception_exit
	END(VEC_syscall)

/*
 * Breakpoint -- determine if breakpoint is for prom monitor, else
 * call trap.
 */
VECTOR(VEC_breakpoint, M_EXCEPT)
	.set	noreorder
	mfc0	k1,C0_CAUSE
	lw	a2,EF_EPC*4(sp)
	and	k1,CAUSE_BD
	.set	reorder
	beq	k1,zero,1f
	addu	a2,4				# advance pc to bdslot
1:
	lw	k0,0(a2)			# read faulting instruction
						#  (might tlbmiss!)
	lw	k1,kernelbp			# what a kernel bp looks like
	bne	k0,k1,2f			# not a kernel bp inst
	lw	k0,+RB_BPADDR			# address of breakpoint handler
	beq	k0,zero,2f			# special handler not active
	and	k1,s0,SR_KUP			# kernel bp executed by user?
	beq	k1,zero,4f			#  yes, so treat as generic bp
2:
	lw	k0,0(a2)			# reload faulting instr again
	/*
	 * Check to see if there is a branch delay slot emulation taking place
	 * which is indicated by a non-zero value in PCB_BD_RA (left there by
	 * emulate_instr() ).  If this is the case go on to check for the two
	 * possible break instructions that emulate_instr() laid down.  If it
	 * is one of those two break instructions set the resulting pc and
	 * branch back to the caller of emulate_instr().  See emulate_instr()
	 * for the interface of how and where all this happens.
	 */
	lw	a2,u+PCB_BD_RA
	beq	a2,zero,VEC_trap
	lw	k1,bd_nottaken_bp	# check for the not taken branch bp
	bne	k0,k1,3f
	.set	noreorder
	nop				# BDSLOT
	or	a3,s0,SR_IEC		# enable interrupts
	MTC0(a3)
	.set	reorder
	sw	zero,u+PCB_BD_RA	# clear the branch delay emulation
	lw	a3,u+PCB_BD_EPC		# the resulting pc in this case is just
	addu	a3,8			#  the pc of the next instruction
	j	a2			# return to caller of emulate_instr()

bd_nottaken_bp:
	break	BRK_BD_NOTTAKEN

3:	lw	k1,bd_taken_bp		# check for the taken branch bp
	bne	k0,k1,VEC_trap
	.set	noreorder
	nop				# BDSLOT
	or	a3,s0,SR_IEC		# enable interrupts
	MTC0(a3)
	.set	reorder
	sw	zero,u+PCB_BD_RA	# clear the branch delay emulation
	lw	a3,u+PCB_BD_EPC		# the resulting pc in this case is the
	lw	a1,u+PCB_BD_INSTR	#  the target of the emulated branch
	sll	a1,16			#  so add the sign extended offset to
	sra	a1,16-2			#  branch's pc for the resulting pc
	addu	a3,a1
	addu	a3,4
	j	a2			# return to caller of emulate_instr()

bd_taken_bp:
	break	BRK_BD_TAKEN

4:
	/* here we are going to jump to the prom break point handler
	 */
	/*
	 * for the R6000:
	 * dbgmon is going to "freeze" the SBC Count (i.e., save and restore ),
	 * which means the kernel's notion of time will fall behind the IOC's
	 * time-of-day chip.  We don't want verify_time() getting upset by all
	 * this and adjusting the CPU hz calculations, so set a flag that
	 * this has happened.
	 *
	 * for the RX3030 the same thing occurs, so in all cases we save the
	 * fact that the accurate version of time has just gone wacky.
	 */
	sw	gp,remote_bp_encountered
	and	k0,s0,SR_KUP
	beq	k0,zero,5f			# breakpoint from kernel mode
	sw	zero,kstackflag
	lw	gp,EF_GP*4(sp)
5:	lw	a0,EF_A0*4(sp)
	lw	a1,EF_A1*4(sp)
	lw	a2,EF_A2*4(sp)
	lw	a3,EF_A3*4(sp)
	lw	s0,EF_S0*4(sp)
#ifdef PIXIE
	lw	s4,EF_S4*4(sp)
	lw	fp,EF_FP*4(sp)
#endif PIXIE
	.set	noreorder
	lw	k1,EF_SR*4(sp)
	lw	ra,EF_RA*4(sp)
	MTC0(k1)
	lw	k0,EF_AT*4(sp)		# save AT in k0
	lw	k1,EF_K1*4(sp)
	.set	reorder
	lw	sp,EF_SP*4(sp)

	.set	noat
	lw	AT,+RB_BPADDR		# address of breakpoint handler
	j	AT			# enter breakpoint handler
	.set	at

kernelbp:
	break	BRK_KERNELBP
	END(VEC_breakpoint)

EXPORT(sstepbp)
	break	BRK_SSTEPBP


/*
 *  Machine-Check exception
 */
VECTOR(VEC_machine_check, M_EXCEPT)
	.set	noreorder
	mfc0	a2,C0_ERROR		# currently:  all Ignored, some Pending
	lw	k0,cp0_error_imask	# see what errors we want to ignore
	mtc0	a2,C0_ERROR		# clear Pending errors by writing 1's
	and	a2,C0_ERROR_MASK	# these are the pending errors
	or	a2,k0			# and arrive at a true reflection of
	sw	a2,cp0_error_check	#  Ignore and Pending bits
	mfc0	a2,C0_BADVADDR
	b	VEC_trap
	sw	a2,EF_BADVADDR*4(sp)	# (BDSLOT)
	.set	reorder
	END(VEC_machine_check)

/*
 *  reset_cp0_error (new_imask)
 */
LEAF(reset_cp0_error)
	.set	noreorder
	j	ra
#ifdef R6000
	mtc0	a0,C0_ERROR
#else
	nop
#endif R6000
	.set	reorder
	END(reset_cp0_error)

/*
 * Coprocessor unusable fault
 */
VECTOR(VEC_cpfault, M_EXCEPT)
	and	a1,s0,SR_KUP
	beq	a1,zero,coproc_panic	# kernel tried to use coprocessor

	and	a1,a3,CAUSE_CEMASK
	srl	a1,CAUSE_CESHIFT
	bne	a1,1,coproc_not1	# not coproc 1

	and	a1,s0,SR_IBIT_FPINTR
	bne	a1,zero,1f		# fp interrupts must be enabled!
	PANIC("VEC_cpfault when FP interrupts disabled")
1:
#ifdef R6000_BUG_FPINTR
	/*
	 * In the case of the R6000/6010, if the FP instruction and its
	 * operands are such that the R6010 would generate an fp interrupt,
	 * then the Cause register will have the FP interrupt bit set.  We
	 * can assure ourselves that this FP interrupt is for the instruction
	 * which triggered the "coprocessor unusable" exception because the
	 * kernel guarantees (we hope!) that (1) when in user mode the SR will
	 * always enable FP interrupts, and (2) the kernel never executes an
	 * FP instruction.
	 * Therefore, if the Cause says an FP interrupt is pending, then we
	 * need to clear that interrupt before we can continue.  We presume
	 * that the FPC state (including the CSR) has already been unloaded
	 * prior to the context-switch.
	 */
	lw	a1,r6000_bug_fpintr	# if no workaround is needed,
	lw	a2,fptype_word		# if there is no FPC,
	beq	a1,zero,1f		#  then don't do anything
	beq	a2,zero,1f
	or	a1,s0,SR_CU1		# otherwise, enable Cp1
	.set	noreorder
	mtc0	a1,C0_SR		# only changing coprocessor enables
	nop				# wait two cycles for it to take effect
	nop
	ctc1	zero,fpc_csr		# and clear any interrupts
	.set	reorder
1:
#endif R6000_BUG_FPINTR
	.set	noreorder
	or	a1,s0,SR_IEC		# enable interrupts
	mtc0	a1,C0_SR		# we are not changing ipl
	.set	reorder

	/*
	 * This is the floating-point coprocessor (coprocessor 1) unusable
	 * fault handling code.  During auto configuration fptype_word
	 * is loaded from the floating-point coprocessor revision word or
	 * zeroed if there is no floating-point coprocessor.
	 */
	sw	gp,u+PCB_OWNEDFP	# mark that fp has been touched
	lw	a2,fptype_word		# check for what type of fp coproc
	bne	a2,zero,1f
	j	softfp_unusable		# no fp coproc (goto fp software)
1:
	or	a1,s0,SR_CU1		# enable coproc 1 for the user process
	sw	a1,EF_SR*4(sp)

	lw	a2,fpowner		# current coproc 1 (fp) owner
	lw	a1,u+U_PROCP		# current process executing
	beq	a2,a1,coproc_done	# owned by the current process

	.set	noreorder
        /* Here we want to turn on CU1, must make sure that we don't
         * turn off interrupts in the process (which we so painstakingly
         * turned on above).
         */
	or	a3,s0,SR_CU1|SR_IEC	# enable fp and interrupts
	MTC0(a3)
	.set	reorder
	beq	a2,zero,fp_notowned	# coproc 1 not currently owned

	/*
	 * Owned by someone other than the current process.
	 * Save state (into the fpowner) before taking possession.
	 */
#if notdef
	lw	a3,P_FLAG(a2)
	and	a3,SLOAD
	bne	a3,zero,1f
	PANIC("savecp swapped out")
1:
#endif notdef
	lw	a3,P_UBPTBL(a2)		# u page ptes
	srl	a3,PTE_PNUMSHFT		# right-justify pfn
	sll	a3,PNUMSHFT		#  then shift back to a virtual addr
	or	a3,K0BASE		#   and form a K0 address

/*
 *  NOTE:  see the related comments prior to RESTCP1REG().
 */
#define SAVECP1REG(reg) \
	swc1	$f/**/reg,PCB_FPREGS+reg*4(a3)

	/*
	 * The floating-point control and status register must be
	 * read first to force all fp operations to complete and insure
	 * that all fp interrupts for this process have been delivered
	 */
	.set	noreorder
	cfc1	a2,fpc_csr
	nop
	sw	a2,PCB_FPC_CSR(a3)
	cfc1	a2,fpc_eir
	nop
	sw	a2,PCB_FPC_EIR(a3)
#ifdef R6000
	/*
	 * Protect the kernel from stumbling over FP parity errors by
	 * inhibiting such errors as we unload the FP registers.
	 */
	li	a2,(FPPARITY_IIB | FPPARITY_IRF)
	ctc1	a2,fpc_parity
	nop
	nop
#endif R6000
	SAVECP1REG(31); SAVECP1REG(30); SAVECP1REG(29); SAVECP1REG(28)
	SAVECP1REG(27); SAVECP1REG(26); SAVECP1REG(25); SAVECP1REG(24)
	SAVECP1REG(23); SAVECP1REG(22); SAVECP1REG(21); SAVECP1REG(20)
	SAVECP1REG(19); SAVECP1REG(18); SAVECP1REG(17); SAVECP1REG(16)
	SAVECP1REG(15); SAVECP1REG(14); SAVECP1REG(13); SAVECP1REG(12)
	SAVECP1REG(11); SAVECP1REG(10); SAVECP1REG(9);  SAVECP1REG(8)
	SAVECP1REG(7);  SAVECP1REG(6);  SAVECP1REG(5);  SAVECP1REG(4)
	SAVECP1REG(3);  SAVECP1REG(2);  SAVECP1REG(1);  SAVECP1REG(0)

fp_notowned:

/*
 * NOTE:  On a Mips2 machine we would like to use LDC1 (and SDC1) to save
 *	  and restore FP registers to the PCB savearea, but these doubleword
 *	  instructions have the unfortunate side-effect of swapping the words
 *	  between the pair of FP registers and the pair of memory words.
 *	  This means that FP registers N/N+1 really go into memory words
 *	  M+1/M, which works fine for blind save/restore, but not so well for
 *	  parts of the kernel which expect register N to live in memory word M.
 */
#define RESTCP1REG(reg) \
	lwc1	$f/**/reg,PCB_FPREGS+reg*4(a3)

	.set	noreorder
	or	a2,s0,SR_CU1
	MTC0(a2)			# disable interrupts, fp enabled
	.set	reorder
	/*
	 * restore coprocessor state (from the current process)
	 */
	/* lw	a3,P_UBPTBL(a0)	*/	# u page ptes
	/* and	a3,PG_ADDR	*/	# isolate physical address of u page
#ifdef R6000
	ctc1	zero,fpc_parity		# make sure parity is enabled
#endif R6000
	li	a3,u
	RESTCP1REG(0);  RESTCP1REG(1);  RESTCP1REG(2);  RESTCP1REG(3)
	RESTCP1REG(4);  RESTCP1REG(5);  RESTCP1REG(6);  RESTCP1REG(7)
	RESTCP1REG(8);  RESTCP1REG(9);  RESTCP1REG(10); RESTCP1REG(11)
	RESTCP1REG(12); RESTCP1REG(13); RESTCP1REG(14); RESTCP1REG(15) 
	RESTCP1REG(16); RESTCP1REG(17); RESTCP1REG(18); RESTCP1REG(19)
	RESTCP1REG(20); RESTCP1REG(21); RESTCP1REG(22); RESTCP1REG(23)
	RESTCP1REG(24); RESTCP1REG(25); RESTCP1REG(26); RESTCP1REG(27)
	RESTCP1REG(28); RESTCP1REG(29); RESTCP1REG(30); RESTCP1REG(31)
	.set	noreorder
	ctc1	zero,fpc_csr
#ifndef R6000
	lw	a2,PCB_FPC_EIR(a3)
	nop
	ctc1	a2,fpc_eir
#endif !R6000
	lw	a2,PCB_FPC_CSR(a3)
	nop
	ctc1	a2,fpc_csr
	sw	a1,fpowner		# we now own fp
coproc_done:
	.set	reorder
	b	exception_exit

coproc_not1:
	li	a1,SEXC_CPU		# handle as software trap
	b	VEC_trap		# not soft_trap, must save regs yet

coproc_panic:
	PANIC("kernel used coprocessor")
	END(VEC_cpfault)

/*
 * checkfp(procp, exiting)
 *	procp = proc pointer of process exiting or being swapped out.
 *	exiting = 1 if exiting.
 *	Called from exit and swapout to release FP ownership.
 */
LEAF(checkfp)
	lw	v0,fpowner
	bne	a0,v0,2f		# not owned by us, just return
	bne	a1,zero,1f		# exiting, don't save state
	lw	a3,fptype_word
	beq	a3,zero,1f		# no fp coprocessor

	/*
	 * The floating-point control and status register must be
	 * read first so to stop the floating-point coprocessor.
	 */
	.set	noreorder
	nop				# BDSLOT
	mfc0	v1,C0_SR		# enable coproc 1 for the kernel
	lw	a3,P_UBPTBL(a0)		# u page ptes
	or	v0,v1,SR_CU1		
	mtc0	v0,C0_SR		# add CP1 to permissions
	srl	a3,PTE_PNUMSHFT		#  right-justify pfn
	sll	a3,PNUMSHFT		#   and shift back to form virt addr
	cfc1	v0,fpc_csr
	or	a3,K0BASE		# change to virtual address
	sw	v0,PCB_FPC_CSR(a3)
	cfc1	v0,fpc_eir
	nop
	sw	v0,PCB_FPC_EIR(a3)
#ifdef R6000
	/*
	 * Protect the kernel from stumbling over FP parity errors by
	 * inhibiting such errors as we unload the FP registers.
	 */
	li	a2,(FPPARITY_IIB | FPPARITY_IRF)
	ctc1	a2,fpc_parity
	nop
	nop
#endif R6000
	SAVECP1REG(31); SAVECP1REG(30); SAVECP1REG(29); SAVECP1REG(28)
	SAVECP1REG(27); SAVECP1REG(26); SAVECP1REG(25); SAVECP1REG(24)
	SAVECP1REG(23); SAVECP1REG(22); SAVECP1REG(21); SAVECP1REG(20)
	SAVECP1REG(19); SAVECP1REG(18); SAVECP1REG(17); SAVECP1REG(16)
	SAVECP1REG(15); SAVECP1REG(14); SAVECP1REG(13); SAVECP1REG(12)
	SAVECP1REG(11); SAVECP1REG(10); SAVECP1REG(9);  SAVECP1REG(8)
	SAVECP1REG(7);  SAVECP1REG(6);  SAVECP1REG(5);  SAVECP1REG(4)
	SAVECP1REG(3);  SAVECP1REG(2);  SAVECP1REG(1);  SAVECP1REG(0)
#ifdef R6000
	/*
	 * If any FP parity errors have occured, then clean up the FP regs.
	 */
	cfc1	a2,fpc_parity
	li	v0,(FPPARITY_RF | FPPARITY_IB)
	nop
	and	a2,v0
	ctc1	zero,fpc_parity		# clean up the parity error register
	beq	a2,zero,1f
	nop
	mtc1 zero,$f0;   mtc1 zero,$f1;   mtc1 zero,$f2;   mtc1 zero,$f3
	mtc1 zero,$f4;   mtc1 zero,$f5;   mtc1 zero,$f6;   mtc1 zero,$f7
	mtc1 zero,$f8;   mtc1 zero,$f9;   mtc1 zero,$f10;  mtc1 zero,$f11
	mtc1 zero,$f12;  mtc1 zero,$f13;  mtc1 zero,$f14;  mtc1 zero,$f15
	mtc1 zero,$f16;  mtc1 zero,$f17;  mtc1 zero,$f18;  mtc1 zero,$f19
	mtc1 zero,$f20;  mtc1 zero,$f21;  mtc1 zero,$f22;  mtc1 zero,$f23
	mtc1 zero,$f24;  mtc1 zero,$f25;  mtc1 zero,$f26;  mtc1 zero,$f27
	mtc1 zero,$f28;  mtc1 zero,$f29;  mtc1 zero,$f30;  mtc1 zero,$f31
#endif R6000
	ctc1	zero,fpc_csr		# clear any pending interrupts
	mtc0	v1,C0_SR		# disable kernel fp access
	.set	reorder

1:	sw	zero,fpowner		# Mark FP as unowned
	lw	a1,u+U_PROCP
	bne	a1,a0,2f		# not current process
	lw	a1,KERNELSTACK-EF_SIZE+(4*EF_SR) # current user's sr
	and	a1,~SR_CU1		# clear fp coprocessor usable bit
	sw	a1,KERNELSTACK-EF_SIZE+(4*EF_SR)
	
2:	j	ra
	END(checkfp)

/*
 * tfi_save -- save enough state so that C routines can be called
 */
LEAF(tfi_save)
	sw	v0,EF_V0*4(sp)
	sw	v1,EF_V1*4(sp)
	sw	t0,EF_T0*4(sp)
	mflo	t0
	sw	t1,EF_T1*4(sp)
	mfhi	t1
	sw	t2,EF_T2*4(sp)
	sw	t3,EF_T3*4(sp)
	sw	t4,EF_T4*4(sp)
	sw	t5,EF_T5*4(sp)
	sw	t6,EF_T6*4(sp)
	sw	t7,EF_T7*4(sp)
	sw	t8,EF_T8*4(sp)
	sw	t9,EF_T9*4(sp)
#ifndef PIXIE
	sw	fp,EF_FP*4(sp)
#endif
	sw	t0,EF_MDLO*4(sp)
	sw	t1,EF_MDHI*4(sp)
	j	ra
	END(tfi_save)

/*
 * tfi_restore -- restore state saved by tfi_save
 */
LEAF(tfi_restore)
	lw	v0,EF_MDLO*4(sp)
	lw	v1,EF_MDHI*4(sp)
	mtlo	v0
	mthi	v1
	lw	v0,EF_V0*4(sp)
	lw	v1,EF_V1*4(sp)
	lw	t0,EF_T0*4(sp)
	lw	t1,EF_T1*4(sp)
	lw	t2,EF_T2*4(sp)
	lw	t3,EF_T3*4(sp)
	lw	t4,EF_T4*4(sp)
	lw	t5,EF_T5*4(sp)
	lw	t6,EF_T6*4(sp)
	lw	t7,EF_T7*4(sp)
	lw	t8,EF_T8*4(sp)
	lw	t9,EF_T9*4(sp)
#ifndef PIXIE
	lw	fp,EF_FP*4(sp)
#endif
	j	ra
	END(tfi_restore)

/*
 * End of exception processing.
 *
 * ENTRY CONDITIONS:
 *	s0 contains SR at time of exception
 */
VECTOR(exception_exit, M_EXCEPT)
	.set	noreorder
	MTC0(s0)				# disable interrupts for sure
	.set	reorder
	EXC_REST_PIXIE_STATE
	lw	a3,machine_type
	/*
	 * If we are returning to user mode, check to see if a resched is
	 * desired.  If so, fake a RESCHED cause bit and let trap save/restore
	 * our state for us.
	 */
	and	k0,s0,SR_KUP
	beq	k0,zero,2f			# returning to kernel mode
	lb	k0,runrun
	bne	k0,zero,3f			# no resched requested
	lw	k0,u+PCB_RESCHED		# check for softfp resched
	beq	k0,zero,1f
3:	move	a0,sp
	li	a1,SEXC_RESCHED			# software exception
	lw	a3,EF_CAUSE*4(sp)
	b	VEC_trap

1:
#ifdef R6000
	lw	k1,EF_SR*4(sp)			# if we're exiting to user and
	li	k0,SR_IMASK2			#  we're not enabling both FP
	and	k1,k0				#   and External ints,
	beq	k1,k0,1f			#    then Panic
	lw	a2,EF_SR*4(sp)
	PANIC("exception_exit to user with SR %x")
1:
	lw	a0,EF_IVECTMASK*4(sp)		# if we're exiting to user and
	li	a1,CSR_IVECTMASK_NONE		#  we're not enabling all ints,
	beq	a0,a1,1f			#   then Panic
	move	a2,a0
	PANIC("exception_exit to user with CSR_IVECTMASK %x")
1:
#endif R6000
#ifdef	DOINTDEBUG
/*
 * we have to check the interrupts here, but we cannot restore it till later.
 */
	.set	noreorder
	lw	a0,EF_SR*4(sp)
	lw	a1,ipl_special_mask
	add	sp,-68
	and	a1,a0
	sw	a0,12(sp)
	sw	a1,16(sp)
	sw	a2,20(sp)
	sw	a3,24(sp)
	sw	t0,28(sp)
	sw	t1,32(sp)
	sw	t2,36(sp)
	sw	t3,40(sp)
	sw	t4,44(sp)
	sw	t5,48(sp)
	sw	t6,52(sp)
	sw	t7,56(sp)
	sw	t8,60(sp)
	sw	t9,64(sp)
	mfc0	a0,C0_SR
	sw	ra,8(sp)
	move	a2,ra
	jal	intdebug
	add	sp,-20
	add	sp,20
	lw	ra,8(sp)
	lw	a0,12(sp)
	lw	a1,16(sp)
	lw	a2,20(sp)
	lw	a3,24(sp)
	lw	t0,28(sp)
	lw	t1,32(sp)
	lw	t2,36(sp)
	lw	t3,40(sp)
	lw	t4,44(sp)
	lw	t5,48(sp)
	lw	t6,52(sp)
	lw	t7,56(sp)
	lw	t8,60(sp)
	lw	t9,64(sp)
	add	sp,68
	.set	reorder
#endif	/* DOINTDEBUG */
	sw	zero,kstackflag			# switching to user stack
	lw	gp,EF_GP*4(sp)
2:
#ifdef R6000
	lw	a0,EF_IVECTMASK*4(sp)		# SBC IVectMask at entry
	sw	a0,CSR_IVECTMASK(zero)		# restore mask
#endif R6000
	lw	a0,EF_A0*4(sp)
	lw	a1,EF_A1*4(sp)
	lw	a2,EF_A2*4(sp)
	lw	s0,EF_S0*4(sp)
#ifdef PIXIE
	lw	s4,EF_S4*4(sp)
	lw	fp,EF_FP*4(sp)
#endif PIXIE
	lw	ra,EF_RA*4(sp)
	lw	k0,ipl_special_mask
	lw	k1,EF_SR*4(sp)
	.set	noreorder
	.set	noat
	addiu	AT,a3,-BRDTYPE_R3030		# LD slot
	lw	a3,EF_A3*4(sp)
	bne	AT,zero,1f
	and	k1,k1,k0			# BD slot
	.set	at
	.set	reorder
	lw	k0,CLOCK_COUNT
	sw	k0,rambo_at_eend
	.set	noreorder
 	.set	noat
1:	lw	AT,EF_AT*4(sp)
	lw	k0,EF_EPC*4(sp)
	lw	sp,EF_SP*4(sp)
	mtc0	k1,C0_SR			# we have already adjusted above
	j	k0
	c0	C0_RFE
 	.set	at
	.set	reorder
	END(exception_exit)

VECTOR(VEC_unexp, M_EXCEPT)
	PANIC("unexpected exception")
	END(VEC_unexp)

/*
 * This is a hack to make sure that someone doesn't
 * try to run a kernel w/o the -mfc0 flag on a 1.5.1 chip.
 * See os/machdep.c:coproc_find() for futher info.
 */
EXPORT(mfc0_start)
	.set	noreorder
	mfc0	v0,C0_SR
	.set	reorder
EXPORT(mfc0_end)
	
/*
 * get_cause: get current value of cause register
 */
LEAF(get_cause)
	.set	noreorder
	mfc0	v0,C0_CAUSE
	.set	reorder
	j	ra
	END(get_cause)

/*
 * reset_sr(ipl) -- reset SR 
 */
LEAF(reset_sr)
#ifdef	DOINTDEBUG
	.set	noreorder
	mfc0	v0,C0_SR
	MTC0(a0)
	j	ra
	nop
	.set	reorder
#else	/* DOINTDEBUG */
	.set	noreorder
	mfc0	v0,C0_SR
	j	ra
	mtc0	a0,C0_SR
	.set	reorder
#endif	/* DOINTDEBUG */
	END(reset_sr)

/*
 * badaddr(addr, len)
 *	check for bus error on read access to addr
 *	len is length of access (1=byte, 2=short, 4=long)
 */
BADADDRFRM=	(4*4)+4		# 4 arg saves plus a ra
NESTED(badaddr, BADADDRFRM, zero)
	.set	noreorder
	mfc0	t0,C0_SR
	MTC0(zero)
	.set	reorder
	subu	sp,BADADDRFRM
	sw	ra,BADADDRFRM-4(sp)
	li	v0,NF_BADADDR
	sw	v0,nofault

	bne	a1,1,1f
	lb	v0,0(a0)
	b	4f

1:	bne	a1,2,2f
	lh	v0,0(a0)
	b	4f

2:	bne	a1,4,3f
	lw	v0,0(a0)
	b	4f

3:	PANIC("baddaddr")

4:	sw	zero,nofault
	lw	ra,BADADDRFRM-4(sp)
	addu	sp,BADADDRFRM
	.set	noreorder
	MTC0(t0)
	j	ra
	move	v0,zero			# BDSLOT
	.set	reorder
	END(badaddr)

/*
 * bad_load_addr(addr, len, value)
 *	Load and return the value at the specified address, in anticipation of
 *	  a possible Data Bus Error.  This routine is identical to badaddr(),
 *	  except we also return the value we loaded.
 *	len is length of access (1=byte, 2=short, 4=long)
 */
NESTED(bad_load_addr, BADADDRFRM, zero)
	.set	noreorder
	mfc0	t0,C0_SR
	MTC0(zero)
	.set	reorder
	subu	sp,BADADDRFRM
	sw	ra,BADADDRFRM-4(sp)
	li	v0,NF_BADADDR
	sw	v0,nofault

	bne	a1,1,1f
	lb	v0,0(a0)
	sb	v0,(a2)
	b	4f

1:	bne	a1,2,2f
	lh	v0,0(a0)
	sh	v0,(a2)
	b	4f

2:	bne	a1,4,3f
	lw	v0,0(a0)
	sw	v0,(a2)
	b	4f

3:	PANIC("bad_load_addr")

4:	sw	zero,nofault
	lw	ra,BADADDRFRM-4(sp)
	addu	sp,BADADDRFRM
	.set	noreorder
	MTC0(t0)
	j	ra
	move	v0,zero			# BDSLOT
	.set	reorder
	END(bad_load_addr)

/*
 * wbadaddr(addr, len)
 *	check for bus error on write access to addr
 *	len is length of access (1=byte, 2=short, 4=long)
 */
NESTED(wbadaddr, BADADDRFRM, zero)
	subu	sp,BADADDRFRM
	sw	ra,BADADDRFRM-4(sp)
	.set	noreorder
	mfc0	t0,C0_SR
	MTC0(zero)
	.set	reorder
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,45f		# no err reg to clear
	beq	v0,BRDTYPE_R3030,45f		# no err reg to clear
	beq	v0,BRDTYPE_RB3125,41f
	bne	v0,BRDTYPE_R3200,42f

/* The earliest R3200 modules re-enable the register on read access
 * but production modules re-enable it on a write access.
 * Simplify life and do both.  This is hardly an efficiency problem.
 */
41:
	lw	zero,BWE_ADDR_R3200|K1BASE	# older modules need on read
	sw	zero,BWE_ADDR_R3200|K1BASE	# re-enable error reg
	b	45f
42:	beq	v0,BRDTYPE_M180,43f
	bne	v0,BRDTYPE_R2400,44f
43:	lw	zero,FAR|K1BASE
	b	45f
44:	lw	zero,SBE_ADDR|K1BASE		# make sure err reg enabled
45:
	li	v0,NF_BADADDR
	sw	v0,nofault

	bne	a1,1,1f
	sb	zero,0(a0)
	b	4f

1:	bne	a1,2,2f
	sh	zero,0(a0)
	b	4f

2:	bne	a1,4,3f
	sw	zero,0(a0)
	b	4f

3:	PANIC("wbaddaddr")

4:	jal	wbflush
	lw	v0,K1BASE
	.set	noreorder
	mfc0	t1,C0_CAUSE
	nop					# BDSLOT
	and	t1,CAUSE_IP8
	bne	t1,zero,baerror
	nop					# BDSLOT
	sw	zero,nofault
	MTC0(t0)
	lw	ra,BADADDRFRM-4(sp)
	addu	sp,BADADDRFRM
	j	ra
	move	v0,zero				# BDSLOT
	.set	reorder
	END(wbadaddr)

/*
 * wbadmemaddr(addr)
 *	check for address error on word access to addr
 *	Assumes addr points to RAM since trap is generated by read-back
 */
 NESTED(wbadmemaddr, BADADDRFRM, zero)
	.set	noreorder
	mfc0	t0,C0_SR
	MTC0(zero)
	.set	reorder
 	subu	sp,BADADDRFRM
 	sw	ra,BADADDRFRM-4(sp)
 	li	v0,NF_BADADDR
 	sw	v0,nofault
 	sw	zero,0(a0)		# store first to generate ECC
 	lw	v0,0(a0)		# load can cause sync DBE
 	sw	zero,nofault
 	lw	ra,BADADDRFRM-4(sp)
 	addu	sp,BADADDRFRM
	.set	noreorder
	MTC0(t0)
 	j	ra
 	move	v0,zero			# BDSLOT
 	.set	reorder
	END(wbadmemaddr)

/*
 * trap() nofault code comes here on bus errors when nofault == NF_BADADDR
 */
NESTED(baerror, BADADDRFRM, zero)
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,45f		# VEC_trap already did clear
	beq	v0,BRDTYPE_R3030,45f		# VEC_trap already did clear
	beq	v0,BRDTYPE_RB3125,41f
	bne	v0,BRDTYPE_R3200,42f
/* The earliest R3200 modules re-enable the register on read access
 * but production modules re-enable it on a write access.
 * Simplify life and do both.  This is hardly an efficiency problem.
 */
41:
	lw	zero,BWE_ADDR_R3200|K1BASE	# older modules need a read
	sw	zero,BWE_ADDR_R3200|K1BASE	# re-enable error reg
	b	45f
42:	beq	v0,BRDTYPE_M180,43f
	bne	v0,BRDTYPE_R2400,44f
43:	lhu	v0,FID|K1BASE
	sh	v0,saved_fid
	lw	v0,FAR|K1BASE
	sw	v0,saved_far
	b	45f
44:	lw	zero,SBE_ADDR|K1BASE
45:
	.set	noreorder
	MTC0(t0)
	lw	ra,BADADDRFRM-4(sp)
	addu	sp,BADADDRFRM
	j	ra
	li	v0,1				# BDSLOT: return 1
	.set	reorder
	END(baerror)

/*
 * ffs(word)
 * BEWARE: that C version of this routine that is distributed with 4.2
 * is incorrect!
 *
 * find first bit set in word (a la VAX instruction)
 * looks at low order bits first, lowest order bit is 1, highest bit is 32
 * no bits returns 0
 */

LEAF(ffs)
	move	v1,zero			# initial table offset
	and	v0,a0,0xffff		# check lower halfword
	bne	v0,zero,1f		# bits in lower halfword
	addu	v1,64			# table offset for halfword
	srl	a0,16			# check upper halfword
1:	and	v0,a0,0xff		# check lower byte of halfword
	bne	v0,zero,2f		# bits in lower byte
	addu	v1,32			# table offset for byte
	srl	a0,8			# check upper byte of halfword
2:	and	v0,a0,0xf		# check lower nibble
	bne	v0,zero,3f		# bits in lower nibble
	addu	v1,16			# table offset for nibble
	srl	v0,a0,4			# check upper nibble
	and	v0,0xf			# isolate lower nibble
3:	addu	v1,v0			# total table offset
	lbu	v0,ffstbl(v1)		# load bit number from table
	j	ra
	END(ffs)

	.data
#define NIBBLE(x) \
	.byte	0,       1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	3+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	4+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	3+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4
ffstbl:
	NIBBLE(0)
	NIBBLE(1)
	NIBBLE(2)
	NIBBLE(3)
	NIBBLE(4)
	NIBBLE(5)
	NIBBLE(6)
	NIBBLE(7)

	.text

/*
 * ffintr(cause_register) -- find first bit set in interrupt pending byte
 * bits are numbered as 8 most significant to 1 least significant,
 * search starts from most significant end, returns 0 in no bits set
 */
LEAF(ffintr)
	and	v0,a0,CAUSE_IPMASK
	srl	a0,v0,CAUSE_IPSHIFT+4	# shift to high nibble of IPEND bits
	bne	a0,zero,1f		# bits set in high nibble
	srl	a0,v0,CAUSE_IPSHIFT	# get 2nd nibble right
	add	a0,16			# to get to 2nd half of table
1:	lbu	v0,ffitbl(a0)		# get value from table
	j	ra
	END(ffintr)

	.data
ffitbl:
	.byte 0,5,6,6,7,7,7,7,8,8,8,8,8,8,8,8
	.byte 0,1,2,2,3,3,3,3,4,4,4,4,4,4,4,4
	.text


/*
 * scanc(count, cp, table, mask)
 * Like VAX instruction
 */
LEAF(scanc)
	move	v0,a0
	b	2f

1:	subu	v0,1		# decr count
2:	beq	v0,zero,3f	# count exhausted
	lbu	v1,0(a1)	# get char at cp
	addu	a1,1		# incr cp
	addu	t8,a2,v1	# offset into table
	lbu	t9,0(t8)	# load table entry
	and	t9,a3		# mask table entry
	beq	t9,zero,1b	# masked bit set
3:	j	ra
	END(scanc)

/*
 * Xchecksum(addr, len, prevcksum)
 *
 * Calculates a 16 bit ones-complement checksum.
 * Note that for a big-endian machine, this routine always adds even
 * address bytes to the high order 8 bits of the 16 bit checksum and
 * odd address bytes are added to the low order 8 bits of the 16 bit checksum.
 * For little-endian machines, this routine always adds even address bytes
 * to the low order 8 bits of the 16 bit checksum and the odd address bytes
 * to the high order 8 bits of the 16 bit checksum.
 */
LEAF(Xchecksum)
	move	v0,a2		# copy previous checksum
	beq	a1,zero,4f	# count exhausted
	and	v1,a0,1
	beq	v1,zero,2f	# already on a halfword boundry
	lbu	t8,0(a0)
	addu	a0,1
#ifdef MIPSEL
	sll	t8,8
#endif MIPSEL
	addu	v0,t8
	subu	a1,1
	b	2f

1:	lhu	t8,0(a0)
	addu	a0,2
	addu	v0,t8
	subu	a1,2
2:	bge	a1,2,1b
	beq	a1,zero,3f	# no trailing byte
	lbu	t8,0(a0)
#ifdef MIPSEB
	sll	t8,8
#endif MIPSEB
	addu	v0,t8
3:	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff
4:	j	ra
	END(Xchecksum)

/*
 * nuxi_s and nuxi_l -- byte swap short and long
 */
LEAF(nuxi_s)			# a0 = ??ab
	srl	v0,a0,8		# v0 = 0??a
	and	v0,0xff		# v0 = 000a
	sll	v1,a0,8		# v1 = ?ab0
	or	v0,v1		# v0 = ?aba
	and	v0,0xffff	# v0 = 00ba
	j	ra
	END(nuxi_s)

LEAF(nuxi_l)			# a0 = abcd
	sll	v0,a0,24	# v0 = d000
	srl	v1,a0,24	# v1 = 000a
	or	v0,v0,v1	# v0 = d00a
	and	v1,a0,0xff00	# v1 = 00c0
	sll	v1,v1,8		# v1 = 0c00
	or	v0,v0,v1	# v0 = dc0a
	srl	v1,a0,8		# v1 = 0abc
	and	v1,a0,0xff00	# v1 = 00b0
	or	v0,v0,v1	# v0 = dcba
	j	ra
	END(nuxi_l)

#define CPU_IMP_R6000	3

LEAF(get_machine_type)
	/*
	 *	uses v0,v1,a0
	 *		returns machine_type in v0
	 */

	.set	noreorder
	mfc0	v0,C0_PRID		# what kind of CPU is this?
	nop				# (LDSLOT)
	.set	reorder
	srl	v0,8			# right-justify "implementation" field
	bne	v0,CPU_IMP_R6000,1f	# R6000 CPU?

	/*
	 * This is an R6000 CPU.  Use the explicit BRDTYPE.
	 */
	li	v0,BRDTYPE_R6300
	j	ra
1:

#ifndef SABLE
	/*
	 * There are no proms on the R2000 side of the Jupiter workstation.
	 * The first page of prom space is remapped to zero.  By writting
	 * a pattern to some offset in the first page and reading that
	 * pattern back in prom space we know that this system is a
	 * Jupiter workstation.
	 */
	li	v1,RESTART_ADDR-4
	li	v0,RESTART_MAGIC
	sw	v0,0(v1)
	or	v1,BOOTPROM
	lw	a0,0(v1)
	bne	a0,RESTART_MAGIC,3f
	li	v0,BRDTYPE_I2000
	j	ra
3:
	/*
	 * The idea here is that on Intrepid, BOOTPROM and BOOTPROM +
	 * BOOTPROM_SIZE point to mirror images of the boot prom.
	 * If it fails, then we are on either a M-series or M2000
	 */

	li	v0,ID_SN5_OFF			# max bytes in IDPROM
1:
	#
	#	check if data in BOOTPROM matches
	#
	lbu	v1,(K1BASE|BOOTPROM)(v0)		
	lbu	a0,K1BASE|(BOOTPROM+BOOTPROM_SIZE)(v0)
	bne	v1,a0,2f
	#
	#	check if passed max bytes in IDPROM
	sub	v0,1
	blt	v0,zero,1b
	#
	# everything matched therefore it's intrepid or the Rx3030 series
	#
	li	v0,32			# max bytes in IDPROM
	li	v1,0			# initialize checksum
	la	a0,K1BASE|IDPROM_R2400+3
	#
	#	check if IDPROM checksum OK
	#
1:
	lbu	a1,0(a0)
	addu	v1,a1
	#	check if passed max bytes in IDPROM
	addu	a0,4
	sub	v0,1
	bgt	v0,zero,1b

	and	v1,0xff
	beq	v1,zero,1f		# if checksum OK, then Intrepid
	li	v0,BRDTYPE_R3030
	j	ra
1:
        lhu     v0,SCR|K1BASE
        andi    v0,SCR_KEY0
        bne     v0,zero,4f
        li      v0,BRDTYPE_M180
        j       ra
4:	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra

2:		
	# something mismatched hence Mbox
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
#ifdef	R3030
	lw	v0,machine_type
	j	ra
#else	R3030
#ifdef INTREPID
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#endif INTREPID
#endif	R3030
#endif SABLE
	END(get_machine_type)

/*
 *  get_ioa_ctlspace_addr (k1_addr)
 *
 *  given a K1 address, returns the appropriate IOA CtlSpace address in v0
 *  (or zero, if k1_addr isn't in IOA GBA space), and the GBA number in v1
 *
 *  uses only v0..v1, t0..t2, and does not modify a0
 */
LEAF(get_ioa_ctlspace_addr)
	move	v0,zero			# init null return value
	li	t1,IOA3
	sltu	t2,a0,t1		# below the lowest IOA?
	bne	t2,zero,1f		#  yes, in Memory space
	sub	t0,a0,t1		# byte addr, relative to IOA3 base
	srl	t0,22			# r-justify signif addr bits
	andi	t0,0x1c			#  and form a simple index
	andi	v1,t0,0x4		# extract GBA number
	srl	v1,2			#  and right-justify it
	la	t1,ioa_ctl_index_tbl	# use simple index to map
	add	t0,t1			#
	lw	t1,(t0)			#    to another index value
	la	t2,ioa_ctlspace_vaddr
	add	t2,t1
	lw	v0,(t2)			# addr of CtlSpace of IOA
1:	j	ra
	END(get_ioa_ctlspace_addr)

	.data
ioa_ctl_index_tbl:
	.word	8,8, 4,4, 0,0, 12,12
	/*      IOA3 IOA2 IOA1 zero	*/
	.text

/*
 * Bootstrap program executed in user mode
 * to bring up the system.
 */
EXPORT(icode)
	la	a0,icode_file
	la	v0,icode		# relocate for user space
	subu	a0,v0
	addu	a0,USRDATA
	la	a1,icode_argv
	la	v0,icode		# relocate for user space
	subu	a1,v0
	addu	a1,USRDATA
	sw	a0,0(a1)		# relocate vector in d space
	move	a2,zero
	li	v0,SYS_exece
	syscall
	li	v0,SYS_exit
	syscall
1:	b	1b

	.align	4
EXPORT(icode_argv)
	.word	icode_file
	.space	10*4			# leave space for boot args

EXPORT(icode_file)
	.asciiz	"/etc/init"
	.space	32
argp:					# leave space for boot args
	.space	64

	.align	4

EXPORT(icode_args)
	.word	argp

EXPORT(icode_argc)
	.word	1

EXPORT(eicode)

LEAF(cntl_p)
	j	ra
	.end	cntl_p

#ifdef	DEBUG
	.globl	fsstray
	.ent	fsstray,0
fsstray:
	move	a1, ra
	j	_fsstray
	.end	fsstray
#endif	DEBUG

#ifdef	R6000
/*
 * Do nothing routine for sync'ing the logic analyzer
 */
LEAF(load_pid_sync)
	.set	noreorder
	mfc0	a2,C0_SR		# save current Cp0 SR
	mtc0	zero,C0_SR		# disable ints -- can use k0/k1
	nop				#  ... wait a bit ...
#ifdef R6000_BUG_PID
	mfc0	k1,C0_ERROR		# save current Cp0 Error Reg
	li	k0,SR_MM_MODE
	mtc0	k0,C0_SR		# enable MM Mode instructions
#endif R6000_BUG_PID
	mfc0	k0,C0_PID		# get current value
	nop
80:	mtc0	k0,C0_PID		# do this enough times to ensure that
	mtc0	k0,C0_PID		#  we have four in four cycles
	mtc0	k0,C0_PID
	mtc0	k0,C0_PID
	mtc0	k0,C0_PID
	mtc0	k0,C0_PID
	mtc0	k0,C0_PID
81:	mtc0	k0,C0_PID
#ifdef R6000_BUG_PID
	la	k0,80b			# invalidate the Write-PID instructions
	inval	(k0)			#  in the I-cache, which will clean
	la	k0,81b			#   up the virtual tags in the I- and
	inval	(k0)			#    S-cache
	or	k1,C0_ERROR_EXT		# restore Cp0 Error Reg, and
	mtc0	k1,C0_ERROR		#  turn off External Parity err bit
#endif R6000_BUG_PID
	j	ra
	mtc0	a2,C0_SR		# restore SR
	.set	reorder
	END(load_pid_sync)	
#endif	R6000
