#ident "$Header: pon_allexc.s,v 1.8.1.1 90/07/18 14:29:08 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restrictive Rights Legend                        |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#  $ */

/*
 *	Functional Description:
 *
 *	This diagnostic attempts to tickle a number of exceptions
 *      simultaneously.  It stores all possible sequences of three
 *      instructions into the last three locations of a page.
 */

#include "machine/asm.h"
#include "machine/dregdef.h"
#include "machine/mach_ops.h"
#include "machine/cp0.h"
#include "machine/orgmacs.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "pon.h"

#undef	BUSERR
#undef	USED

#define	LOCAL(x) \
	.ent	x,0; \
x:; \
	.frame	sp,0,ra

#define	RESULTMAGIC	0x12345678
#define	RESULTMAGIC	0x12345678
#define	BUSERROR	0xafcacaf0	/* unmapped, uncached non-existent memory */
#define MAXINT		0x7fffffff
#define EXCEPT		0xffffffff
#define NOEXCEPT	0

#define ILLEGAL		.word	0x601f0000

#define INSTTABLE	0xC000c010
#ifdef BUSERR
#define ENDOFLIST	0xC000c18c	/* 19 20-byte entries */
#else
#define ENDOFLIST	0xC000c164	/* 17 20-byte entries */
#endif !BUSERR
#define ENDOFPAGE	0xC000fff4
#define MSLOT		0xC000fff4
#define ASLOT		0xC000fff8
#define RSLOT		0xC000fffc
#define NEWPAGE		0xC0010000


	.extern	failure
	.extern	skipped
	.extern	success
ALLEXCFRM=	(16 * 4)		# save s0-s7, t8, t9, k0, k1, gp, fp, ra (15 regs) & SR
NESTED(Pon_Allexc, ALLEXCFRM, zero)

	subu	sp,ALLEXCFRM		# push our frame
	sw	ra,ALLEXCFRM-4(sp)	# save return address
	sw	fp,ALLEXCFRM-8(sp)
	sw	gp,ALLEXCFRM-12(sp)
	sw	k1,ALLEXCFRM-16(sp)
	sw	k0,ALLEXCFRM-20(sp)
	sw	t9,ALLEXCFRM-24(sp)
	sw	t8,ALLEXCFRM-28(sp)
	.set noreorder
	mfc0	v0,C0_SR		# get Status
	sw	s7,7*4(sp)
	.set reorder
	sw	v0,ALLEXCFRM-32(sp)	# and save it
	sw	s6,6*4(sp)
	sw	s5,5*4(sp)
	sw	s4,4*4(sp)
	sw	s3,3*4(sp)
	sw	s2,2*4(sp)
	sw	s1,1*4(sp)
	sw	s0,(sp)

#ifndef	R3030
	lw	v0,machine_type
	bne	v0,BRDTYPE_R6300,1f
	move	v0,zero			# R6000:  disable interrupts
	b	2f
1:
#endif	!R3030
	li	v0,SR_PE		# clear parity error bit, clear IEc
	.set noreorder
2:	mtc0	v0,C0_SR
	.set reorder

#ifndef	R3030
	li	a0,PON_ALLEXC
	jal	pon_set_leds
#endif	!R3030

	la	a0,begintest
	jal	pon_puts

	/* Check dependencies */

	jal	GetDepend

	and	v0,PON_FAULT_CACHE|PON_FAULT_MEM|PON_FAULT_TLB
	beq	v0,zero,run

	la	a0,skipped
	jal	pon_puts

	b	norun

	/* Invalidate all TLB entries */
run:
	jal	flushall_tlb

	/* Put the I and D caches in a known state */

	jal	init_cache1

	/* Install exception vector routines */

	la	a0,unexpected_utlbmiss_vector	# point to utlb code
	li	a1,UT_VEC		# point to vector
	jal	InstallHandle		# move the code into the vector

	la	a0,unexpected_exception_vector # Point to exception code
	li	a1,E_VEC
	jal	InstallHandle		# move the code into the vector

#ifdef BUSERR
	lw	v0,machine_type
	bne	v0,BRDTYPE_R6300,1f
	move	v0,zero			# XXX disable interrupts
	b	2f
1:	li	v0,SR_IEC|SR_IMASK7	# enable BusErr interrupts
#else
	move	v0,zero			# disable interrupts
#endif !BUSERR
	.set noreorder
2:	mtc0	v0,C0_SR
	.set reorder

	/* Set up three valid entries in the TLB */

#ifndef	R3030
	lw	v0,machine_type
	bne	v0,BRDTYPE_R6300,1f
	jal	setup_tlbs_r6000
	b	2f
1:
#endif	!R3030
	jal	setup_tlbs_std
2:
	/* Write the Instruction Table data to mapped memory */

	la	v0,InstTable		# starting address
	la	v1,EndOfList		# ending address
	li	a0,INSTTABLE		# where we'll put it

	/* Loop to move InstTable from PROM to real memory */
1:
	lw	a1,0(v0)
	sw	a1,0(a0)
	addi	a0,4
	addi	v0,4
	bne	v0,v1,1b		# more to do?

/*
 *	v0	EPC offset due to branch delay A
 *	v1	EPC offset due to branch delay M
 *	a0	MAXINT
 *	a1	Mslot address
 *	a2	byte address for jump
 *	a3	magic address for Bus Error
 *	t0	mapped address for jump
 *	t1	default exception EPC (Acycle)
 *	t2	earliest exception's pipelocation
 *	t3	MSlot candidate
 *	t4	ASlot candidate /current instruction's expected EPC
 *	t5	RSlot candidate /current faulting instruction pipelocation
 *		also becomes magic value for ra comparison
 *	t6	current exception flag
 *	t7	current cycle's alternate EPC
 *	s0	current cycles cause
 *	s1	expected exception EPC
 *	s2	real cause value
 *	s3	real EPC value
 *	s4	earliest exception's cause
 *	s5	RError ptr
 *	s6	AError ptr
 *	s7	MError ptr
 *	t8	Return link
 *	t9	bupSad
 *	k0	end of list marker value
 *	k1	Rslot address
 *	gp	Aslot address
 *	a1	Mslot address
 *	sp	**** never touched ****
 *	fp	default exception cause
 *	ra	Destination of exceptional instructions
 */

	li	a0,MAXINT		# maximum positive
 #	li	a1,RESULTMAGIC		# this value in ra shouldn't die
	li	a3,BUSERROR		# address for invoking buserror
	addu	a2,a3,1			# unaligned address for jump
	li	t0,0x8000		# address for mapped jump
	li	t1,NEWPAGE		# default expected EPC
	la	t8,ReturnLink		# how to get back from exception
	la	t9,bupSad		# remote link to bupSad
	li	k0,ENDOFLIST		# value for end of list comparison
	li	k1,RSLOT		# EPC R exceptions
	li	gp,ASLOT		# EPC A exceptions
	li	a1,MSLOT		# EPC M exceptions
	li	fp,EXC_RMISS		# default expected cause

	li	s5,INSTTABLE		# init RLoop table address
	lw	t3,(s5)			# get R instruction
RLoop:
	li	s6,INSTTABLE		# init ALoop table address
	sw	t3,RSLOT(zero)		# move instruction to I cache
	lw	t4,(s6)			# get A instruction
ALoop:
	li	s7,INSTTABLE		# init MLoop table address
	sw	t4,ASLOT(zero)		# move instruction to I cache
	lw	t5,(s7)			# get M instruction
MLoop:
	sw	t5,MSLOT(zero)		# move instruction to I cache

#ifndef	R3030
	lw	v0,machine_type		# for R6000, don't put a branch
	bne	v0,BRDTYPE_R6300,9f	#  instruction in a BD slot
	lw	v0,4(s7)
	beq	v0,zero,2f		# does 1st instruction cause exception?
	lw	v0,8(s7)		# yes, so if 1st and
	lw	v1,8(s6)		#  2nd instruction
	and	v0,v1			#   are both branches,
	beq	v0,zero,1f		#    then replace
	sw	zero,ASLOT(zero)	#     2nd instruction with NOP
1:	b	9f

2:	lw	v0,8(s6)		# 1st instruction doesn't cause
	lw	v1,8(s5)		#  an exception, so check for
	and	v0,v1			#   back-to-back branches in the
	beq	v0,zero,9f		#    2nd and 3rd instructions
	sw	zero,RSLOT(zero)	#     and stuff a NOP in the 3rd if so
9:
#endif	!R3030

#ifdef XXX_OK_NOW
	lw	v0,MSLOT(zero)
	andi	v1,v0,0xFF
	sw	v1,NVRAM+12(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+8(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+4(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM(zero)

	lw	v0,ASLOT(zero)
	andi	v1,v0,0xFF
	sw	v1,NVRAM+28(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+24(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+20(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+16(zero)

	lw	v0,RSLOT(zero)
	andi	v1,v0,0xFF
	sw	v1,NVRAM+44(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+40(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+36(zero)
	srl	v0,8
	andi	v1,v0,0xFF
	sw	v1,NVRAM+32(zero)
#endif XXX_OK_NOW

	jal	FlushWB

	li	ra,RESULTMAGIC		# this should never be clobbered
	li	v0,ENDOFPAGE
	j	v0			# GOTO THE TEST

ReturnLink:				# RETURN HERE
	lw	t4,(s6)			# restore A instruction
	sw	t4,ASLOT(zero)		#  in case NOP'ed
	lw	t3,(s5)			# restore R instruction
	sw	t3,RSLOT(zero)		#  in case NOP'ed

	add	s7,20
	lw	t5,(s7)
	bne	k0,s7,MLoop

	add	s6,20
	lw	t4,(s6)
	bne	k0,s6,ALoop

	add	s5,20
	lw	t3,(s5)
	bne	k0,s5,RLoop

	j	bupHappy		# get here after last combination
bupSad:
	beq	v1,zero,1f		# expected BD?
	li	v0,CAUSE_BD
	or	s4,v0			# yes, fix up Expected Cause
1:
	la	a0,failure
	jal	pon_puts

	la	a0,instructions_msg
	jal	pon_puts
	lw	a0,MSLOT(zero)
	jal	pon_puthex
	la	a0,space_msg
	jal	pon_puts
	lw	a0,ASLOT(zero)
	jal	pon_puthex
	la	a0,space_msg
	jal	pon_puts
	lw	a0,RSLOT(zero)
	jal	pon_puthex
	la	a0,crlf
	jal	pon_puts

	la	a0,epc_msg
	jal	pon_puts
	move	a0,s1
	jal	pon_puthex
	la	a0,slash_msg
	jal	pon_puts
	move	a0,s3
	jal	pon_puthex
	la	a0,crlf
	jal	pon_puts

	la	a0,cause_msg
	jal	pon_puts
	move	a0,s4
	jal	pon_puthex
	la	a0,slash_msg
	jal	pon_puts
	move	a0,s2
	jal	pon_puthex
	la	a0,crlf
	jal	pon_puts

	sw	s1,MSLOT(zero)		# let's see the expected EPC
	sw	s2,ASLOT(zero)		# let's see the REAL CAUSE (maybe BD)
	sw	s3,RSLOT(zero)		# let's see the REAL EPC
	sw	s4,MSLOT(zero)		# let's see the expected cause (no BD)
	sw	v1,ASLOT(zero)		# will be 4 if we expect the BD bit set

	jal	init_cache1		# invalidate all cache entries

	li	a0,PON_ALLEXC
	jal	FastFlash

#ifndef	R3030
	li	a0,PON_ALLEXC
	jal	pon_set_leds
#endif	!R3030
norun:
	li	a0,PON_FAULT_EXCEPT
	jal	SetDepend

	lw	ra,ALLEXCFRM-4(sp)	# restore ra
	lw	fp,ALLEXCFRM-8(sp)
	lw	gp,ALLEXCFRM-12(sp)
	lw	k1,ALLEXCFRM-16(sp)
	lw	k0,ALLEXCFRM-20(sp)
	lw	t9,ALLEXCFRM-24(sp)
	lw	t8,ALLEXCFRM-28(sp)
	lw	v0,ALLEXCFRM-32(sp)	# get entry SR
	.set noreorder
	mtc0	v0,C0_SR		# and restore
	.set reorder
	lw	s7,7*4(sp)
	lw	s6,6*4(sp)
	lw	s5,5*4(sp)
	lw	s4,4*4(sp)
	lw	s3,3*4(sp)
	lw	s2,2*4(sp)
	lw	s1,1*4(sp)
	lw	s0,(sp)
	addu	sp,ALLEXCFRM		# pop stack
	li	v0,1			# indicate an error
	j	ra
bupHappy:
	jal	init_cache1		# invalidate all cache entries

	la	a0,success
	jal	pon_puts

	lw	ra,ALLEXCFRM-4(sp)	# restore ra
	lw	fp,ALLEXCFRM-8(sp)
	lw	gp,ALLEXCFRM-12(sp)
	lw	k1,ALLEXCFRM-16(sp)
	lw	k0,ALLEXCFRM-20(sp)
	lw	t9,ALLEXCFRM-24(sp)
	lw	t8,ALLEXCFRM-28(sp)
	lw	v0,ALLEXCFRM-32(sp)	# get entry SR
	.set noreorder
	mtc0	v0,C0_SR		# and restore
	.set reorder
	lw	s7,7*4(sp)
	lw	s6,6*4(sp)
	lw	s5,5*4(sp)
	lw	s4,4*4(sp)
	lw	s3,3*4(sp)
	lw	s2,2*4(sp)
	lw	s1,1*4(sp)
	lw	s0,(sp)
	addu	sp,ALLEXCFRM		# pop stack
	move	v0,zero			# indicate success
	j	ra

	/* Do extra checking if exception takes us to UTLB vector */
UTLBMiss:
	/* Check Bad Virtual Address */

	.set noreorder
	mfc0	s1,C0_BADVADDR
	.set reorder
	li	s2,0x80000000
	bgeu	s1,s2,OhSoSad

	.set noreorder
	mfc0	s1,C0_CAUSE		# check the cause
	nop
	.set reorder
	andi	s1,0x38			# mask out uninteresting Cause bits
	li	s2,8			# cause should be TLBL or TLBS
	bne	s1,s2,OhSoSad
Exceptions:
#ifdef	BUSERR
	lhu	zero,FID|K1BASE
	lw	zero,FAR|K1BASE
#endif	BUSERR
	nop
	lw	v1,8(s7)		# M cycle BD
	lw	v0,8(s6)		# A cycle BD
	and	t7,v0,v1		# if both are branches
	move	t2,t1			# get the default pipeloc
	bne	t7,zero,TryMore		#   then illegal double branch

	lw	v1,8(s5)		# R cycle BD
	move	s1,t2			# get the default EPC
	and	v0,v1			# if A cycle and R cycle branch
	bne	v0,zero,TryMore		# then illegal double branch

	move	s4,fp			# get the default cause

	lw	t6,4(s7)		# M cycle mask
	beq	zero,t6,LoadACycle	# if no M cycle exception

	move	t5,a1			# get current instructions pipeloc
	move	t4,a1			# get instruction's EPC
	move	s2,zero			# M cycle never in branch delay
	lw	t7,16(s7)		# alternate EPC
	beq	t7,zero,MEPC		# if non c then this is it

	move	t4,t7			# make alternate the expected
	add	t5,8			# effective pipeloc is later
	lw	s2,8(s6)		# BD bit pointer for A cycle
MEPC:
	lw	s0,12(s7)		# M cycle code
	bgtu	t5,t2,LoadACycle	# if M cycle exception earlier

	move	t2,t5			# effective pipeloc
	move	s1,t4			# then EPC
	move	s4,s0			# and cause become expected
	move	v1,s2			# BD bit pointer
LoadACycle:
	lw	t6,4(s6)		# A cycle mask
	beq	zero,t6,LoadRCycle	# if no A cycle exception

	move	t5,gp			# get current instructions pipeloc
	move	t4,gp
	lw	s2,8(s7)		# in M's BD slot
	lw	t7,16(s6)		# alternate EPC
	beq	t7,zero,AEPC		# if non c then this is it

	move	t4,t7			# make alternate the expected
	add	t5,8
	lw	s2,8(s5)		# in R's BD slot
AEPC:
	lw	s0,12(s6)		# A cycle code
	bgtu	t5,t2,LoadRCycle	# if A cycle exception earlier

	move	t2,t5			# effective pipeloc
	move	s1,t4			# then EPC
	move	s4,s0			# and cause become expected
	move	v1,s2			# A cycle branch delay corretion
LoadRCycle:
	lw	t6,4(s5)		# R cycle mask
	beq	zero,t6,CheckException	# if no R cycle exception

	move	t5,k1			# get current instructions pipeloc
	move	t4,k1
	lw	s2,8(s6)		# in A delay slot
	lw	t7,16(s5)		# alternate EPC
	beq	t7,zero,REPC		# if non c then this is it

	move	t4,t7			# make alternate the expected
	add	t5,8
	add	s2,zero,4		# always in delay slot of I
REPC:
	lw	s0,12(s5)		# R cycle code
	bgeu	t5,t2,CheckException	# if R cycle exception earlier

	move	s1,t4			# then EPC
	move	s4,s0			# and cause become expected
	move	v1,s2			# R cycle branch delay corretion
CheckException:
	.set noreorder
	mfc0	s3,C0_EPC		# get the real EPC
	mfc0	s2,C0_CAUSE		# get the exception cause
	.set reorder
	sgt	s0,v1,zero		# s0 == 1 if branch delay expected
	sge	t7,s2,zero		# t7 == 1 if branch bit not set
	xor	s0,t7			# s0 == 1 if branch delay match
	beq	s0,zero,OhSoSad		# Error - delay bit mismatch

	subu	s1,v1			# correct EPC for branch delay
	beq	s1,s3,NotSad		# compare EPC
OhSoSad:
	j	t9			# bad news, EPC doesn't match
NotSad:
	and	s2,0x3c			# extract cause
	bne	s2,s4,OhSoSad		# bad news, Cause doesn't match

	li	t5,RESULTMAGIC		# t5 no longer needed
	bne	ra,t5,OhSoSad		# something got clobbered
TryMore:
	j	t8			# go back and try somemore

unexpected_utlbmiss_vector:
	la	v0,UTLBMiss
	j	v0

unexpected_exception_vector:
	la	v0,Exceptions
	j	v0

END(Pon_Allexc)


InstTable:

#ifdef	BUSERR
	/* Cause a bus error on instruction */

	.word 0x00e00008		# jr	a3
	.word EXCEPT
	.word 4
	.word EXC_IBE
	.word BUSERROR
#endif	BUSERR

	/* Integer overflow */

	.word 0x084f820			# add	ra,a0,a0
	.word EXCEPT
	.word 0
	.word EXC_OV
	.word 0

	/* D TLB MISS load */

	.word 0x8d1f0000		# lw	ra,(t0)
	.word EXCEPT
	.word 0
	.word EXC_RMISS
	.word 0

	/* D TLB MISS store */

	.word 0xad1f0000		# sw	ra,(t0)
	.word EXCEPT
	.word 0
	.word EXC_WMISS
	.word 0

	/* D TLB MISS store, misaligned */

	.word 0xad1f0001		# sw	ra,1(t0)
	.word EXCEPT
	.word 0
	.word EXC_WADE
	.word 0

	/* D TLB dirty */

	.word 0xac1f4000		# sw	ra,16384(zero)
	.word EXCEPT
	.word 0
	.word EXC_MOD
	.word 0

	/* D address error (misaligned) */

	.word 0x8c1f4001		# lw	ra,16385(zero)
	.word EXCEPT
	.word 0
	.word EXC_RADE
	.word 0

	/* D address error (misaligned) */

	.word 0xad1fc001		# sw	ra,-16383(t0)
	.word EXCEPT
	.word 0
	.word EXC_WADE
	.word 0

	/* D address error and miss */

	.word 0x8d1f0001		# lw	ra,1(t0)
	.word EXCEPT
	.word 0
	.word EXC_RADE
	.word 0

	/* D address error, dirty */

	.word 0xac1f4001		# sw	ra,16385(zero)
	.word EXCEPT
	.word 0
	.word EXC_WADE
	.word 0

#ifdef	BUSERR
	/* Cause a bus error on data */

	.word 0x8cff0000		# lw	ra,0(a3)
	.word EXCEPT
	.word 0
	.word EXC_DBE
	.word 0
#endif	BUSERR

	/* System call */

	.word 0x0000000c		# syscall
	.word EXCEPT
	.word 0
	.word EXC_SYSCALL
	.word 0

	/* Break trap */

#ifdef SABLE
	.word 0x0000000c		# syscall  (Sable can't handle brk)
#else
	.word 0x0000000d		# break 0
#endif !SABLE
	.word EXCEPT
	.word 0
#ifdef SABLE
	.word EXC_SYSCALL
#else
	.word EXC_BREAK
#endif !SABLE
	.word 0

	/* Some kind of illegal instruction */

	.word 0x601f0000		# op60	ra,zero,0
	.word EXCEPT
	.word 0
	.word EXC_II
	.word 0

	/* Coprocessor unusable */

	.word 0x441f0000		# mfc1	ra,fp0
	.word EXCEPT
	.word 0
	.word EXC_CPU
	.word 0

	/* Unaligned instruction address */

	.word 0x00c00008		# jr	a2
	.word EXCEPT
	.word 4
	.word EXC_RADE
	.word BUSERROR+1

	/* Mapped instruction address */

	.word 0x01000008		# jr	t0
	.word EXCEPT
	.word 4
	.word EXC_RMISS
	.word 0x8000

	/* D TLB okay */

	.word 0xad1f4000		# sw	ra,16384(t0)
	.word NOEXCEPT
	.word 0
	.word 0
	.word 0

	/* Okay instruction */

	.word 0				# nop
	.word NOEXCEPT
	.word 0
	.word 0
	.word 0

EndOfList:


LOCAL(InstallHandle) 			# write handler code to entrypoint
	/*
	 *  Don't modify a0
	 */
	lw	t0,0(a0)
	lw	t1,4(a0)
	sw	t0,0(a1)
	lw	t0,8(a0)
	sw	t1,4(a1)
	lw	t1,12(a0)
	sw	t0,8(a1)
	sw	t1,12(a1)
	j	ra
END(InstallHandler)


LOCAL(init_cache1)

#ifndef	R3030
	lw	v0,machine_type
	beq	v0,BRDTYPE_R6300,init_cache1_r6000
#endif	!R3030

	.set	noreorder
	mfc0	v1,C0_SR		# save current SR
	mtc0	zero,C0_CAUSE		# clear software exceptions

	/*
	 * Clear I and D-caches to known state.
	 */
	li	v0,SR_ISC
	mtc0	v0,C0_SR
	li	v0,MAXCACHE
1:
	sb	zero,K0BASE(v0)		# invalidate
	subu	v0,4
	bgez	v0,1b
	nop

	li	v0,SR_ISC|SR_SWC
	mtc0	v0,C0_SR
	li	v0,MAXCACHE
1:
	sb	zero,K0BASE(v0)		# invalidate
	subu	v0,4
	bgez	v0,1b
	nop

	ori	v1,SR_PE		# clear PE bit and
	j	ra
	mtc0	v1,C0_SR		# (BDSLOT) swap back original SR
	.set	reorder
END(init_cache1)


#ifndef	R3030
LOCAL(init_cache1_r6000)
	move	s0,ra			# save return address
	jal	init_cache		# reset data portions, ptags, vtags
	j	s0
END(init_cache1_r6000)
#endif	!R3030


/*
 *  setup_tlbs_std
 *
 *  Setup the three test TLBs for R2000/R3000-based machine
 */
LOCAL(setup_tlbs_std)
	.set noreorder
	li	v0,8<<TLBINX_INXSHIFT
	mtc0	v0,C0_INX
	li	v1,0xc000|TLBLO_V	# keep it clean
	mtc0	v1,C0_TLBLO
	li	v0,0x00004000		# VA = 0x4000, PID = 0
	mtc0	v0,C0_TLBHI
	nop				# (pause before WRITEI)
	c0	C0_WRITEI		# at entry 8

	li	v0,63<<TLBINX_INXSHIFT
	mtc0	v0,C0_INX
	li	v0,0xc000|TLBLO_V|TLBLO_D
					# make dirty
	mtc0	v0,C0_TLBLO
	li	v0,0x0000c000		# VA = 0xc000, PID = 0
	mtc0	v0,C0_TLBHI
	nop				# (pause before WRITEI)
	c0	C0_WRITEI		# at entry 63

	li	v0,13<<TLBINX_INXSHIFT
	mtc0	v0,C0_INX		# make dirty and NON-cacheable
	li	v0,0xc000|TLBLO_V|TLBLO_D|TLBLO_N
	mtc0	v0,C0_TLBLO
	li	v0,0xc000c000		# VA = 0xc000c000, PID = 0
	mtc0	v0,C0_TLBHI
	nop				# (pause before WRITEI)
	c0	C0_WRITEI		# at entry 13

	li	v0,25<<TLBINX_INXSHIFT
	mtc0	v0,C0_INX		# make dirty and NON-cacheable
	li	v0,0xf000|TLBLO_V|TLBLO_D|TLBLO_N
	mtc0	v0,C0_TLBLO
	li	v0,0xc000f000		# VA = 0xc000f000, PID = 0
	mtc0	v0,C0_TLBHI
	nop				# (pause before WRITEI)
	c0	C0_WRITEI		# at entry 25
	j	ra
	nop				# (BDSLOT)
	.set reorder
END(setup_tlbs_std)


#ifndef	R3030
/*
 *  setup_tlbs_r6000
 *
 *  Setup the three test TLBs for R6000-based machine
 */
LOCAL(setup_tlbs_r6000)
	.set	noreorder
	mtc0	zero,C0_PID		# reset PID to zero
	mfc0	t7,C0_SR		# save current SR
	li	t5,SR_MM_MODE		# enable MM mode
	mtc0	t5,C0_SR
	/*
	 *  First, clear both sides of the first line of tlbs.
	 *  That's where the action will be.
	 */
	li	t0,0x3c000		# starting scache address
	add	t1,t0,32*4-4		# ending address
1:	scache	zero,0(t0)		# clear side-0
	scache	zero,1(t0)		#   and side-1
	bne	t0,t1,1b		# done?
	addi	t0,4			# (BDSLOT) incr scache address
	/*
	 *  Now set up the three tlbs.
	 *
	 *  The physical address assignments are very scary, but they follow
	 *  historical use.  We actually write to virtual 0xc000 (1 word),
	 *  0xc000c000 (a bunch of words), and 0xc000eff4 (3 words).  Out of
	 *  sheer luck (and looking at the post-Link memory map), we don't
	 *  hit anything important.
	 *  NOTE:  because of the initial CPU bug re s-cache indexing, we
	 *	   do perfect page coloring
	 */
	li	t0,0x3c004		# virt 0x00004000 --> phys 0xc000
	li	t1,0xc00|TLB_V_R6000
	scache	t1,0(t0)

	li	t0,0x3c00c		# virt 0x0000c000 --> phys 0xc000
	li	t1,0xc00|TLB_V_R6000|TLB_D_R6000
	scache	t1,0(t0)

	li	t0,0xc003c00c		# virt 0xc000c000 --> phys 0xc000
	li	t1,0xc00|TLB_V_R6000|TLB_D_R6000|TLB_N_R6000
	scache	t1,1(t0)

	j	ra
	mtc0	t7,C0_SR		# (BDSLOT) restore SR
	.set	reorder
END(setup_tlbs_r6000)
#endif	!R3030

	.data

begintest:
	.asciiz "Exception Test..."
instructions_msg:
	.asciiz	"  Instructions:  "
epc_msg:
	.asciiz	"  EPC   expected/actual:  "
cause_msg:
	.asciiz	"  Cause expected/actual:  "
slash_msg:
	.asciiz	" / "
space_msg:
	.asciiz	" "
