#ident "$Header: pon_fp1.s,v 1.13.3.1 90/07/18 14:31:24 huang Exp $"
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "machine/dregdef.h"
#include "machine/standard.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/delaymacs.h"
#include "machine/asm.h"
#include "machine/fpu.h"
#include "pon.h"

#define	LOCAL(x) \
	.ent	x,0; \
x:; \
	.frame	sp,0,ra

#define MTC1_R0_CSR	.word	0x44C0F800
#define MTC1_R12_IRR	.word	0x44CC0000

#define	VME_OP_CBASE	0x80100000	# *cached* space
#define	VME_OP_UBASE	0xA0100000	# uncached space
#define	VME_MV_BASE	0xA0100000	# uncached space

#define P1_FPU_PRSNT_B	0x01		# 0 = FPU present in the system

		.extern	machine_type
		.extern	success
		.extern	failure
		.extern	skipped
		.extern	SR_IBIT_FP

		.data

		.align	2
sdata:
 #sp0:		.float	0.0
 #sp1:		.float	1.0
 #sp2:		.float	2.0
sp3:		.float	3.0
 #sp4:		.float	4.0
 #sp5:		.float	5.0
 #sp6:		.float	6.0
 #sp7:		.float	7.0
 #sp8:		.float	8.0
 #sp9:		.float	9.0
 #sp10:		.float	10.0
 #sp11:		.float	11.0
 #sp12:		.float	12.0
sp111111:	.float	111111.0
sp222222:	.float	222222.0
sp333333:	.float	333333.0
sp444444:	.float	444444.0
sp555555:	.float	555555.0
sp666666:	.float	666666.0
sp777777:	.float	777777.0
 #sp888888:	.float	888888.0
sp999999:	.float	999999.0

		.align	3
ddata:
dp0:		.double	0.0
dp1:		.double	1.0
 #dp2:		.double	2.0
dp3:		.double	3.0
 #dp4:		.double	4.0
 #dp5:		.double	5.0
 #dp6:		.double	6.0
 #dp7:		.double	7.0
 #dp8:		.double	8.0
 #dp9:		.double	9.0
 #dp10:		.double	10.0
 #dp11:		.double	11.0
 #dp12:		.double	12.0
dp111111111111:	.double 111111111111.0
dp222222222222:	.double 222222222222.0
dp333333333333:	.double 333333333333.0
 #dp444444444444:	.double 444444444444.0
dp555555555555:	.double 555555555555.0
 #dp666666666666:	.double 666666666666.0
dp777777777777:	.double 777777777777.0
 #dp888888888888:	.double 888888888888.0
dp999999999999:	.double 999999999999.0
 #dpInfinity:	.word	0x3ff00000, 0x00000000

		.text
		.set	noreorder

FPFRM=		(4 * 8) + 4
NESTED(Pon_Fp1, FPFRM, zero)

		subu	sp,FPFRM
		sw	ra,0(sp)
		sw	s0,4(sp)
		sw	s1,8(sp)
		sw	s2,12(sp)
		sw	s3,16(sp)
		sw	s4,20(sp)
		sw	s5,24(sp)
		sw	s6,28(sp)
		sw	s7,32(sp)

#ifdef	DEBUG
		la	a0,start
		jal	pon_puts
		nop
#endif	DEBUG
		la	a0,begintest
		jal	pon_puts
		nop

		jal	FindFpu
		nop

		beq	v0,zero,1f	# FPU found
		nop

#ifdef	DEBUG
		la	a0,notfound
		jal	pon_puts
		nop
#endif	DEBUG

		la	a0,skipped
		jal	pon_puts
		nop

		move	v0,zero
		j	NoFpu
		nop
1:
#ifdef	DEBUG
		la	a0,found
		jal	pon_puts
		nop
#endif	DEBUG

#ifndef	R3030
		li	a0,PON_FP1
		jal	pon_set_leds
		nop
#endif	!R3030

		jal	GetDepend
		nop

		and	v0,PON_FAULT_FP
		beq	v0,zero,run
		nop

		la	a0,skipped
		jal	pon_puts
		nop

		b	FpuNoRun
		nop
run:
		jal	ResetFpu	# do a h/w reset of the FPU
		nop

		mfc0	v0,C0_SR
		nop
		and	v0,SR_BEV
		or	v0,SR_CU1	# make FPU ops street-legal
		mtc0	v0,C0_SR
		nop
		nop
		nop
		nop
		MTC1_R0_CSR		# clear the FPU's cntl and status reg

		lw	t0,machine_type	# determine appropriate FP interrupt
		nop			#  mask for this machine_type
		sub	t0,1
		sll	t0,t0,2
		lw	t0,SR_IBIT_FP(t0)
 # 	  	or	v0,(SR_CU1|SR_IEC)
 	  	or	v0,SR_CU1
		or	v0,t0		# enable FPU; set IM's, IEc
		and	v0,~SR_IBIT3	# mask for killing VME rupts
		mtc0	v0,C0_SR
		mtc0	zero,C0_CAUSE	# clear the cause reg's Sw-bits
		nop
forever:
		nop
		jal	FPUclear	# zero-out all the FPRs
		nop

		li	t7,VME_OP_UBASE	# set test-operation  base reg
		li	s7,VME_MV_BASE		# set data-relocation base reg

		# test separated COMPS and bc1t (equal operands)

case1:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp111111	# load up both operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp0		# send them to the FPU
		mtc1	v1,fp30
		nop
		nop
		c.seq.s	fp0,fp30	# do a compare
		delay40(v0)		# to make sure the comp is done
		delay40(v0)
		cfc1	a0,fcr31	# get what's in the CSR now
		cfc1	a0,fcr31
		mfc1	a1,fp0		# fetch back the operands
		mfc1	a2,fp30		#   to see if they got stepped on
		li	a3,0x00800000	# create what we expect the CSR to be
		nop

		nop
		beq	a0,a3,1f	# is the CSR what we want
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a1,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a2,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		bc1t	1f		# bc1t working
		nop

		b	FpuErr
		nop

 #		li	k0,0xdeadbeef
 #		li	k1,0xcacabaff
1:
		jal	ShortWait
		nop

		# test separated COMPS and bc1f (unequal operands)
case2:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		la	v1,sp333333	# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp555555	# load up the 2nd operand
		lw	a0,0(a0)
		mtc1	v1,fp2		# send them to the FPU
		mtc1	a0,fp4
		nop
		nop
		c.seq.s	fp2,fp4		# do a compare
		delay40(v0)		# to make sure the comp is done
		delay40(v0)
		mfc1	a2,fp2		# fetch back the operands
		mfc1	a3,fp4		#   to see if they got stepped on
		cfc1	a1,fcr31	# get what's in the CSR now
		cfc1	a1,fcr31
		li	t0,0x00000000	# create what we expect the CSR to be
		nop
		nop
		beq	a1,t0,1f	# is the CSR what we want
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a2,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a3,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		bc1f	1f		# bc1f working
		nop

		b	FpuErr
		nop

 #		li	k0,0xdeadbeef
 #		li	k1,0xcacabaff
1:
		jal	ShortWait
		nop

		# test separated COMPD and bc1t (equal operands)

case3:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp111111111111	# get addr of both operands
		lw	v1,0(a0)		# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp7			# move msw to FPU's DP f6
		mtc1	a0,fp6			# move lsw to FPU's DP f6
		mtc1	v1,fp9			# move msw to FPU's DP f8
		mtc1	a0,fp8			# move lsw to FPU's DP f8
		nop
		nop
		c.seq.d	fp8,fp6		# do a compare
		delay40(v0)		# to make sure the comp is done
		delay40(v0)
		cfc1	a1,fcr31	# get what's in the CSR now
		cfc1	a1,fcr31
		mfc1	a2,fp6		# fetch back the operands
		mfc1	a3,fp7		#   ...
		mfc1	t0,fp8		#      ...
		mfc1	t1,fp9		#         to see if they got stepped on
		li	t2,0x00800000	# create what we expect the CSR to be
		nop

		nop
		beq	a1,t2,1f	# is the CSR what we want
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a3,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,t1,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,t0,1f	# other operand untouched (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		bc1t	1f		# bc1t working
		nop

		b	FpuErr
		nop

 #		li	k0,0xdeadbeef
 #		li	k1,0xcacabaff
1:
		jal	ShortWait
		nop

		# test separated COMPD and bc1f (unequal operands)
case4:
		li	v0,0x00800000
		ctc1	v0,fcr31		# insure the "c-bit" is 1
		la	a0,dp999999999999	# get addr of the 1st operand
		lw	v1,0(a0)		# load the 1st operand's msw
		lw	a0,4(a0)		# and get the lsw
		mtc1	v1,fp11			# move msw to FPU's DP f10
		mtc1	a0,fp10			# move lsw to FPU's DP f10
		la	t6,dp777777777777	# get addr of the 2nd operand
		lw	t5,0(t6)		# load the 2nd operand's msw
		lw	t6,4(t6)		# and get the lsw
		mtc1	t5,fp13		# move msw to FPU's DP f12
		mtc1	t6,fp12		# move lsw to FPU's DP f12
		nop
		nop
		c.seq.d	fp10,fp12	# do a compare
		delay40(v0)		# to make sure the comp is done
		delay40(v0)
		mfc1	a2,fp10		# fetch back the operands
		mfc1	a3,fp11		#   ...
		mfc1	t0,fp12		#      ...
		mfc1	t1,fp13		#         to see if they got stepped on
		cfc1	a1,fcr31	# get what's in the CSR now
		cfc1	a1,fcr31
		li	t2,0x00000000	# create what we expect the CSR to be
		nop

		nop
		beq	a1,t2,1f	# is the CSR what we want
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a3,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t5,t1,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t6,t0,1f	# other operand untouched (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		bc1f	1f		# bc1f working
		nop

		b	FpuErr
		nop

 #		li	k0,0xdeadbeef
 #		li	k1,0xcacabaff
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1t (equal operands)
case5:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp777777	# load up both operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp14		# send one to the FPU
		nop
		mov.s	fp16,fp14	# use a move to get the other one
		nop
		nop
		c.seq.s	fp14,fp16	# do a compare
		nop
		bc1t	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp14		# fetch back the operands
		mfc1	a1,fp16		#   to see if they got stepped on
		nop

		nop
		beq	v1,a0,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a1,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1t (equal operands)
case6:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp999999	# load up both operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp18		# send one to the FPU
		nop
		mov.s	fp20,fp18	# use a move to get the other one
		nop
		nop
		c.seq.s	fp20,fp18	# do a compare
		nop
		bc1t	1f		# separated check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp18		# fetch back the operands
		mfc1	a1,fp20		#   to see if they got stepped on
		nop

		nop
		beq	v1,a0,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a1,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1f (unequal operands)
case7:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		la	v1,sp111111	# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp999999	# load up the 2nd operand
		lw	a0,0(a0)
		mtc1	v1,fp22		# send them to the FPU
		mtc1	a0,fp24
		nop
		nop
		c.seq.s	fp22,fp24	# do a compare
		nop
		bc1f	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp22		# fetch back the operands
		mfc1	a2,fp24		#   to see if they got stepped on
		nop

		nop
		beq	v1,a1,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1f (unequal operands)
case8:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		la	v1,sp333333	# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp777777	# load up the 2nd operand
		lw	a0,0(a0)
		mtc1	v1,fp26		# send them to the FPU
		mtc1	a0,fp28
		nop
		nop
		c.seq.s	fp28,fp26	# do a compare
		nop
		bc1f	1f		# separated check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp26		# fetch back the operands
		mfc1	a2,fp28		#   to see if they got stepped on
		nop

		nop
		beq	v1,a1,1f	# is operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# other operand untouched
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		jal	FPUclear	# zero-out the FPU regs
		nop

		# test 1 nop separated COMPD and bc1t (equal operands)
case9:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp111111111111	# get addr of both operands
		lw	v1,0(a0)		# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp1			# move msw to FPU's DP f0
		mtc1	a0,fp0			# move lsw to FPU's DP f0
		nop
		mov.d	fp2,fp0			#  move it to FPU's DP f2
		nop
		nop
		c.seq.d	fp2,fp0		# do a compare
		nop
		bc1t	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp0		# fetch back the operands
		mfc1	a2,fp1		#   ...
		mfc1	a3,fp2		#      ...
		mfc1	t0,fp3		#         to see if they got stepped on
		nop

		nop
		beq	v1,a2,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,t0,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a3,1f	# other operand untouched (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPD and bc1t (equal operands)

case10:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp999999999999	# get addr of both operands
		lw	v1,0(a0)		# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp5			# move msw to FPU's DP f4
		mtc1	a0,fp4			# move lsw to FPU's DP f4
		nop
		mov.d	fp6,fp4			#  move it to FPU's DP f6
		nop
		nop
		c.seq.d	fp4,fp6		# do a compare
		nop
		bc1t	1f		# separated check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp4		# fetch back the operands
		mfc1	a2,fp5		#   ...
		mfc1	a3,fp6		#      ...
		mfc1	t0,fp7		#         to see if they got stepped on
		nop

		nop
		beq	v1,a2,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop

1:
		jal	ShortWait
		nop

		beq	v1,t0,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a3,1f	# other operand untouched (lsw)
		nop
		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPD and bc1f (unequal operands)

case11:
		li	v0,0x00800000
		ctc1	v0,fcr31		# insure the "c-bit" is 1
		la	a0,dp555555555555	# get addr of the 1st operand
		lw	v1,0(a0)		# load the 1st operand's msw
		lw	a0,4(a0)		# and get the lsw
		mtc1	v1,fp9			# move msw to FPU's DP f8
		mtc1	a0,fp8			# move lsw to FPU's DP f8
		la	t6,dp333333333333	# get addr of the 2nd operand
		lw	t5,0(t6)		# load the 2nd operand's msw
		lw	t6,4(t6)		# and get the lsw
		mtc1	t5,fp11		# move msw to FPU's DP f10
		mtc1	t6,fp10		# move lsw to FPU's DP f10
		nop
		nop
		c.seq.d	fp8,fp10	# do a compare
		nop
		bc1f	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp8		# fetch back the operands
		mfc1	a2,fp9		#   ...
		mfc1	a3,fp10		#      ...
		mfc1	t0,fp11		#         to see if they got stepped on
		nop

		nop
		beq	v1,a2,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t5,t0,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t6,a3,1f	# other operand untouched (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPD and bc1f (unequal operands)
case12:
		li	v0,0x00800000
		ctc1	v0,fcr31		# insure the "c-bit" is 1
		la	a0,dp777777777777	# get addr of the 1st operand
		lw	v1,0(a0)		# load the 1st operand's msw
		lw	a0,4(a0)		# and get the lsw
		mtc1	v1,fp13			# move msw to FPU's DP f12
		mtc1	a0,fp12			# move lsw to FPU's DP f12
		la	t6,dp111111111111	# get addr of the 2nd operand
		lw	t5,0(t6)		# load the 2nd operand's msw
		lw	t6,4(t6)		# and get the lsw
		mtc1	t5,fp15		# move msw to FPU's DP f14
		mtc1	t6,fp14		# move lsw to FPU's DP f14
		nop
		nop
		c.seq.d	fp8,fp10	# do a compare
		nop
		bc1f	1f		# separated check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp12		# fetch back the operands
		mfc1	a2,fp13		#   ...
		mfc1	a3,fp14		#      ...
		mfc1	t0,fp15		#         to see if they got stepped on
		nop

		nop
		beq	v1,a2,1f	# is operand one ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is operand one ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t5,t0,1f	# other operand untouched (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t6,a3,1f	# other operand untouched (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1t (same operand)

case13:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp999999	# load up the operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp16		# send it to the FPU
		nop
		nop
		c.seq.s	fp16,fp16	# do a self compare
		nop
		bc1t	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp16		# fetch back the operand
		nop

		nop
		beq	v1,a0,1f	# is the operand ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPS and bc1t (same operand)

case14:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp555555	# load up the operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp18		# send it to the FPU
		nop
		nop
		c.seq.s	fp18,fp18	# do a self compare
		nop
		bc1t	1f		# separated check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp18		# fetch back the operand
		nop

		nop
		beq	v1,a0,1f	# is the operand ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPD and bc1t (same operand)
case15:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp777777777777	# get addr the operand
		lw	v1,0(a0)		# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp21			# move msw to FPU's DP f20
		mtc1	a0,fp20			# move lsw to FPU's DP f20
		nop
		nop
		c.seq.d	fp20,fp20	# do a self compare
		nop
		bc1t	1f		# immediate check
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp20		# fetch back the operand
		mfc1	a2,fp21		#   to see if it got stepped on
		nop

		nop
		beq	v1,a2,1f	# is the operand ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is the operand ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated COMPD and bc1t (same operand)
case16:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp333333333333	# get addr the operand
		lw	v1,0(a0)		# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp23			# move msw to FPU's DP f22
		mtc1	a0,fp22			# move lsw to FPU's DP f22
		nop
		nop
		c.seq.d	fp22,fp22	# do a self compare
		nop
		bc1t	1f		# separated check
		nop

		b	FpuErr
		nop

 #		li	k0,0xdeadbeef
 #		li	k1,0xcacabaff
1:
		nop
		jal	ShortWait
		nop

		mfc1	a1,fp22		# fetch back the operand
		mfc1	a2,fp23		#   to see if it got stepped on
		nop

		nop
		beq	v1,a2,1f	# is the operand ok (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# is the operand ok (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back mtc1 and bc1t
case17:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp777777	# load up both operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp24		# send one to the FPU
		nop
		mov.s	fp26,fp24	# use a move to get the other one
		c.seq.s	fp26,fp24	# set the condition bit
		li	v1,0xAAABACAD	# create moveto data
		nop
		mtc1	v1,fp25		# do the moveto
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp25		# fetch back the data we moved over
		nop

		nop
		beq	v1,a0,1f	# did the moveto go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated mtc1 and bc1t
case18:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp333333	# load up both operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp28		# send one to the FPU
		nop
		mov.s	fp30,fp28	# use a move to get the other one
		c.seq.s	fp28,fp30	# set the condition bit
		li	v1,0x9A9B9C9D	# create moveto data
		nop
		nop
		mtc1	v1,fp31		# do the moveto
		nop
		bc1t	1f		# and now the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp31		# fetch back the data we moved over
		nop

		nop
		beq	v1,a0,1f	# did the moveto go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		jal	FPUclear	# clean the FPU regs up (x'00000000')
		nop

		# test back-to-back mfc1 and bc1t
case19:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp111111	# load up both operands
		lw	v1,0(v1)
		nop
		mtc1	v1,fp0		# send one to the FPU
		nop
		mov.s	fp2,fp0		# use a move to get the other one
		c.seq.s	fp0,fp2		# set the condition bit
		li	v1,0x8A8B8C8D	# create movefrom data
		mtc1	v1,fp4		# move it over
		nop
		mfc1	a0,fp4		# do the movefrom
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		beq	v1,a0,1f	# did the movefrom go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated mfc1 and bc1t
case20:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp333333	# load up both operands
		lw	v1,0(v1)
		nop
		mtc1	v1,fp4		# send them to the FPU
		mtc1	v1,fp6		# send one to the FPU
		nop
		c.seq.s	fp6,fp4		# set the condition bit
		li	v1,0x7A7B7C7D	# create movefrom data
		mtc1	v1,fp8		# move it over
		nop
		nop
		mfc1	a0,fp8		# do the movefrom
		nop
		bc1t	1f		# now the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		beq	v1,a0,1f	# did the movefrom go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back FOPS and bc1t
case21:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp444444	# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp333333	# now get the 2nd operand
		lw	a0,0(a0)
		mtc1	v1,fp12		# send them to the FPU
		mtc1	a0,fp10
		c.seq.s	fp12,fp12	# set the condition bit
		nop
		add.s	fp14,fp10,fp12	# f14 = 333333.0 + 444444.0 = 777777.0
		nop
		bc1t	1f		# do the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp10		# fetch back the operands
		mfc1	a2,fp12		#   to see if they got stepped on
		mfc1	a3,fp14		# get the results of the FOPS
		la	t0,sp777777	# and get the expected results
		lw	t0,0(t0)

		nop
		beq	a3,t0,1f	# did the FOPS go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a2,1f	# operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# operand two ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated FOPS and bc1t
case22:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp3		# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp222222	# now get the 2nd operand
		lw	a0,0(a0)
		mtc1	v1,fp18		# send them to the FPU
		mtc1	a0,fp16
		nop
		c.seq.s	fp16,fp16	# set the condition bit
		nop
		nop
		mul.s	fp20,fp16,fp18	# f20 = 222222.0 * 3.0 = 666666.0
		nop
		bc1t	1f		# do the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp16		# fetch back the operands
		mfc1	a2,fp18		#   to see if they got stepped on
		mfc1	a3,fp20		# get the results of the FOPS
		la	t0,sp666666	# and get the expected results
		lw	t0,0(t0)

		nop
		beq	a3,t0,1f	# did the FOPS go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a2,1f	# operand one ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a1,1f	# operand two ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back FOPD and bc1t
case23:
		ctc1	zero,fcr31		# insure the "c-bit" is 0
		la	a0,dp999999999999	# get addr of the 1st operand
		lw	v1,0(a0)		# load the 1st operand's msw
		lw	a0,4(a0)		# and get the lsw
		mtc1	v1,fp23			# move msw to FPU's DP f22
		mtc1	a0,fp22			# move lsw to FPU's DP f22
		la	t6,dp3			# get addr of the 2nd operand
		lw	t5,0(t6)		# load the 2nd operand's msw
		lw	t6,4(t6)		# and get the lsw
		mtc1	t5,fp25		# move msw to FPU's DP f24
		mtc1	t6,fp24		# move lsw to FPU's DP f24
		c.seq.d	fp22,fp22	# set the condition bit
		nop
		div.d	fp26,fp22,fp24	# f26 = 999999999999.0 / 3.0
		nop
		bc1t	1f		# do the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp27		# get msw result of the FOPD; DP f26
		mfc1	a2,fp26		# get lsw result of the FOPD; DP f26
		la	t0,dp333333333333	# get addr of expected result
		lw	a3,0(t0)		# get the expected msw
		lw	t0,4(t0)		# get the expected lsw

		nop
		beq	a1,a3,1f	# did the FOPD msw go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a2,t0,1f	# did the FOPD lsw go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		mfc1	a1,fp23		# get msw of one operand; DP f22
		mfc1	a2,fp22		# get lsw of one operand; DP f22
		nop
		beq	v1,a1,1f	# did the msw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# did the lsw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		mfc1	a1,fp25		# get msw of other operand; DP f24
		mfc1	a2,fp24		# get lsw of other operand; DP f24
		nop
		beq	t5,a1,1f	# did the msw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t6,a2,1f	# did the lsw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		jal	FPUclear	# clear the FPU regs
		nop

		# test 1 nop separated FOPD and bc1t
case24:
		ctc1	zero,fcr31		# insure the "c-bit" is 0
		la	a0,dp777777777777	# get addr of the 1st operand
		lw	v1,0(a0)		# load the 1st operand's msw
		lw	a0,4(a0)		# and get the lsw
		mtc1	v1,fp1			# move msw to FPU's DP f0
		mtc1	a0,fp0			# move lsw to FPU's DP f0
		la	t6,dp222222222222	# get addr of the 2nd operand
		lw	t5,0(t6)		# load the 2nd operand's msw
		lw	t6,4(t6)		# and get the lsw
		mtc1	t5,fp3			# move msw to FPU's DP f2
		mtc1	t6,fp2			# move lsw to FPU's DP f2
		nop
		c.seq.d	fp2,fp2		# set the condition bit
		nop
		nop
		sub.d	fp4,fp0,fp2	# f4 = 777777777777.0 - 222222222222.0
		nop
		bc1t	1f		# do the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a1,fp5		# get msw result of the FOPD; DP f4
		mfc1	a2,fp4		# get lsw result of the FOPD; DP f4
		la	t0,dp555555555555	# get addr of expected result
		lw	a3,0(t0)		# get the expected msw
		lw	t0,4(t0)		# get the expected lsw

		nop
		beq	a1,a3,1f	# did the FOPD msw go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a2,t0,1f	# did the FOPD lsw go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		mfc1	a1,fp1		# get msw of one operand; DP f0
		mfc1	a2,fp0		# get lsw of one operand; DP f0
		nop
		beq	v1,a1,1f	# did the msw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# did the lsw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		mfc1	a1,fp3		# get msw of other operand; DP f2
		mfc1	a2,fp2		# get lsw of other operand; DP f2
		nop
		beq	t5,a1,1f	# did the msw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	t6,a2,1f	# did the lsw get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back lwc1 and bc1t
case25:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp777777	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp6		# send it to the FPU
		nop
		mov.s	fp8,fp6		# use a move to get the other one
		c.seq.s	fp6,fp8		# set the condition bit
		li	v1,0x33343536	# create data to lwc1 later
		sw	v1,0(s7)	# out of reg into memory
		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		nop
		lwc1	fp10,0(t7)	# do the load
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp10		# fetch back the data we moved over
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated lwc1 and bc1t
case26:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp666666	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp12		# send it to the FPU
		nop
		mov.s	fp14,fp12	# use a move to get the other one
		c.seq.s	fp12,fp14	# set the condition bit
		li	v1,0x23242526	# create data to lwc1 later
		sw	v1,4(s7)	# out of reg into memory
		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		nop
		nop
		lwc1	fp16,4(t7)	# do the load
		nop			# and a nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp16		# fetch back the data we moved over
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back swc1 and bc1t
case27:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp111111	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp18		# send it to the FPU
		nop
		c.seq.s	fp18,fp18	# set the condition bit
		nop
		swc1	fp18,8(t7)	# do the store
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		lw	a0,8(t7)	# get what was stored
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated swc1 and bc1t
case28:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp999999	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp20		# send it to the FPU
		nop
		c.seq.s	fp20,fp20	# set the condition bit
		nop
		nop
		swc1	fp20,12(t7)	# do the store
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		lw	a0,12(t7)	# get what was stored
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

 #		li	t7,VME_OP_CBASE	# set test-operation base reg
		li	t7,VME_OP_UBASE	# set test-operation base reg

		# test back-to-back lwc1 and bc1t
case29:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp222222	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp22		# send it to the FPU
		nop
		mov.s	fp24,fp22	# use a move to get the other one
		c.seq.s	fp22,fp24	# set the condition bit
		li	v1,0x87868584	# create data to lwc1 later
		sw	v1,16(s7)	# out of reg into memory
		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		nop
		lwc1	fp26,16(t7)	# do the load
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp26		# fetch back the data we moved over
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated lwc1 and bc1t
case30:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp666666	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp28		# send it to the FPU
		nop
		mov.s	fp30,fp28	# use a move to get the other one
		c.seq.s	fp30,fp28	# set the condition bit
		li	v1,0x77767574	# create data to lwc1 later
		sw	v1,20(s7)	# out of reg into memory
		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		nop
		nop
		lwc1	fp1,20(t7)	# do the load
		nop			# and a nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp1		# fetch back the data we moved over
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back swc1 and bc1t
case31:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp999999	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp0		# send it to the FPU
		nop
		c.seq.s	fp0,fp0		# set the condition bit
		nop
		swc1	fp0,24(t7)	# do the store
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		lw	a0,24(t7)	# get what was stored
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test 1 nop separated swc1 and bc1t
case32:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp222222	# load up an operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp10		# send it to the FPU
		nop
		c.seq.s	fp10,fp10	# set the condition bit
		nop
		nop
		swc1	fp10,28(t7)	# do the store
		nop
		bc1t	1f		# followed by the bc1
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		delay40(v0)		# to make sure the WB's are empty
		delay40(v0)
		lw	a0,28(t7)	# get what was stored
		nop
		nop
		beq	v1,a0,1f	# did the lwc1 go ok
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test that a divide by zero generates the correct FP status and
		# an external interrupt
case33:
		lw	s0,machine_type
		nop
		beq	s0,BRDTYPE_R6300,3f	# Excalibur?
		nop

		li	v1,FP_ENABLE
		ctc1	v1,fcr31		# insure the "c-bit" is 0
						#   and all enable traps
		la	a0,dp1			# get addr of the 1st operand
		lw	t3,0(a0)		# load the 1st operand's msw
		lw	t4,4(a0)		# and get the lsw
		mtc1	t3,fp23		# move msw to FPU's DP f22
		mtc1	t4,fp22		# move lsw to FPU's DP f22
		la	a0,dp0			# get addr of the 2nd operand
		lw	t5,0(a0)		# load the 2nd operand's msw
		lw	t6,4(a0)		# and get the lsw
		mtc1	t5,fp25		# move msw to FPU's DP f24
		mtc1	t6,fp24		# move lsw to FPU's DP f24
		nop
		nop
		div.d	fp22,fp22,fp24	# f22 = 1.0 (fp22) / 0.0 (fp24)

		jal	ShortWait
		nop

		beq	s0,BRDTYPE_R6300,1f	# Excalibur?
		nop

		li	s1,CAUSE_IP6		# FP interrupt on level 3
		b	2f
		nop
1:
		li      s1,CAUSE_IP4		# FP interrupt on level 1
2:
		mfc0	v1,C0_CAUSE
		nop
		and	v1,s1
		bne	v1,zero,1f		# FP interrupt?
		nop

		b	FpuErr
		nop
1:
		cfc1	v1,fcr31	# get what's in the CSR now
		nop
		and	v0,v1,FP_EXC
		beq	v0,FP_EXC_Z,1f		# divide by zero exception?
		nop

		b	FpuErr
		nop
1:
		and	v0,v1,FP_STKY
		beq	s0,BRDTYPE_R6300,1f	# Excalibur?
		nop

		beq	v0,FP_STKY_Z,2f		# divide by zero sticky bit set?
		nop

		b	FpuErr
		nop
1:
		beq	v0,zero,2f		# divide by zero sticky bit not set?
		nop

		b	FpuErr
		nop
2:
		cfc1	v1,fcr31	# get what's in the CSR now
		nop
		and	v1,~FP_EXC_Z
		ctc1	v1,fcr31
		nop
		jal	ShortWait
		nop

		mfc0	v1,C0_CAUSE
		nop
		and	v1,s1
		beq	v1,zero,1f		# FP interrupt cleared?
		nop

		b	FpuErr
		nop
1:
#ifdef	USED
		mfc1	v0,fp23		# move msw from FPU's DP f22
		mfc1	v1,fp22		# move lsw from FPU's DP f22
		nop
		nop
		bne	t3,v0,FpuErr		# did destination msw get clobbered?
		nop
		bne	t4,v1,FpuErr		# did destination lsw get clobbered?
		nop

		mfc1	v0,fp25		# move msw from FPU's DP f24
		mfc1	v1,fp24		# move lsw from FPU's DP f24
		nop
		nop
		bne	t5,v0,FpuErr		# did source msw get clobbered?
		nop
		bne	t6,v1,FpuErr		# did source lsw get clobbered?
		nop
#endif	USED

3:
		jal	ShortWait
		nop

		la	a0,success
		j	LightsOut
		nop

END(Pon_Fp1)


LEAF(LightsOut)

		jal	pon_puts
		nop

		jal	ResetFpu
		nop

		lw	ra,0(sp)
		lw	s0,4(sp)
		lw	s1,8(sp)
		lw	s2,12(sp)
		lw	s3,16(sp)
		lw	s4,20(sp)
		lw	s5,24(sp)
		lw	s6,28(sp)
		lw	s7,32(sp)
		addu	sp,FPFRM
		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		move	v0,zero
		j	ra
		nop

END(LightsOut)


 # FpuErr flashes the current value of the the FPU leds (in t4) several times
 # to indicate an error.

LOCAL(FpuErr)

		jal	ResetFpu
		nop

		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		nop

		la	a0,failure
		jal	pon_puts
		nop

		li	a0,PON_FP1
		jal	FastFlash
		nop

#ifndef	R3030
		li	a0,PON_FP1
		jal	pon_set_leds
		nop
#endif	!R3030

XLEAF(FpuNoRun)

		li	a0,PON_FAULT_FP
		jal	SetDepend
		nop

		li	v0,1		# set fail code

XLEAF(NoFpu)

		lw	ra,0(sp)
		lw	s0,4(sp)
		lw	s1,8(sp)
		lw	s2,12(sp)
		lw	s3,16(sp)
		lw	s4,20(sp)
		lw	s5,24(sp)
		lw	s6,28(sp)
		lw	s7,32(sp)
		addu	sp,FPFRM
		j	ra
		nop

END(FpuErr)


 # ResetFpu asserts the FPU reset line that is part of the CPU's LED register
 # in the M-Series only; the signal is active-low.

LEAF(ResetFpu)

#ifndef	R3030
		.set	noat
		lw	v0,machine_type
		li	AT,BRDTYPE_R2400
		beq	v0,AT,2f	# if M120
		nop
		li	AT,BRDTYPE_R6300
		beq	v0,AT,2f	# if R6000
		nop
		.set	at

		li	v0,PON_FP1
		sb	v0,LED_REG_R2300|K1BASE
		nop
		move	v0,zero
		delay32(v0)		# burn a few clocks (dunno if needed)
		li	v0,0xC0|PON_FP1
		sb	v0,LED_REG_R2300|K1BASE
		nop
2:
#endif	!R3030
		j	ra
		nop

END(ResetFpu)


#ifndef	R3030
/*
 *  FpuHandlerR6000  --  exception handler for R6000
 *
 *  Accessing a nonexistent Cp1 produces a Machine Check exception.  If we get
 *  here because of that (or because of any other exception when accessing
 *  Cp1), then return a zero in v0 (as if the RevId register is zero) and
 *  return to the instruction following the one causing the exception.
 *
 *  Uses v0, t0
 */
LEAF(FpuHandlerR6000)

		.set	noreorder
		mfc0	v0,C0_ERROR
		mfc0	t0,C0_EPC		# (LDSLOT)
		mtc0	v0,C0_ERROR		# turn off any Error bits
		addi	t0,4
		j	t0			# return to EPC+4
		li	v0,0			# (BDSLOT) and make v0 zero
		.set	reorder

END(FpuHandlerR6000)
#endif	R3030


LEAF(FpuHandlerM20)

		.set	noreorder
		mfc0	t0,C0_EPC
		nop
		addi	t0,4
		j	t0			# return to EPC+4
		li	v0,0			# (BDSLOT) and make v0 zero
		.set	reorder

END(FpuHandlerM20)


 # Reads the configuration register and check for a FPU.  Returns zero if one
 # is found.

LEAF(FindFpu)

		.set	noreorder
		mfc0	v1,C0_SR
		nop
		and	v1,SR_BEV
		or	v1,SR_CU1		# make FPU ops street-legal
		mtc0	v1,C0_SR
		.set	reorder

#ifndef	R3030
		lw	v0,machine_type
		beq	v0,BRDTYPE_R2400,2f	# if M120
		beq	v0,BRDTYPE_M180,2f	# if M180
		beq	v0,BRDTYPE_R3200,1f	# if M2000
		beq	v0,BRDTYPE_RB3125,1f	# if GENESIS
		beq	v0,BRDTYPE_R6300,6f	# if R6000

		lbu	v0,K1BASE|CPU_CONFIG
		and	v0,CONFIG_NOCP1
		b	3f
1:
		lw	v0,K1BASE|CPU_CR_M2000
		and	v0,CR_FPPRES
		b	3f
2:
		lhu	v0,K1BASE|SCR
		and	v0,SCR_NOFPP
		b	3f
6:
		la	fp,FpuHandlerR6000	# set up to silently handle
		or	t0,v1,SR_BEV		#  a Machine Check exception
		.set	noreorder
		mtc0	t0,C0_SR
		nop
		nop
		cfc1	v0,$0			# try reading Id register
		nop				# if nonzero, then exists!
		nop
		mtc0	v1,C0_SR		# restore SR
		bne	v0,zero,3f
		li	v0,0			# (BDSLOT) return zero if exists
		li	v0,1			# otherwise, return nonzero
		.set	reorder
		b	3f
#else	!R3030
7:
		move	t0,ra			# save ra
		jal	GetDepend		# get dependency

		move	ra,t0			# restore ra
		and	v0,PON_FAULT_KEYBOARD	# 8042 OK?
		beq	v0,zero,8f		#   yes, use the 8042 to check
						#   for a FPU

		la	fp,FpuHandlerM20	# set fault handler
		.set	noreorder
		mfc0	v1,C0_SR
		nop
		or	t0,v1,SR_BEV
		mtc0	t0,C0_SR
		nop
		nop
		cfc1	v0,$0			# try reading ID register
		nop
		nop
		mtc0	v1,C0_SR		# restore SR
		bne	v0,zero,3f		# if nonzero, then FPU exists
		li	v0,0			# (BDSLOT) return zero if exists
		li	v0,1			# otherwise, return nonzero
		.set	reorder
		b	3f
8:
		move	v1,ra			# save ra
		jal	GetKIntR

		move	ra,v1			# restore ra

		and	v0,P1_FPU_PRSNT_B	# 0 = FPU present in the system
#endif	!R3030
3:
		.set	noreorder
		mfc0	v1,C0_SR
		nop
		and	v1,~SR_CU1		# make FPU ops NON-street-legal
		mtc0	v1,C0_SR
		.set	reorder
		j	ra

END(FindFpu)

		.data

begintest:
		.asciiz	"FP Test #1..."
#ifdef	DEBUG
start:
		.asciiz	"Start of FP Test #1\r\n"
found:
		.asciiz	"Found FPU\r\n"
notfound:
		.asciiz	"No FPU found\r\n"
#endif	DEBUG
