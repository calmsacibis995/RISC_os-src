#ident "$Header: pon_fp2.s,v 1.9.1.1 90/07/18 14:31:28 huang Exp $"
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

#include "machine/dregdef.h"
#include "machine/asm.h"
#include "machine/standard.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/delaymacs.h"
#include "pon.h"

#define	LOCAL(x) \
	.ent	x,0; \
x:; \
	.frame	sp,0,ra

#ifdef SABLE
#define	LongWaitCache	1
#define	ShortWaitCache	1
#else
#define	LongWaitCache	8000000
#define	ShortWaitCache	500000
#endif !SABLE

#define MTC1_R0_CSR	.word	0x44C0F800
#define MTC1_R12_IRR	.word	0x44CC0000

#define	VME_OP_CBASE	0x80100000	# *cached* space
#define	VME_OP_UBASE	0xA0100000	# uncached space
#define	VME_MV_BASE	0xA0100000	# uncached space


		.extern	success
		.extern	failure
		.extern	skipped
		.extern	SR_IBIT_FP
		.extern	FpuNoRun
		.data

		.align	2
sdata:
 #sp0:		.float	0.0
 #sp1:		.float	1.0
 #sp2:		.float	2.0
 #sp3:		.float	3.0
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
 #sp222222:	.float	222222.0
sp333333:	.float	333333.0
 #sp444444:	.float	444444.0
sp555555:	.float	555555.0
 #sp666666:	.float	666666.0
sp777777:	.float	777777.0
sp888888:	.float	888888.0
 #sp999999:	.float	999999.0

		.align	3
ddata:
 #dp0:		.double	0.0
 #dp1:		.double	1.0
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
 #dp111111111111:	.double 111111111111.0
 #dp222222222222:	.double 222222222222.0
dp333333333333:	.double 333333333333.0
 #dp444444444444:	.double 444444444444.0
 #dp555555555555:	.double 555555555555.0
 #dp666666666666:	.double 666666666666.0
 #dp777777777777:	.double 777777777777.0
 #dp888888888888:	.double 888888888888.0
dp999999999999:	.double 999999999999.0

		.text
		.set	noreorder

FPFRM=		(4 * 8) + 4
NESTED(Pon_Fp2, FPFRM, zero)

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
		li	a0,PON_FP2
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

		jal	FpuNoRun
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
		nop			#  mask for this machine type
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
		li	t3,VME_MV_BASE		# set data-relocation base reg

		# test back-to-back bc1t and mtc1
case1:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp111111	# load up both operands
		lw	v1,0(v1)
		nop
		mtc1	v1,fp0		# send them to the FPU
		mtc1	v1,fp30
		nop
		c.seq.s	fp30,fp0	# set the c-bit for the bc1t
		li	v1,0x27262524	# this be the moveto data
		nop
		bc1t	1f		# here's the branch
		nop

		nop
		b	FpuErr
1:
		nop

		mtc1	v1,fp31		# mtc1 in the slot
		jal	ShortWait
		nop

		mfc1	a0,fp31		# get back what we put in the FPR
		nop
		beq	v1,a0,1f	# mtc data get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1t and mfc1
case2:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp333333	# load up one operand
		lw	v1,0(v1)
		nop
		mtc1	v1,fp2		# send it to the FPU
		nop
		neg.s	fp4,fp2		# try out "negs"
		neg.s	fp6,fp4
		c.seq.s	fp6,fp2		# should set the c-bit for the bc1t
		li	v1,0x37363534	# this be the movefrom data
		mtc1	v1,fp7		# ship it to the FPU
 		bc1t	1f		# here's the branch
 		nop

		b	FpuErr
 		nop

1:		nop
		mfc1	a0,fp7		# mfc1 in the slot
		jal	ShortWait
		nop

		beq	v1,a0,1f	# mfc data get stepped on
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1t and COMPS
case3:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	v1,sp777777	# load up both operands
		lw	v1,0(v1)
		nop
		mtc1	v1,fp8		# send it to the FPU
		nop
		neg.s	fp10,fp8	# try neg
		abs.s	fp12,fp10	# try abs
		nop
		bc1f	1f		# here's the branch (c-bit = 0)
		nop

		nop
		b	FpuErr
1:
		nop

		c.seq.s	fp8,fp12	# should set c-bit = 1 for next bc1t
		jal	ShortWait
		nop

		bc1t	1f		# see if the COMPS worked
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a0,fp8		# get back both COMPS operands
		mfc1	a1,fp12		#   to see if they got trashed
		nop
		beq	v1,a0,1f	# check on one operand
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,a1,1f	# now check on the other one
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1t and COMPD
case4:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		la	a0,dp999999999999	# get addr of both operands
		lw	v1,0(a0)	 	# load both operand's msw
		lw	a0,4(a0)		#   and get the lsw
		mtc1	v1,fp15			# move msw to FPU's DP f14
		mtc1	a0,fp14			# move lsw to FPU's DP f14
		nop
		neg.d	fp16,fp14	# try a couple of negs
		neg.d	fp18,fp16
		nop
		bc1f	1f		# here's the branch (c-bit = 0)
		nop

		nop
		b	FpuErr
1:
		nop

		c.seq.d	fp14,fp18	# should set c-bit = 1 for next bc1t
		jal	ShortWait
		nop

		bc1t	1f		# see if the COMPD worked
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a2,fp14		# fetch back the operands
		mfc1	a3,fp15		#   ...
		mfc1	t0,fp18		#      ...
		mfc1	t1,fp19		#         to see if they got stepped on

		beq	v1,a3,1f	# check on one operand  (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,a2,1f	# check on one operand  (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	v1,t1,1f	# check on other operand  (msw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		beq	a0,t0,1f	# check on other operand  (lsw)
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1t and FOPS
case5:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		la	v1,sp333333	# load up the 1st operand
		lw	v1,0(v1)
		la	a0,sp555555	# load up the 2nd operand
		lw	a0,0(a0)
		la	a1,sp888888	# load up the expected results
		lw	a1,0(a1)
		mtc1	v1,fp20		# send them to the FPU
		mtc1	a1,fp24
		mtc1	a0,fp22
		nop
		bc1t	1f		# here's the branch (c-bit = 1)
		nop

		nop
		b	FpuErr
1:
		nop

		add.s	fp26,fp20,fp22	# do the FOPS in the branch-delay slot
		jal	ShortWait
		nop

		c.seq.s	fp24,fp26	# check the results of the FOPS
		mfc1	a2,fp20		# fetch back one operand
		bc1t	1f		# see if the FOPS worked
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		mfc1	a3,fp22		# fetch other operand back
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

		jal	FPUclear	# zero the FPR's
		nop

		# test back-to-back bc1t and FOPD
case6:
		li	v0,0x00800000
		ctc1	v0,fcr31		# insure the "c-bit" is 1
		la	a2,dp333333333333	# get addr of the exp results
		lw	a1,0(a2)	 	# load the exp result's msw
		lw	a2,4(a2)		#   and get the lsw
		la	a0,dp999999999999	# get addr of the 1st operand
		lw	v1,0(a0)	 	# load the 1st operand's msw
		lw	a0,4(a0)		#   and get the lsw
		la	t6,dp3			# get addr of the 2nd operand
		lw	t5,0(t6)	 	# load the 2nd operand's msw
		lw	t6,4(t6)		#   and get the lsw
		li	s7,0xA9A8A7A6		# data for a movefrom between
		mtc1	s7,fp4			#   a COMP and a bc1t/f
		mtc1	a1,fp3			# move msw to FPU's DP f2
		mtc1	a2,fp2			# move lsw to FPU's DP f2
		mtc1	v1,fp29			# move msw to FPU's DP f28
		mtc1	a0,fp28			# move lsw to FPU's DP f28
		mtc1	t5,fp31		# move msw to FPU's DP f30
		mtc1	t6,fp30		# move lsw to FPU's DP f30
		neg.d	fp28,fp28	# negate the operands
		neg.d	fp30,fp30	#   for the hell of it
		nop
		bc1t	1f		# here's the branch (c-bit = 1)
		nop

		nop
		b	FpuErr
1:
		nop

		div.d	fp0,fp28,fp30	# do the FOPD in the branch-delay slot
		jal	ShortWait
		nop

		c.seq.d	fp0,fp2		# check the results of the FOPD
		mfc1	a2,fp4		# fetch back the independent data
		bc1t	1f		# see if the FOPD worked
		nop

		b	FpuErr
1:
		nop

		jal	ShortWait
		nop

		beq	a2,s7,1f	# check the independent data
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		neg.d	fp30,fp30	# negate the operands back
		neg.d	fp28,fp28	#   to their supposed initial values
		mfc1	a2,fp28		# fetch back the operands
		mfc1	a3,fp29		#   ...
		mfc1	t0,fp30		#      ...
		mfc1	t1,fp31		#         to see if they got stepped on
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

		# test back-to-back bc1t and lwc1
case7:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		li	v1,0x11223344	# create data for the lwc1
		sw	v1,0(t3)	# get it out into memory
		delay40(v0)		# wait for the WB's to do it
		delay40(v0)
		nop
		bc1f	1f		# here's the branch
		nop

		nop
		b	FpuErr
1:
		nop

		lwc1	fp4,0(t7)	# lwc1 in the slot
		jal	ShortWait
		nop

		mfc1	a0,fp4		# get back what loaded into the FPU
		nop
		beq	v1,a0,1f	# is what was in memory what we got
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1t and swc1
case8:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		li	v1,0x55667788	# create data for the swc1
		mtc1	v1,fp5		# ship it over
		nop
		bc1t	1f		# here's the branch
		nop

		nop
		b	FpuErr
1:
		nop

		swc1	fp5,4(t7)	# swc1 in the slot
		jal	ShortWait
		nop

		lw	a0,4(t7)	# get back what the FPU stored
		nop
		beq	v1,a0,1f	# is what was in memory what we shipped
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		li	t7,VME_OP_CBASE	# set test-operation  base reg

		# test back-to-back bc1t and lwc1
case9:
		li	v0,0x00800000
		ctc1	v0,fcr31	# insure the "c-bit" is 1
		li	v1,0x778899AA	# create data for the lwc1
		sw	v1,8(t3)	# get it out into memory
		delay40(v0)		# wait for the WB's to do it
		delay40(v0)
		nop
		bc1t	1f		# here's the branch
		nop

		nop
		b	FpuErr
1:
		nop

		lwc1	fp7,8(t7)	# lwc1 in the slot
		jal	ShortWait
		nop

		mfc1	a0,fp7		# get back what loaded into the FPU
		nop
		beq	v1,a0,1f	# is what was in memory what we got
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		# test back-to-back bc1f and swc1
case10:
		ctc1	zero,fcr31	# insure the "c-bit" is 0
		li	v1,0xE1D2C3B4	# create data for the swc1
		mtc1	v1,fp6		# ship it over
		nop
		bc1f	1f		# here's the branch
		nop

		nop
		b	FpuErr
1:
		nop

		swc1	fp6,12(t7)	# swc1 in the slot
		jal	ShortWait
		nop

		lw	a0,12(t7)	# get back what the FPU stored
		nop
		beq	v1,a0,1f	# is what was in memory what we shipped
		nop

		b	FpuErr
		nop
1:
		jal	ShortWait
		nop

		la	a0,success
		j	LightsOut
		nop

END(Pon_Fp2)


 # FPU error is indicated by flashing the CPU LEDs.

LOCAL(FpuErr)

		jal	ResetFpu
		nop

		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		nop

		la	a0,failure
		jal	pon_puts
		nop

		li	a0,PON_FP2
		jal	FastFlash
		nop

#ifndef	R3030
		li	a0,PON_FP2
		jal	pon_set_leds
		nop
#endif	!R3030

		jal	FpuNoRun

END(FpuErr)


 # LongWait provides a delay-loop of approximately 1 second when running from
 # cache;  the return address is assumed to be in ra;  v0 is destroyed.

LEAF(LongWait)

		li 	v0,LongWaitCache	# get the delay count
1:
		bgtz	v0,1b			# spin till it be 0
		addiu	v0,v0,-1		# decrement the count

		j	ra
		nop

END(LongWait)


 # ShortWait provides a delay-loop of approximately 1/4 second when running
 # from cache;  the return address is assumed to be in ra;  v0 is destroyed.

LEAF(ShortWait)

 #		li 	v0,ShortWaitCache	# get the delay count
 #1:		bgtz	v0,1b			# spin till it be 0
 #		addiu	v0,v0,-1		# decrement the count

		nop
		nop
		nop
		j	ra
		nop

END(ShortWait)


 # FPUclear sets all the FGR's to binary 0 using move-to instructions.

LEAF(FPUclear)

		mtc1	zero,$f0
		mtc1	zero,$f1
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
		j	ra
		mtc1	zero,$f31

END(FPUclear)

		.data

begintest:
		.asciiz	"FP Test #2..."

#ifdef	DEBUG
start:
		.asciiz	"Start of FP Test #2\r\n"
found:
		.asciiz	"Found FPU\r\n"
notfound:
		.asciiz	"No FPU found\r\n"
#endif	DEBUG
