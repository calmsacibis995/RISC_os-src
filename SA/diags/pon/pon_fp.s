#ident "$Header: pon_fp.s,v 1.2.7.1 90/07/18 14:31:18 huang Exp $"
/* $Copyright$ */

#include "machine/dregdef.h"
#include "machine/standard.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/delaymacs.h"
#include "machine/asm.h"
#include "pon.h"
#undef  PHYS_TO_K1(x)
#define	PHYS_TO_K1(x)	((x)|0xA0000000)	/* physical to kseg1 */

#define  MTC1_R0_CSR	.word 0x44C0F800

#define	VME_OP_CBASE	0x80100000	# *cached* space
#define	VME_OP_UBASE	0xA0100000	# uncached space
#define	VME_MV_BASE	0xA0100000	# uncached space

		.data

		.align	2
sdata:
sp0:		.float	0.0
sp1:		.float	1.0
sp2:		.float	2.0
sp3:		.float	3.0
sp4:		.float	4.0
sp5:		.float	5.0
sp6:		.float	6.0
sp7:		.float	7.0
sp8:		.float	8.0
sp9:		.float	9.0
sp10:		.float	10.0
sp11:		.float	11.0
sp12:		.float	12.0
sp111111:	.float	111111.0
sp222222:	.float	222222.0
sp333333:	.float	333333.0
sp444444:	.float	444444.0
sp555555:	.float	555555.0
sp666666:	.float	666666.0
sp777777:	.float	777777.0
sp888888:	.float	888888.0
sp999999:	.float	999999.0

		.align	3
ddata:
dp0:		.double	0.0
dp1:		.double	1.0
dp2:		.double	2.0
dp3:		.double	3.0
dp4:		.double	4.0
dp5:		.double	5.0
dp6:		.double	6.0
dp7:		.double	7.0
dp8:		.double	8.0
dp9:		.double	9.0
dp10:		.double	10.0
dp11:		.double	11.0
dp12:		.double	12.0
dp111111111111:	.double 111111111111.0
dp222222222222:	.double 222222222222.0
dp333333333333:	.double 333333333333.0
dp444444444444:	.double 444444444444.0
dp555555555555:	.double 555555555555.0
dp666666666666:	.double 666666666666.0
dp777777777777:	.double 777777777777.0
dp888888888888:	.double 888888888888.0
dp999999999999:	.double 999999999999.0

		.text
		.set	noreorder

LEAF(Pon_FP)
		move	k1,ra
		jal	findfp		# test for CP1, chip or board
		nop
 		bne	v0,zero,1f	# if non-zero we've got a CP1
 #		la	a0,noboard
 #		jal	pon_puts
		nop
		move	v0,zero		# No FPU doesn't mean an ERROR
		j	k1		# and return
		nop
1:
		addu	sp,sp,-40
		sw	k1,0(sp)
		sw	s0,4(sp)
		sw	s1,8(sp)
		sw	s2,12(sp)
		sw	s3,16(sp)
		sw	s4,20(sp)
		sw	s5,24(sp)
		sw	s6,28(sp)
		sw	s7,32(sp)
		sw	fp,36(sp)

 		li	v1,2		# see if an FP chip was found
 		bne	v0,v1,1f
 #		la	a0,fpuchip
 #		jal	pon_puts
		nop
		li	a0,PON_FP_START	# "we've only just begun..."
		jal	pon_set_leds	# write to the CPU LEDS
		nop
		jal	Pon_fp_chip
		nop
		b	chip_done
1:
 #		la	a0,fpuboard	# looks like a board was found
 #		jal	pon_puts
		nop
		jal	ResetFPU	# do a h/w reset of the FPU board
		nop
		li	a0,PON_FP_START	# "we've only just begun..."
		jal	pon_set_leds	# write to the CPU LEDS
		nop

 #		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		nop
		nop

		li	r15,VME_OP_UBASE	# set test-operation  base reg
		li	r16,VME_MV_BASE		# set data-relocation base reg

		# see if all bits in the EIR "work"
case2:
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r3,0xAAAAAAAA	# Pattern, the First
		li	r4,0x55555555	# Pattern, the Second
		ctc1	r3,fcr30	# stuff 1st pattern into the EIR
		nop
		cfc1	r5,fcr30	# fetch it back
		ctc1	r4,fcr30	# now write the other pattern
		nop
		cfc1	r6,fcr30	# fetch back pattern #2
		nop

		beq	r3,r5,1f	# is pattern #1 correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r6,1f	# is pattern #2 correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if all bits in the CSR "work"
case3:
		li	r3,0xAAAAAAAA	# Pattern, the First
		li	r4,0x55555555	# Pattern, the Second
		li	r5,0x0083FFFF	# implemented bits mask
		ctc1	r3,fcr31	# stuff 1st pattern into the CSR
		and	r3,r3,r5	# turn pattern #1 into expected #1
		cfc1	r6,fcr31	# fetch back what got stored
		cfc1	r6,fcr31	# fetch back what got stored
		ctc1	r4,fcr31	# now store the other pattern
		and	r4,r4,r5	# turn pattern #2 into expected #2
		cfc1	r7,fcr31	# fetch back pattern #2
		cfc1	r7,fcr31	# fetch back pattern #2
		nop

		beq	r3,r6,1f	# is pattern #1 correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r7,1f	# is pattern #2 correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a FOPS gets stored in the EIR correctly
case4:
		la	r3,sp111111	# get the address of a couple of
		la	r4,sp222222	#   operands
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp28,0(r3)	# get the operands
		l.s	fp30,0(r4)
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,FOPS		# get the address of the FOPS
		lw	r5,0(r5)	# get the FOPS
		li	r6,0x11223344	# create a pattern to ship around
		nop
		nop
FOPS:
		add.s	fp26,fp28,fp30	# do the FOPS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp27		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		swc1	fp27,0(r15)	#	...
		mfc1	r7,fp27		#	   ...
		lwc1	fp31,0(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a FOPD gets stored in the EIR correctly
case5:
		la	r3,dp999999999999 # get the address of a couple of
		la	r4,dp3		  #   operands
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp24,0(r3)	  # get the operands
		l.d	fp22,0(r4)
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,FOPD		# get the address of the FOPD
		lw	r5,0(r5)	# get the FOPD
		li	r6,0x55667788	# create a pattern to ship around
		nop
		nop
FOPD:
		div.d	fp20,fp24,fp22	# do the FOPD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp0		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp0		#	...
		swc1	fp0,4(r15)	#	   ...
		lwc1	fp0,4(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a COMPD gets stored in the EIR correctly
case6:
		la	r3,dp111111111111 # get the address of a couple of
		la	r4,dp222222222222 #   operands
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp18,0(r3)	  # get the operands
		l.d	fp16,0(r4)
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,COMPD	# get the address of the COMPD
		lw	r5,0(r5)	# get the COMPD
		li	r6,0x99AABBCC	# create a pattern to ship around
		nop
		nop
COMPD:
		c.seq.d	fp18,fp16	# do the COMPD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp1		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp1		#	...
		swc1	fp1,8(r15)	#	   ...
		lwc1	fp1,8(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a COMPS gets stored in the EIR correctly
case7:
		la	r3,sp777777	# get the address of a couple of
		la	r4,sp555555	#   operands
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp14,0(r3)	# get the operands
		l.s	fp12,0(r4)
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,COMPS	# get the address of the COMPS
		lw	r5,0(r5)	# get the COMPS
		li	r6,0xE9E8E7E6	# create a pattern to ship around
		nop
		nop
COMPS:
		c.seq.s	fp12,fp14	# do the COMPS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,12(r15)	#	   ...
		lwc1	fp31,12(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MOVD gets stored in the EIR correctly
case8:
		la	r3,dp555555555555 # get the address of an operand
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp10,0(r3)	  # get the operand
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,MOVD		# get the address of the MOVD
		lw	r5,0(r5)	# get the MOVD
		li	r6,0xBABADADA	# create a pattern to ship around
		nop
		nop
MOVD:
		mov.d	fp8,fp10	# do the MOVD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp20		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp20		#	...
		swc1	fp20,16(r15)	#	   ...
		lwc1	fp20,16(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MOVS gets stored in the EIR correctly
case9:
		la	r3,sp333333	# get the address of an operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp4,0(r3)	# get the operand
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,MOVS		# get the address of the MOVS
		lw	r5,0(r5)	# get the MOVS
		li	r6,0xBEEFDEAD	# create a pattern to ship around
		nop
		nop
MOVS:
		mov.s	fp6,fp4		# do the MOVS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,20(r15)	#	   ...
		lwc1	fp31,20(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an ABSS gets stored in the EIR correctly
case10:
		la	r3,sp111111	# get the address of an operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp2,0(r3)	# get the operand
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,ABSS		# get the address of the ABSS
		lw	r5,0(r5)	# get the ABSS
		li	r6,0xBAD0BEEF	# create a pattern to ship around
		nop
		nop
ABSS:
		abs.s	fp2,fp2		# do the ABSS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,24(r15)	#	   ...
		lwc1	fp31,24(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an ABSD gets stored in the EIR correctly
case11:
		la	r3,dp999999999999 # get the address of an operand
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp0,0(r3)	  # get the operand
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,ABSD		# get the address of the ABSD
		lw	r5,0(r5)	# get the ABSD
		li	r6,0xBEEF0BAD	# create a pattern to ship around
		nop
		nop
ABSD:
		abs.d	fp0,fp0		# do the ABSD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,28(r15)	#	   ...
		lwc1	fp31,28(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an NEGS gets stored in the EIR correctly
case12:
		la	r3,sp333333	# get the address of an operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp30,0(r3)	# get the operand
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,NEGS		# get the address of the NEGS
		lw	r5,0(r5)	# get the NEGS
		li	r6,0x17273747	# create a pattern to ship around
		nop
		nop
NEGS:
		neg.s	fp30,fp30	# do the NEGS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,32(r15)	#	   ...
		lwc1	fp31,32(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an NEGD gets stored in the EIR correctly
case13:
		la	r3,dp999999999999 # get the address of an operand
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp28,0(r3)	  # get the operand
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,NEGD		# get the address of the NEGD
		lw	r5,0(r5)	# get the NEGD
		li	r6,0x57677787	# create a pattern to ship around
		nop
		nop
NEGD:
		neg.d	fp28,fp28	# do the NEGD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp31		#	...
		swc1	fp31,36(r15)	#	   ...
		lwc1	fp31,36(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an CVTDS gets stored in the EIR correctly
case14:
		la	r3,sp333333	# get the address of an operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		l.s	fp28,0(r3)	# get the operand
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,CVTDS	# get the address of the CVTDS
		lw	r5,0(r5)	# get the CVTDS
		li	r6,0x97A7B7C7	# create a pattern to ship around
		nop
		nop
CVTDS:
		cvt.d.s	fp28,fp28	# do the CVTDS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		swc1	fp31,40(r15)	#	...
		mfc1	r7,fp31		#	   ...
		lwc1	fp31,40(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an CVTSD gets stored in the EIR correctly
case15:
		la	r3,dp999999999999 # get the address of an operand
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp26,0(r3)	  # get the operand
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,CVTSD	# get the address of the CVTSD
		lw	r5,0(r5)	# get the CVTSD
		li	r6,0xD7E7F707	# create a pattern to ship around
		nop
		nop
CVTSD:
		cvt.s.d	fp26,fp26	# do the CVTSD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp31		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		swc1	fp31,44(r15)	#	...
		mfc1	r7,fp31		#	   ...
		lwc1	fp31,44(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MULS gets stored in the EIR correctly
case16:
		la	r3,sp333333	# get the address of an operand
		l.s	fp0,0(r3)	# get the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		la	r5,MULS		# get the address of the MULS
		lw	r5,0(r5)	# get the MULS
		li	r6,0x11223344	# create a pattern to ship around
		nop
		nop
MULS:
		mul.s	fp0,fp0,fp0	# do the MULS
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp27		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		swc1	fp27,48(r15)	#	...
		mfc1	r7,fp27		#	   ...
		lwc1	fp31,48(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MULD gets stored in the EIR correctly
case17:
		la	r3,dp333333333333 # get the address of a couple of
		la	r4,dp3		  #   operands
		ctc1	r0,fcr31	  # clear CSR to insure loadable EIR
		l.d	fp4,0(r3)	  # get the operands
		l.d	fp2,0(r4)
		ctc1	r0,fcr30	# stuff 0's in the EIR
		la	r5,MULD		# get the address of the MULD
		lw	r5,0(r5)	# get the MULD
		li	r6,0x55667788	# create a pattern to ship around
		nop
		nop
MULD:
		mul.d	fp2,fp4,fp2	# do the MULD
		cfc1	r9,fcr30	# get the EIR contents immediately
		mtc1	r6,fp0		# do all ops that should NOT cause
		ctc1	r0,fcr31	#   the EIR to get loaded
		mfc1	r7,fp0		#	...
		swc1	fp0,52(r15)	#	   ...
		lwc1	fp0,52(r15)	#	      ...
		bc1t	1f		#		 ...
1:
		nop
		nop
		cfc1	r8,fcr30	# get the EIR contents afterwards
		nop

		beq	r5,r9,1f	# is the immediate EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r8,1f	# is the delayed EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a division by 0 (Z) gives us an unimplemented op
		# (E) rupt in single-precision; CSR-bits initially all 0's

case18:
		la	r3,sp333333	# get the addresses of the operands
		la	r4,sp0
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operands
		lw	r4,0(r4)
		mtc1	r3,fp0		# ship 'em to the FPU
		mtc1	r4,fp2
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,DIV0z	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46020103	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xA1A2A3A4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
DIV0z:
		div.s	fp4,fp0,fp2	# do the divide by zero
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp0		# get the source FPR (operand 1)
		mfc1	r14,fp2		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a division by 0 (Z) gives us an unimplemented op
		# (E) rupt in single-precision; most CSR-bits initially all 1's
		# [exception bits VZOUI all 1's; enable bits VZOUI all 0's]
case19:
		la	r3,sp555555	# get the addresses of the operands
		la	r4,sp0
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operands
		lw	r4,0(r4)
		mtc1	r3,fp2		# ship 'em to the FPU
		mtc1	r4,fp4
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0x00FDF07F	# put the 1's in the CSR *except* for
		ctc1	r5,fcr31	#   the E-bit, which we should set,
					#   and the enable bits
 #		la	r5,DIV0o	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46041183	# load the instruction op code.
		li	r6,0x0082007F	# create the expected CSR
		li	r7,0xB1B2B3B4	# create a pattern for the destination
		mtc1	r7,fp6		# ship it over
		nop
		nop
DIV0o:
		div.s	fp6,fp2,fp4	# do the divide by zero
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp6		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR (operand 1)
		mfc1	r14,fp4		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a 0 divided by 0 (V) gives us an unimplemented op
		# (E) rupt in single-precision; CSR-bits initially all 0's
case20:
		la	r3,sp0		# get the addresses of the operands
		la	r4,sp0
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operands
		lw	r4,0(r4)
		mtc1	r3,fp0		# ship 'em to the FPU
		mtc1	r4,fp2
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,DIV0by0z	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46020103	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xC1C2C3C4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
DIV0by0z:
		div.s	fp4,fp0,fp2	# do the divide of zero by zero
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp0		# get the source FPR (operand 1)
		mfc1	r14,fp2		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a 0 divided by 0 (V) gives us an unimplemented op
		# (E) rupt in single-precision; most CSR-bits initially all 1's
		# [exception bits VZOUI all 0's; enable bits VZOUI all 1's]
case21:
		la	r3,sp0		# get the addresses of the operands
		la	r4,sp0
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operands
		lw	r4,0(r4)
		mtc1	r3,fp2		# ship 'em to the FPU
		mtc1	r4,fp4
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0x00FC0FFF	# put the 1's in the CSR *except* for
		ctc1	r5,fcr31	#   the E-bit, which we should set,
					#   and the exception bits
 #		la	r5,DIV0by0o	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46041183	# load the instruction op code.
		li	r6,0x00820FFF	# create the expected CSR
		li	r7,0xD1D2D3D4	# create a pattern for the destination
		mtc1	r7,fp6		# ship it over
		nop
		nop
DIV0by0o:
		div.s	fp6,fp2,fp4	# do the divide of zero by zero
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp6		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR (operand 1)
		mfc1	r14,fp4		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; CSR-bits initially all 0's
case22:
		la	r3,sp333333	# get the address an operand
		lw	r3,0(r3)	# get the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp0		# ship it
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,SQRTz	# get the address of the sqrt
 #		lw	r5,0(r5)	# get the sqrt instruction (exp EIR)
		li	r5,0x46000084	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xE1E2E3E4	# create a pattern for the destination
		mtc1	r7,fp2		# ship it over
		nop
		nop
SQRTz:
		sqrt.s	fp2,fp0		# do the sqrt op that we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp2		# get the destination FPR
		mfc1	r13,fp0		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 10101; enable bits VZOUI = 01010]
case23:
		la	r3,sp999999	# get the address of the operand
		lw	r3,0(r3)	# get the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp2		# ship it
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0x00FD557F	# put all 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set,
					#   and alternate exception/enable bits
 #		la	r5,SQRTo	# get the address of the sqrt
 #		lw	r5,0(r5)	# get the sqrt instruction (exp EIR)
		li	r5,0x46001104	# load the instruction op code.
		li	r6,0x0082057F	# create the expected CSR
		li	r7,0xF1F2F3F4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTo:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got again
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an overflow operation (O) gives us an unimplemented
		# op (E) rupt in single-precision; CSR-bits initially all 0's
case24:
		li	r3,0x7F7FFFFF	# gen the max SP float operand
		addu	r4,r0,r3
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp0		# ship it to the FPU
		mtc1	r4,fp2		#   to be both operands
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,ADDOV	# get the address of the add
 #		lw	r5,0(r5)	# get the add instruction (exp EIR)
		li	r5,0x46020100	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0x0F0E0D0C	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
ADDOV:
		add.s	fp4,fp0,fp2	# do the overflowing add (MAX + MAX)
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp0		# get the source FPR (operand 1)
		mfc1	r14,fp2		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an overflow operation (O) gives us an unimplemented
		# op (E) rupt in single-precision; many CSR-bits initially 1's
		# [exception bits VZOUI = 01010; enable bits VZOUI = 10101]
case25:
		li	r3,0x7F7FFFFF	# gen the max SP float operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		la	r4,sp2		# get the address of the other operand
		lw	r4,0(r4)	# undt fetch it
		mtc1	r3,fp2		# ship 'em to the FPU
		mtc1	r4,fp4
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0x00FCAAFF	# put all 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set,
					#   and alternate exception/enable bits
 #		la	r5,MULOV	# get the address of the mul
 #		lw	r5,0(r5)	# get the mul instruction (exp EIR)
		li	r5,0x46041182	# load the instruction op code.
		li	r6,0x00820AFF	# create the expected CSR
		li	r7,0x1F1E1D1C	# create a pattern for the destination
		mtc1	r7,fp6		# ship it over
		nop
		nop
MULOV:
		mul.s	fp6,fp2,fp4	# do the multiply that will spill
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp6		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR (operand 1)
		mfc1	r14,fp4		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an underflow operation (U) gives us an unimplemented
		# op (E) rupt in single-precision; CSR-bits initially all 0's
case26:
		li	r3,0x00800000	# gen the min SP float operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		la	r4,sp2		# get the address of the other operand
		lw	r4,0(r4)	# get the other operand
		mtc1	r3,fp0		# ship 'em to the FPU
		mtc1	r4,fp2
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,DIVUNDz	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46020103	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0x2F2E2D2C	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
DIVUNDz:
		div.s	fp4,fp0,fp2	# do the underflowing div
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp0		# get the source FPR (operand 1)
		mfc1	r14,fp2		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an underflow operation (U) gives us an unimplemented
		# op (E) rupt in single-precision; some CSR-bits initially 1's
		# [exception bits and enable bits all 0's]
case27:
		li	r3,0x00800000	# gen the min SP float operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		la	r4,sp999999	# get the address of the other operand
		lw	r4,0(r4)	# undt fetch it
		mtc1	r3,fp2		# ship 'em to the FPU
		mtc1	r4,fp4
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0x00FC007F	# put all 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set,
					#   and all exception and enable bits
 #		la	r5,DIVUNDo	# get the address of the div
 #		lw	r5,0(r5)	# get the div instruction (exp EIR)
		li	r5,0x46041183	# load the instruction op code.
		li	r6,0x0082007F	# create the expected CSR
		li	r7,0x3F3E3D3C	# create a pattern for the destination
		mtc1	r7,fp6		# ship it over
		nop
		nop
DIVUNDo:
		div.s	fp6,fp2,fp4	# do the divide that will spill
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp6		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR (operand 1)
		mfc1	r14,fp4		# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an inexact operation (I) gives us an inexact op (I)
		# rupt in double-to-single; CSR-bits initially all 0's;
		# I-trap NOT enabled
case28:
		li	r3,0x47DFFFFF	# (see the Weitek spec p. 32 "+INEXACT"
		li	r4,0xFFFFFFFF	#  comment for this test case operand)
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp31		# ship the msw to the FPU
		mtc1	r4,fp30		#   then the lsw
		ctc1	r0,fcr30	# stuff 0's in the EIR
		ctc1	r0,fcr31	# clear the CSR (I-trap DISabled)
 #		la	r5,CVToff0	# get the address of the cvt
 #		lw	r5,0(r5)	# get the cvt instruction (exp EIR)
		li	r5,0x4620F020	# load the instruction op code.
		li	r6,0x00001004	# create the expected CSR
		li	r7,0x3F3E3D3C	# create a pattern for the destination
		mtc1	r7,fp0		# ship it over
		nop
		nop
CVToff0:
		cvt.s.d	fp0,fp30	# do the inexact double-to-single cvt
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp0		# get the destination FPR
		mfc1	r13,fp31	# get the source FPR (msw)
		mfc1	r14,fp30	# get the source FPR (lsw)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		bne	r7,r10,1f	# destination SHOULD get changed
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r2,0x7F000000	# Weitek results (p. 32 of spec)
		beq	r2,r10,1f	# did we get the "correct" answer
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did the operand's msw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did the operand's lsw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an inexact operation (I) gives us an inexact op (I)
		# rupt in double-to-single; CSR-bits initially mostly 0's;
		# I-trap IS enabled
case29:
		li	r3,0x47DFFFFF	# (see the Weitek spec p. 32 "+INEXACT"
		li	r4,0xFFFFFFFF	#  comment for this test case operand)
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp1		# ship the msw to the FPU
		mtc1	r4,fp0		#   then the lsw
		ctc1	r0,fcr30	# stuff 0's in the EIR
		li	r5,0x00000080	# enable-bit for I-trap
		ctc1	r5,fcr31	# clear the CSR (I-trap ENabled)
 #		la	r5,CVTon0	# get the address of the cvt
 #		lw	r5,0(r5)	# get the cvt instruction (exp EIR)
		li	r5,0x462007A0	# load the instruction op code.
		li	r6,0x00001080	# create the expected CSR
		li	r7,0x4F4E4D4C	# create a pattern for the destination
		mtc1	r7,fp30		# ship it over
		nop
		nop
CVTon0:
		cvt.s.d	fp30,fp0	# do the inexact double-to-single cvt
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp30	# get the destination FPR
		mfc1	r13,fp1		# get the source FPR (msw)
		mfc1	r14,fp0		# get the source FPR (lsw)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# destination should NOT get changed
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did the operand's msw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did the operand's lsw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an inexact operation (I) gives us an inexact op (I)
		# rupt in double-to-single; many CSR-bits initially 1's;
		# I-trap NOT enabled
case30:
		li	r3,0x47DFFFFF	# (see the Weitek spec p. 32 "+INEXACT"
		li	r4,0xFFFFFFFF	#  comment for this test case operand)
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp21		# ship the msw to the FPU
		mtc1	r4,fp20		#   then the lsw
		ctc1	r0,fcr30	# stuff 0's in the EIR
		li	r5,0xFFFDE07B	# put all 1's in the CSR except for the
		ctc1	r5,fcr31	#   I-bits (leaves I-trap DISabled),
					#   E-bit, and other enables
 #		la	r5,CVToff1	# get the address of the cvt
 #		lw	r5,0(r5)	# get the cvt instruction (exp EIR)
		li	r5,0x4620A2A0	# load the instruction op code.
		li	r6,0x0080107F	# create the expected CSR
		li	r7,0x5F5E5D5C	# create a pattern for the destination
		mtc1	r7,fp10		# ship it over
		nop
		nop
CVToff1:
		cvt.s.d	fp10,fp20	# do the inexact double-to-single cvt
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp10	# get the destination FPR
		mfc1	r13,fp21	# get the source FPR (msw)
		mfc1	r14,fp20	# get the source FPR (lsw)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		bne	r7,r10,1f	# destination SHOULD get changed
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did the operand's msw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did the operand's lsw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an inexact operation (I) gives us an inexact op (I)
		# rupt in double-to-single; many CSR-bits initially 1's;
		# I-trap IS enabled
case31:
		li	r3,0x47DFFFFF	# (see the Weitek spec p. 32 "+INEXACT"
		li	r4,0xFFFFFFFF	#  comment for this test case operand)
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r3,fp11		# ship the msw to the FPU
		mtc1	r4,fp10		#   then the lsw
		ctc1	r0,fcr30	# stuff 0's in the EIR
		li	r5,0xFFFD64FB	# put all 1's in the CSR except for the
		ctc1	r5,fcr31	#   result I-bits (I-trap ENabled),
					#   E-bit, alternate other exception
					#   and enable bits
 #		la	r5,CVTon1	# get the address of the cvt
 #		lw	r5,0(r5)	# get the cvt instruction (exp EIR)
		li	r5,0x46205520	# load the instruction op code.
		li	r6,0x008014FB	# create the expected CSR
		li	r7,0x69966699	# create a pattern for the destination
		mtc1	r7,fp20		# ship it over
		nop
		nop
CVTon1:
		cvt.s.d	fp20,fp10	# do the inexact double-to-single cvt
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp20	# get the destination FPR
		mfc1	r13,fp11	# get the source FPR (msw)
		mfc1	r14,fp10	# get the source FPR (lsw)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# destination should NOT get changed
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did the operand's msw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did the operand's lsw get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

 # NOTE: On the FPU *board*, ALL compares will generate Invalid Op Exceptions
 #	 if either one or both operands is "unordered".  That is, there is
 #	 NO functional difference between (say) the C.EQ.S instruction, and
 #	 the C.SEQ.S instruction.
 #
 #	 This *should* be "fixed" in the FPU chip to do the "correct" (IEEE)
 #	 thing.

		# see if an EQ compare with NANs returns "FALSE", and DOES
		# generate an (E) rupt in single-precision; also that it leaves
		# the sticky bits alone even with all enables on; 2 different
		# NANs
case32:
		li	r3,0x7FAAAAAA	# gen a NAN
		li	r4,0x7FD55555	# and a different NAN
		mtc1	r3,fp22		# ship 'em to the FPU
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		mtc1	r4,fp24
		li	r5,0xFFFFFFFF
		ctc1	r5,fcr30	# stuff 1's in the EIR
		li	r5,0xFFFC0F83	# set the state to check all that shit
		ctc1	r5,fcr31	# stuff it in the CSR
		la	r5,EQNAN	# get the address of the comp
		lw	r5,0(r5)	# get the comp instruction (exp EIR)
		li	r6,0x00820F83	# create the expected CSR
		nop
		nop
EQNAN:
		c.eq.s	fp22,fp24	# do the compare of the NANs
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r13,fp22	# get the source FPR (operand 1)
		mfc1	r14,fp24	# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an SEQ compare with NANs returns "FALSE", and DOES
		# generate an (E) rupt in single-precision; also that it leaves
		# the sticky bits alone even with all enables off; same NAN
		# twice
case33:
		li	r3,0x7FAABEEF	# gen a NAN
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		addu	r4,r0,r3
		mtc1	r3,fp26		# ship it to the FPU
		mtc1	r4,fp28		#   for both operands
		ctc1	r0,fcr30	# stuff 0's in the EIR
		li	r5,0xFF7DF003	# set the state to check that shit
		ctc1	r5,fcr31	# stuff it in the CSR
		la	r5,SEQNAN1	# get the address of the comp
		lw	r5,0(r5)	# get the comp instruction (exp EIR)
		li	r6,0x00020003	# create the expected CSR
		nop
		nop
SEQNAN1:
		c.seq.s	fp26,fp28	# do the compare of the NANs
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r13,fp26	# get the source FPR (operand 1)
		mfc1	r14,fp28	# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if an SEQ compare with NANs returns "FALSE", and DOES
		# generate an (E) rupt in single-precision; also that it leaves
		# the sticky bits alone even with all enables off; 1 NAN
case34:
		li	r3,0x7FAAAF00	# gen a NAN
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		la	r4,sp333333	# get address of other operand
		lw	r4,0(r4)	#   and get it
		mtc1	r3,fp12		# ship them to the FPU
		mtc1	r4,fp14
		ctc1	r0,fcr30	# stuff 0's in the EIR
		li	r5,0xFFFDF07C	# set the state to check that shit
		ctc1	r5,fcr31	# stuff it in the CSR
		la	r5,SEQNAN2	# get the address of the comp
		lw	r5,0(r5)	# get the comp instruction (exp EIR)
		li	r6,0x0082007C	# create the expected CSR
		nop
		nop
SEQNAN2:
		c.seq.s	fp14,fp12	# do the compare
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r13,fp12	# get the source FPR (operand 1)
		mfc1	r14,fp14	# get the source FPR (operand 2)
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR also ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r4,r14,1f	# did operand 2 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 10000]
		# should prevent EIR from getting altered
case35:
		la	r3,sp999999	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x89ABCDEF	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0x00FDF83D	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with one
					#   enable bit
		li	r6,0x0082083D	# create the expected CSR
		li	r7,0xF1F2F3F4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTx:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x89ABCDEF	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MOV of +0 gives an E-rupt; single-precision
case36:
		mtc1	r0,fp2		# ship a +0 over
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r5,0xFFFFFFFF	# all 1's into the EIR
		ctc1	r5,fcr30	# stuff it in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,MOVsp	# get the address of the mov.s
 #		lw	r5,0(r5)	# get the mov.s instruction (exp EIR)
		li	r5,0x46001106	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xE7E8E9EA	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
MOVsp:
		mov.s	fp4,fp2		# do the mov.s of a +0 operand
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r0,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a NEG of +0 gives an E-rupt; single-precision
case37:
		mtc1	r0,fp6		# ship a +0 over
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r5,0xFFFFFFFF	# all 1's into the EIR
		ctc1	r5,fcr30	# stuff it in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,NEGsp	# get the address of the neg.s
 #		lw	r5,0(r5)	# get the neg.s instruction (exp EIR)
		li	r5,0x46003207	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xD7D8D9DA	# create a pattern for the destination
		mtc1	r7,fp8		# ship it over
		nop
		nop
NEGsp:
		neg.s	fp8,fp6		# do the neg.s of a +0 operand
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp8		# get the destination FPR
		mfc1	r13,fp6		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r0,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a NEG of -0 gives an E-rupt; single-precision
case38:
		li	r3,0x80000000	# create a -0
		mtc1	r3,fp6		# ship it over
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r5,0xFFFFFFFF	# all 1's into the EIR
		ctc1	r5,fcr30	# stuff it in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,NEGsn	# get the address of the neg.s
 #		lw	r5,0(r5)	# get the neg.s instruction (exp EIR)
		li	r5,0x46003207	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xC7C8C9CA	# create a pattern for the destination
		mtc1	r7,fp8		# ship it over
		nop
		nop
NEGsn:
		neg.s	fp8,fp6		# do the neg.s of a -0 operand
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp8		# get the destination FPR
		mfc1	r13,fp6		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a MOV of -0 gives an E-rupt; single-precision
case39:
		li	r3,0x80000000	# create a -0
		mtc1	r3,fp2		# ship it over
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		li	r5,0xFFFFFFFF	# all 1's into the EIR
		ctc1	r5,fcr30	# stuff it in the EIR
		ctc1	r0,fcr31	# clear the CSR
 #		la	r5,MOVsn	# get the address of the mov.s
 #		lw	r5,0(r5)	# get the mov.s instruction (exp EIR)
		li	r5,0x46001106	# load the instruction op code.
		li	r6,0x00020000	# create the expected CSR
		li	r7,0xB7B8B9BA	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
MOVsn:
		mov.s	fp4,fp2		# do the mov.s of a -0 operand
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r5,r9,1f	# is the EIR ok
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 01000]
		# should prevent EIR from getting altered; RM = 01
case40:
		la	r3,sp999999	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x89ABCDEF	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0x00FDF45D	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with one
					#   enable bit
		li	r6,0x0082045D	# create the expected CSR
		li	r7,0xC1C2C3C4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTy:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x89ABCDEF	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

	# redundant check to try to nail a particularly nasty Bug we be having
		mfc1	r2,fp2		# get the source FPR
		nop
		beq	r3,r2,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 00100]
		# should prevent EIR from getting altered; RM = 10
case41:
		la	r3,sp777777	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x76757473	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0xFFFDF26E	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with one
					#   enable bit
		li	r6,0x0082026E	# create the expected CSR
		li	r7,0xD1D2D3D4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTp:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r10,fp4		# get the destination FPR
		mfc1	r13,fp2		# get the source FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x76757473	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

	# redundant check to try to nail a particularly nasty Bug we be having
		mfc1	r2,fp2		# get the source FPR
		nop
		beq	r3,r2,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 00010]
		# should prevent EIR from getting altered; RM = 10
case42:
		la	r3,sp555555	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x21222324	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0xFFFDF176	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with one
					#   enable bit
		li	r6,0x00820176	# create the expected CSR
		li	r7,0xE1E2E3E4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTw:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r13,fp2		# get the source FPR
		mfc1	r10,fp4		# get the destination FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x21222324	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

	# redundant check to try to nail a particularly nasty Bug we be having
		mfc1	r2,fp2		# get the source FPR
		nop
		beq	r3,r2,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 00001]
		# should prevent EIR from getting altered; RM = 11
case43:
		la	r3,sp333333	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x91929394	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0x00FDF0FB	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with one
					#   enable bit
		li	r6,0x008200FB	# create the expected CSR
		li	r7,0xA1A2A3A4	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTv:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r9,fcr30	# get the EIR
		mfc1	r13,fp2		# get the source FPR
		mfc1	r10,fp4		# get the destination FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x91929394	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

	# redundant check to try to nail a particularly nasty Bug we be having
		mfc1	r2,fp2		# get the source FPR
		nop
		beq	r3,r2,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		# see if a sqrt operation gives us an unimplemented op
		# (E) rupt; most CSR-bits initially all 1's
		# [exception bits VZOUI = 11111; enable bits VZOUI = 11111]
		# should prevent EIR from getting altered; RM = 00
case44:
		la	r3,sp111111	# get the address of the operand
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
		lw	r3,0(r3)	# get the operand
		nop
		mtc1	r3,fp2		# ship it
		li	r5,0x61626364	# a pattern for the EIR
		ctc1	r5,fcr30	# stuff it in
		li	r5,0x00FDFF80	# put 1's in the CSR except for
		ctc1	r5,fcr31	#   the E-bit, which we should set;
					#   all other exception bits with all
					#   enable bit, no sticky bits
		li	r6,0x00820F80	# create the expected CSR
		li	r7,0x3F4F5F6F	# create a pattern for the destination
		mtc1	r7,fp4		# ship it over
		nop
		nop
SQRTu:
		sqrt.s	fp4,fp2		# do the sqrt we ain't got
		.set at
		li	r1,5
99:
		bne	r1,r0,99b
		addiu	r1,r1,-1
		.set	noat
		cfc1	r9,fcr30	# get the EIR
		cfc1	r8,fcr31	# get the CSR
		cfc1	r8,fcr31	# get the CSR
		mfc1	r13,fp2		# get the source FPR
		mfc1	r10,fp4		# get the destination FPR
		nop

		beq	r6,r8,1f	# is the CSR correct
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		li	r5,0x61626364	# what we put in the EIR should NOT be
		beq	r5,r9,1f	#   changed ... did it?
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r7,r10,1f	# did the destination get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop
		beq	r3,r13,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

	# redundant check to try to nail a particularly nasty Bug we be having
		mfc1	r4,fp2		# get the source FPR
		nop
		beq	r3,r4,1f	# did operand 1 get stepped on
		nop
		b	FpuErr		# incurred ERROR, so let's bail
		nop
1:
		jal	ShortWait
		nop

		.set	reorder

 # k1030.s code added here

		li	r3,0xdeaf
		ctc1	r0,$31
		mtc1	r3,$f0
		mtc1	r0,$f1
		mfc1	r5,$f1
		bne	r5,r0,FpuErr
		mfc1	r4,$f0
		bne	r3,r4,FpuErr
		cvt.s.w	$f2,$f0
		add.s	$f4,$f2,$f2
		sub.s	$f6,$f4,$f2
		c.eq.s	$f6,$f2
		bc1f	FpuErr
		c.eq.s	$f6,$f4
		bc1t	FpuErr
		cvt.w.s	$f8,$f6
		mfc1	r5,$f8
		bne	r3,r5,FpuErr
		cfc1	r5,$31
		bne	r5,r0,FpuErr
		li	r4,0x10
		mtc1	r4,$f10
		cvt.s.w	$f12,$f10
		div.s	$f14,$f2,$f12
		cvt.w.s	$f16,$f14
		mfc1	r5,$f16
		bne	r5,0xdeb,FpuErr
		li	r4,0x2
		mtc1	r4,$f18
		cvt.s.w	$f18,$f18
		mul.s	$f20,$f18,$f2
		c.eq.s	$f20,$f4
		bc1f	FpuErr

 #		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		ctc1	r0,fcr31	# clear CSR to insure loadable EIR
 #		la	a0,success
 #		jal	pon_puts
		move	v0,zero		# indicate success
chip_done:				# use v0 value from chip test!!
		lw	ra,0(sp)
		lw	s0,4(sp)
		lw	s1,8(sp)
		lw	s2,12(sp)
		lw	s3,16(sp)
		lw	s4,20(sp)
		lw	s5,24(sp)
		lw	s6,28(sp)
		lw	s7,32(sp)
		lw	fp,36(sp)
		addu	sp,sp,40
		j	ra		# and return

 # We failed!!!!!!!
FpuErr:
		jal	ResetFPU	# do a h/w reset of the FPU
		la	a0,failure
		jal	pon_puts
		li	a0,FP_FAIL
		jal	FastFlash
		li	a0,FP_FAIL
		jal	pon_set_leds	# write to the CPU LEDS
 		li	v0,NVRAM_DIAG_LOC	# address of DIAG byte
 		li	a0,FP_FAIL		# setup our error code
 		sb	a0, 0(v0)		# and write the diag location
 		li	v0,NVRAM_BOOTMODE_LOC	# 'bootmode' nvram location
 		lb	a0, 0(v0)		# and read it in
 		li	a1,CHAR_d		# let's see if "diag" mode set
  		beq	a0,a1,1f		# don't write 'e' if "diag" mode
 		li	a0,CHAR_e		# setup our error char 'e'
 		sb	a0, 0(v0)		# and write it out
1:
 #		MTC1_R0_CSR		# clear the FPU's cntl and status reg
		mtc0	zero,C0_SR	# make FPU ops NON! street-legal
		li	v0,1		# indicate failure
		lw	ra,0(sp)
		lw	s0,4(sp)
		lw	s1,8(sp)
		lw	s2,12(sp)
		lw	s3,16(sp)
		lw	s4,20(sp)
		lw	s5,24(sp)
		lw	s6,28(sp)
		lw	s7,32(sp)
		lw	fp,36(sp)
		addu	sp,sp,40
		j	ra		# and return
END(Pon_FP)
		.data
		.align	2
failure:
		.asciiz "Floating Point Board test FAILED...\r\n"
success:
		.asciiz "Floating Point Board test was SUCCESSFUL...\r\n"
noboard:
		.asciiz "No Floating Point board...\r\n"
fpuchip:
		.asciiz "FPU chip found...\r\n"
fpuboard:
		.asciiz "FPU board found...\r\n"
