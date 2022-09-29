#ident "$Header: pon_fp_chip.s,v 1.2.7.1 90/07/18 14:31:34 huang Exp $"
/* $Copyright$ */

#include "machine/mach_ops.h"
#include "machine/dregdef.h"
#include "machine/standard.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/cp1.h"
#include "machine/delaymacs.h"
#include "machine/asm.h"
#include "pon.h"
#undef  PHYS_TO_K1(x)
#define	PHYS_TO_K1(x)	((x)|0xA0000000)	/* physical to kseg1 */

#define  MTC1_R0_CSR	.word 0x44C0F800

#define PRINT(y,z)	\
	la	a0,expect; \
	jal	pon_puts;	\
	nop;		\
	move	a0,y;   \
	jal	pon_puthex; \
	nop;	\
	la	a0,actual; \
	jal	pon_puts;	\
	nop;		\
	move	a0,z;	\
	jal	pon_puthex;	\
	nop;	\
	la	a0,CRLF; \
	jal	pon_puts;	\
	nop

	.data
OperandTable:
        .word   0x20000 /* EIR = 0x46001524 EPC = 0xbfc00454*/
        .word   0x5014
        .word   0x20014 /* EIR = 0x4600a524 EPC = 0xbfc00490*/
        .word   0x73a31573
        .word   0x820101        /* EIR = 0x46206320 EPC = 0xbfc004e8*/
        .word   0x800101
        .word   0x20101 /* EIR = 0x46001524 EPC = 0xbfc00454*/
        .word   0x5115
        .word   0x20115 /* EIR = 0x4600a524 EPC = 0xbfc00490*/
        .word   0x73a31573
        .word   0x820101        /* EIR = 0x46206320 EPC = 0xbfc004e8*/
        .word   0x800101
        .word   0x5b84792
        .word   0x0
        .word   0x20105 /* EIR = 0x4600b524 EPC = 0xbfc005d8*/
        .word   0x5b84792
        .word   0x0
        .word   0x20105 /* EIR = 0x4600b524 EPC = 0xbfc005d8*/
        .word   0x1eb60e5a
        .word   0x1eb60e5a
        .word   0x1eb60e5a
        .word   0x1eb60e5a
        .word   0x800105
        .word   0x820105        /* EIR = 0x46208124 EPC = 0xbfc006fc*/
        .word   0x690a21e4
        .word   0x800105
        .word   0x820105        /* EIR = 0x46208124 EPC = 0xbfc006fc*/
        .word   0x690a21e4
        .word   0x0
        .word   0x820105        /* EIR = 0x46201124 EPC = 0xbfc00838*/
        .word   0x716169fe
        .word   0x820115        /* EIR = 0x46201124 EPC = 0xbfc00838*/
        .word   0x0
        .word   0x800115
        .word   0x0
        .word   0x800115
        .word   0xf4d6788
        .word   0x463
        .word   0xf4d6788
        .word   0x463
        .word   0x467
        .word   0x467
        .word   0x820a00        /* EIR = 0x46208320 EPC = 0xbfc00a44*/
        .word   0x801a04
        .word   0x820a04        /* EIR = 0x4620b7a0 EPC = 0xbfc00a84*/
        .word   0x820a04        /* EIR = 0x4620d520 EPC = 0xbfc00a88*/
        .word   0x801a04
        .word   0x820a00        /* EIR = 0x46208320 EPC = 0xbfc00a44*/
        .word   0x801a04
        .word   0x820a04        /* EIR = 0x4620b7a0 EPC = 0xbfc00a84*/
        .word   0x820a04        /* EIR = 0x4620d520 EPC = 0xbfc00a88*/
        .word   0x801a04
        .word   0x8206cb        /* EIR = 0x46201085 EPC = 0xbfc00b8c*/
        .word   0x20901 /* EIR = 0x4620d0a0 EPC = 0xbfc00bd4*/
        .word   0x820804        /* EIR = 0x4620d0a0 EPC = 0xbfc00bd4*/
        .word   0x800804
        .word   0x60000000
        .word   0x467462ae
        .word   0x800f00
        .word   0xceb
        .word   0x60000000
        .word   0x467462ae
        .word   0x800f00
        .word   0x467462ae
        .word   0x1ceb  /* EIR = 0x4612f400 EPC = 0xbfc00d74*/
        .word   0x800ceb
        .word   0x801ceb        /* EIR = 0x4636b782 EPC = 0xbfc00dd4*/
        .word   0x467462ae
        .word   0x800ceb
        .word   0x801ceb        /* EIR = 0x4636b782 EPC = 0xbfc00dd4*/
        .word   0x820ceb        /* EIR = 0x463ab035 EPC = 0xbfc00dec*/
        .word   0x8eae4d97
        .word   0x801ceb        /* EIR = 0x4610e502 EPC = 0xbfc00e28*/
        .word   0x820ceb        /* EIR = 0x4636d100 EPC = 0xbfc00e34*/
        .word   0x800ceb
        .word   0x801ceb        /* EIR = 0x4620b124 EPC = 0xbfc00e70*/
        .word   0x800ceb
        .word   0x20102 /* EIR = 0x463ab203 EPC = 0xbfc00ec8*/
        .word   0x8eae4d97
        .word   0x801106
        .word   0x801106
        .word   0x5d73079e
        .word   0x1
        .word   0x1
        .word   0x1a1f
        .word   0x10fdd /* EIR = 0x4620a6a4 EPC = 0xbfc01020*/
        .word   0x20fdd /* EIR = 0x460e2580 EPC = 0xbfc01024*/
        .word   0xfdd
        .word   0x800a35
        .word   0x810a35        /* EIR = 0x4620a1a4 EPC = 0xbfc010c8*/
        .word   0x800a35
        .word   0x10fdd /* EIR = 0x4620a6a4 EPC = 0xbfc01020*/
        .word   0x20fdd /* EIR = 0x460e2580 EPC = 0xbfc01024*/
        .word   0xfdd
        .word   0x800a35
        .word   0x810a35        /* EIR = 0x4620a1a4 EPC = 0xbfc010c8*/
        .word   0x100
        .word   0x8eae4d97
        .word   0x114
        .word   0x8eae4d97
        .word   0x114
        .word   0x114
        .word   0xf8ec7b97
        .word   0xf8ec7b97
        .word   0xf8ec7b97
        .word   0xf8ec7b97
        .word   0x805b07        /* EIR = 0x46206120 EPC = 0xbfc012cc*/
        .word   0x101
        .word   0x101
        .word   0xc836f841
        .word   0x4d152f4e
        .word   0xc836f841
        .word   0x4d152f4e
        .word   0x115
        .word   0x4d152f4e
        .word   0x0
        .word   0x9a400000
        .word   0xcdb0a831
        .word   0x5a0d1247
        .word   0xcdb0a831
        .word   0x8eae4d97
        .word   0xc836f841
        .word   0x0
        .word   0x5d73079f
        .word   0xe9eaf9e8
        .word   0x0
        .word   0xf8ec7b97
        .word   0x2b6e683c
        .word   0x5d73079f
        .word   0x3667f4bf
        .word   0x80000000
        .word   0xfdf61ef
        .word   0x800115
        .word   0x4d152f4e
        .word   0x0
        .word   0x9a400000
        .word   0xcdb0a831
        .word   0x5a0d1247
        .word   0xcdb0a831
        .word   0x8eae4d97
        .word   0xc836f841
        .word   0x0
        .word   0x5d73079f
        .word   0xe9eaf9e8
        .word   0x0
        .word   0xf8ec7b97
        .word   0x2b6e683c
        .word   0x5d73079f
        .word   0x3667f4bf
        .word   0x80000000
        .word   0xfdf61ef
        .word   0xd7b
        .word   0xd7b
        .word   0x20d7b /* EIR = 0x4632903e EPC = 0xbfc01910*/
        .word   0xcdb0a831
        .word   0x0
        .word   0x20d7f /* EIR = 0x463ef202 EPC = 0xbfc0198c*/
        .word   0x20d7f /* EIR = 0x4632903e EPC = 0xbfc01910*/
        .word   0xcdb0a831
        .word   0x0
        .word   0x20d7f /* EIR = 0x463ef202 EPC = 0xbfc0198c*/
        .word   0x820d7f        /* EIR = 0x4632f583 EPC = 0xbfc019b8*/
        .word   0xfdf61ef
        .word   0x5a0d1247
        .word   0x820d7f        /* EIR = 0x46329582 EPC = 0xbfc01a38*/
        .word   0x820400        /* EIR = 0x4632f583 EPC = 0xbfc019b8*/
        .word   0xfdf61ef
        .word   0xcdb0a831
        .word   0x820414        /* EIR = 0x46329582 EPC = 0xbfc01a38*/
        .word   0x820003        /* EIR = 0x4626903c EPC = 0xbfc01a80*/
        .word   0x805017
        .word   0x820003        /* EIR = 0x4626903c EPC = 0xbfc01a80*/
        .word   0x820003        /* EIR = 0x462031a0 EPC = 0xbfc01a8c*/
        .word   0x800003
        .word   0x8204b7        /* EIR = 0x463ef031 EPC = 0xbfc01ae4*/
        .word   0x8054b7        /* EIR = 0x462045a0 EPC = 0xbfc01c04*/
        .word   0x14b7  /* EIR = 0x4618e081 EPC = 0xbfc01c20*/
        .word   0x4b7
        .word   0x90469e6f
        .word   0x101f
        .word   0x2fcc4bc8
        .word   0x100f
        .word   0x4d152f4e
        .word   0x0
        .word   0x2000f /* EIR = 0x4620f7a4 EPC = 0xbfc01da4*/
        .word   0xf
        .word   0xf
        .word   0xf
        .word   0x4d152f4e
        .word   0x0
        .word   0x2000f /* EIR = 0x4620f7a4 EPC = 0xbfc01da4*/
        .word   0xf
        .word   0xf
        .word   0xf
        .word   0x3f800000
        .word   0xf
        .word   0xf
        .word   0x3f800000
        .word   0xf
        .word   0xf
        .word   0x1a80  /* EIR = 0x461cd680 EPC = 0xbfc01f50*/
        .word   0xa80
        .word   0x1a80  /* EIR = 0x461cd680 EPC = 0xbfc01f50*/
        .word   0x2fcc4bc8
        .word   0x1a80  /* EIR = 0x46364702 EPC = 0xbfc01fac*/
        .word   0x2fcc4bc8
        .word   0x1a80  /* EIR = 0x46364702 EPC = 0xbfc01fac*/
        .word   0x801c90        /* EIR = 0x460017a4 EPC = 0xbfc02020*/
        .word   0x801c90        /* EIR = 0x46284702 EPC = 0xbfc02038*/
        .word   0x801c90        /* EIR = 0x461c4582 EPC = 0xbfc0203c*/
        .word   0x1c90  /* EIR = 0x460017a4 EPC = 0xbfc02020*/
        .word   0x801c90        /* EIR = 0x46284702 EPC = 0xbfc02038*/
        .word   0x801c90        /* EIR = 0x461c4582 EPC = 0xbfc0203c*/
        .word   0x2011b /* EIR = 0x46204624 EPC = 0xbfc020b8*/
        .word   0x11b
        .word   0x2011b /* EIR = 0x46204624 EPC = 0xbfc020b8*/
        .word   0x11b
        .word   0x0
        .word   0x11b
        .word   0x0
        .word   0x11b
        .word   0x800b03
        .word   0x800b03
        .word   0x4d152f4e
        .word   0x2fcc4bc8
        .word   0x0
        .word   0x0
        .word   0x0
        .word   0x0
        .word   0x1e000000
        .word   0x0
        .word   0xcdb0a831
        .word   0x0
        .word   0x0
        .word   0x0
        .word   0x2b6e683c
        .word   0x30b06559
        .word   0x2fcc4bc8
        .word   0x2b6e683c
        .word   0x800b03

	.text
	.extern	_fp_test_on
	.extern	Lstore
	.ent	Pon_fp_chip 0
	.globl	Pon_fp_chip
Pon_fp_chip:
	subu	sp,24
	sw	ra,20(sp)
	b	SetUp

	.align	5
bupHappy:
	sw	zero,_fp_test_on	# de-arm "_exception_handler()"
	la	v0, 3f
 	or	v0, v0, 0xA0000000 	# back to uncached operation
	j	v0
3:
 #	MTC1_R0_CSR		# clear the FPU's cntl and status reg
	ctc1	r0,fcr31	# clear CSR to insure loadable EIR
 #	la	a0,success
 #  	jal	pon_puts
 #  	la	a0,CRLF
 #  	jal	pon_puts
	move	v0,zero		# indicate success for return
	li	t6,SR_PE | SR_CU1
	mtc0	t6,C0_SR	# keep FPU operations enabled but NOT ints
	lw	ra,20(sp)
	addu	sp,24
	j	ra
	.align	6
bupSad:	
	sw	zero,_fp_test_on	# de-arm "_exception_handler()"
 #	PRINT(t4,t3)
	la	v0, 3f
 	or	v0, v0, 0xA0000000 	# back to uncached operation
	j	v0
3:
 #	la	a0,failure
 #  	jal	pon_puts
 #  	la	a0,CRLF
 #  	jal	pon_puts
	li	a0,FP_FAIL
	jal	FastFlash
	li	a0,FP_FAIL
	jal	pon_set_leds		# write to the CPU LEDS
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
 #	MTC1_R0_CSR		# clear the FPU's cntl and status reg
	ctc1	r0,fcr31	# clear CSR to insure loadable EIR
	li	t0,SR_PE	# make FPU ops NON-street-legal
	mtc0	t0,C0_SR	# and disable INTS etc.
	li	v0,1		# indicate failure
	lw	ra,20(sp)
	addu	sp,24
	j	ra

failure:
        .asciiz "Floating Point Chip test FAILED..." 
CRLF:
	.asciiz "\r\n"
success:
	.asciiz "Floating Point Chip test was SUCCESSFUL...\r\n"
expect:
        .asciiz "Expected: "
actual:
        .asciiz " Actual was: " 

	.text
	.align	5
SetUp:
	li	t7,FP_RN		# clear enables & stickies 
	ctc1	t7,C1_SR		# condition : round nearest
	li	v0,1
	sw	v0,_fp_test_on		# arm "_exception_handler()"
	li	t6,SR_IEC | SR_PE | SR_CU1 | SR_IMASK
	mtc0	t6,C0_SR		# enable  FP exceptions
	sw	zero,Lstore
	la	t5,OperandTable
	la	v0, 3f
	and	v0, v0, 0x9fffffff	# let's run cached i-fetches
	j	v0			# to allow cached loop testing
3:
	.data
LI0:	.word	0x3e6613e2
LI1:	.word	0x6b792aa2
LI2:	.word	0x716169fe
LI3:	.word	0x7fe52c4d
LI4:	.word	0x142e3e40
LI5:	.word	0x34022589
LI6:	.word	0x740f103f
LI7:	.word	0x690a21e4
LI8:	.word	0x73a31573
LI9:	.word	0x6b75572f
LI10:	.word	0x5fcc7499
LI11:	.word	0x290a0a74
LI12:	.word	0x36343d88
LI13:	.word	0x2a5a7cbb
LI14:	.word	0x5b84792
LI15:	.word	0x438938ac
LI16:	.word	0xeae4d97
LI17:	.word	0x6a1964a8
LI18:	.word	0x3c921b2a
LI19:	.word	0x14a62878
LI20:	.word	0xfff65371
LI21:	.word	0x523d4b46
LI22:	.word	0x5de129d1
LI23:	.word	0x2071171d
LI24:	.word	0x33c162a4
LI25:	.word	0xf4d6788
LI26:	.word	0x1eb60e5a
LI27:	.word	0x164b58a2
LI28:	.word	0x498d767c
LI29:	.word	0x1fd36566
LI30:	.word	0x338d0946
LI31:	.word	0x2f3c0782
	.text
	ctc1	zero,C1_SR
	lwc1	$f0,LI0
	lwc1	$f1,LI1
	lwc1	$f2,LI2
	lwc1	$f3,LI3
	lwc1	$f4,LI4
	lwc1	$f5,LI5
	lwc1	$f6,LI6
	lwc1	$f7,LI7
	lwc1	$f8,LI8
	lwc1	$f9,LI9
	lwc1	$f10,LI10
	lwc1	$f11,LI11
	lwc1	$f12,LI12
	lwc1	$f13,LI13
	lwc1	$f14,LI14
	lwc1	$f15,LI15
	lwc1	$f16,LI16
	lwc1	$f17,LI17
	lwc1	$f18,LI18
	lwc1	$f19,LI19
	lwc1	$f20,LI20
	lwc1	$f21,LI21
	lwc1	$f22,LI22
	lwc1	$f23,LI23
	lwc1	$f24,LI24
	lwc1	$f25,LI25
	lwc1	$f26,LI26
	lwc1	$f27,LI27
	lwc1	$f28,LI28
	lwc1	$f29,LI29
	lwc1	$f30,LI30
	lwc1	$f31,LI31
	move	t1,zero
1:
	li	t0,2
7:
	swc1	$f8,Lstore
L0:	cvt.w.s	$f20,$f2
L1:	cvt.s.d	$f20,$f2
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L2:	cvt.w.s	$f20,$f20
	lwc1	$f20,Lstore
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f8
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	li	r5,0x800101
	ctc1	r5,C1_SR
L3:	cvt.s.d	$f12,$f12
	swc1	$f4,Lstore

	nop
	nop
	nop
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	mtc1	t1,$f13
L4:	c.seq.s	$f8,$f6
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	swc1	$f16,Lstore
L5:	add.s	$f10,$f20,$f14
L6:	sub.s	$f0,$f26,$f10
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f14
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	mfc1	t1,$f9
	.data
L7:	.word	0x40690c50
	.text
	lw	r5,L7
	mtc1	r5,$f30
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f13
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L8:	cvt.w.s	$f20,$f22
L9:	c.seq.s	$f20,$f14
L10:	div.d	$f20,$f22,$f18
L11:	mul.d	$f20,$f26,$f8
L12:	div.d	$f16,$f8,$f16
L13:	c.ngt.d	$f16,$f28
L14:	c.ngt.s	$f30,$f2
L15:	add.d	$f10,$f16,$f16
	mfc1	t1,$f10
L16:	c.seq.d	$f4,$f14
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f26
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f26
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L17:	c.lt.s	$f10,$f30
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:
/* Register definitions: 0xffdfd7fd */
L18:	cvt.w.d	$f4,$f16
	mfc1	t1,$f23
L19:	cvt.d.s	$f4,$f4
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f7
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	.set	noreorder
	bc1t	1f
	nop
	.set	reorder
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	li	r5,0xd01
	ctc1	r5,C1_SR
L20:	mov.d	$f4,$f22
L21:	sub.s	$f6,$f6,$f22
L22:	add.s	$f6,$f8,$f12
L23:	cvt.s.w	$f8,$f6
	swc1	$f22,Lstore
L24:	c.nge.s	$f10,$f14
	li	r5,0x777
	ctc1	r5,C1_SR
L25:	sub.d	$f8,$f28,$f30
L26:	sub.d	$f4,$f28,$f26
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
1:
/* Register definitions: 0xffdfd55d */
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f4
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L27:	add.d	$f4,$f22,$f2
L28:	cvt.w.d	$f4,$f2
L29:	mul.s	$f18,$f4,$f0
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
L30:	sub.s	$f18,$f28,$f0
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L31:	cvt.d.s	$f0,$f18
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f0
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L32:	cvt.w.s	$f0,$f30
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f25
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
1:
/* Register definitions: 0xffd7d55d */
L33:	cvt.d.w	$f0,$f18
	li	r5,0x463
	ctc1	r5,C1_SR
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	.data
L34:	.word	0x4d7850aa
	.text
	lw	r5,L34
	mtc1	r5,$f17
	.data
L35:	.word	0xac078ea
	.text
	lwc1	$f19,L35
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
L36:	sub.s	$f18,$f28,$f18
	.data
L37:	.word	0x1bd122f8
	.text
	lwc1	$f22,L37
L38:	c.olt.s	$f8,$f18
L39:	add.s	$f20,$f22,$f20
	.data
L40:	.word	0x6dfc4e64
	.text
	lw	r5,L40
	mtc1	r5,$f2
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L41:	cvt.w.d	$f24,$f26
	mfc1	t1,$f24
L42:	cvt.d.w	$f16,$f16
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:
/* Register definitions: 0xfdd7d55d */
L43:	c.ngl.s	$f16,$f10
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	.data
L44:	.word	0x21aa79ba
	.text
	lwc1	$f17,L44
L45:	c.seq.d	$f16,$f14
	mfc1	t1,$f18
	li	r5,0x800a00
	ctc1	r5,C1_SR
L46:	cvt.s.d	$f12,$f16
	swc1	$f14,Lstore
L47:	mul.s	$f12,$f4,$f30
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L48:	cvt.s.d	$f30,$f22
L49:	cvt.s.d	$f20,$f26
	lwc1	$f20,Lstore
L50:	cvt.s.w	$f20,$f20
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
L51:	add.d	$f20,$f26,$f16
	li	r5,0x800203
	ctc1	r5,C1_SR
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	mfc1	t1,$f20
L52:	neg.d	$f28,$f28
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	li	r5,0x800400
	ctc1	r5,C1_SR
L53:	mul.s	$f14,$f4,$f22
	lwc1	$f14,Lstore
L54:	c.lt.s	$f10,$f22
L55:	c.ngl.d	$f22,$f22
	swc1	$f2,Lstore
	li	r5,0x8006cb
	ctc1	r5,C1_SR
	.data
L56:	.word	0x5c154cbb
	.text
	lwc1	$f9,L56
	mtc1	t1,$f3
	swc1	$f2,Lstore
	.data
L57:	.word	0x6cc25ca4
	.text
	lwc1	$f9,L57
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L58:	abs.d	$f2,$f2
	li	r5,0x8005ac
	ctc1	r5,C1_SR
	swc1	$f2,Lstore
	mtc1	t1,$f2
1:
/* Register definitions: 0x7dd7555d */
	li	r5,0x901
	ctc1	r5,C1_SR
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L59:	cvt.w.d	$f2,$f2
	mtc1	t1,$f12
L60:	cvt.s.d	$f2,$f26

	nop
	nop
	nop
	.set	noreorder
	bc1t	1f
	nop
	.set	reorder
	mfc1	t1,$f20
	li	r5,0x91e
	ctc1	r5,C1_SR
	li	r5,0x800
	ctc1	r5,C1_SR
L61:	cvt.s.w	$f28,$f30
L62:	sub.s	$f10,$f20,$f10
L63:	cvt.d.s	$f16,$f8
	mfc1	t1,$f12
L64:	abs.d	$f16,$f16
L65:	c.ngt.s	$f16,$f16
	swc1	$f18,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	swc1	$f16,Lstore
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f16
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f17
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	li	r5,0x800f00
	ctc1	r5,C1_SR
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	mtc1	t1,$f27
	li	r5,0xb02
	ctc1	r5,C1_SR
	li	r5,0xceb
	ctc1	r5,C1_SR
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f17
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L66:	add.s	$f16,$f30,$f18
L67:	c.le.s	$f12,$f8
L68:	div.d	$f30,$f22,$f22
	swc1	$f30,Lstore
	mfc1	t1,$f20
1:
/* Register definitions: 0x5dd55555 */
L69:	cvt.d.s	$f30,$f16
L70:	mul.s	$f30,$f30,$f16
	mtc1	t1,$f14
	mfc1	t1,$f6
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L71:	mul.d	$f30,$f22,$f22
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L72:	c.ult.d	$f22,$f26
	mfc1	t1,$f14
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f10
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L73:	mul.s	$f20,$f28,$f16
	.data
L74:	.word	0x545c343a
	.text
	lwc1	$f0,L74
	swc1	$f20,Lstore
L75:	add.d	$f4,$f26,$f22
	swc1	$f0,Lstore
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L76:	cvt.w.d	$f4,$f22
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	li	r5,0x800301
	ctc1	r5,C1_SR
	li	r5,0x102
	ctc1	r5,C1_SR
L77:	div.d	$f8,$f22,$f26
L78:	cvt.s.w	$f26,$f8
L79:	c.nge.d	$f22,$f22
L80:	cvt.d.s	$f26,$f26
L81:	c.ueq.d	$f22,$f22
	swc1	$f12,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	mfc1	t1,$f20
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
	li	r5,0xa1f
	ctc1	r5,C1_SR
	.data
L82:	.word	0x2b6e683c
	.text
	lwc1	$f24,L82
	swc1	$f26,Lstore
L83:	nop	
	.data
L84:	.word	0x5d73079f
	.text
	lw	r5,L84
	mtc1	r5,$f16
L85:	add.s	$f22,$f10,$f16
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f22
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f4
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	mtc1	t1,$f22
	.data
L86:	.word	0x6e647eab
	.text
	lwc1	$f21,L86
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	swc1	$f22,Lstore
	li	r5,0xfdd
	ctc1	r5,C1_SR
L87:	cvt.w.d	$f26,$f20
	lwc1	$f12,Lstore
L88:	add.s	$f22,$f4,$f14
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	li	r5,0x800ae3
	ctc1	r5,C1_SR
	li	r5,0x800a35
	ctc1	r5,C1_SR
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	.data
L89:	.word	0x79065d48
	.text
	lw	r5,L89
	mtc1	r5,$f8
	swc1	$f22,Lstore
L90:	abs.d	$f6,$f20
L91:	cvt.w.d	$f6,$f20
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	mtc1	t1,$f6
	li	r5,0x100
	ctc1	r5,C1_SR
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L92:	c.nge.d	$f20,$f20
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L93:	mul.d	$f6,$f20,$f20
L94:	c.ult.s	$f6,$f6
L95:	cvt.s.w	$f6,$f18
	.data
L96:	.word	0x2f576629
	.text
	lw	r5,L96
	mtc1	r5,$f7
L97:	c.lt.d	$f20,$f6
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f10
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
1:
/* Register definitions: 0x55555555 */
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	swc1	$f12,Lstore
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
	.data
L98:	.word	0x80393a1e
	.text
	lwc1	$f11,L98
	swc1	$f24,Lstore
L99:	cvt.w.s	$f10,$f20
	li	r5,0x800e00
	ctc1	r5,C1_SR
	.data
L100:	.word	0x3e22342b
	.text
	lwc1	$f0,L100
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
1:
/* Register definitions: 0x55555555 */
	.data
L101:	.word	0x1c00076b
	.text
	lw	r5,L101
	mtc1	r5,$f9
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
L102:	abs.d	$f12,$f8
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	lwc1	$f13,Lstore
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f13
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L103:	c.ole.d	$f12,$f12
	mfc1	t1,$f12
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f13
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L104:	cvt.s.d	$f4,$f12
L105:	mov.s	$f4,$f30
L106:	mul.s	$f12,$f8,$f10
	li	r5,0xb03
	ctc1	r5,C1_SR
L107:	cvt.w.d	$f26,$f8
L108:	cvt.s.w	$f26,$f6
L109:	sub.s	$f26,$f6,$f0
L110:	add.s	$f26,$f26,$f28
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	mfc1	t1,$f4
L111:	add.s	$f26,$f16,$f8
L112:	div.s	$f28,$f0,$f26
L113:	cvt.w.d	$f8,$f8
	li	r5,0x800101
	ctc1	r5,C1_SR
	mfc1	t1,$f8
	mtc1	t1,$f20
	swc1	$f16,Lstore
	.data
L114:	.word	0x5a0d1247
	.text
	lwc1	$f7,L114
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	mfc1	t1,$f6
L115:	add.d	$f8,$f6,$f6
	mfc1	t1,$f8
L116:	c.olt.s	$f8,$f2
L117:	c.ngle.d	$f6,$f6
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	.data
L118:	.word	0xfdf61ef
	.text
	lwc1	$f31,L118
	mfc1	t1,$f20
	.data
L119:	.word	0x4d152f4e
	.text
	lw	r5,L119
	mtc1	r5,$f0
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f12
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f0
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L120:	cvt.s.d	$f4,$f6
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L121:	cvt.d.w	$f4,$f10
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L122:	c.olt.s	$f4,$f28
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f0
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f2
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f4
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f6
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f7
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f8
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f10
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f12
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f14
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f16
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f18
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f20
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f22
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f24
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f26
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f28
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f30
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f31
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	swc1	$f31,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	li	r5,0x800701
	ctc1	r5,C1_SR
L123:	nop	
L124:	c.ngle.d	$f30,$f30
	swc1	$f7,Lstore
L125:	c.f.d	$f30,$f30
L126:	c.sf.d	$f30,$f30
L127:	sub.s	$f20,$f30,$f0
L128:	cvt.d.s	$f2,$f20
	li	r5,0xd7b
	ctc1	r5,C1_SR
	lwc1	$f2,Lstore
L129:	neg.d	$f26,$f30
L130:	mov.d	$f22,$f6
L131:	add.d	$f22,$f30,$f30
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	lwc1	$f22,Lstore
	mtc1	t1,$f19
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L132:	c.le.d	$f18,$f18
L133:	sub.d	$f28,$f30,$f6
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f28
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f19
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L134:	cvt.w.d	$f10,$f30
L135:	sub.s	$f10,$f6,$f24
L136:	mul.d	$f8,$f30,$f30
L137:	sub.s	$f22,$f8,$f28
	mfc1	t1,$f22
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L138:	cvt.d.s	$f22,$f20
L139:	c.olt.d	$f30,$f6
	swc1	$f19,Lstore
L140:	div.d	$f22,$f30,$f18
	swc1	$f28,Lstore
L141:	mul.s	$f22,$f18,$f18
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f31
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L142:	neg.s	$f22,$f28
	.data
L143:	.word	0x275e6cb7
	.text
	lwc1	$f26,L143
	swc1	$f6,Lstore
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f2
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L144:	mul.d	$f22,$f18,$f18
L145:	add.s	$f2,$f30,$f28
	li	r5,0x400
	ctc1	r5,C1_SR
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	li	r5,0x800003
	ctc1	r5,C1_SR
L146:	nop	
L147:	c.lt.d	$f18,$f6
L148:	mov.s	$f20,$f16
L149:	cvt.s.d	$f6,$f6
L150:	mul.s	$f6,$f6,$f18
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L151:	neg.s	$f16,$f22
	.data
L152:	.word	0x3d742121
	.text
	lwc1	$f19,L152
L153:	c.un.d	$f30,$f30
	.data
L154:	.word	0x10031cc8
	.text
	lwc1	$f28,L154
	li	r5,0x800e02
	ctc1	r5,C1_SR
L155:	add.s	$f30,$f12,$f18
	swc1	$f2,Lstore
L156:	c.ngt.s	$f6,$f20
L157:	c.nge.d	$f18,$f18
1:
/* Register definitions: 0x55555555 */
	lwc1	$f9,Lstore
	li	r5,0x8004b7
	ctc1	r5,C1_SR
	mfc1	t1,$f9
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L158:	cvt.d.s	$f30,$f30

	nop
	nop
	nop
	.set	noreorder
	bc1t	1f
	nop
	.set	reorder
L159:	c.f.s	$f4,$f28
L160:	c.f.d	$f8,$f8
L161:	c.lt.d	$f8,$f8
L162:	cvt.w.s	$f6,$f0
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L163:	cvt.d.w	$f2,$f30
L164:	mul.s	$f4,$f2,$f4
	mfc1	t1,$f8
	.data
L165:	.word	0xfffa4cb3
	.text
	lwc1	$f4,L165
L166:	cvt.s.w	$f28,$f24
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L167:	sub.d	$f28,$f8,$f8
	lwc1	$f29,Lstore
L168:	cvt.d.w	$f28,$f22
L169:	add.s	$f6,$f6,$f8
	.data
L170:	.word	0x430e5ea1
	.text
	lwc1	$f11,L170
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L171:	sub.d	$f6,$f28,$f28
	swc1	$f6,Lstore
L172:	cvt.s.d	$f22,$f8
L173:	c.nge.d	$f10,$f8
	.data
L174:	.word	0x3aa060d3
	.text
	lw	r5,L174
	mtc1	r5,$f23
L175:	sub.s	$f2,$f28,$f24
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	lwc1	$f19,Lstore
1:
/* Register definitions: 0x55555755 */
	.data
L176:	.word	0x668f57c1
	.text
	lwc1	$f23,L176
	li	r5,0xf
	ctc1	r5,C1_SR
L177:	mul.d	$f2,$f8,$f8
L178:	div.d	$f2,$f8,$f22
	mfc1	t1,$f6
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f2
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L179:	nop	
L180:	add.s	$f20,$f6,$f10
L181:	c.lt.d	$f8,$f22
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L182:	add.d	$f10,$f22,$f22
L183:	c.nge.d	$f22,$f22
	mfc1	t1,$f14
	.data
L184:	.word	0x783f3405
	.text
	lwc1	$f6,L184
L185:	mul.d	$f30,$f22,$f8
	lwc1	$f31,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f0
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f31
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L186:	cvt.w.d	$f30,$f30
L187:	div.s	$f30,$f10,$f18
L188:	sub.s	$f28,$f26,$f28
L189:	cvt.s.w	$f20,$f0
L190:	c.sf.s	$f8,$f30
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L191:	cvt.d.w	$f12,$f30
L192:	cvt.s.w	$f6,$f12
	mfc1	t1,$f18
L193:	div.s	$f28,$f4,$f4
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	swc1	$f10,Lstore
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f28
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
	swc1	$f8,Lstore
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
L194:	sub.d	$f10,$f22,$f22
L195:	mov.d	$f10,$f22
	li	r5,0xa80
	ctc1	r5,C1_SR
L196:	cvt.d.s	$f10,$f0
	swc1	$f14,Lstore
L197:	add.s	$f26,$f26,$f28

	nop
	nop
	nop
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
1:
/* Register definitions: 0x55d55755 */
L198:	cvt.d.w	$f12,$f24
L199:	c.ult.s	$f12,$f8
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f2
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L200:	mul.d	$f28,$f8,$f22
	.set	noreorder
	bc1f	1f
	nop
	.set	reorder
L201:	cvt.w.s	$f14,$f30
	swc1	$f14,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L202:	c.seq.s	$f28,$f14
L203:	sub.d	$f18,$f8,$f8
	li	r5,0xc90
	ctc1	r5,C1_SR
L204:	cvt.w.s	$f22,$f6
L205:	cvt.d.s	$f30,$f28
L206:	cvt.d.s	$f20,$f20
L207:	sub.s	$f28,$f2,$f14
L208:	c.ole.d	$f8,$f8
	mfc1	t1,$f24
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L209:	cvt.d.s	$f30,$f22
L210:	cvt.w.s	$f30,$f2
L211:	c.seq.d	$f8,$f8
	mfc1	t1,$f30
L212:	c.ueq.d	$f8,$f8
1:
/* Register definitions: 0x55555755 */
L213:	mul.d	$f28,$f8,$f8
L214:	mul.s	$f22,$f8,$f28
L215:	c.olt.s	$f26,$f14
	swc1	$f12,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	.set	noreorder
	bc1t	1f
	nop
	.set	reorder
L216:	c.eq.d	$f8,$f8
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	li	r5,0x800e36
	ctc1	r5,C1_SR
L217:	cvt.w.s	$f6,$f14
L218:	div.d	$f18,$f8,$f8
L219:	cvt.d.s	$f4,$f2
	li	r5,0x11b
	ctc1	r5,C1_SR
L220:	c.un.s	$f4,$f4
L221:	cvt.w.d	$f24,$f8
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
L222:	div.s	$f10,$f22,$f24
L223:	cvt.d.w	$f10,$f10
	swc1	$f24,Lstore
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f6
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
L224:	cvt.w.s	$f8,$f10
1:
/* Register definitions: 0x55555555 */
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	lwc1	$f30,Lstore
	.data
L225:	.word	0x30b06559
	.text
	lwc1	$f26,L225
	.set	noreorder
	bc1t	1f
	nop
	.set	reorder
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
1:
/* Register definitions: 0x55555555 */
	lwc1	$f14,Lstore
	li	r5,0x800b03
	ctc1	r5,C1_SR
	swc1	$f14,Lstore
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/
	swc1	$f10,Lstore
	swc1	$f8,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
	li	t0,2
7:
	lwc1	$f8,Lstore
1:	sub	t0,1
	bne	t0,zero,7b
9:
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f0
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f2
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f4
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f6
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f8
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f10
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f12
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f14
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f16
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f18
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f20
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f22
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f24
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f26
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f28
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU integer compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
5:	nop
	nop
	mfc1	t3,$f30
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU integer macro*/
/*macro expansion for FPU control register compare*/
/*	assume t3,t4,t5,t6,t7 unused in source*/
6:	nop
	nop
	cfc1	t3,C1_SR
	.set	noreorder
	lw	t4,(t5)	/*must be atomic operation without faults*/
	addu	t5,4
	.set	reorder
	beq	t4,t3,4f
	j	bupSad
4:
/*end FPU control register macro*/

	j	bupHappy
	.end	Pon_fp_chip

#include "saio/saioctl.h"
	.extern	_regs NREGS*4

	.ent 	localexception 0
	.globl	localexception
localexception:					#from Exc or UTLBM
	subu	sp,24
	sw	ra,20(sp)
/*	assume t5,t6,t7,t2 unused in source*/
	lw	t5,_regs+R_T5*4	/*get saved OperandTable pointer*/
	lw	t6,(t5)
	addu	t5,4
	sw	t5,_regs+R_T5*4	/*update saved OperandTable pointer*/
	cfc1	t7,C1_SR
	beq	t6,t7,4f
	j	bupSad
4:
	lw	t7,_regs+R_C1_SR*4		# get FP status
	and	t6,t7,FP_RMODE | FP_STKY | FP_ENABLE | FP_COND
	sw	t6,_regs+R_C1_SR*4		# and put it back
	lw	ra,20(sp)
	addu	sp,24
	j	ra
	.end	localexception

