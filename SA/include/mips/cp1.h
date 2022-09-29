#ident "$Header: cp1.h,v 1.2 90/01/23 14:09:45 huang Exp $"
/* $Copyright$ */

#define fc0 $0
#define C1_IR $0
#define fc31 $31
#define C1_SR $31

#define fp0	$f0
#define fp1	$f1
#define fp2	$f2
#define fp3	$f3
#define fp4	$f4
#define fp5	$f5
#define fp6	$f6
#define fp7	$f7
#define fp8	$f8
#define fp9	$f9
#define fp10	$f10
#define fp11	$f11
#define fp12	$f12
#define fp13	$f13
#define fp14	$f14
#define fp15	$f15
#define fp16	$f16
#define fp17	$f17
#define fp18	$f18
#define fp19	$f19
#define fp20	$f20
#define fp21	$f21
#define fp22	$f22
#define fp23	$f23
#define fp24	$f24
#define fp25	$f25
#define fp26	$f26
#define fp27	$f27
#define fp28	$f28
#define fp29	$f29
#define fp30	$f30
#define fp31	$f31

#define RN 0
#define RZ 1
#define RP 2
#define RM 3
#define FP_RN 0
#define FP_RZ 1
#define FP_RP 2
#define FP_RM 3

#define FP_RMODE 0x3
#define FP_STKY 0x7c
#define FP_ENABLE 0xf80
#define FP_EXC 0x3f000

#define FP_STKY_I 0x4
#define FP_STKY_U 0x8
#define FP_STKY_O 0x10
#define FP_STKY_Z 0x20
#define FP_STKY_V 0x40
#define FP_EN_I 0x80
#define FP_EN_U 0x100
#define FP_EN_O 0x200
#define FP_EN_Z 0x400
#define FP_EN_V 0x800
#define FP_EXC_I 0x1000
#define FP_EXC_U 0x2000
#define FP_EXC_O 0x4000
#define FP_EXC_Z 0x8000
#define FP_EXC_V 0x10000
#define FP_EXC_E 0x20000
#define FP_COND 0x800000

#define USECP1\
		li	r2,0x30102001;\
		mtc0	r2,C0_SR
#define TRAP\
		j	diagnostic;\
bupRunaway:	j	bupRunaway;\
bupSad:		j   	bupSad;\
bupHappy:	j	bupHappy;\
diagnostic:
#define HAPPY 	j bupHappy
#define SAD 	j bupSad
#define RUNAWAY	j bupRunaway

		
