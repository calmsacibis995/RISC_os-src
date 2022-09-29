#ident "$Header: delaymacs.h,v 1.2 90/01/23 14:10:42 huang Exp $"
/* $Copyright$ */

#define delay1     			\
					\
		nop;


#define delay2     			\
					\
		nop;			\
		nop;


#define delay3     			\
					\
		nop;			\
		nop;			\
		nop;


#define delay4     			\
					\
		nop;			\
		nop;			\
		nop;			\
		nop;


#define delay5(reg)			\
					\
		addiu	reg,r0,1;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay6(reg)			\
					\
		nop;			\
		addiu	reg,r0,1;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay7(reg)			\
					\
		addiu	reg,r0,2;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay8(reg)			\
					\
		nop;			\
		addiu	reg,r0,2;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay9(reg)			\
					\
		addiu	reg,r0,3;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay10(reg)			\
					\
		nop;			\
		addiu	reg,r0,3;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay11(reg)			\
					\
		addiu	reg,r0,4;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay12(reg)			\
					\
		nop;			\
		addiu	reg,r0,4;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay13(reg)			\
					\
		addiu	reg,r0,5;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay14(reg)			\
					\
		nop;			\
		addiu	reg,r0,5;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay15(reg)			\
					\
		addiu	reg,r0,6;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay16(reg)			\
					\
		nop;			\
		addiu	reg,r0,6;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay17(reg)			\
					\
		addiu	reg,r0,7;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay18(reg)			\
					\
		nop;			\
		addiu	reg,r0,7;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay19(reg)			\
					\
		addiu	reg,r0,8;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay20(reg)			\
					\
		nop;			\
		addiu	reg,r0,8;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay21(reg)			\
					\
		addiu	reg,r0,9;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay22(reg)			\
					\
		nop;			\
		addiu	reg,r0,9;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay23(reg)			\
					\
		addiu	reg,r0,10;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay24(reg)			\
					\
		nop;			\
		addiu	reg,r0,10;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay25(reg)			\
					\
		addiu	reg,r0,11;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay26(reg)			\
					\
		nop;			\
		addiu	reg,r0,11;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay27(reg)			\
					\
		addiu	reg,r0,12;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay28(reg)			\
					\
		nop;			\
		addiu	reg,r0,12;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay29(reg)			\
					\
		addiu	reg,r0,13;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay30(reg)			\
					\
		nop;			\
		addiu	reg,r0,13;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay31(reg)			\
					\
		addiu	reg,r0,14;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay32(reg)			\
					\
		nop;			\
		addiu	reg,r0,14;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay33(reg)			\
					\
		addiu	reg,r0,15;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay34(reg)			\
					\
		nop;			\
		addiu	reg,r0,15;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay35(reg)			\
					\
		addiu	reg,r0,16;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay36(reg)			\
					\
		nop;			\
		addiu	reg,r0,16;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay37(reg)			\
					\
		addiu	reg,r0,17;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay38(reg)			\
					\
		nop;			\
		addiu	reg,r0,17;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay39(reg)			\
					\
		addiu	reg,r0,18;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;


#define delay40(reg)			\
					\
		nop;			\
		addiu	reg,r0,18;	\
1:		bne	reg,r0,1b;	\
		addiu	reg,reg,-1;
