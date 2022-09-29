#ident "$Header: orgmacs.h,v 1.2 90/01/23 14:18:24 huang Exp $"
/* $Copyright$ */

#define leave1instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;	\
		nop;		\
		.align	6;	\
		nop;		\
		.align	5;	\
		nop;		\
		.align	4;	\
		nop;		\
		.align	3;	\
		nop;		\
		.align	2;

#define leave2instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;	\
		nop;		\
		.align	6;	\
		nop;		\
		.align	5;	\
		nop;		\
		.align	4;	\
		nop;		\
		.align	3;

#define leave4instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;	\
		nop;		\
		.align	6;	\
		nop;		\
		.align	5;	\
		nop;		\
		.align	4;

#define leave8instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;	\
		nop;		\
		.align	6;	\
		nop;		\
		.align	5;

#define leave16instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;	\
		nop;		\
		.align	6;

#define leave32instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;	\
		nop;		\
		.align	7;

#define leave64instr		\
				\
		.align	11;	\
		nop;		\
		.align	10;	\
		nop;		\
		.align	9;	\
		nop;		\
		.align	8;

#define nextpage		\
				\
		.align	12

#define newpage			\
				\
		.align	12

