/* --------------------------------------------------- */
/* | Copyright (c) 1988 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: machine_info.h,v 1.1.3.4.1.2.1.2 90/11/06 17:17:24 beacker Exp $ */

#ifndef	_MACHINE_INFO_
#define	_MACHINE_INFO_	1

int machine_info();

/*
 * Commands
 */

#define MI_MACHINE	0	/* machine type */
#define MI_MACHCLASS	1	/* machine class */

/*
 * Machine types
 */

#define MIT_UNKNOWN	0
#define MIT_M500	1
#define MIT_M800	2
#define MIT_M1000	3
#define MIT_M2000_6	4
#define MIT_M2000_8	5
#define MIT_M120_3	6
#define MIT_M120_5	7
#define MIT_R2030	8
#define MIT_R3240	9
#define MIT_R6280	10
#define MIT_R3030	11
#define MIT_RB3125	12
#define	MIT_RESERVED	13
#define MIT_R6260	14
#define MIT_RB3133      105

/*
 * Machine classes
 */

#define MIC_UNKNOWN	0
#define MIC_1		1	/* class 1: M500/800/1000 */
#define MIC_2		2	/* class 2: RC6280/M2000* */
#define MIC_3		3	/* class 3: RC3240/M120*  */
#define MIC_4		4	/* class 4: RC2030/RC3030 */

#endif	_MACHINE_INFO_
