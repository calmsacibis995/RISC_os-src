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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: fixpoint.h,v 1.1.4.2 90/05/10 06:13:28 wje Exp $ */

/*
 * Fix-point arithmetic package
 */

/*
 * Basic fix-point types
 */
/*
 * TODO: should probably move this over to types.h so that avenrun is
 * not defined in vm_sched.c
 */
typedef	int 		fix;
typedef	unsigned int	ufix;

/*
 * Number of fraction bits.
 */
#define FBITS		8

/*
 * Conversion to fix-point representation
 * works with int, float, double, char, ....
 */
#define	TO_FIX(x)	((fix)((x)*(1<<FBITS)))

/*
 * Conversion from fix-point to various integer datatypes
 */
#define	FIX_TO_SHORT(x)		((short)((x)>>FBITS))
#define	FIX_TO_INT(x)		((int)((x)>>FBITS))

/*
 * Conversion from fix-point to double
 */
#define	FIX_TO_DBL(x)	(((double)(x))/(1<<FBITS))

/*
 * Multiplication/division of 2 fix-point values
 */
#define	MUL_2FIX(x, y)	(((x)*(y))>>FBITS)
#define	DIV_2FIX(x, y)	(((x)<<FBITS)/(y))
