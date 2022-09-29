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
/* $Header: print.h,v 1.7.2.2 90/05/10 01:42:50 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* Maximum number of digits in any integer representation */
#define MAXDIGS 11

/* Maximum total number of digits in E format */
#if u3b || u3b5 || mips
#define MAXECVT 17
#else
#define MAXECVT 18
#endif

/* Maximum number of digits after decimal point in F format */
#define MAXFCVT 60

/* Maximum significant figures in a floating-point number */
#define MAXFSIG MAXECVT

/* Maximum number of characters in an exponent */
#if u3b || u3b5 || mips
#define MAXESIZ 5
#else
#define MAXESIZ 4
#endif

/* Maximum (positive) exponent */
#if u3b || u3b5 || mips
#define MAXEXP 325	/* DAG -- bug fix (was 310) */
#else
#if gould
#define MAXEXP 80
#else
#define MAXEXP 40
#endif
#endif

/* Data type for flags */
typedef char bool;

/* Convert a digit character to the corresponding number */
#define tonumber(x) ((x)-'0')

/* Convert a number between 0 and 9 to the corresponding digit */
#define todigit(x) ((x)+'0')

/* Max and Min macros */
#define max(a,b) ((a) > (b)? (a): (b))
#define min(a,b) ((a) < (b)? (a): (b))

