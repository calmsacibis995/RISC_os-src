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
/* $Header: fpi.h,v 1.2.2.2 90/05/10 01:00:43 wje Exp $ */

#ifdef LANGUAGE_C
/* The counters */
extern int fpi_counts[];

/* string names of the causes */
extern char *fpi_list[];

extern void fpi();
extern void printfpi_counts();
#endif LANGUAGE_C

/* causes of fp interrupts in the order they are counted */
#define	FPI_SRCSNAN	0	/* source signaling NaN */
#define	FPI_SRCQNAN	1	/* source quiet NaN */
#define FPI_SRCDENORM	2	/* source denormalized value */
#define	FPI_MOVEZERO	3	/* moving a zero value R2360 only */
#define	FPI_NEGZERO	4	/* negating a zero value R2360 only */
#define	FPI_UNIMP	5	/* implemented in software only (sqrt) */
#define	FPI_INVALID	6	/* invalid operation */
#define	FPI_DIVIDE0	7	/* divide by zero */
#define	FPI_OVERFLOW	8	/* destination overflow */
#define	FPI_UNDERFLOW	9	/* destination underflow */
#define	FPI_SIZE	10
