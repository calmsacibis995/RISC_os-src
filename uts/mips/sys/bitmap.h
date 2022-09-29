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
/* $Header: bitmap.h,v 1.1.1.2 90/05/10 06:05:41 wje Exp $ */

#ifndef	__BITMAP__
#define	__BITMAP__

typedef	unsigned long	Axis;	/* value on axis */
typedef	unsigned long	Bits;	/* we use a word to hold bits */

typedef	struct	{
	Axis	x;
	Axis	y;
} Point;

typedef	struct	{
	Point	origin;
	Point	corner;
} Rectangle;

typedef	struct	{
	Bits	*base;
	unsigned width;
} Bitmap;

extern	Bitmap	screen;

#define	Pt(x, y)	(Axis)(x), (Axis)(y)
#define	Rpt(x, y)	x, y
#define	Rect(a, b, c, d) (Axis)(a), (Axis)(b), (Axis)(c), (Axis)(d)

#define	SRC	3			/* source bitmap code */
#define	DST	5			/* dest bitmap code */
#define	NOT(x)	((~(x))&0xf)		/* inverse of function */
#define	CLEAR	(DST & NOT(DST))	/* clear bitmap to 0 */
#define	FILL	(DST | NOT(DST))	/* set bitmap to 1 */
#define	XOR	(SRC ^ DST)		/* XOR of src and dest */
#define	SOURCE	(SRC)			/* just move source bits */

Rectangle mover();
Point	poffset();
Rectangle border();

#endif	__BITMAP__
