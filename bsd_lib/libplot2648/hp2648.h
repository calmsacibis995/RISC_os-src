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
/* $Header: hp2648.h,v 1.1.3.2 90/05/07 21:46:44 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)hp2648.h	5.1 (Berkeley) 5/7/85
 *
 *
 * Displays plot files on an hp2648a graphics terminals.  I have heard
 * that all hp plotting devices use the same control sequences, so this
 * might work for all hp devices capable of plotting.
 */

#include <stdio.h>

#define TERMINAL "/dev/tty"

#define	ENQ	05
#define ACK	06
#define ESC	033
#define GRAPHIC	'*'
#define MODE	'm'
#define PLOT	'p'
#define DISPLAY 'd'
#define PENUP	'a'
#define BINARY	'i'
#define ASCII	'f'
#define CR	'\n'

#define TRUE  1
#define FALSE 0

#define xsc(xi) ((int) (xi - lowx) * scalex + 0.5)
#define ysc(yi) ((int) (yi - lowy) * scaley + 0.5)

extern int shakehands;
extern int currentx;
extern int currenty;
extern int buffcount;
extern int fildes;
extern float lowx;
extern float lowy;
extern float scalex;
extern float scaley;
extern struct sgttyb sarg;
