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
/* $Header: saghdr.h,v 1.5.2.2 90/05/09 18:27:40 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	saghdr.h 1.4 of 5/13/85	*/
#include <stdio.h>
#define	NPTS	100
#define NFLD	9
#define	FLDCH	10
#ifndef	DEBUG
#define	DEBUG	0
#endif

struct	entry	{
	char	tm[9];
	float	hr;
	float	val;
	char	qfld[8];
	};

struct	array	{
	char	hname[56];
	struct	entry	ent[NPTS];
	};


struct	c	{
	char	name[60];
	char	op;
	struct	array	*dptr;
	};

struct	p	{
	char	spec[60];
	struct	c	c[5];
	char	mn[10], mx[10];
	float	min, max;
	int	jitems;
	int	mode;
	};
