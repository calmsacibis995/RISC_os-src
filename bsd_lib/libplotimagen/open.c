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
#ident	"$Header: open.c,v 1.1.2.2 90/05/09 14:40:50 wje Exp $"

/*
 * Copyright (c) 1985 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char sccsid[] = "@(#)open.c	5.2 (Berkeley) 9/21/85";
#endif not lint

#include "imPcodes.h"
#include "imp.h"

openpl(){

	putch(imP_SET_HV_SYSTEM);
	  putch((3<<3)|5);
	putch(imP_SET_FAMILY);
	  putch(2);
	setfont(imP_charset,imPcsize);
	putch(imP_SET_IL);
	  putwd(imPcsize+3);
	putch(imP_SET_SP);
	  putwd(imPcsize);
	putch(imP_SET_PEN);
	  putch(2);
	putch(imP_SET_ABS_H);
	  putwd(0);
	putch(imP_SET_ABS_V);
	  putwd(0);
}
setfont(c, sz) char *c; int sz;
{
	imPcsize = sz;
	putch(imP_CREATE_FAMILY_TABLE);
	  putch(2);
	  putch(1);
	  putch(0);
	  fprintf(stdout, c);
	  putch(0);
}
