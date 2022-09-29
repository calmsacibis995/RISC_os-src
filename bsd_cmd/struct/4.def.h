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
/* $Header: 4.def.h,v 1.1.1.2 90/05/07 19:24:38 wje Exp $ */

/*	4.def.h	4.2	83/08/11	*/

#define YESTAB	TRUE
#define NOTAB	FALSE
#define TABOVER(n)	tabover(n,outfd)
#define OUTSTR(x)		fprintf(outfd,"%s",x)
#define OUTNUM(x)		fprintf(outfd,"%d",x)


extern LOGICAL *brace;
#define YESBRACE(v,i)	{ if (DEFINED(LCHILD(v,i))) brace[LCHILD(v,i)] = TRUE; }
#define NOBRACE(v,i)	{ if (DEFINED(LCHILD(v,i))) brace[LCHILD(v,i)] = FALSE; }
#define HASBRACE(v,i)	 ((DEFINED(LCHILD(v,i))) ? brace[LCHILD(v,i)] : TRUE)
