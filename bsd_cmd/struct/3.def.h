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
/* $Header: 3.def.h,v 1.1.1.2 90/05/07 19:23:30 wje Exp $ */

/*	3.def.h	4.2	83/08/11	*/

#define RECURSE(p,v,r)	{ for (r = 0; r < CHILDNUM(v); ++r) if (DEFINED(LCHILD(v,r))) p(LCHILD(v,r)); if (DEFINED(RSIB(v))) p(RSIB(v)); }

#define IFTHEN(v)		( NTYPE(v) == IFVX && !DEFINED(LCHILD(v,ELSE)))

#define BRK(v)	FATH(v)		/* lexical successor of v, for ITERVX only */
#define LABEL(v)	REACH(v)
