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
/* $Header: 2.def.h,v 1.1.1.2 90/05/07 19:22:46 wje Exp $ */

/*	2.def.h	4.2	83/08/11	*/

extern int accessnum;		/* number of nodes accessible from START */
extern VERT *after;		/* node numbers associated with after numbers of depth first search */
extern int *ntobef;		/* before numbers associated with nodes */
extern int *ntoaft;		/* after numbers associated with nodes */
