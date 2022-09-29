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
/* $Header: dlist.h,v 1.1.1.2 90/05/09 18:00:28 wje Exp $ */

/*
 * Dependency list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Dependency list block
 */
typedef struct _dlblk
	{
	int d_type;			/* source file type */
	struct slblk *d_src;		/* points to a source list block */
	struct _iblk *d_incl;		/* pointer to include block chain */
	struct _dlblk *d_next;		/* ptr to next list block */
	} DLBLK;
/*
 * Dependency list head block
 */
typedef struct _dlisthb
	{
	DLBLK *d_head;			/* pointer to first list block */
	DLBLK *d_tail;			/* pointer to last list block */
	} DLIST;
/*
 * Functions defined for dependency list operations
 */
extern DLBLK *dlappend();		/* append to list */
extern DLIST *dlinit();			/* initialize list */
extern void dlprint();			/* print list */
