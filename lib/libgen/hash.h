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
/* $Header: hash.h,v 1.5.2.2 90/05/10 02:36:58 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


typedef	struct T_NODE {
	struct	T_NODE	*L, *R;		/* pointers to children		*/
	short	deleted;		/* TRUE-deleted; FALSE-not del.	*/
	short	balance_factor;		/* balance factor;		*/
	int	*data;			/* pointer to data;		*/
}	T_NODE;

typedef	struct T_HEAD {
	struct	T_NODE	*head;		/* pointers to children		*/
	short	height;			/* height of the tree		*/
	short	numnodes;		/* number of nodes in the tree	*/
	int	(*compare)();		/* local comparison routine	*/
}	T_HEAD;

typedef	struct	H_ENTRY {
	struct	H_ENTRY	*next;		/* chain link to next member	*/
	int	*data;
}	H_ENTRY;

typedef	struct	H_HEAD {
	struct	H_ENTRY	*head;		/* pointer to the table		*/
	short	numentries;		/* number of active entries	*/
	int	(*compare)();		/* local comparison routine	*/
	unsigned (*getkey)();		/* local key extraction routine	*/
	unsigned m;			/* size of the table		*/
}	H_HEAD;
