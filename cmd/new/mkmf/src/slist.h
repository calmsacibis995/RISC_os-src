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
/* $Header: slist.h,v 1.1.1.2 90/05/09 18:04:02 wje Exp $ */

/*
 * Singly-linked list definitions
 *
 * Author: Peter J. Nicklin
 */

/*
 * Singly-linked list macros
 */
#define SLNUM(slist)	(slist)->nk
/*
 * Singly-linked list block
 */
typedef struct slblk
	{
	char *key;			/* points to a key */
	struct slblk *next;		/* ptr to next list block */
	} SLBLK;
/*
 * Singly-linked list head block
 */
typedef struct slisthb
	{
	int nk;				/* number of keys in list */
	int maxkey;			/* length of longest key */
	SLBLK *head;			/* pointer to first list block */
	SLBLK *curblk;			/* pointer to current block */
	SLBLK *tail;			/* pointer to last list block */
	} SLIST;
/*
 * Functions defined for singly-linked list operations
 */
extern char *slappend();		/* append key */
extern char *slget();			/* get next key */
extern SLIST *slinit();			/* initialize list */
extern char *slinsert();		/* insert key */
extern int slpop();			/* pop key */
extern char *slprepend();		/* prepend key */
extern void slprint();			/* print list */
extern void slrewind();			/* rewind list */
extern void slrm();			/* remove list item */
extern int slsort();			/* sort list */
extern void slsplice();			/* splice two lists */
