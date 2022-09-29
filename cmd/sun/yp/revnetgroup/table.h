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
/* $Header: table.h,v 1.1.1.2 90/05/09 19:30:17 wje Exp $ */
/*      @(#)table.h	1.1 88/03/07 4.0NFSSRC SMI   */

#define NUMLETTERS 27 /* 26 letters  + 1 for anything else */
#define TABLESIZE (NUMLETTERS*NUMLETTERS)

typedef struct tablenode *tablelist;
struct tablenode {
	char *key;
	char *datum;
	tablelist next;
};
typedef struct tablenode tablenode;

typedef tablelist stringtable[TABLESIZE];

int tablekey();
char *lookup();
void store();
