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
#ident	"$Header: table.c,v 1.1.1.2 90/05/09 19:30:12 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)table.c	1.1 88/03/07 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

#include <ctype.h>
#include "util.h"
#include "table.h"



/*
 * Hash table manager. Store/lookup strings, keyed by string
 */

/*
 * Generate the key into the table using the first two letters
 * of "str".  The table is alphabetized, with no distinction between
 * upper and lower case.  Non-letters are given least significance.
 */
int
tablekey(str)
	register char *str;
{
#	define TOLOWER(c) (islower(c) ? c : \
							(isupper(c) ? tolower(c) : ('a'+NUMLETTERS-1)))

	register int c1,c2;

	c1 = *str++;
	c2 = *str;
	if (c1 == EOS) {
		c2 = EOS;	/* just in case */
	}
	c1 = TOLOWER(c1) - 'a';
	c2 = TOLOWER(c2) - 'a';
	return (c1*NUMLETTERS + c2);
}


void
store(table,key,datum)
	stringtable table;
	char *key;
	char *datum;
{
	int index;
	tablelist cur,new;

	index = tablekey(key);
	cur = table[index];	

	new = MALLOC(tablenode);
	new->key = key;
	new->datum = datum;
	new->next = cur;
	table[index] = new;
}
	
	
char *
lookup(table,key)
	stringtable table;
	char *key;
{
	tablelist cur;

	cur = table[tablekey(key)];
	while (cur && strcmp(cur->key,key)) {
		cur = cur->next;
	} 
	if (cur) {
		return(cur->datum);
	} else {
		return(NULL);
	}
}
		
	
