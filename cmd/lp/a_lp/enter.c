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
#ident	"$Header: enter.c,v 1.4.2.2 90/05/09 16:25:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* enter(name, array, size, max) -- enters name into an argv-type array.

	name -- item to be entered into array
	array -- array to be added to
	size -- pointer to the # of items in array
	max -- max # of items allowed in array

	return codes: 0 for success
		     -1 for failure
*/

#include	"lp.h"


int
enter(name, array, size, max)
char *name;
char *array[];
int *size;
int max;
{
	char *p, *malloc(), *strcpy();

	if(*size >= max)	/* no room in array */
		return(-1);
	if(name == NULL)
		array[(*size)++] = NULL;
	else {
		if((p = malloc((unsigned)(strlen(name)+1))) == NULL)
			return(-1);
		array[(*size)++] = p;
		strcpy(p, name);
	}
	return(0);
}
