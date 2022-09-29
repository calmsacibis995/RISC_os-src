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
#ident	"$Header: cat.c,v 1.6.2.4 90/05/10 20:25:37 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include <varargs.h>

/*
	Concatenate strings.
 
	cat(destination,source1,source2,...,sourcen,0);
 
	returns destination.
*/

char *cat(dest,va_alist)
char *dest;
va_dcl
{
	register char *d, *s;
	register va_list sp;

	dummy_call();
	d = dest;
	for (va_start(sp); s = *(char **)sp; va_arg(sp, char *) ) {
		while (*d++ = *s++) ;
		d--;
	}
	return(dest);
}

/* This routine is just a stub here to hack around a compiler bug with
 * homing variables of leaf routines.
 * This should come out when the compiler bug is fixed.
 */
dummy_call()
{
}
