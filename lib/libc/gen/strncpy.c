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
#ident	"$Header: strncpy.c,v 1.6.2.2 90/05/10 01:38:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Copy s2 to s1, truncating or null-padding to always copy n bytes
 * return s1
 */
#include <sys/types.h>

char *
strncpy(s1, s2, n)
register char *s1, *s2;
register size_t n;
{
	register char *os1 = s1;

	n++;				
	while ((--n > 0) &&  ((*s1++ = *s2++) != '\0'))
		;
	if (n > 0)
		while (--n > 0)
			*s1++ = '\0';
	return (os1);
}
