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
#ident	"$Header: old_tolower.c,v 1.2.2.2 90/05/10 01:35:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	3.0 SID #	1.2	*/
/*LINTLIBRARY*/
/*
 * If arg is upper-case, return the lower-case, else return the arg.
 * The purpose of this file is to prevent a.outs compiled with the
 * shared C library from failing with the new tolower function
 * The new tolower function introduces an unresolved reference to _libc__ctype
 * that will make those a.outs not work right.
 */

int
old_tolower(c)
register int c;
{
	if(c >= 'A' && c <= 'Z')
		c -= 'A' - 'a';
	return(c);
}
