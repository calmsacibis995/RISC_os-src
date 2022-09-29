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
#ident	"$Header: xlink.c,v 1.5.2.2 90/05/10 01:11:53 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	Interface to link(II) which handles all error conditions.
	Returns 0 on success,
	fatal() on failure.
*/

# include	"errno.h"

xlink(f1,f2)
{
	extern errno;
	extern char Error[];

	if (link(f1,f2)) {
		if (errno == EEXIST || errno == EXDEV) {
			sprintf(Error,"can't link `%s' to `%s' (%d)",
				f2,f1,errno == EEXIST ? 111 : 112);
			return(fatal(Error));
		}
		if (errno == EACCES)
			f1 = f2;
		return(xmsg(f1,"xlink"));
	}
	return(0);
}
