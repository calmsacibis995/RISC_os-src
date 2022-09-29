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
#ident	"$Header: zlink.c,v 1.5.2.2 90/05/10 02:45:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	link(2) with error checking
*/

#include	<stdio.h>
#include	"errmsg.h"

int
zlink( severity, path1, path2 )
int	severity;
char	*path1;
char	*path2;
{

	int	err_ind;

	if( (err_ind = link(path1, path2 )) == -1 )
	    _errmsg("UXzlink1", severity,
		  "Cannot create link for \"%s\" to file \"%s\".",
		   path2 , path1);

	return err_ind;
}
