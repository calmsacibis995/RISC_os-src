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
#ident	"$Header: zulimit.c,v 1.5.2.2 90/05/10 02:47:48 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	ulimit(2) with error checking
*/

#include	"errmsg.h"

daddr_t
zulimit( severity, cmd, newlimit )
int	 severity;
int	 cmd;
daddr_t	 newlimit;
{
	daddr_t	err_ind;

	if( (err_ind = ulimit(cmd, newlimit )) ==  - 1 )
	    _errmsg("UXzulimit1", severity,
		  "Cannot get or set user limits, cmd %d ,newlimit is %d.",
		   cmd, newlimit);

	return err_ind;
}
