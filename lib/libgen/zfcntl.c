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
#ident	"$Header: zfcntl.c,v 1.5.2.2 90/05/10 02:43:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	fcntl(2) with error checking
*/

#include	"errmsg.h"

int
zfcntl( severity, fildes, cmd, arg )
int	 severity;
int	 fildes;
int	 cmd;
int	 arg;
{

	int	err_ind;

	if( (err_ind = fcntl(fildes, cmd, arg )) == -1 )
	    _errmsg ( "UXzfcntl1", severity,
		  "Cannot provide file control for fildes %d cmd %d and arg %d.",
		   fildes, cmd, arg);

	return err_ind;
}
