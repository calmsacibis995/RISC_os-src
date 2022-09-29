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
#ident	"$Header: zmknod.c,v 1.5.2.2 90/05/10 02:46:05 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	mknod(2) with error checking
*/

#include	<stdio.h>
#include	"errmsg.h"

int
zmknod( severity, path, mode, dev )
int	severity;
char	*path;
int	mode;
int	dev;
{

	int	err_ind;

	if( (err_ind = mknod(path, mode, dev )) == -1 )
	    _errmsg("UXzmknod1", severity,
		  "Cannot create file \"%s\" of mode %d on device %d.",
		   path, mode, dev);

	return err_ind;
}
