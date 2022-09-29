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
#ident	"$Header: zfseek.c,v 1.5.2.2 90/05/10 02:44:52 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	fseek(3S) with error checking
*/

#include	<stdio.h>
#include	"errmsg.h"

int
zfseek( severity, stream, offset, ptrname )
int	 severity;
FILE	*stream;
long	 offset;
int	 ptrname;
{

	int	err_ind;

	if( (err_ind = fseek(stream, offset, ptrname )) < 0 )
	    _errmsg ( "UXzfseek1", severity,
		  "Cannot reposition file pointer, offset %d and ptrname %d.",
		   offset, ptrname );

	return err_ind;
}
