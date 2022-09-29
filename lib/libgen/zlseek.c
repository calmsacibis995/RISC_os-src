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
#ident	"$Header: zlseek.c,v 1.5.2.2 90/05/10 02:45:27 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	lseek(2) with error checking
*/


#include <stdio.h>
#include "errmsg.h"

long
zlseek( severity, fildes, offset, whence)
int	severity;
int	fildes;
long	offset;
int	whence;
{
	int	rc;

	if( (rc = lseek(fildes, offset, whence)) == -1 )
		_errmsg("UXzlseek1", severity,
		       "Cannot set file pointer for fildes '%d', offset '%d', whence '%d'.",
			fildes,offset,whence);


	return  rc;
}
