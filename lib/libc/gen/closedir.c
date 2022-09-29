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
#ident	"$Header: closedir.c,v 1.8.2.3 90/05/10 01:29:43 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	closedir -- C library extension routine

*/

#include	<sys/types.h>
#include	<dirent.h>


extern void	free();
extern int	close();

int
closedir( dirp )
register DIR	*dirp;		/* stream from opendir() */
{
	int err;

	err = close( dirp->dd_fd );
	dirp->dd_fd = -1;
	dirp->dd_size = 0;
	if (dirp->dd_buf)
		free( dirp->dd_buf );
	free( (char *)dirp );
	return(err);
}
