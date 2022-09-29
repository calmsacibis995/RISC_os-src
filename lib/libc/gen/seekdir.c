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
#ident	"$Header: seekdir.c,v 1.9.2.2 90/05/10 01:37:18 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	seekdir -- C library extension routine

*/

#include	<sys/types.h>
#include	<dirent.h>

extern long	lseek();

#define NULL	0

void
seekdir(dirp, loc)
register DIR	*dirp;		/* stream from opendir() */
long		loc;		/* position from telldir() */
{

	register struct dirent * dp;
	 
	if (telldir(dirp) == loc)
		return; 		/* save time */
	dirp->dd_loc = 0;
	lseek(dirp->dd_fd, loc, 0);
	dirp->dd_size = 0;
}
