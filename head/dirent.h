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
/* $Header: dirent.h,v 1.9.3.2 90/05/10 00:59:57 wje Exp $ */

#ifndef	_DIRENT_
#define	_DIRENT_	1



/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define MAXNAMLEN	255		/* maximum filename length */
#define DIRBUF		4096		/* buffer size for fs-indep. dirs */


typedef struct
	{
	int	dd_fd;			/* file descriptor */
	int	dd_loc;			/* offset in block */
	int	dd_size;		/* amount of valid data */
	char	*dd_buf;		/* directory block */
	}	DIR;			/* stream data from opendir() */

extern DIR		*opendir();
extern struct dirent	*readdir();
extern long		telldir();
extern void		seekdir();
extern int		closedir();

#define rewinddir( dirp )	seekdir( dirp, 0L )
#include <sys/dirent.h>

#endif	_DIRENT_
