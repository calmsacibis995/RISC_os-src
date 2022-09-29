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
/* $Header: dirent.h,v 1.8.4.2 90/05/10 06:09:27 wje Exp $ */

#ifndef	_SYS_DIRENT_H_
#define	_SYS_DIRENT_H_

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * The following structure defines the file
 * system independent directory entry.
 *
 */

struct dirent				/* data from readdir() */
	{
	long		d_ino;		/* inode number of entry */
	off_t		d_off;		/* offset of disk direntory entry */
	unsigned short	d_reclen;	/* length of this record */
	char		d_name[1];	/* name of file */
	};

#define	DIRENTBASESIZE \
	(((struct dirent *) 0)->d_name - (char *) 0)
#define	DIRENTSIZE(namelen) \
	((DIRENTBASESIZE + (namelen) + NBPW) & ~(NBPW - 1))

#endif	_SYS_DIRENT_H_
