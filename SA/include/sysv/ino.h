#ident "$Header: ino.h,v 1.3 90/01/23 13:37:22 huang Exp $"
/* $Copyright$ */

/*
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:22 $
 * $State: Exp $	$Author: huang $
 * $Log:	ino.h,v $
 * Revision 1.3  90/01/23  13:37:22  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:11:08  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  15:59:54  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  11:03:10  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

	/*
	 #ident "@(#)kern-3b2:sys/ino.h	1.3"
	 */
/*
 *	Inode structure as it appears on a disk block.
 */

struct	dinode
{
	ushort	di_mode;	/* mode and type of file */
	short	di_nlink;    	/* number of links to file */
	ushort	di_uid;      	/* owner's user id */
	ushort	di_gid;      	/* owner's group id */
	off_t	di_size;     	/* number of bytes in file */
	char  	di_addr[40];	/* disk block addresses */
	time_t	di_atime;   	/* time last accessed */
	time_t	di_mtime;   	/* time last modified */
	time_t	di_ctime;   	/* time created */
};
/*
 * The 40 address bytes:
 *	39 used; 13 addresses
 *	of 3 bytes each.
 */
