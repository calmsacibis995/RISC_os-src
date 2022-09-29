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
/* $Header: fsid.h,v 1.10.4.2 90/05/10 06:19:57 wje Exp $ */

#ifndef	_SYS_FSID_
#define	_SYS_FSID_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/* Fstyp names for use in fsinfo structure. These names */
/* must be constant across releases and will be used by a */
/* user level routine to map fstyp to fstyp index as used */
/* ip->i_fstyp. This is necessary for programs like mount. */

#define S51K	"S51K"
#define CS51K	"S51K"
#define PROC	"PROC"
#define DUFST	"DUFST"
#define	FSID_COM	"com"
#define	FSID_BFS	"ffs"
#define	FSID_EFS	"efs"
#define	FSID_EFS2	"efs2"
#define	FSID_NFS	"nfs"
#define	FSID_SOCKET	"socket"

#endif	_SYS_FSID_
