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
/* $Header: bfs_magic.h,v 1.4.4.2 90/05/10 06:14:59 wje Exp $ */

#ifndef	_SYS_FS_BFS_MAGIC_
#define	_SYS_FS_BFS_MAGIC_	1


/*
 * bfs_magic.h
 *
 * This isn't magic. It just contains BFS_IMAGIC for external inclusion.
 * This lets other modules see the inode magic number without getting 
 * lots of structures and #defs.
 */
#define BFS_IMAGIC 0x0BFD4343

#endif	_SYS_FS_BFS_MAGIC_
