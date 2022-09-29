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
/* $Header: cs5_dir.h,v 1.4.4.2 90/05/10 06:16:01 wje Exp $ */

#ifndef	_SYS_FS_CS5_DIR_
#define	_SYS_FS_CS5_DIR_	1


/*
 * Directory format for the cs5 file system.  This is the same structure
 * as the regular s5 file system.
 */

struct	cs5_direct {
	ushort	d_ino;
	char	d_name[14];
};

#endif	_SYS_FS_CS5_DIR_
