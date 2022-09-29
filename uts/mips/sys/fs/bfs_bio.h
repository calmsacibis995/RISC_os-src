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
/* $Header: bfs_bio.h,v 1.6.4.2 90/05/10 06:14:32 wje Exp $ */

#ifndef	_SYS_FS_BFS_BIO_
#define	_SYS_FS_BFS_BIO_	1


/*
 * Map berkeley style bio into bfs_bio style.
 */
extern struct buf *bread(), *breada(), *getblk(), *geteblk();
extern int bwrite(), bdwrite(), bawrite(), brelse();

/*
 * Map required constants to bsd style.
 */
#include "sys/var.h"
#define nbuf (v.v_buf)

/*
 * In the fs_bio code, both blkno's and sizes are specified 
 * in BB's (Basic Blocks = 512).  In BSD, blkno's are same,
 * but sizes are in bytes.
 */
#define BSD_bread(dev, blkno, size)  bread(dev, blkno, BTOBB(size))
#define BSD_getblk(dev, blkno, size) getblk(dev, blkno, BTOBB(size))
#define BSD_geteblk(size) geteblk(BTOBB(size))
#define BSD_brealloc(bp, size) brealloc(bp, BTOBB(size))
#define BSD_btoss(dev, blkno, size)  btoss(dev, blkno, BTOBB(size))

/*
 * These routines don't have sizes, so they don't need mapping:
 *	 bdwrite bwrite brelse binval
 */

#endif	_SYS_FS_BFS_BIO_
