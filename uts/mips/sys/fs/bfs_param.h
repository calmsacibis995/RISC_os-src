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
/* $Header: bfs_param.h,v 1.6.4.3 90/05/10 06:15:05 wje Exp $ */

#ifndef	_SYS_FS_BFS_PARAM_
#define	_SYS_FS_BFS_PARAM_	1


/*
 * File system parameters and macros.
 *
 * The file system is made out of blocks of at most BFS_MAXBSIZE units,
 * with smaller units (fragments) only in the last direct block.
 * BFS_MAXBSIZE primarily determines the size of buffers in the buffer
 * pool. It may be made larger without any effect on existing
 * file systems; however making it smaller make make some file
 * systems unmountable.
 *
 * Note that the blocked devices are assumed to have DEV_BSIZE
 * "sectors" and that fragments must be some multiple of this size.
 * Block devices are read in BLKDEV_IOSIZE units. This number must
 * be a power of two and in the range of
 *	DEV_BSIZE <= BLKDEV_IOSIZE <= BFS_MAXBSIZE
 * This size has no effect upon the file system, but is usually set
 * to the block size of the root file system, so as to maximize the
 * speed of ``fsck''.
 *
 * SysV Note: BLKDEV_IOSIZE is not used in the BFS implementation and
 * has been removed.  It was (4*1024) in BSD.
 */
#define BFS_MAXBSIZE	(8*1024)	/* ~~Anyone use this? */
#define BFS_DEV_BSIZE	512		/* ~~ Define to BBSIZE? */
#define BFS_DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define BFS_MAXFRAG 	8


/*
 * Bytes_to_disk_blocks and visa/versa.
 */
#define	btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> BFS_DEV_BSHIFT)
#define	dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << BFS_DEV_BSHIFT)

/*
 * MAXPATHLEN defines the longest permissable path length
 * after expanding symbolic links. It is used to allocate
 * a temporary buffer from the buffer pool in which to do the
 * name expansion, hence should be a power of two, and must
 * be less than or equal to BFS_MAXBSIZE.
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name. It should be set high
 * enough to allow all legitimate uses, but halt infinite loops
 * reasonably quickly.
 */
#define BFS_MAXPATHLEN	1024
#define BFS_MAXSYMLINKS	8

/*
 * bit map related macros
 */
#define	setbit(a,i)	((a)[(i)/NBBY] |= 1<<((i)%NBBY))
#define	clrbit(a,i)	((a)[(i)/NBBY] &= ~(1<<((i)%NBBY)))
#define	isset(a,i)	((a)[(i)/NBBY] & (1<<((i)%NBBY)))
#define	isclr(a,i)	(((a)[(i)/NBBY] & (1<<((i)%NBBY))) == 0)

/*
 * Macros for counting and rounding.
 */
#define howmany(x, y)   (((unsigned)((x)+((y)-1)))/(unsigned)(y))
#define	roundup(x, y)	((howmany((x), (y)))*(y))

#endif	_SYS_FS_BFS_PARAM_
