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
/* $Header: bfs_yech.h,v 1.7.3.2 90/05/10 06:15:42 wje Exp $ */

#ifndef	_SYS_FS_BFS_YECH_
#define	_SYS_FS_BFS_YECH_	1


/*
 * bsd_yech.h
 *
 * This file is a collection of little things that don't
 * really belong anywhere else.
 */

/*
 * These macros allow tracing to be turned on by poking bfs_dbg in kmem.
 */
#ifdef DEBUG
    extern int bfs_dbg;
#   define BFS_DBG_TRACE	0x0002
#   define BFS_DBG_WARN		0x0004
#   define dbg		(!(bfs_dbg))?0:printf
#   define dbg_trace	(!(bfs_dbg&BFS_DBG_TRACE))?0:printf
#   define dbg_warn	(!(bfs_dbg&BFS_DBG_WARN))?0:printf
#else
#   define dbg
#   define dbg_trace
#   define dbg_warn
#endif

/*
 * Sys V has no uprintf, so print user's message on the console instead.
 * ~~ It would be good to support uprintf.
 */
#define uprintf printf

/*
 * Random definitions.
 */
#ifndef NULL
#    define NULL 0
#endif


/*
 * Required definitions from files that don't need to be
 * completely included.
 * 
 * ~~ Can I pick some of these up out of Sys V headers, so that
 * if sysV changes, this header need not?
 */
typedef	unsigned short	uid_t;		/* BSD: types.h */
typedef	unsigned short	gid_t;		/* BSD: types.h */
typedef	struct	_quad { long val[2]; } quad;	/* BSD: types.h ~~~ needed?*/
#define NBBY	8		/* BSD: types.h */

extern daddr_t	rablock;	/* BSD: systm.h */
extern int	rasize;		/* BSD: systm.h */

#ifdef R6000
#define CLBYTES		16384
#else !R6000
#define CLBYTES		4096	/* BSD: param.h + machine/machparam.h */
				/* ~~~ Should really get this from SV 
				 * defines. */
#endif !R6000
#define CLSIZE		1	/* BSD: machine/machparam.h */


#ifdef KERNEL
/**
 ** Fight with inconsitencies between SV and BSD.
 **/
#define log					/* ~~~ Fix this better. */
#define LOG_NOTICE	1			/* ~~~ And this */
#define LOG_ERR		1			/* ~~~ And this */

#define panic(X)	cmn_err(CE_PANIC, X)

/*
 * Function declaration
 */
    struct buf *BSD_realloccg(), *BSD_alloc();		/* BSD: buf.h */
    daddr_t BSD_bmap();

#define bfs_getfs(mp) \
	( (struct fs *)((struct buf *)(mp)->m_bufp)->b_un.b_addr )

#endif KERNEL

#endif	_SYS_FS_BFS_YECH_
