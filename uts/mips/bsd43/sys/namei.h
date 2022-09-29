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
/* $Header: namei.h,v 1.8.1.2 90/05/10 04:53:28 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)namei.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef BSD43__NAMEI_
#define	BSD43__NAMEI_

#ifdef KERNEL
#include "uio.h"
#else
#include <bsd43/sys/uio.h>
#endif

/*
 * Encapsulation of namei parameters.
 * One of these is located in the u. area to
 * minimize space allocated on the kernel stack.
 */
struct bsd43_(nameidata) {
	caddr_t	ni_dirp;		/* pathname pointer */
	short	ni_nameiop;		/* see below */
	short	ni_error;		/* error return if any */
	off_t	ni_endoff;		/* end of useful stuff in directory */
	struct	bsd43_(inode) *ni_pdir;		/* inode of parent directory of dirp */
	struct	bsd43_(iovec) ni_iovec;		/* MUST be pointed to by ni_iov */
	struct	bsd43_(uio) ni_uio;		/* directory I/O parameters */
	struct	bsd43_(direct) ni_dent;		/* current directory entry */
};

#define	bsd43_ni_base		ni_iovec.iov_base
#define	bsd43_ni_count	ni_iovec.iov_len
#define	bsd43_ni_iov		ni_uio.uio_iov
#define	bsd43_ni_iovcnt	ni_uio.uio_iovcnt
#define	bsd43_ni_offset	ni_uio.uio_offset
#define	bsd43_ni_segflg	ni_uio.uio_segflg
#define	bsd43_ni_resid	ni_uio.uio_resid

/*
 * namei operations and modifiers
 */
#define	BSD43_LOOKUP		0	/* perform name lookup only */
#define	BSD43_CREATE		1	/* setup for file creation */
#define	BSD43_DELETE		2	/* setup for file deletion */
#define	BSD43_LOCKPARENT	0x10	/* see the top of namei */
#define BSD43_NOCACHE		0x20	/* name must not be left in cache */
#define BSD43_FOLLOW		0x40	/* follow symbolic links */
#define	BSD43_NOFOLLOW	0x0	/* don't follow symbolic links (pseudo) */

/*
 * This structure describes the elements in the cache of recent
 * names looked up by namei.
 */
struct	bsd43_(nch) {
	struct	bsd43_(nch) *nc_forw, *nc_back;	/* hash chain, MUST BE FIRST */
	struct	bsd43_(nch) *nc_nxt, **nc_prev;	/* LRU chain */
	struct	bsd43_(inode) *nc_ip;		/* inode the name refers to */
	ino_t	nc_ino;			/* ino of parent of name */
	dev_t	nc_dev;			/* dev of parent of name */
	dev_t	nc_idev;		/* dev of the name ref'd */
	long	nc_id;			/* referenced inode's id */
	char	nc_nlen;		/* length of name */
#define	BSD43_NCHNAMLEN	15	/* maximum name segment length we bother with */
	char	nc_name[BSD43_NCHNAMLEN];	/* segment name */
};
#ifdef KERNEL
struct	bsd43_(nch) *bsd43_(nch);
int	bsd43_(nchsize);
#endif

/*
 * Stats on usefulness of namei caches.
 */
struct	bsd43_(nchstats) {
	long	ncs_goodhits;		/* hits that we can reall use */
	long	ncs_badhits;		/* hits we must drop */
	long	ncs_falsehits;		/* hits with id mismatch */
	long	ncs_miss;		/* misses */
	long	ncs_long;		/* long names that ignore cache */
	long	ncs_pass2;		/* names found with passes == 2 */
	long	ncs_2passes;		/* number of times we attempt it */
};
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CREATE BSD43_CREATE
#   define DELETE BSD43_DELETE
#   define FOLLOW BSD43_FOLLOW
#   define LOCKPARENT BSD43_LOCKPARENT
#   define LOOKUP BSD43_LOOKUP
#   define NCHNAMLEN BSD43_NCHNAMLEN
#   define NOCACHE BSD43_NOCACHE
#   define NOFOLLOW BSD43_NOFOLLOW
#   define _NAMEI_ BSD43__NAMEI_
#   define ni_base bsd43_ni_base
#   define ni_count bsd43_ni_count
#   define ni_iov bsd43_ni_iov
#   define ni_iovcnt bsd43_ni_iovcnt
#   define ni_offset bsd43_ni_offset
#   define ni_resid bsd43_ni_resid
#   define ni_segflg bsd43_ni_segflg
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


