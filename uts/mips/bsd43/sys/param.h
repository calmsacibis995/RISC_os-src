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
/* $Header: param.h,v 1.8.1.4 90/05/10 04:53:34 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)param.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define	BSD43_BSD	43		/* 4.3 * 10, as cpp doesn't do floats */
#define BSD43_BSD4_3	1
#define BSD43_BSD43_NFS	1

/*
 * Machine type dependent parameters.
 */
#ifdef KERNEL
#include "../machine/machparam.h"
#else
#include <bsd43/machine/machparam.h>
#endif

#define	BSD43_NPTEPG		(BSD43_NBPG/(sizeof (struct bsd43_(pte))))

/*
 * Machine-independent constants
 */
#define	BSD43_NMOUNT	20		/* number of mountable file systems */
/* NMOUNT must be <= 255 unless c_mdev (cmap.h) is expanded */
#define	BSD43_MSWAPX	BSD43_NMOUNT		/* pseudo mount table index for swapdev */
#define	BSD43_MAXUPRC	40		/* max processes per user */
#define	BSD43_NOFILE	64		/* max open files per process */
#define BSD43_MAXLINK 32767           /* max links */
#define	BSD43_CANBSIZ	256		/* max size of typewriter line */
#define	BSD43_NCARGS	20480		/* # characters in exec arglist */
#define	BSD43_NGROUPS	16		/* max number groups */

#define	BSD43_NOGROUP	65535		/* marker for empty group set member */

/*
 * Priorities
 */
#define BSD43_PCATCH  0400    /* return if sleep interrupted, don't longjmp */
#define	BSD43_PSWP	0
#define	BSD43_PINOD	10
#define	BSD43_PRIBIO	20
#define	BSD43_PRIUBA	24
#define	BSD43_PZERO	25
#define	BSD43_PPIPE	26
#define	BSD43_PVFS	27
#define	BSD43_PWAIT	30
#define	BSD43_PLOCK	35
#define	BSD43_PSLEP	40
#define	BSD43_PUSER	50

#define	BSD43_NZERO	0

/*
 * Signals
 */
#ifdef KERNEL
#include "signal.h"
#else
#include <bsd43/signal.h>
#endif

#define	BSD43_ISSIG(p) \
	((p)->p_sig && ((p)->p_flag&BSD43_STRC || \
	 ((p)->p_sig &~ ((p)->p_sigignore | (p)->p_sigmask))) && bsd43_(issig)())

#define	BSD43_NBPW	sizeof(int)	/* number of bytes in an integer */

#define	BSD43_NULL	0
#define	BSD43_CMASK	022		/* default mask for file creation */
#define	BSD43_NODEV	(dev_t)(-1)

/*
 * Clustering of hardware pages on machines with ridiculously small
 * page sizes is done here.  The paging subsystem deals with units of
 * CLSIZE pte's describing NBPG (from vm.h) pages each.
 *
 * NOTE: SSIZE, SINCR and UPAGES must be multiples of CLSIZE
 */
#define	BSD43_CLBYTES		(BSD43_CLSIZE*BSD43_NBPG)
#define	BSD43_CLOFSET		(BSD43_CLSIZE*BSD43_NBPG-1)	/* for clusters, like PGOFSET */
#define	bsd43_claligned(x)	((((int)(x))&BSD43_CLOFSET)==0)
#define	BSD43_CLOFF		BSD43_CLOFSET
#define	BSD43_CLSHIFT		(BSD43_PGSHIFT+BSD43_CLSIZELOG2)

#if BSD43_CLSIZE==1
#define	bsd43_clbase(i)	(i)
#define	bsd43_clrnd(i)	(i)
#define	bsd43_for_CLSIZE(x)	(x) = 0;
#else
/* give the base virtual address (first of CLSIZE) */
#define	bsd43_clbase(i)	((i) &~ (BSD43_CLSIZE-1))
/* round a number of clicks up to a whole cluster */
#define	bsd43_clrnd(i)	(((i) + (BSD43_CLSIZE-1)) &~ (BSD43_CLSIZE-1))
#define	bsd43_for_CLSIZE(x)	bsd43_(for) ((x) = 0; (x) < BSD43_CLSIZE; (x)++)
#endif

/* CBLOCK is the size of a clist block, must be power of 2 */
#define	BSD43_CBLOCK	64
#define	BSD43_CBSIZE	(BSD43_CBLOCK - sizeof(struct bsd43_(cblock) *))	/* data chars/clist */
#define	BSD43_CROUND	(BSD43_CBLOCK - 1)				/* clist rounding */

#ifndef KERNEL
#include	<bsd43/sys/types.h>
#else
#ifndef LOCORE
#include	"types.h"
#endif
#endif

/*
 * File system parameters and macros.
 *
 * The file system is made out of blocks of at most MAXBSIZE units,
 * with smaller units (fragments) only in the last direct block.
 * MAXBSIZE primarily determines the size of buffers in the buffer
 * pool. It may be made larger without any effect on existing
 * file systems; however making it smaller make make some file
 * systems unmountable.
 *
 * Note that the blocked devices are assumed to have DEV_BSIZE
 * "sectors" and that fragments must be some multiple of this size.
 * Block devices are read in BLKDEV_IOSIZE units. This number must
 * be a power of two and in the range of
 *	DEV_BSIZE <= BLKDEV_IOSIZE <= MAXBSIZE
 * This size has no effect upon the file system, but is usually set
 * to the block size of the root file system, so as to maximize the
 * speed of ``fsck''.
 */
#define	BSD43_MAXBSIZE	8192
#define	BSD43_DEV_BSIZE	512
#define	BSD43_DEV_BSHIFT	9		/* log2(DEV_BSIZE) */
#define BSD43_BLKDEV_IOSIZE	2048
#define BSD43_MAXFRAG 	8

#define	bsd43_btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> BSD43_DEV_BSHIFT)
#define	bsd43_dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << BSD43_DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and will be after we
 * add an entry to cdevsw for that purpose.  For now though
 * just use DEV_BSIZE.
 */
#define	bsd43_bdbtofsb(bn)	((bn) / (BSD43_BLKDEV_IOSIZE/BSD43_DEV_BSIZE))

/*
 * MAXPATHLEN defines the longest permissable path length,
 * including the termination null, after expanding symbolic links. 
 * It is used to allocate a temporary buffer from the buffer 
 * pool in which to do the name expansion, hence should be a power 
 * of two, and must be less than or equal to MAXBSIZE.
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name. It should be set high
 * enough to allow all legitimate uses, but halt infinite loops
 * reasonably quickly.
 */
#define BSD43_MAXPATHLEN	1024
#define BSD43_MAXSYMLINKS	30

/*
 * bit map related macros
 */
#define	bsd43_setbit(a,i)	((a)[(i)/BSD43_NBBY] |= 1<<((i)%BSD43_NBBY))
#define	bsd43_clrbit(a,i)	((a)[(i)/BSD43_NBBY] &= ~(1<<((i)%BSD43_NBBY)))
#define	bsd43_isset(a,i)	((a)[(i)/BSD43_NBBY] & (1<<((i)%BSD43_NBBY)))
#define	bsd43_isclr(a,i)	(((a)[(i)/BSD43_NBBY] & (1<<((i)%BSD43_NBBY))) == 0)

/*
 * Macros for fast min/max.
 */
#define	BSD43_MIN(a,b) (((a)<(b))?(a):(b))
#define	BSD43_MAX(a,b) (((a)>(b))?(a):(b))

/*
 * Macros for counting and rounding.
 */
#ifndef bsd43_howmany
#define bsd43_howmany(x, y)   	(((unsigned)((x)+((y)-1)))/(unsigned)(y))
#endif
#define	bsd43_roundup(x, y)	((bsd43_howmany((x), (y)))*(y))

/*
 * Maximum size of hostname recognized and stored in the kernel.
 */
#define	BSD43_MAXHOSTNAMELEN	64		/* maximum host name length */
#define	BSD43_MAXDOMNAMELEN	64		/* maximum domain name length */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BLKDEV_IOSIZE BSD43_BLKDEV_IOSIZE
#   define BSD BSD43_BSD
#   define BSD4_3 BSD43_BSD4_3
#   define BSD43_NFS BSD43_BSD43_NFS
#   define CANBSIZ BSD43_CANBSIZ
#   define CBLOCK BSD43_CBLOCK
#   define CBSIZE BSD43_CBSIZE
#   define CLBYTES BSD43_CLBYTES
#   define CLOFF BSD43_CLOFF
#   define CLOFSET BSD43_CLOFSET
#   define CLSHIFT BSD43_CLSHIFT
#   define CMASK BSD43_CMASK
#   define CROUND BSD43_CROUND
#   define DEV_BSHIFT BSD43_DEV_BSHIFT
#   define DEV_BSIZE BSD43_DEV_BSIZE
#   define ISSIG BSD43_ISSIG
#   define MAX BSD43_MAX
#   define MAXBSIZE BSD43_MAXBSIZE
#   define MAXDOMNAMELEN BSD43_MAXDOMNAMELEN
#   define MAXFRAG BSD43_MAXFRAG
#   define MAXHOSTNAMELEN BSD43_MAXHOSTNAMELEN
#   define MAXLINK BSD43_MAXLINK
#   define MAXPATHLEN BSD43_MAXPATHLEN
#   define MAXSYMLINKS BSD43_MAXSYMLINKS
#   define MAXUPRC BSD43_MAXUPRC
#   define MIN BSD43_MIN
#   define MSWAPX BSD43_MSWAPX
#   define NBPW BSD43_NBPW
#   define NCARGS BSD43_NCARGS
#   define NGROUPS BSD43_NGROUPS
#   define NMOUNT BSD43_NMOUNT
#   define NODEV BSD43_NODEV
#   define NOFILE BSD43_NOFILE
#   define NOGROUP BSD43_NOGROUP
#   define NPTEPG BSD43_NPTEPG
#   define NULL BSD43_NULL
#   define NZERO BSD43_NZERO
#   define PCATCH BSD43_PCATCH
#   define PINOD BSD43_PINOD
#   define PLOCK BSD43_PLOCK
#   define PPIPE BSD43_PPIPE
#   define PRIBIO BSD43_PRIBIO
#   define PRIUBA BSD43_PRIUBA
#   define PSLEP BSD43_PSLEP
#   define PSWP BSD43_PSWP
#   define PUSER BSD43_PUSER
#   define PVFS BSD43_PVFS
#   define PWAIT BSD43_PWAIT
#   define PZERO BSD43_PZERO
#   define bdbtofsb bsd43_bdbtofsb
#   define btodb bsd43_btodb
#   define claligned bsd43_claligned
#   define clbase bsd43_clbase
#   define clrbit bsd43_clrbit
#   define clrnd bsd43_clrnd
#   define dbtob bsd43_dbtob
#   define for_CLSIZE bsd43_for_CLSIZE
#   define howmany bsd43_howmany
#   define isclr bsd43_isclr
#   define isset bsd43_isset
#   define roundup bsd43_roundup
#   define setbit bsd43_setbit
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


