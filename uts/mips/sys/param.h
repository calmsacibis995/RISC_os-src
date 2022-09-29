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
/* $Header: param.h,v 1.31.1.4 90/05/10 06:31:10 wje Exp $ */

#ifndef	_SYS_PARAM_
#define	_SYS_PARAM_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#undef SYSV
#define SYSV	30	/* 3.0 * 10 to dodge float */

#ifdef RISCOS

#ifdef KERNEL
#include <sys/types.h>
#endif /* KERNEL */

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(a,b) (((a)>(b))?(a):(b))

#endif /* RISCOS */

/*
 * don't change too often
 */
#define	BSIZE		512		/* size of secondary block (bytes) */

#define	MAXPID	30000		/* max process id */
#define	MAXUID	65536		/* max user id */
#define MAXGID	65536		/* max group id */
#define	MAXLINK	1000		/* max links */

#define	SSIZE	1		/* initial stack size (*NBPP bytes) */
#define	SINCR	1		/* increment of stack (*NBPP bytes) */
#ifdef R6000
#define USIZE	1		/* size of user block (*NBPP bytes) */
#else
#define	USIZE	2		/* size of user block (*NBPP bytes) */
#endif !R6000

#define	CANBSIZ	256		/* max size of typewriter line	*/
#define	HZ	100		/* 100 ticks/second of the clock */

#define	NCARGS	20480		/* # characters in exec arglist */
				/*   must be multiple of NBPW.  */
#define	NGROUPS	16		/* max number groups */

#define	NOGROUP	65535		/* marker for empty group set member */

/*	The following define is here for temporary compatibility
**	and should be removed in the next release.  It gives a
**	value for the maximum number of open files per process.
**	However, this value is no longer a constant.  It is a
**	configurable parameter, NOFILES, specified in the kernel
**	master file and available in v.v_nofiles.  Programs which
**	include this header file and use the following value may
**	not operate correctly if the system has been configured
**	to a different value.
*/

#ifndef NOFILE
#define	NOFILE	20
#endif NOFILE

/*	The following represent the minimum and maximum values to
**	which the paramater NOFILES in the kernel master file may
**	be set.
*/

#define	NOFILES_MIN	 20
#define	NOFILES_MAX	100

/*
 * priorities
 * should not be altered too much
 */
#define	PMASK	0177
#define	PCATCH	0400
#define	PSWP	0
#define	PINOD	10
#define	PRIBIO	20
#define	PZERO	25
#define PMEM	0
#define	NZERO	20
#define	PPIPE	26
#define	PWAIT	30
#define	PSLEP	39
#define	PUSER	60
#define	PIDLE	127

/* some from NFS4.0 release */
#define PVFS	27
#define	PLOCK	35

/*
 * fundamental constants of the implementation--
 * cannot be changed easily
 *
 */

#ifndef NBBY
#define NBBY	8		/* number of bits in a byte */
#endif
#define	NBPW	sizeof(int)	/* number of bytes in an integer */

/*
 * On the MIPS Mark I CPUs a click is a page (4K), a segment is what a 
 * click can map (i.e. a page of ptes or 4K/8 * 4K = 2 Meg).
 * On the MIPS Mark II CPUs, a segment is the same size (2 Meg), but it
 * takes fewer than a page of ptes to map a segment.
 */

#define NBPC_I	4096		/* Mips1 page size */
#define NCPS_I	(NBPC_I/8)	/*   and num bytes per click */
#define NBPS	(NCPS_I*NBPC_I)	/* Number of bytes per segment */

#ifdef R6000
#define	NBPC	16384		/* Number of bytes per click */
#define	BPCSHIFT	14	/* LOG2(NBPC) if exact */
#else
#define	NBPC	4096		/* Number of bytes per click */
#define	BPCSHIFT	12	/* LOG2(NBPC) if exact */
#endif !R6000

#define	NCPS	(NBPS/NBPC)	/* Number of clicks per segment */

#ifndef NULL
#define	NULL	0
#endif
#define	CMASK	0		/* default mask for file creation */
#define	CDLIMIT	((1L<<(31-SCTRSHFT))-1)	/* default max write address */
#define	NODEV	(dev_t)(-1)
#define	NBPSCTR		512	/* Bytes per disk sector.	*/
#define SCTRSHFT	9	/* Shift for BPSECT.		*/

/*
 *	The following defines the maximum size of the kernel
 *	virtual address space in pages.  The actual size is determined 
 *	in startup.c based on factors such as physical memory size.
 *	This value is used where a number is required for static array
 *	allocation.  Increasing this value is cheap, but 512M should 
 *	last a while.
 */
#define	SYSSEGSZ_MAX (512*1024*1024/NBPC)

#define	UMODE	3		/* current Xlevel == user */
/* in mips the psw is the status register */
#define	USERMODE(psw)	(((psw) & SR_KUP) == SR_KUP)
#ifdef R6000
#define BASEPRI(psw)	is_spl_basepri(psw)
#else
#define	BASEPRI(psw)	(((psw) & SR_IMASK) == (SR_IMASK&ipl_special_mask))
#endif !R6000

#ifdef MIPSEB
#define	lobyte(X)	(((unsigned char *)&X)[1])
#define	hibyte(X)	(((unsigned char *)&X)[0])
#define	loword(X)	(((ushort *)&X)[1])
#define	hiword(X)	(((ushort *)&X)[0])
#else
#define	lobyte(X)	(((unsigned char *)&X)[0])
#define	hibyte(X)	(((unsigned char *)&X)[1])
#define	loword(X)	(((ushort *)&X)[0])
#define	hiword(X)	(((ushort *)&X)[1])
#endif

#ifdef INKERNEL
/*
 * DELAY(n) should be n microseconds, roughly.  This is a first guess.
 */
#define DELAY(n)	{ \
	extern int delay_mult; \
	register int N = delay_mult*(n); \
	while (--N > 0); \
	}
#endif INKERNEL

/*
 *  Interrupt stack size in STKENT units XXX - check this out
 */
#define QSTKSZ	1000
#define ISTKSZ	200

#define	MAXSUSE	255

/* REMOTE -- whether machine is primary, secondary, or regular */
#define SYSNAME 9		/* # chars in system name */
#define PREMOTE 39

#ifdef KERNEL
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
#define	FSMAXBSIZE	8192
#define	DEV_BSIZE	NBPSCTR
#define	DEV_BSHIFT	SCTRSHFT		/* log2(DEV_BSIZE) */
#define BLKDEV_IOSIZE	4096
#define MAXFRAG 	8

#define	btodb(bytes)	 		/* calculates (bytes / DEV_BSIZE) */ \
	((unsigned)(bytes) >> DEV_BSHIFT)
#define	dbtob(db)			/* calculates (db * DEV_BSIZE) */ \
	((unsigned)(db) << DEV_BSHIFT)

/*
 * Map a ``block device block'' to a file system block.
 * This should be device dependent, and will be after we
 * add an entry to cdevsw for that purpose.  For now though
 * just use DEV_BSIZE.
 */
#define	bdbtofsb(bn)	((bn) / (BLKDEV_IOSIZE/DEV_BSIZE))
#endif KERNEL

/*
 * MAXPATHLEN defines the longest permissable path length
 * after expanding symbolic links. It is used to allocate
 * a temporary buffer from the buffer pool in which to do the
 * name expansion, hence should be a power of two, and must
 * be less than or equal to MAXBSIZE.
 */
#ifndef MAXPATHLEN
#define MAXPATHLEN	1024
#endif

#ifdef KERNEL
/*
 * MAXSYMLINKS defines the maximum number of symbolic links
 * that may be expanded in a path name. It should be set high
 * enough to allow all legitimate uses, but halt infinite loops
 * reasonably quickly.
 */
#ifndef MAXSYMLINKS
#define MAXSYMLINKS	30
#endif

/*
 * bit map related macros
 */
#ifndef setbit
#define	setbit(a,i)	((a)[(i)/NBBY] |= 1<<((i)%NBBY))
#define	clrbit(a,i)	((a)[(i)/NBBY] &= ~(1<<((i)%NBBY)))
#define	isset(a,i)	((a)[(i)/NBBY] & (1<<((i)%NBBY)))
#define	isclr(a,i)	(((a)[(i)/NBBY] & (1<<((i)%NBBY))) == 0)
#endif setbit

/*
 * Macros for counting and rounding.
 */
#ifndef howmany
#define howmany(x, y)   (((unsigned)((x)+((y)-1)))/(unsigned)(y))
#endif
#ifndef roundup
#define	roundup(x, y)	((howmany((x), (y)))*(y))
#endif
#endif KERNEL

/* Maximum size of hostname and domainname recognized and stored in the
 * kernel by sethostname and setdomainname
 */
#define MAXHOSTNAMELEN 64

#endif	_SYS_PARAM_
