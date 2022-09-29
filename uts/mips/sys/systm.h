/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: systm.h,v 1.18.1.2.1.1.1.2 90/11/07 11:44:53 beacker Exp $ */

#ifndef	_SYS_SYSTM_
#define	_SYS_SYSTM_	1

#include <bsd/sys/time.h>


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * Random set of variables used by more than one routine.
 */

#ifdef KERNEL
extern struct vnode *rootdir;	/* pointer to vnode of root directory */
extern struct vnode *rootvp;	/* vnode of root filesystem */
extern short cputype;		/* type of cpu = 40, 45, 70, 780, 0x3b5 */
extern time_t lbolt;		/* time in HZ since last boot */
extern struct timeval time;	/* time from 1970 */
extern struct timeval boottime; /* time from 1970 of last boot */

extern char runin;		/* scheduling flag */
extern char runout;		/* scheduling flag */
extern char runrun;		/* scheduling flag */
extern char curpri;		/* current priority */
extern struct proc *curproc;	/* current proc */
extern struct proc *runq;	/* head of linked list of running processes */

extern		maxmem;		/* max available memory (clicks) */
extern		physmem;	/* physical memory (clicks) on this CPU */
extern		syssegsz;	/* Size of kernel virtual map in clicks. */
extern		maxclick;	/* Highest physical click + 1.		*/
extern daddr_t	swplo;		/* block number of start of swap space */
extern		nswap;		/* size of swap space in blocks*/
extern dev_t	rootdev;	/* device of the root */
extern dev_t	swapdev;	/* swapping device */
extern dev_t	pipedev;	/* pipe device */
extern dev_t	dumpdev;	/* dump device */
extern long	dumplo;		/* location on dump device where dump starts */
extern char	*panicstr;	/* panic string pointer */
extern		blkacty;	/* active block devices */
extern		pwr_cnt, pwr_act;
extern int 	(*pwr_clr[])();

extern	char	*kmemalloc();
extern	char	*kmemzalloc();
extern	void	kmemfree();

#ifndef	SGI_KMEM_MACROS
extern	char	*kern_calloc();
extern	char	*kern_malloc();
extern	void	kern_free();
#endif	SGI_KMEM_MACROS

#ifndef	NFS4_KMEM_MACROS
extern	char	*kmem_alloc();
extern	char	*kmem_zalloc();
extern	void	kmem_free();
extern	char	*kmem_fast_alloc();
extern	char	*kmem_fast_zalloc();
extern	void	kmem_fast_free();
#endif	NFS4_KMEM_MACROS

extern	int	showconfig;
extern	int	disable_parity;
/* extern	dev_t getmdev(); Conflict with ufs/fs/ufs_vnodeops.c */
extern	struct vnode *bdevvp();
extern	struct vnode *specvp();
/* extern	struct inode *namei(); */
/* extern	struct inode *remote_call(); */
extern	struct buf *getblk();
extern	struct buf *geteblk();
extern	struct buf *bread();
extern	struct buf *breada();
extern	struct file *falloc();
extern	int	upath();

extern unsigned int ipl_special_mask;
#endif /* KERNEL */

/*
 * Structure of the system-entry table
 */
extern struct sysent {
	char	sy_narg;		/* total number of arguments */
	int	(*sy_call)();		/* handler */
	char	sy_flags;		/* See below */
} sysent[], bsd_sysent[], posix_sysent[];

/* sy_flags values. */
#define SY_SETJMP	0x01		/* Do a setjmp before the syscall. */
#define SY_SIGRET	0x02		/* Return from signal handler. */

/*
 * To support NUMIPS, syscall() has been generalized to search for system
 * calls in more than one array.  (See numips.h).  The sysent_tab structure
 * lists the sysent arrays that syscall() should look for when handling
 * a system call.
 */
struct sysent_tab {
	struct sysent *sysentp;
	int nents;
};

#endif	_SYS_SYSTM_
