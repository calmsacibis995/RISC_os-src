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
/* $Header: systm.h,v 1.7.1.2 90/05/10 04:56:39 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systm.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) -----------------------------------------------------*/

#ifndef _BSD43_SYS_SYSTM_
#define _BSD43_SYS_SYSTM_	1

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Random set of variables
 * used by more than one
 * routine.
 */
extern	char bsd43_(version)[];		/* system version */
#ifdef mips
extern	char *bsd43_(release_version);	/* system release version */
#endif mips

/*
 * Nblkdev is the number of entries
 * (rows) in the block switch. It is
 * set in binit/bio.c by making
 * a pass over the switch.
 * Used in bounds checking on major
 * device numbers.
 */
#ifdef mips
extern int	bsd43_(nblkdev);
#else !mips
int		bsd43_(nblkdev);
#endif !mips

/*
 * Number of character switch entries.
 * Set by cinit/prim.c
 */
#ifdef mips
extern int	bsd43_(nchrdev);
#else !mips
int	bsd43_(nchrdev);
#endif !mips

int	nswdev;			/* number of swap devices */
int	mpid;			/* generic for unique process id's */
char	runin;			/* scheduling flag */
char	runout;			/* scheduling flag */
int	runrun;			/* scheduling flag */
char	kmapwnt;		/* kernel map want flag */
char	curpri;			/* more scheduling */

int	maxmem;			/* actual max memory per process */
int	physmem;		/* physical memory on this CPU */

int	nswap;			/* size of swap space */
int	updlock;		/* lock for sync */
daddr_t	rablock;		/* block to be read ahead */
int	rasize;			/* size of block in rablock */
extern	int bsd43_(intstack)[];	/* stack for interrupts */
dev_t	rootdev;	/* device of the root */
struct vnode *rootvp;	/* vnode of root filesystem */
dev_t	dumpdev;	/* device to take dumps on */
struct vnode *dumpvp;           /* vnode to dump on */
long	dumplo;			/* offset into dumpdev */

#ifdef mips
extern dev_t	bsd43_(swapdev);		/* swapping device */
#else !mips
dev_t	bsd43_(swapdev);		/* swapping device */
#endif !mips
struct vnode	*swapdev_vp;	/* vnode equivalent to above */

dev_t	argdev;			/* device for argument lists */
struct vnode	*argdev_vp;	/* vnode equivalent to above */

#ifdef vax
extern	int bsd43_(icode)[];		/* user init code */
extern	int bsd43_(szicode);		/* its size */
#endif vax
#ifdef mips
extern	int bsd43_(icode)();		/* user init code */
extern char bsd43_(eicode)[];		/* address of end of icode */
#endif mips

daddr_t	bsd43_(bmap)();
caddr_t	bsd43_(calloc)();
int	bsd43_(memall)();
int	bsd43_(vmemall)();
caddr_t	bsd43_(wmemall)();
swblk_t	bsd43_(vtod)();
struct vnode *bdevvp();
struct vnode *specvp();

/*
 * Structure of the system-entry table
 */
extern struct bsd43_(sysent)
{
	int	sy_narg;		/* total number of arguments */
	int	(*sy_call)();		/* handler */
} bsd43_(sysent)[];

int	noproc;			/* no one is running just now */
char	*panicstr;
int	wantin;
#ifdef vax
int	boothowto;		/* reboot flags, from console subsystem */
#endif vax
int	selwait;

#ifndef mips
extern	char bsd43_(vmmap)[];		/* poor name! */
#endif !mips

/* casts to keep lint happy */
#define	bsd43_insque(q,p)	_insque((caddr_t)q,(caddr_t)p)
#define	bsd43_remque(q)	_remque((caddr_t)q)
#define	bsd43_queue(q,p)	_queue((caddr_t)q,(caddr_t)p)
#define	bsd43_dequeue(q)	_dequeue((caddr_t)q)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define dequeue bsd43_dequeue
#   define insque bsd43_insque
#   define queue bsd43_queue
#   define remque bsd43_remque
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/
#endif _BSD43_SYS_SYSTM_


