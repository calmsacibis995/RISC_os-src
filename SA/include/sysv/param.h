#ident "$Header: param.h,v 1.3 90/01/23 13:37:27 huang Exp $"
/* $Copyright$ */

/*
 * $Locker:  $
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:27 $
 * $State: Exp $	$Author: huang $
 * $Log:	param.h,v $
 * Revision 1.3  90/01/23  13:37:27  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:11:17  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  15:59:57  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  11:06:09  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * fundamental variables
 * don't change too often
 */

#define	MAXPID	30000		/* max process id */
#define	MAXUID	60000		/* max user id */
#define	MAXLINK	1000		/* max links */

#define	SSIZE	1		/* initial stack size (*4096 bytes) */
#define	SINCR	1		/* increment of stack (*4096 bytes) */
#define	USIZE	2		/* size of user block (*4096 bytes) */

#define	CANBSIZ	256		/* max size of typewriter line	*/
#define	HZ	100		/* 100 ticks/second of the clock */
#define	NCARGS	5120		/* # characters in exec arglist */

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

#define	NOFILE	20

/*	The following represent the minimum and maximum values to
**	which the paramater NOFILES in the kernel master file may
**	be set.
*/

#define	NOFILES_MIN	 20
#define	NOFILES_MAX	100

/*	The following defines apply to the kernel virtual
**	address space.
*/

#define	SYSSEGSZ 1024	/* The size of the kernel segment	*/
			/* sysseg in pages.  The starting	*/
			/* address comes from the vuifile.	*/
#define	NS0SDE	0x120!	/* Nbr of kernel segments in section 0.	*/
#define	NS1SDE	0x38!	/* Nbr of kernel segments in section 1.	*/
#define	NS2SDE	0x0!	/* Nbr of kernel segments in section 2.	*/
#define	NS3SDE	0x1!	/* Nbr of kernel segments in section 3.	*/

/*	To avoid prefetch errors at the end of a region, it must
**	be padded with the following number of bytes.
*/

#define	PREFETCH	12!

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

/*
 * fundamental constants of the implementation--
 * cannot be changed easily
 */

/*
 * For the MIPS R2000, a "segment" is one page of page tables == 4 Meg
 */

#ifndef LOCORE
#define	NBPW	sizeof(int)	/* number of bytes in an integer */
#endif !LOCORE
#define	NCPS	1024		/* Number of clicks per segment */
#define	NBPC	4096		/* Number of bytes per click */
#define NBPS	(NCPS*NBPC)	/* Number of bytes per segment */
#define	BPCSHIFT	12	/* LOG2(NBPC) if exact */
#define	NICFREE	50		/* number of superblock free blocks */
#define	NULL	0
#define	CMASK	0		/* default mask for file creation */
#define	CDLIMIT	(1L<<11)	/* default max write address */
#define	NODEV	(dev_t)(-1)
#define	ROOTINO	((ino_t)2)	/* i number of all roots */
#define	SUPERB	((daddr_t)1)	/* block number of the super block */
#define	DIRSIZ	14		/* max characters per directory */
#define	NICINOD	100		/* number of superblock inodes */
#define	NBPSCTR		512	/* Bytes per disk sector.	*/
#define SCTRSHFT	9	/* Shift for BPSECT.		*/

/*
 * filesystem parameters
 */

#ifndef FsTYPE
#define FsTYPE	3
#endif


#if FsTYPE==1
/* Original 512 byte file system */
#define	BSIZE		512		/* size of secondary block (bytes) */
#define SBUFSIZE	512		/* system buffer size */
#define	BSHIFT		9		/* log2(BSIZE) */
#define	NINDIR		(BSIZE/sizeof(daddr_t))	/* BSIZE/sizeof(daddr_t) */
#define	BMASK		0777		/* BSIZE-1 */
#define INOPB		8		/* BSIZE/sizeof(struct dinode) */
#define INOSHIFT	3		/* log2(INOPB) */
#define	NMASK		0177		/* NINDIR-1 */
#define	NSHIFT		7		/* log2(NINDIR) */
#define NDPC		4		/* number of blocks/click */
#define Fs2BLK		0x8000		/* large block flag in dev */
#define FsBSIZE(dev)	BSIZE
#define FsBSHIFT(dev)	BSHIFT
#define FsNINDIR(dev)	NINDIR
#define FsBMASK(dev)	BMASK
#define FsINOPB(dev)	INOPB
#define FsLTOP(dev, b)	b
#define FsPTOL(dev, b)	b
#define FsNMASK(dev)	NMASK
#define FsNSHIFT(dev)	NSHIFT
#define FsITOD(dev, x)	(daddr_t)(((unsigned)x+(2*INOPB-1)) >> INOSHIFT)
#define FsITOO(dev, x)	(daddr_t)(((unsigned)x+(2*INOPB-1)) & (INOPB-1))
#define FsINOS(dev, x)	((x&~07)+1)
#endif

#if FsTYPE==2
/* New 1024 byte file system */
#define	BSIZE		1024		/* size of secondary block (bytes) */
#define SBUFSIZE	1024		/* system buffer size */
#define	BSHIFT		10		/* log2(BSIZE) */
#define	NINDIR		(BSIZE/sizeof(daddr_t))	/* BSIZE/sizeof(daddr_t) */
#define	BMASK		01777		/* BSIZE-1 */
#define INOPB		16		/* BSIZE/sizeof(struct dinode) */
#define INOSHIFT	4		/* log2(INOPB) */
#define	NMASK		0377		/* NINDIR-1 */
#define	NSHIFT		8		/* log2(NINDIR) */
#define NDPC		4		/* number of blocks per click */
#define Fs2BLK		0x8000		/* large block flag in dev */
#define FsBSIZE(dev)	BSIZE
#define FsBSHIFT(dev)	BSHIFT
#define FsNINDIR(dev)	NINDIR
#define FsBMASK(dev)	BMASK
#define FsINOPB(dev)	INOPB
#define FsLTOP(dev, b)	(b<<1)
#define FsPTOL(dev, b)	(b>>1)
#define FsNMASK(dev)	NMASK
#define FsNSHIFT(dev)	NSHIFT
#define FsITOD(dev, x)	(daddr_t)(((unsigned)x+(2*INOPB-1)) >> INOSHIFT)
#define FsITOO(dev, x)	(daddr_t)(((unsigned)x+(2*INOPB-1)) & (INOPB-1))
#define FsINOS(dev, x)	((x&~017)+1)
#endif

#if FsTYPE==3
/* Dual file system */
#define	BSIZE		512		/* size of secondary block (bytes) */
#define SBUFSIZE	1024		/* system buffer size */
#define	BSHIFT		9		/* log2(BSIZE) */
#define	NINDIR		128		/* BSIZE/sizeof(daddr_t) */
#define	BMASK		0777		/* BSIZE-1 */
#define INOPB		8		/* BSIZE/sizeof(struct dinode) */
#define INOSHIFT	3		/* log2(INOPB) */
#define	NMASK		0177		/* NINDIR-1 */
#define	NSHIFT		7		/* log2(NINDIR) */
#define NDPC		4
#define Fs2BLK		0x8000		/* large block flag in dev */
#define FsLRG(dev)	(dev&Fs2BLK)
#define FsBSIZE(dev)	(FsLRG(dev) ? 1024 : 512)
#define FsBSHIFT(dev)	(FsLRG(dev) ? 10 : 9)
#define FsNINDIR(dev)	(FsLRG(dev) ? 256 : 128)
#define FsBMASK(dev)	(FsLRG(dev) ? 01777 : 0777)
#define FsINOPB(dev)	(FsLRG(dev) ? 16 : 8)
#define FsLTOP(dev, b)	(FsLRG(dev) ? (b<<1) : b)
#define FsPTOL(dev, b)	(FsLRG(dev) ? (b>>1) : b)
#define FsNMASK(dev)	(FsLRG(dev) ? 0377 : 0177)
#define FsNSHIFT(dev)	(FsLRG(dev) ? 8 : 7)
#define FsITOD(dev, x)	(daddr_t)(FsLRG(dev) ? \
	((unsigned)x+31)>>4 : ((unsigned)x+15)>>3)
#define FsITOO(dev, x)	(daddr_t)(FsLRG(dev) ? \
	((unsigned)x+31)&017 : ((unsigned)x+15)&07)
#define FsINOS(dev, x)	(FsLRG(dev) ? \
	((x&~017)+1) : ((x&~07)+1))
#endif

#define SUPERBOFF	512	/* superblock offset */
#define	UMODE	3		/* current Xlevel == user */
#define	USERMODE(psw)	(((psw) & SR_KUP) == SR_KUP)
#define	BASEPRI(psw)	(((psw) & SR_IMASK) == SR_IMASK)

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

/*
 *  Interrupt stack size in STKENT units
 */
#define ISTKSZ	200
#define	QSTKSZ	1000

#define	MAXSUSE	255
