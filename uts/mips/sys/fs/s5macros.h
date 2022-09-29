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
/* $Header: s5macros.h,v 1.8.4.2 90/05/10 06:18:57 wje Exp $ */

#ifndef	_SYS_FS_S5MACROS_
#define	_SYS_FS_S5MACROS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#if FsTYPE==1

#define FsBSIZE(bsize)	BSIZE
#define FsBSHIFT(bsize)	BSHIFT
#define FsNINDIR(bsize)	NINDIR
#define FsBMASK(bsize)	BMASK
#define FsINOPB(bsize)	INOPB
#define FsLTOP(bsize, b)	(b)
#define FsPTOL(bsize, b)	(b)
#define FsNMASK(bsize)	NMASK
#define FsNSHIFT(bsize)	NSHIFT
#define FsITOD(bsize, x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1)) >> INOSHIFT)
#define FsITOO(bsize, x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1)) & (INOPB-1))
#define FsINOS(bsize, x)	(((x)&~07)+1)
#endif

#if FsTYPE==2
#define FsBSIZE(bsize)	BSIZE
#define FsBSHIFT(bsize)	BSHIFT
#define FsNINDIR(bsize)	NINDIR
#define FsBMASK(bsize)	BMASK
#define FsINOPB(bsize)	INOPB
#define FsLTOP(bsize, b)	((b)<<1)
#define FsPTOL(bsize, b)	((b)>>1)
#define FsNMASK(bsize)	NMASK
#define FsNSHIFT(bsize)	NSHIFT
#define FsITOD(bsize, x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1)) >> INOSHIFT)
#define FsITOO(bsize, x)	(daddr_t)(((unsigned)(x)+(2*INOPB-1)) & (INOPB-1))
#define FsINOS(bsize, x)	(((x)&~017)+1)
#endif
 
#if FsTYPE==3

#define FsLRG(bsize)	((bsize)==1024)
/*
#define FsBSIZE(bsize)	(FsLRG(bsize) ? 1024 : 512)
*/
#define FsBSIZE(bsize)	(bsize)
#define FsBSHIFT(bsize)	(FsLRG(bsize) ? 10 : 9)
#define FsNINDIR(bsize)	(FsLRG(bsize) ? 256 : 128)
#define FsBMASK(bsize)	(FsLRG(bsize) ? 01777 : 0777)
#define FsINOPB(bsize)	(FsLRG(bsize) ? 16 : 8)
#define FsLTOP(bsize, b)	(FsLRG(bsize) ? ((b)<<1) : (b))
#define FsPTOL(bsize, b)	(FsLRG(bsize) ? ((b)>>1) : (b))
#define FsNMASK(bsize)	(FsLRG(bsize) ? 0377 : 0177)
#define FsNSHIFT(bsize)	(FsLRG(bsize) ? 8 : 7)
#define FsITOD(bsize, x)	(daddr_t)(FsLRG(bsize) ? \
	((unsigned)(x)+31)>>4 : ((unsigned)(x)+15)>>3)
#define FsITOO(bsize, x)	(daddr_t)(FsLRG(bsize) ? \
	((unsigned)(x)+31)&017 : ((unsigned)(x)+15)&07)
#define FsINOS(bsize, x)	(FsLRG(bsize) ? \
	(((x)&~017)+1) : (((x)&~07)+1))
#endif

/*
 * Backwards more-or-less compatabile interface to new buffer cache
 */
#define LTOPBLK(blkno, bsize)	((blkno) * (((bsize)>>SCTRSHFT)))

#define	s5bread(dev, bn, bsize) \
	bread(dev, LTOPBLK(bn, bsize), BTOBB(bsize))

#define	s5breada(dev, bn, rabn, bsize) \
	breada(dev, LTOPBLK(bn, bsize), BTOBB(bsize), \
		    LTOPBLK(rabn, bsize), BTOBB(bsize))

#define	s5getblk(dev, bn, bsize) \
	getblk(dev, LTOPBLK(bn, bsize), BTOBB(bsize))

#define	s5geteblk() \
	geteblk(BTOBB(SBUFSIZE))

#endif	_SYS_FS_S5MACROS_
