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
/* $Header: mtio.h,v 1.7.1.2 90/05/10 04:53:22 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)mtio.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Structures and definitions for mag tape io control commands
 */

/* structure for MTIOCTOP - mag tape op command */
struct	bsd43_(mtop)	{
	short	mt_op;		/* operations defined below */
	daddr_t	mt_count;	/* how many of them */
};

/* operations */
#define BSD43_MTWEOF	0	/* write an end-of-file record */
#define BSD43_MTFSF	1	/* forward space file */
#define BSD43_MTBSF	2	/* backward space file */
#define BSD43_MTFSR	3	/* forward space record */
#define BSD43_MTBSR	4	/* backward space record */
#define BSD43_MTREW	5	/* rewind */
#define BSD43_MTOFFL	6	/* rewind and put the drive offline */
#define BSD43_MTNOP	7	/* no operation, sets status only */
#define BSD43_MTCACHE	8	/* enable controller cache */
#define BSD43_MTNOCACHE 9	/* disable controller cache */
#define BSD43_MTRET	10	/* retention operation */
#define BSD43_MTRST	11	/* reset operation */
#define BSD43_MTERASE	12	/* secuity erase (9-track only) */
#define BSD43_MTRETEN	13	/* retension the tape */

/* structure for MTIOCGET - mag tape get status command */

struct	bsd43_(mtget)	{
	short	mt_type;	/* type of magtape device */
/* the following two registers are grossly device dependent */
	short	mt_dsreg;	/* ``drive status'' register */
	short	mt_erreg;	/* ``error'' register */
/* end device-dependent registers */
	short	mt_resid;	/* residual count */
/* the following two are not yet implemented */
	daddr_t	mt_fileno;	/* file number of current position */
	daddr_t	mt_blkno;	/* block number of current position */
/* end not yet implemented */
};

/*
 * Constants for mt_type byte.  These are the same
 * for controllers compatible with the types listed.
 */
#define	BSD43_MT_ISTS		0x01		/* TS-11 */
#define	BSD43_MT_ISHT		0x02		/* TM03 Massbus: TE16, TU45, TU77 */
#define	BSD43_MT_ISTM		0x03		/* TM11/TE10 Unibus */
#define	BSD43_MT_ISMT		0x04		/* TM78/TU78 Massbus */
#define	BSD43_MT_ISUT		0x05		/* SI TU-45 emulation on Unibus */
#define	BSD43_MT_ISCPC	0x06		/* SUN */
#define	BSD43_MT_ISAR		0x07		/* SUN */
#define	BSD43_MT_ISTMSCP	0x08		/* DEC TMSCP protocol (TU81, TK50) */
#define BSD43_MT_ISQIC	0x09		/* ISI ts11 qic-2 tape controller */
#define	BSD43_MT_ISSC		0x0a		/* SUN */
#define	BSD43_MT_ISXY		0x0b		/* SUN */

/* mag tape io control commands */
#define	BSD43_MTIOCTOP	BSD43__IOW(m, 1, struct bsd43_(mtop))		/* do a mag tape op */
#define	BSD43_MTIOCGET	BSD43__IOR(m, 2, struct bsd43_(mtget))	/* get tape status */
#define BSD43_MTIOCIEOT	BSD43__IO(m, 3)			/* ignore EOT error */
#define BSD43_MTIOCEEOT	BSD43__IO(m, 4)			/* enable EOT error */

#ifndef KERNEL
#define	BSD43_DEFTAPE	"/dev/rmt12"
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DEFTAPE BSD43_DEFTAPE
#   define MTBSF BSD43_MTBSF
#   define MTBSR BSD43_MTBSR
#   define MTCACHE BSD43_MTCACHE
#   define MTERASE BSD43_MTERASE
#   define MTFSF BSD43_MTFSF
#   define MTFSR BSD43_MTFSR
#   define MTIOCEEOT BSD43_MTIOCEEOT
#   define MTIOCGET BSD43_MTIOCGET
#   define MTIOCIEOT BSD43_MTIOCIEOT
#   define MTIOCTOP BSD43_MTIOCTOP
#   define MTNOCACHE BSD43_MTNOCACHE
#   define MTNOP BSD43_MTNOP
#   define MTOFFL BSD43_MTOFFL
#   define MTRET BSD43_MTRET
#   define MTRETEN BSD43_MTRETEN
#   define MTREW BSD43_MTREW
#   define MTRST BSD43_MTRST
#   define MTWEOF BSD43_MTWEOF
#   define MT_ISAR BSD43_MT_ISAR
#   define MT_ISCPC BSD43_MT_ISCPC
#   define MT_ISHT BSD43_MT_ISHT
#   define MT_ISMT BSD43_MT_ISMT
#   define MT_ISQIC BSD43_MT_ISQIC
#   define MT_ISSC BSD43_MT_ISSC
#   define MT_ISTM BSD43_MT_ISTM
#   define MT_ISTMSCP BSD43_MT_ISTMSCP
#   define MT_ISTS BSD43_MT_ISTS
#   define MT_ISUT BSD43_MT_ISUT
#   define MT_ISXY BSD43_MT_ISXY
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


