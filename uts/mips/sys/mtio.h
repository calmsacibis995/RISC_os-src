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
/* $Header: mtio.h,v 1.12.3.2 90/05/10 06:29:42 wje Exp $ */

#ifndef	_SYS_MTIO_
#define	_SYS_MTIO_	1


/*	mtio.h	6.1	83/07/29	*/

/*
 * Structures and definitions for mag tape io control commands
 */

/* structure for MTIOCTOP - mag tape op command */
struct	mtop	{
	short	mt_op;		/* operations defined below */
	daddr_t	mt_count;	/* how many of them */
};

/* operations */
#define MTWEOF	0	/* write an end-of-file record */
#define MTFSF	1	/* forward space file */
#define MTBSF	2	/* backward space file */
#define MTFSR	3	/* forward space record */
#define MTBSR	4	/* backward space record */
#define MTREW	5	/* rewind */
#define MTOFFL	6	/* rewind and put the drive offline */
#define MTNOP	7	/* no operation, sets status only */
#define MTRET	8	/* retention operation */
#define MTRST	9	/* reset operation */
#define MTBLKSIZE 10	/* get block size */
#define MTONL	11	/* put drive online */
#define MTAPP	12	/* space to end of data */
#ifdef QIC100
#define MTERASE  13	/* format qic100 tape */
/*#define GTBUFFER	30	/* enable/disable buffering */
/*#define GTPARITY	31	/* enable/disable parity checking */
/*#define GTAUTOLD	32	/* enable/disable tape autoload */
#define GTPREVENT	34	/* prevent/allow tape removal */
				/* count = 1 -> prevent, 0 -> allow */
#define GTLOAD		35	/* load/unload tape */
				/* count = 1 -> load, 0 -> unload */
#define GTSETBKSZ	36	/* set tape blocksize */
				/* count -> blocksize, count = 0 -> variable */
#endif QIC100

/* structure for MTIOCGET - mag tape get status command */

struct	mtget	{
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
 * Constants for mt_type byte
 */
#ifdef mips
#define MT_ISQIC	0x01		/* qic-2 tape controller */
#define MT_ISXM		0x08		/* Xylogics magtape */
#define MT_M120		0x09		/* M120 magtape */
#define MT_JAG		0x0a		/* Interphase magtape */
#ifdef QIC100
#define	MT_ISQ100	0x0b		/* QIC-100 random access tape */
#define	MT_ISGT		0x0c		/* standard SCSI tape drives */
#endif QIC100
#else
#define	MT_ISTS		0x01
#define	MT_ISHT		0x02
#define	MT_ISTM		0x03
#define	MT_ISMT		0x04
#define	MT_ISUT		0x05
#define	MT_ISCPC	0x06
#define	MT_ISAR		0x07
#endif

/* mag tape io control commands */
#define MTIOC		('t'<<8)
#define	MTIOCTOP	(MTIOC|1)		/* do a mag tape op */
#define	MTIOCGET	(MTIOC|2)		/* get tape status */

#ifndef KERNEL
#define	DEFTAPE	"/dev/rmt/m0"
#define	DEFNRTAPE	"/dev/rmt/m4"
#endif

#endif	_SYS_MTIO_
