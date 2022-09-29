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
/* $Header: disktab.h,v 1.3.2.2 90/05/07 20:07:34 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Disk description table, see disktab(5)
 */

#ifdef mips
#include <bsd43/sys/types.h>
#include <bsd43/mips/dvh.h>
#endif

#ifndef NPARTAB
#define BSD43_NUPART 	8
#else
/*
 * Number of user partitions is the total number of partitions minus
 * the volume header, sector forwarding, and entire volume partitions.
 */
#define BSD43_NUPART	(NPARTAB - 3)
#endif

#define	BSD43_DISKTAB		"/etc/disktab"

struct	bsd43_(disktab) {
	char	*d_name;		/* drive name */
	char	*d_type;		/* drive type */
	int	d_secsize;		/* sector size in bytes */
	int	d_ntracks;		/* # tracks/cylinder */
	int	d_nsectors;		/* # sectors/track */
	int	d_ncylinders;		/* # cylinders */
	int	d_rpm;			/* revolutions/minute */
	int	d_badsectforw;		/* supports DEC bad144 std */
	int	d_sectoffset;		/* use sect rather than cyl offsets */
	struct	bsd43_(partition) {
		int	p_size;		/* #sectors in partition */
		short	p_bsize;	/* block size in bytes */
		short	p_fsize;	/* frag size in bytes */
	} d_partitions[8];
};

struct	bsd43_(disktab) *bsd43_(getdiskbyname)();

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define DISKTAB BSD43_DISKTAB
#   define NUPART BSD43_NUPART
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


