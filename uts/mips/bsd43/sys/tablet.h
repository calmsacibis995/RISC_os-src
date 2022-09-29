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
/* $Header: tablet.h,v 1.7.3.2 90/05/10 04:56:45 wje Exp $ */
/*
 * Copyright (c) 1985, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tablet.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifndef _TABLET_
/*
 * Tablet line discipline.
 */
#ifdef KERNEL
#include "../h/ioctl.h"
#else
#include <bsd43/sys/ioctl.h>
#endif

/*
 * Reads on the tablet return one of the following
 * structures, depending on the underlying tablet type.
 * The first two are defined such that a read of
 * sizeof (gtcopos) on a non-gtco tablet will return
 * meaningful info.  The in-proximity bit is simulated
 * where the tablet does not directly provide the information.
 */
struct	bsd43_(tbpos) {
	int	xpos, ypos;	/* raw x-y coordinates */
	short	bsd43_(status);		/* buttons/pen down */
#define	BSD43_TBINPROX	0100000		/* pen in proximity of tablet */
	short	scount;		/* sample count */
};

struct	bsd43_(gtcopos) {
	int	xpos, ypos;	/* raw x-y coordinates */
	short	bsd43_(status);		/* as above */
	short	scount;		/* sample count */
	short	xtilt, ytilt;	/* raw tilt */
	short	pressure;
	short	pad;		/* pad to longword boundary */
};

struct	bsd43_(polpos) {
	short	p_x, p_y, p_z;	/* raw 3-space coordinates */
	short	p_azi, p_pit, p_rol;	/* azimuth, pitch, and roll */
	short	p_stat;		/* status, as above */
	char	p_key;		/* calculator input keyboard */
};

#define BSD43_BIOSMODE	BSD43__IOW(b, 1, int)		/* set mode bit(s) */
#define BSD43_BIOGMODE	BSD43__IOR(b, 2, int)		/* get mode bit(s) */
#define	BSD43_TBMODE		0xfff0		/* mode bits: */
#define		BSD43_TBPOINT		0x0010		/* single point */
#define		BSD43_TBRUN		0x0000		/* runs contin. */
#define		BSD43_TBSTOP		0x0020		/* shut-up */
#define		BSD43_TBGO		0x0000		/* ~TBSTOP */
#define	BSD43_TBTYPE		0x000f		/* tablet type: */
#define		BSD43_TBUNUSED	0x0000
#define		BSD43_TBHITACHI	0x0001		/* hitachi tablet */
#define		BSD43_TBTIGER		0x0002		/* hitachi tiger */
#define		BSD43_TBGTCO		0x0003		/* gtco */
#define		BSD43_TBPOL		0x0004		/* polhemus 3space */
#define		BSD43_TBHDG		0x0005		/* hdg-1111b, low res */
#define		BSD43_TBHDGHIRES	0x0006		/* hdg-1111b, high res */
#define BSD43_BIOSTYPE	BSD43__IOW(b, 3, int)		/* set tablet type */
#define BSD43_BIOGTYPE	BSD43__IOR(b, 4, int)		/* get tablet type*/
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BIOGMODE BSD43_BIOGMODE
#   define BIOGTYPE BSD43_BIOGTYPE
#   define BIOSMODE BSD43_BIOSMODE
#   define BIOSTYPE BSD43_BIOSTYPE
#   define TBGO BSD43_TBGO
#   define TBGTCO BSD43_TBGTCO
#   define TBHDG BSD43_TBHDG
#   define TBHDGHIRES BSD43_TBHDGHIRES
#   define TBHITACHI BSD43_TBHITACHI
#   define TBINPROX BSD43_TBINPROX
#   define TBMODE BSD43_TBMODE
#   define TBPOINT BSD43_TBPOINT
#   define TBPOL BSD43_TBPOL
#   define TBRUN BSD43_TBRUN
#   define TBSTOP BSD43_TBSTOP
#   define TBTIGER BSD43_TBTIGER
#   define TBTYPE BSD43_TBTYPE
#   define TBUNUSED BSD43_TBUNUSED
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


