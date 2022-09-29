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
/* $Header: dk.h,v 1.4.3.2 90/05/10 01:04:56 wje Exp $ */

#ifndef	_SUN_SYS_DK_
#define	_SUN_SYS_DK_	1


/*	@(#)dk.h 1.1 85/05/30 SMI; from UCB 4.2 81/02/19	*/

/*
 * Instrumentation
 */
#define	CPUSTATES	4

#define	CP_USER		0
#define	CP_NICE		1
#define	CP_SYS		2
#define	CP_IDLE		3

#define	DK_NDRIVE	4

#ifdef KERNEL
long	cp_time[CPUSTATES];
int	dk_busy;
long	dk_time[DK_NDRIVE];
long	dk_seek[DK_NDRIVE];
long	dk_xfer[DK_NDRIVE];
long	dk_wds[DK_NDRIVE];
long	dk_bps[DK_NDRIVE];

long	tk_nin;
long	tk_nout;
#endif

#endif	_SUN_SYS_DK_
