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
/* $Header: netisr.h,v 1.11.3.2 90/05/10 04:25:12 wje Exp $ */

#ifndef	_BSD_NET_NETISR_
#define	_BSD_NET_NETISR_	1


/*
 * Copyright (c) 1980, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)netisr.h	7.1 (Berkeley) 6/4/86
 */

/*
 * The networking code runs off software interrupts.
 *
 * You can switch into the network by doing splnet() and return by splx().
 * The software interrupt level for the network is higher than the software
 * level for the clock (so you can enter the network in routines called
 * at timeout time).
 */
#ifdef vax
#define	setsoftnet()	mtpr(SIRR, 12)
#endif

/*
 * Each ``pup-level-1'' input queue has a bit in a ``netisr'' status
 * word which is used to de-multiplex a single software
 * interrupt used for scheduling the network code to calls
 * on the lowest level routine of each protocol.
 */
#define	NETISR_RAW	0		/* same as AF_UNSPEC */
#define	NETISR_IP	2		/* same as AF_INET */
#define	NETISR_IMP	3		/* same as AF_IMPLINK */
#define	NETISR_NS	6		/* same as AF_NS */

#ifdef mips	/* was sgi */
#define SCHEDBIT(anisr)		(1<<(anisr))
#define	schednetisr(anisr)	{ register int soft_s = splall(); \
				 netisr |= SCHEDBIT(anisr); \
				 splx(soft_s); \
				 setsoftnet(); \
				 }
#else
#define	schednetisr(anisr)	{ netisr |= 1<<(anisr); setsoftnet(); }
#endif

#ifndef LOCORE
#ifdef KERNEL
int	netisr;				/* scheduling bits for network */
#endif
#endif

#endif	_BSD_NET_NETISR_
