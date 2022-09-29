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
/* $Header: tcp-param.h,v 1.10.1.2 90/05/10 04:38:49 wje Exp $ */

#ifndef	_BSD_TCP-PARAM_
#define	_BSD_TCP-PARAM_	1

#define INET				/* generate internet code, of course */
#define NETHER 1			/* surely have an ethernet board */
#define NHY 0				/* no hyper-channel */
#define NIMP 0				/* no IMP interface */

#define	ovbcopy(f, t, n)	bcopy((f), (t), (n))
#define	imin(a,b)		MIN((a),(b))

#define insque(q,p)	_insque((caddr_t)(q), (caddr_t)(p))
#define remque(q)	_remque((caddr_t)(q))

#endif	_BSD_TCP-PARAM_
