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
/* $Header: ttydev.h,v 1.7.3.2 90/05/10 04:57:35 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ttydev.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Terminal definitions related to underlying hardware.
 */
#ifndef BSD43__TTYDEV_
#define	BSD43__TTYDEV_

/*
 * Speeds
 */
#define BSD43_B0	0
#define BSD43_B50	1
#define BSD43_B75	2
#define BSD43_B110	3
#define BSD43_B134	4
#define BSD43_B150	5
#define BSD43_B200	6
#define BSD43_B300	7
#define BSD43_B600	8
#define BSD43_B1200	9
#define	BSD43_B1800	10
#define BSD43_B2400	11
#define BSD43_B4800	12
#define BSD43_B9600	13
#define BSD43_EXTA	14
#define BSD43_EXTB	15

#ifdef KERNEL
/*
 * Hardware bits.
 * SHOULD NOT BE HERE.
 */
#define	BSD43_DONE	0200
#define	BSD43_IENABLE	0100

/*
 * Modem control commands.
 */
#define	BSD43_DMSET	0
#define	BSD43_DMBIS	1
#define	BSD43_DMBIC	2
#define	BSD43_DMGET	3

#endif KERNEL
#endif BSD43__TTYDEV_

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define B0 BSD43_B0
#   define B110 BSD43_B110
#   define B1200 BSD43_B1200
#   define B134 BSD43_B134
#   define B150 BSD43_B150
#   define B1800 BSD43_B1800
#   define B200 BSD43_B200
#   define B2400 BSD43_B2400
#   define B300 BSD43_B300
#   define B4800 BSD43_B4800
#   define B50 BSD43_B50
#   define B600 BSD43_B600
#   define B75 BSD43_B75
#   define B9600 BSD43_B9600
#   define EXTA BSD43_EXTA
#   define EXTB BSD43_EXTB
#   define _TTYDEV_ BSD43__TTYDEV_
#   ifdef KERNEL
#	define DMBIC BSD43_DMBIC
#	define DMBIS BSD43_DMBIS
#	define DMGET BSD43_DMGET
#	define DMSET BSD43_DMSET
#	define DONE BSD43_DONE
#	define IENABLE BSD43_IENABLE
#   endif KERNEL
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


