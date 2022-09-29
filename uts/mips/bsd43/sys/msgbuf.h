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
/* $Header: msgbuf.h,v 1.6.3.2 90/05/10 04:53:15 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)msgbuf.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define	BSD43_MSG_MAGIC	0x063061
#define	BSD43_MSG_BSIZE	(4096 - 3 * sizeof (long))
struct	bsd43_(msgbuf) {
	long	msg_magic;
	long	msg_bufx;
	long	msg_bufr;
	char	msg_bufc[BSD43_MSG_BSIZE];
};
#ifdef KERNEL
#ifdef mips
/*
 * The message buffer will be at the end of physical address
 * space instead of an absolute virtual address. This pointer
 * will be initialized during the memory sizing phase of the boot sequence.
 */
struct	bsd43_(msgbuf) *bsd43_(pmsgbuf);
#else
struct	bsd43_(msgbuf) bsd43_(msgbuf);
#endif mips
#endif KERNEL

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define MSG_BSIZE BSD43_MSG_BSIZE
#   define MSG_MAGIC BSD43_MSG_MAGIC
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


