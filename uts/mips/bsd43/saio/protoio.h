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
/* $Header: protoio.h,v 1.6.3.2 90/05/10 04:47:10 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * protoio.h (prom version) -- protocol io interface file
 * used to adapt protocol code to prom or unix environment
 */
typedef	int	bsd43_(pdev_t);

#define	BSD43_PUTC(c, fd)	bsd43_putc(c, fd)
#define	BSD43_GETC(fd)	bsd43_getc(fd)
#define	BSD43_PUTFLUSH(fd)
#define	BSD43_PINIT(fd)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define GETC BSD43_GETC
#   define PINIT BSD43_PINIT
#   define PUTC BSD43_PUTC
#   define PUTFLUSH BSD43_PUTFLUSH
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


