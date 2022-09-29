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
/* $Header: syslog.h,v 1.7.3.2 90/05/10 04:55:55 wje Exp $ */
/*
 * Copyright (c) 1982, 1986, 1988 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 *
 *	@(#)syslog.h	7.10 (Berkeley) 6/27/88
 */

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 *  Facility codes
 */

#define BSD43_LOG_KERN	(0<<3)	/* kernel messages */
#define BSD43_LOG_USER	(1<<3)	/* random user-level messages */
#define BSD43_LOG_MAIL	(2<<3)	/* mail system */
#define BSD43_LOG_DAEMON	(3<<3)	/* system daemons */
#define BSD43_LOG_AUTH	(4<<3)	/* security/authorization messages */
#define BSD43_LOG_SYSLOG	(5<<3)	/* messages generated internally by syslogd */
#define BSD43_LOG_LPR	(6<<3)	/* line printer subsystem */
#define BSD43_LOG_NEWS	(7<<3)	/* network news subsystem */
#define BSD43_LOG_UUCP	(8<<3)	/* UUCP subsystem */
	/* other codes through 15 reserved for system use */
#define BSD43_LOG_LOCAL0	(16<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL1	(17<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL2	(18<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL3	(19<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL4	(20<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL5	(21<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL6	(22<<3)	/* reserved for local use */
#define BSD43_LOG_LOCAL7	(23<<3)	/* reserved for local use */

#define BSD43_LOG_NFACILITIES	24	/* maximum number of facilities */
#define BSD43_LOG_FACMASK	0x03f8	/* mask to extract facility part */

#define BSD43_LOG_FAC(p)	(((p) & BSD43_LOG_FACMASK) >> 3)	/* facility of pri */

/*
 *  Priorities (these are ordered)
 */

#define BSD43_LOG_EMERG	0	/* system is unusable */
#define BSD43_LOG_ALERT	1	/* action must be taken immediately */
#define BSD43_LOG_CRIT	2	/* critical conditions */
#define BSD43_LOG_ERR	3	/* error conditions */
#define BSD43_LOG_WARNING	4	/* warning conditions */
#define BSD43_LOG_NOTICE	5	/* normal but signification condition */
#define BSD43_LOG_INFO	6	/* informational */
#define BSD43_LOG_DEBUG	7	/* debug-level messages */

#define BSD43_LOG_PRIMASK	0x0007	/* mask to extract priority part (internal) */
#define BSD43_LOG_PRI(p)	((p) & BSD43_LOG_PRIMASK)	/* extract priority */

#define	BSD43_LOG_MAKEPRI(fac, pri)	(((fac) << 3) | (pri))

#ifdef KERNEL
#define BSD43_LOG_PRINTF	-1	/* pseudo-priority to indicate use of printf */
#endif

/*
 * arguments to setlogmask.
 */
#define	BSD43_LOG_MASK(pri)  (1 << (pri))		/* mask for one priority */
#define	BSD43_LOG_UPTO(pri)  ((1 << ((pri)+1)) - 1)	/* all priorities through pri */

/*
 *  Option flags for openlog.
 *
 *	LOG_ODELAY no longer does anything; LOG_NDELAY is the
 *	inverse of what it used to be.
 */
#define	BSD43_LOG_PID	0x01	/* log the pid with each message */
#define	BSD43_LOG_CONS	0x02	/* log on the console if errors in sending */
#define	BSD43_LOG_ODELAY	0x04	/* delay open until first syslog() (default */
#define BSD43_LOG_NDELAY	0x08	/* don't delay open */
#define BSD43_LOG_NOWAIT	0x10	/* if forking to log on console, don't wait() */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define LOG_ALERT BSD43_LOG_ALERT
#   define LOG_AUTH BSD43_LOG_AUTH
#   define LOG_CONS BSD43_LOG_CONS
#   define LOG_CRIT BSD43_LOG_CRIT
#   define LOG_DAEMON BSD43_LOG_DAEMON
#   define LOG_DEBUG BSD43_LOG_DEBUG
#   define LOG_EMERG BSD43_LOG_EMERG
#   define LOG_ERR BSD43_LOG_ERR
#   define LOG_FAC BSD43_LOG_FAC
#   define LOG_FACMASK BSD43_LOG_FACMASK
#   define LOG_INFO BSD43_LOG_INFO
#   define LOG_KERN BSD43_LOG_KERN
#   define LOG_LOCAL0 BSD43_LOG_LOCAL0
#   define LOG_LOCAL1 BSD43_LOG_LOCAL1
#   define LOG_LOCAL2 BSD43_LOG_LOCAL2
#   define LOG_LOCAL3 BSD43_LOG_LOCAL3
#   define LOG_LOCAL4 BSD43_LOG_LOCAL4
#   define LOG_LOCAL5 BSD43_LOG_LOCAL5
#   define LOG_LOCAL6 BSD43_LOG_LOCAL6
#   define LOG_LOCAL7 BSD43_LOG_LOCAL7
#   define LOG_LPR BSD43_LOG_LPR
#   define LOG_MAIL BSD43_LOG_MAIL
#   define LOG_MAKEPRI BSD43_LOG_MAKEPRI
#   define LOG_MASK BSD43_LOG_MASK
#   define LOG_NDELAY BSD43_LOG_NDELAY
#   define LOG_NEWS BSD43_LOG_NEWS
#   define LOG_NFACILITIES BSD43_LOG_NFACILITIES
#   define LOG_NOTICE BSD43_LOG_NOTICE
#   define LOG_NOWAIT BSD43_LOG_NOWAIT
#   define LOG_ODELAY BSD43_LOG_ODELAY
#   define LOG_PID BSD43_LOG_PID
#   define LOG_PRI BSD43_LOG_PRI
#   define LOG_PRIMASK BSD43_LOG_PRIMASK
#   define LOG_PRINTF BSD43_LOG_PRINTF
#   define LOG_SYSLOG BSD43_LOG_SYSLOG
#   define LOG_UPTO BSD43_LOG_UPTO
#   define LOG_USER BSD43_LOG_USER
#   define LOG_UUCP BSD43_LOG_UUCP
#   define LOG_WARNING BSD43_LOG_WARNING
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


