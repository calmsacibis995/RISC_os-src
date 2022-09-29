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
/* $Header: numips.h,v 1.2.1.2 90/05/10 06:30:52 wje Exp $ */

#ifndef	_SYS_NUMIPS_
#define	_SYS_NUMIPS_	1

#ifdef NUMIPS
/* 
 * Under NUMIPS a variety of systypes are supported in the same kernel.
 * The following truth macros are define to allow common code to be
 * customized to a particular systype.
 *
 * Currently, systype is determined entirely at system call time.
 * The *_SYSCALL macros identify which the systype to which the current 
 * syscall belongs.  For some problems it may be necessary to give processes
 * a lasting association with a particular system type.  (e.g. Signals?)
 * More macros may be added as required.
 */
#define SYSV_SYSTYPE 1
#define BSD_SYSTYPE 2
#define POSIX_SYSTYPE 3

#define SYSV_SYSCALL	(u.u_syscall_type == SYSV_SYSTYPE)
#define BSD_SYSCALL	(u.u_syscall_type == BSD_SYSTYPE)
#define POSIX_SYSCALL	(u.u_syscall_type == POSIX_SYSTYPE)

#else

#   define BSD_SYSCALL	(0)
#   define SYSV_SYSCALL	(1)
#   define POSIX_SYSCALL	(0)

#endif

#endif	_SYS_NUMIPS_
