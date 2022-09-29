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
/* $Header: wait.h,v 1.6.3.2 90/05/10 05:01:39 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)wait.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * This file holds definitions relevent to the wait system call.
 * Some of the options here are available only through the ``wait3''
 * entry point; the old entry point with one argument has more fixed
 * semantics, never returning status of unstopped children, hanging until
 * a process terminates if any are outstanding, and never returns
 * detailed information about process resource utilization (<vtimes.h>).
 */

/*
 * Structure of the information in the first word returned by both
 * wait and wait3.  If w_stopval==WSTOPPED, then the second structure
 * describes the information returned, else the first.  See WUNTRACED below.
 */

#ifdef mips
/*
 * The structures returned by wait() are defined by bit field names
 * in 4.2BSD, although not used consistently. In system V, the definition
 * is by byte and bit positions (gak!). We try to satisfy both by
 * conditionaly compiling the 4.2 bit fields to line up with the
 * system V position scheme.
 */
#endif
union bsd43_(wait)	{
	unsigned int	w_status;		/* used in syscall */
	/*
	 * Terminated process status.
	 */
	struct {
#ifdef vax
		unsigned short	w_Termsig:7;	/* termination signal */
		unsigned short	w_Coredump:1;	/* core dump indicator */
		unsigned short	w_Retcode:8;	/* exit code if w_termsig==0 */
#endif
#ifdef MIPSEL
		unsigned int	w_Termsig:7;	/* termination signal */
		unsigned int	w_Coredump:1;	/* core dump indicator */
		unsigned int	w_Retcode:8;	/* exit code if w_termsig==0 */
		unsigned int	w_Filler:16;	/* pad to word boundary */
#endif
#ifdef MIPSEB
		unsigned int	w_Filler:16;	/* pad to word boundary */
		unsigned int	w_Retcode:8;	/* exit code if w_termsig==0 */
		unsigned int	w_Coredump:1;	/* core dump indicator */
		unsigned int	w_Termsig:7;	/* termination signal */
#endif
	} w_T;
	/*
	 * Stopped process status.  Returned
	 * only for traced children unless requested
	 * with the WUNTRACED option bit.
	 */
	struct {
#ifdef vax
		unsigned short	w_Stopval:8;	/* == W_STOPPED if stopped */
		unsigned short	w_Stopsig:8;	/* signal that stopped us */
#endif
#ifdef MIPSEL
		unsigned int	w_Stopval:8;	/* == W_STOPPED if stopped */
		unsigned int	w_Stopsig:8;	/* signal that stopped us */
		unsigned int	w_Filler:16;	/* pad to word boundary */
#endif
#ifdef MIPSEB
		unsigned int	w_Filler:16;	/* pad to word boundary */
		unsigned int	w_Stopsig:8;	/* signal that stopped us */
		unsigned int	w_Stopval:8;	/* == W_STOPPED if stopped */
#endif
	} w_S;
};
#define	bsd43_w_termsig	w_T.w_Termsig
#define bsd43_w_coredump	w_T.w_Coredump
#define bsd43_w_retcode	w_T.w_Retcode
#define bsd43_w_stopval	w_S.w_Stopval
#define bsd43_w_stopsig	w_S.w_Stopsig


#define	BSD43_WSTOPPED	0177	/* value of s.stopval if process is stopped */

/*
 * Option bits for the second argument of wait3.  WNOHANG causes the
 * wait to not hang if there are no stopped or terminated processes, rather
 * returning an error indication in this case (pid==0).  WUNTRACED
 * indicates that the caller should receive status about untraced children
 * which stop due to signals.  If children are stopped and a wait without
 * this option is done, it is as though they were still running... nothing
 * about them is returned.
 */
#define BSD43_WNOHANG		1	/* dont hang in wait */
#define BSD43_WUNTRACED	2	/* tell about stopped, untraced children */

#define BSD43_WIFSTOPPED(x)	((x).bsd43_w_stopval == BSD43_WSTOPPED)
#define BSD43_WIFSIGNALED(x)	((x).bsd43_w_stopval != BSD43_WSTOPPED && (x).bsd43_w_termsig != 0)
#define BSD43_WIFEXITED(x)	((x).bsd43_w_stopval != BSD43_WSTOPPED && (x).bsd43_w_termsig == 0)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define WIFEXITED BSD43_WIFEXITED
#   define WIFSIGNALED BSD43_WIFSIGNALED
#   define WIFSTOPPED BSD43_WIFSTOPPED
#   define WNOHANG BSD43_WNOHANG
#   define WSTOPPED BSD43_WSTOPPED
#   define WUNTRACED BSD43_WUNTRACED
#   define w_coredump bsd43_w_coredump
#   define w_retcode bsd43_w_retcode
#   define w_stopsig bsd43_w_stopsig
#   define w_stopval bsd43_w_stopval
#   define w_termsig bsd43_w_termsig
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


