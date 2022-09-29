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
/* $Header: sh.local.h,v 1.2.1.2 90/05/07 18:16:44 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)sh.local.h	5.3 (Berkeley) 10/13/86
 */

/*
 * This file defines certain local parameters
 * A symbol should be defined in Makefile for local conditional
 * compilation, e.g. IIASA or ERNIE, to be tested here and elsewhere.
 */

/*
 * Fundamental definitions which may vary from system to system.
 *
 *	BUFSIZ		The i/o buffering size; also limits word size
 *	SHELLPATH	Where the shell will live; initalizes $shell
 *	MAILINTVL	How often to mailcheck; more often is more expensive
 *	OTHERSH		Shell for scripts which don't start with #
 */

#define	BUFSIZ	1024		/* default buffer size */
#define	SHELLPATH	"/bin/csh"
#define	OTHERSH		"/bin/sh"
#define FORKSLEEP	10	/* delay loop on non-interactive fork failure */
#define	MAILINTVL	600	/* 10 minutes */

/*
 * The shell moves std in/out/diag and the old std input away from units
 * 0, 1, and 2 so that it is easy to set up these standards for invoked
 * commands.
 */
#define	FSHTTY	(NOFILE-5)	/* /dev/tty when manip pgrps */
#define	FSHIN	(NOFILE-4)	/* Preferred desc for shell input */
#define	FSHOUT	(NOFILE-3)	/* ... shell output */
#define	FSHDIAG	(NOFILE-2)	/* ... shell diagnostics */
#define	FOLDSTD	(NOFILE-1)	/* ... old std input */

#ifdef IIASA
#undef	OTHERSH
#endif

#if defined(vax) || defined(tahoe)
#define	copy(to, from, size)	bcopy(from, to, size)
#endif

#ifdef PROF
#define	exit(n)	done(n)
#endif
