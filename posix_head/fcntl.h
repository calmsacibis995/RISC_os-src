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
/* $Header: fcntl.h,v 1.3.1.4 90/05/10 04:07:38 wje Exp $ */

#ifndef	_POSIX_FCNTL_
#define	_POSIX_FCNTL_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include <sys/types.h>

/* Flag values accessible to open(2) and fcntl(2) */
/*  (The first three can only be set by open) */
#define	O_RDONLY	 0
#define	O_WRONLY 	01
#define	O_RDWR	 	02
#define	O_NONBLOCK   	04	/* Non-blocking I/O */
#define	O_APPEND    	010	/* append (writes guaranteed at the end) */
#define O_SYNC	 	020	/* synchronous write option */

#define O_ACCMODE  	0x3	/* mask for access modes */

/* Flag values accessible only to open(2) */
#define O_NOCTTY   	040	/* don't acquire controlling terminal */
#define	O_CREAT		00400	/* open with file create (uses third open arg)*/
#define	O_TRUNC		01000	/* open with truncation */
#define	O_EXCL		02000	/* exclusive open */

/* fcntl(2) requests */
#define	F_DUPFD		0	/* Duplicate fildes */
#define	F_GETFD		1	/* Get fildes flags */
#define	F_SETFD		2	/* Set fildes flags */
#define	F_GETFL		3	/* Get file flags */
#define	F_SETFL		4	/* Set file flags */
#define	F_GETLK		5	/* Get file lock */
#define	F_SETLK		6	/* Set file lock */
#define	F_SETLKW	7	/* Set file lock and wait */

/* file segment locking set data type - information passed to system by user */
/* by removing the l_sysid (which wasn't used anyway) this structure remains */
/* the same size as the sysv flock structure */
struct flock {
	short	l_type;
	short	l_whence;
	off_t	l_start;
	off_t	l_len;		/* len = 0 means until end of file */
        pid_t   l_pid;
};

/* file segment locking types */
	/* Read lock */
#define	F_RDLCK	01
	/* Write lock */
#define	F_WRLCK	02
	/* Remove lock(s) */
#define	F_UNLCK	03

/* flags for F_SETFL/F_GETFL */
#define FD_CLOEXEC 1		/* close on exec */

extern int open(), creat(), fcntl();

#endif	_POSIX_FCNTL_
