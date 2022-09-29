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
/* $Header: fcntl.h,v 1.7.1.3 90/05/10 04:51:30 wje Exp $ */
/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */
/*----- COPYRIGHT (END) -----------------------------------------------------*/

#ifndef _BSD43_SYS_FCNTL_
#define _BSD43_SYS_FCNTL_ 1

/*
 * Flag values accessible to open(2) and fcntl(2)
 *  (The first three can only be set by open)
 */
#define	BSD43_O_RDONLY	0
#define	BSD43_O_WRONLY	1
#define	BSD43_O_RDWR	2
#define	BSD43_O_NDELAY	BSD43_FNDELAY	/* Non-blocking I/O */
#define	BSD43_O_APPEND	BSD43_FAPPEND /* append(writes guaranteed at the end)*/
#define	BSD43_O_CREAT	BSD43_FCREAT	/* open with file create */
#define	BSD43_O_TRUNC	BSD43_FTRUNC	/* open with truncation */
#define	BSD43_O_EXCL	BSD43_FEXCL	/* error on create if file exists */
#define	BSD43_O_SYNC	BSD43_FSYNC	/* Synchronous writes */

/* flags for BSD43_F_GETFL, BSD43_F_SETFL-- needed by <sys/file.h> */
#define	BSD43_FNDELAY	00004		/* non-blocking reads */
#define	BSD43_FAPPEND	00010		/* append on each write */
#define	BSD43_FASYNC	00100		/* signal pgrp when data ready */
#define	BSD43_FCREAT	01000		/* create if nonexistant */
#define	BSD43_FTRUNC	02000		/* truncate to zero length */
#define	BSD43_FEXCL	04000		/* error if already created */
#define	BSD43_FSYNC	020000		/* Synchronous writes */

/* fcntl(2) requests */
#define	BSD43_F_DUPFD	0	/* Duplicate fildes */
#define	BSD43_F_GETFD	1	/* Get fildes flags */
#define	BSD43_F_SETFD	2	/* Set fildes flags */
#define	BSD43_F_GETFL	3	/* Get file flags */
#define	BSD43_F_SETFL	4	/* Set file flags */
#define BSD43_F_GETLK	5	/* Get record-locking information */
#define BSD43_F_SETLK	6	/* Set or Clear a record-lock (Non-Blocking) */
#define BSD43_F_SETLKW	7	/* Set or Clear a record-lock (Blocking) */
#define BSD43_F_CHKFL	8	/* Check legality of file flag changes */

/* only for sockets */
/*			9	???? */
#define	BSD43_F_GETOWN	10	/* Get owner */
#define BSD43_F_SETOWN	11	/* Set owner */


/* access(2) requests */
#define	BSD43_F_OK		0	/* does file exist */
#define	BSD43_X_OK		1	/* is it executable by caller */
#define	BSD43_W_OK		2	/* writable by caller */
#define	BSD43_R_OK		4	/* readable by caller */

/* System-V record-locking options */
/* lockf(2) requests */
#define BSD43_F_ULOCK 0       /* Unlock a previously locked region */
#define BSD43_F_LOCK  1       /* Lock a region for exclusive use */ 
#define BSD43_F_TLOCK 2       /* Test and lock a region for exclusive use */
#define BSD43_F_TEST  3       /* Test a region for other processes locks */

/* fcntl(2) flags (l_type field of flock structure) */
#define BSD43_F_RDLCK 1       /* read lock */
#define BSD43_F_WRLCK 2       /* write lock */
#define BSD43_F_UNLCK 3       /* remove lock(s) */


/* file segment locking set data type - information passed to system by user */
struct bsd43_(flock) {
        short   l_type;		/* BSD43_F_RDLCK, BSD43_F_WRLCK, or BSD43_F_UNLCK */
        short   l_whence;	/* flag to choose starting offset */
        long    l_start;	/* relative offset, in bytes */
        long    l_len;          /* length, in bytes; 0 means lock to EOF */
        short   l_pid;		/* returned with BSD43_F_GETLK */
        short   l_xxx;		/* reserved for future use */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define O_RDONLY	BSD43_O_RDONLY
#   define O_WRONLY	BSD43_O_WRONLY
#   define O_RDWR	BSD43_O_RDWR
#   define O_NDELAY	BSD43_O_NDELAY
#   define O_APPEND	BSD43_O_APPEND
#   define O_CREAT	BSD43_O_CREAT
#   define O_TRUNC	BSD43_O_TRUNC
#   define O_EXCL	BSD43_O_EXCL
#   define O_SYNC	BSD43_O_SYNC
#   define FNDELAY	BSD43_FNDELAY
#   define FAPPEND	BSD43_FAPPEND
#   define FASYNC	BSD43_FASYNC
#   define FCREAT	BSD43_FCREAT
#   define FTRUNC	BSD43_FTRUNC
#   define FEXCL	BSD43_FEXCL
#   define FSYNC	BSD43_FSYNC
#   define F_DUPFD	BSD43_F_DUPFD
#   define F_GETFD	BSD43_F_GETFD
#   define F_SETFD	BSD43_F_SETFD
#   define F_GETFL	BSD43_F_GETFL
#   define F_SETFL	BSD43_F_SETFL
#   define F_GETOWN	BSD43_F_GETOWN
#   define F_SETOWN	BSD43_F_SETOWN
#   define F_GETLK	BSD43_F_GETLK
#   define F_SETLK	BSD43_F_SETLK
#   define F_SETLKW	BSD43_F_SETLKW
#   define F_OK	BSD43_F_OK
#   define X_OK	BSD43_X_OK
#   define W_OK	BSD43_W_OK
#   define R_OK	BSD43_R_OK
#   define F_ULOCK	BSD43_F_ULOCK
#   define F_LOCK	BSD43_F_LOCK
#   define F_TLOCK	BSD43_F_TLOCK
#   define F_TEST	BSD43_F_TEST
#   define F_RDLCK	BSD43_F_RDLCK
#   define F_WRLCK	BSD43_F_WRLCK
#   define F_UNLCK	BSD43_F_UNLCK
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

#endif _BSD43_SYS_FCNTL_
