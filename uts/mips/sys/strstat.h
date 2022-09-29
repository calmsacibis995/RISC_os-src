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
/* $Header: strstat.h,v 1.7.1.2 90/05/10 06:39:23 wje Exp $ */

#ifndef	_SYS_STRSTAT_
#define	_SYS_STRSTAT_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 * Streams Statistics header file.  This file
 * defines the counters which are maintained for statistics gathering
 * under Streams.
 */

typedef struct {
	int use;			/* current item usage count */
	int total;			/* total item usage count */
	int max;			/* maximum item usage count */
	int fail;			/* count of allocation failures */
} alcdat;

struct  strstat {
	alcdat stream;			/* stream allocation data */
	alcdat queue;			/* queue allocation data */
	alcdat mblock;			/* message block allocation data */
	alcdat dblock;			/* aggregate data block allocation */
	alcdat dblk[NCLASS];		/* data block class allocation */
};


/* in the following macro, x is assumed to be of type alcdat */
#if defined(OS_METER) || defined(mips)
#define BUMPUP(X)	{(X).use++;  (X).total++;\
	(X).max=((X).use>(X).max?(X).use:(X).max); }
#define BUMPDOWN(X) ((X).use--)
#else
#define BUMPUP(X)
#define BUMPDOWN(X)
#endif


/* per-module statistics structure */
struct module_stat {
	long ms_pcnt;			/* count of calls to put proc */
	long ms_scnt;			/* count of calls to service proc */
	long ms_ocnt;			/* count of calls to open proc */
	long ms_ccnt;			/* count of calls to close proc */
	long ms_acnt;			/* count of calls to admin proc */
	char *ms_xptr;			/* pointer to private statistics */
	short ms_xsize;			/* length of private statistics buf */
};

#endif	_SYS_STRSTAT_
