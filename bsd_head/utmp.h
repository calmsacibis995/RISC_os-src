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
/* $Header: utmp.h,v 1.6.2.2 90/05/07 20:09:55 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Structure of utmp and wtmp files.
 *
 * Since UMIPS is based on System V, the utmp file is different.
 * Specifically, it contains some extra fields and doesn't contain
 * the ut_host field.  This file tries to make up for that as well
 * as possible.
 *
 * Assuming the number 8 is unwise.
 */
struct bsd43_(utmp) {
	char	ut_name[8];		/* user id */
	char	ut_spare1[4];
	char	ut_line[12];		/* tty name */
	short ut_spare2;	
	short ut_type;			/* Not normally in BSD */
	struct xxx_spares
	  {
	    short e_spare3;
	    short e_spare4;
	  }
	ut_spare5;
	long	ut_time;		/* time on */
};

#define bsd43_ut_host	???UT_HOST NOT AVAILABLE???

/*	Definitions for ut_type						*/

#define	EMPTY		0
#define	RUN_LVL		1
#define	BOOT_TIME	2
#define	OLD_TIME	3
#define	NEW_TIME	4
#define	INIT_PROCESS	5	/* Process spawned by "init" */
#define	LOGIN_PROCESS	6	/* A "getty" process waiting for login */
#define	USER_PROCESS	7	/* A user process */
#define	DEAD_PROCESS	8
#define	ACCOUNTING	9

#define	UTMAXTYPE	ACCOUNTING	/* Largest legal value of ut_type */

#ifdef SYSTYPE_BSD43

/*
 * The following defines are designed to support code dealing with MIPS-
 * style utmp files which has been inserted into bsd43 programs.
 * This makes all of the functionality of the sysv/utmp.h file available
 * to clients who compile with -systype bsd43.
 *
 * This must only be visible if -systype bsd43 has been used, or we may
 * conflict with defines/fields in sysv/utmp.h or posix/utmp.h or whatever.
 */

#define	UTMP_FILE	"/etc/utmp"
#define	WTMP_FILE	"/etc/wtmp"
#define	ut_user	ut_name
#define ut_id ut_spare1
#define ut_pid ut_spare2
#define ut_exit ut_spare5
#define e_termination e_spare3
#define e_exit e_spare4

/*	Special strings or formats used in the "ut_line" field when	*/
/*	accounting for something other than a process.			*/
/*	No string for the ut_line field can be more than 11 chars +	*/
/*	a NULL in length.						*/

#define	RUNLVL_MSG	"run-level %c"
#define	BOOT_MSG	"system boot"
#define	OTIME_MSG	"old time"
#define	NTIME_MSG	"new time"

#endif /* SYSTYPE_BSD43 */

/*
 * This is a utmp entry that does not correspond to a genuine user
 */
#define bsd43_nonuser(ut) (bsd43_(strncmp)((ut).ut_line, "tty", 3) == 0 && ((ut).ut_line[3] == 'p' \
	|| (ut).ut_line[3] == 'q' || (ut).ut_line[3] == 'r'))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define nonuser bsd43_nonuser
#   define ut_host bsd43_ut_host
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


