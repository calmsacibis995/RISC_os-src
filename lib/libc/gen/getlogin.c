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
#ident	"$Header: getlogin.c,v 1.8.1.2 90/05/10 01:32:36 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <sys/types.h>
#ifdef	SYSTYPE_POSIX
#include <sysv/utmp.h>
#define	ttyslot	_ttyslot
#else
#include "utmp.h"
#endif

#define NULL 0

extern long lseek();
extern int open(), read(), close(), ttyslot();

static char utmp_file[] = "/etc/utmp";

char *
getlogin()
{
	register me, uf;
	struct utmp ubuf ;
	static char answer[sizeof(ubuf.ut_user)+1] ;

	if ((me = ttyslot()) < 0)
		return(NULL);
	if ((uf = open(utmp_file, 0)) < 0)
		return(NULL);
	(void) lseek(uf, (long)(me * sizeof(ubuf)), 0);
	if (read(uf, (char*)&ubuf, sizeof(ubuf)) != sizeof(ubuf)) {
		(void) close(uf);
		return(NULL);
	}
	(void) close(uf);
	if (ubuf.ut_user[0] == '\0') {
		return(NULL);
	}
	if (ubuf.ut_type != USER_PROCESS) {
		return(NULL);
	}
	strncpy(&answer[0],&ubuf.ut_user[0],sizeof(ubuf.ut_user)) ;
	answer[sizeof(ubuf.ut_user)] = '\0' ;
	return(&answer[0]);
}
