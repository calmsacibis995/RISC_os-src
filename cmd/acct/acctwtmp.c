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
#ident	"$Header: acctwtmp.c,v 1.1.2.2 90/05/09 15:03:54 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
 *	acctwtmp reason >> /etc/wtmp
 *	writes utmp.h record (with current time) to end of std. output
 *	acctwtmp `uname` >> /etc/wtmp as part of startup
 *	acctwtmp pm >> /etc/wtmp  (taken down for pm, for example)
 */
#include <stdio.h>
#include "acctdef.h"
#include <sys/types.h>
#include <utmp.h>

struct	utmp	wb;
extern	time_t time();

main(argc, argv)
char **argv;
{
	if(argc < 2) {
		fprintf(stderr, "Usage: %s reason [ >> %s ]\n",
			argv[0], WTMP_FILE);
		exit(1);
	}

	strncpy(wb.ut_line, argv[1], LSZ);
	wb.ut_line[11] = NULL;
	wb.ut_type = ACCOUNTING;
	time(&wb.ut_time);
	fseek(stdout, 0L, 2);
	fwrite(&wb, sizeof(wb), 1, stdout);
}
