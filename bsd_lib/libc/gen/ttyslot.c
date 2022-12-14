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
#ident	"$Header: ttyslot.c,v 1.2.2.2 90/05/07 20:45:02 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * Return the number of the slot in the utmp file
 * corresponding to the current user: try for file 0, 1, 2.
 * Returns -1 if slot not found.
 */
#include <sys/types.h>
#include <utmp.h>
#define	NULL	0

extern char *ttyname(), *strrchr();
extern int strncmp(), open(), read(), close();

static char utmp_file[] = "/etc/utmp";

int
ttyslot()
{
	struct utmp ubuf;
	register char *tp, *p;
	register int s, fd;

	if((tp=ttyname(0)) == NULL && (tp=ttyname(1)) == NULL &&
					(tp=ttyname(2)) == NULL)
		return(-1);

	if((p=strrchr(tp, '/')) == NULL)
		p = tp;
	else
		p++;

	if((fd=open(utmp_file, 0)) < 0)
		return(-1);
	s = 0;
	while(read(fd, (char*)&ubuf, sizeof(ubuf)) == sizeof(ubuf)) {
		if(    (ubuf.ut_type == INIT_PROCESS ||
			ubuf.ut_type == LOGIN_PROCESS ||
			ubuf.ut_type == USER_PROCESS ||
			ubuf.ut_type == DEAD_PROCESS ) &&
			strncmp(p, ubuf.ut_line, sizeof(ubuf.ut_line)) == 0){

			(void) close(fd);
			return(s);
		}
		s++;
	}
	(void) close(fd);
	return(-1);
}
