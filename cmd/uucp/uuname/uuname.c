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
#ident	"$Header: uuname.c,v 1.4.2.2 90/05/10 00:45:00 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"
 
/*
 * returns a list of remote systems accessible to uucico
 * option:
 *	-l	-> returns only the local system name.
 *	-c	-> print remote systems accessible to cu
 */
main(argc,argv, envp)
int argc;
char **argv, **envp;
{
	int c, lflg = 0, cflg = 0;
	char s[BUFSIZ], prev[BUFSIZ], name[BUFSIZ];

	while ( (c = getopt(argc, argv, "lc")) != EOF )
		switch(c) {
		case 'c':
			cflg++;
			break;
		case 'l':
			lflg++;
			break;
		default:
			(void) fprintf(stderr, "usage: uuname [-l] [-c]\n");
			exit(1);
		}
 
	if (lflg) {
		if ( cflg )
			(void) fprintf(stderr,
			"uuname: -l overrides -c ... -c option ignored\n");
		uucpname(name);

		puts(name);
		exit(0);
	}
 
	if ( cflg )
		setservice("cu");
	else
		setservice("uucico");

	while ( getsysline(s, sizeof(s)) ) {
		if((s[0] == '#') || (s[0] == ' ') || (s[0] == '\t') || 
		    (s[0] == '\n'))
			continue;
		(void) sscanf(s, "%s", name);
		if (EQUALS(name, prev))
		    continue;
		puts(name);
		(void) strcpy(prev, name);
	}
	exit(0);
}
