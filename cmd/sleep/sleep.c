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
#ident	"$Header: sleep.c,v 1.5.2.2 90/05/09 19:02:21 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
**	sleep -- suspend execution for an interval
**
**		sleep time
*/

#include	<stdio.h>

main(argc, argv)
char **argv;
{
	int	c, n;
	char	*s;

	n = 0;
	if(argc < 2) {
		fprintf(stderr, "usage: sleep time\n");
		exit(2);
	}
	s = argv[1];
	while(c = *s++) {
		if(c<'0' || c>'9') {
			fprintf(stderr, "sleep: bad character in argument\n");
			exit(2);
		}
		n = n*10 + c - '0';
	}
	sleep(n);
}
