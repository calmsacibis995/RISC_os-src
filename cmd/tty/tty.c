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
#ident	"$Header: tty.c,v 1.7.2.2 90/05/10 00:20:20 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*
** Type tty name
*/

#include	"stdio.h"
#include	"sys/stermio.h"

char	*ttyname();

extern int	optind;
int		lflg;
int		sflg;

main(argc, argv)
char **argv;
{
	register char *p;
	register int	i;

	while((i = getopt(argc, argv, "ls")) != EOF)
		switch(i) {
		case 'l':
			lflg = 1;
			break;
		case 's':
			sflg = 1;
			break;
		case '?':
			fprintf(stderr, "Usage: tty [-l] [-s]\n");
			exit(2);
		}
	p = ttyname(0);
	if(!sflg)
		printf("%s\n", (p? p: "not a tty"));
	if(lflg) {
		if((i = ioctl(0, STWLINE, 0)) == -1)
			printf("not on an active synchronous line\n");
		else
			printf("synchronous line %d\n", i);
	}
	exit(p? 0: 1);
}
