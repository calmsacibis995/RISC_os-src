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
#ident	"$Header: chown.c,v 1.5.2.3 90/05/09 15:21:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * chown uid file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <pwd.h>

struct	passwd	*pwd,*getpwnam();
struct	stat	stbuf;
ushort	uid;
int	status;

main(argc, argv)
char *argv[];
{
	register c;
	ushort atoushort() ;

	if(argc < 3) {
		fprintf(stderr, "usage: chown uid file ...\n");
		exit(4);
	}
	if(isnumber(argv[1])) {
		uid = atoushort(argv[1]); /* uid is unsigned short */
		goto cho;
	}
	if((pwd=getpwnam(argv[1])) == NULL) {
		fprintf(stderr, "chown: unknown user id %s\n",argv[1]);
		exit(4);
	}
	uid = pwd->pw_uid;

cho:
	for(c=2; c<argc; c++) {
		stat(argv[c], &stbuf);
		if(chown(argv[c], uid, stbuf.st_gid) < 0) {
			perror(argv[c]);
			status = 1;
		}
	}
	exit(status);
}

isnumber(s)
char *s;
{
	register c;

	while(c = *s++)
		if(!isdigit(c))
			return(0);
	return(1);
}

ushort
atoushort(s)
register char *s;
{
	register char c;
	register int i = 0 ;
	register ushort maxushort = ((ushort) ~0) ;

	while(c = *s++) {
		i = c - '0' + 10 * i;
		if(i > maxushort) {
			fprintf(stderr,"chown: numeric user id too large\n");
			exit(4);
   		}
	}
	return (ushort) i ;
}
