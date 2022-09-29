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
#ident	"$Header: chgrp.c,v 1.1.2.2 90/05/10 03:56:38 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * chgrp gid file ...
 */

#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <grp.h>

struct	group	*gr,*getgrnam();
struct	stat	stbuf;
ushort	gid;
int	status;

chgrpcmd(argc, argv)
char *argv[];
{
	register c;

	if(argc < 3) {
		fprintf(stderr,"chgrp: usage: chgrp gid file ...\n");
		return(4);
	}
	if(isnumber(argv[1])) {
		gid = atoushort(argv[1]); /* gid is unsigned short */
	} else {
		if((gr=getgrnam(argv[1])) == NULL) {
			fprintf(stderr,"chgrp: unknown group: %s\n",argv[1]);
			return(4);
		}
		gid = gr->gr_gid;
	}
	for(c=2; c<argc; c++) {
		stat(argv[c], &stbuf);
		if(chown(argv[c], stbuf.st_uid, gid) < 0) {
			perror(argv[c]);
			status = 1;
		}
	}
	return(status);
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

atoushort(s)
register char *s;
{
	register char c;
	ushort register i, previ;
	i = previ = 0;
	while(c = *s++) {
		i = c - '0' + 10 * i;
		if(i < previ) {
			fprintf(stderr,"chgrp: numeric group id too large\n");
			return(4);
    		}
		previ = i;
	}
	return(i);
}
