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
#ident	"$Header: getmem.c,v 1.3.2.2 90/05/10 03:48:17 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/***********************************************************
 *
 *	Utility to get content of variables from system
 *
 ***********************************************************/

#include <nlist.h>

struct	nlist	nlistbuf[2];
int	err;
int	fd;
int	freemem[1];

main(argc,argv)
int argc;
char *argv[];
{
	/* get the address of freemem */
	nlistbuf[0].n_name = argv[2];
	nlist(argv[1],&nlistbuf[0]);

	/* try to get size of system memory */
	fd = open("/dev/kmem",0);
	if(fd < 0)
	{ perror("getmem"); exit(-1); }

	lseek(fd,nlistbuf[0].n_value,0);
	read(fd,&freemem[0],sizeof(int));
	close(fd);

	if(argc > 3)
	{
		printf("%s %d\n",argv[2],freemem[0]);
	}

	exit((freemem[0] & 0xFFFF) / 256);	/* return size */
}

