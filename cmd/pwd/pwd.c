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
#ident	"$Header: pwd.c,v 1.9.2.2 90/05/09 18:21:21 wje Exp $"

/*
**	Print working (current) directory
*/


#include	<stdio.h>
#include	<sys/types.h>
#include	<sys/param.h>
#include	<sys/nami.h>

extern char *getcwd();

main(argc)
	int argc;
{
	char	*dirname;	

	if (argc != 1) {
		fprintf(stderr, "usage: pwd\n");
		exit(2);
	}
	dirname = getcwd(NULL,MAXPATHLEN);
	if (dirname == NULL) {
		perror("..");
		exit(2);
	};
	printf("%s\n",dirname);
	exit(0);
}
