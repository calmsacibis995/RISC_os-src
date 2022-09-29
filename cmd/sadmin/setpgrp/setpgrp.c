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
#ident	"$Header: setpgrp.c,v 1.5.2.2 90/05/09 18:37:47 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	Set process group ID to this process ID and exec the command line
	that is the argument list.
*/


#include <stdio.h>

main( argc, argv )
int	argc;
char	*argv[];
{
	char	*cmd;

	cmd = *argv;
	if( argc <= 1 ) {
		fprintf( stderr, "Usage:  %s command [ arg ... ]\n", cmd );
		exit(1);
	}
	argv++;
	argc--;
	setpgrp();
	execvp( *argv, argv );
	fprintf( stderr, "%s: %s not executed.  ", cmd, *argv );
	perror( "" );
	exit( 1 );
}
