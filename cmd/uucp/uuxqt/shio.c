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
#ident	"$Header: shio.c,v 1.4.2.2 90/05/10 00:46:59 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "uucp.h"

/*
 * use shell to execute command with
 * fi, fo, and fe as standard input/output/error
 *	cmd 	-> command to execute
 *	fi 	-> standard input
 *	fo 	-> standard output
 *	fe 	-> standard error
 * return:
 *	0		-> success 
 *	non zero	-> failure  -  status from child
			(Note - -1 means the fork failed)
 */
shio(cmd, fi, fo, fe)
char *cmd, *fi, *fo, *fe;
{
	register int pid, ret;
	int status;

	if (fi == NULL)
		fi = "/dev/null";
	if (fo == NULL)
		fo = "/dev/null";
	if (fe == NULL)
		fe = "/dev/null";

	DEBUG(3, "shio - %s\n", cmd);
	if ((pid = fork()) == 0) {
		(void) signal(SIGINT, SIG_IGN);
		(void) signal(SIGHUP, SIG_IGN);
		(void) signal(SIGQUIT, SIG_IGN);
		closelog();
		(void) close(Ifn);	/* close connection fd's */
		(void) close(Ofn);
		(void) close(0);	/* get stdin from file fi */
		if (open(fi, 0) != 0)
			exit(errno);
		(void) close(1);	/* divert stdout to fo */
		if (creat(fo, 0666) != 1)
			exit(errno);
		(void) close(2);	/* divert stderr to fe */
		if (creat(fe, 0666) != 2)
			exit(errno);
		(void) execle(SHELL, "sh", "-c", cmd, (char *) 0, Env);
		exit(100);
	}

	/*
	 * the status returned from wait can never be -1
	 * see man page wait(2)
	 * So we use the -1 value to indicate fork failed
	 */
	if (pid == -1)
		return(-1);
	

	while ((ret = wait(&status)) != pid && ret != -1);
	DEBUG(3, "status %d\n", status);
	return(status);
}
