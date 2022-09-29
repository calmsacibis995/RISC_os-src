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
#ident	"$Header: p2open.c,v 1.6.2.3 90/05/09 18:32:20 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*	
	Similar to popen(3S) but with pipe to cmd's stdin and from stdout.
*/

/*LINTLIBRARY*/
#include <stdio.h>
#include <signal.h>
#include <fcntl.h>

extern FILE	*fdopen();
extern int	close(),
		execl(),
		fcntl(),
		fork(),
		pipe();
static int	popen_pid[20];


p2open(cmd, fpin, fpout)
char	*cmd;
FILE	**fpin;	/* file pointer to cmd stdin */
FILE	**fpout;	/* file pointer from cmd stdout */
{
	int		tocmd[2];
	int		fromcmd[2];
	register int	pid;

	if( pipe(tocmd) < 0  ||  pipe(fromcmd) < 0 )
		return  0;
	if( (pid = fork()) == 0 ) {
		(void) close( tocmd[1] );
		(void) close( 0 );
		(void) fcntl( tocmd[0], F_DUPFD, 0 );
		(void) close( tocmd[0] );
		(void) close( fromcmd[0] );
		(void) close( 1 );
		(void) fcntl( fromcmd[1], F_DUPFD, 1 );
		(void) close( fromcmd[1] );
		(void) execl("/bin/sh", "sh", "-c", cmd, 0);
		_exit(1);
	}
	if(pid == -1)
		return  0;
	popen_pid[ tocmd[1] ] = pid;
	popen_pid[ fromcmd[0] ] = pid;
	(void) close( tocmd[0] );
	(void) close( fromcmd[1] );
	*fpin = fdopen( tocmd[1], "w" );
	*fpout = fdopen( fromcmd[0], "r" );
	return  pid;
}

int
p2close( fp1, fp2 )
FILE	*fp1;
FILE	*fp2;
{
	register int	pid,
			r;
	int		status;
	void		(*hstat)(),
			(*istat)(),
			(*qstat)();

	pid = popen_pid[ fileno(fp1) ];
	if( pid != popen_pid[ fileno(fp2) ] ) {
		pgmname();
		fprintf( stderr,
			"p2close(fp1, fp2):  fp1 not from same p2open() as fp2\n" );
		exit( 1 );
	}
	(void) fclose(fp1);
	(void) fclose(fp2);
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	hstat = signal(SIGHUP, SIG_IGN);
	while( (r = wait(&status)) != pid  &&  r != -1 )
		;
	if( r == -1 )
		status = -1;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	(void) signal(SIGHUP, hstat);
	return  status;
}
