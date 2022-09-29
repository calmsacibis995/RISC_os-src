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
#ident	"$Header: lock.c,v 1.6.1.2 90/05/09 16:40:39 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#

/*
 * mailx -- a modified version of a University of California at Berkeley
 *	mail program
 *
 * Stuff to do version 7 style locking.
 */

#include "rcv.h"
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>
#include <dirent.h>
#if	LOCKS
#include <bsd43/syscall.h>
#include <bsd43/sys/file.h>
#endif	LOCKS

extern int errno;
extern char *strrchr();

#ifdef USG
char	*maillock	= ".lock";		/* Lock suffix for mailname */
#else
char	*maillock	= ".mail";		/* Lock suffix for mailname */
#endif
static char		curlock[MAXNAMLEN+1];	/* Last used name of lock */
static	int		locked;			/* To note that we locked it */
#if	LOCKS
static  int lockfd = -1;			/* advisory lock fd */
#endif	LOCKS

lock(file)
char *file;
{
	int filedes, i, lpid, len;
	FILE *stream;
	char *cptr, buf[10];

	if (file == NOSTR) {
		printf("Locked = %d\n", locked);
		return(0);
	}

	/*
		Cannot lock a file with a basename of more than
		MAXNAMLEN - sizeof(maillock) characters, as we couldn't
		discern between the lockfile and the file itself.
	*/
	if ((cptr = strrchr(file,'/')) != NULL) cptr++;
	else cptr = file;

	if (strlen(cptr) > MAXNAMLEN-sizeof(maillock)) {
		fprintf(stderr,"mailx: Cannot lock file as length of basename '%s' is > %d characters\n",cptr,MAXNAMLEN-sizeof(maillock));
		return;
	}

	if (locked) return;
	strcpy(curlock,file);
	strcat(curlock,maillock);

	/*
		Check to see if lock file exists.

		If it does and it contains the pid of a live process,
		or we fail to read a numeric from the file, leave it 
		alone; else delete the file.
	*/
	if ((stream = fopen(curlock,"r")) != NULL) {

		if (fscanf(stream,"%d",&lpid) == 1)
			if (kill(lpid,0) == -1) 
				(void)unlink(curlock);

		(void)fclose(stream);
	}

	for (i = 0; i < 10; i++) {
		if ((filedes = open(curlock,O_WRONLY | O_CREAT | O_EXCL)) != -1) {
			chmod(curlock,0660);
			len = sprintf(buf,"%d",getpid());
			if (write(filedes,buf,len) != len) {
				close(filedes);
				perror("Error writing lock pid");
				if ( unlink(curlock) != 0 ) 
					perror("Couldn't unlink lock file");
				exit(1);
			}
			close(filedes);
			locked = 1;
#if	LOCKS
			if (lockfd==-1 && (lockfd=open(file, O_RDONLY))!=-1)
			     syscall(BSD43_SYS_flock, lockfd, BSD43_LOCK_EX);
#endif	LOCKS
			return;
		}
		sleep(5*(i+1));

	/* 
	   Recheck the contents of the lock file to make sure that
	   the process that created the file is still alive. 
	*/
		if ((stream = fopen(curlock,"r")) != NULL) {
	
			if (fscanf(stream,"%d",&lpid) == 1)
				if (kill(lpid,0) == -1) 
					(void)unlink(curlock);

			(void)fclose(stream);
		}
	}
	fprintf(stderr,"mailx: Cannot create lock file\n");
	exit(1);
}
/*
 * Remove the mail lock, and note that we no longer
 * have it locked.
 */

unlock()
{

#if	LOCKS
  	if (lockfd != -1) {
	  	close(lockfd);
		lockfd = -1;
	}
#endif	LOCKS
	remove(curlock);
	locked = 0;
}
