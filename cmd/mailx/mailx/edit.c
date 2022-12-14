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
#ident	"$Header: edit.c,v 1.6.1.2 90/05/09 16:39:04 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#

#include "rcv.h"
#include <stdio.h>
#include <sys/stat.h>
typedef	SIG	(*sigtype)();

/*
 * mailx -- a modified version of a University of California at Berkeley
 *	mail program
 *
 * Perform message editing functions.
 */


/*
 * Edit a message list.
 */

editor(msgvec)
	int *msgvec;
{
	char *edname;

	if ((edname = value("EDITOR")) == NOSTR)
		edname = EDITOR;
	return(edit1(msgvec, edname));
}

/*
 * Invoke the visual editor on a message list.
 */

visual(msgvec)
	int *msgvec;
{
	char *edname;

	if ((edname = value("VISUAL")) == NOSTR)
		edname = VISUAL;
	return(edit1(msgvec, edname));
}

/*
 * Edit a message by writing the message into a funnily-named file
 * (which should not exist) and forking an editor on it.
 * We get the editor from the stuff above.
 */

edit1(msgvec, ed)
	int *msgvec;
	char *ed;
{
	register int c;
	int *ip, pid, mesg, lines;
	long ms;
	SIG (*sigint)(), (*sigquit)();
	FILE *ibuf, *obuf;
	struct message *mp;
	extern char tempZedit[];
	off_t fsize(), size;
	struct stat statb;
	long modtime;
	int nchk[2];	/* for newline checking */

	/*
	 * Set signals; locate editor.
	 */

	sigint = sigset(SIGINT, SIG_IGN);
	sigquit = sigset(SIGQUIT, SIG_IGN); 

	/*
	 * Deal with each message to be edited . . .
	 */

	for (ip = msgvec; *ip && ip-msgvec < msgCount; ip++) {
		mesg = *ip;
		mp = &message[mesg-1];
		mp->m_flag |= MODIFY;

		if (!access(tempZedit, 2)) {
			printf("%s: file exists\n", tempZedit);
			goto out;
		}

		/*
		 * Copy the message into the edit file.
		 */

		close(creat(tempZedit, 0600));
		if ((obuf = fopen(tempZedit, "w")) == NULL) {
			perror(tempZedit);
			goto out;
		}
		if (send(mp, obuf, 0) < 0) {
			perror(tempZedit);
			fclose(obuf);
			remove(tempZedit);
			goto out;
		}
		fflush(obuf);
		if (ferror(obuf)) {
			remove(tempZedit);
			fclose(obuf);
			goto out;
		}
		fclose(obuf);

		/*
		 * If we are in read only mode, make the
		 * temporary message file readonly as well.
		 */

		if (readonly)
			chmod(tempZedit, 0400);

		/*
		 * Fork/execl the editor on the edit file.
		 */

		if (stat(tempZedit, &statb) < 0)
			modtime = 0;
		modtime = statb.st_mtime;
		pid = vfork();
		if (pid == -1) {
			perror("fork");
			remove(tempZedit);
			goto out;
		}
		if (pid == 0) {
			sigchild();
			setuid(getuid());
			setgid(getgid());
			if (sigint != (sigtype) SIG_IGN)
				sigsys(SIGINT, SIG_DFL);
			if (sigquit != (sigtype) SIG_IGN)
				sigsys(SIGQUIT, SIG_DFL);
			execlp(ed, ed, tempZedit, 0);
			perror(ed);
			_exit(1);
		}
		while (wait(&mesg) != pid)
			;

		/*
		 * If in read only mode, just remove the editor
		 * temporary and return.
		 */

		if (readonly) {
			remove(tempZedit);
			continue;
		}

		/*
		 * Now copy the message to the end of the
		 * temp file.
		 */

		if (stat(tempZedit, &statb) < 0) {
			perror(tempZedit);
			goto out;
		}
		if (modtime == statb.st_mtime) {
			remove(tempZedit);
			goto out;
		}
		if ((ibuf = fopen(tempZedit, "r")) == NULL) {
			perror(tempZedit);
			remove(tempZedit);
			goto out;
		}
		remove(tempZedit);
		fseek(otf, (long) 0, 2);
		size = fsize(otf);
		mp->m_block = blockof(size);
		mp->m_offset = offsetof(size);
		nchk[0] = nchk[1] = '\0';
		ms = 0L;
		lines = 0;
		while ((c = getc(ibuf)) != EOF) {
			if (c == '\n')
				lines++;
			putc(c, otf);
			nchk[1] = nchk[0];
			nchk[0] = c;
			if (ferror(otf))
				break;
			ms++;
		}

		/*
		 * Make sure that the last two characters were newlines to
		 * avoid joined messages.
		 *
		 * At this point, nchk[0] is the last character and nchk[1]
		 * is the next to last one. If the blank line at the end was
		 * deleted, but the last line ends with a newline, nchk[0]
		 * will be a newline and nchk[1] will not.
		 */

		if (!ferror(otf) && nchk[1] != '\n') {
			putc('\n', otf);
			ms++;
			lines++;
		}

		if (!ferror(otf) && nchk[0] != '\n') {
			putc('\n', otf);
			ms++;
			lines++;
		}

		mp->m_size = ms;
		mp->m_lines = lines;
		if (ferror(otf))
			perror("/tmp");
		fclose(ibuf);
	}

	/*
	 * Restore signals and return.
	 */

out:
	sigset(SIGINT, sigint);
	sigset(SIGQUIT, sigquit);
}
