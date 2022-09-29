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
#ident	"$Header: openchild.c,v 1.2.1.2 90/05/07 20:58:56 wje Exp $"
/*
 * @(#)openchild.c 1.3 88/07/27 4.0NFSSRC Copyr 1988 Sun Micro
 * @(#)openchild.c 1.7 88/02/08 SMI
 *
 * Open two pipes to a child process, one for reading, one for writing.
 * The pipes are accessed by FILE pointers. This is NOT a public
 * interface, but for internal use only!
 */
#include <stdio.h>

extern char *malloc();
extern char *strrchr();
#ifdef SYSTYPE_BSD43
extern char *sprintf();
#endif

static char *basename();
static char SHELL[] = "/bin/sh";


/*
 * returns pid, or -1 for failure
 */
_openchild(command, fto, ffrom)
	char *command;
	FILE **fto;
	FILE **ffrom;
{
	int i;
	int pid;
	int pdto[2];
	int pdfrom[2];
	char *com;

		
	if (pipe(pdto) < 0) {
		goto error1;
	}
	if (pipe(pdfrom) < 0) {
		goto error2;
	}
	switch (pid = vfork()) {
	case -1:
		goto error3;

	case 0:
		/* 
		 * child: read from pdto[0], write into pdfrom[1]
		 */
		(void) close(0); 
		(void) dup(pdto[0]);
		(void) close(1); 
		(void) dup(pdfrom[1]);
		for (i = _rpc_dtablesize() - 1; i >= 3; i--) {
			(void) close(i);
		}
		com = malloc((unsigned) strlen(command) + 6);
		if (com == NULL) {
			_exit(~0);
		}	
		(void) sprintf(com, "exec %s", command);
		execl(SHELL, basename(SHELL), "-c", com, NULL);
		_exit(~0);
 
	default:
		/*
		 * parent: write into pdto[1], read from pdfrom[0]
		 */
		*fto = fdopen(pdto[1], "w");
		(void) close(pdto[0]);
		*ffrom = fdopen(pdfrom[0], "r");
		(void) close(pdfrom[1]);
		break;
	}
	return (pid);

	/*
	 * error cleanup and return
	 */
error3:
	(void) close(pdfrom[0]);
	(void) close(pdfrom[1]);
error2:
	(void) close(pdto[0]);
	(void) close(pdto[1]);
error1:
	return (-1);
}

static char *
basename(path)
	char *path;
{
	char *p;

	p = strrchr(path, '/');
	if (p == NULL) {
		return (path);
	} else {
		return (p + 1);
	}
}
