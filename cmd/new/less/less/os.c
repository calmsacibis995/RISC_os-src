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
#ident	"$Header: os.c,v 1.1.2.2 90/05/09 17:57:35 wje Exp $"

/*
 * Operating system dependent routines.
 *
 * Most of the stuff in here is based on Unix, but an attempt
 * has been made to make things work on other operating systems.
 * This will sometimes result in a loss of functionality, unless
 * someone rewrites code specifically for the new operating system.
 *
 * The makefile provides defines to decide whether various
 * Unix features are present.
 */

#include "less.h"
#include <signal.h>

char *getenv();

/*
 * Pass the specified command to a shell to be executed.
 * Like plain "system()", but handles resetting terminal modes, etc.
 */
	public void
lsystem(cmd)
	char *cmd;
{
	int inp;
	char cmdbuf[256];
	char *shell;

	/*
	 * Print the command which is to be executed,
	 * unless the command starts with a "-".
	 */
	if (cmd[0] == '-')
		cmd++;
	else
	{
		lower_left();
		clear_eol();
		putstr("!");
		putstr(cmd);
		putstr("\n");
	}

	/*
	 * De-initialize the terminal and take out of raw mode.
	 */
	deinit();
	flush();
	raw_mode(0);

	/*
	 * Restore signals to their defaults.
	 */
	SIGNAL(SIGINT, SIG_DFL);
#ifdef SIGTSTP
	SIGNAL(SIGTSTP, SIG_DFL);
#endif
	/*
	 * Force standard input to be the terminal, "/dev/tty",
	 * even if less's standard input is coming from a pipe.
	 */
	inp = dup(0);
	close(0);
	if (open("/dev/tty", 0) < 0)
		dup(inp);

	/*
	 * Pass the command to the system to be executed.
	 */
	if ((shell = getenv("SHELL")) != NULL)
	{
		sprintf(cmdbuf, "%s -c \"%s\"", shell, cmd);
		cmd = cmdbuf;
	}
	system(cmd);

	/*
	 * Restore standard input, reset signals, raw mode, etc.
	 */
	close(0);
	dup(inp);
	close(inp);

	init_signals();
	raw_mode(1);
	init();
}

/*
 * Expand a filename, substituting any environment variables, etc.
 * The implementation of this is necessarily very operating system
 * dependent.  This implementation is unabashedly only for Unix systems.
 */
#if GLOB

#include <stdio.h>
FILE *popen();

	public char *
glob(filename)
	char *filename;
{
	FILE *f;
	char *p;
	int ch;
	static char ECHO[] = "echo ";
	static char filebuf[FILENAME+sizeof(ECHO)+1];

	if (filename[0] == '#')
		return (filename);
	strcpy(filebuf, ECHO);
	strtcpy(filebuf+sizeof(ECHO)-1, filename, sizeof(filebuf)-sizeof(ECHO));
	if ((f = popen(filebuf, "r")) == NULL)
		return (filename);
	for (p = filebuf;  p < &filebuf[sizeof(filebuf)-1];  p++)
	{
		if ((ch = getc(f)) == '\n' || ch == EOF)
			break;
		*p = ch;
	}
	*p = '\0';
	pclose(f);
	return (filebuf);
}

#else

	public char *
glob(filename)
	char *filename;
{
	return (filename);
}

#endif


/*
 * Returns NULL if the file can be opened and
 * is an ordinary file, otherwise an error message
 * (if it cannot be opened or is a directory, etc.)
 */

#if STAT

#include <sys/types.h>
#include <sys/stat.h>

	public char *
bad_file(filename, message, len)
	char *filename;
	char *message;
	int len;
{
	struct stat statbuf;

	if (stat(filename, &statbuf) < 0)
		return (errno_message(filename, message, len));

	if ((statbuf.st_mode & S_IFMT) == S_IFDIR)
	{
		static char is_dir[] = " is a directory";
		strtcpy(message, filename, len-sizeof(is_dir)-1);
		strcat(message, is_dir);
		return (message);
	}
	if ((statbuf.st_mode & S_IFMT) != S_IFREG)
	{
		static char not_reg[] = " is not a regular file";
		strtcpy(message, filename, len-sizeof(not_reg)-1);
		strcat(message, not_reg);
		return (message);
	}
	return (NULL);
}

#else

	public char *
bad_file(filename, message, len)
	char *filename;
	char *message;
	int len;
{
	return (NULL);
}

#endif

/*
 * Return an error message based on the value of "errno".
 */

#if PERROR

	public char *
errno_message(filename, message, len)
	char *filename;
	char *message;
	int len;
{
	char *p;
	static char msg[16];

	extern char *sys_errlist[];
	extern int sys_nerr;
	extern int errno;

	if (errno < sys_nerr)
		p = sys_errlist[errno];
	else
	{
		sprintf(msg, "Error %d", errno);
		p = msg;
	}
	strtcpy(message, filename, len-strlen(p)-3);
	strcat(message, ": ");
	strcat(message, p);
	return (message);
}

#else

	public char *
errno_message(filename, message, len)
	char *filename;
	char *message;
	int len;
{
	static char msg[] = ": cannot open";

	strtcpy(message, filename, len-sizeof(msg)-1);
	strcat(message, msg);
	return (message);
}

#endif
