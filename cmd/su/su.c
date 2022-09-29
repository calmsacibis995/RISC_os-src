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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: su.c,v 1.15.2.4.1.1.1.2 90/11/02 17:53:01 beacker Exp $"

#ifdef SYSTYPE_BSD43
#undef BSD
#define BSD 1
#endif SYSTYPE_BSD43

#include <sys/param.h>
#include <sys/types.h>
#include <stdio.h>
#include <pwd.h>
#include <sysv/shadow.h>
#include <grp.h>
#include <string.h>

#if	defined(RISCOS) || defined(BSD)
#ifndef BSD
#include <bsd/syslog.h>
#include <bsd43/sys/syscall.h>
#define setpriority(a,b,c) syscall(BSD43_SYS_setpriority,(a),(b),(c))
#include <bsd/sys/time.h>
#include <bsd/sys/resource.h>
#else BSD
#include <syslog.h>
#include <sys/time.h>
#include <sys/resource.h>
#endif BSD
#endif

#include <sys/stat.h>

#ifndef BSD
struct passwd *getpwuid();
struct passwd *getpwnam();
struct group *getgrgid();
struct spwd *getspent();
#endif

char	userbuf[16]	= "USER=";
#if	defined(RISCOS) || ! defined(BSD)
char	lnambuf[20]	= "LOGNAME=";
#endif 
char	homebuf[128]	= "HOME=";
char	shellbuf[128]	= "SHELL=";
#if	defined(RISCOS) || ! defined(BSD)
char	pathbuf[128]	= "PATH=/usr/net:/bin:/usr/bin:/usr/ucb";
char	*cleanenv[] = { userbuf, lnambuf, homebuf, shellbuf,
			pathbuf, 0, 0, 0 };
#else 
char	pathbuf[128]	= "PATH=:/usr/ucb:/bin:/usr/bin";
char	*cleanenv[] = { userbuf, homebuf, shellbuf, pathbuf, 0, 0 };
#endif 
char	*user = "root";

#define DEFAULT_SHELL	"/bin/sh"
char	*shell = DEFAULT_SHELL;
int	fulllogin;
int	fastlogin;

/*
 * New flags:
 *
 *	-e	Do not reset the environment (in UMIPS-V, do not reset
 *		the shell; environment is only reset if '-' is given).
 *	-c	Execute extra arguments directly as command instead of using
 *		shell. If none are given, use the shell.
 *
 * If su is called as 'ssu', it is the same as 'su -e -c root'.
 *
 */

/*
 * Reset_env is set to 1 if we need to use the environment of the substituted
 * user. Reset_shell is set to 1 if we need to use the substituted user's
 * shell.
 */
#ifdef BSD
int	Reset_env = 1;
int	Reset_shell = 1;
#else
int	Reset_env = 0;
int	Reset_shell = 1;
#endif
int	Reset_home = 1;
int	Command = 0;
int	Have_user = 0;

/*
 * New feature:
 *
 *	If the user is trying to su to root, the file /etc/su_people is read
 *	to see if the user is in it. If so, no password is required.
 *
 *	This file MUST have owner and group 0 and mode 600 to be read, and no
 *	messages are printed if it isn't. This avoids someone accidentally
 *	leaving the file in such a state that users can change it and gain
 *	free system access.
 */

#define FREE_FILE	"/etc/su_people"
int	ck_free_entry();

char	*get_tty();

extern char	**environ;
struct	passwd *pwd;
struct	spwd *sp;
char	*crypt();
char	*getpass();
char	*get_enveq();
char	*getenv();
char	*getlogin();

main(argc,argv)
	int argc;
	char *argv[];
{
	char *password;
	char name[1000];
	FILE *fp;
	int su_for_free;
	register char *mylogin;
#if	defined(RISCOS) || ! defined(BSD)
	char *lname;
#endif 
#ifdef	RISCOS
	struct	stat	sb;
#endif RISCOS

#if	defined(RISCOS) || defined(BSD)
	openlog("su", LOG_ODELAY, LOG_AUTH);
#endif

	ckname(argv[0]);	/* Set flags based on name */

	while (argc > 1 && argv[1][0] == '-') {
		switch (argv[1][1]) {

			case 'f':
				fastlogin = 1;
				break;

			case '\0':
				fulllogin = 1;
				Reset_home = 1;
#ifndef BSD
				Reset_env = 1;
				Reset_shell = 1;
#endif BSD
				break;

			case 'e':
				Reset_env = 0;
				Reset_shell = 0;
				Reset_home = 0;
				break;

			case 'c':
				Command = 1;
				break;
			
			default:
				fprintf(stderr, "su: usage: su [-e] [-] [-f] [-c] [user] [command [args...]]\n");
				exit(1);
				break;

		}
		argv++;
		argc--;
	}
				
	
	if (!Have_user && argc > 1 && argv[1][0] != '-') {
		user = argv[1];
		argc--, argv++;
	}
	if ((pwd = getpwuid(getuid())) == NULL) {
#ifdef RISCOS
		syslog(LOG_CRIT,"can't find uid %d in passwd",getuid());
		closelog();
#endif
		fprintf(stderr, "Who are you?\n");
		exit(1);
	}
	strcpy(name, pwd->pw_name);
	if ((pwd = getpwnam(user)) == NULL) {
		fprintf(stderr, "Unknown login: %s\n", user);
		exit(1);
	}
	/*
	 * Only allow those in group zero or in the su_people file to su to
	 * root, unless the effective groupid is 0 (i.e., if the command
	 * is setgid 0).
	 */

	if ((mylogin = getlogin()) == NULL || *mylogin == '\0') {
		mylogin = name;
	}
	if (pwd->pw_uid == 0 && ck_free_entry(mylogin, name)) {
		su_for_free = 1;
	} else {
		su_for_free == 0;
	}

/*
 * In UMIPS-BSD or RISC/os, to su to root, one of the following must apply:
 *
 * 	1) You are able to su without a password
 *	2) You are root
 *	3) The su command is setgid(0)
 *	4) You are in group 0
 *
 * In UMIPS-V, this isn't a good idea since you can only be in one
 * group.
 */
#if	defined(RISCOS) || defined(BSD)
	if (!su_for_free && pwd->pw_uid == 0 && getegid() != 0) {
		struct	group *gr;
		int i;

		if ((gr = getgrgid(0)) != NULL) {
			for (i = 0; gr->gr_mem[i] != NULL; i++)
				if (strcmp(name, gr->gr_mem[i]) == 0)
					goto userok;
			fprintf(stderr, "You do not have permission to su %s\n",
				user);
			exit(1);
		}
	userok:
		setpriority(PRIO_PROCESS, 0, -2);
	}
#endif

	if (access(SHADOW,0) == 0) {	/* shadow passowrd file */
		setspent();
		sp = getspnam((char *)user);
		endspent();
		if ((struct spwd *)sp == NULL) {
			fprintf(stderr, "The shadow password file does not contain %s\n",user);
			fprintf(stderr, "Please contact the System Administrator.\n");
			syslog(LOG_CRIT, "shadow password file out of sync, missing entry for %s",user);
			closelog();
			exit(2);
		}
		if (sp->sp_pwdp[0] != '\0' && getuid() != 0 && !su_for_free) {
			password = getpass("Password:");
			if (strcmp(sp->sp_pwdp,crypt(password,sp->sp_pwdp)) != 0) {
				fprintf(stderr, "Sorry\n");
#if defined(RISCOS) || defined(BSD)
				if (pwd->pw_uid == 0) {
					syslog(LOG_CRIT, "BAD SU %s to %s on %s",
						mylogin, user, get_tty());
				}
				else {
				    syslog(LOG_WARNING,"%s failed to su to %s on %s",mylogin,user,get_tty());
				}
				closelog();
#endif
				exit(2);
			}
		}
	}  else  {
		if (pwd->pw_passwd[0] != '\0' && getuid() != 0 && !su_for_free) {
			password = getpass("Password:");
			if (strcmp(pwd->pw_passwd, crypt(password, pwd->pw_passwd)) != 0) {
				fprintf(stderr, "Sorry\n");
#if 	defined(RISCOS) || defined(BSD)
				if (pwd->pw_uid == 0) {
					syslog(LOG_CRIT, "BAD SU %s to %s on %s",
						mylogin, user, get_tty());
				}
				else {
				    syslog(LOG_WARNING,"%s failed to su to %s on %s",mylogin,user,get_tty());
				}
				closelog();
#endif
				exit(2);
			}
		}
	}
	endpwent();
#if	defined(RISCOS) || defined(BSD)
	if (pwd->pw_uid == 0) {
		syslog(LOG_NOTICE, "%s to %s on %s", mylogin, user, get_tty());
		closelog();
	}
	closelog();
#endif
	if (setgid(pwd->pw_gid) < 0) {
		perror("su: setgid");
		exit(3);
	}
#if	defined(RISCOS) || defined(BSD)
#ifdef	RISCOS
	if (!stat ("/etc/login.initgroups.ok", &sb))
#endif  RISCOS
		if (initgroups(user, pwd->pw_gid)) {
			fprintf(stderr, "su: initgroups failed\n");
			exit(4);
		}
#endif
	if (setuid(pwd->pw_uid) < 0) {
		perror("su: setuid");
		exit(5);
	}
	if (Reset_shell) {
		if (pwd->pw_shell && *pwd->pw_shell) {
			shell = pwd->pw_shell;
		}
	} else {
		shell = getenv("SHELL");
		if (shell == NULL) {
			shell = DEFAULT_SHELL;
		}
	}
	if (fulllogin) {
#if	defined(RISCOS) || ! defined(BSD)
		cleanenv[5] = get_enveq("TERM");
		if (cleanenv[5]) {
			cleanenv[6] = get_enveq("TZ");
		} else {
			cleanenv[5] = get_enveq("TZ");
		}
#else
		cleanenv[4] = get_enveq("TERM");
#endif
		environ = cleanenv;
	}

	if (Reset_home)
		setenv("HOME", pwd->pw_dir, homebuf);

	if (Reset_env) {
		if (strcmp(user, "root")) {
			setenv("USER", pwd->pw_name, userbuf);
#if	defined(RISCOS) || ! defined(BSD)
			setenv("LOGNAME", pwd->pw_name, lnambuf);
#endif 
		}
	}

#if	defined(RISCOS) || ! defined(BSD)
	/*
	 * Make sure that LOGNAME is actually set to something so
	 * that the shells don't scream.
	 */

	lname = getenv("LOGNAME");
	if (lname == NULL || *lname == '\0') {
		setenv("LOGNAME", pwd->pw_name, lnambuf);
	}
#endif

	if (Reset_shell) {
		setenv("SHELL", shell, shellbuf);
	}
#if	defined(RISCOS) || defined(BSD)
	setpriority(PRIO_PROCESS, 0, 0);
#endif
	if (Command && argc > 1) {
		execvp(argv[1], argv + 1);
		fprintf(stderr, "Could not execute %s\n", argv[1]);
		exit(7);
	}
	if (fastlogin) {
		*argv-- = "-f";
		*argv = "su";
	} else if (fulllogin) {
		if (chdir(pwd->pw_dir) < 0) {
			fprintf(stderr, "No directory\n");
			exit(6);
		}
		*argv = "-su";
	} else
		*argv = "su";
	execv(shell, argv);
	fprintf(stderr, "No shell\n");
	exit(7);
}

setenv(ename, eval, buf)
	char *ename, *eval, *buf;
{
	register char *cp, *dp;
	register char **ep = environ;

	/*
	 * this assumes an environment variable "ename" already exists
	 */
	while (dp = *ep++) {
		for (cp = ename; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0)) {
			strcat(buf, eval);
			*--ep = buf;
			return;
		}
	}
}

/*
 * The subroutine get_enveq() was called getenv(), but this isn't a
 * good idea, since it may cause profiling to break. Also, we need the
 * normal getenv behavior, too.
 *
 * This routine returns the full "NAME=value" string instead of just the
 * value portion.
 */

char *
get_enveq(ename)
	char *ename;
{
	register char *cp, *dp;
	register char **ep = environ;

	while (dp = *ep++) {
		for (cp = ename; *cp == *dp && *cp; cp++, dp++)
			continue;
		if (*cp == 0 && (*dp == '=' || *dp == 0))
			return (*--ep);
	}
	return ((char *)0);
}

/*
 * The subroutine ckname() sets options based on the name of the command.
 */

ckname(name)
char *name;
{
	char *base;

#ifndef BSD
#define rindex strrchr
#endif

	base = rindex(name, '/');
	if (base) {
		base++;
	} else {
		base = name;
	}

	if (strcmp(base, "ssu") == 0) {
		Have_user = 1;
		Reset_env = 0;
		Reset_shell = 0;
		Command = 1;
	}
}

/*
 * The subroutine ck_free_entry() checks to see if the user need not give
 * a password. This is only true if the file /etc/su_people is mode 0600,
 * has owner and group 0, and contains one of the given names. We look up
 * both the current username and the login name, since the user could
 * already be su'ed.
 *
 * The return value is 1 if the above are true, and 0 if not.
 */

static int ck_apply();
#define A_NOT		0
#define A_ALLOW		1
#define A_DENY		2

ck_free_entry(name1, name2)
char *name1;
char *name2;
{

	struct stat statb;
	FILE *fp;
	char buf[1024];		/* Buffer for holding data	*/

	if (name1 == NULL || name1[0] == '\0' ||
	    name2 == NULL || name2[0] == '\0') {	/* Unsafe */
		return 0;
	}

	if (stat(FREE_FILE, &statb) < 0) {
		return 0;
	}

	if ((statb.st_mode & ~S_IFMT) != 0600 || statb.st_uid != 0 ||
	    statb.st_gid != 0) {
		return 0;
	}

	if ((fp = fopen(FREE_FILE, "r")) == NULL) {
		return 0;
	}

	while (fgets(buf, sizeof(buf), fp) != NULL) {
		if (buf[strlen(buf) - 1] != '\n') {	/* Line too long */
			(void) fclose(fp);
			return 0;
		}
		if (buf[0] == '#') {			/* Comment */
			continue;
		}

		switch(ck_apply(name1, buf)) {
			case A_ALLOW:
				(void) fclose(fp);
				return 1;

			case A_DENY:
				(void) fclose(fp);
				return 0;
		}

		if (strcmp(name1, name2) == 0) {
			continue;
		}
		switch(ck_apply(name2, buf)) {
			case A_ALLOW:
				(void) fclose(fp);
				return 1;

			case A_DENY:
				(void) fclose(fp);
				return 0;
		}
	}

	(void) fclose(fp);
	return 0;
}

/*
 * The subroutine ck_apply() checks to see if the buffer applies to the
 * given name. If not, A_NOT is returned. Otherwise, the buffer is checked
 * to see how it applies, based on the buffer type.
 *
 * The buffer may be one of the following types:
 *
 *	{name}
 *		Return A_ALLOW if either of the names is {name}
 *	{name} {hostname list}
 *		Return A_ALLOW if either of the names is {name} and the
 *		current hostname is in {hostname list}
 *	{name} !{hostname list}
 *		Return A_DENY if either of the names is {name} and the
 *		current hostname is in {hostname list}
 *
 * The hostname list is a list of names separated by commas or whitespace.
 */

static int ck_host();

static int
ck_apply(name, buf)
char *name;
char *buf;
{

	int len;	/* Length of name */

	len = strlen(name);
	if (strncmp(name, buf, len) == 0) {
		switch(buf[len]) {
			case '\n':
				return A_ALLOW;
			case '\t':
			case ' ':
				if (ck_empty(&buf[len + 1])) {
					return A_ALLOW;
				}
				return ck_host(&buf[len + 1]);
		}

		/*
		 * Name doesn't match. May not be a syntax error. Example:
		 * 	name = foo
		 *	buffer = foobar
		 */
	}

	return A_NOT;
}

/*
 * The subroutine ck_empty() returns 1 if the given string contains only
 * spaces and tabs followed by a newline, and 0 otherwise.
 */

static
ck_empty(str)
char *str;
{

	while (*str != '\n') {
		if (*str != ' ' && *str != '\t') {
			return 0;
		}
		str++;
	}

	return 1;
}

/*
 * The subroutine ck_host() takes a list of hostnames and checks to see
 * if the current hostname is in the list.
 *
 * If the current hostname is in the list, A_ALLOW is returned. If not,
 * A_DENY is returned. If the list begins with '!', these two values are
 * reversed.
 *
 * If there are any syntax errors, A_DENY is returned for the sake of safety.
 */

static int
ck_host(list)
char *list;
{

	int not = 0;				/* 1 if list begins with ! */
	static char host[MAXHOSTNAMELEN + 1];	/* Current hostname */
	static int hlen = 0;			/* Length of hostname */

	if (hlen == 0) {
		gethostname(host, MAXHOSTNAMELEN);
		hlen = strlen(host);
	}

	/*
	 * Find beginning of list.
	 */

	while (*list == ' ' || *list == '\t') {
		list++;
	}

	if (*list == '!') {
		not = 1;
		list++;
		while (*list == ' ' || *list == '\t') {
			list++;
		}
		if (*list == '\n') {
			return A_DENY;		/* Syntax */
		}
	}

	/*
	 * Look at each list element
	 */

	while (*list != '\n') {
		if (strncmp(list, host, hlen) == 0) {
			switch (list[hlen]) {
				case ' ':
				case ',':
				case '\t':
				case '\n':
					if (not) {
						return A_DENY;
					} else {
						return A_ALLOW;
					}
			}
		}

		/*
		 * Go to next list item.
		 */

		list++;
		while (*list != ' ' && *list != '\t' && *list != ',' &&
		       *list != '\n') {
			list++;
		}
		while (*list == ' ' || *list == '\t' || *list == ',') {
			list++;
		}
	}

	if (not) {
		return A_ALLOW;
	} else {
		return A_DENY;
	}
}

/*
 * The subroutine get_tty() attempts to locate a tty port to use in
 * printing messages. It tries stderr, stdout, and stdin. If none
 * of these are ttys, the word "(unknown tty)" is returned.
 */

char *ttyname();

char *
get_tty()
{
	char *tty;

	tty = ttyname(2);
	if (tty && *tty) {
		return tty;
	}
	tty = ttyname(1);
	if (tty && *tty) {
		return tty;
	}
	tty = ttyname(0);
	if (tty && *tty) {
		return tty;
	}
	return "(unknown tty)";
}

#ifndef RISCOS
#ifndef BSD
/*
 * The subroutine gethostname() places the name of the current host
 * into the given buffer. This routine is for non-BSD systems, but it
 * does require uname().
 */

#include <sys/utsname.h>

gethostname(buf, len)
	char *buf;
	int len;
{
	struct utsname utb;

	uname(&utb);
	strncpy(buf, utb.sysname, len);
	buf[len] = '\0';
}
#endif BSD
#endif RISCOS
