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
#ident	"$Header: uucpd.c,v 1.2.2.2 90/05/10 00:41:48 wje Exp $"

/*
 * 4.2BSD or 2.9BSD TCP/IP server for uucico
 * uucico's TCP channel causes this server to be run at the remote end.
 */

#include "uucp.h"
#include <bsd/netdb.h>
#include <signal.h>
#include <errno.h>
#include <bsd/sys/socket.h>
#include <bsd/netinet/in.h>
#include <bsd/sys/wait.h>
#include <bsd/sys/ioctl.h>
#include <sun/pwd.h>

struct	sockaddr_in hisctladdr;
int hisaddrlen = sizeof hisctladdr;
struct	sockaddr_in myctladdr;
int mypid;

char Username[64];
char *nenv[] = {
	Username,
	NULL,
};
extern char **environ;

main(argc, argv)
int argc;
char **argv;
{
#ifndef BSDINETD
	register int s, tcp_socket;
	struct servent *sp;
#endif !BSDINETD
	extern int errno;
	int dologout();

	environ = nenv;
#ifdef BSDINETD
	close(1); close(2);
	dup(0); dup(0);
	hisaddrlen = sizeof (hisctladdr);
	if (getpeername(0, &hisctladdr, &hisaddrlen) < 0) {
		fprintf(stderr, "%s: ", argv[0]);
		perror("getpeername");
		_exit(1);
	}
	if (fork() == 0)
		doit(&hisctladdr);
	dologout();
	exit(1);
#else !BSDINETD
	sp = getservbyname("uucp", "tcp");
	if (sp == NULL){
		perror("uucpd: getservbyname");
		exit(1);
	}
	if (fork())
		exit(0);
	if ((s=open("/dev/tty", 2)) >= 0){
		ioctl(s, TIOCNOTTY, (char *)0);
		close(s);
	}

	bzero((char *)&myctladdr, sizeof (myctladdr));
	myctladdr.sin_family = AF_INET;
	myctladdr.sin_port = sp->s_port;
	tcp_socket = socket(AF_INET, SOCK_STREAM, 0);
	if (tcp_socket < 0) {
		perror("uucpd: socket");
		exit(1);
	}
	if (bind(tcp_socket, (char *)&myctladdr, sizeof (myctladdr)) < 0) {
		perror("uucpd: bind");
		exit(1);
	}
	listen(tcp_socket, 3);	/* at most 3 simultaneuos uucp connections */
	signal(SIGCHLD, dologout);

	for(;;) {
		s = accept(tcp_socket, &hisctladdr, &hisaddrlen);
		if (s < 0){
			if (errno == EINTR) 
				continue;
			perror("uucpd: accept");
			exit(1);
		}
		if (fork() == 0) {
			close(0); close(1); close(2);
			dup(s); dup(s); dup(s);
			close(tcp_socket); close(s);
			doit(&hisctladdr);
			exit(1);
		}
		close(s);
	}

#endif	!BSDINETD
}

doit(sinp)
struct sockaddr_in *sinp;
{
	char user[64], passwd[64];
	char *xpasswd, *crypt();
	struct passwd *pw, *getpwnam();

	alarm(60);
	printf("login: "); fflush(stdout);
	if (readline(user, sizeof user) < 0) {
		fprintf(stderr, "user read\n");
		return;
	}
	/* truncate username to 8 characters */
	user[8] = '\0';
	pw = getpwnam(user);
	if (pw == NULL) {
		fprintf(stderr, "user unknown\n");
		return;
	}
	if (strcmp(pw->pw_shell, UUCICO)) {
		fprintf(stderr, "Login incorrect.");
		return;
	}
	if (pw->pw_passwd && *pw->pw_passwd != '\0') {
		printf("Password: "); fflush(stdout);
		if (readline(passwd, sizeof passwd) < 0) {
			fprintf(stderr, "passwd read\n");
			return;
		}
		xpasswd = crypt(passwd, pw->pw_passwd);
		if (strcmp(xpasswd, pw->pw_passwd)) {
			fprintf(stderr, "Login incorrect.");
			return;
		}
	}
	alarm(0);
	sprintf(Username, "USER=%s", user);
	dologin(pw, sinp);
	setgid(pw->pw_gid);
#ifdef BSD4_X
	initgroups(pw->pw_name, pw->pw_gid);
#endif BSD4_X
	chdir(pw->pw_dir);
	setuid(pw->pw_uid);
	execl(UUCICO, "uucico", (char *)0);
	perror("uucico server: execl");
}

readline(p, n)
register char *p;
register int n;
{
	char c;

	while (n-- > 0) {
		if (read(0, &c, 1) <= 0)
			return(-1);
		c &= 0177;
		if (c == '\n' || c == '\r') {
			*p = '\0';
			return(0);
		}
		*p++ = c;
	}
	return(-1);
}

#include <utmp.h>
#ifdef BSD4_X
#include <fcntl.h>
#endif BSD4_X

#define	SCPYN(a, b)	strncpy(a, b, sizeof (a))

struct utmp entry;

dologout()
{
	union wait status;
	int pid;
	char line[32];
	FILE *fp;

#ifdef BSDINETD
	while ((pid=wait(&status)) > 0) {
#else  !BSDINETD
	while ((pid=wait3(&status,WNOHANG,0)) > 0) {
#endif !BSDINETD
		if ((fp = fopen("/etc/wtmp", "r+")) == NULL) {
			return;
		}

		SCPYN(entry.ut_user, "");
		SCPYN(entry.ut_id, "0");
		sprintf(line, "uucp%.4d", getpid());
		SCPYN(entry.ut_line, line);
		entry.ut_pid = getpid();
		entry.ut_type = DEAD_PROCESS;
		entry.ut_time = time(0);

		fseek(fp, 0L, 2);
		fwrite((char *)&entry, sizeof(entry), 1, fp);
		fclose(fp);
	}
}

/*
 * Record login in wtmp file.
 */
dologin(pw, sin)
struct passwd *pw;
struct sockaddr_in *sin;	/* not used at this time */
{
	char line[32];
	FILE *fp;

	if ((fp = fopen("/etc/wtmp", "r+")) == NULL) {
		return;
	}

	SCPYN(entry.ut_user, "uucpd");
	SCPYN(entry.ut_id, "0");
	sprintf(line, "uucp%.4d", getpid());
	SCPYN(entry.ut_line, line);
	entry.ut_pid = getpid();
	entry.ut_type = LOGIN_PROCESS;
	entry.ut_time = time(0);

	fseek(fp, 0L, 2);
	fwrite((char *)&entry, sizeof(entry), 1, fp);
	fclose(fp);
}
