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
#ident	"$Header: rmttape.c,v 1.1.2.2.1.1.1.2 90/10/05 10:04:27 beacker Exp $"

#ifndef SYSTYPE_BSD43
#include <stdio.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <signal.h>
#include <errno.h>
#include <fcntl.h>
#include <bsd/sys/types.h>
#include <bsd/sys/socket.h>
#include <bsd/netinet/in.h>
#include <bsd/netdb.h>
#include <pwd.h>
#include <bsd43/sys/syscall.h>
#define socketpair(domain,socktype,protocol,sv) \
	syscall(BSD43_SYS_socketpair,domain,socktype,protocol,sv)
#else
#include <sys/param.h>
#include <sys/mtio.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <sys/vnode.h>

#include <netinet/in.h>

#include <stdio.h>
#include <pwd.h>
#include <netdb.h>
#endif

#define	TS_CLOSED	0
#define	TS_OPEN		1

static	int rmtstate = TS_CLOSED;
int	rmtape;
int	rmtconnaborted();
char	*rmtpeer;
struct passwd *getpwuid();

extern char *rmtprogramname;
extern int nblock;		/* blocking factor on tape */
#define TP_BSIZE 512

rmthost(host)
	char *host;
{

	rmtpeer = host;
	signal(SIGPIPE, rmtconnaborted);
	rmtgetconn();
	if (rmtape < 0)
		return (0);
	return (1);
}

rmtconnaborted()
{

	fprintf(stderr, "%s: Lost connection to remote host.\n",
		rmtprogramname);
	exit(1);
}

rmtgetconn()
{
	static struct servent *sp = 0;
	struct passwd *pw;
	char *name = "root";
	int size;
	int	uid;

	if (sp == 0) {
		sp = getservbyname("shell", "tcp");
		if (sp == 0) {
			fprintf(stderr, "%s: shell/tcp: unknown service\n",
				rmtprogramname);
			exit(1);
		}
	}
	uid = getuid();
	pw = getpwuid(uid);
	if (pw && pw->pw_name)
		name = pw->pw_name;
	if (uid == 0) {
		rmtape = rcmd(&rmtpeer, sp->s_port, name, name, "/etc/rmt", 0);
		size = nblock * TP_BSIZE;
		while (size > TP_BSIZE &&
		    setsockopt(rmtape, SOL_SOCKET, SO_SNDBUF, &size, sizeof (size)) < 0)
			size -= TP_BSIZE;
	} else {
		int	sv[2];
		int	child;

		if (socketpair(AF_UNIX,SOCK_STREAM,PF_UNSPEC,sv) == -1) {
			fprintf(stderr,"%s: ",rmtprogramname);
			perror("socketpair");
			exit(1);
		};
		child = fork();
		if (child == -1) {
			fprintf(stderr,"%s: ",rmtprogramname);
			perror("fork");
			exit(1);
		};
		if (child == 0) {
			int	nfds;
			int	i;

			dup2(sv[1],0);
			dup2(sv[1],1);
			dup2(sv[1],2);
			nfds = getdtablesize();
			for (i = 3; i < nfds; i++) 
				close(i);
			execl("/usr/ucb/rsh","/usr/ucb/rsh",
				rmtpeer,"-l",name,"/etc/rmt",NULL);
			execl("/usr/net/rsh","/usr/ucb/rsh",
				rmtpeer,"-l",name,"/etc/rmt",NULL);
			fprintf(stderr,"%s: ",rmtprogramname);
			perror("execl /usr/net/rsh");
			exit(1);
		};
		rmtape = sv[0];
	};
}

rmtopen(tape, mode)
	char *tape;
	int mode;
{
	char buf[256];

	sprintf(buf, "O%s\n%d\n", tape, mode);
	rmtstate = TS_OPEN;
	return (rmtcall(tape, buf));
}

rmtclose()
{

	if (rmtstate != TS_OPEN)
		return;
	rmtcall("close", "C\n");
	rmtstate = TS_CLOSED;
}

rmtread(buf, count)
	char *buf;
	int count;
{
	char line[30];
	int n, i, cc;
	extern errno;

	sprintf(line, "R%d\n", count);
	n = rmtcall("read", line);
	if (n < 0) {
		errno = n;
		return (-1);
	}
	for (i = 0; i < n; i += cc) {
		cc = read(rmtape, buf+i, n - i);
		if (cc <= 0) {
			rmtconnaborted();
		}
	}
	return (n);
}

rmtwrite(buf, count)
	char *buf;
	int count;
{
	char line[30];

	sprintf(line, "W%d\n", count);
	write(rmtape, line, strlen(line));
	write(rmtape, buf, count);
	return (rmtreply("write"));
}


rmtseek(offset, pos)
	int offset, pos;
{
	char line[80];

	sprintf(line, "L%d\n%d\n", offset, pos);
	return (rmtcall("seek", line));
}

struct	mtget mts;

struct mtget *
rmtstatus()
{
	register int i;
	register char *cp;

	if (rmtstate != TS_OPEN)
		return (0);
	rmtcall("status", "S\n");
	for (i = 0, cp = (char *)&mts; i < sizeof(mts); i++)
		*cp++ = rmtgetb();
	return (&mts);
}

rmtioctl(cmd, count)
	int cmd, count;
{
	char buf[256];

	if (count < 0)
		return (-1);
	sprintf(buf, "I%d\n%d\n", cmd, count);
	return (rmtcall("ioctl", buf));
}

rmtcall(cmd, buf)
	char *cmd, *buf;
{

	if (write(rmtape, buf, strlen(buf)) != strlen(buf))
		rmtconnaborted();
	return (rmtreply(cmd));
}

rmtreply(cmd)
	char *cmd;
{
	register int c;
	char code[30], emsg[BUFSIZ];

	rmtgets(code, sizeof (code));
	if (*code == 'E' || *code == 'F') {
		rmtgets(emsg, sizeof (emsg));
		fprintf(stderr,"%s: %s\n", cmd, emsg, code + 1);
		if (*code == 'F') {
			rmtstate = TS_CLOSED;
			return (-1);
		}
		return (-1);
	}
	if (*code != 'A') {
		fprintf(stderr,"Protocol to remote tape server botched (code %s?).\n",
		    code);
		rmtconnaborted();
	}
	return (atoi(code + 1));
}

rmtgetb()
{
	char c;

	if (read(rmtape, &c, 1) != 1)
		rmtconnaborted();
	return (c);
}

rmtgets(cp, len)
	char *cp;
	int len;
{

	while (len > 1) {
		*cp = rmtgetb();
		if (*cp == '\n') {
			cp[1] = 0;
			return;
		}
		cp++;
		len--;
	}
	fprintf(stderr,"Protocol to remote tape server botched (in rmtgets).\n");
	rmtconnaborted();
}
