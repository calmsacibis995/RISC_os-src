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
#ident	"$Header: rcmd.c,v 1.5.2.5.1.1.1.2 90/11/06 12:28:13 beacker Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms are permitted
 * provided that the above copyright notice and this paragraph are
 * duplicated in all such forms and that any documentation,
 * advertising materials, and other materials related to such
 * distribution and use acknowledge that the software was developed
 * by the University of California, Berkeley.  The name of the
 * University may not be used to endorse or promote products derived
 * from this software without specific prior written permission.
 * THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
 * WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE.
 */

#ifdef SYSTYPE_SYSV
#ifndef CASE_SENSITIVE_HOSTS
#define CASE_SENSITIVE_HOSTS 1
#endif
#endif SYSTYPE_SYSV

#include <stdio.h>
#include <ctype.h>
#include <pwd.h>

#ifndef SYSTYPE_SYSV
#include <sys/param.h>
#include <sys/file.h>
#include <sys/signal.h>
#include <sys/socket.h>
#else SYSTYPE_SYSV
#ifdef SYSTYPE_POSIX
#ifndef _U_CHAR
#define _U_CHAR
typedef unsigned long u_long;
#endif 	/* _U_CHAR */
#endif SYSTYPE_POSIX
#include <bsd/sys/param.h>
#include <bsd/sys/file.h>
#include <signal.h>
#include <bsd/sys/socket.h>
#endif SYSTYPE_SYSV

#include <sys/stat.h>

#ifndef SYSTYPE_SYSV
#include <netinet/in.h>
#include <netdb.h>
#else SYSTYPE_SYSV
#include <bsd/netinet/in.h>
#include <bsd/netdb.h>
#endif SYSTYPE_SYSV

#include <errno.h>
extern	struct passwd *getpwnam();

extern	errno;
char	*index();
#ifndef SYSTYPE_SYSV
char	*sprintf();
#endif SYSTYPE_SYSV
static char *domain;	/* for innetgr */

rcmd(ahost, rport, locuser, remuser, cmd, fd2p)
	char **ahost;
	u_short rport;
	char *locuser, *remuser, *cmd;
	int *fd2p;
{
#ifdef SYSTYPE_POSIX
	sigset_t	oldset;
	sigset_t	newset;
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV
	void (*oldmask)();
#else SYSTYPE_SYSV
	int oldmask;
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
	int s, timo = 1, pid, retval;

	struct sockaddr_in sin, sin2, from;
	char c;
	int lport = IPPORT_RESERVED - 1;
	struct hostent *hp;

	pid = getpid();
	hp = gethostbyname(*ahost);
	if (hp == 0) {
		fprintf(stderr, "%s: unknown host\n", *ahost);
		return (-1);
	}
	*ahost = hp->h_name;
#ifdef SYSTYPE_POSIX
	sigemptyset(&newset);
	sigaddset(&newset,SIGURG);
	sigprocmask(SIG_BLOCK,&newset,&oldset);
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV
	oldmask = sigset(SIGURG, SIG_HOLD);
#else SYSTYPE_SYSV
	oldmask = sigblock(sigmask(SIGURG));
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
	for (;;) {
		s = rresvport(&lport);
		if (s < 0) {
			if (errno == EAGAIN)
				fprintf(stderr, "socket: All ports in use\n");
			else
				perror("rcmd: socket");
#ifdef SYSTYPE_POSIX
			sigprocmask(SIG_SETMASK,&oldset);
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV

			(void)sigset(SIGURG, oldmask);
#else SYSTYPE_SYSV
			sigsetmask(oldmask);
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
			return (-1);
		}
		fcntl(s, F_SETOWN, pid);
		sin.sin_family = hp->h_addrtype;
		bcopy(hp->h_addr_list[0], (caddr_t)&sin.sin_addr, hp->h_length);
		sin.sin_port = rport;
		if (connect(s, (caddr_t)&sin, sizeof (sin), 0) >= 0)
			break;
		(void) close(s);
		if (errno == EADDRINUSE) {
			lport--;
			continue;
		}
		if (errno == ECONNREFUSED && timo <= 16) {
			sleep(timo);
			timo *= 2;
			continue;
		}
		if (hp->h_addr_list[1] != NULL) {
			int oerrno = errno;

			fprintf(stderr,
			    "connect to address %s: ", inet_ntoa(sin.sin_addr));
			errno = oerrno;
			perror("");
			hp->h_addr_list++;
			bcopy(hp->h_addr_list[0], (caddr_t)&sin.sin_addr,
			    hp->h_length);
			fprintf(stderr, "Trying %s...\n",
				inet_ntoa(sin.sin_addr));
			continue;
		}
		perror(hp->h_name);
#ifdef SYSTYPE_POSIX
		sigprocmask(SIG_SETMASK,&oldset);
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV
		(void)sigset(SIGURG, oldmask);
#else SYSTYPE_SYSV
		sigsetmask(oldmask);
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
		return (-1);
	}
	lport--;
	if (fd2p == 0) {
		write(s, "", 1);
		lport = 0;
	} else {
		char num[8];
		int s2 = rresvport(&lport), s3;
		int len = sizeof (from);

		if (s2 < 0)
			goto bad;
		listen(s2, 1);
		(void) sprintf(num, "%d", lport);
		if (write(s, num, strlen(num)+1) != strlen(num)+1) {
			perror("write: setting up stderr");
			(void) close(s2);
			goto bad;
		}
		s3 = accept(s2, &from, &len, 0);
		(void) close(s2);
		if (s3 < 0) {
			perror("accept");
			lport = 0;
			goto bad;
		}
		*fd2p = s3;
		from.sin_port = ntohs((u_short)from.sin_port);
		if (from.sin_family != AF_INET ||
		    from.sin_port >= IPPORT_RESERVED) {
			fprintf(stderr,
			    "socket: protocol failure in circuit setup.\n");
			goto bad2;
		}
	}
	(void) write(s, locuser, strlen(locuser)+1);
	(void) write(s, remuser, strlen(remuser)+1);
	(void) write(s, cmd, strlen(cmd)+1);
	retval = read(s, &c, 1);
	if (retval != 1) {
		if (retval == 0) {
		    fprintf(stderr,
		      "Protocol error, %s closed connection\n", *ahost);
		} else if (retval < 0) {
		    perror(*ahost);
		} else {
		    fprintf(stderr,
		      "Protocol error, %s sent %d bytes\n", *ahost, retval);
		}
		goto bad2;
	}
	if (c != 0) {
		while (read(s, &c, 1) == 1) {
			(void) write(2, &c, 1);
			if (c == '\n')
				break;
		}
		goto bad2;
	}
#ifdef SYSTYPE_POSIX
	sigprocmask(SIG_SETMASK,&oldset);
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV
	(void)sigset(SIGURG, oldmask);
#else SYSTYPE_SYSV
	sigsetmask(oldmask);
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
	return (s);
bad2:
	if (lport)
		(void) close(*fd2p);
bad:
	(void) close(s);
#ifdef SYSTYPE_POSIX
	sigprocmask(SIG_SETMASK,&oldset);
#else SYSTYPE_POSIX
#ifdef SYSTYPE_SYSV
	(void)sigset(SIGURG, oldmask);
#else SYSTYPE_SYSV
	sigsetmask(oldmask);
#endif SYSTYPE_SYSV
#endif SYSTYPE_POSIX
	return (-1);
}

rresvport(alport)
	int *alport;
{
	struct sockaddr_in sin;
	int s;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = INADDR_ANY;
	s = socket(AF_INET, SOCK_STREAM, 0);
	if (s < 0)
		return (-1);
	for (;;) {
		sin.sin_port = htons((u_short)*alport);
		if (bind(s, (caddr_t)&sin, sizeof (sin)) >= 0)
			return (s);
		if (errno != EADDRINUSE) {
			(void) close(s);
			return (-1);
		}
		(*alport)--;
		if (*alport == IPPORT_RESERVED/2) {
			(void) close(s);
			errno = EAGAIN;		/* close */
			return (-1);
		}
	}
}

ruserok(rhost, superuser, ruser, luser)
	char *rhost;
	int superuser;
	char *ruser, *luser;
{
	FILE *hostf;
	char fhost[MAXHOSTNAMELEN];
	register char *sp, *p;
	int baselen = -1;

	struct stat sbuf;
	struct passwd *pwd;
	char pbuf[MAXPATHLEN];
	int euid = -1;

	sp = rhost;
	p = fhost;
	while (*sp) {
		if (*sp == '.') {
			if (baselen == -1)
				baselen = sp - rhost;
			*p++ = *sp++;
		} else {
#ifdef CASE_SENSITIVE_HOSTS
			*p++ = *sp++;
#else CASE_SENSITIVE_HOSTS
			*p++ = isupper(*sp) ? tolower(*sp++) : *sp++;
#endif CASE_SENSITIVE_HOSTS
		}
	}
	*p = '\0';
	/* this code comes from NFSSRC 4.0 */
	/* check /etc/hosts.equiv */
	if (!superuser) {
		if ((hostf = fopen("/etc/hosts.equiv", "r")) != NULL) {
			if (!_validuser(hostf, fhost, luser, ruser, baselen)) {
				(void) fclose(hostf);
				return(0);
		        }
			(void) fclose(hostf);
		}
	}

	/* check ~/.rhosts */

	if ((pwd = getpwnam(luser)) == NULL)
       		return(-1);
	(void)strcpy(pbuf, pwd->pw_dir);
	(void)strcat(pbuf, "/.rhosts");

	/* 
	 * Read .rhosts as the local user to avoid NFS mapping the root uid
	 * to something that can't read .rhosts.
	 */
	euid = geteuid();
	(void) seteuid (pwd->pw_uid);
	if ((hostf = fopen(pbuf, "r")) == NULL) {
		if (euid != -1)
	    		(void) seteuid (euid);
	  	return(-1);
	}
	(void)fstat(fileno(hostf), &sbuf);
	if (sbuf.st_uid && sbuf.st_uid != pwd->pw_uid) {
	  	fclose(hostf);
		if (euid != -1)
		  	(void) seteuid (euid);
		return(-1);
	}

	if (!_validuser(hostf, fhost, luser, ruser, baselen)) {
	  	(void) fclose(hostf);
		if (euid != -1)
			(void) seteuid (euid);
		return(0);
	}

	(void) fclose(hostf);
	if (euid != -1)
       		(void) seteuid (euid);
	return (-1);
}

_validuser(hostf, rhost, luser, ruser, baselen)
char *rhost, *luser, *ruser;
FILE *hostf;
int baselen;
{
	char *user;
	char ahost[MAXHOSTNAMELEN];
	int hostmatch, usermatch;
	register char *p;

	/* set up the NIS domain name */
	if (domain == NULL) {
		(void) usingypmap(&domain, NULL);
	}
	while (fgets(ahost, sizeof (ahost), hostf)) {
		p = ahost;
		while (*p != '\n' && *p != ' ' && *p != '\t' && *p != '\0') {
#ifndef CASE_SENSITIVE_HOSTS
			*p = isupper(*p) ? tolower(*p) : *p;
#endif CASE_SENSITIVE_HOSTS
			p++;
		}
		if (*p == ' ' || *p == '\t') {
			*p++ = '\0';
			while (*p == ' ' || *p == '\t')
				p++;
			user = p;
			while (*p != '\n' && *p != ' ' && *p != '\t' && *p != '\0')
				p++;
		} else
			user = p;
		*p = '\0';

        /*
         *  + - anything goes
         *  +@<name> - group <name> allowed
         *  -@<name> - group <name> disallowed
         *  -<name> - host <name> disallowed
         */
		if (ahost[0] == '+' && ahost[1] == 0)
			hostmatch = 1;
		else if (ahost[0] == '+' && ahost[1] == '@')
			hostmatch = innetgr(ahost + 2, rhost,
			    NULL, domain);
		else if (ahost[0] == '-' && ahost[1] == '@') {
			if (innetgr(ahost + 2, rhost, NULL, domain))
				break;
		}
		else if (ahost[0] == '-') {
			if (_checkhost(rhost, ahost+1, baselen))
				break;
		}
		else
			hostmatch = _checkhost(rhost, ahost, baselen);
		if (user[0]) {
			if (user[0] == '+' && user[1] == 0)
				usermatch = 1;
			else if (user[0] == '+' && user[1] == '@')
				usermatch = innetgr(user+2, NULL,
				    ruser, domain);
			else if (user[0] == '-' && user[1] == '@') {
				if (hostmatch && innetgr(user+2, NULL,
				    ruser, domain))
					break;
			}
			else if (user[0] == '-') {
				if (hostmatch && !strcmp(user+1, ruser))
					break;
			}
			else
				usermatch = !strcmp(user, ruser);
		}
		else
			usermatch = !strcmp(ruser, luser);
		if (hostmatch && usermatch)
			return (0);
	}
	return (-1);
}

_checkhost(rhost, lhost, len)
char *rhost, *lhost;
int len;
{
	static char ldomain[MAXHOSTNAMELEN + 1];
	static char *domainp = NULL;
	static int nodomain = 0;
	register char *cp;

	if (len == -1)
		return(!strcmp(rhost, lhost));
	if (strncmp(rhost, lhost, len))
		return(0);
	if (!strcmp(rhost, lhost))
		return(1);
	if (*(lhost + len) != '\0')
		return(0);
	if (nodomain)
		return(0);
	if (!domainp) {
		if (getdomainname(ldomain, sizeof(ldomain)) == -1) {
			nodomain = 1;
			return(0);
		}
		ldomain[MAXHOSTNAMELEN] = NULL;
		if ((domainp = ldomain) == (char *)NULL) {
			nodomain = 1;
			return(0);
		}
#ifndef CASE_SENSITIVE_HOSTS
		for (cp = domainp; *cp; ++cp)
			if (isupper(*cp))
				*cp = tolower(*cp);
#endif CASE_SENSITIVE_HOSTS
	}
	return(!strcmp(domainp, rhost + len +1));
}
