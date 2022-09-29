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
#ident	"$Header: rcp.c,v 1.13.2.6.1.2 90/08/07 16:31:07 hawkes Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
char copyright[] =
"@(#) Copyright (c) 1983 Regents of the University of California.\n\
 All rights reserved.\n";
#endif not lint

#ifndef lint
static char sccsid[] = "@(#)rcp.c	5.9 (Berkeley) 10/22/87";
#endif not lint

/*
 * rcp
 */
#include <sys/param.h>
#include <sys/file.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/ioctl.h>

#include <netinet/in.h>

#include <stdio.h>
#include <signal.h>
#include <pwd.h>
#include <ctype.h>
#include <netdb.h>
#include <errno.h>

/* "rcp" program invokes "/bin/cp" to copy files locally.  However, "/bin/cp"
 * program is system V program which does not support BSD "cp" semantics.
 * Since BSD environment installation is optional, there is no guarantee
 * that BSD version "cp" will be available.  Instead of moving BSD 
 * "cp" program to standard system and hide it under different name, etc.,
 * I have decided to pull in that functionality here.  In other words,
 * instead of invoking "/bin/cp" to do local copy, I have added a new
 * routine "local_copy" to do exactly the same thing.  This can all be
 * turned off by defining "LOCAL_COPY" to 0.
 */

#define	LOCAL_COPY	1	/* set it to non-zero value to invoke 
				 * local_copy() routine instead of
				 * invoking /bin/cp for local copy.
				 */

int	rem;
char	*colon(), *index(), *rindex(), *malloc(), *strcpy();
int	errs;
int	lostconn();
#if RISCOS
extern	int errno;
extern	char *sys_errlist[];
#else
int	errno;
char	*sys_errlist[];
#endif
int	iamremote, targetshouldbedirectory;
int	iamrecursive;
int	pflag;
struct	passwd *pwd;
struct	passwd *getpwuid();
int	userid;
int	port;

#if RISCOS
#define MAXALIASES	35
char myhostname[MAXHOSTNAMELEN + 1];
char myaliases[MAXALIASES + 1][MAXHOSTNAMELEN + 1];
#endif

struct buffer {
	int	cnt;
	char	*buf;
} *allocbuf();

/*VARARGS*/
int	error();

#define	ga()	 	(void) write(rem, "", 1)

main(argc, argv)
	int argc;
	char **argv;
{
	char *targ, *host, *src;
	char *suser, *tuser, *thost;
	int i;
	char buf[BUFSIZ], cmd[16];
	struct servent *sp;
#if RISCOS
/*
   OB_BSD42_COMPAT=0 => support fully qualified internet domainized hostnames.
   OB_BSD42_COMPAT=1 => support all domainized hostnames but hostnames of the
   	form "hh.uu" which is interpreted as BSD 4.2 old style dotted names as
	host=hh and user=uu.  It is used for backward compatibility with 4.2.

   The old style 4.2 doted names 'host.user' is incompatible with internet
   hostdomain syntax.
   See also comments below in sethostuser(). 
   OB_BSD42_COMPAT should be defined to 0.
*/
#define OB_BSD42_COMPAT	0
#endif

#if RISCOS && OB_BSD42_COMPAT
	int dot = 0;		/* use old style host.user:file */
	int junk;
  
#endif
#if RISCOS
	setmyhost ();
#endif
	sp = getservbyname("shell", "tcp");
	if (sp == NULL) {
		fprintf(stderr, "rcp: shell/tcp: unknown service\n");
		exit(1);
	}
	port = sp->s_port;
	pwd = getpwuid(userid = getuid());
	if (pwd == 0) {
		fprintf(stderr, "who are you?\n");
		exit(1);
	}

	for (argc--, argv++; argc > 0 && **argv == '-'; argc--, argv++) {
		(*argv)++;
		while (**argv) switch (*(*argv)++) {

		    case 'r':
			iamrecursive++;
			break;

		    case 'p':		/* preserve mtimes and atimes */
			pflag++;
			break;

		    /* The rest of these are not for users. */
		    case 'd':
			targetshouldbedirectory = 1;
			break;

		    case 'f':		/* "from" */
			iamremote = 1;
			(void) response();
			(void) setuid(userid);
			source(--argc, ++argv);
			exit(errs);

		    case 't':		/* "to" */
			iamremote = 1;
			(void) setuid(userid);
			sink(--argc, ++argv);
			exit(errs);

		    default:
			usage();
		}
	}
	if (argc < 2)
		usage();
	if (argc > 2)
		targetshouldbedirectory = 1;
	rem = -1;
	(void) sprintf(cmd, "rcp%s%s%s",
	    iamrecursive ? " -r" : "", pflag ? " -p" : "", 
	    targetshouldbedirectory ? " -d" : "");
	(void) signal(SIGPIPE, lostconn);
	targ = colon(argv[argc - 1]);
	if (targ) {				/* ... to remote */
		*targ++ = 0;
		if (*targ == 0)
			targ = ".";
#if RISCOS && OB_BSD42_COMPAT
		sethostuser (argv[argc - 1], & thost, & tuser, NULL, & dot);
#else
		thost = index(argv[argc - 1], '@');
		if (thost) {
			*thost++ = 0;
			tuser = argv[argc - 1];
			if (*tuser == '\0')
				tuser = NULL;
			else if (!okname(tuser))
				exit(1);
		} else {
			thost = argv[argc - 1];
			tuser = NULL;
		}
#endif
		for (i = 0; i < argc - 1; i++) {
			src = colon(argv[i]);
			if (src) {		/* remote to remote */
				*src++ = 0;
				if (*src == 0)
					src = ".";
#if RISCOS && OB_BSD42_COMPAT
				sethostuser (argv[i], & host,
					& suser, pwd->pw_name, & junk);
				if (suser && !okname(suser))
					continue;
				/*
				 * this always passes the user name,
				 * but big deal.
				 */
				if (dot)	/* set only for dest */
		(void) sprintf(buf, "/usr/ucb/rsh %s -l %s -n %s %s '%s%s%s:%s'",
					    host, suser, cmd, src,
					    thost ? thost : "",
					    tuser ? "." : "",
					    tuser ? tuser : "",
					    targ ? targ : "");
				else
		(void) sprintf(buf, "/usr/ucb/rsh %s -l %s -n %s %s '%s%s%s:%s'",
					    host, suser, cmd, src,
					    tuser ? tuser : "",
					    tuser ? "@" : "",
					    thost ? thost : "",
					    targ ? targ : "");
#else
				host = index(argv[i], '@');
				if (host) {
					*host++ = 0;
					suser = argv[i];
					if (*suser == '\0')
						suser = pwd->pw_name;
					else if (!okname(suser))
						continue;
		(void) sprintf(buf, "/usr/ucb/rsh %s -l %s -n %s %s '%s%s%s:%s'",
					    host, suser, cmd, src,
					    tuser ? tuser : "",
					    tuser ? "@" : "",
					    thost, targ);
				}
				else {
				  	host = argv[i];
		(void) sprintf(buf, "/usr/ucb/rsh %s -n %s %s '%s%s%s:%s'",
					    host, cmd, src,
					    tuser ? tuser : "",
					    tuser ? "@" : "",
					    thost, targ);
	      			}
#endif
#if RISCOS
				if (sourceistarget (host, src, thost, targ))
					continue;
#endif
				(void) susystem(buf);
			} else {		/* local to remote */
#if RISCOS
				if (sourceistarget (myhostname, argv[i],
							thost, targ)) {
					continue;
				}
#endif
				if (rem == -1) {
					(void) sprintf(buf, "%s -t %s",
					    cmd, targ);
					host = thost;
					rem = rcmd(&host, port, pwd->pw_name,
					    tuser ? tuser : pwd->pw_name,
					    buf, 0);
					if (rem < 0)
						exit(1);
					if (response() < 0)
						exit(1);
					(void) setuid(userid);
				}
				source(1, argv+i);
			}
		}
	} else {				/* ... to local */
		if (targetshouldbedirectory)
			verifydir(argv[argc - 1]);
		for (i = 0; i < argc - 1; i++) {
			src = colon(argv[i]);
			if (src == 0) {		/* local to local */
#if LOCAL_COPY
				(void) local_copy(argv[i], argv[argc - 1]);
#else
				(void) sprintf(buf, "/bin/cp%s%s %s %s",
				    iamrecursive ? " -r" : "",
				    pflag ? " -p" : "",
				    argv[i], argv[argc - 1]);
				(void) susystem(buf);
#endif
			} else {		/* remote to local */
				*src++ = 0;
				if (*src == 0)
					src = ".";
#if RISCOS && OB_BSD42_COMPAT
				sethostuser (argv[i], & host, & suser,
							pwd->pw_name, & junk);
				if (suser && !okname(suser))
					continue;
  				(void) sprintf(buf, "%s -f %s", cmd, src);
#else
				host = index(argv[i], '@');
				if (host) {
					*host++ = 0;
					suser = argv[i];
					if (*suser == '\0')
						suser = pwd->pw_name;
					else if (!okname(suser))
						continue;
				} else {
					host = argv[i];
					suser = pwd->pw_name;
				}
				(void) sprintf(buf, "%s -f %s", cmd, src);
#endif
#if RISCOS
				if (sourceistarget (host, src, myhostname,
							argv[argc-1]))
					continue;
#endif
				rem = rcmd(&host, port, pwd->pw_name, suser,
				    buf, 0);
#if RISCOS
				if (rem < 0) {
					errs++;
					continue;
				}
#else
				if (rem < 0)
					continue;
#endif
				(void) setreuid(0, userid);
				sink(1, argv+argc-1);
				(void) setreuid(userid, 0);
				(void) close(rem);
				rem = -1;
			}
		}
	}
	exit(errs);
}

verifydir(cp)
	char *cp;
{
	struct stat stb;

	if (stat(cp, &stb) >= 0) {
		if ((stb.st_mode & S_IFMT) == S_IFDIR)
			return;
		errno = ENOTDIR;
	}
	error("rcp: %s: %s.\n", cp, sys_errlist[errno]);
	exit(1);
}

char *
colon(cp)
	char *cp;
{

	while (*cp) {
		if (*cp == ':')
			return (cp);
		if (*cp == '/')
			return (0);
		cp++;
	}
	return (0);
}

okname(cp0)
	char *cp0;
{
	register char *cp = cp0;
	register int c;

	do {
		c = *cp;
		if (c & 0200)
			goto bad;
		if (!isalpha(c) && !isdigit(c) && c != '_' && c != '-')
			goto bad;
		cp++;
	} while (*cp);
	return (1);
bad:
	fprintf(stderr, "rcp: invalid user name %s\n", cp0);
	return (0);
}

susystem(s)
	char *s;
{
	int status, pid, w;
	register int (*istat)(), (*qstat)();

	if ((pid = vfork()) == 0) {
		(void) setuid(userid);
		execl("/bin/sh", "sh", "-c", s, (char *)0);
		_exit(127);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	return (status);
}

source(argc, argv)
	int argc;
	char **argv;
{
	char *last, *name;
	struct stat stb;
	static struct buffer buffer;
	struct buffer *bp;
	int x, readerr, f, amt;
	off_t i;
	char buf[BUFSIZ];

	for (x = 0; x < argc; x++) {
		name = argv[x];
		if ((f = open(name, O_NDELAY, 0)) < 0) {
			error("rcp: %s: %s\n", name, sys_errlist[errno]);
			continue;
		}
		if (fstat(f, &stb) < 0)
			goto notreg;
		switch (stb.st_mode&S_IFMT) {

		case S_IFREG:
			break;

		case S_IFDIR:
			if (iamrecursive) {
				(void) close(f);
				rsource(name, &stb);
				continue;
			}
			/* fall into ... */
		default:
notreg:
			(void) close(f);
			error("rcp: %s: not a plain file\n", name);
			continue;
		}
		last = rindex(name, '/');
		if (last == 0)
			last = name;
		else
			last++;
		if (pflag) {
			/*
			 * Make it compatible with possible future
			 * versions expecting microseconds.
			 */
			(void) sprintf(buf, "T%ld 0 %ld 0\n",
			    stb.st_mtime, stb.st_atime);
			(void) write(rem, buf, strlen(buf));
			if (response() < 0) {
				(void) close(f);
				continue;
			}
		}
		(void) sprintf(buf, "C%04o %ld %s\n",
		    stb.st_mode&07777, stb.st_size, last);
		(void) write(rem, buf, strlen(buf));
		if (response() < 0) {
			(void) close(f);
			continue;
		}
#ifdef RISCOS
		if ((bp = allocbuf(&buffer, f, BUFSIZ)) == NULL)
#else
		if ((bp = allocbuf(&buffer, f, BUFSIZ)) < 0)
#endif
		{
			(void) close(f);
			continue;
		}
		readerr = 0;
		for (i = 0; i < stb.st_size; i += bp->cnt) {
			amt = bp->cnt;
			if (i + amt > stb.st_size)
				amt = stb.st_size - i;
			if (readerr == 0 && read(f, bp->buf, amt) != amt)
				readerr = errno;
			(void) write(rem, bp->buf, amt);
		}
		(void) close(f);
		if (readerr == 0)
			ga();
		else
			error("rcp: %s: %s\n", name, sys_errlist[readerr]);
		(void) response();
	}
}

#include <sys/dir.h>

rsource(name, statp)
	char *name;
	struct stat *statp;
{
	DIR *d = opendir(name);
	char *last;
	struct direct *dp;
	char buf[BUFSIZ];
	char *bufv[1];

	if (d == 0) {
		error("rcp: %s: %s\n", name, sys_errlist[errno]);
		return;
	}
	last = rindex(name, '/');
	if (last == 0)
		last = name;
	else
		last++;
	if (pflag) {
		(void) sprintf(buf, "T%ld 0 %ld 0\n",
		    statp->st_mtime, statp->st_atime);
		(void) write(rem, buf, strlen(buf));
		if (response() < 0) {
			closedir(d);
			return;
		}
	}
	(void) sprintf(buf, "D%04o %d %s\n", statp->st_mode&07777, 0, last);
	(void) write(rem, buf, strlen(buf));
	if (response() < 0) {
		closedir(d);
		return;
	}
	while (dp = readdir(d)) {
		if (dp->d_ino == 0)
			continue;
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		if (strlen(name) + 1 + strlen(dp->d_name) >= BUFSIZ - 1) {
			error("%s/%s: Name too long.\n", name, dp->d_name);
			continue;
		}
		(void) sprintf(buf, "%s/%s", name, dp->d_name);
		bufv[0] = buf;
		source(1, bufv);
	}
	closedir(d);
	(void) write(rem, "E\n", 2);
	(void) response();
}

response()
{
	char resp, c, rbuf[BUFSIZ], *cp = rbuf;

	if (read(rem, &resp, 1) != 1)
		lostconn();
	switch (resp) {

	case 0:				/* ok */
		return (0);

	default:
		*cp++ = resp;
		/* fall into... */
	case 1:				/* error, followed by err msg */
	case 2:				/* fatal error, "" */
		do {
			if (read(rem, &c, 1) != 1)
				lostconn();
			*cp++ = c;
		} while (cp < &rbuf[BUFSIZ] && c != '\n');
		if (iamremote == 0)
			(void) write(2, rbuf, cp - rbuf);
		errs++;
		if (resp == 1)
			return (-1);
		exit(1);
	}
	/*NOTREACHED*/
}

lostconn()
{

	if (iamremote == 0)
		fprintf(stderr, "rcp: lost connection\n");
	exit(1);
}

sink(argc, argv)
	int argc;
	char **argv;
{
	off_t i, j;
	char *targ, *whopp, *cp;
	int of, mode, wrerr, exists, first, count, amt, size;
	struct buffer *bp;
	static struct buffer buffer;
	struct stat stb;
	int targisdir = 0;
	int mask = umask(0);
	char *myargv[1];
	char cmdbuf[BUFSIZ], nambuf[BUFSIZ];
	int setimes = 0;
	struct timeval tv[2];
#define atime	tv[0]
#define mtime	tv[1]
#define	SCREWUP(str)	{ whopp = str; goto screwup; }

	if (!pflag)
		(void) umask(mask);
	if (argc != 1) {
		error("rcp: ambiguous target\n");
		exit(1);
	}
	targ = *argv;
	if (targetshouldbedirectory)
		verifydir(targ);
	ga();
	if (stat(targ, &stb) == 0 && (stb.st_mode & S_IFMT) == S_IFDIR)
		targisdir = 1;
	for (first = 1; ; first = 0) {
		cp = cmdbuf;
		if (read(rem, cp, 1) <= 0)
			return;
		if (*cp++ == '\n')
			SCREWUP("unexpected '\\n'");
		do {
			if (read(rem, cp, 1) != 1)
				SCREWUP("lost connection");
		} while (*cp++ != '\n');
		*cp = 0;
		if (cmdbuf[0] == '\01' || cmdbuf[0] == '\02') {
			if (iamremote == 0)
				(void) write(2, cmdbuf+1, strlen(cmdbuf+1));
			if (cmdbuf[0] == '\02')
				exit(1);
			errs++;
			continue;
		}
		*--cp = 0;
		cp = cmdbuf;
		if (*cp == 'E') {
			ga();
			return;
		}

#define getnum(t) (t) = 0; while (isdigit(*cp)) (t) = (t) * 10 + (*cp++ - '0');
		if (*cp == 'T') {
			setimes++;
			cp++;
			getnum(mtime.tv_sec);
			if (*cp++ != ' ')
				SCREWUP("mtime.sec not delimited");
			getnum(mtime.tv_usec);
			if (*cp++ != ' ')
				SCREWUP("mtime.usec not delimited");
			getnum(atime.tv_sec);
			if (*cp++ != ' ')
				SCREWUP("atime.sec not delimited");
			getnum(atime.tv_usec);
			if (*cp++ != '\0')
				SCREWUP("atime.usec not delimited");
			ga();
			continue;
		}
		if (*cp != 'C' && *cp != 'D') {
			/*
			 * Check for the case "rcp remote:foo\* local:bar".
			 * In this case, the line "No match." can be returned
			 * by the shell before the rcp command on the remote is
			 * executed so the ^Aerror_message convention isn't
			 * followed.
			 */
			if (first) {
				error("%s\n", cp);
				exit(1);
			}
			SCREWUP("expected control record");
		}
		cp++;
		mode = 0;
		for (; cp < cmdbuf+5; cp++) {
			if (*cp < '0' || *cp > '7')
				SCREWUP("bad mode");
			mode = (mode << 3) | (*cp - '0');
		}
		if (*cp++ != ' ')
			SCREWUP("mode not delimited");
		size = 0;
		while (isdigit(*cp))
			size = size * 10 + (*cp++ - '0');
		if (*cp++ != ' ')
			SCREWUP("size not delimited");
		if (targisdir)
			(void) sprintf(nambuf, "%s%s%s", targ,
			    *targ ? "/" : "", cp);
		else
			(void) strcpy(nambuf, targ);
		exists = stat(nambuf, &stb) == 0;
		if (cmdbuf[0] == 'D') {
			if (exists) {
				if ((stb.st_mode&S_IFMT) != S_IFDIR) {
					errno = ENOTDIR;
					goto bad;
				}
				if (pflag)
					(void) chmod(nambuf, mode);
			} else if (mkdir(nambuf, mode) < 0)
				goto bad;
			myargv[0] = nambuf;
			sink(1, myargv);
			if (setimes) {
				setimes = 0;
				if (utimes(nambuf, tv) < 0)
					error("rcp: can't set times on %s: %s\n",
					    nambuf, sys_errlist[errno]);
			}
			continue;
		}
		if ((of = open(nambuf, O_WRONLY|O_CREAT, mode)) < 0) {
	bad:
			error("rcp: %s: %s\n", nambuf, sys_errlist[errno]);
			continue;
		}
		if (exists && pflag)
			(void) fchmod(of, mode);
		ga();
#ifdef RISCOS
		if ((bp = allocbuf(&buffer, of, BUFSIZ)) == NULL)
#else
		if ((bp = allocbuf(&buffer, of, BUFSIZ)) < 0) {
#endif
		{
			(void) close(of);
			continue;
		}
		cp = bp->buf;
		count = 0;
		wrerr = 0;
		for (i = 0; i < size; i += BUFSIZ) {
			amt = BUFSIZ;
			if (i + amt > size)
				amt = size - i;
			count += amt;
			do {
				j = read(rem, cp, amt);
				if (j <= 0) {
					if (j == 0)
					    error("rcp: dropped connection");
					else
					    error("rcp: %s\n",
						sys_errlist[errno]);
					exit(1);
				}
				amt -= j;
				cp += j;
			} while (amt > 0);
			if (count == bp->cnt) {
				if (wrerr == 0 &&
				    write(of, bp->buf, count) != count)
					wrerr++;
				count = 0;
				cp = bp->buf;
			}
		}
		if (count != 0 && wrerr == 0 &&
		    write(of, bp->buf, count) != count)
			wrerr++;
		if (ftruncate(of, size))
			error("rcp: can't truncate %s: %s\n",
			    nambuf, sys_errlist[errno]);
		(void) close(of);
		(void) response();
		if (setimes) {
			setimes = 0;
			if (utimes(nambuf, tv) < 0)
				error("rcp: can't set times on %s: %s\n",
				    nambuf, sys_errlist[errno]);
		}				   
		if (wrerr)
			error("rcp: %s: %s\n", nambuf, sys_errlist[errno]);
		else
			ga();
	}
screwup:
	error("rcp: protocol screwup: %s\n", whopp);
	exit(1);
}

struct buffer *
allocbuf(bp, fd, blksize)
	struct buffer *bp;
	int fd, blksize;
{
	struct stat stb;
	int size;

	if (fstat(fd, &stb) < 0) {
		error("rcp: fstat: %s\n", sys_errlist[errno]);
		return ((struct buffer *)-1);
	}
	size = roundup(stb.st_blksize, blksize);
	if (size == 0)
		size = blksize;
	if (bp->cnt < size) {
		if (bp->buf != 0)
			free(bp->buf);
		bp->buf = (char *)malloc((unsigned) size);
		if (bp->buf == 0) {
			error("rcp: malloc: out of memory\n");
			return ((struct buffer *)-1);
		}
	}
	bp->cnt = size;
	return (bp);
}

/*VARARGS1*/
error(fmt, a1, a2, a3, a4, a5)
	char *fmt;
	int a1, a2, a3, a4, a5;
{
	char buf[BUFSIZ], *cp = buf;

	errs++;
	*cp++ = 1;
	(void) sprintf(cp, fmt, a1, a2, a3, a4, a5);
	(void) write(rem, buf, strlen(buf));
	if (iamremote == 0)
		(void) write(2, buf+1, strlen(buf+1));
}

usage()
{
	fputs("usage: rcp [-p] f1 f2; or: rcp [-rp] f1 ... fn d2\n", stderr);
	exit(1);
}

#if RISCOS && OB_BSD42_COMPAT
/*
 * sethostuser
 *
 * info is string containing "host", "host.user", "host.domain" ,
 * "user@host", or "user@host.domain".
 * Set host to host part, user to user part, or to defuser if no
 * user is available.  Set dot if name was old style dotted name.
 *
 * Note that there is a conflict in syntax between BSD4.2 old style syntax
 * "host.user" and internet domain syntax "host.domain".  This routine
 * will interpret names with one dot in it (e.g. hhh.uuu) as 4.2 old style
 * syntax while names with more than one dot in it is interpreted as
 * internet domain name (e.g. dunkshot.mips.com).
 * If full internet domain name support is desired, that is, treats names
 * with only one dot (e.g mips.com) as host names, do not use this routine.
 */

sethostuser (info, host, user, defuser, dot)
register char *info, **host, **user, *defuser;
int *dot;
{
	register char *cp = info;
	register char *p;

	while (*cp && *cp != '@' && *cp != '.')
		cp++;
	
	if (! *cp)	/* host */
	{
		*host = info;
		*user = defuser;
	}
	else if (*cp == '@')	/* user@host, or user@host.domain */
	{
		*cp++ = '\0';
		*host = cp;
		*user = info;
	}
	else	/* *cp == '.' */	/* host.user, or host.domain */
	{
	  	for (p=cp+1; *p && *p != '.'; p++) ;
		if (! *p)
		{
			*cp++ = '\0';
			*user = cp;
			*host = info;
			*dot = 1;
	        }
		else
		{
		  	*host = info;
			*user = defuser;
		}
	}
	return 0;
}
#endif

#if RISCOS
setmyhost ()
{
	struct hostent *h;
	char buf[BUFSIZ];
	int i;

	if (gethostname (buf, sizeof buf) < 0)
	{
		error ("rcp: gethostname: %s\n", sys_errlist[errno]);
		exit (1);
	}

	sethostent (0);
	if ((h = gethostbyname (buf)) == NULL)
	{
		error ("rcp: gethostbyname (\"%s\") failed.\n", buf);
		endhostent ();
		exit (1);
	}

	/*
	 * Copy the hostname and the first MAXALIASES aliases into the
	 * save areas.
	 */

	(void) strcpy(myhostname, (*h).h_name);
	for (i = 0; (*h).h_aliases[i] && i < MAXALIASES; i++) {
		strcpy(myaliases[i], (*h).h_aliases[i]);
	}
	myaliases[i][0] = '\0';

	endhostent ();
}

/*
 * sourceistarget
 *
 * attempt to prevent rcp from copying a file onto itself by seeing
 * if both hosts are really the current host, and if so, if the files
 * are identical.
 */

sourceistarget (shost, sfile, dhost, dfile)
char *shost, *sfile, *dhost, *dfile;
{
	static char localhost[] = "localhost";
	struct stat sbuf1, sbuf2;
	int i;
	char buf[BUFSIZ], *cp, *rindex ();

	if (strcmp (shost, myhostname) == 0 || strcmp (shost, localhost) == 0)
		goto out1;
	else
		for (i = 0; myaliases[i][0]; i++)
			if (strcmp (shost, myaliases[i]) == 0)
				goto out1;
	return 0;

out1:
	/* source host is local, check destination host */
	if (strcmp (dhost, myhostname) == 0 || strcmp (dhost, localhost) == 0)
		goto out2;
	else
		for (i = 0; myaliases[i][0]; i++)
			if (strcmp (dhost, myaliases[i]) == 0)
				goto out2;
	return 0;

out2:
	/*
	 * At this point, the two hosts are the same, and
	 * they are actually the local host.
	 * See if the files are the same.
	 *
	 * This gets complicated because the destination could
	 * be a directory, so if it is, get file name part of the
	 * source, tack it onto the directory, and check that.
	 *
	 * If the stat call fails on the source file, play it safe
	 * and return 1, forcing the code which checks the status to skip
	 * copying this particular file. If it fails on the destination
	 * file, it does not exist yet, so the source and target are
	 * not the same file.
	 */

	if (stat (sfile, & sbuf1) < 0)
	{
		error ("rcp: %s: %s\n", sfile, sys_errlist[errno]);
		return 1;
	}

	if (stat (dfile, & sbuf2) < 0)	/* destination does not exist */
		return 0;
	else if ((sbuf2.st_mode & S_IFMT) == S_IFDIR &&
		(sbuf1.st_mode & S_IFMT) != S_IFDIR)	/* file to directory */
	{
		cp = rindex (sfile, '/');
		if (cp)
			cp++;
		else
			cp = sfile;
		sprintf (buf, "%s/%s", dfile, cp);
		if (stat (buf, & sbuf2) < 0)	/* target does not exist */
			return 0;
		else
			dfile = buf;	/* for error message, below */
	}

	if (sbuf1.st_dev == sbuf2.st_dev && sbuf1.st_ino == sbuf2.st_ino)
	{
		error ("rcp: %s:%s is the same file as %s:%s.\n",
				shost, sfile, dhost, dfile);
		return 1;
	}
	else
		return 0;
}
#endif /* RISCOS */

#if LOCAL_COPY

/* The following code is pulled in from BSD version "cp" program.  In
 * future, when /bin/cp " supports BSD "cp" semantics, one can define 
 * LOCAL_COPY to 0 to invoke /bin/cp instead of using this code.
 */

int	iflag = 0;

local_copy(from, to)
char	*from, *to;
{
	int status, pid, w;
	register int (*istat)(), (*qstat)();

	if ((pid = vfork()) == 0) {
		register int	mask, stat;
		(void) signal(SIGPIPE, SIG_DFL);
		(void) setuid(userid);
		if (pflag)
		    mask = umask(0);
		stat = copy(from, to);
		if (pflag)
		    (void)umask(mask);
		exit(stat);
	}
	istat = signal(SIGINT, SIG_IGN);
	qstat = signal(SIGQUIT, SIG_IGN);
	while ((w = wait(&status)) != pid && w != -1)
		;
	if (w == -1)
		status = -1;
	(void) signal(SIGINT, istat);
	(void) signal(SIGQUIT, qstat);
	return (status);
}

			/* I/O buffer; guarantee long-word alignment */
static char	cpbuf[MAXBSIZE];

copy(from, to)
	char *from, *to;
{
	int fold, fnew, n, exists;
	char *last, destname[MAXPATHLEN + 1];
	struct stat stfrom, stto;

	fold = open(from, 0);
	if (fold < 0) {
		Perror(from);
		return (1);
	}
	if (fstat(fold, &stfrom) < 0) {
		Perror(from);
		(void) close(fold);
		return (1);
	}
	if (stat(to, &stto) >= 0 &&
	   (stto.st_mode&S_IFMT) == S_IFDIR) {
		last = rindex(from, '/');
		if (last) last++; else last = from;
		if (strlen(to) + strlen(last) >= sizeof destname - 1) {
			fprintf(stderr, "rcp: %s/%s: Name too long", to, last);
			(void) close(fold);
			return(1);
		}
		(void) sprintf(destname, "%s/%s", to, last);
		to = destname;
	}
	if (iamrecursive && (stfrom.st_mode&S_IFMT) == S_IFDIR) {
		int fixmode = 0;	/* cleanup mode after rcopy */

		(void) close(fold);
		if (stat(to, &stto) < 0) {
			if (mkdir(to, (stfrom.st_mode & 07777) | 0700) < 0) {
				Perror(to);
				return (1);
			}
			fixmode = 1;
		} else if ((stto.st_mode&S_IFMT) != S_IFDIR) {
			fprintf(stderr, "rcp: %s: Not a directory.\n", to);
			return (1);
		} else if (pflag)
			fixmode = 1;
		n = rcopy(from, to);
		if (fixmode)
			(void) chmod(to, stfrom.st_mode & 07777);
		return (n);
	}

	if ((stfrom.st_mode&S_IFMT) == S_IFDIR)
		fprintf(stderr,
			"rcp: %s: Is a directory (copying as plain file).\n",
				from);

	exists = stat(to, &stto) == 0;
	if (exists) {
		if (stfrom.st_dev == stto.st_dev &&
		   stfrom.st_ino == stto.st_ino) {
			fprintf(stderr,
				"rcp: %s and %s are identical (not copied).\n",
					from, to);
			(void) close(fold);
			return (1);
		}
		if (iflag && isatty(fileno(stdin))) {
			int i, c;

			fprintf (stderr, "overwrite %s? ", to);
			i = c = getchar();
			while (c != '\n' && c != EOF)
				c = getchar();
			if (i != 'y') {
				(void) close(fold);
				return(1);
			}
		}
	}
	fnew = creat(to, stfrom.st_mode & 07777);
	if (fnew < 0) {
		Perror(to);
		(void) close(fold); return(1);
	}
	if (exists && pflag)
		(void) fchmod(fnew, stfrom.st_mode & 07777);
			
	for (;;) {
		n = read(fold, cpbuf, sizeof cpbuf);
		if (n == 0)
			break;
		if (n < 0) {
			Perror(from);
			(void) close(fold); (void) close(fnew); return (1);
		}
		if (write(fnew, cpbuf, n) != n) {
			Perror(to);
			(void) close(fold); (void) close(fnew); return (1);
		}
	}
	(void) close(fold); (void) close(fnew); 
	if (pflag)
		return (setimes(to, &stfrom));
	return (0);
}

rcopy(from, to)
	char *from, *to;
{
	DIR *fold = opendir(from);
	struct direct *dp;
	struct stat statb;
	int errs = 0;
	char fromname[MAXPATHLEN + 1];

	if (fold == 0 || (pflag && fstat(fold->dd_fd, &statb) < 0)) {
		Perror(from);
		return (1);
	}
	for (;;) {
		dp = readdir(fold);
		if (dp == 0) {
			closedir(fold);
			if (pflag)
				return (setimes(to, &statb) + errs);
			return (errs);
		}
		if (dp->d_ino == 0)
			continue;
		if (!strcmp(dp->d_name, ".") || !strcmp(dp->d_name, ".."))
			continue;
		if (strlen(from)+1+strlen(dp->d_name) >= sizeof fromname - 1) {
			fprintf(stderr, "rcp: %s/%s: Name too long.\n",
			    from, dp->d_name);
			errs++;
			continue;
		}
		(void) sprintf(fromname, "%s/%s", from, dp->d_name);
		errs += copy(fromname, to);
	}
}

int
setimes(path, statp)
	char *path;
	struct stat *statp;
{
	struct timeval tv[2];
	
	tv[0].tv_sec = statp->st_atime;
	tv[1].tv_sec = statp->st_mtime;
	tv[0].tv_usec = tv[1].tv_usec = 0;
	if (utimes(path, tv) < 0) {
		Perror(path);
		return (1);
	}
	return (0);
}

Perror(s)
	char *s;
{

	fprintf(stderr, "rcp: ");
	perror(s);
}
#endif /* LOCAL_COPY */
