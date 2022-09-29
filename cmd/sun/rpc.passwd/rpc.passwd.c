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
#ident	"$Header: rpc.passwd.c,v 1.8.2.5 90/05/09 19:21:31 wje Exp $"

#define sgi	1

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <signal.h>
#ifdef sgi
#include <sys/types.h>
#include <ufs/inode.h>
#endif
#include <sys/file.h>
#include <rpc/rpc.h>
#include <pwd.h>
#include <rpcsvc/yppasswd.h>
#include <errno.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/wait.h>
#ifdef RISCOS
#include <syslog.h>
#endif

struct passwd * getpwnam();

char *File;			/* File in which passwd's are found */
char	temp[]	 = "/etc/ptmp";	/* lockfile for modifying 'File' */
int mflag;			/* do a make */

char *index();
int boilerplate();
extern int errno;
int Argc;
char **Argv;

main(argc, argv)
	char **argv;
{
	SVCXPRT *transp;
	int s;

#ifdef sgi
	/* protect ourselves from the SYSTEM V init daemon */
	(void) signal(SIGHUP, SIG_IGN);
#endif
	
#ifdef RISCOS
	openlog("rpc.passwd", LOG_PID, LOG_DAEMON);
#endif
	Argc = argc;
	Argv = argv;
	if (argc < 2) {
	    	fprintf(stderr,"usage: %s file [-m arg1 arg2 ...]\n", argv[0]);
		exit(1);
	}
	File = argv[1];
	if (access(File, W_OK) < 0) {
#ifdef RISCOS
		syslog(LOG_ERR, "can't write %s\n", File);
#else
		fprintf(stderr, "can't write %s\n", File);
#endif
		exit(1);
	}
	if (argc > 2 && argv[2][0] == '-' && argv[2][1] == 'm')
		mflag++;
#ifdef sgi		
	if (chdir("/usr/etc/yp") < 0) {
#ifdef RISCOS
		syslog(LOG_ERR, "can't chdir to /usr/etc/yp\n");
#else
		fprintf(stderr, "yppasswdd: can't chdir to /usr/etc/yp\n");
#endif
		exit(1);
	}
#else
	if (chdir("/etc/yp") < 0) {
#ifdef RISCOS
                syslog(LOG_ERR, "can't chdir to /etc/yp\n");
#else
                fprintf(stderr, "yppasswdd: can't chdir to /etc/yp\n");
#endif
		exit(1);
	}
#endif

	if ((s = rresvport()) < 0) {
#ifdef RISCOS
		syslog(LOG_ERR,
		    "can't bind to a privileged socket\n");
#else
		fprintf(stderr,
		    "yppasswdd: can't bind to a privileged socket\n");
#endif
		exit(1);
	}
	transp = svcudp_create(s);
	if (transp == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"couldn't create an RPC server\n");
#else
		fprintf(stderr, "yppasswdd: couldn't create an RPC server\n");
#endif
		exit(1);
	}
	pmap_unset(YPPASSWDPROG, YPPASSWDVERS);
	if (!svc_register(transp, YPPASSWDPROG, YPPASSWDVERS,
	    boilerplate, IPPROTO_UDP)) {
#ifdef RISCOS
		syslog(LOG_ERR,"couldn't register yppasswdd\n");
#else
		fprintf(stderr, "yppasswdd: couldn't register yppasswdd\n");
#endif
		exit(1);
	}

#ifndef DEBUG
	if (fork())
		exit(0);
	{ int t;
	for (t = getdtablesize()-1; t >= 0; t--)
		if (t != s)
			(void) close(t);
	}
	(void) open("/", O_RDONLY);
	(void) dup2(0, 1);
	(void) dup2(0, 2);
	{ int tt = open("/dev/tty", O_RDWR);
	  if (tt > 0) {
		ioctl(tt, TIOCNOTTY, 0);
		close(tt);
	  }
	}
#endif
#ifdef sgi
	/*
	 * Ignore SIGCHLD so we never get zombies that we have to
	 * wait for.
	 */
	(void) signal(SIGCHLD, SIG_IGN);
#endif

	svc_run();
#ifdef RISCOS
	syslog(LOG_ERR,"svc_run shouldn't have returned\n");
#else
	fprintf(stderr, "yppasswdd: svc_run shouldn't have returned\n");
#endif
}

boilerplate(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	switch(rqstp->rq_proc) {
		case NULLPROC:
			if (!svc_sendreply(transp, xdr_void, 0))
#ifdef RISCOS
				syslog(LOG_ERR,"couldn't reply to RPC call\n");
#else
				fprintf(stderr,
				    "yppasswdd: couldn't reply to RPC call\n");
#endif
			break;
		case YPPASSWDPROC_UPDATE:
			changepasswd(rqstp, transp);
			break;
	}
}

changepasswd(rqstp, transp)
	struct svc_req *rqstp;
	SVCXPRT *transp;
{
	int tempfd, i, len;
	static int ans;
	FILE *tempfp, *filefp, *fp;
	char buf[256], *p;
	char cmdbuf[BUFSIZ];
#ifdef SYSTYPE_BSD43
	int (*f1)(), (*f2)(), (*f3)();
#else
	void (*f1)(), (*f2)(), (*f3)();
#endif
	struct passwd *oldpw, *newpw;
	struct yppasswd yppasswd;
	union wait status;
	
	bzero(&yppasswd, sizeof(yppasswd));
	if (!svc_getargs(transp, xdr_yppasswd, &yppasswd)) {
		svcerr_decode(transp);
		return;
	}
	/* 
	 * Clean up from previous changepasswd() call
	 */
#ifndef sgi
	while (wait3(&status, WNOHANG, 0) > 0)
		continue;
#endif

	newpw = &yppasswd.newpw;
	ans = 1;
	oldpw = getpwnam(newpw->pw_name);
	if (oldpw == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"no passwd for %s\n", newpw->pw_name);
#else
		fprintf(stderr, "yppasswdd: no passwd for %s\n",
		    newpw->pw_name);
#endif
		goto done;
	}
	if (oldpw->pw_passwd && *oldpw->pw_passwd &&
	    strcmp(crypt(yppasswd.oldpass,oldpw->pw_passwd),
	    oldpw->pw_passwd) != 0) {
#ifdef RISCOS
		syslog(LOG_ERR, "%s: bad passwd\n", newpw->pw_name);
#else
		fprintf(stderr, "yppasswdd: %s: bad passwd\n", newpw->pw_name);
#endif
		goto done;
	}
	(void) umask(0);

	f1 = signal(SIGHUP, SIG_IGN);
	f2 = signal(SIGINT, SIG_IGN);
	f3 = signal(SIGQUIT, SIG_IGN);
	tempfd = open(temp, O_WRONLY|O_CREAT|O_EXCL, 0644);
	if (tempfd < 0) {
#ifndef RISCOS
		fprintf(stderr, "yppasswdd: ");
#endif
		if (errno == EEXIST)
#ifdef RISCOS
			syslog(LOG_ERR,"password file busy - try again.\n");
#else
			fprintf(stderr, "password file busy - try again.\n");
#endif
		else
#ifdef RISCOS
			syslog(LOG_ERR,"Error = %m\n");
#else
			perror(temp);
#endif
		goto cleanup;
	}
#ifndef sgi
	(void)signal(SIGTSTP, SIG_IGN);
#endif
	if ((tempfp = fdopen(tempfd, "w")) == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"fdopen failed?\n");
#else
		fprintf(stderr, "yppasswdd: fdopen failed?\n");
#endif
		goto cleanup;
	}
	/*
	 * Copy passwd to temp, replacing matching lines
	 * with new password.
	 */
	if ((filefp = fopen(File, "r")) == NULL) {
#ifdef RISCOS
		syslog(LOG_ERR,"fopen of %s failed?\n", File);
#else
		fprintf(stderr, "yppasswdd: fopen of %s failed?\n", File);
#endif
		goto cleanup;
	}
	len = strlen(newpw->pw_name);
	while (fgets(buf, sizeof(buf), filefp)) {
		p = index(buf, ':');
		if (p && p - buf == len
		    && strncmp(newpw->pw_name, buf, p - buf) == 0) {
			fprintf(tempfp,"%s:%s:%d:%d:%s:%s:%s\n",
			    oldpw->pw_name,
			    newpw->pw_passwd,
			    oldpw->pw_uid,
			    oldpw->pw_gid,
			    oldpw->pw_gecos,
			    oldpw->pw_dir,
			    oldpw->pw_shell);
		}
		else
			fputs(buf, tempfp);
	}
	fclose(filefp);
	fclose(tempfp);
	if (rename(temp, File) < 0) {
#ifdef RISCOS
		syslog(LOG_ERR,"rename - %m\n");
#else
		fprintf(stderr, "yppasswdd: "); perror("rename");
#endif
		unlink(temp);
		goto cleanup;
	}
	ans = 0;
#ifdef sgi
	/*
	 * Reset SIGCHLD status to SIG_DFL before forking, so that
	 * descendants of this process (e.g. the shell) can wait for kids.
	 */
	(void) signal(SIGCHLD, SIG_DFL);
#endif
	if (mflag && fork() == 0) {
		strcpy(cmdbuf, "make");
		for (i = 3; i < Argc; i++) {
			strcat(cmdbuf, " ");
			strcat(cmdbuf, Argv[i]);
		}
#ifdef DEBUG
		fprintf(stderr, "about to execute %s\n", cmdbuf);
#endif
		system(cmdbuf);
		exit(0);
	}
    cleanup:
#ifdef sgi
	/*
	 * Reset SIGCHLD status to SIG_IGN in the parent process, so
	 * that zombies will not be created.
	 */
	(void) signal(SIGCHLD, SIG_IGN);
#endif
	fclose(tempfp);
	(void)signal(SIGHUP, f1);
	(void)signal(SIGINT, f2);
	(void)signal(SIGQUIT, f3);
    done:
	if (!svc_sendreply(transp, xdr_int, &ans))
#ifdef RISCOS
		syslog(LOG_ERR,"couldnt reply to RPC call\n");
#else
		fprintf(stderr, "yppasswdd: couldnt reply to RPC call\n");
#endif
}

rresvport()
{
	struct sockaddr_in sin;
	int s, alport = IPPORT_RESERVED - 1;

	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = 0;
	s = socket(AF_INET, SOCK_DGRAM, 0, 0);
	if (s < 0)
		return (-1);
	for (;;) {
		sin.sin_port = htons((u_short)alport);
		if (bind(s, (caddr_t)&sin, sizeof (sin), 0) >= 0)
			return (s);
		if (errno != EADDRINUSE && errno != EADDRNOTAVAIL) {
#ifdef RISCOS
			syslog(LOG_ERR,"socket - %m\n");
#else
			perror("socket");
#endif
			return (-1);
		}
		(alport)--;
		if (alport == IPPORT_RESERVED/2) {
#ifdef RISCOS
			syslog(LOG_ERR, "socket: All ports in use\n");
#else
			fprintf(stderr, "socket: All ports in use\n");
#endif
			return (-1);
		}
	}
}

static char *
pwskip(p)
register char *p;
{
	while( *p && *p != ':' && *p != '\n' )
		++p;
	if( *p ) *p++ = 0;
	return(p);
}

struct passwd *
getpwnam(name)
	char *name;
{
	FILE *pwf;
	int cnt;
	char *p;
	static char line[BUFSIZ+1];
	static struct passwd passwd;
	
	pwf = fopen(File, "r");
	if (pwf == NULL)
		return (NULL);
	cnt = strlen(name);
	while ((p = fgets(line, BUFSIZ, pwf)) && strncmp(name, line, cnt))
		continue;
	if (p) {
		passwd.pw_name = p;
		p = pwskip(p);
		passwd.pw_passwd = p;
		p = pwskip(p);
		passwd.pw_uid = atoi(p);
		p = pwskip(p);
		passwd.pw_gid = atoi(p);
#ifndef sgi
		passwd.pw_quota = 0;
#endif
		passwd.pw_comment = "";
		p = pwskip(p);
		passwd.pw_gecos = p;
		p = pwskip(p);
		passwd.pw_dir = p;
		p = pwskip(p);
		passwd.pw_shell = p;
		while(*p && *p != '\n') p++;
		*p = '\0';
		fclose(pwf);
		return (&passwd);
	}
	else {
		fclose(pwf);
		return (NULL);
	}
}
