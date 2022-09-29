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
#ident	"$Header: umount.c,v 1.13.1.4 90/05/09 19:27:51 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * @(#)umount.c	1.2 88/03/15 4.0NFSSRC
 *	from 5.1 (Berkeley) 5/28/85
 */


/*
 * umount
 */

#include <sys/param.h>
#include <sys/file.h>
#include <sys/wait.h>
#include <stdio.h>
#include <mntent.h>
#include <errno.h>
#include <sys/time.h>
#include <rpc/rpc.h>
#define bsd43_bool_t bool_t
#include <nfs/nfs.h>
#include <rpcsvc/mount.h>
#include <sys/socket.h>
#include <netdb.h>

#define PRE_RISCOS	/* changes made before risc/os 4.50 */

/*
 * This structure is used to build a list of mntent structures
 * in reverse order from /etc/mtab.
 */
struct mntlist {
	struct mntent *mntl_mnt;
	struct mntlist *mntl_next;
};

int	all = 0;
#if defined PRE_RISCOS
#define	KILLSLEEP	5
int	do_kill = 0;
#endif
int	verbose = 0;
int	host = 0;
int	type = 0;

char	*typestr;
char	*hoststr;

char	*xmalloc();
char	*index();
char	*realpath() ;
struct mntlist *mkmntlist();
struct mntent *mntdup();

int eachreply();

extern	int errno;

main(argc, argv)
	int argc;
	char **argv;
{
	char *options;

#ifdef PRE_RISCOS
	if (geteuid() != 0) {
		fprintf(stderr, "Must be root to use umount\n");
		exit(1);
	}
#endif
	argc--, argv++;
	sync();
	umask(0);
	while (argc && *argv[0] == '-') {
		options = &argv[0][1];
		while (*options) {
			switch (*options) {
			case 'a':
				all++;
				break;
			case 'd':	/* For RFS SVID compatibility */
				break;
			case 'h':
				all++;
				host++;
				hoststr = argv[1];
				argc--;
				argv++;
				break;
#ifdef KILLSLEEP
			case 'k':
				do_kill++;
				break;
#endif
			case 't':
				all++;
				type++;
				typestr = argv[1];
				argc--;
				argv++;
				break;
			case 'v':
				verbose++;
				break;
			default:
				fprintf(stderr, "umount: unknown option '%c'\n",
				    *options);
				usage();
			}
			options++;
		}
		argv++;
		argc--;
	}

	if ((all && argc) || (!all && !argc)){
		usage();
	}

	if (geteuid() != 0) {
		fprintf(stderr, "Must be root to use umount\n");
		exit(1);
	}

	umountlist(argc, argv);
	exit(0) ;
}

#ifdef PRE_RISCOS
unsigned char	interrupted = 0;

catch(sig)
	int sig;
{
	interrupted = 1;
	(void) signal(sig, catch);
}

#define	HANDLE(handler) { \
	(void) signal(SIGHUP, handler); \
	(void) signal(SIGINT, handler); \
	(void) signal(SIGTERM, handler); \
}
#endif /* PRE_RISCOS */

umountlist(argc, argv)
	int argc;
	char *argv[];
{
	int i, pid;
	struct mntent *mnt;
	struct mntlist *mntl;
	struct mntlist *mntcur = NULL;
	struct mntlist *mntrev = NULL;
	int tmpfd;
	char *colon;
	FILE *tmpmnt;
	char *tmpname = "/etc/umountXXXXXX";

#ifdef PRE_RISCOS
	HANDLE(catch);
#endif
	mktemp(tmpname);
	if ((tmpfd = open(tmpname, O_RDWR|O_CREAT|O_TRUNC, 0644)) < 0) {
		perror(tmpname);
		exit(1);
	}
	close(tmpfd);
	tmpmnt = setmntent(tmpname, "w");
	if (tmpmnt == NULL) {
		perror(tmpname);
		exit(1);
	}
	if (all) {
		if (!host &&
		    (!type || (type && strcmp(typestr, MNTTYPE_NFS) == 0))) {
			pid = fork();
			if (pid < 0)
				perror("umount: fork");
			if (pid == 0) {
				endmntent(tmpmnt);
				clnt_broadcast(MOUNTPROG,
				    MOUNTVERS, MOUNTPROC_UMNTALL,
				    xdr_void, NULL, xdr_void, NULL, eachreply);
				exit(0);
			}
		}
	}
	/*
	 * get a last first list of mounted stuff, reverse list and
	 * null out entries that get unmounted.
	 */
	for (mntl = mkmntlist(); mntl != NULL;
	    mntcur = mntl, mntl = mntl->mntl_next,
	    mntcur->mntl_next = mntrev, mntrev = mntcur) {
		mnt = mntl->mntl_mnt;
		if (strcmp(mnt->mnt_dir, "/") == 0) {
			continue;
		}
		if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0) {
			continue;
		}
		if (all) {
			if (type && !strcmp(typestr, "local")) {
				if (!strcmp(mnt->mnt_type, MNTTYPE_NFS) ||
				    !strcmp(mnt->mnt_type, MNTTYPE_RFS))
					continue;
			} else if (type && strcmp(typestr, mnt->mnt_type)) {
				continue;
			}
			if (host) {
				if (strcmp(MNTTYPE_NFS, mnt->mnt_type)) {
					continue;
				}
				colon = index(mnt->mnt_fsname, ':');
				if (colon) {
					*colon = '\0';
					if (strcmp(hoststr, mnt->mnt_fsname)) {
						*colon = ':';
						continue;
					}
					*colon = ':';
				} else {
					continue;
				}
			}
			if (umountmnt(mnt)) {
				mntl->mntl_mnt = NULL;
			}
			continue;
		}

		for (i=0; i<argc; i++) {
		   if (*argv[i]) {
			if ((strcmp(mnt->mnt_dir, argv[i]) == 0) ||
			    (strcmp(mnt->mnt_fsname, argv[i]) == 0)) {
				if (umountmnt(mnt)) {
					mntl->mntl_mnt = NULL;
				}
				*argv[i] = '\0';
				break;
			}
		    }
		}
	}

	for (i = 0 ; i < argc ; i++) {
	    if (*argv[i]) break ;
	}
	if (i < argc) {
	    struct mntlist *temp, *previous;
	    int oldi ;
	    oldi = i ;
	    /*
	     * Now find those arguments which are links, resolve them
	     * and then umount them. This is done separately because it
	     * "stats" the arg, and it may hang if the server is down.
	     */
	    previous = NULL ;
	    for (; mntcur; temp = mntcur, mntcur = mntcur->mntl_next,
		 temp->mntl_next = previous, previous = temp) ;

	    mntrev = NULL ;
	    for (mntl = previous; mntl != NULL;
		mntcur = mntl, mntl = mntl->mntl_next,
		mntcur->mntl_next = mntrev, mntrev = mntcur) {
		if ((mnt = mntl->mntl_mnt)==NULL) 
			continue; /* Already unmounted */
		if (strcmp(mnt->mnt_dir, "/") == 0)
			continue;
		if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0)
			continue;
		for (i=oldi; i<argc; i++) {
		    char resolvebuf[MAXPATHLEN] ;
		    char *resolve = resolvebuf ;

		    if (*argv[i]) {
			if (realpath(argv[i], resolve) == 0) {
				fprintf(stderr, "umount: ") ;
				perror(resolve) ;
				*argv[i] = NULL ;
				continue ;
			}
			if ((strcmp(mnt->mnt_dir, resolve) == 0)) {
				if (umountmnt(mnt)) {
					mntl->mntl_mnt = NULL;
				}
				*argv[i] = '\0';
				break;
			}
		    }
		}
	    }
	    /*
	     * For all the remaining ones including the ones which have
	     * no corresponding entry in /etc/mtab file.
	     */
	    for (i=oldi; i<argc; i++) {
		if (*argv[i] && *argv[i] != '/') {
			fprintf(stderr,
			    "umount: must use full path, %s not unmounted\n",
			    argv[i]);
			continue;
		}
		if (*argv[i]) {
			struct mntent tmpmnt;

			tmpmnt.mnt_fsname = NULL;
			tmpmnt.mnt_dir = argv[i];
#ifdef PRE_RISCOS
			/* XXX type doesn't matter: oh yeah? who said? */
			tmpmnt.mnt_type = MNTTYPE_FFS;
#else
			tmpmnt.mnt_type = MNTTYPE_43;
#endif /* PRE_RISCOS */
			(void) umountmnt(&tmpmnt);
		}
	    }
	}

	/*
	 * Build new temporary mtab by walking mnt list
	 */
	for (; mntcur != NULL; mntcur = mntcur->mntl_next) {
		if (mntcur->mntl_mnt) {
			addmntent(tmpmnt, mntcur->mntl_mnt);
		}
	}
	endmntent(tmpmnt);

	/*
	 * Move tmp mtab to real mtab
	 */
	if (rename(tmpname, MOUNTED) < 0) {
		perror(MOUNTED);
		exit(1);
	}
}

#ifdef PRE_RISCOS
#include <setjmp.h>
jmp_buf	context;

jump(sig)
	int sig;
{
	catch(sig);
	longjmp(context, 1);
}
#endif /* PRE_RISCOS */

umountmnt(mnt)
	struct mntent *mnt;
{
#ifdef PRE_RISCOS
  	int	first_time = 1;
	int	was_mounted = 1;

	if (interrupted)
		return 0;
#endif
	if (unmount(mnt->mnt_dir) < 0) {	/* could call SysV umount() */
#ifdef KILLSLEEP
		if (errno == EBUSY &&
		    first_time &&
		    do_kill) {
			char cmd[MNTMAXSTR];

			first_time = 0;
			if (verbose) {
				sprintf(cmd, "/etc/fuser -k %s", mnt->mnt_dir);
			} else {
				sprintf(cmd, "2>&1 /etc/fuser -k %s > /dev/null",
					mnt->mnt_dir);
			}
			(void) system(cmd);
			sleep(KILLSLEEP);
		}
#endif
		if (errno != EINVAL) {
			perror(mnt->mnt_dir);
			return(0);
		}
		fprintf(stderr, "%s not mounted\n", mnt->mnt_dir);
#ifdef PRE_RISCOS
		was_mounted = 0;
	} /* the next line is indented funny to preserve old indentation */
		if (strcmp(mnt->mnt_type, MNTTYPE_NFS) == 0) {
			if (!setjmp(context)) {
				HANDLE(jump);
				(void) rpctoserver(mnt);
				HANDLE(catch);
			}
#else /* !PRE_RISCOS */
		return(1);
	} else {
		if (strcmp(mnt->mnt_type, MNTTYPE_NFS) == 0) {
			rpctoserver(mnt);
#endif /* PRE_RISCOS */
		} else if (strcmp(mnt->mnt_type, MNTTYPE_43) == 0) {
#ifdef PCFS
		} else if (strcmp(mnt->mnt_type, MNTTYPE_PC) == 0) {
#endif
		} else {
			char umountcmd[128];
			int pid;
			union wait status;

			/*
			 * user-level filesystem...attempt to exec
			 * external umount_FS program.
			 */
			pid = fork();
			switch (pid) {
			case -1:
				fprintf(stderr, "umount: ");
				perror(mnt->mnt_fsname);
				break;
			case 0:
				sprintf(umountcmd, "umount_%s", mnt->mnt_type);
				(void) execlp(umountcmd, umountcmd,
				    mnt->mnt_fsname, mnt->mnt_dir,
				    mnt->mnt_type, mnt->mnt_opts, 0);
				/*
				 * Don't worry if there is no user-written
				 * umount_FS program.
				 */
				exit(1);
			default:
				while (wait(&status) != pid);
				/*
				 * Ignore errors in user-written umount_FS.
				 */
			}
		}
#ifdef PRE_RISCOS
		if (verbose && was_mounted) {
#else
		if (verbose) {
#endif
			fprintf(stderr, "%s: Unmounted\n", mnt->mnt_dir);
		}
		return(1);
#ifndef PRE_RISCOS
	}
#endif
}

usage()
{
#ifdef KILLSLEEP
	fprintf(stderr, "usage: umount -a[kv] [-t <type>] [-h <host>]\n");
	fprintf(stderr, "       umount [-kv] <path> | <dev> ...\n");
#else
	fprintf(stderr, "usage: umount -a[v] [-t <type>] [-h <host>]\n");
	fprintf(stderr, "       umount [-v] <path> | <dev> ...\n");
#endif
	exit(1);
}

rpctoserver(mnt)
	struct mntent *mnt;
{
	char *p;
	struct sockaddr_in sin;
	struct hostent *hp;
	int s;
	struct timeval timeout;
	CLIENT *client;
	enum clnt_stat rpc_stat;

	if ((p = index(mnt->mnt_fsname, ':')) == NULL)
		return;
	*p++ = 0;
	if ((hp = gethostbyname(mnt->mnt_fsname)) == NULL) {
		fprintf(stderr, "%s not in hosts database\n", mnt->mnt_fsname);
		return;
	}
	bzero(&sin, sizeof(sin));
	bcopy(hp->h_addr, (char *) & sin.sin_addr, hp->h_length);
	sin.sin_family = AF_INET;
	s = RPC_ANYSOCK;
	timeout.tv_usec = 0;
	timeout.tv_sec = 10;
	if ((client = clntudp_create(&sin, MOUNTPROG, MOUNTVERS,
	    timeout, &s)) == NULL) {
		clnt_pcreateerror("Warning: umount");
		return;
	}
	client->cl_auth = authunix_create_default();
	timeout.tv_usec = 0;
	timeout.tv_sec = 25;
	rpc_stat = clnt_call(client, MOUNTPROC_UMNT, xdr_path, &p,
	    xdr_void, NULL, timeout);
	if (rpc_stat != RPC_SUCCESS) {
		clnt_perror(client, "Warning: umount");
#ifdef PRE_RISCOS
		clnt_destroy(client);
		close(s);
#endif
		return;
	}
#ifdef PRE_RISCOS
	clnt_destroy(client);
	close(s);
#endif
}

/*ARGSUSED*/
eachreply(resultsp, addrp)
	char *resultsp;
	struct sockaddr_in *addrp;
{

	return (1);
}

char *
xmalloc(size)
	int size;
{
	char *ret;

	if ((ret = (char *)malloc(size)) == NULL) {
		fprintf(stderr, "umount: ran out of memory!\n");
		exit(1);
	}
	return (ret);
}

struct mntent *
mntdup(mnt)
	struct mntent *mnt;
{
	struct mntent *new;

	new = (struct mntent *)xmalloc(sizeof(*new));

	new->mnt_fsname = (char *)xmalloc(strlen(mnt->mnt_fsname) + 1);
	strcpy(new->mnt_fsname, mnt->mnt_fsname);

	new->mnt_dir = (char *)xmalloc(strlen(mnt->mnt_dir) + 1);
	strcpy(new->mnt_dir, mnt->mnt_dir);

	new->mnt_type = (char *)xmalloc(strlen(mnt->mnt_type) + 1);
	strcpy(new->mnt_type, mnt->mnt_type);

	new->mnt_opts = (char *)xmalloc(strlen(mnt->mnt_opts) + 1);
	strcpy(new->mnt_opts, mnt->mnt_opts);

	new->mnt_freq = mnt->mnt_freq;
	new->mnt_passno = mnt->mnt_passno;

	return (new);
}

struct mntlist *
mkmntlist()
{
	FILE *mounted;
	struct mntlist *mntl;
	struct mntlist *mntst = NULL;
	struct mntent *mnt;

	mounted = setmntent(MOUNTED, "r");
	if (mounted == NULL) {
		perror(MOUNTED);
		exit(1);
	}
	while ((mnt = getmntent(mounted)) != NULL) {
		mntl = (struct mntlist *)xmalloc(sizeof(*mntl));
		mntl->mntl_mnt = mntdup(mnt);
		mntl->mntl_next = mntst;
		mntst = mntl;
	}
	endmntent(mounted);
	return(mntst);
}
