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
#ident	"$Header: mount.c,v 1.14.1.8 90/05/09 19:13:52 wje Exp $"

/*
 * @(#)mount.c	1.4 88/06/02 4.0NFSSRC; from 2.61 88/02/07 SMI
 * Copyr 1985 Sun Micro
 *
 * Copyright (c) 1985 Sun Microsystems, Inc.
 */

#define	NFSCLIENT
/*
 * mount
 */
#include <ctype.h>
#include <strings.h>
#include <sys/param.h>
#include <rpc/rpc.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/vfs.h>
#define bsd43_bool_t bool_t
#include <nfs/nfs.h>
#include <rpcsvc/mount.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <netdb.h>
#include <stdio.h>
#include <mntent.h>
#include <sys/mount.h>
#include <sys/wait.h>

#if defined(RISCOS)
/*
 * Previous changes from pre-NFS 4.0.  Was incorporated under the SVR3
 * macro in previous releases.  See the revision history.
 */
#define PRE_RISCOS
#endif

int	ro = 0;
#if !defined(RISCOS)
int	quota = 0;	/* this is now meaningless */
#endif
int	fake = 0;
int	freq = 1;
int	passno = 2;
int	all = 0;
int	verbose = 0;
int	printed = 0;
int	nomtab = 0;
#if defined(PRE_RISCOS)
int	check = 0;
#endif



#define	NRETRY	10000	/* number of times to retry a mount request */
#define	BGSLEEP	5	/* initial sleep time for background mount in seconds */
#define MAXSLEEP 120	/* max sleep time for background mount in seconds */
/*
 * Fake errno for RPC failures that don't map to real errno's
 */
#define ERPC	(10000)

extern int errno;

extern char	*realpath();

static void	replace_opts();
static void	printmtab();

static char	host[MNTMAXSTR];
static char	name[MNTMAXSTR];
static char	dir[MNTMAXSTR];
static char	type[MNTMAXSTR];
static char	opts[MNTMAXSTR];
static char	netname[MAXNETNAMELEN+1];

/*
 * Structure used to build a mount tree.  The tree is traversed to do
 * the mounts and catch dependencies.
 */
struct mnttree {
	struct mntent *mt_mnt;
	struct mnttree *mt_sib;
	struct mnttree *mt_kid;
};
static struct mnttree *maketree();

main(argc, argv)
	int argc;
	char **argv;
{
	struct mntent mnt;
	struct mntent *mntp;
	FILE *mnttab;
	char *options;
	struct mnttree *mtree;

	if (argc == 1) {
		mnttab = setmntent(MOUNTED, "r");
		while ((mntp = getmntent(mnttab)) != NULL) {
			if (strcmp(mntp->mnt_type, MNTTYPE_IGNORE) == 0) {
				continue;
			}
			printent(mntp);
		}
		(void) endmntent(mnttab);
		exit(0);
	}

	(void) close(2);
	if (dup2(1, 2) < 0) {
		perror("dup");
		exit(1);
	}

	opts[0] = '\0';
	type[0] = '\0';

	/*
	 * Set options
	 */
	while (argc > 1 && argv[1][0] == '-') {
		options = &argv[1][1];
		while (*options) {
			switch (*options) {
			case 'a':
				all++;
				break;
#if defined(PRE_RISCOS)
			case 'c':
				check++;
				break;
#endif
			case 'd':	/* For RFS SVID compatibility */
				(void) strcpy(type, "rfs");
				break;
			case 'f':
				fake++;
				break;
			case 'o':
				if (argc < 3) {
					usage();
				}
				(void) strcpy(opts, argv[2]);
				argv++;
				argc--;
				break;
			case 'n':
				nomtab++;
				break;
			case 'p':
				if (argc != 2) {
					usage();
				}
				printmtab(stdout);
				exit(0);
#if !defined(RISCOS)
			case 'q':
				quota++;
				break;
#endif
			case 'r':
				ro++;
				break;
			case 't':
				if (argc < 3) {
					usage();
				}
				(void) strcpy(type, argv[2]);
				argv++;
				argc--;
				break;
			case 'v':
				verbose++;
				break;
			default:
				(void) fprintf(stderr,
				    "mount: unknown option: %c\n", *options);
				usage();
			}
			options++;
		}
		argc--, argv++;
	}

	if (geteuid() != 0) {
		(void) fprintf(stderr, "Must be root to use mount\n");
		exit(1);
	}

	if (all) {
		struct stat mnttab_stat;
		long mnttab_size;
		int count;

		if (argc != 1) {
			usage();
		}
		mnttab = setmntent(MNTTAB, "r");
		if (mnttab == NULL) {
			(void) fprintf(stderr, "mount: ");
			perror(MNTTAB);
			exit(1);
		}
		if (fstat(fileno(mnttab), &mnttab_stat) == -1) {
			(void) fprintf(stderr, "mount: ");
			perror(MNTTAB);
			exit(1);
		}
		mnttab_size = mnttab_stat.st_size;
		mtree = NULL;

		for (count = 1 ; ; count++) {
		        if ((mntp = getmntent(mnttab)) == NULL) {
				if (ftell(mnttab) >= mnttab_size)
					break;		/* it's EOF */
				(void) fprintf(stderr, 
				    "mount: %s: illegal entry on line %d\n",
				    MNTTAB, count);
				continue;
		        }
			if ((strcmp(mntp->mnt_type, MNTTYPE_IGNORE) == 0) ||
			    (strcmp(mntp->mnt_type, MNTTYPE_SWAP) == 0) ||
			    hasmntopt(mntp, MNTOPT_NOAUTO) ||
#if defined RISCOS
			    hasmntopt(mntp, MNTOPT_HIDE) ||
#endif
			    (strcmp(mntp->mnt_dir, "/") == 0) ) {
				continue;
			}
#if defined(RISCOS)
			if (type[0] != '\0' && !strcmp(type, "local") &&
			    ! (strcmp(mntp->mnt_type, MNTTYPE_NFS) == 0 ||
			       strcmp(mntp->mnt_type, MNTTYPE_RFS) == 0))
			  /* do nothing */;
			else if (type[0] != '\0' &&
#else
			if (type[0] != '\0' &&
#endif
			    strcmp(mntp->mnt_type, type) != 0) {
				continue;
			}
#if defined(RISCOS)
			/* Always add noquota to mtab(4).  quotaon changes it.
			 * Add NFS file system, so /usr/ucb/quota and /bin/login
			 * are fast by default when lots of NFS file systems
			 * are mounted. */
			if (IS_RISCOS_FS_TYPE(mntp->mnt_type) ||
			    !strcmp(mntp->mnt_type, MNTTYPE_NFS))
				replace_opts(mntp->mnt_opts, TRUE,
				    MNTOPT_NOQUOTA, MNTOPT_QUOTA);
#endif
			mtree = maketree(mtree, mntp);
		}
		(void) endmntent(mnttab);
#if defined PRE_RISCOS
		exit(mounttree(mtree));
#else
		mounttree(mtree);
		exit(0);
#endif
	}

	/*
	 * Command looks like: mount <dev>|<dir>
	 * we walk through /etc/fstab til we match either fsname or dir.
	 */
	if (argc == 2) {
		struct stat mnttab_stat;
		long mnttab_size;
		int count;

		mnttab = setmntent(MNTTAB, "r");
		if (mnttab == NULL) {
			(void) fprintf(stderr, "mount: ");
			perror(MNTTAB);
			exit(1);
		}
		if (fstat(fileno(mnttab), &mnttab_stat) == -1) {
			(void) fprintf(stderr, "mount: ");
			perror(MNTTAB);
			exit(1);
		}
		mnttab_size = mnttab_stat.st_size;

		for (count = 1;; count++) {
			if ((mntp = getmntent(mnttab)) == NULL) {
				if (ftell(mnttab) >= mnttab_size)
					break;		/* it's EOF */
				(void) fprintf(stderr, 
				    "mount: %s: illegal entry on line %d\n",
				    MNTTAB, count);
				continue;			   
		        }
		   	if ((strcmp(mntp->mnt_type, MNTTYPE_IGNORE) == 0) ||
			    (strcmp(mntp->mnt_type, MNTTYPE_SWAP) == 0)) {
				continue;
			}
			if ((strcmp(mntp->mnt_fsname, argv[1]) == 0) ||
			    (strcmp(mntp->mnt_dir, argv[1]) == 0) ) {
				if (opts[0] != '\0') {
					/*
					 * "-o" specified; override fstab with
					 * command line options, unless it's
					 * "-o remount", in which case do
					 * nothing if the fstab says R/O (you
					 * can't remount from R/W to R/O, and
					 * remounting from R/O to R/O is not
					 * only invalid but pointless).
					 */
					if (strcmp(opts, MNTOPT_REMOUNT) == 0
					  && hasmntopt(mntp, MNTOPT_RO))
						exit(0);
					mntp->mnt_opts = opts;
				}
				replace_opts(mntp->mnt_opts, ro, MNTOPT_RO,
				    MNTOPT_RW);
#if defined(RISCOS)
				/* Always add noquota to mtab(4).  quotaon adds
				 * the quota option depending on the setting
				 * in fstab(4).  Add NFS file systems, so
				 * /usr/ucb/quota & /bin/login are fast by
				 * default when lots of NFS file systems are
				 * mounted.  */
				if (IS_RISCOS_FS_TYPE(mntp->mnt_type)    ||
				    IS_RISCOS_FS_TYPE(type)              ||
				    !strcmp(mntp->mnt_type, MNTTYPE_NFS) ||
				    !strcmp(type, MNTTYPE_NFS))
					replace_opts(mntp->mnt_opts, TRUE,
					    MNTOPT_NOQUOTA, MNTOPT_QUOTA);
#else
				/* these semantics are wacko because
				 * quotaon modifies the sense.  mount(1M)
				 * should always put it in a state of
				 * noquota.  */
				if (strcmp(type, MNTTYPE_43) == 0)
					replace_opts(mntp->mnt_opts, quota,
					    MNTOPT_QUOTA, MNTOPT_NOQUOTA);
#endif /* RISCOS */
#if defined PRE_RISCOS
				exit(mounttree(maketree((struct mnttree *)NULL,
							mntp)));
#else
				mounttree(maketree((struct mnttree *)NULL,
				    mntp));
				exit(0);
#endif
			}
		}
		(void) fprintf(stderr, "mount: %s not found in %s\n", argv[1],
		    MNTTAB);
		exit(1);
	}

	if (argc != 3) {
		usage();
	}
	if (realpath(argv[2], dir) == NULL) {
		(void) fprintf(stderr, "mount: ");
		perror(dir);
		exit(1);
	}
	(void) strcpy(name, argv[1]);

	/*
	 * If the file system type is not given then
	 * assume "nfs" if the name is of the form
	 *     host:path
	 * otherwise assume "4.3".
	 */
	if (type[0] == '\0')
		(void) strcpy(type,
#if defined(RISCOS)
		    index(name, ':') ? MNTTYPE_NFS : MNTTYPE_FFS);
#else
		    index(name, ':') ? MNTTYPE_NFS : MNTTYPE_43);
#endif

	if (dir[0] != '/') {
		(void) fprintf(stderr,
		    "mount: directory path must begin with '/'\n");
		exit(1);
	}

	replace_opts(opts, ro, MNTOPT_RO, MNTOPT_RW);
#if defined(RISCOS)
	/* Add NFS file system, so /usr/ucb/quota and /bin/login run fast
	 * by default when there are lots of NFS mounted file systems. */
	if (IS_RISCOS_FS_TYPE(type) || !strcmp(type, MNTTYPE_NFS))
		replace_opts(opts, TRUE, MNTOPT_NOQUOTA, MNTOPT_QUOTA);
#else
	if (strcmp(type, MNTTYPE_43) == 0)
		replace_opts(opts, quota, MNTOPT_QUOTA, MNTOPT_NOQUOTA);
#endif /* RISCOS */

	if (strcmp(type, MNTTYPE_NFS) == 0) {
		passno = 0;
		freq = 0;
	}

	mnt.mnt_fsname = name;
	mnt.mnt_dir = dir;
	mnt.mnt_type = type;
	mnt.mnt_opts = opts;
	mnt.mnt_freq = freq;
	mnt.mnt_passno = passno;
#if defined PRE_RISCOS
	exit(mounttree(maketree((struct mnttree *)NULL, &mnt)));
#else
	mounttree(maketree((struct mnttree *)NULL, &mnt));
#endif
}


/*
 * attempt to mount file system, return errno or 0
 */
int
mountfs(print, mnt, opts)
	int print;
	struct mntent *mnt;
	char *opts;
{
	int error;
	extern int errno;
	int flags = 0;
	union data {
		struct ufs_args	ufs_args;
		struct nfs_args nfs_args;
#ifdef PCFS
		struct pc_args pc_args;
#endif
#if defined(RISCOS)
		struct proc_args proc_args;
#endif
	} data;

	if (hasmntopt(mnt, MNTOPT_REMOUNT) == 0) {
		if (mounted(mnt)) {
			if (print && verbose) {
				(void) fprintf(stderr,
				    "mount: %s already mounted\n",
				    mnt->mnt_fsname);
			}
			return (0);
		}
	} else
		if (print && verbose)
			(void) fprintf (stderr,
			    "mountfs: remount ignoring mtab\n");
	if (fake) {
		addtomtab(mnt);
		return (0);
	}

#if defined(RISCOS)
	if (IS_RISCOS_FS_TYPE(mnt->mnt_type)) {
#else
	if (strcmp(mnt->mnt_type, MNTTYPE_43) == 0) {
#endif
#if defined(PRE_RISCOS)
		if (check && (error=checkfs(mnt)) != 0)
			return(error);
#endif
		error = mount_43(mnt, &data.ufs_args);
	} else if (strcmp(mnt->mnt_type, MNTTYPE_NFS) == 0) {
		error = mount_nfs(mnt, &data.nfs_args);
#ifdef PCFS
	} else if (strcmp(mnt->mnt_type, MNTTYPE_PC) == 0) {
		error = mount_pc(mnt, &data.pc_args);
#endif
#if defined(RISCOS)
	} else if (strcmp(mnt->mnt_type, MNTTYPE_PROC) == 0) {
		error = mount_proc(mnt, &data.proc_args);
#endif
	} else {
		/*
		 * Invoke "mount" command for particular file system type.
		 */
		char mountcmd[128];
		int pid;
		union wait status;

		pid = fork();
		switch (pid) {

		case -1:
			(void) fprintf(stderr,
			    "mount: %s on ", mnt->mnt_fsname);
			perror(mnt->mnt_dir);
			return (errno);
		case 0:
			(void) sprintf(mountcmd, "mount_%s", mnt->mnt_type);
			execlp(mountcmd, mountcmd, mnt->mnt_fsname,
			    mnt->mnt_dir, mnt->mnt_type, mnt->mnt_opts,
			    (char *)NULL);
			if (errno == ENOENT) {
				(void) fprintf(stderr,
				    "mount: unknown filesystem type: %s\n",
				    mnt->mnt_type);
			} else if (print) {
				(void) fprintf(stderr, "%s: %s on %s ",
				    mountcmd, mnt->mnt_fsname,
				    mnt->mnt_dir);
				perror("exec failed");
			}
			exit(errno);
		default:
			while (wait(&status) != pid);
			error = status.w_retcode;
			if (!error) {
				*mnt->mnt_opts = '\0';
				goto itworked;
			}
		}
	}
	if (error) {
		return (error);
	}

#if defined(RISCOS)
	flags |= eatmntopt(mnt, MNTOPT_NFSSYNC) ? M_NFSSYNC : 0;
	flags |= eatmntopt(mnt, MNTOPT_NFSASYNC) ? M_NFSASYNC : 0;
	if ((flags & M_NFSSYNC) && (flags & M_NFSASYNC)) {
		fprintf(stderr,"mount: Can't use both nfs_sync & nfs_async options\n");
		return (EINVAL);
	}
#endif
	flags |= eatmntopt(mnt, MNTOPT_RO) ? M_RDONLY : 0;
	flags |= eatmntopt(mnt, MNTOPT_NOSUID) ? M_NOSUID : 0;
	flags |= eatmntopt(mnt, MNTOPT_GRPID) ? M_GRPID : 0;
	flags |= eatmntopt(mnt, MNTOPT_REMOUNT) ? M_REMOUNT : 0;
	flags |= M_NEWTYPE;

	if (mount(mnt->mnt_type, mnt->mnt_dir, flags, &data) < 0) {
		if (errno == ENODEV) {
			/*
			 * might be an old kernel, need to try again
			 * with file type number instead of string.
#ifdef RISCOS
			 * NOTE: In looking through old kernel and mount code
			 * it appears that we never supported the below mount
			 * interface as it was always SysV().  The order is
			 * important as it matches what old systems believe.
			 * (but we never used or supported it before! Oh well!)
			 * Yes, we have other types, but if you want to access
			 * them, use the other interface.
#endif
			 */
#define	MAXTYPE 3
			int typeno;
			static char *typetab[] =
			    { MNTTYPE_43, MNTTYPE_NFS, MNTTYPE_PC };

			for (typeno = 0; typeno < MAXTYPE; typeno++) {
				if (strcmp(typetab[typeno], mnt->mnt_type) == 0)
					break;
			}
			if (typeno < MAXTYPE) {
				error = errno;
				if (mount(typeno, mnt->mnt_dir, flags, &data)
				    >= 0) {
					goto itworked;
				}
				errno = error;  /* restore original error */
			}
		}
		if (print) {
			(void) fprintf(stderr, "mount: %s on ",
			    mnt->mnt_fsname);
#if defined PRE_RISCOS
			if (errno == ENOSPC) {
				fprintf(stderr,
			"%s: dirty filesystem, clean with fsck or mount -c\n",
				    mnt->mnt_dir);
			} else {
				perror(mnt->mnt_dir);
			}
#else
			perror(mnt->mnt_dir);
#endif /* PRE_RISCOS */
		}
		return (errno);
	}

itworked:
	fixopts(mnt, opts);
	if (*opts) {
		(void) fprintf(stderr,
		    "mount: %s on %s WARNING unknown options %s\n",
		    mnt->mnt_fsname, mnt->mnt_dir, opts);
	}
	addtomtab(mnt);
	return (0);
}	/* end of mountfs */


#if defined PRE_RISCOS
int checkfs(mnt)
	struct mntent *mnt;
{
	int status;
	char cmd[MNTMAXSTR];
	char *ckname;
	char *strchr();

	sprintf(cmd, "2>&1 /etc/fsstat %s > /dev/null", mnt->mnt_fsname);
	status = system(cmd);
	if (status >> 8 != 1) {
		return;		/* filesystem clean or invalid */
	}
	if (strcmp(mnt->mnt_fsname, "/")
	    && (ckname = hasmntopt(mnt, MNTOPT_RAW)) != NULL
	    && (ckname = strchr(ckname, '=')) != NULL) {
		ckname++;	/* advance to option rhs */
	} else {
		ckname = mnt->mnt_fsname;
	}
	sprintf(cmd, "/etc/fsck -p -d -T %s %s", mnt->mnt_type, ckname);
	return(system(cmd));
}	/* end of checkfs */
#endif /* PRE_RISCOS */


mount_43(mnt, args)
	struct mntent *mnt;
	struct ufs_args *args;
{
	static char name[MNTMAXSTR];

	(void) strcpy(name, mnt->mnt_fsname);
	args->fspec = name;
	return (0);
}

#if defined(RISCOS)
mount_proc(mnt, args)
	struct mntent *mnt;
	struct proc_args *args;
{
	static char name[MNTMAXSTR];

	(void) strcpy(name, mnt->mnt_fsname);
	args->fspec = name;
	return (0);
}
#endif

mount_nfs(mnt, args)
	struct mntent *mnt;
	struct nfs_args *args;
{
	static struct sockaddr_in sin;
	struct hostent *hp;
	static struct fhstatus fhs;
	char *cp;
	char *hostp = host;
	char *path;
	int s;
	struct timeval timeout;
	CLIENT *client;
	enum clnt_stat rpc_stat;
	u_short port;

	cp = mnt->mnt_fsname;
	while ((*hostp = *cp) != ':') {
		if (*cp == '\0') {
			(void) fprintf(stderr,
			    "mount: nfs file system; use host:path\n");
			return (1);
		}
		hostp++;
		cp++;
	}
	*hostp = '\0';
	path = ++cp;
	/*
	 * Get server's address
	 */
	if ((hp = gethostbyname(host)) == NULL) {
		/*
		 * XXX
		 * Failure may be due to yellow pages, try again
		 */
		if ((hp = gethostbyname(host)) == NULL) {
			(void) fprintf(stderr,
			    "mount: %s not in hosts database\n", host);
			return (1);
		}
	}

	args->flags = 0;
	if (eatmntopt(mnt, MNTOPT_SOFT)) {
		args->flags |= NFSMNT_SOFT;
	}
	if (eatmntopt(mnt, MNTOPT_INTR)) {
		args->flags |= NFSMNT_INT;
	}
	if (eatmntopt(mnt, MNTOPT_SECURE)) {
		args->flags |= NFSMNT_SECURE;
	}
	if (eatmntopt(mnt, "noac")) {
		args->flags |= NFSMNT_NOAC;
	}

	/*
	 * get fhandle of remote path from server's mountd
	 */
	bzero((char *)&sin, sizeof(sin));
	bcopy(hp->h_addr, (char *) &sin.sin_addr, hp->h_length);
	sin.sin_family = AF_INET;
	timeout.tv_usec = 0;
	timeout.tv_sec = 20;	/* but not used */
	s = RPC_ANYSOCK;

	if ((client = clntudp_create(&sin, (u_long)MOUNTPROG,
	    (u_long)MOUNTVERS, timeout, &s)) == NULL) {
		if (!printed) {
			(void) fprintf(stderr,
			    "mount: %s server not responding",
			    mnt->mnt_fsname);
			clnt_pcreateerror("");
			printed = 1;
		}
		return (ETIMEDOUT);
	}
	client->cl_auth = authunix_create_default();
	timeout.tv_usec = 0;
	timeout.tv_sec = 20;
	rpc_stat = clnt_call(client, MOUNTPROC_MNT, xdr_path, &path,
	    xdr_fhstatus, &fhs, timeout);
	errno = 0;
	if (rpc_stat != RPC_SUCCESS) {
		if (!printed) {
			(void) fprintf(stderr,
			    "mount: %s server not responding",
			    mnt->mnt_fsname);
			clnt_perror(client, "");
			printed = 1;
		}
		switch (rpc_stat) {
		case RPC_TIMEDOUT:
		case RPC_PMAPFAILURE:
		case RPC_PROGNOTREGISTERED:
			errno = ETIMEDOUT;
			break;
		case RPC_AUTHERROR:
			errno = EACCES;
			break;
		default:
			errno = ERPC;
			break;
		}
	}
	clnt_destroy(client);
	if (errno) {
		return(errno);
	}

	if (errno = fhs.fhs_status) {
		if (errno == EACCES) {
			(void) fprintf(stderr,
			    "mount: access denied for %s:%s\n", host, path);
		} else {
			(void) fprintf(stderr, "mount: ");
			perror(mnt->mnt_fsname);
		}
		return (errno);
	}
	if (printed) {
		(void) fprintf(stderr, "mount: %s server ok\n",
		    mnt->mnt_fsname);
		printed = 0;
	}

	/*
	 * set mount args
	 */
	args->fh = (caddr_t)&fhs.fhs_fh;
	args->hostname = host;
	args->flags |= NFSMNT_HOSTNAME;
	if (args->rsize = nopt(mnt, "rsize")) {
		args->flags |= NFSMNT_RSIZE;
	}
	if (args->wsize = nopt(mnt, "wsize")) {
		args->flags |= NFSMNT_WSIZE;
	}
	if (args->timeo = nopt(mnt, "timeo")) {
		args->flags |= NFSMNT_TIMEO;
	}
	if (args->retrans = nopt(mnt, "retrans")) {
		args->flags |= NFSMNT_RETRANS;
	}
	if (args->acregmin = nopt(mnt, "actimeo")) {
		args->acregmax = args->acregmin;
		args->acdirmin = args->acregmin;
		args->acdirmax = args->acregmin;
		args->flags |= NFSMNT_ACREGMIN;
		args->flags |= NFSMNT_ACREGMAX;
		args->flags |= NFSMNT_ACDIRMIN;
		args->flags |= NFSMNT_ACDIRMAX;
	} else {
		if (args->acregmin = nopt(mnt, "acregmin")) {
			args->flags |= NFSMNT_ACREGMIN;
		}
		if (args->acregmax = nopt(mnt, "acregmax")) {
			args->flags |= NFSMNT_ACREGMAX;
		}
		if (args->acdirmin = nopt(mnt, "acdirmin")) {
			args->flags |= NFSMNT_ACDIRMIN;
		}
		if (args->acdirmax = nopt(mnt, "acdirmax")) {
			args->flags |= NFSMNT_ACDIRMAX;
		}
	}
	if (args->flags & NFSMNT_SECURE) {
		/*
		 * XXX: need to support other netnames outside domain
		 * and not always just use the default conversion
		 */
		if (!host2netname(netname, host, (char *)NULL)) {
			return (EHOSTDOWN); /* really unknown host */
		}
		args->netname = netname;
	}
	if (port = nopt(mnt, "port")) {
		sin.sin_port = htons(port);
	} else {
		sin.sin_port = htons(NFS_PORT);	/* XXX should use portmapper */
	}
	args->addr = &sin;

	/*
	 * should clean up mnt ops to not contain defaults
	 */
	return (0);
}

#ifdef PCFS
mount_pc(mnt, args)
	struct mntent *mnt;
	struct pc_args *args;
{
	args->fspec = mnt->mnt_fsname;
	return (0);
}
#endif

printent(mnt)
	struct mntent *mnt;
{
	(void) fprintf(stdout, "%s on %s type %s (%s)\n",
	    mnt->mnt_fsname, mnt->mnt_dir, mnt->mnt_type, mnt->mnt_opts);
}

static void
printmtab(outp)
	FILE *outp;
{
	FILE *mnttab;
	struct mntent *mntp;
	int maxfsname = 0;
	int maxdir = 0;
	int maxtype = 0;
	int maxopts = 0;

	/*
	 * first go through and find the max width of each field
	 */
	mnttab = setmntent(MOUNTED, "r");
	while ((mntp = getmntent(mnttab)) != NULL) {
		if (strlen(mntp->mnt_fsname) > maxfsname) {
			maxfsname = strlen(mntp->mnt_fsname);
		}
		if (strlen(mntp->mnt_dir) > maxdir) {
			maxdir = strlen(mntp->mnt_dir);
		}
		if (strlen(mntp->mnt_type) > maxtype) {
			maxtype = strlen(mntp->mnt_type);
		}
		if (strlen(mntp->mnt_opts) > maxopts) {
			maxopts = strlen(mntp->mnt_opts);
		}
	}
	(void) endmntent(mnttab);

	/*
	 * now print them oput in pretty format
	 */
	mnttab = setmntent(MOUNTED, "r");
	while ((mntp = getmntent(mnttab)) != NULL) {
		(void) fprintf(outp, "%-*s", maxfsname+1, mntp->mnt_fsname);
		(void) fprintf(outp, "%-*s", maxdir+1, mntp->mnt_dir);
		(void) fprintf(outp, "%-*s", maxtype+1, mntp->mnt_type);
		(void) fprintf(outp, "%-*s", maxopts+1, mntp->mnt_opts);
		(void) fprintf(outp, " %d %d\n", mntp->mnt_freq,
		    mntp->mnt_passno);
	}
	(void) endmntent(mnttab);
}

/*
 * Check to see if mntck is already mounted.
 * We have to be careful because getmntent modifies its static struct.
 */
mounted(mntck)
	struct mntent *mntck;
{
	int found = 0;
	struct mntent *mnt, mntsave;
	FILE *mnttab;
#if defined(RISCOS)
	struct mntent mntsave2;
#endif

	if (nomtab) {
		return (0);
	}
	mnttab = setmntent(MOUNTED, "r");
	if (mnttab == NULL) {
		(void) fprintf(stderr, "mount: ");
		perror(MOUNTED);
		exit(1);
	}
	mntcp(mntck, &mntsave);
	while ((mnt = getmntent(mnttab)) != NULL) {
		if (strcmp(mnt->mnt_type, MNTTYPE_IGNORE) == 0) {
			continue;
		}
#if defined(RISCOS)
		/* Need the following code to pull out the 'retry' option
		 * before comparing.  The mounttree() routine pulls out
		 * this option before calling mountfs().
		 */
		mntcp2(mnt, &mntsave2);
		if ((strcmp(mntsave.mnt_fsname, mntsave2.mnt_fsname) == 0) &&
		    (strcmp(mntsave.mnt_dir, mntsave2.mnt_dir) == 0) &&
		    (strcmp(mntsave.mnt_opts, mntsave2.mnt_opts) == 0) ) {
			found = 1;
			break;
		}
#else
		if ((strcmp(mntsave.mnt_fsname, mnt->mnt_fsname) == 0) &&
		    (strcmp(mntsave.mnt_dir, mnt->mnt_dir) == 0) &&
		    (strcmp(mntsave.mnt_opts, mnt->mnt_opts) == 0) ) {
			found = 1;
			break;
		}
#endif
	}
	(void) endmntent(mnttab);
	*mntck = mntsave;
	return (found);
}

#if defined(RISCOS)
mntcp2(mnt1, mnt2)
	struct mntent *mnt1, *mnt2;
{
	static char fsname[128], dir[128], type[128], opts[128];

	mnt2->mnt_fsname = fsname;
	(void) strcpy(fsname, mnt1->mnt_fsname);
	mnt2->mnt_dir = dir;
	(void) strcpy(dir, mnt1->mnt_dir);
	mnt2->mnt_type = type;
	(void) strcpy(type, mnt1->mnt_type);
	mnt2->mnt_opts = opts;
	(void) strcpy(opts, mnt1->mnt_opts);
	rmopt(mnt2, "retry");
	if (IS_RISCOS_FS_TYPE(type) || !strcmp(type, MNTTYPE_NFS))
		replace_opts(opts, TRUE, MNTOPT_NOQUOTA, MNTOPT_QUOTA);
	mnt2->mnt_freq = mnt1->mnt_freq;
	mnt2->mnt_passno = mnt1->mnt_passno;
}
#endif

mntcp(mnt1, mnt2)
	struct mntent *mnt1, *mnt2;
{
	static char fsname[128], dir[128], type[128], opts[128];

	mnt2->mnt_fsname = fsname;
	(void) strcpy(fsname, mnt1->mnt_fsname);
	mnt2->mnt_dir = dir;
	(void) strcpy(dir, mnt1->mnt_dir);
	mnt2->mnt_type = type;
	(void) strcpy(type, mnt1->mnt_type);
	mnt2->mnt_opts = opts;
	(void) strcpy(opts, mnt1->mnt_opts);
	mnt2->mnt_freq = mnt1->mnt_freq;
	mnt2->mnt_passno = mnt1->mnt_passno;
}

/*
 * Return the value of a numeric option of the form foo=x, if
 * option is not found or is malformed, return 0.
 */
nopt(mnt, opt)
	struct mntent *mnt;
	char *opt;
{
	int val = 0;
	char *equal;
	char *str;

	if (str = hasmntopt(mnt, opt)) {
		if (equal = index(str, '=')) {
			val = atoi(&equal[1]);
		} else {
			(void) fprintf(stderr,
			    "mount: bad numeric option '%s'\n", str);
		}
	}
	rmopt(mnt, opt);
	return (val);
}

/*
 * same as hasmntopt but remove the option from the option string and return
 * true or false
 */
eatmntopt(mnt, opt)
	struct mntent *mnt;
	char *opt;
{
	int has;

	has = (hasmntopt(mnt, opt) != NULL);
	rmopt(mnt, opt);
	return (has);
}

/*
 * remove an option string from the option list
 */
rmopt(mnt, opt)
	struct mntent *mnt;
	char *opt;
{
	char *str;
	char *optstart;

	if (optstart = hasmntopt(mnt, opt)) {
		for (str = optstart; *str != ',' && *str != '\0'; str++)
			;
		if (*str == ',') {
			str++;
		} else if (optstart != mnt->mnt_opts) {
			optstart--;
		}
		while (*optstart++ = *str++)
			;
	}
}

/*
 * mnt->mnt_ops has un-eaten opts, opts is the original opts list.
 * Set mnt->mnt_opts to the original list minus the un-eaten opts.
 * Set "opts" to the un-eaten opts minus the "default" options ("rw", "quota",
 * "hard", "noquota", "noauto") and "bg".  If there are any options left after
 * this, they are uneaten because they are unknown; our caller will print a
 * warning message.
 */
fixopts(mnt, opts)
	struct mntent *mnt;
	char *opts;
{
	char *comma;
	char *ue;
	char uneaten[1024];

	rmopt(mnt, MNTOPT_RW);
	rmopt(mnt, MNTOPT_HARD);
#if defined(RISCOS)			/* fixed NFS 4.0 bug: 1012018 */
	rmopt(mnt, MNTOPT_QUOTA);
	rmopt(mnt, "fg");
	rmopt(mnt, "retry");		/* fixed NFS 4.0 bug: 1012860 */
#endif
	rmopt(mnt, MNTOPT_NOQUOTA);
	rmopt(mnt, MNTOPT_NOAUTO);
#if defined RISCOS
	rmopt(mnt, MNTOPT_HIDE);
#endif
	rmopt(mnt, "bg");
	(void) strcpy(uneaten, mnt->mnt_opts);
	(void) strcpy(mnt->mnt_opts, opts);
	(void) strcpy(opts, uneaten);

	for (ue = uneaten; *ue; ) {
		for (comma = ue; *comma != '\0' && *comma != ','; comma++)
			;
		if (*comma == ',') {
			*comma = '\0';
			rmopt(mnt, ue);
			ue = comma+1;
		} else {
			rmopt(mnt, ue);
			ue = comma;
		}
	}
	if (*mnt->mnt_opts == '\0') {
		(void) strcpy(mnt->mnt_opts, MNTOPT_RW);
	}
}	/* end of fixopts */

/*
 * update /etc/mtab
 */
addtomtab(mnt)
	struct mntent *mnt;
{
	FILE *mnted;

	if (nomtab) {
		return;
	}
	mnted = setmntent(MOUNTED, "r+");
	if (mnted == NULL) {
		(void) fprintf(stderr, "mount: ");
		perror(MOUNTED);
		exit(1);
	}
	if (addmntent(mnted, mnt)) {
		(void) fprintf(stderr, "mount: ");
		perror(MOUNTED);
		exit(1);
	}
	(void) endmntent(mnted);

	if (verbose) {
		(void) fprintf(stdout, "%s mounted on %s\n",
		    mnt->mnt_fsname, mnt->mnt_dir);
	}
}

char *
xmalloc(size)
	int size;
{
	char *ret;
	
	if ((ret = (char *)malloc((unsigned)size)) == NULL) {
		(void) fprintf(stderr, "umount: ran out of memory!\n");
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
	(void) strcpy(new->mnt_fsname, mnt->mnt_fsname);

	new->mnt_dir = (char *)xmalloc(strlen(mnt->mnt_dir) + 1);
	(void) strcpy(new->mnt_dir, mnt->mnt_dir);

	new->mnt_type = (char *)xmalloc(strlen(mnt->mnt_type) + 1);
	(void) strcpy(new->mnt_type, mnt->mnt_type);

	new->mnt_opts = (char *)xmalloc(strlen(mnt->mnt_opts) + 1);
	(void) strcpy(new->mnt_opts, mnt->mnt_opts);

	new->mnt_freq = mnt->mnt_freq;
	new->mnt_passno = mnt->mnt_passno;

	return (new);
}

/*
 * Build the mount dependency tree
 */
static struct mnttree *
maketree(mt, mnt)
	struct mnttree *mt;
	struct mntent *mnt;
{
	if (mt == NULL) {
		mt = (struct mnttree *)xmalloc(sizeof (struct mnttree));
		mt->mt_mnt = mntdup(mnt);
		mt->mt_sib = NULL;
		mt->mt_kid = NULL;
	} else {
		if (substr(mt->mt_mnt->mnt_dir, mnt->mnt_dir)) {
			mt->mt_kid = maketree(mt->mt_kid, mnt);
		} else {
			mt->mt_sib = maketree(mt->mt_sib, mnt);
		}
	}
	return (mt);
}

printtree(mt)
	struct mnttree *mt;
{
	if (mt) {
		printtree(mt->mt_sib);
		(void) printf("   %s\n", mt->mt_mnt->mnt_dir);
		printtree(mt->mt_kid);
	}
}

mounttree(mt)
	struct mnttree *mt;
{
	int error;
	int slptime;
	int forked;
	int retry;
	int firsttry;
	int background;
#if defined(PRE_RISCOS)
	int status = 0;
#endif

	if (mt) {
		char opts[1024];
#if defined(PRE_RISCOS)
		/* the following is used to make sure the default of 
		 * retry=0 is implemented.  The original code would
		 * hang (almost) forever.
		 */
		u_char bgenabled = (hasmntopt(mt->mt_mnt, "bg") != 0);

		if ((error = mounttree(mt->mt_sib)) != 0)
			status = error;
#else
		mounttree(mt->mt_sib);
#endif /* PRE_RISCOS */
		(void) strcpy(opts, mt->mt_mnt->mnt_opts);
		forked = 0;
		printed = 0;
		firsttry = 1;
		slptime = BGSLEEP;
		retry = nopt(mt->mt_mnt, "retry");
#if defined(PRE_RISCOS)
		if (retry == 0 && bgenabled)
			retry = NRETRY;
#else
		if (retry == 0) {
			retry = NRETRY;
		}
#endif /* PRE_RISCOS */

		do {
			error = mountfs(!forked, mt->mt_mnt, opts);
			if (error != ETIMEDOUT && error != ENETDOWN &&
			    error != ENETUNREACH && error != ENOBUFS &&
			    error != ECONNREFUSED && error != ECONNABORTED) {
				break;
			}
			/*
			 * mount failed due to network problems, reset options
			 * string and retry (maybe)
			 */
			(void) strcpy(mt->mt_mnt->mnt_opts, opts);
			background = eatmntopt(mt->mt_mnt, "bg");
			if (!forked && background) {
				(void) fprintf(stderr,
				    "mount: backgrounding\n");
				(void) fprintf(stderr, "   %s\n",
				    mt->mt_mnt->mnt_dir);
				printtree(mt->mt_kid);
				if (fork()) {
#if defined(PRE_RISCOS)
					return (0);
#else
					return;
#endif
				} else {
					forked = 1;
				}
			}
#if defined(PRE_RISCOS)
			if (!forked && firsttry && retry) {
#else
			if (!forked && firsttry) {
#endif
				(void) fprintf(stderr, "mount: retrying\n");
				(void) fprintf(stderr, "   %s\n",
				    mt->mt_mnt->mnt_dir);
				printtree(mt->mt_kid);
				firsttry = 0;
			}
			sleep((unsigned)slptime);
			slptime = MIN(slptime << 1, MAXSLEEP);
		} while (retry--);

		if (!error) {
#if defined(PRE_RISCOS)			
			if ((error = mounttree(mt->mt_kid)) != 0)
				status = error;
#else
			mounttree(mt->mt_kid);
#endif
		} else {
#if defined(PRE_RISCOS)
			status = error;
#endif
			(void) fprintf(stderr, "mount: giving up on:\n");
			(void) fprintf(stderr, "   %s\n", mt->mt_mnt->mnt_dir);
			printtree(mt->mt_kid);
		}
		if (forked) {
			exit(0);
		}
	}
#if defined(PRE_RISCOS)
	return(status);
#endif
}	/* end of mounttree */

/*
 * Returns true if s1 is a pathname substring of s2.
 */
substr(s1, s2)
	char *s1;
	char *s2;
{
	while (*s1 == *s2) {
		s1++;
		s2++;
	}
	if (*s1 == '\0' && *s2 == '/') {
		return (1);
	}
	return (0);
}

usage()
{
	(void) fprintf(stdout,
	    "Usage: mount [-rqavpfto [type|option]] ... [fsname] [dir]\n");
	exit(1);
}

/*
 * Returns the next option in the option string.
 */
static char *
getnextopt(p)
        char **p;
{
        char *cp = *p;
        char *retstr;

        while (*cp && isspace(*cp))
                cp++;
        retstr = cp;
        while (*cp && *cp != ',')
                cp++;
        if (*cp) {
                *cp = '\0';
                cp++;
        }
        *p = cp;
        return (retstr);
}

/*
 * "trueopt" and "falseopt" are two settings of a Boolean option.
 * If "flag" is true, forcibly set the option to the "true" setting; otherwise,
 * if the option isn't present, set it to the false setting.
 */
static void
replace_opts(options, flag, trueopt, falseopt)
	char *options;
	int flag;
	char *trueopt;
	char *falseopt;
{
	register char *f;
	char tmptopts[MNTMAXSTR];
	char *tmpoptsp;
	register int found;

	(void) strcpy(tmptopts, options);
	tmpoptsp = tmptopts;
	(void) strcpy(options, "");

	found = 0;
	for (f = getnextopt(&tmpoptsp); *f; f = getnextopt(&tmpoptsp)) {
		if (options[0] != '\0')
			(void) strcat(options, ",");
		if (strcmp(f, trueopt) == 0) {
			(void) strcat(options, f);
			found++;
		} else if (strcmp(f, falseopt) == 0) {
			if (flag)
				(void) strcat(options, trueopt);
			else
				(void) strcat(options, f);
			found++;
		} else
			(void) strcat(options, f);
        }
	if (!found) {
		if (options[0] != '\0')
			(void) strcat(options, ",");
		(void) strcat(options, flag ? trueopt : falseopt);
	}
}	/* end of replace_opts */

