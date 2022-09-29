/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: auto_main.c,v 1.2.1.5.1.2 90/07/11 18:08:46 hawkes Exp $ */
/*
 * @(#)auto_main.c	1.5 89/04/28 4.0NFSSRC Copyr 1988 Sun Micro
 *
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <syslog.h>
#include <errno.h>
#include <string.h>
#include <rpc/rpc.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <sys/wait.h>
#include <sys/time.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include "nfs_prot.h"
#include <netinet/in.h>
#include <mntent.h>
#include <netdb.h>
#include <sys/signal.h>
#include <sys/file.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypclnt.h>
#include <nfs/nfs_clnt.h>
#if defined(RISCOS) && defined(LOOP_BACK)
#include <sys/dir.h>
#endif

#define NFSCLIENT
typedef nfs_fh fhandle_t;
#include <sys/mount.h>

#include "automount.h"

extern errno;

void catch();
void reap();
void set_timeout();
void domntent();
void loadmaster_file();
void loadmaster_yp();

#define	MAXDIRS	10

#define	MASTER_MAPNAME	"auto.master"

#if defined(RISCOS)
int maxwait = 60*60;
int mount_timeout = 30;
int max_link_time = 60*60;
#else
int maxwait = 60;
int mount_timeout = 30;
int max_link_time = 5*60;
#endif /* RISCOS */
int verbose;

u_short myport;

main(argc, argv)
	int argc;
	char *argv[];
{
	SVCXPRT *transp;
	extern void nfs_program_2();
	extern void check_mtab();
	static struct sockaddr_in sin;	/* static causes zero init */
	struct nfs_args args;
	struct autodir *dir, *dir_next;
	int pid;
	int bad;
	int master_yp = 1;
	char *master_file;
	char *myname = argv[0];
	struct hostent *hp;
	extern int trace;
	char pidbuf[64];


	if (geteuid() != 0) {
		(void) fprintf(stderr, "Must be root to use automount\n");
		exit(1);
	}

	argc--;
	argv++;

	openlog("automount", LOG_PID | LOG_NOWAIT | LOG_CONS, LOG_DAEMON);

	(void) setbuf(stdout, (char *)NULL);
	(void) gethostname(self, sizeof self);
	hp = gethostbyname(self);
	if (hp == NULL) {
		syslog(LOG_ERR, "Can't get my address");
		exit(1);
	}
	bcopy(hp->h_addr, (char *)&my_addr, hp->h_length);
	(void) getdomainname(mydomain, sizeof mydomain);
	if (bad = yp_bind(mydomain))
		syslog(LOG_ERR, "YP bind failed: %s", yperr_string(bad));

#if defined(RISCOS)
	(void) strcpy(tmpdir, TMPDIR);
	rootdir[0] = '\0';
	access_point[0] = '\0';
	suppress_access_name = 0;
#if defined(LOOP_BACK)
	loop_back = 0;
#endif /* LOOP_BACK */
#else
	(void) strcpy(tmpdir, "/tmp_mnt");
#endif /* RISCOS */
	master_file = NULL;

	while (argc && argv[0][0] == '-') switch (argv[0][1]) {
	case 'n':
		nomounts++;
		argc--;
		argv++;
		break;
	case 'm':
		master_yp = 0;
		argc--;
		argv++;
		break;
	case 'f':
		master_file = argv[1];
		argc -= 2;
		argv += 2;
		break;
	case 'M':		/* Mount point */
		(void) strcpy(tmpdir, argv[1]);	/* tmpdir== top path seg */
		argc -= 2;
		argv += 2;
		break;
#if defined(RISCOS)
	case 'R':		/* Root or tail directory name */
		(void) strcpy(rootdir, argv[1]);
		argc -= 2;
		argv += 2;
		if (*rootdir != '/') {
			char foobar[200];

			(void) strcpy(foobar, rootdir);
			(void) strcpy(rootdir, "/");
			(void) strcat(rootdir, foobar);
		}
		if (rootdir[strlen(rootdir)-1] == '/')
			rootdir[strlen(rootdir)-1] = '\0';
		break;
	case 'S':
		suppress_access_name++;
		argc--;
		argv++;
		break;
#if defined(LOOP_BACK)
	case 'L':
		loop_back++;
		argc--;
		argv++;
		break;
#endif /* LOOP_BACK */
#endif /* RISCOS */
	case 't':			/* timeouts */
		if (argc < 2) {
			(void) fprintf(stderr, "Bad timeout value\n");
			usage();
		}
		if (argv[0][2]) {
			set_timeout(argv[0][2], atoi(argv[1]));
		} else {
			char *s;

			for (s = strtok(argv[1], ","); s ;
				s = strtok(NULL, ",")) {
				if (*(s+1) != '=') {
					(void) fprintf(stderr,
						"Bad timeout value\n");
					usage();
				}
				set_timeout(*s, atoi(s+2));
			}
		}
		argc -= 2;
		argv += 2;
		break;

	case 'T':
		trace++;
		argc--;
		argv++;
		break;

	case 'D':
		if (argv[0][2])
			(void) putenv(&argv[0][2]);
		else {
			(void) putenv(argv[1]);
			argc--;
			argv++;
		}
		argc--;
		argv++;
		break;

	case 'v':
		verbose++;
		argc--;
		argv++;
		break;

	default:
		usage();
	}

	if (verbose && argc == 0 && master_yp == 0 && master_file == NULL) {
		syslog(LOG_ERR, "no mount maps specified");
		usage();
	}

	check_mtab();
	/*
	 * Get mountpoints and maps off the command line
	 */
	while (argc >= 2) {
#if defined(RISCOS) && defined(LOOP_BACK)
		if (loop_back)
			strcpy(access_point, argv[0]);
#endif
		if (argc >= 3 && argv[2][0] == '-') {
			dirinit(argv[0], argv[1], argv[2]+1, 0);
			argc -= 3;
			argv += 3;
		} else {
			dirinit(argv[0], argv[1], "rw", 0);
			argc -= 2;
			argv += 2;
		}
	}
	if (argc)
		usage();

#ifdef SET_ARCH     /* non-SunOS systems may not have 'arch' script */
	if (getenv("ARCH") == NULL) {
		char buf[16], str[32];
		int len;
		FILE *f;

		f = popen("arch", "r");
		(void) fgets(buf, 16, f);
		(void) pclose(f);
		if (len = strlen(buf))
			buf[len - 1] = '\0';
		(void) sprintf(str, "ARCH=%s", buf);
		(void) putenv(str);
	}
#endif /* def SET_ARCH */
	
	if (master_file) {
		loadmaster_file(master_file);
	}
	if (master_yp) {
		loadmaster_yp(MASTER_MAPNAME);
	}

	/*
	 * Remove -null map entries
	 */
	for (dir = HEAD(struct autodir, dir_q); dir; dir = dir_next) {
	    	dir_next = NEXT(struct autodir, dir);
		if (strcmp(dir->dir_map, "-null") == 0) {
			REMQUE(dir_q, dir);
		}
	}
	if (HEAD(struct autodir, dir_q) == NULL)   /* any maps ? */
		exit(1);

	transp = svcudp_create(RPC_ANYSOCK);
	if (transp == NULL) {
		syslog(LOG_ERR, "Cannot create UDP service");
		exit(1);
	}
	if (!svc_register(transp, NFS_PROGRAM, NFS_VERSION, nfs_program_2, 0)) {
		syslog(LOG_ERR, "svc_register failed");
		exit(1);
	}

#if defined(RISCOS) && defined(LOOP_BACK)
	if (loop_back)
		setup_loop_back_links();
#endif

#ifdef DEBUG
	pid = getpid();
	if (fork()) {
		/* parent */
		signal(SIGTERM, catch);
		signal(SIGHUP,  check_mtab);
		signal(SIGCHLD, reap);
		auto_run();
		syslog(LOG_ERR, "svc_run returned");
		exit(1);
	}
#else NODEBUG
	switch (pid = fork()) {
	case -1:
		syslog(LOG_ERR, "Cannot fork: %m");
		exit(1);
	case 0:
		/* child */
		{ int tt = open("/dev/tty", O_RDWR);
		  if (tt > 0) {
			(void) ioctl(tt, TIOCNOTTY, (char *)0);
			(void) close(tt);
		  }
		}
		signal(SIGTERM, catch);
		signal(SIGHUP, check_mtab);
		signal(SIGCHLD, reap);
		auto_run();
		syslog(LOG_ERR, "svc_run returned");
		exit(1);
	}
#endif

	/* parent */
	sin.sin_family = AF_INET;
	sin.sin_port = htons(transp->xp_port);
	myport = transp->xp_port;
	sin.sin_addr.s_addr = htonl(INADDR_LOOPBACK);
	bzero(sin.sin_zero, sizeof(sin.sin_zero));
	args.addr = &sin;
	args.flags = NFSMNT_INT + NFSMNT_TIMEO +
#ifdef RISCOS
		     NFSMNT_HOSTNAME + NFSMNT_RETRANS + NFSMNT_SOFT;
#else
		     NFSMNT_HOSTNAME + NFSMNT_RETRANS;
#endif
	args.timeo = (mount_timeout + 5) * 10;
	args.retrans = 5;
	bad = 1;

	/*
	 * Mount the daemon at it's mount points.
	 * Start at the end of the list because that's
	 * the first on the command line.
	 */
	for (dir = TAIL(struct autodir, dir_q); dir;
	    dir = PREV(struct autodir, dir)) {
		(void) sprintf(pidbuf, "(pid%d@%s)", pid, dir->dir_name);
		if (strlen(pidbuf) >= HOSTNAMESZ-1)
			(void) strcpy(&pidbuf[HOSTNAMESZ-3], "..");
		args.hostname = pidbuf;
		args.fh = (caddr_t)&dir->dir_vnode.vn_fh; 
#ifdef OLDMOUNT
#define MOUNT_NFS 1
		if (mount(MOUNT_NFS, dir->dir_name, M_RDONLY, &args)) {
#else
		if (mount("nfs", dir->dir_name, M_RDONLY|M_NEWTYPE, &args)) {
#endif
			syslog(LOG_ERR, "Can't mount %s: %m", dir->dir_name);
			bad++;
		} else {
			domntent(pid, dir);
			bad = 0;
		}
	}
	if (bad)
		(void) kill(pid, SIGKILL);
	exit(bad);
	/*NOTREACHED*/
}

#if defined(RISCOS) && defined(LOOP_BACK)
setup_loop_back_links()
{
    DIR *dirp;			/* directory pointer */
    register struct direct *dp;	/* directory entry */
    struct stat dbuf;		/* stat buffer for directory entry */
    char hostentry_dir[MAXPATHLEN];  /* directory with hostnames */
    char host_dir[MAXPATHLEN];	/* hostname directory */
    char fname[MAXPATHLEN];	/* root directory */
    char linkbuf[MAXPATHLEN];	/* symlink name */

    sprintf(hostentry_dir,"%s%s", tmpdir,
	    suppress_access_name ? "" : access_point);
    dirp = opendir(hostentry_dir);
    if (dirp == NULL) {
 	syslog(LOG_ERR, "%s unreadable: %m\n", hostentry_dir);
	exit(1);
    }
    while (dp = readdir(dirp)) {
	sprintf(host_dir, "%s/%s", hostentry_dir, dp->d_name);

	if (dp->d_ino == 0)		/* ignore entries */
	    continue;
	if (dp->d_name[0]==0 ||
	    (dp->d_name[0]=='.' &&
	     (dp->d_name[1]==0 ||
	      (dp->d_name[1]=='.' && dp->d_name[2]==0))))
	    continue;
	if (lstat(host_dir, &dbuf))
	    continue;
	if (dbuf.st_mode & S_IFLNK) {	/* remove link, it might be bad */
	    if (unlink(host_dir) != 0) {
		syslog(LOG_ERR,"Can't unlink: %s: %m", host_dir);
		continue;
	    }
	} else if (!(dbuf.st_mode & S_IFDIR))
	    continue;

	/* find root */
	sprintf(fname, "%s%s", host_dir, rootdir);
	if (lstat(fname, &dbuf)) {
	    if (errno != ENOENT) {
	        syslog(LOG_ERR,"Can't lstat %s: %m", fname);
		continue;
	    }
	    /* else file already deleted or just non-existent */
	} else {
	    if (dbuf.st_mode & S_IFLNK) {
		if (unlink(fname) != 0) {
		    syslog(LOG_ERR,"Can't unlink: %s: %m", fname);
		    continue;
		}
	    } else if (!(dbuf.st_mode & S_IFDIR))
	        continue;
	    safe_rmdir(fname);
	}
	if (stat(fname, &dbuf)) {
	    sprintf(linkbuf,"%s/%s", access_point, dp->d_name);
	    if (symlink(linkbuf, fname))
		syslog(LOG_ERR,"failed to create symlink: %s: %m\n",linkbuf);
	}
    }
    closedir(dirp);
}	/* end of setup_loop_back_links */
#endif /* RISCOS */

void
set_timeout(letter, t)
	char letter ; int t;
{
	if (t <= 0) {
		(void) fprintf(stderr, "Bad timeout value\n");
		usage();
	}
	switch (letter) {
	case 'm':
		mount_timeout = t;
		break;
	case 'l':
		max_link_time = t;
		break;
	case 'w':
		maxwait = t;
		break;
	default:
		(void) fprintf(stderr, "automount: bad timeout switch\n");
		usage();
		break;
	}
}

void
domntent(pid, dir)
	int pid;
	struct autodir *dir;
{
	FILE *f;
	struct mntent mnt;
	struct stat st;
	char fsname[64];
	char mntopts[100];
	char buf[16];

	f = setmntent(MOUNTED, "a");
	if (f == NULL) {
		syslog(LOG_ERR, "Can't update %s", MOUNTED);
		return;
	}
	(void) sprintf(fsname, "%s:(pid%d)", self, pid);
	mnt.mnt_fsname = fsname;
	mnt.mnt_dir = dir->dir_name;
#ifdef RISCOS
	(void) sprintf(mntopts, "ro,soft,intr,port=%d,map=%s,%s", myport,
#else
	(void) sprintf(mntopts, "ro,intr,port=%d,map=%s,%s", myport,
#endif
		dir->dir_map, 
		dir->dir_vnode.vn_type == VN_LINK ? "direct" : "indirect");
	if (dir->dir_vnode.vn_type == VN_LINK) {
		mnt.mnt_type = MNTTYPE_IGNORE;
	} else {
		mnt.mnt_type = MNTTYPE_NFS;
	}
	mnt.mnt_opts = mntopts;
	mnt.mnt_freq = 0;
	mnt.mnt_passno = 0;	
	(void) addmntent(f, &mnt);	
	(void) endmntent(f);
}

void
catch()
{
	struct autodir *dir;
	int child;
	struct filsys *fs, *fs_next;
	struct stat stbuf;
	struct fattr *fa;

	/*
	 *  Remove automounter's mount points from /etc/mtab
	 */
	for (dir = HEAD(struct autodir, dir_q); dir;
	    dir = NEXT(struct autodir, dir)) {
			clean_mtab(dir->dir_name, 0);
	}

	/*
	 *  The automounter has received a SIGTERM.
	 *  Here it forks a child to carry on servicing
	 *  its mount points in order that those
	 *  mount points can be unmounted.  The child
	 *  checks for symlink mount points and changes them
	 *  into directories to prevent the unmount system
	 *  call from following them.
	 */
	if ((child = fork()) == 0) {
		for (dir = HEAD(struct autodir, dir_q); dir;
		    dir = NEXT(struct autodir, dir)) {
			if (dir->dir_vnode.vn_type != VN_LINK)
				continue;

			dir->dir_vnode.vn_type = VN_DIR;
			fa = &dir->dir_vnode.vn_fattr;
			fa->type = NFDIR;
			fa->mode = NFSMODE_DIR + 0555;
		}
		return;
	}

	for (dir = HEAD(struct autodir, dir_q); dir;
	    dir = NEXT(struct autodir, dir)) {

		/*  This lstat is a kludge to force the kernel
		 *  to flush the attr cache.  If it was a direct
		 *  mount point (symlink) the kernel needs to
		 *  do a getattr to find out that it has changed
		 *  back into a directory.
		 */
		if (lstat(dir->dir_name, &stbuf) < 0) {
			syslog(LOG_ERR, "lstat %s: %m", dir->dir_name);
		}

		if (unmount(dir->dir_name) < 0) {
			if (errno != EBUSY)
				syslog(LOG_ERR, "unmount %s: %m", dir->dir_name);
		} else {
			if (dir->dir_remove)
				(void) rmdir(dir->dir_name);
		}
	}
	(void) kill (child, SIGKILL);

	/*
	 *  Unmount any mounts done by the automounter
	 */
	for (fs = HEAD(struct filsys, fs_q); fs; fs = fs_next) {
		fs_next = NEXT(struct filsys, fs);
		if (fs->fs_mine && fs == fs->fs_rootfs) {
			if (do_unmount(fs))
				fs_next = HEAD(struct filsys, fs_q);
		}
	}

	syslog(LOG_ERR, "exiting");
	exit(0);
}

void
reap()
{
	while (wait3((union wait *)0, WNOHANG, (struct rusage *)0) > 0)
		;
}

auto_run()
{
	int read_fds, n;
	time_t time();
	long last;
	struct timeval tv;

	last = time((long *)0);
	tv.tv_sec = maxwait;
	tv.tv_usec = 0;
	for (;;) {
		read_fds = svc_fds;
		n = select(32, &read_fds, (int *)0, (int *)0, &tv);
		time_now = time((long *)0);
		if (n)
			svc_getreq(read_fds);
		if (time_now >= last + maxwait) {
			last = time_now;
			do_timeouts();
		}
	}
}

/* Note that the -L option is not described because it was
 * not implemented properly.
 */
usage()
{
#if defined(RISCOS)
	(void) fprintf(stderr, "Usage: automount\n%s%s%s%s%s%s%s%s%s%s%s%s%s",
#else
	(void) fprintf(stderr, "Usage: automount\n%s%s%s%s%s%s%s%s%s%s%s",
#endif /* RISCOS */
		       "\t[-n]\t\t(no mounts)\n",
		       "\t[-m]\t\t(no auto.master)\n",
		       "\t[-T]\t\t(trace nfs requests)\n",
		       "\t[-v]\t\t(verbose error msgs)\n",
		       "\t[-tl n]\t\t(mount duration)\n",
		       "\t[-tm n]\t\t(attempt interval)\n",
		       "\t[-tw n]\t\t(unmount interval)\n",
		       "\t[-M dir]\t(temporary mount dir)\n",
#if defined(RISCOS)
		       "\t[-R dir]\t(root or tail path segment for mount dir)\n",
		       "\t[-S]\t\t(suppress use of access point in mount dir)\n",
#endif /* RISCOS */
		       "\t[-D n=s]\t(define env variable)\n",
		       "\t[-f file]\t(get mntpnts from file)\n",
		       "\t[ dir map [-mntopts] ] ...\n");
	exit(1);
}

void
loadmaster_yp(mapname)
	char *mapname;
{
	int first, err;
	char *key, *nkey, *val;
	int kl, nkl, vl;
	char dir[100], map[100];
	char *p, *opts;


	first = 1;
	key  = NULL; kl  = 0;
	nkey = NULL; nkl = 0;
	val  = NULL; vl  = 0;

	for (;;) {
		if (first) {
			first = 0;
			err = yp_first(mydomain, mapname, &nkey, &nkl, &val, &vl);
		} else {
			err = yp_next(mydomain, mapname, key, kl, &nkey, &nkl,
				&val, &vl);
		}
		if (err) {
			if (err != YPERR_NOMORE && err != YPERR_MAP)
				syslog(LOG_ERR, "%s: %s",
					mapname, yperr_string(err));
			return;
		}
		if (key)
			free(key);
		key = nkey;
		kl = nkl;

		if (kl >= 100 || vl >= 100)
			return;
		if (kl < 2 || vl < 1)
			return;
		if (isspace(*key) || *key == '#')
			return;
		(void) strncpy(dir, key, kl);
		dir[kl] = '\0';
		(void) strncpy(map, val, vl);
		map[vl] = '\0';
		p = map;
		while (*p && !isspace(*p))
			p++;
		opts = "rw";
		if (*p) {
			*p++ = '\0';
			while (*p && isspace(*p))
				p++;
			if (*p == '-')
				opts = p+1;
		}

		dirinit(dir, map, opts, 0);

		free(val);
	}
}

void
loadmaster_file(masterfile)
	char *masterfile;
{
	FILE *fp;
	char *line, *dir, *map, *opts;
	extern char *get_line();
	char linebuf[1024];

	if ((fp = fopen(masterfile, "r")) == (FILE *)NULL) {
		syslog(LOG_ERR, "%s:%m", masterfile);
		return;
	}

	while ((line = get_line(fp, linebuf, sizeof linebuf)) != NULL) {
		dir = line;
		while (*dir && isspace(*dir)) dir++;
		if (*dir == '\0')
			continue;
		map = dir;
		while (*map && !isspace(*map)) map++;
		if (*map)
			*map++ = '\0';
		if (*dir == '+') {
			loadmaster_yp(dir+1);
		} else {
			while (*map && isspace(*map)) map++;
			if (*map == '\0')
				continue;
			opts = map;
			while (*opts && !isspace(*opts)) opts++;
			if (*opts) {
				*opts++ = '\0';
				while (*opts && isspace(*opts)) opts++;
			}
			if (*opts != '-')
				opts = "-rw";
			
			dirinit(dir, map, opts+1, 0);
		}
	}
	(void) fclose(fp);
}
