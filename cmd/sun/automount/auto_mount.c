/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: auto_mount.c,v 1.2.1.5.1.2 90/07/11 18:09:03 hawkes Exp $ */
/*
 * @(#)auto_mount.c	1.4 89/04/28 4.0NFSSRC Copyr 1988 Sun Micro
 *
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <syslog.h>
#include <sys/param.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <sys/signal.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <mntent.h>
#include <netdb.h>
#include <errno.h>
#include "nfs_prot.h"
typedef nfs_fh fhandle_t;
#include <rpcsvc/mount.h>
#define NFSCLIENT
#include <sys/mount.h>
#include "automount.h"
extern int errno;

#if defined(RISCOS)
#include <syslog.h>
#define fprintf syslog
#ifdef stderr
#undef stderr
#endif
#define stderr LOG_DEBUG
#endif /* RISCOS */

#define MAXHOSTS  20

struct mapfs *find_server();
struct filsys *already_mounted();
void addtomtab();
char *inet_ntoa();
extern int trace;

static long last_mtab_time = 0;

nfsstat
do_mount(dir, me, rootfs, linkpath)
	struct autodir *dir;
	struct mapent *me;
	struct filsys **rootfs;
	char **linkpath;
{
	char mntpnt[MAXPATHLEN];
	static char linkbuf[MAXPATHLEN];
	enum clnt_stat pingmount();
	struct filsys *tfs;
	struct mapfs *mfs;
	struct mapent *m;
	nfsstat status = NFSERR_NOENT;
	struct in_addr prevhost;
	int imadeit;
	struct stat stbuf;

	*rootfs = NULL;
	*linkpath = "";
	prevhost.s_addr = (u_long)0;

	for (m = me ; m ; m = m->map_next) {
		tfs = NULL;
#ifdef RISCOS
		mfs = find_server(m, &tfs, m == me, prevhost, dir->dir_name);
#else
		mfs = find_server(m, &tfs, m == me, prevhost);
#endif
		if (mfs == NULL)
			continue;

#ifdef DEBUG
		syslog(LOG_DEBUG,
		       "map->root: %s  map->next: %x  map->next->root: %s   mfs.addr: %x   my-addr: %x  i",
		       me->map_root, me->map_next,
		       me->map_next ? me->map_next->map_root: 0,
		       mfs->mfs_addr.s_addr, my_addr.s_addr);
#endif DEBUG
		/*
		 * It may be possible to return a symlink
		 * to an existing mount point without
		 * actually having to do a mount.
		 */

		/* Is it my host ? */
		if (me->map_next == NULL &&
#ifdef RISCOS
		    samehost(mfs->mfs_addr.s_addr, my_addr.s_addr))
#else
		    mfs->mfs_addr.s_addr == my_addr.s_addr)

#endif
		{
			(void) sprintf(linkbuf, "%s%s",
				mfs->mfs_dir, mfs->mfs_subdir);
			*linkpath = linkbuf;
			return (NFS_OK);
		}

		/*
		 * already mounted ?
		 * Can just point the symbolic link at the current
		 * mount point.
		 * If a mount root is specified, then it must match.
		 */
		if (m == me && tfs != NULL) {
			if (trace > 1)
				(void) fprintf(stderr,
				"already mounted %s:%s on %s (%s)\n",
				tfs->fs_host, tfs->fs_dir,
				tfs->fs_mntpnt, tfs->fs_opts);

#if defined(RISCOS)
			(void) sprintf(mntpnt, "%s%s%s%s%s", tmpdir,
				(suppress_access_name) ? "" : dir->dir_name,
				me->map_root, rootdir, m->map_mntpnt);
#else
			(void) sprintf(mntpnt, "%s%s%s%s", tmpdir,
			   dir->dir_name, me->map_root, m->map_mntpnt);
#endif /* RISCOS */
			if (*me->map_root == '\0' ||
			    strcmp(mntpnt, tfs->fs_mntpnt) == 0) {
				*rootfs = tfs;
#if defined(RISCOS)
				(void) sprintf(linkbuf, "%s%s%s%s%s",
					tmpdir,
					suppress_access_name?"" : dir->dir_name,
					me->map_root,
					rootdir,
					mfs->mfs_subdir);
#else
				(void) sprintf(linkbuf, "%s%s%s%s",
					tmpdir, dir->dir_name,
					me->map_root,
					mfs->mfs_subdir);
#endif /* RISCOS */
				*linkpath = linkbuf;
				return (NFS_OK);
			}
		}

		if (nomounts)
			return (NFSERR_PERM);

		 /* Create the mount point if necessary */
#if defined(RISCOS)
		(void) sprintf(mntpnt, "%s%s%s%s%s", tmpdir,
			(suppress_access_name) ? "" : dir->dir_name,
			me->map_root, rootdir, m->map_mntpnt);
#else
		(void) sprintf(mntpnt, "%s%s%s%s", tmpdir,
			dir->dir_name, me->map_root, m->map_mntpnt);
#endif /* RISCOS */
		imadeit = 0;
		if (stat(mntpnt, &stbuf) != 0) {
			if (mkdir_r(mntpnt) == 0)
				imadeit = 1;
			else {
				if (trace > 1)
					(void) fprintf(stderr,
					"couldn't create mntpnt %s\n",
					mntpnt);
				continue;
			}
		}

		if (pingmount(mfs->mfs_addr) != RPC_SUCCESS) {
			syslog(LOG_ERR, "host %s not responding", mfs->mfs_host);
			continue;
		}
		prevhost.s_addr = mfs->mfs_addr.s_addr;

		/*  Now do the mount */

		tfs = NULL;
		status = nfsmount(mfs->mfs_host, mfs->mfs_addr, mfs->mfs_dir,
				mntpnt, m->map_mntopts, &tfs);
		if (status == NFS_OK) {
			if (*rootfs == NULL) {
				*rootfs = tfs;
#if defined(RISCOS)
				(void) sprintf(linkbuf, "%s%s%s%s%s", tmpdir,
					suppress_access_name?"" : dir->dir_name,
					       me->map_root,
					       rootdir,
					       mfs->mfs_subdir);
#else
				(void) sprintf(linkbuf, "%s%s%s%s",
					tmpdir, dir->dir_name,
					me->map_root,
					mfs->mfs_subdir);
#endif /* RISCOS */
				*linkpath = linkbuf;
			}
			tfs->fs_rootfs = *rootfs;
		} else if (imadeit)	/* delete dir if created it */
			safe_rmdir(mntpnt);
	}
	if (*rootfs != NULL) {
		addtomtab(*rootfs);
		return (NFS_OK);
	}
	return (status);
}	/* end of do_mount */

/*
 * Implement so that, hopefully, addr2 is the oldest and most stable address.
 */
int
samehost(addr1, addr2)
u_long addr1, addr2;
{
	struct hostent *hp;
	unsigned char *bp;
	int index;

	/* NOTE: addr1 is not a 'struct in_addr', but it'll work anyway */
	if ((hp=gethostbyaddr(&addr1, sizeof(addr1), AF_INET)) == NULL)
		return(addr1==addr2);
	if ((hp=gethostbyname(hp->h_name)) == NULL)
		return(addr1==addr2);
	if (hp->h_addrtype != AF_INET) {
		syslog(LOG_ERR,"Unknown address type: %d", hp->h_addrtype);
		return(addr1==addr2);
	} else {
		for (index=0; ;index++) {
			bp = hp->h_addr_list[index];
			if (bp == NULL)
				break;
			if (((struct in_addr *)bp)->s_addr == addr2)
				return(1);
		}
		return(addr1==addr2);
	}
}	/* end of samehost */


struct mapfs *
#ifdef RISCOS
find_server(me, fsp, rootmount, preferred, access_name)
#else
find_server(me, fsp, rootmount, preferred)
#endif
	struct mapent *me;
	struct filsys **fsp;
	int rootmount;
	struct in_addr preferred;
#ifdef RISCOS
	char *access_name;
#endif
{
	int entrycount, localcount;
	struct mapfs *mfs;
	struct hostent *hp;
	struct in_addr addrs[MAXHOSTS], addr, *addrp, trymany();


	/*
	 * get addresses & see if any are myself
	 * or were mounted from previously in a
	 * hierarchical mount.
	 */
	entrycount = localcount = 0;
	for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next) {
		if (mfs->mfs_addr.s_addr == 0) {
			hp = gethostbyname(mfs->mfs_host);
			if (hp)
				bcopy(hp->h_addr, (char *)&mfs->mfs_addr,
					hp->h_length);
		}
		if (mfs->mfs_addr.s_addr) {
			entrycount++;
#ifdef RISCOS
			if (samehost(mfs->mfs_addr.s_addr, my_addr.s_addr))
#else
			if (mfs->mfs_addr.s_addr == my_addr.s_addr)
#endif
				return mfs;
#ifdef RISCOS
			if (samehost(mfs->mfs_addr.s_addr, preferred.s_addr))
#else
			if (mfs->mfs_addr.s_addr == preferred.s_addr)
#endif
				return mfs;
		}
	}
	if (entrycount == 0)
		return NULL;

	/* see if any already mounted */
	if (rootmount)
	    for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next) {
#ifdef RISCOS
	    	char mntpnt[MAXPATHLEN];

		(void) sprintf(mntpnt, "%s%s%s%s%s", tmpdir,
			(suppress_access_name) ? "" : access_name,
			me->map_root, rootdir, me->map_mntpnt);

		    if (*fsp = already_mounted(mfs->mfs_host, mfs->mfs_dir,
					       mntpnt, mfs->mfs_addr))
#else
		    if (*fsp = already_mounted(mfs->mfs_host, mfs->mfs_dir,
					       me->map_mntopts,mfs->mfs_addr))
#endif
			return mfs;
	    }

	if (entrycount == 1) 	/* no replication involved */
		return me->map_fs;

	/* try local net */
	addrp = addrs;
	for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next) {
		if (inet_netof(mfs->mfs_addr) == inet_netof(my_addr)) {
			localcount++;
			*addrp++ = mfs->mfs_addr;
		}
	}
	if (localcount > 0) {	/* got some */
		(*addrp).s_addr = 0;	/* terminate list */
		addr = trymany(addrs, mount_timeout / 2);
		if (addr.s_addr) {	/* got one */
			for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next)
#ifdef RISCOS
			if (samehost(mfs->mfs_addr.s_addr, addr.s_addr))
#else
			if (addr.s_addr == mfs->mfs_addr.s_addr)
#endif
				return mfs;
		}
	}
	if (entrycount == localcount)
		return NULL;

	/* now try them all */
	addrp = addrs;
	for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next)
		*addrp++ = mfs->mfs_addr;
	(*addrp).s_addr = 0;	/* terminate list */
	addr = trymany(addrs, mount_timeout / 2);
	if (addr.s_addr) {	/* got one */
		for (mfs = me->map_fs; mfs; mfs = mfs->mfs_next)
#ifdef RISCOS
		if (samehost(mfs->mfs_addr.s_addr, addr.s_addr))
#else
		if (addr.s_addr == mfs->mfs_addr.s_addr)
#endif
				return mfs;
	}
	return NULL;
}

/*
 *  If mtab has changed update internal fs info
 */
void
check_mtab()
{
	struct stat stbuf;
	struct filsys *fs, *fsnext;
	FILE *table;
	register struct mntent *mt;
	char *index();
	int found;
	struct hostent *hp;

	/* 
	 * read in /etc/mtab if it's changed
	 */
	if (stat(MOUNTED, &stbuf) != 0) {
		syslog(LOG_ERR, "Cannot stat %s: %m", MOUNTED);
		return;
	}
	if (stbuf.st_mtime == last_mtab_time)
		return;				/* no change */

	last_mtab_time = stbuf.st_mtime;

	/* reset the present flags */
	for (fs = HEAD(struct filsys, fs_q); fs;
	    fs = NEXT(struct filsys, fs))
		    fs->fs_present = 0;

	/* now see what's been mounted */
	table = setmntent(MOUNTED, "r");
	while ((mt = getmntent(table)) != NULL) {
		char *tmphost, *tmppath, *p, tmpc;

		if (strcmp(mt->mnt_type, MNTTYPE_NFS) != 0)
			continue;
		p = index(mt->mnt_fsname, ':');
		if (p == NULL)
			continue;
		tmpc = *p;
		*p = '\0';
		tmphost = mt->mnt_fsname;
		tmppath = p+1;
		if (tmppath[0] != '/')
			continue;
		found = 0;
		for (fs = HEAD(struct filsys, fs_q); fs;
			fs = NEXT(struct filsys, fs)) {
			if (strcmp(mt->mnt_dir, fs->fs_mntpnt) == 0 &&
			    strcmp(tmphost, fs->fs_host) == 0 &&
			    strcmp(tmppath, fs->fs_dir) == 0 &&
			    strcmp(mt->mnt_opts, fs->fs_opts) == 0) {
				fs->fs_present = 1;
				found++;
				break;
			}
		}
		if (!found) {
			fs = alloc_fs(tmphost, tmppath,
				mt->mnt_dir, mt->mnt_opts);
			if (fs == NULL)
				return;
			fs->fs_present = 1;
			hp = gethostbyname(tmphost);
			if (hp != NULL) {
				bcopy(hp->h_addr, &fs->fs_addr.sin_addr,
				      hp->h_length);
			}
		}
		*p = tmpc;
	}
	(void) endmntent(table);

	/* free fs's that are no longer present */
	for (fs = HEAD(struct filsys, fs_q); fs; fs = fsnext) {
		fsnext = NEXT(struct filsys, fs);
		if (!fs->fs_present) {
			flush_links(fs);
			free_filsys(fs);
		}
	}
}

/*
 * Search the mount table to see if the given file system is already
 * mounted.  NOTE: This routine is not called when accessing the local
 * filesystem through the automounter.  This case is handled higher up
 * in find_server().
 */
struct filsys *
already_mounted(host, fsname, opts, hostaddr)
	char *host;
	char *fsname;
	char *opts;
	struct in_addr hostaddr;
{
	struct filsys *fs;
	struct stat stbuf;
	struct mntent m1, m2;
	int has1, has2;
	int fd;
	struct autodir *dir;
	int mydir;
	extern int verbose;

	check_mtab();
	m1.mnt_opts = opts;
	for (fs = HEAD(struct filsys, fs_q); fs; fs = NEXT(struct filsys, fs)){
		if (strcmp(fsname, fs->fs_dir) != 0)
			continue;
#ifdef RISCOS
		if (!samehost(fs->fs_addr.sin_addr.s_addr, hostaddr.s_addr))
#else
		if (hostaddr.s_addr != fs->fs_addr.sin_addr.s_addr)
#endif
			continue;

		/*
		 * Check it's not on one of my mount points.
		 * I might be mounted on top of a previous 
		 * mount of the same file system.
		 */

		for (mydir = 0, dir = HEAD(struct autodir, dir_q); dir;
			dir = NEXT(struct autodir, dir)) {
			if (strcmp(dir->dir_name, fs->fs_mntpnt) == 0) {
				mydir = 1;
				if (verbose)
					syslog(LOG_ERR,
					"%s:%s already mounted on %s",
					host, fsname, fs->fs_mntpnt);
				break;
			}
		}
		if (mydir)
			continue;

#ifdef RISCOS
		/* If it's mounted, then why worry about mount options.
		 * Check to see if mount points match.  'opts' formals
		 * parameter is used differently in this case to represent
		 * the mountpoint of the current access attempt.  This mod
		 * also fixes a problem when all of a remote machine's
		 * filesystems are mounted manually and the directories are
		 * accessed.  If the mount options were different for the
		 * root filesystem, the automounter would attempt to mount
		 * all of the remote filesystems even though they were mounted.
		 * Because all would fail (already mounted), the do_mount
		 * routine would return a code as if the machine was down.
		 * With the current mod, the above behavior is no longer
		 * present; however, if all but one of a machine's filesystems
		 * are mounted manually, the current code will not mount
		 * the remaining filesystem.  The old code would have mounted
		 * it because [in my opinion] of a side-affect of a bug.
		 */
		if (strcmp(opts, fs->fs_mntpnt))	/* if different */
			continue;
#else
		m2.mnt_opts = fs->fs_opts;
		has1 = hasmntopt(&m1, MNTOPT_RO) != NULL;
		has2 = hasmntopt(&m2, MNTOPT_RO) != NULL;
		if (has1 != has2)
			continue;
		has1 = hasmntopt(&m1, MNTOPT_NOSUID) != NULL;
		has2 = hasmntopt(&m2, MNTOPT_NOSUID) != NULL;
		if (has1 != has2)
			continue;
		has1 = hasmntopt(&m1, MNTOPT_SOFT) != NULL;
		has2 = hasmntopt(&m2, MNTOPT_SOFT) != NULL;
		if (has1 != has2)
			continue;
		has1 = hasmntopt(&m1, MNTOPT_INTR) != NULL;
		has2 = hasmntopt(&m2, MNTOPT_INTR) != NULL;
		if (has1 != has2)
			continue;
#ifdef MNTOPT_SECURE
		has1 = hasmntopt(&m1, MNTOPT_SECURE) != NULL;
		has2 = hasmntopt(&m2, MNTOPT_SECURE) != NULL;
		if (has1 != has2)
			continue;
#endif MNTOPT_SECURE
#endif /* !RISCOS */
		/*
		 * dir under mountpoint may have been removed by other means
		 * If so, we get a stale file handle error here if we fsync
		 * the dir to remove the attr cache info
		 */
		fd = open(fs->fs_mntpnt, 0);
		if (fd < 0)
			continue;
		if (fsync(fd) != 0 || fstat(fd, &stbuf) != 0) {
			(void) close(fd);
			continue;
		}
		(void) close(fd);
		return (fs);
	}
	return(0);
}

nfsunmount(fs)
	struct filsys *fs;
{
	struct sockaddr_in sin;
	struct timeval timeout;
	CLIENT *client;
	enum clnt_stat rpc_stat;
	int s;

	sin = fs->fs_addr;
	/*
	 * Port number of "fs->fs_addr" is NFS port number; make port
	 * number 0, so "clntudp_create" finds port number of mount
	 * service.
	 */
	sin.sin_port = 0;
	s = RPC_ANYSOCK;
	timeout.tv_usec = 0;
	timeout.tv_sec = 10;
	if ((client = clntudp_create(&sin, MOUNTPROG, MOUNTVERS,
	    timeout, &s)) == NULL) {
		syslog(LOG_WARNING, "%s:%s %s",
		    fs->fs_host, fs->fs_dir,
		    clnt_spcreateerror("server not responding"));
		return;
	}
#ifdef OLDMOUNT
	if (bindresvport(s, NULL))
		syslog(LOG_ERR, "Warning: cannot do local bind");
#endif
	client->cl_auth = authunix_create_default();
	timeout.tv_usec = 0;
	timeout.tv_sec = 25;
	rpc_stat = clnt_call(client, MOUNTPROC_UMNT, xdr_path, &fs->fs_dir,
	    xdr_void, (char *)NULL, timeout);
	(void) close(s);
	AUTH_DESTROY(client->cl_auth);
	clnt_destroy(client);
	if (rpc_stat != RPC_SUCCESS)
		syslog(LOG_WARNING, "%s", clnt_sperror(client, "unmount"));
}

enum clnt_stat
pingmount(hostaddr)
	struct in_addr hostaddr;
{
	struct timeval tottime;
	struct sockaddr_in server_addr;
	enum clnt_stat clnt_stat;
	u_long port;
	static struct in_addr goodhost, deadhost;
	static time_t goodtime, deadtime;
	int cache_time = 60;  /* sec */

#ifdef RISCOS
	if (goodtime > time_now && samehost(hostaddr.s_addr, goodhost.s_addr))
			return RPC_SUCCESS;
	if (deadtime > time_now && samehost(hostaddr.s_addr, deadhost.s_addr))
			return RPC_TIMEDOUT;
#else
	if (goodtime > time_now && hostaddr.s_addr == goodhost.s_addr)
			return RPC_SUCCESS;
	if (deadtime > time_now && hostaddr.s_addr == deadhost.s_addr)
			return RPC_TIMEDOUT;
#endif

	if (trace > 1)
		fprintf(stderr, "ping %s ", inet_ntoa(hostaddr));

	tottime.tv_sec = 10;
	tottime.tv_usec = 0;

	/* 
	 * We use the pmap_rmtcall interface because the normal
	 * portmapper has a wired-in 60 second timeout
	 */
	server_addr.sin_family = AF_INET;
	server_addr.sin_addr = hostaddr;
	server_addr.sin_port = 0;
	clnt_stat = pmap_rmtcall(&server_addr, MOUNTPROG, MOUNTVERS, NULLPROC,
			xdr_void, 0, xdr_void, 0, tottime, &port);

	if (clnt_stat == RPC_SUCCESS) {
		goodhost = hostaddr;
		goodtime = time_now + cache_time;
	} else {
		deadhost = hostaddr;
		deadtime = time_now + cache_time;
	}

	if (trace > 1)
		fprintf(stderr, "%s\n", clnt_stat == RPC_SUCCESS ?
			"OK" : "NO RESPONSE");

	return (clnt_stat);
}

struct in_addr gotaddr;

/* ARGSUSED */
catchfirst(ignore, raddr)
	struct sockaddr_in *raddr;
{
	gotaddr = raddr->sin_addr;
	return (1);	/* first one ends search */
}

/*
 * ping a bunch of hosts at once and find out who
 * responds first
 */
struct in_addr
trymany(addrs, timeout)
	struct in_addr *addrs;
	int timeout;
{
	enum clnt_stat many_cast();
	enum clnt_stat clnt_stat;

	if (trace > 1) {
		register struct in_addr *a;

		fprintf(stderr, "many_cast: ");
		for (a = addrs ; a->s_addr ; a++)
			fprintf(stderr, "%s ", inet_ntoa(*a));
		fprintf(stderr, "\n");
	}
		
	gotaddr.s_addr = 0;
	clnt_stat = many_cast(addrs, MOUNTPROG, MOUNTVERS, NULLPROC,
			xdr_void, 0, xdr_void, 0, catchfirst, timeout);
	if (trace > 1) {
		fprintf(stderr, "many_cast: got %s\n",
			(int) clnt_stat ? "no response" : inet_ntoa(gotaddr));
	}
	if (clnt_stat)
		syslog(LOG_ERR, "trymany: servers not responding: %s",
			clnt_sperrno(clnt_stat));
	return (gotaddr);
}

nfsstat
nfsmount(host, hostaddr, dir, mntpnt, opts, fsp)
	char *host, *dir, *mntpnt, *opts;
	struct in_addr hostaddr;
	struct filsys **fsp;
{
	struct filsys *fs;
	char netname[MAXNETNAMELEN+1];
	char remname[MAXPATHLEN];
	struct mntent m;
	struct nfs_args args;
	int flags;
	struct sockaddr_in sin;
	static struct fhstatus fhs;
	struct timeval timeout;
	CLIENT *client;
	enum clnt_stat rpc_stat;
	enum clnt_stat pingmount();
	int s;
	u_short port;
	nfsstat status;
	struct stat stbuf;

	if (lstat(mntpnt, &stbuf) < 0) {
		syslog(LOG_ERR, "Couldn't stat %s: %m", mntpnt);
		return (NFSERR_NOENT);
	}
	if ((stbuf.st_mode & S_IFMT) == S_IFLNK) {
		if (readlink(mntpnt, remname, sizeof remname) < 0)
			return (NFSERR_NOENT);
		if (remname[0] == '/') {
			syslog(LOG_ERR,
				"%s -> %s from %s: absolute symbolic link",
				mntpnt, remname, host);
			return (NFSERR_NOENT);
		}
	}

	/*
	 * get fhandle of remote path from server's mountd
	 */
	(void) sprintf(remname, "%s:%s", host, dir);
	bzero((char *)&sin, sizeof(sin));
	sin.sin_family = AF_INET;
	sin.sin_addr.s_addr = hostaddr.s_addr;
	timeout.tv_usec = 0;
	timeout.tv_sec = mount_timeout;
	s = RPC_ANYSOCK;
	if ((client = clntudp_create(&sin, MOUNTPROG, MOUNTVERS,
	    timeout, &s)) == NULL) {
		syslog(LOG_ERR, "%s %s", remname,
		    clnt_spcreateerror("server not responding"));
		return (NFSERR_NOENT);
	}
#ifdef OLDMOUNT
	if (bindresvport(s, NULL))
		syslog(LOG_ERR, "Warning: cannot do local bind");
#endif
	client->cl_auth = authunix_create_default();
	timeout.tv_usec = 0;
	timeout.tv_sec = mount_timeout;
	rpc_stat = clnt_call(client, MOUNTPROC_MNT, xdr_path, &dir,
	    xdr_fhstatus, &fhs, timeout);
	if (rpc_stat != RPC_SUCCESS) {
		/*
		 * Given the way "clnt_sperror" works, the "%s" immediately
		 * following the "not responding" is correct.
		 */
		syslog(LOG_ERR, "%s server not responding%s", remname,
		    clnt_sperror(client, ""));
		(void) close(s);
		clnt_destroy(client);
		return (NFSERR_NOENT);
	}
	(void) close(s);
	AUTH_DESTROY(client->cl_auth);
	clnt_destroy(client);

	if (errno = fhs.fhs_status)  {
                if (errno == EACCES) {
			status = NFSERR_ACCES;
                } else {
			syslog(LOG_ERR, "%s: %m", remname);
			status = NFSERR_IO;
                }
                return (status);
        }        

	/*
	 * set mount args
	 */
	args.fh = (caddr_t)&fhs.fhs_fh;
	args.hostname = host;
	args.flags = 0;
	args.flags |= NFSMNT_HOSTNAME;
	args.addr = &sin;
	m.mnt_opts = opts;

	if (hasmntopt(&m, MNTOPT_SOFT) != NULL) {
		args.flags |= NFSMNT_SOFT;
	}
	if (hasmntopt(&m, MNTOPT_INTR) != NULL) {
		args.flags |= NFSMNT_INT;
	}
#ifdef MNTOPT_SECURE
	if (hasmntopt(&m, MNTOPT_SECURE) != NULL) {
		args.flags |= NFSMNT_SECURE;
		/*
                 * XXX: need to support other netnames outside domain
                 * and not always just use the default conversion
                 */
                if (!host2netname(netname, host, NULL)) {
                        return (NFSERR_NOENT); /* really unknown host */
                }
                args.netname = netname;
	}
#endif MNTOPT_SECURE
	if (port = nopt(&m, "port")) {
		sin.sin_port = htons(port);
	} else {
		sin.sin_port = htons(NFS_PORT);	/* XXX should use portmapper */
	}

	if (args.rsize = nopt(&m, "rsize")) {
		args.flags |= NFSMNT_RSIZE;
	}
	if (args.wsize = nopt(&m, "wsize")) {
		args.flags |= NFSMNT_WSIZE;
	}
	if (args.timeo = nopt(&m, "timeo")) {
		args.flags |= NFSMNT_TIMEO;
	}
	if (args.retrans = nopt(&m, "retrans")) {
		args.flags |= NFSMNT_RETRANS;
	}

	flags = 0;
	flags |= (hasmntopt(&m, MNTOPT_RO) == NULL) ? 0 : M_RDONLY;
	flags |= (hasmntopt(&m, MNTOPT_NOSUID) == NULL) ? 0 : M_NOSUID;

	if (trace > 1) {
		fprintf(stderr, "mount %s %s (%s)\n",
			remname, mntpnt, opts);
	}
#ifdef OLDMOUNT
#define MOUNT_NFS 1
	if (mount(MOUNT_NFS, mntpnt, flags, &args)) {
#else
	if (mount("nfs", mntpnt, flags | M_NEWTYPE, &args)) {
#endif
		syslog(LOG_ERR, "Mount of %s on %s: %m", remname, mntpnt);
		return (NFSERR_IO);
	}
	if (trace > 1) {
		fprintf(stderr, "mount %s OK\n", remname);
	}
	if (*fsp)
		fs = *fsp;
	else {
		fs = alloc_fs(host, dir, mntpnt, opts);
		if (fs == NULL)
			return (NFSERR_NOSPC);
	}
	fs->fs_type = MNTTYPE_NFS;
	fs->fs_mine = 1;
	fs->fs_nfsargs = args;
	fs->fs_mflags = flags;
	fs->fs_nfsargs.hostname = fs->fs_host;
	fs->fs_nfsargs.addr = &fs->fs_addr;
	fs->fs_nfsargs.fh = (caddr_t)&fs->fs_rootfh;
	fs->fs_addr = sin;
	bcopy(&fhs.fhs_fh, &fs->fs_rootfh, sizeof fs->fs_rootfh);
	*fsp = fs;
	return (NFS_OK);
}

nfsstat
remount(fs)
	struct filsys *fs;
{
	char remname[1024];

	if (fs->fs_nfsargs.fh == 0) 
		return nfsmount(fs->fs_host, fs->fs_addr.sin_addr, fs->fs_dir,
				fs->fs_mntpnt, fs->fs_opts, &fs);
	(void) sprintf(remname, "%s:%s", fs->fs_host, fs->fs_dir);
	if (trace > 1) {
		fprintf(stderr, "remount %s %s (%s)\n",
			remname, fs->fs_mntpnt, fs->fs_opts);
	}
#ifdef OLDMOUNT
	if (mount(MOUNT_NFS, fs->fs_mntpnt, fs->fs_mflags, &fs->fs_nfsargs)) {
#else
	if (mount("nfs", fs->fs_mntpnt, fs->fs_mflags | M_NEWTYPE, &fs->fs_nfsargs)) {
#endif
		syslog(LOG_ERR, "Remount of %s on %s: %m", remname,
		    fs->fs_mntpnt);
		return (NFSERR_IO);
	}
	if (trace > 1) {
		fprintf(stderr, "remount %s OK\n", remname);
	}
	return (NFS_OK);
}


/*
 * Add one or more entries to /etc/mtab
 */
void
addtomtab(rootfs)
	struct filsys *rootfs;
{
	FILE *fp;
	struct filsys *fs;
	struct stat stbuf;
	struct mntent mnt;
	char remname[MAXPATHLEN];
	char buf[16], opts[1024];
	int forked;
	
	/*
	 * In SunOs 4.0 only mount and automount flock the mtab.
	 * In releases prior to 4.0 any programs that browse mtab
	 * (df, find etc) also lock the mtab and deadlock with
	 * the automounter is a distinct possibility. Avoid
	 * deadlock here by backgrounding the update.
	 */
	forked = 0;
	if (islocked(MOUNTED)) {
		if (fork())
			return;
		forked++;
	}

	fp = setmntent(MOUNTED, "r+");
	if (fp == NULL) {
		syslog(LOG_ERR, "%s: %m", MOUNTED);
		return;
	}

	for (fs = TAIL(struct filsys, fs_q); fs; fs = PREV(struct filsys, fs)) {
		if (fs->fs_rootfs != rootfs)
			continue;

		(void) sprintf(remname, "%s:%s", fs->fs_host, fs->fs_dir);
		mnt.mnt_fsname = remname;
		mnt.mnt_dir = fs->fs_mntpnt;
		mnt.mnt_type = MNTTYPE_NFS;
		mnt.mnt_freq = mnt.mnt_passno = 0;

		(void) strcpy(opts, fs->fs_opts);
		mnt.mnt_opts = opts;

		if (addmntent(fp, &mnt)) {
			(void) endmntent(fp);
			syslog(LOG_ERR, "%s: %m", MOUNTED);
			return;
		}
	}
	(void) endmntent(fp);
	if (stat(MOUNTED, &stbuf) < 0)
		syslog(LOG_ERR, "%s: %m", MOUNTED);
	else
		last_mtab_time = stbuf.st_mtime;

	if (forked)
		_exit(0);
}

#include <sys/file.h>
#include <fcntl.h>
/*
 * Check whether the file is flock'ed.
 */
int
islocked(file)
	char *file;
{
	int f;

	f = open(file, O_RDONLY);
	if (f < 0) {
		syslog(LOG_ERR, "%s: %m", MOUNTED);
		return 1;
	}
	if (flock(f, LOCK_EX | LOCK_NB) < 0) {
		sleep(1);
		if (flock(f, LOCK_EX | LOCK_NB) < 0) {
			(void) close(f);
			return 1;
		}
	}
	(void) close(f);	/* close and release the lock */
	return 0;
}

/*
 * Remove one or more entries from the mount table.
 * If mntpnt is non-null then remove the entry
 * for that mount point.
 * Otherwise use rootfs - it is the root fs of
 * a mounted hierarchy.  Remove all entries for
 * the hierarchy.
 */
void
clean_mtab(mntpnt, rootfs)
	char *mntpnt;
	struct filsys *rootfs;
{
	FILE *fold, *fnew;
	struct mntent *mnt;
	struct stat stbuf;
	struct filsys *fs;
	char tmpnam[64];
	int delete;

	fold = setmntent(MOUNTED, "r+");
	if (fold == NULL) {
		syslog(LOG_ERR, "%s: %m", MOUNTED);
		return;
	}
	(void) sprintf(tmpnam, "%s.XXXXXX", MOUNTED);
	(void) mktemp(tmpnam);
	fnew = setmntent(tmpnam, "w");
	if (fnew == NULL) {
		syslog(LOG_ERR, "%s: %m", tmpnam);
		(void) endmntent(fold);
		return;
	}

	while (mnt = getmntent(fold)) {

		if (mntpnt) {
			if (strcmp(mnt->mnt_dir, mntpnt) == 0)
				continue;
		} else {
			delete = 0;
			for (fs = rootfs ; fs ; fs = NEXT(struct filsys, fs)) {
				if (strcmp(mnt->mnt_dir, fs->fs_mntpnt) == 0) {
					delete = 1;
					break;
				}
			}
			if (delete)
				continue;
		}
		if (addmntent(fnew, mnt)) {
			syslog(LOG_ERR, "addmntent %s: %m", tmpnam);
			(void) endmntent(fold);
			(void) endmntent(fnew);
			(void) unlink(tmpnam);
			return;
		}
	}

	(void) endmntent(fold);
	(void) endmntent(fnew);
	if (rename(tmpnam, MOUNTED) < 0) {
		syslog(LOG_ERR, "rename %s %s: %m", tmpnam, MOUNTED);
		(void) unlink(tmpnam);
		return;
	}
	if (stat(MOUNTED, &stbuf) < 0)
		syslog(LOG_ERR, "%s: %m", MOUNTED);
	else
		last_mtab_time = stbuf.st_mtime;
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
			syslog(LOG_ERR, "Bad numeric option '%s'", str);
		}
	}
	return (val);
}
