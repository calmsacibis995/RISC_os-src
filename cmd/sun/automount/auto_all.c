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
#ident	"$Header: auto_all.c,v 1.3.1.5 90/05/22 18:24:20 wje Exp $"
/*
 * @(#)auto_all.c	1.4 89/04/28 4.0NFSSRC Copyr 1988 Sun Micro
 *
 * Copyright (c) 1987 by Sun Microsystems, Inc.
 */

#include <stdio.h>
#include <ctype.h>
#include <syslog.h>
#include <sys/types.h>
#include <sys/errno.h>
#include <sys/time.h>
#include <sys/stat.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <rpc/rpc.h>
#include <rpc/pmap_clnt.h>
#include <mntent.h>
#include <netdb.h>
#include <errno.h>
#include "nfs_prot.h"
#define NFSCLIENT
typedef nfs_fh fhandle_t;
#include <rpcsvc/mount.h>
#include <sys/mount.h>
#if defined(RISCOS) && defined(LOOP_BACK)
#include <sys/param.h>
#endif /* RISCOS && LOOP_BACK */
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

do_unmount(fsys)
	struct filsys *fsys;
{
	struct queue tmpq;
	struct filsys *fs, *nextfs;
	nfsstat remount();
	extern int trace;
#if defined(RISCOS) && defined(LOOP_BACK)
	static char linkbuf[MAXPATHLEN];
	struct stat tbuf;
#endif /* RISCOS && LOOP_BACK */

	tmpq.q_head = tmpq.q_tail = NULL;
	for (fs = HEAD(struct filsys, fs_q); fs; fs = nextfs) {
		nextfs = NEXT(struct filsys, fs);
		if (fs->fs_rootfs == fsys) {
			REMQUE(fs_q, fs);
			INSQUE(tmpq, fs);
		}
	}
	/* walk backwards trying to unmount */
	for (fs = TAIL(struct filsys, tmpq); fs; fs = nextfs) {
		nextfs = PREV(struct filsys, fs);
		if (trace > 1) {
			fprintf(stderr, "unmount %s ", fs->fs_mntpnt);
#if !defined(RISCOS)
			fflush(stderr);
#endif
		}
		if (unmount(fs->fs_mntpnt) < 0) {
			if (trace > 1)
				fprintf(stderr, "BUSY\n");
			/* remount previous unmounted ones */
			for (fs = NEXT(struct filsys, fs); fs; fs = nextfs) {
				nextfs = NEXT(struct filsys, fs);
				(void) remount(fs);
			}
			goto inuse;
		}
		if (trace > 1)
			fprintf(stderr, "OK\n");
	}
	/* all ok - walk backwards removing directories */
	clean_mtab(0, HEAD(struct filsys, tmpq));
	for (fs = TAIL(struct filsys, tmpq); fs; fs = nextfs) {
		nextfs = PREV(struct filsys, fs);
		nfsunmount(fs);
#if defined(RISCOS)
		/* Only remove if original name */
		if (!strcmp(tmpdir, TMPDIR))
			safe_rmdir(fs->fs_mntpnt);
#else
		safe_rmdir(fs->fs_mntpnt);
#endif /* RISCOS */
#if defined(RISCOS) && defined(LOOP_BACK)
		if (loop_back && 
		    stat(fs->fs_mntpnt, &tbuf) &&
		    !strcmp(fs->fs_dir, "/")) {
			sprintf(linkbuf,"%s/%s", access_point ,fs->fs_host);
			if (symlink(linkbuf, fs->fs_mntpnt))
				syslog(LOG_ERR,
				       "failed to create symlink: %s: %m\n",
				       linkbuf);
		}
#endif /* RISCOS && LOOP_BACK */
		REMQUE(tmpq, fs);
		INSQUE(fs_q, fs);
		free_filsys(fs);
	}
	/* success */
	return (1);

inuse:	/* put things back on the correct list */
	for (fs = HEAD(struct filsys, tmpq); fs; fs = nextfs) {
		nextfs = NEXT(struct filsys, fs);
		REMQUE(tmpq, fs);
		INSQUE(fs_q, fs);
	}
	return (0);
}

freeex(ex)
	struct exports *ex;
{
	struct groups *groups, *tmpgroups;
	struct exports *tmpex;

	while (ex) {
		free(ex->ex_name);
		groups = ex->ex_groups;
		while (groups) {
			tmpgroups = groups->g_next;
			free((char *)groups);
			groups = tmpgroups;
		}
		tmpex = ex->ex_next;
		free((char *)ex);
		ex = tmpex;
	}
}

mkdir_r(dir)
	char *dir;
{
	int err;
	char *slash;
	char *rindex();

	if (mkdir(dir, 0555) == 0)
		return (0);
	if (errno != ENOENT)
		return (-1);
	slash = rindex(dir, '/');
	if (slash == NULL)
		return (-1);
	*slash = '\0';
	err = mkdir_r(dir);
	*slash++ = '/';
	if (err || !*slash)
		return (err);
	return mkdir(dir, 0555);
}

safe_rmdir(dir)
	char *dir;
{
	dev_t tmpdev;
	struct stat stbuf;

	if (stat(tmpdir, &stbuf))
		return;
	tmpdev = stbuf.st_dev;

	if (stat(dir, &stbuf))
		return;
	if (tmpdev == stbuf.st_dev)
		(void) rmdir(dir);
}
