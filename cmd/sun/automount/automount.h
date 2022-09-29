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
/* $Header: automount.h,v 1.2.1.5 90/05/22 18:25:11 wje Exp $ */
/*
 * @(#)automount.h	1.2 89/04/28 4.0NFSSRC SMI
 *
 * automount.h 1.9 88/12/14 Copyright 1987 Sun Microsystems, Inc.
 */

/* 3.x compatibility */
#ifdef OLDMOUNT
#define LOG_DAEMON 0
#define MAXHOSTNAMELEN  64
#define MAXNETNAMELEN   255
#endif OLDMOUNT

#define	FH_HASH_SIZE	8

/*
 * General queue structure 
 */
struct queue {
	struct queue	*q_next;
#define	q_head	q_next
	struct queue	*q_prev;
#define	q_tail	q_prev
};

#define	INSQUE(head, ptr) my_insque(&(head), &(ptr)->q)
#define	REMQUE(head, ptr) my_remque(&(head), &(ptr)->q)
#define HEAD(type, head) ((type *)(head.q_head))
#define NEXT(type, ptr)	((type *)(ptr->q.q_next))
#define	TAIL(type, head) ((type *)(head.q_tail))
#define PREV(type, ptr)	((type *)(ptr->q.q_prev))
	

/*
 * Types of filesystem entities (vnodes)
 * We support only one level of DIR; everything else is a symbolic LINK
 */
enum vn_type { VN_DIR, VN_LINK};
struct vnode {
	struct queue q;
	nfs_fh	vn_fh;		/* fhandle */
	struct fattr vn_fattr;	/* file attributes */
	enum vn_type vn_type;	/* type of vnode */
	caddr_t	vn_data;	/* vnode private data */
};

struct vnode *fhtovn();		/* lookup vnode given fhandle */

/*
 * Structure describing a host/filesystem/dir tuple in a YP map entry
 */
struct mapfs {
	struct mapfs *mfs_next;		/* next in entry */
	char	*mfs_host;		/* host name */
	struct in_addr mfs_addr;	/* address of host */
	char	*mfs_dir;		/* dir to mount */
	char	*mfs_subdir;		/* subdir of dir */
};

/*
 * YP entry - lookup of name in DIR gets us this
 */
struct mapent {
	char	*map_root;		/* "/hostname" */
	char	*map_mntpnt;		/* "/partition" */
	char	*map_mntopts;
	struct mapfs *map_fs;
	struct mapent *map_next;
};
struct mapent *getmapent();

/*
 * Everthing we know about a mounted filesystem
 * Can include things not mounted by us (fs_mine == 0)
 */
struct filsys {
	struct queue q;			/* next in queue */
	int	fs_death;		/* time when no longer valid */
	int	fs_mine;		/* 1 if we mounted this fs */
	int	fs_present;		/* for checking unmounts */
	char	*fs_type;		/* type of filesystem */
	char	*fs_host;		/* host name */
	char	*fs_dir;		/* dir of host mounted */
	char	*fs_mntpnt;		/* local mount point */
	char	*fs_opts;		/* mount options */
	struct nfs_args fs_nfsargs;	/* nfs mount args */
	struct sockaddr_in fs_addr;	/* host address */
	struct filsys *fs_rootfs;	/* root for this hierarchy */
	nfs_fh	fs_rootfh;		/* file handle for nfs mount */
	int	fs_mflags;		/* mount flags */
};
struct queue fs_q;
struct filsys *already_mounted(), *alloc_fs();

/*
 * Structure for recently referenced links
 */
struct link {
	struct queue q;		/* next in queue */
	struct vnode link_vnode;	/* space for vnode */
	struct autodir *link_dir;	/* dir which we are part of */
	char	*link_name;	/* this name in dir */
	struct filsys *link_fs;	/* mounted file system */
	char	*link_path;	/* dir within file system */
	long	link_death;	/* time when no longer valid */
};
struct link *makelink();
struct link *findlink();
	
/*
 * Descriptor for each directory served by the automounter 
 */
struct autodir {
	struct queue q;
	struct	vnode dir_vnode;	/* vnode */
	char	*dir_name;	/* mount point */
	char	*dir_map;	/* name of map for dir */
	char	*dir_opts;	/* default mount options */
	int	dir_remove;	/* remove mount point */
	struct queue dir_head;
};
struct queue dir_q;

char self[64];		/* my hostname */
char mydomain[64];	/* my domain name */
char tmpdir[200];	/* real name of /tmp */
#if defined(RISCOS)
char rootdir[200];	/* added name for root mount pt below hostname */
char access_point[200]; /* mount point which automounter gains control over */
int  suppress_access_name;	/* include access name in generated mount pt */
#define TMPDIR "/tmp_dir"
#if defined(LOOP_BACK)
int  loop_back;		/* generate ability to have two access points */
#endif /* LOOP_BACK */
#endif /* RISCOS */
struct in_addr my_addr;	/* my inet address */

char *malloc(), *index(), *strdup();
time_t time_now;		/* set at start of processing of each RPC call */
int mount_timeout;	/* max seconds to wait for mount */
int max_link_time;	/* seconds to keep link around */
int nomounts;		/* don't do any mounts - for cautious servers */
nfsstat lookup(), nfsmount();
