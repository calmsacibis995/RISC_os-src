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
/* $Header: mount.h,v 1.7.1.5 90/05/10 04:52:35 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * mount options
 */
#define BSD43_M_RDONLY	0x01		/* mount fs read only */
#define BSD43_M_NOSUID	0x02		/* mount fs with setuid not allowed */
#define	BSD43_M_NEWTYPE	0x04		/* use type string instead of int */
#define	BSD43_M_GRPID	0x08		/* Old BSD group-id on create */
#define BSD43_M_REMOUNT	0x10		/* change opts on an existing mount */
#define BSD43_M_NOSUB	0x20		/* Disallow mounts under this mount */
#define BSD43_M_MULTI	0x40		/* Multi-component lookup on files */
#define BSD43_M_NFSSYNC 0x40000000	/* NFS option: synchronous writes */
#define BSD43_M_NFSASYNC 0x20000000	/* NFS option: asynchronous writes */


#ifdef KERNEL
/*
 * File system types, these correspond to entries in fsconf
 */
#define	BSD43_MOUNT_UFS	1
#define	BSD43_MOUNT_NFS	2
#define	BSD43_MOUNT_PC	3
#define	BSD43_MOUNT_LO	4
#define	BSD43_MOUNT_MAXTYPE	5
#endif KERNEL

struct bsd43_(ufs_args) {
	char	*fspec;
};

struct bsd43_(proc_args) {
	char	*fspec;
};

#ifdef NFSCLIENT

struct bsd43_(nfs_args) {
	struct bsd43_(sockaddr_in)	*addr;	/* file server address */
	caddr_t			fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	int			wsize;		/* write size in bytes */
	int			rsize;		/* read size in bytes */
	int			timeo;		/* initial timeout in .1 secs */
	int			retrans;	/* times to retry send */
	char			*bsd43_(hostname);	/* server's hostname */
	int			acregmin;	/* attr cache file min secs */
	int			acregmax;	/* attr cache file max secs */
	int			acdirmin;	/* attr cache dir min secs */
	int			acdirmax;	/* attr cache dir max secs */
	char			*netname;	/* server's netname */
};

/*
 * NFS mount option flags
 */
#define	BSD43_NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	BSD43_NFSMNT_WSIZE	0x002	/* set write size */
#define	BSD43_NFSMNT_RSIZE	0x004	/* set read size */
#define	BSD43_NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	BSD43_NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	BSD43_NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	BSD43_NFSMNT_INT	0x040	/* allow interrupts on hard mount */
#define	BSD43_NFSMNT_NOAC	0x080	/* don't cache attributes */
#define	BSD43_NFSMNT_ACREGMIN	0x0100	/* set min secs for file attr cache */
#define	BSD43_NFSMNT_ACREGMAX	0x0200	/* set max secs for file attr cache */
#define	BSD43_NFSMNT_ACDIRMIN	0x0400	/* set min secs for dir attr cache */
#define	BSD43_NFSMNT_ACDIRMAX	0x0800	/* set max secs for dir attr cache */
#define BSD43_NFSMNT_SECURE	0x1000	/* secure mount */
#endif NFSCLIENT

#ifdef PCFS
struct bsd43_(pc_args) {
	char	*fspec;
};
#endif PCFS

#ifdef LOFS
struct bsd43_(lo_args) {
	char    *fsdir;
};
#endif LOFS

#ifdef RFS

struct bsd43_(token) {
	int	t_id;	 /* token id for differentiating multiple ckts	*/
	char	t_uname[64]; /* full domain name of machine, 64 = MAXDNAME */
};

struct bsd43_(rfs_args) {
	char    *rmtfs;		/* name of service (fs) */
	struct bsd43_(token) *token;	/* identifier of remote mach */
};

/*
 * RFS mount option flags
 */
#define BSD43_RFS_RDONLY	0x001	/* read-only: passed with remote mount request */
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * These defines strip the "BSD43_" and "bsd43_" prefixes from the names
 * above so that these files may be included in standard BSD code.
 */
#ifdef SYSTYPE_BSD43
#   define M_RDONLY	BSD43_M_RDONLY
#   define M_NOSUID	BSD43_M_NOSUID
#   define M_NEWTYPE	BSD43_M_NEWTYPE
#   define M_GRPID	BSD43_M_GRPID
#   define M_REMOUNT	BSD43_M_REMOUNT
#   define M_NOSUB	BSD43_M_NOSUB
#   define M_MULTI	BSD43_M_MULTI
#   define M_NFSSYNC	BSD43_M_NFSSYNC
#   define M_NFSASYNC	BSD43_M_NFSASYNC
#   define MOUNT_UFS	BSD43_MOUNT_UFS
#   define MOUNT_NFS	BSD43_MOUNT_NFS
#   define MOUNT_PC	BSD43_MOUNT_PC
#   define MOUNT_LO	BSD43_MOUNT_LO
#   define MOUNT_MAXTYPE	BSD43_MOUNT_MAXTYPE
#   define NFSMNT_SOFT	BSD43_NFSMNT_SOFT
#   define NFSMNT_WSIZE	BSD43_NFSMNT_WSIZE
#   define NFSMNT_RSIZE	BSD43_NFSMNT_RSIZE
#   define NFSMNT_TIMEO	BSD43_NFSMNT_TIMEO
#   define NFSMNT_RETRANS	BSD43_NFSMNT_RETRANS
#   define NFSMNT_HOSTNAME	BSD43_NFSMNT_HOSTNAME
#   define NFSMNT_INT	BSD43_NFSMNT_INT
#   define NFSMNT_NOAC	BSD43_NFSMNT_NOAC
#   define NFSMNT_ACREGMIN	BSD43_NFSMNT_ACREGMIN
#   define NFSMNT_ACREGMAX	BSD43_NFSMNT_ACREGMAX
#   define NFSMNT_ACDIRMIN	BSD43_NFSMNT_ACDIRMIN
#   define NFSMNT_ACDIRMAX	BSD43_NFSMNT_ACDIRMAX
#   define NFSMNT_SECURE	BSD43_NFSMNT_SECURE
#   define RFS_RDONLY	BSD43_RFS_RDONLY
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) ----------------------------------*/

