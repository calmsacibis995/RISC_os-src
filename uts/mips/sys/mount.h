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
/* $Header: mount.h,v 1.12.1.4 90/05/10 06:29:10 wje Exp $ */

/*	@(#)mount.h	1.3 88/05/18 4.0NFSSRC SMI	*/
/*	@(#)mount.h 1.1 86/02/03 SMI	*/

#ifndef	_SYS_MOUNT_
#define	_SYS_MOUNT_	1

/* Flag bits passed to the (old) System V mount system call */
#define	MS_RDONLY	0x1	/* read only bit */
#define MS_FSS		0x2	/* FSS (4-argument) mount */

/*
 * mount options
 */
#define M_RDONLY	0x01		/* mount fs read only */
#define M_NOSUID	0x02		/* mount fs with setuid not allowed */
#define	M_NEWTYPE	0x04		/* use type string instead of int */
#define	M_GRPID		0x08		/* Old BSD group-id on create */
#define M_REMOUNT	0x10		/* change opts on an existing mount */
#define M_NOSUB		0x20		/* Disallow mounts under this mount */
#define M_MULTI		0x40		/* Multi-component lookup on files */
#define M_NFSSYNC	0x40000000	/* NFS option: synchronous writes */
#define M_NFSASYNC	0x20000000	/* NFS option: asynchronous writes */

#ifdef KERNEL
/*
 * File system types, these correspond to entries in fsconf
 */
#define MOUNT_SPEC	0
#define	MOUNT_UFS	1
#define	MOUNT_NFS	2
#define	MOUNT_PC	3
#define	MOUNT_LO	4
#define	MOUNT_PROC	5
#define MOUNT_RFS	6
#define	MOUNT_MAXTYPE	6
#endif KERNEL

struct ufs_args {
	char	*fspec;
};

#ifdef NFSCLIENT

struct nfs_args {
	struct sockaddr_in	*addr;		/* file server address */
	caddr_t			fh;		/* File handle to be mounted */
	int			flags;		/* flags */
	int			wsize;		/* write size in bytes */
	int			rsize;		/* read size in bytes */
	int			timeo;		/* initial timeout in .1 secs */
	int			retrans;	/* times to retry send */
	char			*hostname;	/* server's hostname */
	int			acregmin;	/* attr cache file min secs */
	int			acregmax;	/* attr cache file max secs */
	int			acdirmin;	/* attr cache dir min secs */
	int			acdirmax;	/* attr cache dir max secs */
	char			*netname;	/* server's netname */
};

/*
 * NFS mount option flags
 */
#define	NFSMNT_SOFT	0x001	/* soft mount (hard is default) */
#define	NFSMNT_WSIZE	0x002	/* set write size */
#define	NFSMNT_RSIZE	0x004	/* set read size */
#define	NFSMNT_TIMEO	0x008	/* set initial timeout */
#define	NFSMNT_RETRANS	0x010	/* set number of request retrys */
#define	NFSMNT_HOSTNAME	0x020	/* set hostname for error printf */
#define	NFSMNT_INT	0x040	/* allow interrupts on hard mount */
#define	NFSMNT_NOAC	0x080	/* don't cache attributes */
#define	NFSMNT_ACREGMIN	0x0100	/* set min secs for file attr cache */
#define	NFSMNT_ACREGMAX	0x0200	/* set max secs for file attr cache */
#define	NFSMNT_ACDIRMIN	0x0400	/* set min secs for dir attr cache */
#define	NFSMNT_ACDIRMAX	0x0800	/* set max secs for dir attr cache */
#define NFSMNT_SECURE	0x1000	/* secure mount */
#endif NFSCLIENT

#ifdef PCFS
struct pc_args {
	char	*fspec;
};
#endif PCFS

#ifdef LOFS
struct lo_args {
	char    *fsdir;
};
#endif LOFS

#ifdef RFS

struct token {
	int	t_id;	 /* token id for differentiating multiple ckts	*/
	char	t_uname[64]; /* full domain name of machine, 64 = MAXDNAME */
};

struct rfs_args {
	char    *rmtfs;		/* name of service (fs) */
	struct token *token;	/* identifier of remote mach */
};

/*
 * RFS mount option flags
 */
#define RFS_RDONLY	0x001	/* read-only: passed w/ remote mount request */
#endif

#endif	_SYS_MOUNT_
