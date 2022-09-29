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
/* $Header: mntent.h,v 1.12.1.4 90/05/10 01:04:44 wje Exp $ */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef	_SUN_MNTENT_
#define	_SUN_MNTENT_	1

/*	@(#)mntent.h 1.1 86/02/03 SMI	*/
/* @(#)mntent.h	2.1 86/04/14 NFSSRC */

/*
 * File system table, see mntent (5)
 *
 * Used by dump, mount, umount, swapon, fsck, df, ...
 *
 * Quota files are always named "quotas", so if type is "rq",
 * then use concatenation of mnt_dir and "quotas" to locate
 * quota file.
 */

#ifdef SYSTYPE_SYSV
#include <sys/fsid.h>
#endif SYSTYPE_SYSV

#define	MNTTAB		"/etc/fstab"
#define	MOUNTED		"/etc/mtab"

#define	MNTMAXSTR	128

#define MNTTYPE_PIPE	"com"	/* pipe pseudo-filesystem */
#define MNTTYPE_BELL	"S51K"	/* bell (att s5) filesystem */
#define MNTTYPE_PROC	"PROC"	/* process pseudo-filesystem */
#define MNTTYPE_SOCKET	"socket"/* socket pseudo filesystem */
#define MNTTYPE_42	"4.2"	/* 4.2 file system */
#define MNTTYPE_43	"4.3"	/* 4.3 file system */
#define MNTTYPE_FFS	"ffs"	/* RISCOS file system */
#define MNTTYPE_UFS	"ufs"	/* RISCOS, NFS dir?, SVR* file sys */
#define MNTTYPE_NFS	"nfs"	/* network file system */
#define	MNTTYPE_EFS	"efs"	/* extent filesystem */
#define	MNTTYPE_EFS2	"efs2"	/* longname directory efs */
#define MNTTYPE_RFS	"rfs"	/* remote file system: SVR4? */
#define MNTTYPE_PC	"pc"	/* IBM PC (MSDOS) file system */
#define	MNTTYPE_SWAP	"swap"	/* swap file system */
#define	MNTTYPE_IGNORE	"ignore"/* No type specified, ignore this entry */
#define MNTTYPE_LO	"lo"	/* Loopback file system*/

#define MNTOPT_RO	"ro"		/* read only */
#define MNTOPT_RW	"rw"		/* read/write */
#define MNTOPT_RQ	"rq"		/* read/write with quotas */
#define MNTOPT_QUOTA	"quota"		/* quotas */
#define	MNTOPT_NOQUOTA	"noquota"	/* no quotas */
#define MNTOPT_SOFT	"soft"		/* soft mount */
#define MNTOPT_HARD	"hard"		/* hard mount */
#define MNTOPT_NOSUID	"nosuid"	/* no set uid allowed */
#define MNTOPT_NOAUTO	"noauto"	/* hide entry from mount -a */
#define MNTOPT_HIDE	"hide"		/* same as noauto: for back. compat. */
#define MNTOPT_INTR	"intr"		/* allow interrupts on hard mount */
#define	MNTOPT_SECURE	"secure"	/* use secure RPC for NFS */
#define	MNTOPT_GRPID	"grpid"		/* SysV-compatible group-id on create*/
#define MNTOPT_REMOUNT  "remount"	/* change options on previous mount */
#define MNTOPT_NOSUB    "nosub" 	/* disallow mounts beneath this one */
#define MNTOPT_MULTI    "multi"		/* Do multi-component lookup */
#define MNTOPT_NFSSYNC	"nfs_sync"	/* NFS option: synchronous writes */
#define MNTOPT_NFSASYNC "nfs_async"	/* NFS option: asynchronous writes */

#define	MNTOPT_RAW	"raw"		/* raw device name */
#define	MNTOPT_FSCK	"fsck"		/* fsck by default */
#define	MNTOPT_NOFSCK	"nofsck"	/* do not fsck */

#define IS_RISCOS_FS_TYPE(_type) \
	(strcmp(_type, MNTTYPE_FFS) == 0 || \
	 strcmp(_type, MNTTYPE_UFS) == 0 || \
	 strcmp(_type, MNTTYPE_43)  == 0)
#define HAS_RISCOS_QUOTAS(_mnt) \
	(hasmntopt(_mnt, MNTOPT_RQ) || hasmntopt(_mnt, MNTOPT_QUOTA))


struct	mntent{
	char	*mnt_fsname;		/* name of mounted file system */
	char	*mnt_dir;		/* file system path prefix */
	char	*mnt_type;		/* MNTTYPE_* */
	char	*mnt_opts;		/* MNTOPT* */
	int	mnt_freq;		/* dump frequency, in days */
	int	mnt_passno;		/* pass number on parallel fsck */
};

struct	mntent *getmntent();
char	*hasmntopt();
FILE	*setmntent();
int	endmntent();

#endif /* _SUN_MNTENT_ */
