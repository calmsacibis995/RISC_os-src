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
#ident	"$Header: vfs_conf.c,v 1.5.1.2 90/05/10 05:58:54 wje Exp $"

/*	@(#)vfs_conf.c	2.2 88/06/13 4.0NFSSRC SMI;  from SMI 1.6 87/01/27	*/

/* Originally included  param.h vfs.h bootconf.h */

#include "sys/types.h"
#include "sys/vfs.h"
#include "sys/bootconf.h"
#include "sys/errno.h"

extern	struct vfsops ufs_vfsops;	/* XXX Should be ifdefed */
#ifdef RISCOS
extern	struct vfsops proc_vfsops;
#endif

#ifdef NFSCLIENT
extern	struct vfsops nfs_vfsops;
#endif

#ifdef PCFS
extern	struct vfsops pcfs_vfsops;
#endif

#ifdef LOFS
extern	struct vfsops lo_vfsops;
#endif

#ifdef RFS
extern	struct vfsops rfs_vfsops;
#endif

extern	struct vfsops spec_vfsops;

static int nofs_badop();

struct vfsops nofs_vfsops = {
	nofs_badop,
	nofs_badop,
	nofs_badop,
	nofs_badop,
	nofs_badop,
	nofs_badop,
	nofs_badop,
	nofs_badop,
};

static int
nofs_badop()
{
	return (EINVAL);
}

/* 
 * WARNING: THE POSITIONS OF FILESYSTEM TYPES IN THIS TABLE SHOULD NOT
 * BE CHANGED. These positions are used in generating fsids and fhandles.
 * Thus, changing positions will cause a server to change the fhandle it
 * gives out for a file.
 */

struct vfssw vfssw[] = {
	/* [MOUNT_SPEC]: */
	"spec", &spec_vfsops,		/* SPEC */
	/* [MOUNT_UFS]: */
#if RISCOS
	"ffs", &ufs_vfsops,		/* UFS */
#else
	"4.3", &ufs_vfsops,		/* UFS */
#endif /* RISCOS */
	/* [MOUNT_NFS]: */
#ifdef NFSCLIENT
	"nfs", &nfs_vfsops,		/* NFS */
#else
	(char *)0, &nofs_vfsops,
#endif
	/* [MOUNT_PC]: */
#ifdef PCFS
	"pc", &pcfs_vfsops,		/* PC */
#else
	(char *)0, &nofs_vfsops,
#endif
	/* [MOUNT_LO]: */
#ifdef LOFS
	"lo", &lo_vfsops,		/* LOopback */
#else
	(char *)0, &nofs_vfsops,
#endif
	/* [MOUNT_PROC]: */
	"PROC", &proc_vfsops,		/* PROC */
	/* [MOUNT_RFS]: */
#ifdef RFS
	"rfs", &rfs_vfsops,		/* RFS */
#else
	(char *)0, &nofs_vfsops,
#endif
#if RISCOS
	"4.3", &ufs_vfsops,		/* alias for UFS */
	"ufs", &ufs_vfsops,		/* alias for UFS */
#endif /* RISCOS */
};

#define	NVFS	(sizeof vfssw / sizeof vfssw[0])

struct vfssw *vfsNVFS = &vfssw[NVFS];

struct bootobj rootfs = {
#if RISCOS
	{ "ffs",           "",     0, 0, (struct vnode *)0 },
#else
	{ "ufs",           "",     0, 0, (struct vnode *)0 },
#endif
};
struct bootobj dumpfile = {
	{ "",           "",     0, 0, (struct vnode *)0 },
};
