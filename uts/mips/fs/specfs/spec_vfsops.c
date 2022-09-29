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
#ident	"$Header: spec_vfsops.c,v 1.2.1.4 90/06/05 16:51:14 wje Exp $"
/* Original include:
*  param.h systm.h user.h buf.h vfs.h vnode.h bootconf.h
*  ../specfs/snode.h reboot.h
*/
#include "sys/types.h"
#include "sys/signal.h"		/* needed by user.h */
#include "sys/pcb.h"		/* needed by user.h */
#include "sys/user.h"
#include "sys/buf.h"
#include "sys/vfs.h"
#include "sys/vnode.h"
#include "sys/systm.h"
#include "sys/errno.h"
#include "sys/bootconf.h"
#include "sys/sysmacros.h"
#include "sys/fs/snode.h"

extern	int spec_sync();	/* XXX - should be static */
/*
 * Used in spec_vnodeops.c
 */
int spec_badop();
static	int spec_mountroot();
static	int spec_swapvp();

struct vfsops spec_vfsops = {
	spec_badop,		/* mount */
	spec_badop,		/* unmount */
	spec_badop,		/* root */
	spec_badop,		/* statfs */
	spec_sync,
	spec_badop,		/* vget */
	spec_mountroot,
	spec_swapvp,
};

int
spec_badop()
{

	panic("spec_badop");
}

/*
 * Run though all the snodes and force write back
 * of all dirty pages on the block devices.
 */
/*ARGSUSED*/
static int
spec_sync(vfsp)
	struct vfs *vfsp;
{
	static int spec_lock;
	register struct snode **spp, *sp;
	register struct vnode *vp;

	if (spec_lock)
		return (0);

	spec_lock++;
	if (vfsp == (struct vfs *) NULL) {
	    bflush((struct vnode *)NULL);
	} else {
	    for (spp = stable; spp < &stable[STABLESIZE]; spp++) {
		for (sp = *spp; sp != (struct snode *)NULL; sp = sp->s_next) {
			vp = STOV(sp);
			/*
			 * Don't bother sync'ing a vp if it
			 * is part of virtual swap device.
			 */
			if (((vp)->v_flag & VISSWAP) != 0)
				continue;
			if (vp->v_type == VBLK) {
				bflush(vp);	/* start delayed writes */
			}
		}
	    }
	}
	spec_lock = 0;
	return (0);
}

/*ARGSUSED*/
static int
#if RISCOS
spec_mountroot(vfsp, vpp, name, op)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
	mountrootop_t op;
#else
spec_mountroot(vfsp, vpp, name)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
#endif /* RISCOS */
{

	return (EINVAL);
}

/*ARGSUSED*/
static int
spec_swapvp(vfsp, vpp, name)
	struct vfs *vfsp;
	struct vnode **vpp;
	char *name;
{
	extern char *strcpy();
	extern struct vnodeops spec_vnodeops;
	dev_t dev;

	if (*name == '\0') {
		/*
		 * No swap name specified, use root dev partition "b"
		 * if it is a block device, otherwise fail.
		 * XXX - should look through device list or something here
		 * if root is not local.
		 */
#ifdef RISCOS
		if (rootvp->v_op == &spec_vnodeops) {
#else
		if (rootvp->v_op == &spec_vnodeops &&
		    (boothowto & RB_ASKNAME) == 0) {
#endif
			dev = makedev(major(rootvp->v_rdev), 1);
			(void) strcpy(name, rootfs.bo_name);
		} 
#ifdef sun
		else {
	extern dev_t getblockdev();
retry:
			if (!(dev = getblockdev("swap", name))) {
				return (ENODEV);
			}
			/*
			 * Check for swap on root device
			 */
			if (rootvp->v_op == &spec_vnodeops &&
			    dev == rootvp->v_rdev) {
				char resp[128];

				printf("Swapping on root device, ok? ");
				gets(resp);
				if (*resp != 'y' && *resp != 'Y') {
					goto retry;
				}
			}
		}
#endif
		name[3] = 'b';
	} else {
		if (name[2] == '\0') {
			/*
			 * Name doesn't include device number, default to
			 * device 0.
			 */
			name[2] = '0';
			name[3] = '\0';
		}
		if (name[3] == '\0') {
			/*
			 * name doesn't include partition, default to
			 * partition b.
			 */
			name[3] = 'b';
			name[4] = '\0';
		}
#ifdef sun
		if (!(dev = getblockdev("swap", name))) {
			return (ENODEV);
		}
#endif
	}
	*vpp = bdevvp(dev);
	return (0);
}
