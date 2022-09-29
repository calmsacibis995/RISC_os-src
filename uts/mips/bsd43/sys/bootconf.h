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
/* $Header: bootconf.h,v 1.2.1.2 90/05/10 04:48:45 wje Exp $ */
#ifndef _BSD43_SYS_BOOTCONF_
#define _BSD43_SYS_BOOTCONF_	1

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Boot time configuration information objects
 */
#define	MAXFSNAME	16
#define	MAXOBJNAME	128
/*
 * Boot configuration information
 */
struct bsd43_(bootobj) {
	char	bo_fstype[MAXFSNAME];	/* filesystem type name (e.g. nfs) */
	char	bo_name[MAXOBJNAME];	/* name of object */
	int	bo_flags;		/* flags, see below */
	int	bo_size;		/* number of blocks */
	struct vnode *bo_vp;	/* vnode of object */
};

/*
 * flags
 */
#define	BO_VALID	0x01	/* all information in object is valid */
#define	BO_BUSY		0x02	/* object is busy */

extern struct bsd43_(bootobj) bsd43_(rootfs);
extern struct bsd43_(bootobj) bsd43_(dumpfile);
extern struct bsd43_(bootobj) bsd43_(argsfile);
extern struct bsd43_(bootobj) bsd43_(swaptab[]);
extern int bsd43_(Nswaptab);

#endif	_BSD43_SYS_BOOTCONF_
