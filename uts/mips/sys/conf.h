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
/* $Header: conf.h,v 1.14.3.2 90/05/10 06:07:59 wje Exp $ */

#ifndef	_SYS_CONF_
#define	_SYS_CONF_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Declaration of block device switch. Each entry (row) is
 * the only link between the main unix code and the driver.
 * The initialization of the device switches is in the file conf.c.
 */
struct bdevsw {
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_strategy)();
	int	(*d_print)();
	int	(*d_dump)();
	int	(*d_size)();
};
extern struct bdevsw bdevsw[];

/*
 * Character device switch.
 */
struct cdevsw {
	int	(*d_open)();
	int	(*d_close)();
	int	(*d_read)();
	int	(*d_write)();
	int	(*d_ioctl)();
	struct tty *d_ttys;
	struct streamtab *d_str;
	int	(*d_mmap)();
	int	(*d_select)();
};
extern struct cdevsw cdevsw[];

#define	FMNAMESZ	8

struct fmodsw {
	char	f_name[FMNAMESZ+1];
	struct  streamtab *f_str;
};
extern struct fmodsw fmodsw[];

extern int	bdevcnt;
extern int	cdevcnt;
extern int	fmodcnt;


/*file system switch structure */
struct fstypsw {
/* 0*/	int		(*fs_init)();
/* 1*/	int		(*fs_iput)();
/* 2*/	struct inode	*(*fs_iread)();
/* 3*/	int		(*fs_filler)();
/* 4*/	int		(*fs_iupdat)();
/* 5*/	int		(*fs_readi)();
/* 6*/	int		(*fs_writei)();
/* 7*/	int		(*fs_itrunc)();
/* 8*/	int		(*fs_statf)();
/* 9*/	int		(*fs_namei)();
/*10*/	int		(*fs_mount)();
/*11*/	int		(*fs_umount)();
/*12*/	struct inode	*(*fs_getinode)();
/*13*/	int		(*fs_openi)();		/* open inode */
/*14*/	int		(*fs_closei)();		/* close inode */
/*15*/	int		(*fs_update)();		/* update */
/*16*/	int		(*fs_statfs)();		/* statfs and ustat */
/*17*/	int		(*fs_access)();
/*18*/	int		(*fs_getdents)();
/*19*/	int		(*fs_allocmap)();	/* Let the fs decide if */
						/* if can build a map so */
						/* this fs can be used for */
						/* paging */
/*20*/	int		*(*fs_freemap)();	/* free block list */
/*21*/	int		(*fs_readmap)();	/* read a page from the fs */
						/* using the block list */
/*22*/	int		(*fs_setattr)();	/* set attributes */
/*23*/	int		(*fs_notify)();		/* notify fs of action */
/*24*/	int		(*fs_fcntl)();		/* fcntl */
/*25*/	int		(*fs_fsinfo)();		/* additional info */
/*26*/	int		(*fs_ioctl)();		/* ioctl */
/*
 * Extentsions.  These extensions are only used if the filesystem
 * shares code with the "com" filesystem.
 */
/*27*/	int		(*fs_dirlookup)();	/* lookup name in directory */
/*28*/	int		(*fs_direnter)();	/* enter name,inum in dir */
/*29*/	int		(*fs_dirremove)();	/* remove a dir entry */
/*30*/	int		(*fs_dirinit)();	/* initialize a new dir */
/*31*/	int		(*fs_dirisempty)();	/* dir empty predicate */
/*32*/	int		(*fs_bmap)();		/* bmap */
/*33*/	struct inode	*(*fs_ialloc)();	/* allocate an inode */
/*34*/	int		(*fs_idestroy)();	/* free inode representation */
/*35*/	int		(*fs_ifree)();		/* free inode */
/*36*/	int		(*fs_setsize)();	/* set file size */

#ifdef	mips	/* was sgi */
/* these extensions are used only by the sgi system */
/*37*/	int		(*fs_rmount)();		/* mount root */
/*38*/	int		(*fs_rumount)();	/* unmount root */
/*
 * extensions for BSD compatibility
 */
/*39*/  int		(*fs_fsync)();		/* fsync inode */
#endif
};
extern struct fstypsw fstypsw[];
extern short nfstyp;


/* FS specific data */
struct fsinfo {
	long		fs_flags;	/* flags - see below */
	struct mount	*fs_pipe;	/* The mount point to be used */
					/* as the pipe device for */
					/* this fstyp */
	char		*fs_name; 	/* Pointer to fstyp name */
					/* See above */
	long		fs_notify;	/* Flags for fs_notify */
					/* e.g., NO_CHDIR, NO_CHROOT */
					/* see nami.h */
};
extern struct fsinfo fsinfo[];

#endif	_SYS_CONF_
