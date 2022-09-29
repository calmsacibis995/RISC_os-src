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
/* $Header: inode.h,v 1.10.3.2 90/05/10 06:24:06 wje Exp $ */

#ifndef	_SYS_INODE_
#define	_SYS_INODE_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define FSPTR 1
/*
 *	The I node is the focus of all
 *	file activity in unix. There is a unique
 *	inode allocated for each active file,
 *	each current directory, each mounted-on
 *	file, text file, and the root. An inode is 'named'
 *	by its dev/inumber pair. (iget/iget.c)
 *	Data, from mode on, is read in
 *	from permanent inode on volume.
 */

/* 
 * ATT release this header with a "typeless" pointer for file system
 * independant data: int * i_fsptr.  This makes errors easy to make and
 * hard to find.  Unions fix everything.  The #define for i_fsptr lets
 * old code work unmodified.
 */

typedef	struct	inode
{
	struct	inode	*i_forw;	/* inode hash chain */
	struct	inode	*i_back;	/* '' */
	struct	inode	*av_forw;	/* freelist chain */
	struct	inode	*av_back;	/* '' */
#ifdef SV_BFS
	union {
		int *ifsd_int;
		struct bfs_inode *ifsd_bfsp;
	} i_fsdata;
#	define i_fsptr i_fsdata.ifsd_int
#	define i_bfsptr i_fsdata.ifsd_bfsp
#else
	int	*i_fsptr;	/* "typeless" pointer to fs dependent */
#endif SV_BFS
	long	i_number;	/* i number, 1-to-1 with dev address */
	ushort	i_ftype;	/* file type = IFDIR, IFREG, etc. */
	short	i_fstyp;	/* File system type */
	off_t	i_size;		/* size of file */
	ushort	i_uid;		/* owner */
	ushort	i_gid;		/* group of owner */
	ushort	i_flag;
	cnt_t	i_count;	/* reference count */
	short	i_nlink;	/* directory entries */
	dev_t	i_rdev;		/* Raw device number */
	dev_t	i_dev;		/* device where inode resides */

	struct	mount	*i_mntdev;	/* ptr to mount dev inode resides on */
	union i_u {

		struct	mount	*i_mton;	/* pntr to mount table entry */
						/* that this inode is "mounted on" */
		struct stdata	*i_sp;  /* Associated stream.		*/
	} i_un;
#ifdef FSPTR
	struct fstypsw *i_fstypp;	/* pointer to file system */
					/* switch structure */
#endif
	long	*i_filocks;	/* pointer to filock (structure) list */
	struct	rcvd	*i_rcvd;	/* receive descriptor */
	long	i_gen;		/* generation number (for nfs) */
} inode_t;

extern struct inode inode[];	/* The inode table itself */

struct	ifreelist
{	int	pad[2];		/* must match struct inode !*/
	struct inode	*av_forw;
	struct inode	*av_back;
} ;

extern struct ifreelist ifreelist;

#define	i_sptr	i_un.i_sp
#define	i_mnton	i_un.i_mton

/* flags */

#define	ILOCK	0x01		/* inode is locked */
#define	IUPD	0x02		/* file has been modified */
#define	IACC	0x04		/* inode access time to be updated */
#define	IMOUNT	0x08		/* inode is mounted on */
#define	IWANT	0x10		/* some process waiting on lock */
#define	ITEXT	0x20		/* inode is pure text prototype */
#define	ICHG	0x40		/* inode has been changed */
#define ISYN	0x80		/* do synchronous write for iupdate */
#define	IADV	0x100		/* advertised */
#define	IDOTDOT	0x200		/* object of remote mount */
#define	ILBIN	0x400		/* mounted remotely  */
#define	IRMOUNT	0x800		/* remotely mounted	*/
#define	IISROOT	0x1000		/* This is a root inode of an fs */

/* file types */
/* WARNING: The following defines should NOT change!If more */
/* file types need to be added they should be added in the low */
/* bits */

#define	IFMT	0xf000		/* type of file */
#define		IFDIR	0x4000	/* directory */
#define		IFCHR	0x2000	/* character special */
#define		IFBLK	0x6000	/* block special */
#define		IFREG	0x8000	/* regular */
#define		IFMPC	0x3000	/* multiplexed char special */
#define		IFMPB	0x7000	/* multiplexed block special */
#define		IFIFO	0x1000	/* fifo special */
#define		IFLNK	0xA000	/* symbolic link */
#define		IFSOCK	0xC000	/* socket */

/* file modes */
/* the System V Rel 2 chmod system call only knows about */
/* ISUID, ISGID, ISVTX */
/* Therefore, the bit positions of ISUID, ISGID, and ISVTX */
/* should not change */
#define	ISUID	0x800		/* set user id on execution */
#define	ISGID	0x400		/* set group id on execution */
#define ISVTX	0x200		/* save swapped text even after use */

/* access requests */
/* the System V Rel 2 chmod system call only knows about */
/* IREAD, IWRITE, IEXEC */
/* Therefore, the bit positions of IREAD, IWRITE, and IEXEC */
/* should not change */
#define	IREAD		0x100	/* read permission */
#define	IWRITE		0x080	/* write permission */
#define	IEXEC		0x040	/* execute permission */
#define	ICDEXEC		0x020	/* cd permission */
#define	IOBJEXEC	0x010	/* execute as an object file */
				/* i.e., 410, 411, 413 */
#define IMNDLCK		0x001	/* mandatory locking set */

#define	MODEMSK		0xfff	/* Nine permission bits - read/write/ */
				/* execute for user/group/others and */
				/* ISUID, ISGID, and ISVTX */	
				/* This is another way of saying: */
				/* (ISUID|ISGID|ISVTX| */
				/* (IREAD|IWRITE|IEXEC)| */
				/* ((IREAD|IWRITE|IEXEC)>>3)| */
				/* ((IREAD|IWRITE|IEXEC)>>6)) */
#define	PERMMSK		0x1ff	/* Nine permission bits: */
				/* ((IREAD|IWRITE|IEXEC)| */
				/* ((IREAD|IWRITE|IEXEC)>>3)| */
				/* ((IREAD|IWRITE|IEXEC)>>6)) */

#endif	_SYS_INODE_
