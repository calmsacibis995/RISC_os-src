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
/* $Header: saio.h,v 1.7.1.2 90/05/10 04:47:16 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * saio.h -- Header file for standalone package
 */

/*
 * EXSTKSZ -- sizeof stack needed by saio exception handling code
 * address of top of stack assumed to be in _fault_sp
 */
#define	BSD43_EXSTKSZ	1024

#ifdef LANGUAGE_C
/*
 * device table is interface between monitor and device drivers
 */
struct bsd43_(device_table) {
	char *dt_string;	/* device name */
	int (*dt_init)();	/* device init routine */
	int (*dt_open)();	/* device open routine */
	int (*dt_strategy)();	/* device strategy routine, returns cnt */
	int (*dt_close)();	/* device close routine */
	int (*dt_ioctl)();	/* device ioctl routine */
	int dt_type;		/* device "type" */
	int dt_fs;		/* file system type */
	char *dt_desc;		/* device description */
};
#endif LANGUAGE_C

/*
 * device types
 */
#define	BSD43_DTTYPE_CHAR	0x1	/* character device */
#define	BSD43_DTTYPE_CONS	0x2	/* can be console */
#define	BSD43_DTTYPE_BLOCK	0x4	/* block device */
#define BSD43_DTTYPE_RAW	0x8	/* raw device that uses fs switch */

/*
 * File structure types
 * NOTE: these are indices into fs_table in io.c and changes here
 * must be reflected in fs_table
 */
#define	BSD43_DTFS_NONE	0	/* no file structure on device */
#define	BSD43_DTFS_BFS	1	/* bfs protocol */
#define	BSD43_DTFS_DVH	2	/* disk volume header */
#define	BSD43_DTFS_TPD	3	/* boot tape directory */
#define BSD43_DTFS_NCP	4	/* Network console protocol */
#define	BSD43_DTFS_BSD42	5	/* 4.2 BSD file system */
#define	BSD43_DTFS_SYSV	6	/* System V file system */
#define	BSD43_DTFS_AUTO	-1	/* determined from partition table */

#ifdef LANGUAGE_C
struct bsd43_(fs_table) {
	int (*fs_init)();	/* fs init routine */
	int (*fs_open)();	/* fs open routine */
	int (*fs_read)();	/* fs read routine, returns count */
	int (*fs_write)();	/* fs write routine, return count */
	int (*fs_ioctl)();	/* fs ioctl routine */
	int (*fs_close)();	/* fs close routine */
};
#endif LANGUAGE_C


/*
 * character device flags
 */
#define	BSD43_DB_RAW		0x1	/* don't interpret special chars */
#define	BSD43_DB_STOPPED	0x2	/* stop output */

/*
 * character device buffer
 * (prom equivalent of c-list)
 */
#define	BSD43_CBUFSIZE	1024

#ifdef LANGUAGE_C
struct bsd43_(device_buf) {
	int db_flags;		/* character device flags */
	char *db_in;		/* pts at next free char */
	char *db_out;		/* pts at next filled char */
	char db_buf[BSD43_CBUFSIZE];	/* circular buffer for input */
};
#endif LANGUAGE_C

/*
 * Simple circular buffer functions
 */
#define	BSD43_CIRC_EMPTY(x)	((x)->db_in == (x)->db_out)
#define	BSD43_CIRC_FLUSH(x)	((x)->db_in = (x)->db_out = (x)->db_buf)
#define	BSD43_CIRC_STOPPED(x)	((x)->db_flags & BSD43_DB_STOPPED)

/*
 * iob.i_fs-tape and iob.i_ino-dir should be cast to the opproiate
 * structure tape in each file system or driver routine.
 * The size of these arrays a define below and should checked in the
 * init routine to make sure there is enough space available.
 */
#define BSD43_IOB_INODE	300
#ifdef PROM
#define BSD43_IOB_FS		512
#else
#define BSD43_IOB_FS		8196
#endif PROM

/*
 * Io block: includes an
 * inode, cells for the use of seek, etc,
 * and a buffer.
 */
#ifdef LANGUAGE_C
struct	bsd43_(iob) {
	int	i_flgs;		/* see F_ below */
	int	i_ctlr;		/* controller board */
	int	i_unit;		/* pseudo device unit */
	int	i_part;		/* disk partition */
	char	*i_ma;		/* memory address of i/o buffer */
	int	i_cc;		/* character count of transfer */
	off_t	i_offset;	/* seek offset in file */
	daddr_t	i_bn;		/* 1st block # of next read */
	int	i_fstype;	/* file system type */
	int	i_errno;	/* error # return */
	unsigned int	i_devaddr;	/* csr address */
	struct bsd43_(device_table) *i_dp;	/* pointer into device_table */
	char	*i_buf;			/* i/o buffer for blk devs */
	char	i_ino_dir[BSD43_IOB_INODE];	/* inode or disk/tape directory */
	char	i_fs_tape[BSD43_IOB_FS];	/* file system or tape header */
};
#endif LANGUAGE_C

#ifndef BSD43_NULL
#define BSD43_NULL 0
#endif

/*
 * file flags
 */
#define BSD43_F_READ		0x0001		/* file opened for reading */
#define BSD43_F_WRITE		0x0002		/* file opened for writing */
#define	BSD43_F_NBLOCK	0x0004		/* non-blocking io */
#define	BSD43_F_SCAN		0x0008		/* device should be scanned */
#define	BSD43_F_QIC11		0x0010		/* qic 11 device */
#define	BSD43_F_QIC24		0x0020		/* qic 24 device */

/*
 * Request codes
 */
#define	BSD43_READ	1
#define	BSD43_WRITE	2

#define	BSD43_DEVIOCTL(io, cmd, arg)	(*(io)->i_dp->dt_ioctl)(io, cmd, arg)
#define	BSD43_DEVREAD(io)		(*(io)->i_dp->dt_strategy)(io, BSD43_READ)
#define	BSD43_DEVWRITE(io)		(*(io)->i_dp->dt_strategy)(io, BSD43_WRITE)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define CBUFSIZE BSD43_CBUFSIZE
#   define CIRC_EMPTY BSD43_CIRC_EMPTY
#   define CIRC_FLUSH BSD43_CIRC_FLUSH
#   define CIRC_STOPPED BSD43_CIRC_STOPPED
#   define DB_RAW BSD43_DB_RAW
#   define DB_STOPPED BSD43_DB_STOPPED
#   define DEVIOCTL BSD43_DEVIOCTL
#   define DEVREAD BSD43_DEVREAD
#   define DEVWRITE BSD43_DEVWRITE
#   define DTFS_AUTO BSD43_DTFS_AUTO
#   define DTFS_BFS BSD43_DTFS_BFS
#   define DTFS_BSD42 BSD43_DTFS_BSD42
#   define DTFS_DVH BSD43_DTFS_DVH
#   define DTFS_NCP BSD43_DTFS_NCP
#   define DTFS_NONE BSD43_DTFS_NONE
#   define DTFS_SYSV BSD43_DTFS_SYSV
#   define DTFS_TPD BSD43_DTFS_TPD
#   define DTTYPE_BLOCK BSD43_DTTYPE_BLOCK
#   define DTTYPE_CHAR BSD43_DTTYPE_CHAR
#   define DTTYPE_CONS BSD43_DTTYPE_CONS
#   define DTTYPE_RAW BSD43_DTTYPE_RAW
#   define EXSTKSZ BSD43_EXSTKSZ
#   define F_NBLOCK BSD43_F_NBLOCK
#   define F_QIC11 BSD43_F_QIC11
#   define F_QIC24 BSD43_F_QIC24
#   define F_READ BSD43_F_READ
#   define F_SCAN BSD43_F_SCAN
#   define F_WRITE BSD43_F_WRITE
#   define IOB_FS BSD43_IOB_FS
#   define IOB_INODE BSD43_IOB_INODE
#   define NULL BSD43_NULL
#   define READ BSD43_READ
#   define WRITE BSD43_WRITE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


