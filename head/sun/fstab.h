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
/* $Header: fstab.h,v 1.3.3.2 90/05/10 01:04:38 wje Exp $ */

#ifndef	_SUN_FSTAB_
#define	_SUN_FSTAB_	1


/*	fstab.h	4.4	83/05/24	*/

/*
 * File system table, see fstab (5)
 *
 * Used by dump, mount, umount, swapon, fsck, df, ...
 *
 * The fs_spec field is the block special name.  Programs
 * that want to use the character special name must create
 * that name by prepending a 'r' after the right most slash.
 * Quota files are always named "quotas", so if type is "rq",
 * then use concatenation of fs_file and "quotas" to locate
 * quota file.
 */
#define	FSTAB		"/etc/fstab"

#define	FSTAB_RW	"rw"	/* read/write device */
#define	FSTAB_RQ	"rq"	/* read/write with quotas */
#define	FSTAB_RO	"ro"	/* read-only device */
#define	FSTAB_SW	"sw"	/* swap device */
#define	FSTAB_XX	"xx"	/* ignore totally */

struct	fstab{
	char	*fs_spec;		/* block special device name */
	char	*fs_file;		/* file system path prefix */
	char	*fs_type;		/* FSTAB_* */
	int	fs_freq;		/* dump frequency, in days */
	int	fs_passno;		/* pass number on parallel dump */
};

struct	fstab *getfsent();
struct	fstab *getfsspec();
struct	fstab *getfsfile();
struct	fstab *getfstype();
int	setfsent();
int	endfsent();

#endif	_SUN_FSTAB_
