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
/* $Header: fstab.h,v 1.2.2.2 90/05/07 20:07:39 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


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
#define	BSD43_FSTAB	"/etc/fstab"

#define	BSD43_FSTAB_RW	"rw"	/* read/write device */
#define	BSD43_FSTAB_RQ	"rq"	/* read/write with quotas */
#define	BSD43_FSTAB_RO	"ro"	/* read-only device */
#define	BSD43_FSTAB_SW	"sw"	/* swap device */
#define	BSD43_FSTAB_XX	"xx"	/* ignore totally */

struct	bsd43_(fstab){
	char	*fs_spec;		/* block special device name */
	char	*fs_file;		/* file system path prefix */
	char	*fs_type;		/* FSTAB_* */
	int	fs_freq;		/* dump frequency, in days */
	int	fs_passno;		/* pass number on parallel dump */
};

struct	bsd43_(fstab) *bsd43_(getfsent)();
struct	bsd43_(fstab) *bsd43_(getfsspec)();
struct	bsd43_(fstab) *bsd43_(getfsfile)();
struct	bsd43_(fstab) *bsd43_(getfstype)();
int	bsd43_(setfsent)();
int	bsd43_(endfsent)();

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define FSTAB BSD43_FSTAB
#   define FSTAB_RO BSD43_FSTAB_RO
#   define FSTAB_RQ BSD43_FSTAB_RQ
#   define FSTAB_RW BSD43_FSTAB_RW
#   define FSTAB_SW BSD43_FSTAB_SW
#   define FSTAB_XX BSD43_FSTAB_XX
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


