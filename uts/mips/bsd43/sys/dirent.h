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
/* $Header: dirent.h,v 1.2.1.2 90/05/10 04:50:13 wje Exp $ */
#ifndef _BSD43_SYS_DIRENT_
#define _BSD43_SYS_DIRENT_

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Filesystem-independent directory information. 
 * Directory entry structures are of variable length.
 * Each directory entry is a struct dirent containing its file number, the
 * offset of the next entry (a cookie interpretable only the filesystem 
 * type that generated it), the length of the entry, and the length of the 
 * name contained in the entry.  These are followed by the name. The
 * entire entry is padded with null bytes to a 4 byte boundary. All names 
 * are guaranteed null terminated. The maximum length of a name in a 
 * directory is MAXNAMLEN, plus a null byte.
 */

#define	MAXNAMLEN	255

struct	bsd43_(dirent) {
	off_t   d_off;		/* offset of next disk directory entry */
	u_long	d_fileno;	/* file number of entry */
	u_short	d_reclen;	/* length of this record */
	u_short	d_namlen;	/* length of string in d_name */
	char	d_name[MAXNAMLEN + 1];	/* name (up to MAXNAMLEN + 1) */
};

/* 
 * The macro DIRSIZ(dp) gives the minimum amount of space required to represent
 * a directory entry.  For any directory entry dp->d_reclen >= DIRSIZ(dp).
 * Specific filesystem types may use this use this macro to construct the value
 * for d_reclen.
 */
#undef DIRSIZ
#define DIRSIZ(dp)  \
(((sizeof (struct bsd43_(dirent)) - (MAXNAMLEN+1) + ((dp)->d_namlen+1)) + 3) & ~3)
#endif _BSD43_SYS_DIRENT_
