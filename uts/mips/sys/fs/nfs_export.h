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
/* $Header: nfs_export.h,v 1.9.1.2 90/05/10 06:16:42 wje Exp $ */

/*      @(#)export.h	2.1 88/05/20 NFSSRC4.0 from 1.4 88/02/08 SMI      */

/*	Copyright (C) 1988 Sun Microsystems Inc.	*/

#ifndef _SYS_FS_NFS_EXPORT_
#define _SYS_FS_NFS_EXPORT_	1

/*
 * exported vfs flags.
 */
#define EX_RDONLY     0x01		/* exported read only */
#define EX_RDMOSTLY   0x02              /* exported read mostly */

#define EXMAXADDRS 10			/* max number in address list */
struct exaddrlist {
	unsigned naddrs;		/* number of addresses */
	struct sockaddr *addrvec;	/* pointer to array of addresses */
};

/*
 * Associated with AUTH_UNIX is an array of internet addresses
 * to check root permission.
 */
#define EXMAXROOTADDRS	10		/* should be config option */
struct unixexport {
	struct exaddrlist rootaddrs;
};

/*
 * Associated with AUTH_DES is a list of network names to check
 * root permission, plus a time window to check for expired
 * credentials.
 */
#define EXMAXROOTNAMES 10	   	/* should be config option */
struct desexport {
	unsigned nnames;
	char **rootnames;
	int window;
};


/*
 * The export information passed to exportfs()
 */
struct export {
	int ex_flags;	/* flags */
	int ex_anon;	/* uid for unauthenticated requests */
	int ex_auth;	/* switch */
	union {
		struct unixexport exunix;	/* case AUTH_UNIX */
		struct desexport exdes;		/* case AUTH_DES */
	} ex_u;
	struct exaddrlist ex_writeaddrs;
};
#define ex_des ex_u.exdes
#define ex_unix ex_u.exunix

#ifdef KERNEL
/*
 * A node associated with an export entry on the list of exported
 * filesystems.
 */
struct exportinfo {
	struct export exi_export;
	fsid_t exi_fsid;
	struct fid *exi_fid;
	struct exportinfo *exi_next;
};
extern struct exportinfo *findexport();
#endif

#endif _SYS_FS_NFS_EXPORT_
