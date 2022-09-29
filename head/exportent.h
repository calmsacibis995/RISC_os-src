/* |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: exportent.h,v 1.1.1.2.1.2 90/08/03 11:14:23 hawkes Exp $ */

/*	@(#)exportent.h	1.1 88/03/15 4.0NFSSRC SMI	*/
/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 *  1.5 88/02/07 (C) 1986 SMI
 */


/*
 * Exported file system table, see exportent(3)
 */ 

#define TABFILE "/etc/xtab"		/* where the table is kept */

/*
 * Options keywords
 */
#define ACCESS_OPT	"access"	/* machines that can mount fs */
#define ROOT_OPT	"root"		/* machines with root access of fs */
#define RO_OPT		"ro"		/* export read-only */
#define RW_OPT		"rw"		/* export read-mostly */
#define ANON_OPT	"anon"		/* uid for anonymous requests */
#define SECURE_OPT	"secure"	/* require secure NFS for access */
#define WINDOW_OPT	"window"	/* expiration window for credential */
#define ROOTID_OPT	"rootid"	/* like anon except takes userid */
#define NOHIDE_OPT	"nohide"	/* allow to look below mount point */

struct exportent {
	char *xent_dirname;	/* directory (or file) to export */
	char *xent_options;	/* options, as above */
};

extern FILE *setexportent();
extern void endexportent();
extern int remexportent();
extern int addexportent();
extern char *getexportopt();
extern struct exportent *getexportent();
