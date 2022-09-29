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
/* $Header: comply.h,v 1.9.2.2 90/05/10 03:44:39 wje Exp $ */
#ifdef MAIN
static char rcsm[]=MAKEID;
static char rcsh[]="$Header: comply.h,v 1.9.2.2 90/05/10 03:44:39 wje Exp $";
#endif

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#ifdef BSD
#else
#include <sys/types.h>
#include <sys/nami.h>
#endif
#include <sys/stat.h>
#ifdef BSD
#define PATH_MAX MAXPATHLEN
#else
#include <sys/limits.h>
#endif
#ifdef BSD
#include <sys/time.h>
#else
#include <time.h>
#endif

#define FIXMODE_m	0x01
#define FIXMODE_t	0x02
#define FIXMODE_o	0x04
#define FIXMODE_g	0x08
#define FIXMODE_p	0x10
#define FIXMODE_s	0x20
#define FIXMODE_l	0x40


#define  IREAD_O 	S_IREAD
#define  IWRITE_O 	S_IWRITE
#define  IEXEC_O	S_IEXEC

#define  IREAD_G 	S_IREAD >> 3
#define  IWRITE_G 	S_IWRITE >> 3
#define  IEXEC_G	S_IEXEC >> 3

#define  IREAD_A 	S_IREAD >> 6
#define  IWRITE_A 	S_IWRITE >> 6
#define  IEXEC_A	S_IEXEC >> 6

/* neato GLOBAL declaration macros */

#ifdef MAIN
#define DCL
#else
#define DCL extern
#endif

#define GLOBAL(THING) DCL THING

#define skipspace(p) while(isspace(*p)) p++

/* error types */

#define ERRTYPE_WARN 0
#define ERRTYPE_ERROR 1
#define ERRTYPE_FATAL 2

/* following values used by passwd & group reading routines: */

#define MAXUNAM 16   /* max length of user name */
#define NUIDDIR 200  /* number of uid's to index directly */
#define NUIDIND 2048 /* number of uid's to index indirectly */

#define MAXGNAM 16   /* max length of group name */
#define NGIDDIR 200  /* number of gid's to map directly */
#define NGIDIND 2048 /* number of gid's to map indirectly */

extern int errno;
extern char *sys_errlist[];

GLOBAL (char emsg[256]);	/* for sprintf's of error messages */
GLOBAL (char *passfile);	/* passwd file name */
GLOBAL (char *groupfile);	/* group file name */
GLOBAL (char *targetpath);	/* relative root of target tree to check */
GLOBAL (int fixmode);		/* flag nonzero if in -f (fix) mode */
GLOBAL (int silent);		/* flag nonzero if in -s (silent) mode */
GLOBAL (int extra);		/* flag nonzero if -e (detect extra files) */
GLOBAL (int delete);		/* flag nonzero for removal of extra files */
GLOBAL (long tstamp);		/* parameter to -t option */
GLOBAL (int kthresh);		/* kill on errors threshold */
GLOBAL (int conform);		/* boolean true if 100% conformance */
GLOBAL (int bomcheck);		/* boolean true if check bom record only */
