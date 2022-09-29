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
/* $Header: space.h,v 2.1.1.2 90/05/10 04:04:49 wje Exp $ */
#ifdef MAIN
static char rcsm[]=MAKEID;
static char rcsh[]="$Header: space.h,v 2.1.1.2 90/05/10 04:04:49 wje Exp $";
#endif

#include <sys/param.h>
#include <math.h>
#include <stdio.h>
#include <ctype.h>
#if defined(SYSTYPE_BSD43)
#include <sys/file.h>
#include <sys/vfs.h>
#include <mntent.h>
#else
#include <sys/types.h>
#include <fcntl.h>
#include <sys/statfs.h>
#include <sun/mntent.h>
#include <sys/fsid.h>
#endif /* SYSTYPE_BSD43 */
#include <sys/stat.h>
#include <errno.h>
#include <signal.h>

struct inocred
  {
    ino_t ino;
    struct inocred *next;
  };

struct dirnam
  {
    char *name;
    struct dirnam *next;
    struct dirnam *prev;
  };

struct nondirnam
  {
    char *oldname;
    char *newname;
    struct nondirnam *next;
    struct nondirnam *prev;
  };

struct fsd
  {
    dev_t dev;
    long frsize;
    long req;
    long cred;
    long free;
    long blocks;
    long ino_req;
    long ino_cred; 
    long ino_free;
    char *devpath;
  };

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

#define MAXPRESREC 512   /* maximum size of record in a sizfile */
#define MAXFILPATH 256   /* maximum size for a file or link target path */
#define MAXFS 128        /* maximum number of filesystems to track */
#define BLOCKSIZE 512    /* size of units accounted for in struct fsd */

extern int errno;

GLOBAL (char emsg[256]);	/* for sprintf's of error messages */
GLOBAL (char *targetpath);	/* relative root of target tree to check */
GLOBAL (struct fsd fstab[MAXFS]);
GLOBAL (int nfs);
GLOBAL (struct dirnam *dirnames);
GLOBAL (struct dirnam *lastdirname);
GLOBAL (struct nondirnam *nondirnames);
GLOBAL (struct nondirnam *lastnondirname);
GLOBAL (char touchfile[MAXFILPATH]);
GLOBAL (FILE *fss);
GLOBAL (double margin);
GLOBAL (struct inocred *inocreds);
GLOBAL (struct inocred *lastinocred);
GLOBAL (int excode);
