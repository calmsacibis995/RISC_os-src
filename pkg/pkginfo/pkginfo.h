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
/* $Header: pkginfo.h,v 2.0.1.2 90/05/10 04:02:58 wje Exp $ */
#include <stdio.h>
#include <sys/param.h>
#ifdef BSD
#else
#include <sys/limits.h>
#endif

extern char *getenv();
extern struct token *toke();

#ifdef BSD
#define PATH_MAX MAXPATHLEN
#endif

#ifdef MAIN
static char rcsm[]=MAKEID;
static char rcsh[]="$Header: pkginfo.h,v 2.0.1.2 90/05/10 04:02:58 wje Exp $";
#endif

/* neato GLOBAL declaration macros */

#ifdef MAIN
#define DCL
#else
#define DCL extern
#endif

#define GLOBAL(THING) DCL THING

/* error types */

#define ERRTYPE_WARN 0
#define ERRTYPE_ERROR 1
#define ERRTYPE_FATAL 2

/*
** A note on the philosphy here...
** I've decided to implement this with statically allocated structures,
** rather malloc & linked lists. Ain't virtual memory wonderful?
** May need to bump some of these limits someday. - rmg 10/16/87
*/

#define MAXID 128
#define MAXNAME 64
#define MAXALIAS 32
#define MAXSBOMS 256
#define MAXSUBPKG 50

#define PKGFL_OS 0x0001
#define PKGFL_OS2 0x0002

struct stateent
  {
    char *stname;
    char *(*func)();
  };

struct cmdent
  {
    char *cmdname;
    int (*func)();
  };

struct token
  {
    char name[MAXNAME+1];
    char value[PATH_MAX+1];
  };

struct pkginfo
  {
    unsigned flags;
    char id[MAXNAME+1];
    char name[MAXNAME+1];
    char timestamp[MAXNAME+1];
    char ver[MAXNAME+1];
  };

#define FL_OPT  0x0001
#define FL_MRO  0x0002

struct subpkginfo
  {
    char bom[MAXNAME+1];
    unsigned flags;
    char id[MAXID+1];
    char aliasnum;
    char name[MAXALIAS][MAXNAME+1];
    char splitboms[MAXSBOMS+1];
    char ver[MAXNAME+1];
  };

#define MAXVOLS 20
#define MAXMEDIA 10

struct mediainfo
  {
    char name[MAXNAME+1];
    int firstinvols[MAXVOLS];
    int curvol;
  };
 
GLOBAL (struct pkginfo pkg);
GLOBAL (struct subpkginfo subpkg[MAXSUBPKG]);
GLOBAL (int cursubpkg);
GLOBAL (struct mediainfo media[MAXMEDIA]);
GLOBAL (int curmedia);
GLOBAL (char emsg[PATH_MAX+64+1]);
GLOBAL (int gargc);
GLOBAL (char *gargv[100]); /* a hundred should be enough! */
GLOBAL (int aliasnum);

