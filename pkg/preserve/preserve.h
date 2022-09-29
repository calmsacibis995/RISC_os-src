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
/* $Header: preserve.h,v 1.3.2.2 90/05/10 04:03:29 wje Exp $ */

#ifdef MAIN
static char rcsm[]=MAKEID;
static char rcsh[]="$Header: preserve.h,v 1.3.2.2 90/05/10 04:03:29 wje Exp $";
#endif

#include <sys/param.h>
#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#ifdef BSD
#else
#include <sys/types.h>
#include <dirent.h>
#endif
#include <sys/stat.h>
#ifdef BSD
#include <sys/time.h>
#else
#include <time.h>
#endif

#define MAXPRESREC 512  /* maximum size of an input record */
#define MAXFILPATH 256  /* maximum size for a file or link target path */

#if 0 /* JustIinCase we need this again someday */
#define  IREAD_O 	S_IREAD
#define  IWRITE_O 	S_IWRITE
#define  IEXEC_O	S_IEXEC

#define  IREAD_G 	S_IREAD >> 3
#define  IWRITE_G 	S_IWRITE >> 3
#define  IEXEC_G	S_IEXEC >> 3

#define  IREAD_A 	S_IREAD >> 6
#define  IWRITE_A 	S_IWRITE >> 6
#define  IEXEC_A	S_IEXEC >> 6
#endif

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

extern int errno;

GLOBAL (char emsg[256]);	/* for sprintf's of error messages */
GLOBAL (char *targetpath);	/* relative root of target tree to check */
GLOBAL (char *version[80]);
GLOBAL (char updsuffix[80]);
GLOBAL (char savsuffix[80]);
