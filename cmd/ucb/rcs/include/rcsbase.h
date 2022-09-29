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
/* $Header: rcsbase.h,v 1.8.1.3 90/05/10 00:26:44 wje Exp $ */


/*
 *                     RCS common definitions and data structures
 */
/*****************************************************************************
 * INSTRUCTIONS:
 * =============
 * For USG Unix, define USG; for BSD Unix, don't (see ifdef USG).
 * For 4.2 or 4.3 bsd, define V4_XBSD; this will replace the routines
 * getwd() and rename() with the corresponding ones in the C-library (this is
 * done automatically in 4.3).
 * V4_XBSD also selects different definitions for the macros NCPFN and NCPPN
 * (max. number of characters per filename, number of characters per path name).
 * Define STRICT_LOCKING appropriately (see STRICT_LOCKING).
 * Change BYTESIZ if necessary.
 *****************************************************************************
 *
 * Copyright (C) 1982 by Walter F. Tichy
 *                       Purdue University
 *                       Computer Science Department
 *                       West Lafayette, IN 47907
 *
 * All rights reserved. No part of this software may be sold or distributed
 * in any form or by any means without the prior written permission of the
 * author.
 * Report problems and direct all inquiries to Tichy@purdue (ARPA net).
 */

#include <stdio.h>
#undef putc         /* will be redefined */

#include <sys/param.h>
#ifdef BSD4_3
#define V4_XBSD
#else
#define USG
#endif

#ifndef DIFF
#define DIFF		"/usr/lib/rdiff"
#endif
#ifndef MERGE
#define MERGE		"/usr/ucb/merge"
#endif
#ifndef TARGETDIR
#define TARGETDIR	"/usr/ucb"
#endif

#ifdef USG
#       define rindex    strrchr
#       define DATEFORM  "%.2d.%.2d.%.2d.%.2d.%.2d.%.2d"
#else
#       define DATEFORM  "%02d.%02d.%02d.%02d.%02d.%02d"
#endif
/* Make sure one of %02d or %.2d prints a number with a field width 2, with
 * leading zeroes. For example, 0, 1, and 22 must be printed as 00, 01, and
 * 22. Otherwise, there will be problems with the dates.
 */

#define PRINTDATE(file,date) fprintf(file,"%.2s/%.2s/%.2s",date,date+3,date+6)
#define PRINTTIME(file,date) fprintf(file,"%.2s:%.2s:%.2s",date+9,date+12,date+15)
/* print RCS format date and time in nice format from a string              */

/*
 * Parameters
 */
#define BYTESIZ             8 /* number of bits in a byte                   */

#define STRICT_LOCKING      1 /* 0 sets the default locking to non-strict;  */
                              /* used in experimental environments.         */
                              /* 1 sets the default locking to strict;      */
                              /* used in production environments.           */
#define hshsize           239 /* hashtable size; MUST be prime and -1 mod 4 */
                              /* other choices: 547 or 719                  */
#define strtsize (hshsize * 50) /* string table size                        */
#ifdef pdp11
# define logsize           1024 /* size of logmessage                        */
#else
# define logsize	   4096
#endif
#define revlength          30 /* max. length of revision numbers            */
#define datelength         20 /* length of a date in RCS format             */
#define joinlength         20 /* number of joined revisions permitted       */
#define RCSDIR         "RCS/" /* subdirectory for RCS files                 */
#define RCSSUF            'v' /* suffix for RCS files                       */
#define RCSSEP            ',' /* separator for RCSSUF                       */
#define KDELIM            '$' /* delimiter for keywords                     */
#define VDELIM            ':' /* separates keywords from values             */
#define DEFAULTSTATE    "Exp" /* default state of revisions                 */
#if defined(V4_XBSD) || defined(mips)
#  define NCPFN           256 /* number of characters per filename          */
#  define NCPPN          1024 /* number of characters per pathname          */
#else
#  define NCPFN            14 /* number of characters per filename          */
#  define NCPPN       6*NCPFN /* number of characters per pathname          */
#endif
#define keylength          20 /* buffer length for expansion keywords       */
#define keyvallength NCPPN+revlength+datelength+60
                              /* buffer length for keyword expansion        */



#define true     1
#define false    0
#define nil      0
#define elsif    else if
#define elif     else if


/* temporary file names */

#define NEWRCSFILE  ",RCSnewXXXXXX"
#define DIFFILE     ",RCSciXXXXXX"
#define TMPFILE1    ",RCSt1XXXXXX"
#define TMPFILE2    ",RCSt2XXXXXX"
#define TMPFILE3    ",RCSt3XXXXXX"
#define JOINFIL2    ",RCSj2XXXXXX"
#define JOINFIL3    ",RCSj3XXXXXX"


#define putc(x,p) (--(p)->_cnt>=0? ((int)(*(p)->_ptr++=(unsigned)(x))):fflsbuf((unsigned)(x),p))
/* This version of putc prints a char, but aborts on write error            */

#define GETC(in,out,echo) (echo?putc(getc(in),out):getc(in))
/* GETC writes a del-character (octal 177) on end of file                   */

#define WORKMODE(RCSmode) (RCSmode&~0222)|((lockflag||!StrictLocks)?0600:0000)
/* computes mode of working file: same as RCSmode, but write permission     */
/* determined by lockflag and StrictLocks.                                  */


/* character classes and token codes */
enum tokens {
/* char classes*/  DIGIT, IDCHAR, NEWLN, LETTER, PERIOD, SBEGIN, SPACE, UNKN,
/* tokens */       COLON, DATE, EOFILE, ID, KEYW, NUM, SEMI, STRING,
};

#define AT      SBEGIN  /* class SBEGIN (string begin) is returned by lex. anal. */
#define SDELIM  '@'     /* the actual character is needed for string handling*/
/* these must be changed consistently, for instance to:
 * #define DQUOTE       SBEGIN
 * #define SDELIM       '"'
 * #define AT           IDCHAR
 * there should be no overlap among SDELIM, KDELIM, and VDELIM
 */

/* other characters */

#define ACCENT   IDCHAR
#define AMPER    IDCHAR
#define BACKSL   IDCHAR
#define BAR      IDCHAR
#define COMMA    UNKN
#define DIVIDE   IDCHAR
#define DOLLAR   IDCHAR
#define DQUOTE   IDCHAR
#define EQUAL    IDCHAR
#define EXCLA    IDCHAR
#define GREAT    IDCHAR
#define HASH     IDCHAR
#define INSERT   UNKN
#define LBRACE   IDCHAR
#define LBRACK   IDCHAR
#define LESS     IDCHAR
#define LPARN    IDCHAR
#define MINUS    IDCHAR
#define PERCNT   IDCHAR
#define PLUS     IDCHAR
#define QUEST    IDCHAR
#define RBRACE   IDCHAR
#define RBRACK   IDCHAR
#define RPARN    IDCHAR
#define SQUOTE   IDCHAR
#define TILDE    IDCHAR
#define TIMES    IDCHAR
#define UNDER    IDCHAR
#define UPARR    IDCHAR




/***************************************
 * Data structures for the symbol table
 ***************************************/


/* Hash table entry */
struct hshentry {
        char              * num;      /* pointer to revision number (ASCIZ) */
        char              * date;     /* pointer to date of checking        */
        char              * author;   /* login of person checking in        */
        char              * lockedby; /* who locks the revision             */
        char              * log;      /* log message requested at checkin   */
        char              * state;    /* state of revision (Exp by default) */
        struct branchhead * branches; /* list of first revisions on branches*/
        struct hshentry   * next;     /* next revision on same branch       */
        int                 insertlns;/* lines inserted (computed by rlog)  */
        int                 deletelns;/* lines deleted  (computed by rlog)  */
        char                selector; /* marks entry for selection/deletion */
};

/* list element for branch lists */
struct branchhead {
        struct hshentry   * hsh;
        struct branchhead * nextbranch;
};

/* accesslist element */
struct access {
        char              * login;
        struct access     * nextaccess;
};

/* list element for locks  */
struct lock {
        char              * login;
        struct hshentry   * delta;
        struct lock       * nextlock;
};

/* list element for symbolic names */
struct assoc {
        char              * symbol;
        struct hshentry   * delta;
        struct assoc      * nextassoc;
};


/* common variables (getadmin and getdelta())*/
extern char            * Comment;
extern struct access   * AccessList;
extern struct assoc    * Symbols;
extern struct lock     * Locks;
extern struct hshentry * Head;
extern int               StrictLocks;
extern int               TotalDeltas;
static char copyright[]="Copyright (C) 1982 by Walter F. Tichy";

/* common variables (lexical analyzer)*/
extern enum tokens map[];
#define ctab (&map[1])
extern struct hshentry   hshtab[];
extern struct hshentry * nexthsh;
extern enum tokens       nexttok;
extern int               hshenter;
extern char            * NextString;
extern char            * cmdid;

/* common routines */
extern int serror();
extern int faterror();
extern int fatserror();
extern void ignoreints();
extern void catchints();
  
extern char *strcpy();
extern char *strcat();
extern char *strncpy();
#ifdef USG
extern int sprintf();
#else
extern char *sprintf();
#endif


/*
 * Markers for keyword expansion (used in co and ident)
 */
#define AUTHOR          "Author"
#define DATE            "Date"
#define HEADER          "Header"
#define LOCKER          "Locker"
#define LOG             "Log"
#define REVISION        "Revision"
#define SOURCE          "Source"
#define STATE           "State"

enum markers { Nomatch, Author, Date, Header,
               Locker, Log, Revision, Source, State };

#define DELNUMFORM      "\n\n%s\n%s\n"
/* used by putdtext and scanlogtext */
#define DELETE          'D'
/* set by rcs -o and used by puttree() in rcssyn */

#if defined(USG) || defined(BSD)
#define VOID	(void)
#else
typedef int void;
#define VOID
#endif
