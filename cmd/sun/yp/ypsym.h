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
/* $Header: ypsym.h,v 1.3.1.2 90/05/09 19:35:01 wje Exp $ */
/*      @(#)ypsym.h	1.2 88/06/18 4.0NFSSRC SMI   */

/*
 * This contains symbol and structure definitions for modules in the yellow
 * pages server.  
 */

#include <dbm.h>			/* Pull this in first */
#define DATUM
/*extern void dbmclose();*/		/* Refer to dbm routine not in dbm.h */
#ifdef NULL
#undef NULL				/* Remove dbm.h's definition of NULL */
#endif
#include <stdio.h>
#include <errno.h>
#include <signal.h>
#include <rpc/rpc.h>
#include <sys/socket.h>
#include <sys/stat.h>
/*#include <dirent.h>*/
#include <sys/dir.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <rpcsvc/yp_prot.h>
#include <rpcsvc/ypv1_prot.h>
#include <rpcsvc/ypclnt.h>

typedef void (*PFV)();
typedef int (*PFI)();
typedef unsigned int (*PFU)();
typedef long int (*PFLI)();
typedef unsigned long int (*PFULI)();
typedef short int (*PFSI)();
typedef unsigned short int (*PFUSI)();

#ifndef TRUE
#define TRUE 1
#endif

#ifndef FALSE
#define FALSE 0
#endif

#ifdef NULL
#undef NULL
#define NULL 0
#endif

#define YPINTERTRY_TIME 10		/* Secs between tries for peer bind */
#define YPTOTAL_TIME 30			/* Total secs until timeout */
#define YPNOPORT ((unsigned short) 0)	/* Out-of-range port value */

/* External refs to cells and functions outside of the yp */

extern int errno;
extern char *malloc();
extern char *strcpy();
extern char *strcat();
extern long atol();

/* External refs to yp server data structures */

extern bool ypinitialization_done;
extern struct timeval ypintertry;
extern struct timeval yptimeout;
extern char myhostname[];

/* External refs to yp server-only functions */

extern bool ypcheck_map_existence();
extern bool ypset_current_map();
extern void ypclr_current_map();
extern bool ypbind_to_named_server();
extern void ypmkfilename();
extern int yplist_maps();


