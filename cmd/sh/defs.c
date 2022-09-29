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
#ident	"$Header: defs.c,v 1.6.2.2 90/05/09 18:56:25 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */

#include		"defaults.h"
#include 		<setjmp.h>
#include		"mode.h"
#include		"name.h"
#include		<sys/param.h>
#ifndef RISCOS
#ifndef NOFILE
#define NOFILE 20
#endif
#endif RISCOS

/* temp files and io */

int				output = 2;
int				ioset;
struct ionod	*iotemp;	/* files to be deleted sometime */
struct ionod	*fiotemp;	/* function files to be deleted sometime */
struct ionod	*iopend;	/* documents waiting to be read at NL */
#ifdef RISCOS
struct fdsave	fdmap[NOFILE];
#else RISCOS
struct	fdsave	*fdmap;
#endif RISCOS

#ifdef pyr
/* keep track of the current universe */
int				cur_univ;
#endif

/* substitution */
int				dolc;
char			**dolv;
struct dolnod	*argfor;
struct argnod	*gchain;


/* name tree and words */
int				wdval;
int				wdnum;
int				fndef;
int				nohash;
struct argnod	*wdarg;
int				wdset;
BOOL			reserv;

/* special names */
char			*pcsadr;
char			*pidadr;
char			*cmdadr;

/* transput */ 
char 			*tmpnam;
int 			serial; 
unsigned 		peekc;
unsigned		peekn;
char 			*comdiv;

long			flags;
int				rwait;	/* flags read waiting */

/* error exits from various parts of shell */
jmp_buf			subshell;
jmp_buf			errshell;

/* fault handling */
BOOL			trapnote;

/* execflgs */
int				exitval;
int				retval;
#if gould
int			execbrk;
#else
BOOL			execbrk;
#endif
int				loopcnt;
int				breakcnt;
int 			funcnt;

int				wasintr;	/* used to tell if break or delete is hit
				   			   while executing a wait
							*/

int				eflag;

#if BRL
char			*argv0;		/* shell's argv[0] */
BOOL			loginsh;	/* TRUE iff login shell */
int			timeout;	/* TIMEOUT value in seconds */
int			userid; 	/* effective UID */
#endif BRL

/* The following stuff is from stak.h	*/

char 			*stakbas;
char			*staktop;
char			*stakbot;
char			*stakbsy;
char 			*brkend;
