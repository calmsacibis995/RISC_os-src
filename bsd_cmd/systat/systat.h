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
/* $Header: systat.h,v 1.3.1.2 90/05/07 19:32:18 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)systat.h	5.2 (Berkeley) 7/20/86
 */

#include <netdb.h>
#include <nlist.h>
#include <signal.h>
#include <curses.h>

#include <sys/param.h>
#include <sys/file.h>
#ifdef RISCOS
#include <sys/sysinfo.h>
#include <sys/var.h>
#ifndef CPUSTATES
#define	CPUSTATES	4

#define	CP_USER		0
#define	CP_NICE		1
#define	CP_SYS		2
#define	CP_IDLE		3
#endif CPUSTATES
#include <sys/fixpoint.h>
#include <math.h>
#else RISCOS
#include <sys/dkstat.h>
#endif RISCOS

#include <netinet/in.h>
#include <arpa/inet.h>

struct p_times {
        short   pt_pid;
        float   pt_pctcpu;
        int     pt_uid;
        int     pt_paddr;
        struct  proc *pt_pp;
} *pt;
long    nproc, procp;
struct	proc *kprocp;

struct procs {
        int     pid;
        char    cmd[16];
} procs[200];
int     numprocs;

struct users {
        int     k_uid;
        char    k_name[16];
} known[30];
int     numknown;

struct  cmdtab {
        char    *c_name;		/* command name */
        int     (*c_refresh)();		/* display refresh */
        int     (*c_fetch)();		/* sets up data structures */
        int     (*c_label)();		/* label display */
	int	(*c_init)();		/* initialize namelist, etc. */
	WINDOW	*(*c_open)();		/* open display */
	int	(*c_close)();		/* close display */
	int	(*c_cmd)();		/* display command interpreter */
	char	c_flags;		/* see below */
};

#define	CF_INIT		0x1		/* been initialized */
#define	CF_LOADAV	0x2		/* display w/ load average */

struct	cmdtab *curcmd;
extern
struct	cmdtab cmdtab[];
struct	cmdtab *lookup();

int     kmem, mem, swap;
int     naptime, col;

long	ntext, textp;
struct	text *xtext;

double  lccpu;
double	avenrun[3];

char    *kmemf, *memf, *swapf;
int	hz, phz;
char	**dr_name;
int	dk_ndrive;
int	*dk_select;
float	*dk_mspw;
char    c, *namp, hostname[32];

int	nports;
int	nhosts;
int	protos;
#define	TCP	0x1
#define	UDP	0x2

struct  pte *usrpt;
struct  pte *Usrptma;

WINDOW  *wnd;
int	CMDLINE;

char    *malloc(), *calloc(), *strncpy();
long    getw();
