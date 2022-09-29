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
/* $Header: dump.h,v 1.2.1.2 90/05/09 15:52:23 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)dump.h	5.4 (Berkeley) 2/23/87
 */

#define	NI		16
#define MAXINOPB	(MAXBSIZE / sizeof(struct dinode))
#define MAXNINDIR	(MAXBSIZE / sizeof(daddr_t))

#include <stdio.h>
#include <ctype.h>
#include <sys/param.h>
#include <sys/stat.h>
#include <ufs/fs.h>
#include <sys/time.h>
#include <sys/vnode.h>
#include <ufs/inode.h>
#include <protocols/dumprestore.h>
#include <ufs/fsdir.h>
#include <utmp.h>
#include <signal.h>
#include <fstab.h>

#define	MWORD(m,i)	(m[(unsigned)(i-1)/NBBY])
#define	MBIT(i)		(1<<((unsigned)(i-1)%NBBY))
#define	BIS(i,w)	(MWORD(w,i) |=  MBIT(i))
#define	BIC(i,w)	(MWORD(w,i) &= ~MBIT(i))
#define	BIT(i,w)	(MWORD(w,i) & MBIT(i))

int	msiz;
char	*clrmap;
char	*dirmap;
char	*nodmap;

/*
 *	All calculations done in 0.1" units!
 */

char	*disk;		/* name of the disk file */
char	*tape;		/* name of the tape file */
char	*increm;	/* name of the file containing incremental information*/
char	*temp;		/* name of the file for doing rewrite of increm */
char	lastincno;	/* increment number of previous dump */
char	incno;		/* increment number */
int	uflag;		/* update flag */
int	fi;		/* disk file descriptor */
int	to;		/* tape file descriptor */
int	pipeout;	/* true => output to standard output */
ino_t	ino;		/* current inumber; used globally */
int	nsubdir;
int	newtape;	/* new tape flag */
int	nadded;		/* number of added sub directories */
int	dadded;		/* directory added flag */
int	density;	/* density in 0.1" units */
long	tsize;		/* tape size in 0.1" units */
long	esize;		/* estimated tape size, blocks */
long	asize;		/* number of 0.1" units written on current tape */
int	etapes;		/* estimated number of tapes */

int	notify;		/* notify operator flag */
int	blockswritten;	/* number of blocks written on current tape */
int	tapeno;		/* current tape number */
time_t	tstart_writing;	/* when started writing the first tape block */
char	*processname;
struct fs *sblock;	/* the file system super block */
char	buf[MAXBSIZE];
#if defined(RISCOS)	/* used from Tahoe 4.3 sources */
long	dev_bsize;
#endif

char	*ctime();
char	*prdate();
long	atol();
int	mark();
int	add();
int	dirdump();
int	dump();
int	tapsrec();
int	dmpspc();
int	dsrch();
int	nullf();
char	*getsuffix();
char	*rawname();
struct dinode *getino();

int	interrupt();		/* in case operator bangs on console */

#define	HOUR	(60L*60L)
#define	DAY	(24L*HOUR)
#define	YEAR	(365L*DAY)

/*
 *	Exit status codes
 */
#define	X_FINOK		0	/* normal exit */
#define	X_REWRITE	2	/* restart writing from the check point */
#define	X_ABORT		3	/* abort all of dump; don't attempt checkpointing*/

#define	NINCREM	"/etc/dumpdates"	/*new format incremental info*/
#define	TEMP	"/etc/dtmp"		/*output temp file*/

#if defined(RISCOS)
#define	TAPE	"/dev/rmt/ctape0"	/* default tape device */
#define	DISK	"/dev/usr"		/* default disk */
#else
#define	TAPE	"/dev/rmt8"		/* default tape device */
#define	DISK	"/dev/rrp1g"		/* default disk */
#endif
#define	OPGRENT	"operator"		/* group entry to notify */
#define DIALUP	"ttyd"			/* prefix for dialups */

struct	fstab	*fstabsearch();	/* search in fs_file and fs_spec */

/*
 *	The contents of the file NINCREM is maintained both on
 *	a linked list, and then (eventually) arrayified.
 */
struct	idates {
	char	id_name[MAXNAMLEN+3];
	char	id_incno;
	time_t	id_ddate;
};
struct	itime{
	struct	idates	it_value;
	struct	itime	*it_next;
};
struct	itime	*ithead;	/* head of the list version */
int	nidates;		/* number of records (might be zero) */
int	idates_in;		/* we have read the increment file */
struct	idates	**idatev;	/* the arrayfied version */
#define	ITITERATE(i, ip) for (i = 0,ip = idatev[0]; i < nidates; i++, ip = idatev[i])

#if defined(RISCOS)
/*
 * Date/increment printed.
 */
time_t	Print_date;
int	Print_incno;
#endif

/*
 *	We catch these interrupts
 */
int	sighup();
int	sigquit();
int	sigill();
int	sigtrap();
int	sigfpe();
int	sigkill();
int	sigbus();
int	sigsegv();
int	sigsys();
int	sigalrm();
int	sigterm();
