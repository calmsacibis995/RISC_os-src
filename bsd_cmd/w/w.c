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
#ident	"$Header: w.c,v 1.4.1.3 90/05/07 19:49:55 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */


/*
 * w - print system status (who and what)
 *
 * This program is similar to the systat command on Tenex/Tops 10/20
 * It needs read permission on /dev/mem, /dev/kmem, and /dev/drum.
 */
#include <sys/types.h>
#include <sys/param.h>
#include <nlist.h>
#include <stdio.h>
#include <ctype.h>
#include <utmp.h>
#include <time.h>
#include <sys/stat.h>
#include <sys/dir.h>
#include <sys/signal.h>
#include <sys/sbd.h>
#include <sys/pcb.h>
#include <sys/immu.h>
#include <sys/region.h>
#include <sys/proc.h>
#include <sys/user.h>
#include <sys/termio.h>
#include <sys/fixpoint.h>
#include <sys/var.h>
#include <sys/vnode.h>

#ifdef RISCOS
#include "../../lib/libmips/libutil.h"

#define strcmpn strncmp
#define strcatn strncat
#define nonuser(utmp) (utmp.ut_type != USER_PROCESS)
#endif RISCOS

#define NMAX sizeof(utmp.ut_name)
#define LMAX min(sizeof(utmp.ut_line),8)
#ifndef RISCOS
#define HMAX sizeof(utmp.ut_host)
#endif RISCOS

#define ARGWIDTH	33	/* # chars left on 80 col crt for args */

struct pr {
	short	w_pid;			/* proc.p_pid */
	char	w_flag;			/* proc.p_flag */
	short	w_size;			/* proc.p_size */
	long	w_seekaddr;		/* where to find args */
	long	w_lastpg;		/* disk address of stack */
	int	w_igintr;		/* INTR+3*QUIT, 0=die, 1=ign, 2=catch */
	time_t	w_time;			/* CPU time used by this process */
	time_t	w_ctime;		/* CPU time used by children */
	dev_t	w_tty;			/* tty device of process */
	int	w_uid;			/* uid of process */
	char	w_comm[15];		/* user.u_comm, null terminated */
	char	w_args[ARGWIDTH+1];	/* args if interesting process */
} *pr;
int	nproc;

struct	nlist nl[] = {
	{ "proc" },
#define	X_PROC		0
	{ "swplo" },
#define X_SWPLO		1
	{ "v" },
#define	X_V		2
	{ "id_string" },
#define X_ID_STRING	3
	{ "loadavg" },
#define X_LOADAVG	4
	{ "sq_avenrun" },
#define	X_SQ_AVENRUN	5
	{ "boottime" },
#define	X_BOOTTIME	6
	{ "" },
};

FILE	*ps;
FILE	*ut;
FILE	*bootfd;
int	kmem;
int	mem;
int	swap;			/* /dev/kmem, mem, and swap */
int	nswap;
int	dmmin, dmmax;
dev_t	tty;
int	uid;
char	doing[520];		/* process attached to terminal */
time_t	proctime;		/* cpu time of process in doing */
fix	loadavg[6];
fix	sq_avenrun[3];
struct	proc *aproc;

#define	DIV60(t)	((t+30)/60)    /* x/60 rounded */ 
#define	TTYEQ		(tty == pr[i].w_tty)
#define IGINT		(1+3*1)		/* ignoring both SIGINT & SIGQUIT */

char	*getargs();
char	*ctime();
char	*rindex();
FILE	*popen();
struct	tm *localtime();
time_t	findidle();

int	debug;			/* true if -d flag: debugging output */
int	ttywidth = 80;		/* width of tty */
int	header = 1;		/* true if -h flag: don't print heading */
int	lflag = 1;		/* true if -l flag: long style output */
int	prfrom = 0;		/* true if -f flag: print host from */
int	login;			/* true if invoked as login shell */
time_t	idle;			/* number of minutes user is idle */
int	nusers;			/* number of users logged in now */
char *	sel_user;		/* login of particular user selected */
char firstchar;			/* first char of name of prog invoked as */
time_t	jobtime;		/* total cpu time visible */
time_t	now;			/* the current time of day */
struct	timeval boottime;
time_t	uptime;			/* time of last reboot & elapsed time since */
int	np;			/* number of processes currently active */
struct	utmp utmp;
struct	proc mproc;
#define UPAGES 2
union {
	struct user U_up;
	char	pad[NBPP][UPAGES];
} Up;
#define	up	Up.U_up
struct	var	v;

int	page_size;
int	pte_to_pfn_shift;
int	pfn_to_byte_shift;
#define	pte_to_byte(x)	(((x) >> pte_to_pfn_shift) << pfn_to_byte_shift)
extern int	getpagesize();

main(argc, argv)
	char **argv;
{
	int days, hrs, mins;
	register int i, j;
	char *cp;
	register int curpid, empty;
	struct winsize win;

	page_size = getpagesize();
	switch (page_size) {
	case 4096:	pte_to_pfn_shift = 12;
			pfn_to_byte_shift = 12;
			break;
	case 16384:	pte_to_pfn_shift = 10;
			pfn_to_byte_shift = 14;
			break;
	default:	fprintf(stderr,"w: cannot handle %d byte page size\n",
				page_size);
			exit(1);
	} /* switch */

	login = (argv[0][0] == '-');
	cp = rindex(argv[0], '/');
	firstchar = login ? argv[0][1] : (cp==0) ? argv[0][0] : cp[1];
	cp = argv[0];	/* for Usage */

	while (argc > 1) {
		if (argv[1][0] == '-') {
			for (i=1; argv[1][i]; i++) {
				switch(argv[1][i]) {

				case 'd':
					debug++;
					break;

				case 'f':
					prfrom = !prfrom;
					break;

				case 'h':
					header = 0;
					break;

				case 'l':
					lflag++;
					break;

				case 's':
					lflag = 0;
					break;

				case 'u':
				case 'w':
					firstchar = argv[1][i];
					break;

				default:
					printf("Bad flag %s\n", argv[1]);
					exit(1);
				}
			}
		} else {
			if (!isalnum(argv[1][0]) || argc > 2) {
				printf("Usage: %s [ -hlsfuw ] [ user ]\n", cp);
				exit(1);
			} else
				sel_user = argv[1];
		}
		argc--; argv++;
	}

	if ((kmem = open("/dev/kmem", 0)) < 0) {
		fprintf(stderr, "No kmem\n");
		exit(1);
	}
	nlist("/unix", nl);
	if (nl[0].n_type==0) {
		fprintf(stderr, "No namelist\n");
		exit(1);
	}

#ifdef RISCOS
	/* Check to ensure that the kernel is strings match */
	i = check_kernel_id(kmem,
			       (long)nl[X_ID_STRING].n_value,
			       "/unix");

	if (i > 0)
	  { /* Print suitable error message before exiting */
	  switch (i)
	    {
	    case LIB_ERR_NOMATCH:
	     { /* wrong kernel id strings */
	     fprintf (stderr,
		      "w: Wrong kernel; kernel id strings mismatch\n");
	     break;
	     }

	    case LIB_ERR_KFILEOPEN:
	     { /* failed to open kernel file loaded */
	     fprintf(stderr, "w: failed to open kernel file %s\n", "/unix");
	     break;
	     }

	    default:
	     {
	    /*
	     * LIB_ERR_BADOFFSET 1   Bad offset value for symbol
	     * LIB_ERR_SHORTREAD 2   Short memory read error
	     * LIB_ERR_KLSEEK    4   Failed on kernel lseek operation
	     * LIB_ERR_KTEXTREAD 5   Failed reading kernel text segment
	     * LIB_ERR_KNODATA   6   Cannot locate data in kernel text file
	     */
	     fprintf (stderr,
 "w: check_kernel_id; mismatch between kernel file and memory; error= %d\n",
	      i);
	     break;
	     }
	  } /* End of switch */
	 exit (i);
	 } /* End of if */
#endif RISCOS

	if (firstchar == 'u')	/* uptime(1) */
		nl[X_BOOTTIME+1].n_name = "";
	else {			/* then read in procs, get window size */
		readpr();
		if (ioctl(1, TIOCGWINSZ, &win) != -1 && win.ws_col > 70)
			ttywidth = win.ws_col;
	}

	ut = fopen("/etc/utmp","r");
	time(&now);
	if (header) {
		/* Print time of day */
		prtat(&now);

		/*
		 * Print how long system has been up.
		 * (Found by looking for "boottime" in kernel)
		 */
		lseek(kmem, (long)nl[X_BOOTTIME].n_value, 0);
		read(kmem, &boottime, sizeof (boottime));

		uptime = now - boottime.tv_sec;
		uptime += 30;
		days = uptime / (60*60*24);
		uptime %= (60*60*24);
		hrs = uptime / (60*60);
		uptime %= (60*60);
		mins = uptime / 60;

		printf("  up");
		if (days > 0)
			printf(" %d day%s,", days, days>1?"s":"");
		if (hrs > 0 && mins > 0) {
			printf(" %2d:%02d,", hrs, mins);
		} else {
			if (hrs > 0)
				printf(" %d hr%s,", hrs, hrs>1?"s":"");
			if (mins > 0)
				printf(" %d min%s,", mins, mins>1?"s":"");
		}

		/* Print number of users logged in to system */
		while (fread(&utmp, sizeof(utmp), 1, ut)) {
			if (utmp.ut_name[0] != '\0' && !nonuser(utmp))
				nusers++;
		}
		rewind(ut);
		printf("  %d user%s", nusers, nusers == 1 ? "" : "s");

		/*
		 * Print 1, 5, and 15 minute load averages.
		 * (Found by looking in kernel for avenrun).
		 */
		printf(",  load average:");
		if (nl[X_LOADAVG].n_value) {
			(void)lseek(kmem, nl[X_LOADAVG].n_value, 0);
			(void)read(kmem, (char *)loadavg, sizeof (loadavg));
		}

		if (nl[X_SQ_AVENRUN].n_value) {
			(void)lseek(kmem, nl[X_SQ_AVENRUN].n_value, 0);
			(void)read(kmem, (char *)sq_avenrun, sizeof (sq_avenrun));
		}

		printf(" %.2f, %.2f, %.2f\n",
			FIX_TO_DBL(loadavg[3]),
			FIX_TO_DBL(loadavg[4]),
			FIX_TO_DBL(loadavg[5]));
		if (firstchar == 'u')	/* if this was uptime(1), finished */
			exit(0);

		/* Headers for rest of output */
		if (lflag && prfrom)
			printf("User     tty from           login@  idle   JCPU   PCPU  what\n");
		else if (lflag)
			printf("User     tty       login@  idle   JCPU   PCPU  what\n");
		else if (prfrom)
			printf("User    tty from            idle  what\n");
		else
			printf("User    tty  idle  what\n");
		fflush(stdout);
	}


	for (;;) {	/* for each entry in utmp */
		if (fread(&utmp, sizeof(utmp), 1, ut) == NULL) {
			fclose(ut);
			exit(0);
		}
		if (utmp.ut_name[0] == '\0' || nonuser(utmp))
			continue;	/* that tty is free */
		if (sel_user && strcmpn(utmp.ut_name, sel_user, NMAX) != 0)
			continue;	/* we wanted only somebody else */

		gettty();
		jobtime = 0;
		proctime = 0;
		strcpy(doing, "-");	/* default act: normally never prints */
		empty = 1;
		curpid = -1;
		idle = findidle();
		for (i=0; i<np; i++) {	/* for each process on this tty */
			if (!(TTYEQ))
				continue;
			jobtime += pr[i].w_time + pr[i].w_ctime;
			proctime += pr[i].w_time;
			/* 
			 * Meaning of debug fields following proc name is:
			 * & by itself: ignoring both SIGINT and QUIT.
			 *		(==> this proc is not a candidate.)
			 * & <i> <q>:   i is SIGINT status, q is quit.
			 *		0 == DFL, 1 == IGN, 2 == caught.
			 * *:		proc pgrp == tty pgrp.
			 */
			 if (debug) {
				printf("\t\t%d\t%s", pr[i].w_pid, pr[i].w_args);
				if ((j=pr[i].w_igintr) > 0)
					if (j==IGINT)
						printf(" &");
					else
						printf(" & %d %d", j%3, j/3);
				printf("\n");
			}
			if (empty && pr[i].w_igintr!=IGINT) {
				empty = 0;
				curpid = -1;
			}
			if(pr[i].w_pid>curpid && (pr[i].w_igintr!=IGINT || empty)){
				curpid = pr[i].w_pid;
				strcpy(doing, lflag ? pr[i].w_args : pr[i].w_comm);
#ifdef notdef
				if (doing[0]==0 || doing[0]=='-' && doing[1]<=' ' || doing[0] == '?') {
					strcat(doing, " (");
					strcat(doing, pr[i].w_comm);
					strcat(doing, ")");
				}
#endif
			}
		}
		putline();
	}
}

/* figure out the major/minor device # pair for this tty */
gettty()
{
	char ttybuf[20];
	struct stat statbuf;

	ttybuf[0] = 0;
	strcpy(ttybuf, "/dev/");
	strcat(ttybuf, utmp.ut_line);
	stat(ttybuf, &statbuf);
	tty = statbuf.st_rdev;
	uid = statbuf.st_uid;
}

/*
 * putline: print out the accumulated line of info about one user.
 */
putline()
{
	register int tm;
	int width = ttywidth - 1;

	/* print login name of the user */
	printf("%-*.*s ", NMAX, NMAX, utmp.ut_name);
	width -= NMAX + 1;

	/* print tty user is on */
	if (lflag && !prfrom) {
		/* long form: all (up to) LMAX chars */
		printf("%-*.*s", LMAX, LMAX, utmp.ut_line);
		width -= LMAX;
	 } else {
		/* short form: 2 chars, skipping 'tty' if there */
		if (utmp.ut_line[0]=='t' && utmp.ut_line[1]=='t' && utmp.ut_line[2]=='y')
			printf("%-2.2s", &utmp.ut_line[3]);
		else
			printf("%-2.2s", utmp.ut_line);
		width -= 2;
	}

	if (prfrom) {
#ifdef RISCOS
		printf(" %-14.14s", "");
#else RISCOS
		printf(" %-14.14s", utmp.ut_host);
#endif RISCOS
		width -= 15;
	}

	if (lflag) {
		/* print when the user logged in */
		prtat(&utmp.ut_time);
		width -= 8;
	}

	/* print idle time */
	if (idle >= 36 * 60)
		printf("%2ddays ", (idle + 12 * 60) / (24 * 60));
	else
		prttime(idle," ");
	width -= 7;

	if (lflag) {
		/* print CPU time for all processes & children */
		prttime(jobtime," ");
		width -= 7;
		/* print cpu time for interesting process */
		prttime(proctime," ");
		width -= 7;
	}

	/* what user is doing, either command tail or args */
	printf(" %-.*s\n", width-1, doing);
	fflush(stdout);
}

/* find & return number of minutes current tty has been idle */
time_t
findidle()
{
	struct stat stbuf;
	long lastaction, diff;
	char ttyname[20];

	strcpy(ttyname, "/dev/");
	strcatn(ttyname, utmp.ut_line, LMAX);
	stat(ttyname, &stbuf);
	time(&now);
	lastaction = stbuf.st_atime;
	diff = now - lastaction;
	diff = DIV60(diff);
	if (diff < 0) diff = 0;
	return(diff);
}

#define	HR	(60 * 60)
#define	DAY	(24 * HR)
#define	MON	(30 * DAY)

/*
 * prttime prints a time in hours and minutes or minutes and seconds.
 * The character string tail is printed at the end, obvious
 * strings to pass are "", " ", or "am".
 */
prttime(tim, tail)
	time_t tim;
	char *tail;
{

	if (tim >= 60) {
		printf("%3d:", tim/60);
		tim %= 60;
		printf("%02d", tim);
	} else if (tim > 0)
		printf("    %2d", tim);
	else
		printf("      ");
	printf("%s", tail);
}

char *weekday[] = { "Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat" };
char *month[] = { "Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec" };

/* prtat prints a 12 hour time given a pointer to a time of day */
prtat(time)
	long *time;
{
	struct tm *p;
	register int hr, pm;

	p = localtime(time);
	hr = p->tm_hour;
	pm = (hr > 11);
	if (hr > 11)
		hr -= 12;
	if (hr == 0)
		hr = 12;
	if (now - *time <= 18 * HR)
		prttime(hr * 60 + p->tm_min, pm ? "pm" : "am");
	else if (now - *time <= 7 * DAY)
		printf(" %s%2d%s", weekday[p->tm_wday], hr, pm ? "pm" : "am");
	else
		printf(" %2d%s%2d", p->tm_mday, month[p->tm_mon], p->tm_year);
}

/*
 * readpr finds and reads in the array pr, containing the interesting
 * parts of the proc and user tables for each live process.
 *
 * We only accept procs whos controlling tty has a pgrp equal to the
 * pgrp of the proc.  This accurately defines the notion of the current
 * process(s), but because of time skew, we always read in the stream head
 * after reading the proc, even though the same stream head may have been
 * read earlier on.
 */
readpr()
{
	int pn, mf, addr, c;
	int szpt, pfnum, i;
	int	swplo;
	struct	vnode	ttyvp;

	if((mem = open("/dev/mem", 0)) < 0) {
		fprintf(stderr, "No mem\n");
		exit(1);
	}
#ifndef RISCOS
	if ((swap = open("/dev/swap", 0)) < 0) {
		fprintf(stderr, "No swap\n");
		exit(1);
	}
#endif RISCOS
	/*
	 * Find base of and parameters of swap
	 */
	lseek(kmem, (long)nl[X_SWPLO].n_value, 0);
	read(kmem, &swplo, sizeof(swplo));
	/*
	 * Locate proc table
	 */
	lseek(kmem, (long)nl[X_V].n_value, 0);
	read(kmem, &v, sizeof(v));
	nproc = v.v_proc;
	pr = (struct pr *)calloc(nproc, sizeof (struct pr));
	np = 0;
	aproc = (struct proc *) nl[X_PROC].n_value;
	for (pn=0; pn<nproc; pn++) {
		lseek(kmem, (int)(aproc + pn), 0);
		read(kmem, &mproc, sizeof mproc);
		/* decide if it's an interesting process */
		if (mproc.p_stat==0 || mproc.p_stat==SZOMB 
		    || mproc.p_stat==SSTOP || mproc.p_jcpgrp==0)
			continue;
		/* find & read in the user structure */
		if ((mproc.p_flag & SLOAD) == 0) {
#ifdef RISCOS
			continue;
#else RISCOS
			/* not in memory - get from swap device */
			addr = dtob(mproc.p_swaddr);
			lseek(swap, (long)addr, 0);
			if (read(swap, &up, sizeof(up)) != sizeof(up)) {
				continue;
			      }
#endif RISCOS
		} else {
			/* loaded, get each page from memory separately */
			char *cp;
			char *cp1;
			int i;

			cp1 = ((char *)&up) + sizeof(Up);
			for (i = 0,cp = (char *) &up; cp < cp1; i++, cp += page_size) {

				lseek(mem,
					pte_to_byte(mproc.p_ubptbl[i].pgi.pg_pde),
				        0);
				if (read(mem, cp, cp1-cp > page_size ? page_size : cp1-cp) < 0)
					goto next_proc;
			}
#ifdef U_BSD43_EXTENSION
			if (up.u_bsd43_extension_p != NULL) {
				up.u_bsd43_extension_p = 
					(struct u_bsd43_extension *)
				   (((char *) up.u_bsd43_extension_p) -
					(((char *) UADDR) - ((char *) &up)));
			};
#endif U_BSD43_EXTENSION
		}
#ifndef RISCOS
		pr[np].w_lastpg = dtob(db.db_base);
#endif RISCOS
		if (mproc.p_ttyvp == NULL) {
next_proc:
			continue;
		};

		/* save the interesting parts */
		pr[np].w_pid = mproc.p_pid;
		pr[np].w_flag = mproc.p_flag;
#ifndef RISCOS
		pr[np].w_size = mproc.p_dsize + mproc.p_ssize; /* XXX */
#endif  RISCOS
		pr[np].w_igintr = (((int)up.u_signal[2]==1) +
		    2*((int)up.u_signal[2]>1) + 3*((int)up.u_signal[3]==1)) +
		    6*((int)up.u_signal[3]>1);
		pr[np].w_time =
		    up.u_ru.ru_utime.tv_sec + up.u_ru.ru_stime.tv_sec;
		pr[np].w_ctime =
		    up.u_cru.ru_utime.tv_sec + up.u_cru.ru_stime.tv_sec;
		lseek(kmem, (int)mproc.p_ttyvp, 0);
		read(kmem, &ttyvp, sizeof ttyvp);
		pr[np].w_tty = ttyvp.v_rdev;
		pr[np].w_uid = mproc.p_uid;
		up.u_comm[14] = 0;	/* Bug: This bombs next field. */
		strcpy(pr[np].w_comm, up.u_comm);
		/*
		 * Get args if there's a chance we'll print it.
		 * Cant just save pointer: getargs returns static place.
		 * Cant use strcpyn: that crock blank pads.
		 */
		pr[np].w_args[0] = 0;
		strcatn(pr[np].w_args,up.u_psargs,ARGWIDTH);
		if (pr[np].w_args[0]==0 || pr[np].w_args[0]=='-' && pr[np].w_args[1]<=' ' || pr[np].w_args[0] == '?') {
			strcat(pr[np].w_args, " (");
			strcat(pr[np].w_args, pr[np].w_comm);
			strcat(pr[np].w_args, ")");
		}
		np++;
	}
}


#ifndef RISCOS
/*
 * Given a base/size pair in virtual swap area,
 * return a physical base/size pair which is the
 * (largest) initial, physically contiguous block.
 */
vstodb(vsbase, vssize, dmp, dbp, rev)
	register int vsbase;
	int vssize;
	struct dmap *dmp;
	register struct dblock *dbp;
{
	register int blk = dmmin;
	register swblk_t *ip = dmp->dm_map;

	vsbase = ctod(vsbase);
	vssize = ctod(vssize);
	if (vsbase < 0 || vsbase + vssize > dmp->dm_size)
		panic("vstodb");
	while (vsbase >= blk) {
		vsbase -= blk;
		if (blk < dmmax)
			blk *= 2;
		ip++;
	}
	if (*ip <= 0 || *ip + blk > nswap)
		panic("vstodb *ip");
	dbp->db_size = min(vssize, blk - vsbase);
	dbp->db_base = *ip + (rev ? blk - (vsbase + dbp->db_size) : vsbase);
}
#endif RISCOS

panic(cp)
	char *cp;
{

	/* printf("%s\n", cp); */
}

min(a, b)
{

	return (a < b ? a : b);
}


max(a, b)
{
	return (a > b ? a : b);
}
