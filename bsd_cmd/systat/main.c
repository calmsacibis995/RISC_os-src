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
#ident	"$Header: main.c,v 1.3.1.3 90/05/07 19:31:06 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#include "systat.h"
#ifdef RISCOS
#define signal sigset
#endif RISCOS

#ifndef RISCOS
static struct nlist nlst[] = {
#define X_CCPU          0
        { "_ccpu" },
#define X_AVENRUN       1
        { "_avenrun" },
#define	X_HZ		2
	{ "_hz" },
#define	X_PHZ		3
	{ "_phz" },
        { "" }
};
#else RISCOS
static struct nlist nlst[] = {
#define X_CCPU          0
        { "ccpu" },
#define X_AVENRUN       1
        { "avenrun" },
#define X_ID_STRING	2
	{ "id_string" },
        { "" }
};
#endif RISCOS

int     kmem = -1;
int     mem = -1;
#ifndef RISCOS
int     swap = -1;
#endif RISCOS
int	naptime = 5;

int     die();
int     display();
int     suspend();
int	(*sigtstpdfl)();

double	ccpu;
int     dellave;

static	WINDOW *wload;			/* one line window for load average */

main(argc, argv)
        int argc;
        char **argv;
{

	argc--, argv++;
	while (argc > 0) {
		if (argv[0][0] == '-') {
			struct cmdtab *p;

			p = lookup(&argv[0][1]);
			if (p == (struct cmdtab *)-1 || p == NULL) {
				fprintf(stderr, "%s: unknown request\n",
				    &argv[0][1]);
				exit(1);
			}
			curcmd = p;
		} else {
			naptime = atoi(argv[0]);
			if (naptime <= 0)
				naptime = 5;
		}
		argc--, argv++;
	}
#ifdef RISCOS
        nlist("/unix", nlst);
	if (nlst[X_CCPU].n_type == 0) {
		fprintf(stderr, "Couldn't namelist /unix.\n");
		exit(1);
	}
#else RISCOS
        nlist("/vmunix", nlst);
	if (nlst[X_CCPU].n_type == 0) {
		fprintf(stderr, "Couldn't namelist /vmunix.\n");
		exit(1);
	}
#endif RISCOS
	kmemf = "/dev/kmem";
	kmem = open(kmemf, O_RDONLY);
	if (kmem < 0) {
		perror(kmemf);
		exit(1);
	}
	memf = "/dev/mem";
	mem = open(memf, O_RDONLY);
	if (mem < 0) {
		perror(memf);
		exit(1);
	}
#ifdef RISCOS
	/* Check to ensure that the kernel is strings match */
	if (check_kernel_id (kmem,
			(long)nlst[X_ID_STRING].n_value, "/unix") > 0) {
		fprintf(stderr,"/unix does not match /dev/kmem\n");
		exit(1);
	};
#else RISCOS
	swapf = "/dev/drum";
	swap = open(swapf, O_RDONLY);
	if (swap < 0) {
		perror(swapf);
		exit(1);
	}
#endif RISCOS
        signal(SIGINT, die);
        signal(SIGQUIT, die);
        signal(SIGTERM, die);

        /*
	 * Initialize display.  Load average appears in a one line
	 * window of its own.  Current command's display appears in
	 * an overlapping sub-window of stdscr configured by the display
	 * routines to minimize update work by curses.
	 */
        initscr();
	CMDLINE = LINES - 1;
	wnd = (*curcmd->c_open)();
	if (wnd == NULL) {
		fprintf(stderr, "Couldn't initialize display.\n");
		die();
	}
	wload = newwin(1, 0, 3, 20);
	if (wload == NULL) {
		fprintf(stderr, "Couldn't set up load average window.\n");
		die();
	}

        gethostname(hostname, sizeof (hostname));
        lseek(kmem, nlst[X_CCPU].n_value, L_SET);
#ifdef RISCOS
	{
	  	fix	ccpu_tmp;
	        read(kmem, &ccpu_tmp, sizeof (ccpu_tmp));
		ccpu = FIX_TO_DBL(ccpu_tmp);
	}
#else RISCOS
        read(kmem, &ccpu, sizeof (ccpu));
#endif RISCOS
        lccpu = log(ccpu);
#ifdef RISCOS
	hz = 100;
	phz = 100;
#else RISCOS
	hz = getw(nlst[X_HZ].n_value);
	phz = getw(nlst[X_PHZ].n_value);
#endif RISCOS
	(*curcmd->c_init)();
	curcmd->c_flags |= CF_INIT;
        labels();

        known[0].k_uid = -1;
	known[0].k_name[0] = '\0';
        numknown = 1;
	procs[0].pid = -1;
	strcpy(procs[0].cmd, "<idle>");
	numprocs = 1;
        dellave = 0.0;

        signal(SIGALRM, display);
#ifdef RISCOS
        sigtstpdfl = (int (*)()) signal(SIGTSTP, suspend);
#else RISCOS
        sigtstpdfl = signal(SIGTSTP, suspend);
#endif RISCOS
        display();
        noecho();
        crmode();
	keyboard();
	/*NOTREACHED*/
}

labels()
{

	if (curcmd->c_flags & CF_LOADAV) {
		mvaddstr(2, 20,
		    "/0   /1   /2   /3   /4   /5   /6   /7   /8   /9   /10");
		mvaddstr(3, 5, "Load Average");
	}
        (*curcmd->c_label)();
#ifdef notdef
        mvprintw(21, 25, "CPU usage on %s", hostname);
#endif
        refresh();
}

display()
{
        register int i, j;

        /* Get the load average over the last minute. */
        lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
#ifdef RISCOS
	{
		fix	avenrun_tmp[3];
		read(kmem,avenrun_tmp,sizeof(avenrun_tmp));
		avenrun[0] = FIX_TO_DBL(avenrun_tmp[0]);
		avenrun[1] = FIX_TO_DBL(avenrun_tmp[1]);
		avenrun[2] = FIX_TO_DBL(avenrun_tmp[2]);
	}
#else RISCOS
	read(kmem, avenrun, sizeof (avenrun));
#endif RISCOS
        (*curcmd->c_fetch)();
	if (curcmd->c_flags & CF_LOADAV) {
		j = 5.0*avenrun[0] + 0.5;
		dellave -= avenrun[0];
		if (dellave >= 0.0)
			c = '<';
		else {
			c = '>';
			dellave = -dellave;
		}
		if (dellave < 0.1)
			c = '|';
		dellave = avenrun[0];
		wmove(wload, 0, 0); wclrtoeol(wload);
		for (i = (j > 50) ? 50 : j; i > 0; i--)
			waddch(wload, c);
		if (j > 50)
			wprintw(wload, " %4.1f", avenrun[0]);
	}
        (*curcmd->c_refresh)();
	if (curcmd->c_flags & CF_LOADAV)
		wrefresh(wload);
        wrefresh(wnd);
        move(CMDLINE, col);
        refresh();
        alarm(naptime);
}

load()
{
	double	avenrun[3];

	lseek(kmem, nlst[X_AVENRUN].n_value, L_SET);
#ifdef RISCOS
	{
		fix	avenrun_tmp[3];
		read(kmem,avenrun_tmp,sizeof(avenrun_tmp));
		avenrun[0] = FIX_TO_DBL(avenrun_tmp[0]);
		avenrun[1] = FIX_TO_DBL(avenrun_tmp[1]);
		avenrun[2] = FIX_TO_DBL(avenrun_tmp[2]);
	}
#else RISCOS
	read(kmem, avenrun, sizeof (avenrun));
#endif RISCOS
	mvprintw(CMDLINE, 0, "%4.1f %4.1f %4.1f",
	    avenrun[0], avenrun[1], avenrun[2]);
	clrtoeol();
}

die()
{

        endwin();
        exit(0);
}

error(fmt, a1, a2, a3)
{

	mvprintw(CMDLINE, 0, fmt, a1, a2, a3);
	clrtoeol();
	refresh();
}
