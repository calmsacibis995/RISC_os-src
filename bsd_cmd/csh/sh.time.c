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
#ident	"$Header: sh.time.c,v 1.2.1.2 90/05/07 18:18:16 wje Exp $"

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley Software License Agreement
 * specifies the terms and conditions for redistribution.
 */

#ifndef lint
static char *sccsid = "@(#)sh.time.c	5.6 (Berkeley) 5/19/88";
#endif

#include "sh.h"

/*
 * C Shell - routines handling process timing and niceing
 */

settimes()
{
	struct rusage ruch;

	(void) gettimeofday(&time0, (struct timezone *)0);
	(void) getrusage(RUSAGE_SELF, &ru0);
	(void) getrusage(RUSAGE_CHILDREN, &ruch);
	ruadd(&ru0, &ruch);
}

/*
 * dotime is only called if it is truly a builtin function and not a
 * prefix to another command
 */
dotime()
{
	struct timeval timedol;
	struct rusage ru1, ruch;

	(void) getrusage(RUSAGE_SELF, &ru1);
	(void) getrusage(RUSAGE_CHILDREN, &ruch);
	ruadd(&ru1, &ruch);
	(void) gettimeofday(&timedol, (struct timezone *)0);
	prusage(&ru0, &ru1, &timedol, &time0);
}

/*
 * donice is only called when it on the line by itself or with a +- value
 */
donice(v)
	register char **v;
{
	register char *cp;
	int nval;

	v++, cp = *v++;
	if (cp == 0)
		nval = 4;
	else if (*v == 0 && any(cp[0], "+-"))
		nval = getn(cp);
	(void) setpriority(PRIO_PROCESS, 0, nval);
}

ruadd(ru, ru2)
	register struct rusage *ru, *ru2;
{
	register long *lp, *lp2;
	register int cnt;

	tvadd(&ru->ru_utime, &ru2->ru_utime);
	tvadd(&ru->ru_stime, &ru2->ru_stime);
	if (ru2->ru_maxrss > ru->ru_maxrss)
		ru->ru_maxrss = ru2->ru_maxrss;
	cnt = &ru->ru_last - &ru->ru_first + 1;
	lp = &ru->ru_first; lp2 = &ru2->ru_first;
	do
		*lp++ += *lp2++;
	while (--cnt > 0);
}

prusage(r0, r1, e, b)
	register struct rusage *r0, *r1;
	struct timeval *e, *b;
{
	static int kb_per_page = 0;
	register time_t t =
	    (r1->ru_utime.tv_sec-r0->ru_utime.tv_sec)*100+
	    (r1->ru_utime.tv_usec-r0->ru_utime.tv_usec)/10000+
	    (r1->ru_stime.tv_sec-r0->ru_stime.tv_sec)*100+
	    (r1->ru_stime.tv_usec-r0->ru_stime.tv_usec)/10000;
	register char *cp;
	register long i;
	register struct varent *vp = adrof("time");
	long ms =
	    (e->tv_sec-b->tv_sec)*100 + (e->tv_usec-b->tv_usec)/10000;

	/*
	 * Get number of kilobytes per page. Must be an integer.
	 */

	if (kb_per_page == 0) {
		kb_per_page = getpagesize()/1024;
	}

	cp = "%Uu %Ss %E %P %X+%Dk %I+%Oio %Fpf+%Ww";
	if (vp && vp->vec[0] && vp->vec[1])
		cp = vp->vec[1];
	for (; *cp; cp++)
	if (*cp != '%')
		cshputchar(*cp);
	else if (cp[1]) switch(*++cp) {

	case 'U':
		pdeltat(&r1->ru_utime, &r0->ru_utime);
		break;

	case 'S':
		pdeltat(&r1->ru_stime, &r0->ru_stime);
		break;

	case 'E':
		psecs(ms / 100);
		break;

	case 'P':
		/*
		 * ms = total time
		 * t  = system + user time
		 *
		 * Due to the fact that these are stored with different
		 * units and are rounded differently, t can be bigger
		 * than ms, which would cause a number bigger than 100%.
		 * In this case, or when ms is 0, we just print 100%.
		 */

		if (t > ms || ms == 0) {
			printf("100%%");
		} else {
			printf("%d%%", (int) (t*100 / ms));
		}
		break;

	case 'W':
		i = r1->ru_nswap - r0->ru_nswap;
		printf("%ld", i);
		break;

	case 'X':
		printf("%ld", t == 0 ? 0L :
		    ((r1->ru_ixrss-r0->ru_ixrss) * kb_per_page)/t);
		break;

	case 'D':
		printf("%ld", t == 0 ? 0L :
		    ((r1->ru_idrss+r1->ru_isrss-(r0->ru_idrss+r0->ru_isrss)) *
		    kb_per_page)/t);
		break;

	case 'K':
		printf("%ld", t == 0 ? 0L :
		    (((r1->ru_ixrss+r1->ru_isrss+r1->ru_idrss) -
		    (r0->ru_ixrss+r0->ru_idrss+r0->ru_isrss)) * kb_per_page)/t);
		break;

	case 'M':
		printf("%ld", (r1->ru_maxrss * kb_per_page)/2);
		break;

	case 'F':
		printf("%ld", r1->ru_majflt-r0->ru_majflt);
		break;

	case 'R':
		printf("%ld", r1->ru_minflt-r0->ru_minflt);
		break;

	case 'I':
		printf("%ld", r1->ru_inblock-r0->ru_inblock);
		break;

	case 'O':
		printf("%ld", r1->ru_oublock-r0->ru_oublock);
		break;
	}
	cshputchar('\n');
}

pdeltat(t1, t0)
	struct timeval *t1, *t0;
{
	struct timeval td;

	tvsub(&td, t1, t0);
	printf("%d.%01d", td.tv_sec, td.tv_usec/100000);
}

tvadd(tsum, t0)
	struct timeval *tsum, *t0;
{

	tsum->tv_sec += t0->tv_sec;
	tsum->tv_usec += t0->tv_usec;
	if (tsum->tv_usec > 1000000)
		tsum->tv_sec++, tsum->tv_usec -= 1000000;
}

tvsub(tdiff, t1, t0)
	struct timeval *tdiff, *t1, *t0;
{

	tdiff->tv_sec = t1->tv_sec - t0->tv_sec;
	tdiff->tv_usec = t1->tv_usec - t0->tv_usec;
	if (tdiff->tv_usec < 0)
		tdiff->tv_sec--, tdiff->tv_usec += 1000000;
}
