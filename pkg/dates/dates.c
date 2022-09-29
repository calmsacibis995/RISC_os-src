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
#ident	"$Header: dates.c,v 1.4.2.2 90/05/10 03:45:24 wje Exp $"
#define MAIN 1
#include "dates.h"
static char rcs[]="$Header: dates.c,v 1.4.2.2 90/05/10 03:45:24 wje Exp $";

#define OPTIONS	"fl:ns:u:"

/*
** The following items are used by the gtime() routine lifted from date.c
*/
struct timeval tv, now;
#ifdef BSD
#else
extern long timezone;
extern int daylight;
extern char *tzname[2];
#endif

main(argc, argv)
int argc;
char *argv[];
{
   extern int optind;
   extern char *optarg;
   int c;
   time_t tmpclock;

   struct stat sbuf;
   struct tm *ttm;
   long   atime = 0;

   while ((c = getopt(argc, argv, OPTIONS)) != EOF)
     switch (c)
       {
	 case 'f': while (optind < argc)
	             if ((stat(argv[optind++], &sbuf)) == 0)
		       {
			 printf("%s:\n", argv[optind-1]);
                         showdate(sbuf.st_mtime);
		       }
	           exit(0);
	           break;
	 case 'l': ttm = localtime(&atime);
	           if (gtime(optarg)) usage();
#ifdef BSD
 	           showdate(tv.tv_sec-(ttm->tm_gmtoff));
#else
		   tv.tv_sec += timezone;
		   tmpclock = tv.tv_sec;
		   if (localtime(&tmpclock)->tm_isdst) {
			tv.tv_sec -= (timezone - altzone);
		   }
	           showdate(tv.tv_sec);
#endif
	           exit(0);
	           break;
	 case 'n': showdate(time((long)0));
	           exit(0);
	           break;
	 case 's': showdate(atol(optarg));
	           exit(0);
	           break;
	 case 'u': if (gtime(optarg)) usage();
	           showdate(tv.tv_sec);
	           exit(0);
	           break;
       }

      usage();

}

showdate(s)
long s;
{
  struct tm *tmpt;
  char loc[64], gmt[64];

#ifdef BSD
#else
  daylight = 1;
#endif
  tmpt = localtime(&s);

  sprintf(loc, "%02d-%02d-%02d %02d:%02d:%02d %s",
            tmpt->tm_mon+1,
	    tmpt->tm_mday,
	    tmpt->tm_year,
	    tmpt->tm_hour,
	    tmpt->tm_min,
	    tmpt->tm_sec,
#ifdef BSD
	    tmpt->tm_zone);
#else
	    tzname[tmpt->tm_isdst]);
#endif

  tmpt = gmtime(&s);
  sprintf(gmt, "%02d-%02d-%02d %02d:%02d:%02d %s",
            tmpt->tm_mon+1,
	    tmpt->tm_mday,
	    tmpt->tm_year,
	    tmpt->tm_hour,
	    tmpt->tm_min,
	    tmpt->tm_sec,
#ifdef BSD
	    tmpt->tm_zone);
#else
	    "GMT");
#endif


  printf("%13ld   %s   %s\n", s, loc, gmt);
}

usage()
{
  fprintf(stderr, 
"usage: dates: -now | -f file ... | -s seconds | [-l | -u] yymmddhhmm[.ss]\n");
  exit(1);
}

/*
** Following lifted from BSD date.c
*/

#define	ATOI2(ar)	(ar[0] - '0') * 10 + (ar[1] - '0'); ar += 2;

static	int dmsize[] =
    { -1, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };

/*
 * gtime --
 *	convert user's time into number of seconds
 */
static
gtime(ap)
	register char	*ap;		/* user argument */
{
	register int	year, month;
	register char	*C;		/* pointer into time argument */
	struct tm	*L;
	int	day, hour, mins, secs;

	for (secs = 0, C = ap;*C;++C) {
		if (*C == '.') {		/* seconds provided */
			if (strlen(C) != 3)
				return(1);
			*C = NULL;
			secs = (C[1] - '0') * 10 + (C[2] - '0');
			break;
		}
		if (!isdigit(*C))
			return(-1);
	}

#ifdef BSD
	time((time_t *)&tv.tv_sec);
#endif
	L = localtime((time_t *)&tv.tv_sec);
	year = L->tm_year;			/* defaults */
	month = L->tm_mon + 1;
	day = L->tm_mday;

	switch ((int)(C - ap)) {		/* length */
		case 10:			/* yymmddhhmm */
			year = ATOI2(ap);
		case 8:				/* mmddhhmm */
			month = ATOI2(ap);
		case 6:				/* ddhhmm */
			day = ATOI2(ap);
		case 4:				/* hhmm */
			hour = ATOI2(ap);
			mins = ATOI2(ap);
			break;
		default:
			return(1);
	}

	if (*ap || month < 1 || month > 12 || day < 1 || day > 31 ||
	     mins < 0 || mins > 59 || secs < 0 || secs > 59)
		return(1);
	if (hour == 24) {
		++day;
		hour = 0;
	}
	else if (hour < 0 || hour > 23)
		return(1);

	tv.tv_sec = 0;
	year += TM_YEAR_BASE;
	if (isleap(year) && month > 2)
		++tv.tv_sec;
	for (--year;year >= EPOCH_YEAR;--year)
		tv.tv_sec += isleap(year) ? DAYS_PER_LYEAR : DAYS_PER_NYEAR;
	while (--month)
		tv.tv_sec += dmsize[month];
	tv.tv_sec += day - 1;
	tv.tv_sec = HOURS_PER_DAY * tv.tv_sec + hour;
	tv.tv_sec = MINS_PER_HOUR * tv.tv_sec + mins;
	tv.tv_sec = SECS_PER_MIN * tv.tv_sec + secs;
	return(0);
}
