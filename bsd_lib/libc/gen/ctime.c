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
#ident	"$Header: ctime.c,v 1.2.1.2 90/05/07 20:35:17 wje Exp $"

/*
 * Copyright (c) 1987 Regents of the University of California.
 * This file may be freely redistributed provided that this
 * notice remains attached.
 */

#include "sys/param.h"
#include "sys/time.h"
#include "tzfile.h"

char *
ctime(t)
time_t *t;
{
	struct tm	*localtime();
	char	*asctime();

	return(asctime(localtime(t)));
}

/*
** A la X3J11
*/

char *
asctime(timeptr)
register struct tm *	timeptr;
{
	static char	wday_name[DAYS_PER_WEEK][3] = {
		"Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"
	};
	static char	mon_name[MONS_PER_YEAR][3] = {
		"Jan", "Feb", "Mar", "Apr", "May", "Jun",
		"Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
	};
	static char	result[26];

	(void) sprintf(result, "%.3s %.3s%3d %02d:%02d:%02d %d\n",
		wday_name[timeptr->tm_wday],
		mon_name[timeptr->tm_mon],
		timeptr->tm_mday, timeptr->tm_hour,
		timeptr->tm_min, timeptr->tm_sec,
		TM_YEAR_BASE + timeptr->tm_year);
	return result;
}

#ifndef TRUE
#define TRUE		1
#define FALSE		0
#endif /* !TRUE */

extern char *		getenv();
extern char *		strcpy();
extern char *		strcat();
struct tm *		offtime();

struct ttinfo {				/* time type information */
	long		tt_gmtoff;	/* GMT offset in seconds */
	int		tt_isdst;	/* used to set tm_isdst */
	int		tt_abbrind;	/* abbreviation list index */
};

struct state {
	int		timecnt;
	int		typecnt;
	int		charcnt;
	time_t		ats[TZ_MAX_TIMES];
	unsigned char	types[TZ_MAX_TIMES];
	struct ttinfo	ttis[TZ_MAX_TYPES];
	char		chars[TZ_MAX_CHARS + 1];
};

static struct state	s;

static int		tz_is_set;

char *			tzname[2] = {
	"GMT",
	"GMT"
};

#ifdef USG_COMPAT
time_t			timezone = 0;
int			daylight = 0;
#endif /* USG_COMPAT */

static long
detzcode(codep)
char *	codep;
{
	register long	result;
	register int	i;

	result = 0;
	for (i = 0; i < 4; ++i)
		result = (result << 8) | (codep[i] & 0xff);
	return result;
}

static
tzload(name)
register char *	name;
{
	register int	i;
	register int	fid;

	if (name == 0 && (name = TZDEFAULT) == 0)
		return -1;
	{
		register char *	p;
		register int	doaccess;
		char		fullname[MAXPATHLEN];

		doaccess = name[0] == '/';
		if (!doaccess) {
			if ((p = TZDIR) == 0)
				return -1;
			if ((strlen(p) + strlen(name) + 1) >= sizeof fullname)
				return -1;
			(void) strcpy(fullname, p);
			(void) strcat(fullname, "/");
			(void) strcat(fullname, name);
			/*
			** Set doaccess if '.' (as in "../") shows up in name.
			*/
			while (*name != '\0')
				if (*name++ == '.')
					doaccess = TRUE;
			name = fullname;
		}
		if (doaccess && access(name, 4) != 0)
			return -1;
		if ((fid = open(name, 0)) == -1)
			return -1;
	}
	{
		register char *			p;
		register struct tzhead *	tzhp;
		char				buf[sizeof s];

		i = read(fid, buf, sizeof buf);
		if (close(fid) != 0 || i < sizeof *tzhp)
			return -1;
		tzhp = (struct tzhead *) buf;
		s.timecnt = (int) detzcode(tzhp->tzh_timecnt);
		s.typecnt = (int) detzcode(tzhp->tzh_typecnt);
		s.charcnt = (int) detzcode(tzhp->tzh_charcnt);
		if (s.timecnt > TZ_MAX_TIMES ||
			s.typecnt == 0 ||
			s.typecnt > TZ_MAX_TYPES ||
			s.charcnt > TZ_MAX_CHARS)
				return -1;
		if (i < sizeof *tzhp +
			s.timecnt * (4 + sizeof (char)) +
			s.typecnt * (4 + 2 * sizeof (char)) +
			s.charcnt * sizeof (char))
				return -1;
		p = buf + sizeof *tzhp;
		for (i = 0; i < s.timecnt; ++i) {
			s.ats[i] = detzcode(p);
			p += 4;
		}
		for (i = 0; i < s.timecnt; ++i)
			s.types[i] = (unsigned char) *p++;
		for (i = 0; i < s.typecnt; ++i) {
			register struct ttinfo *	ttisp;

			ttisp = &s.ttis[i];
			ttisp->tt_gmtoff = detzcode(p);
			p += 4;
			ttisp->tt_isdst = (unsigned char) *p++;
			ttisp->tt_abbrind = (unsigned char) *p++;
		}
		for (i = 0; i < s.charcnt; ++i)
			s.chars[i] = *p++;
		s.chars[i] = '\0';	/* ensure '\0' at end */
	}
	/*
	** Check that all the local time type indices are valid.
	*/
	for (i = 0; i < s.timecnt; ++i)
		if (s.types[i] >= s.typecnt)
			return -1;
	/*
	** Check that all abbreviation indices are valid.
	*/
	for (i = 0; i < s.typecnt; ++i)
		if (s.ttis[i].tt_abbrind >= s.charcnt)
			return -1;
	/*
	** Set tzname elements to initial values.
	*/
	tzname[0] = tzname[1] = &s.chars[0];
#ifdef USG_COMPAT
	timezone = s.ttis[0].tt_gmtoff;
	daylight = 0;
#endif /* USG_COMPAT */
	for (i = 1; i < s.typecnt; ++i) {
		register struct ttinfo *	ttisp;

		ttisp = &s.ttis[i];
		if (ttisp->tt_isdst) {
			tzname[1] = &s.chars[ttisp->tt_abbrind];
#ifdef USG_COMPAT
			daylight = 1;
#endif /* USG_COMPAT */ 
		} else {
			tzname[0] = &s.chars[ttisp->tt_abbrind];
#ifdef USG_COMPAT
			timezone = ttisp->tt_gmtoff;
#endif /* USG_COMPAT */ 
		}
	}
	return 0;
}

static
tzsetkernel()
{
	struct timeval	tv;
	struct timezone	tz;
	char	*_tztab();
	static dstsetkernel();

	if (gettimeofday(&tv, &tz))
		return -1;
	s.timecnt = 0;
	s.typecnt = 1;
			/* UNIX counts *west* of Greenwich */
	s.ttis[0].tt_gmtoff = tz.tz_minuteswest * -SECS_PER_MIN;
	s.ttis[0].tt_isdst = 0;
	s.ttis[0].tt_abbrind = 0;
	(void)strcpy(s.chars, _tztab(tz.tz_minuteswest, 0));
	tzname[0] = tzname[1] = s.chars;
#ifdef USG_COMPAT
	timezone = tz.tz_minuteswest * 60;
	daylight = tz.tz_dsttime;
#endif /* USG_COMPAT */

	if(tz.tz_dsttime)
		dstsetkernel(&tz);

#ifdef DEBUG
	printstate();
#endif

	return 0;
}

static
tzsetgmt()
{
	s.timecnt = 0;
	s.ttis[0].tt_gmtoff = 0;
	s.ttis[0].tt_abbrind = 0;
	(void) strcpy(s.chars, "GMT");
	tzname[0] = tzname[1] = s.chars;
#ifdef USG_COMPAT
	timezone = 0;
	daylight = 0;
#endif /* USG_COMPAT */
}

void
tzset()
{
	register char *	name;

	tz_is_set = TRUE;
	name = getenv("TZ");
	if (!name || *name) {			/* did not request GMT */
		if (name && !tzload(name))	/* requested name worked */
			return;
		if (!tzload((char *)0))		/* default name worked */
			return;
		if (!tzsetkernel())		/* kernel guess worked */
			return;
	}
	tzsetgmt();				/* GMT is default */
}

struct tm *
localtime(timep)
time_t *	timep;
{
	register struct ttinfo *	ttisp;
	register struct tm *		tmp;
	register int			i;
	time_t				t;

	if (!tz_is_set)
		(void) tzset();
	t = *timep;
	if (s.timecnt == 0 || t < s.ats[0]) {
		i = 0;
		while (s.ttis[i].tt_isdst)
			if (++i >= s.timecnt) {
				i = 0;
				break;
			}
	} else {
		for (i = 1; i < s.timecnt; ++i)
			if (t < s.ats[i])
				break;
		i = s.types[i - 1];
	}
	ttisp = &s.ttis[i];
	/*
	** To get (wrong) behavior that's compatible with System V Release 2.0
	** you'd replace the statement below with
	**	tmp = offtime((time_t) (t + ttisp->tt_gmtoff), 0L);
	*/
	tmp = offtime(&t, ttisp->tt_gmtoff);
	tmp->tm_isdst = ttisp->tt_isdst;
	tzname[tmp->tm_isdst] = &s.chars[ttisp->tt_abbrind];
	tmp->tm_zone = &s.chars[ttisp->tt_abbrind];
	return tmp;
}

struct tm *
gmtime(clock)
time_t *	clock;
{
	register struct tm *	tmp;

	tmp = offtime(clock, 0L);
	tzname[0] = "GMT";
	tmp->tm_zone = "GMT";		/* UCT ? */
	return tmp;
}

static int	mon_lengths[2][MONS_PER_YEAR] = {
	31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
	31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static int	year_lengths[2] = {
	DAYS_PER_NYEAR, DAYS_PER_LYEAR
};

struct tm *
offtime(clock, offset)
time_t *	clock;
long		offset;
{
	register struct tm *	tmp;
	register long		days;
	register long		rem;
	register int		y;
	register int		yleap;
	register int *		ip;
	static struct tm	tm;

	tmp = &tm;
	days = *clock / SECS_PER_DAY;
	rem = *clock % SECS_PER_DAY;
	rem += offset;
	while (rem < 0) {
		rem += SECS_PER_DAY;
		--days;
	}
	while (rem >= SECS_PER_DAY) {
		rem -= SECS_PER_DAY;
		++days;
	}
	tmp->tm_hour = (int) (rem / SECS_PER_HOUR);
	rem = rem % SECS_PER_HOUR;
	tmp->tm_min = (int) (rem / SECS_PER_MIN);
	tmp->tm_sec = (int) (rem % SECS_PER_MIN);
	tmp->tm_wday = (int) ((EPOCH_WDAY + days) % DAYS_PER_WEEK);
	if (tmp->tm_wday < 0)
		tmp->tm_wday += DAYS_PER_WEEK;
	y = EPOCH_YEAR;
	if (days >= 0)
		for ( ; ; ) {
			yleap = isleap(y);
			if (days < (long) year_lengths[yleap])
				break;
			++y;
			days = days - (long) year_lengths[yleap];
		}
	else do {
		--y;
		yleap = isleap(y);
		days = days + (long) year_lengths[yleap];
	} while (days < 0);
	tmp->tm_year = y - TM_YEAR_BASE;
	tmp->tm_yday = (int) days;
	ip = mon_lengths[yleap];
	for (tmp->tm_mon = 0; days >= (long) ip[tmp->tm_mon]; ++(tmp->tm_mon))
		days = days - (long) ip[tmp->tm_mon];
	tmp->tm_mday = (int) (days + 1);
	tmp->tm_isdst = 0;
	tmp->tm_zone = "";
	tmp->tm_gmtoff = offset;
	return tmp;
}

/*
 *  Backwards-compatible DST information tables.
 *
 *  The tables give the day number of the first day after the
 *  Sunday of the change.
 *
 *  DO NOT FIX THESE TABLES.
 *  Yes, they're wrong in several ways, including 1987 and beyond
 *  in the United States, but they happen to match the old ctime.c
 *  that is compiled into virtually all programs under 4.2bsd-
 *  derived systems.  This is important if programs compiled with
 *  this version of ctime are to work correctly when shipped (in
 *  binary form) to systems which have not upgraded to the
 *  /etc/zoneinfo scheme.
 *
 *  These hardwired tables are only used when /etc/zoneinfo cannot
 *  be accessed.  A system which does not have /etc/zoneinfo is
 *  probably handling DST fluctuations by changing the system clock.
 *  Therefore, all programs on such a system (whether linked with
 *  old or new versions of ctime) must use the same DST rules.
 *  On a system with old-fashioned versions of ctime, handling
 *  DST fluctuations by changing the system clock, a "correct"
 *  version of ctime would in fact display incorrect results.
 */

struct dstab {
	int	dayyr;
	int	daylb;
	int	dayle;
};

static struct dstab usdaytab[] = {
	1974,	5,	333,	/* 1974: Jan 6 - last Sun. in Nov */
	1975,	58,	303,	/* 1975: Last Sun. in Feb - last Sun in Oct */
	0,	119,	303,	/* all other years: end Apr - end Oct */
};

static struct dstab ausdaytab[] = {
	1970,	400,	0,	/* 1970: no daylight saving at all */
	1971,	303,	0,	/* 1971: daylight saving from Oct 31 */
	1972,	303,	58,	/* 1972: Jan 1 -> Feb 27 & Oct 31 -> dec 31 */
	0,	303,	65,	/* others: -> Mar 7, Oct 31 -> */
};

/*
 * The European tables ... based on hearsay
 * Believed correct for:
 *	WE:	Great Britain, Ireland, Portugal
 *	ME:	Belgium, Luxembourg, Netherlands, Denmark, Norway,
 *		Austria, Poland, Czechoslovakia, Sweden, Switzerland,
 *		DDR, DBR, France, Spain, Hungary, Italy, Jugoslavia
 * Eastern European dst is unknown, we'll make it ME until someone speaks up.
 *	EE:	Bulgaria, Finland, Greece, Rumania, Turkey, Western Russia
 */

static struct dstab wedaytab[] = {
	1983,	86,	303,	/* 1983: end March - end Oct */
	1984,	86,	303,	/* 1984: end March - end Oct */
	1985,	86,	303,	/* 1985: end March - end Oct */
	0,	400,	0,	/* others: no daylight saving at all ??? */
};

static struct dstab medaytab[] = {
	1983,	86,	272,	/* 1983: end March - end Sep */
	1984,	86,	272,	/* 1984: end March - end Sep */
	1985,	86,	272,	/* 1985: end March - end Sep */
	0,	400,	0,	/* others: no daylight saving at all ??? */
};

static struct dayrules {
	int		dst_type;	/* number obtained from system */
	int		dst_hrs;	/* hours to add when dst on */
	struct	dstab *	dst_rules;	/* one of the above */
	enum {STH,NTH}	dst_hemi;	/* southern, northern hemisphere */
} dayrules [] = {
	DST_USA,	1,	usdaytab,	NTH,
	DST_AUST,	1,	ausdaytab,	STH,
	DST_WET,	1,	wedaytab,	NTH,
	DST_MET,	1,	medaytab,	NTH,
	DST_EET,	1,	medaytab,	NTH,	/* XXX */
	-1,
};

static
dstsetkernel(tzp)
struct timezone *tzp;
{
	struct dayrules *drp;
	int tabsize;
	int timei;
	int y;
	int yleap;
	int i;
	int d, di;
	time_t t;
	int daylb, dayle;
	char *p;

	for(drp = dayrules; drp->dst_type >= 0; drp++)
		if(drp->dst_type == tzp->tz_dsttime)
			break;

	if(drp->dst_type < 0)
		return;

	/* this ends up computing tabsize - 1, but that's what we want */

	for(tabsize = 0; drp->dst_rules[tabsize].dayyr > 0; tabsize++)
		;

	/* 2038 is the year that signed 32 bit time_t's give out */

	for(y = 1970, d = 0, t = 0, timei = 0; y < 2038; y++) {
		daylb = drp->dst_rules[tabsize].daylb;
		dayle = drp->dst_rules[tabsize].dayle;

		for(i = 0; i < tabsize; i++)
			if(y == drp->dst_rules[i].dayyr) {
				daylb = drp->dst_rules[i].daylb;
				dayle = drp->dst_rules[i].dayle;
				break;
			}

		yleap = isleap(y);

		if(yleap) {
			if(daylb >= 58)
				daylb++;

			if(dayle >= 58)
				dayle++;
		}

		/*
		 *  January 1, 1970 was a Wednesday.
		 *  d is the difference between January 1 of the loop
		 *  year (y) and January 1, 1970, in days.
		 *  daylb and dayle are (0-origin) day offsets with
		 *  respect to January 1.
		 *  So (d + dayl[be] - 3) % 7 is the day (0 == Sunday)
		 *  of daylb or dayle.
		 *  That's also the number to subtract from daylb or
		 *  dayle to get the day number (since January 1 of
		 *  the loop year) of the preceding Sunday.
		 */

		daylb -= (d + daylb - 3) % 7;
		dayle -= (d + dayle - 3) % 7;

		s.ats[timei] = t + SECS_PER_DAY * daylb
				+ tzp->tz_minuteswest * SECS_PER_MIN
							+ 2 * SECS_PER_HOUR;

		s.ats[timei + 1] = t + SECS_PER_DAY * dayle
				+ tzp->tz_minuteswest * SECS_PER_MIN
					+ (drp->dst_hemi == NTH ? 1 : 2)
							* SECS_PER_HOUR;

		if(drp->dst_hemi == NTH) {
			s.types[timei] = tzp->tz_dsttime;
			s.types[timei + 1] = 0;
		} else {
			s.types[timei] = 0;
			s.types[timei + 1] = tzp->tz_dsttime;
		}

		timei += 2;

		di = year_lengths[yleap];

		d += di;
		t += di * SECS_PER_DAY;
	}

	s.timecnt = timei;

	s.ttis[1].tt_gmtoff = tzp->tz_minuteswest * -SECS_PER_MIN
						+ drp->dst_hrs * SECS_PER_HOUR;

	s.ttis[1].tt_isdst = tzp->tz_dsttime;

	for(p = s.chars; *p != '\0'; p++)
		;

	(void)strcpy(++p, _tztab(tzp->tz_minuteswest, tzp->tz_dsttime));

	s.ttis[1].tt_abbrind = p - s.chars;

	tzname[1] = p;

	s.typecnt = 2;
}
