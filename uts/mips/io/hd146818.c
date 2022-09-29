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
#ident	"$Header: hd146818.c,v 1.9.3.2 90/05/10 10:50:52 wje Exp $"


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/hd146818.h"
#include "sys/clock.h"
#include "sys/nvram.h"
#include "sys/cmn_err.h"

extern int RT_CLOCK_ADDR[];		/* Lboot'able addresses */
extern int prom_has_version;

/*
 * This is an attempt to use the brain-damaged 146818 real-time clock chip.
 * We always set the chip for a leap year (1972) and use the chip only to
 * calculate seconds from the first of the year.
 */
int month_days[12] = {
	31,	/* January */
	29,	/* February */
	31,	/* March */
	30,	/* April */
	31,	/* May */
	30,	/* June */
	31,	/* July */
	31,	/* August */
	30,	/* September */
	31,	/* October */
	30,	/* November */
	31	/* December */
};

rtodc()
{
	register volatile struct rt_clock *rt = 
		(struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register unsigned int month, day, year, hours, mins, secs;
	int i;
	int s;
	static int first_time = 1;

	/* 
	 * The first time rtodc() is called, check if either the chip or 
	 * the prom has noticed that battery backup has been lost.
	 *
	 * If we are using 4.0 proms or later, we use prom_nv_get to
	 * get information out of the prom, else we use last known
	 * address for information in nvram.  (Early proms have no "version"
	 * environment variable.)
	 */
	if (first_time) {
		first_time = 0;
		if (prom_has_version) {
			if ((rt->rt_regd & RTD_VRT) == 0
				|| ((int)(*(char *)prom_nv_get(NVINDEX_STATE))
					 & NVSTATE_TODVALID) == 0)
				return(0);
		} else {
			if ((rt->rt_regd & RTD_VRT) == 0
				|| ((int)rt->rt_mem[RT_MEMX(NVADDR_STATE)]
					 & NVSTATE_TODVALID) == 0)
				return(0);
		}
	}

	/*
	 * If UIP (update in progress) is set, the 146818 should not be read.
	 * Furthermore, the 146818 can not be read more than 1/4 of the
	 * available bus cycles and not more than 50 contiguous references
	 * can be to the chip.  Therefore the DELAY(10).
	 *
	 * If UIP is not set, then we have 244us to read the state of the
	 * 146818, hence the spls.
	 */
	s = splall();
	while (rt->rt_rega & RTA_UIP) {
		splx(s);
		DELAY(10);
		s = splall();
	}
	secs = rt->rt_secs;
	mins = rt->rt_mins;
	hours = rt->rt_hours;
	day = rt->rt_daym;
	month = rt->rt_month;
	year = rt->rt_year;
	splx(s);

	if (year != BASEYEAR && year != BASEYEAR+1)
		return(0);

	/*
	 * Sum up seconds from beginning of year
	 */
	secs += mins * SECMIN;
	secs += hours * SECHOUR;
	secs += (day-1) * SECDAY;
	for (i = 0; i < month-1; i++)
		 secs += month_days[i] * SECDAY;
	year -= BASEYEAR;
	secs += year * (SECYR + SECDAY);
	secs += TODRZERO;
	return(secs);
}

wtodc(newtime)
long newtime;
{
	register long year_secs = newtime;
	register volatile struct rt_clock *rt = 
		(struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register month, day, hours, mins, secs, year;
	int dummy;
	int s;
	char *state;

	/*
	 * Whittle the time down to an offset in the current year,
	 * by subtracting off whole years as long as possible.
	 */
	year = YRREF;
	for (;;) {
		register secyr = SECYR;
		if (LEAPYEAR(year))
			secyr += SECDAY;
		if (year_secs < secyr)
			break;
		year_secs -= secyr;
		year++;
	}
	/*
	 * Break seconds in year into month, day, hours, minutes, seconds
	 */
	for (month = 0;
	   year_secs >= month_days[month]*SECDAY;
	   year_secs -= month_days[month++]*SECDAY)
		continue;
	month++;

	for (day = 1; year_secs >= SECDAY; day++, year_secs -= SECDAY)
		continue;

	for (hours = 0;
	    year_secs >= SECHOUR;
	    hours++, year_secs -= SECHOUR)
		continue;

	for (mins = 0;
	    year_secs >= SECMIN;
	    mins++, year_secs -= SECMIN)
		continue;

	secs = year_secs;	/* the final value */
	
	/*
	 * 146818 is brain damaged and may not be initialized to
	 * certain dates, check for them and avoid them by backing
	 * up a couple of seconds.
	 */
	if (day >= 28 && day <= 30 && hours == 23 && mins == 59
	    && secs >= 58 && secs <= 59)
		secs = 57;
	
	s = splall();
	dummy = rt->rt_regd;	/* set VRT */
	rt->rt_regb = RTB_SET|RTB_DMBINARY|RTB_24HR;
	rt->rt_rega = RTA_DVRESET;
	rt->rt_secs = secs;
	rt->rt_mins = mins;
	rt->rt_hours = hours;
	rt->rt_daym = day;
	rt->rt_month = month;
	rt->rt_year = BASEYEAR;
	rt->rt_rega = RTA_DV32K;
	rt->rt_regb = RTB_DMBINARY|RTB_24HR;

	if (prom_has_version) { /* if prom version >= 4.0  */
				/* mark the TOD as valid   */
		state = (char *)prom_nv_get(NVINDEX_STATE); 
		*state |= NVSTATE_TODVALID;
		(void) prom_nv_set(NVINDEX_STATE, state); 
	}
	splx(s);
	wbflush();
}

