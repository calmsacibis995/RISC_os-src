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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: mk48t02.c,v 1.7.1.1.1.2 90/08/20 17:56:04 hawkes Exp $"


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/mk48t02.h"
#include "sys/clock.h"
#include "sys/nvram.h"
#include "sys/cmn_err.h"

extern int TODC_CLOCK_ADDR[];		/* Lboot'able addresses */

extern prom_getenv(), prom_nv_get();
extern int prom_has_version;

int todc_valid = 0;

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
	register volatile struct todc_clock *todc = 
		(volatile struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	register unsigned int month, day, year, hours, mins, secs;
	int i;

	/* 
	 * Check if the prom has noticed that battery backup has been lost.
	 *
	 * If we are using 4.0 proms or later, we use prom_nv_get to
	 * get information out of the prom, else we use last known
	 * address for information in nvram.  (Early proms have no "version"
	 * environment variable.)
	 * For efficiency, once the todc is valid we retain this information
	 * rather than invoking prom code to determine validity on each call.
	 */
	if (!todc_valid)
	  if (prom_has_version) {
	    if ((todc_valid = ((int)(*(char *)prom_nv_get(NVINDEX_STATE))
		 & NVSTATE_TODVALID)) == 0)
	      return(0);
	  } else {
	    if (((int)todc->todc_mem[NVADDR_STATE].value
		 & NVSTATE_TODVALID) == 0)
	      return(0);
	  }
	
	todc->todc_control |= TODC_CONT_READ;	/* Halt chip */
	wbflush();
	secs = todc->todc_secs & TODC_SECS_MASK;/* Did these lines separate */
	secs = bcd_to_dec(secs);		/* to prevent poking the */
	mins = todc->todc_mins & TODC_MINS_MASK;/* hell out of the chip and */
	mins = bcd_to_dec(mins);		/* having the data change */
	hours = todc->todc_hours & TODC_HOUR_MASK;	/* in the interim */
	hours = bcd_to_dec(hours);
	day = todc->todc_daym & TODC_DAYM_MASK;
	day = bcd_to_dec(day);
	month = todc->todc_month & TODC_MONTH_MASK;
	month = bcd_to_dec(month);
	year = todc->todc_year;
	year = bcd_to_dec(year);
	todc->todc_control &= ~TODC_CONT_READ;	/* Start it back up */
	wbflush();


	/*
	 * Sum up seconds from beginning of year
	 */
	secs += mins * SECMIN;
	secs += hours * SECHOUR;
	secs += (day-1) * SECDAY;
	for (i = 0; i < ((int)month)-1; i++)
		 secs += month_days[i] * SECDAY;
	secs += TODRZERO;
	return(secs);
}

wtodc(newtime)
long	newtime;
{
	register long year_secs = newtime;
	register volatile struct todc_clock *todc = 
		(volatile struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
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
	year = year % 100;				/* Down to 2 digit */
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
	
	/*
	 * Write information to mk48t02 chip
	 */
	s = splall();
	todc->todc_control |= TODC_CONT_WRITE;	/* Halt to set */
	wbflush();
	todc->todc_secs = dec_to_bcd(year_secs); wbflush();
	todc->todc_mins = dec_to_bcd(mins); wbflush();
	todc->todc_hours = dec_to_bcd(hours); wbflush();
	todc->todc_daym = dec_to_bcd(day); wbflush();
	todc->todc_month = dec_to_bcd(month); wbflush();
	todc->todc_year = dec_to_bcd(year); wbflush();
	todc->todc_control &= ~TODC_CONT_WRITE;	/* Fire it up */

	if (prom_has_version) { /* if prom version >= 4.0  */
				/* mark the TOD as valid   */
		state = (char *)prom_nv_get(NVINDEX_STATE); 
		*state |= NVSTATE_TODVALID;
		(void) prom_nv_set(NVINDEX_STATE, state); 
	}
	splx(s);

	wbflush();
}

