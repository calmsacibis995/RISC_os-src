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
#ident	"$Header: strftime.c,v 1.3.1.7 90/05/10 20:27:50 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
/*
 * This routine converts time as follows.  The epoch is 0000  Jan  1
 * 1970  GMT.   The argument brokentime is a pointer to an array containing:
 *
 *		  seconds (0-59)
 *		  minutes (0-59)
 *		  hours (0-23)
 *		  day of month (1-31)
 *		  month (0-11)
 *		  year
 *		  weekday (0-6, Sun is 0)
 *		  day of the year
 *		  daylight savings flag
 *
 * The routine corrects for daylight saving time and  will  work  in
 * any  time  zone provided "timezone" is adjusted to the difference
 * between Greenwich and local standard time (measured in seconds).
 *
 *	 strftime(buf, bufsiz, format, t)	
 */

#include	<stdlib.h>
#include	<ctype.h>
#include	<time.h>
#include	<stdio.h>
#include 	<fcntl.h>
#include	<sys/types.h>
#include 	<sys/stat.h>
#include	<locale.h>
#include	"_locale.h"

extern char	*getenv(), *malloc(), *memchr(), *memcpy(), *strncpy(); 
static char	*itoa();
static void	_setcftime();
static char     saved_lc_time[LC_NAMELEN] = DFL_LANG;

enum STR {
	aJan, aFeb, aMar, aApr, aMay, aJun, aJul, aAug, aSep, aOct, aNov, aDec,
	Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec,
	aSun, aMon, aTue, aWed, aThu, aFri, aSat,
	Sun, Mon, Tue, Wed, Thu, Fri, Sat,
	Local_time, Local_date, DFL_FMT,
	AM, PM,
	LAST
} ;

static char * _cftime[] = {
	"Jan","Feb","Mar","Apr","May","Jun","Jul", "Aug", "Sep","Oct", "Nov", "Dec",
	"January", "February", "March","April",
	"May","June", "July", "August", "September",
	"October", "November", "December",
	"Sun","Mon", "Tue", "Wed","Thu", "Fri","Sat",
	"Sunday","Monday","Tuesday","Wednesday", "Thursday","Friday","Saturday",
	"%H:%M:%S","%m/%d/%y", "%a %b %e %H:%M:%S %Z %Y",
	"AM", "PM"
};

size_t 
strftime(string, maximum, format, brokentime)
char *string; 
size_t maximum; 
char *format; 
struct tm *brokentime;
{
	register char	*cp, *p,  c;
	register int	r;
	int		i, count=0, temp;
	char		*ptr;
	

	tzset();
	_setcftime();

	/* Set format string, if not already set */
	if (format == NULL)
		format = _cftime[(int)DFL_FMT];

	/* Build date string by parsing format string */
	cp = string;
	while ((c = *format++) != '\0' && count < maximum) {
		if (c != '%') {
			*cp++ = c;
			count++;
			continue;
		}
		switch (*format++) {
		case '%':	/* Percent sign */
			*cp++ = '%';
			count++;
			break;
		case 'a':	/* Abbreviated weekday name */
			for(p = _cftime[(int)aSun + brokentime->tm_wday]; 
			    *p != '\0' && count < maximum; p++) {
				*cp++ = *p;
				count++;
			}
			break;
		case 'A':	/* Weekday name */
			for(p =  _cftime[(int)Sun + brokentime->tm_wday]; 
			    *p != '\0' && count < maximum; p++) {
				*cp++ = *p;
				count++;
			}
			break;
		case 'b':	/* Abbreviated month name */
			for(p = _cftime[(int)aJan + brokentime->tm_mon];
			    *p != '\0' && count < maximum; p++) {
				*cp++ = *p;
				count++;
			}
			break;
		case 'B':	/* Month name */
			for(p = _cftime[(int)Jan + brokentime->tm_mon];
			    *p != '\0' && count < maximum; p++) {
				*cp++ = *p;
				count++;
			}
			break;
		case 'c':	/* Locales default for date and time */
		{
			char	buf[256];
			char	*bufp = &buf[0];

			temp = strftime(bufp, 256, _cftime[(int)DFL_FMT],
					brokentime);
			if ((count + temp) >= maximum)
				temp = maximum - count;
			cp = strncpy(cp, bufp, temp);
			cp += temp;
			count += temp;
			break;
		}
		case 'd':	/* Day number */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_mday, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'e':       /* Day number without leading zero */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_mday, bufp, 2);
			if (brokentime->tm_mday < 10) {
				*cp++ = ' ';
			} else {
				*cp++ = buf[0];
			}
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'H':	/* Hour (24 hour version) */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_hour, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'I':	/* Hour (12 hour version) */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_hour > 12 ?
				      brokentime->tm_hour - 12 : 
					brokentime->tm_hour, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'j':	/* Julian date */
		{
			char	buf[3];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_yday + 1, bufp, 3);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
			}
			if (count++ < maximum) {
				*cp++ = buf[2];
			}
			break;
		}
		case 'm':	/* Month number */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_mon + 1, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'M':	/* Minute */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_min, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'p':	/* AM or PM */
			if (brokentime->tm_hour >= 12) 
				for (p = _cftime[(int)PM]; 
				     *p != '\0' && count < maximum; p++) {
					*cp++ = *p;
					count++;
				}
			else
				for (p = _cftime[(int)AM]; 
				     *p != '\0' && count < maximum; p++) {
					*cp++ = *p;
					count++;
				}
			break;
		case 'S':	/* Seconds */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_sec, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'U':	/* Week of the year, regarding Sunday as */
				/* the first day of the week */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			temp = brokentime->tm_yday - brokentime->tm_wday;
			if (temp >= -3 ) {
				/* +1 for - brokentime->tm_wday */
				i = (temp + 1) / 7 + 1;	
				if (temp % 7 >= 4) i++;
			} else i = 52;

			bufp = itoa(i, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'w':	/* Weekday number, regarding Sunday as */
				/* the first day of the week */
			cp = itoa(brokentime->tm_wday, cp, 1);
			count++;
			break;
		case 'W':	/* Week of the year, regarding Monday as */
				/* the first day of the week */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			if (brokentime->tm_wday == 0)
                        	temp = brokentime->tm_yday - 6;
                        else
                                temp = brokentime->tm_yday - brokentime->tm_wday + 1;
                        if (temp >= -3 ) {
				/* +1 for - brokentime->tm_wday */
                                i = (temp + 1) / 7 + 1; 
				if (temp % 7 >= 4) i++;
                        } else  i = 52; /* less than 4 days in the first */
                                        /* week causes it to belong to 	 */
                                        /* the tail of prev year 	 */
                          
			bufp = itoa(i, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'x':	/* Localized date format */
		{
			char	buf[256];
			char	*bufp = &buf[0];

			temp = strftime(bufp, 256, _cftime[(int)Local_date],
					brokentime);
			if ((count + temp) >= maximum)
				temp = maximum - count;
			cp = strncpy(cp, bufp, temp);
			cp += temp;
			count += temp;
			break;
		}
		case 'X':	/* Localized time format */
		{
			char	buf[256];
			char	*bufp = &buf[0];

			temp = strftime(bufp, 256, _cftime[(int)Local_time],
					brokentime);
			if ((count + temp) >= maximum)
				temp = maximum - count;
			cp = strncpy(cp, bufp, temp);
			cp += temp;
			count += temp;
			break;
		}
		case 'y':	/* Year in the form yy */
		{
			char	buf[2];
			char	*bufp = &buf[0];

			bufp = itoa(brokentime->tm_year, bufp, 2);
			*cp++ = buf[0];
			if (count++ < maximum) {
				*cp++ = buf[1];
				count++;
			}
			break;
		}
		case 'Y':	/* Year in the form ccyy */
		{
			char	buf[4];
			char	*bufp = &buf[0];

			bufp = itoa(1900 + brokentime->tm_year, bufp, 4);
			temp = 4;
			if ((count + temp) >= maximum)
				temp = maximum - count;
			cp = strncpy(cp, buf, temp);
			cp += temp;
			count += temp;
			break;
		}
		case 'Z':	/* Timezone */
			for(p = tzname[brokentime->tm_isdst]; 
			    *p != '\0' && count < maximum; p++) {
				*cp++ = *p;
				count++;
			}
			break;
		default:
			*cp++ = c;
			if (++count < maximum) {
				*cp++ = *(format - 1);
				count++;
			}
			break;
		}
	}
	if (count < maximum) {
		*cp = '\0';
		return((size_t) count);
	} else
		return(0);
}

static void
_setcftime()
{
	register char *p;
	register int j;
	char *my_cftime[LAST], *locale,  *q;
	static char *ostr = (char *)0 ;
	char *str;
	struct stat buf;
	static char pathname[128] = LOCALE_DIR;
	int fd;

	locale = _catinfo[LC_TIME].locale;

	/* If we are setting time locale to what it is already, 
	 * then do nothing.
	 */
	if (strcmp(locale, saved_lc_time) == 0)
		return;

	(void) strcpy(&pathname[LEN_LC_ROOT],locale);
	(void) strcat(pathname,"/LC_TIME");

	if ( (fd = open(pathname,O_RDONLY)) == -1) 
		goto out1;

	if ( (fstat(fd,&buf)) != 0)
		goto out2;


	if ( (str = malloc(buf.st_size + 2)) == NULL )
		goto out2;

	if ( (read(fd, str, buf.st_size)) != buf.st_size)
		goto out3;

	/* Set last character of str to '\0' */
	q = &str[buf.st_size];
	q[0] = '\n';
	q[1] = '\0';

	/* p will "walk thru" str */
	p = str;  	

	j = -1;
	while (*p != '\0')
	{ 
		/* "Look for a newline, i.e. end of sub-string
		 * and  change it to a '\0'. If LAST pointers
		 * have been set in my_cftime, but the newline hasn't been seen
		 * yet, keep going thru the string leaving my_cftime alone.
		 */
		if (++j < (int)LAST) 
			my_cftime[j] = p;
		p = memchr(p,'\n',buf.st_size + 2);
		*p++ = '\0';
	}
	if (j == (int)LAST)
	{
		memcpy(_cftime, my_cftime, sizeof(my_cftime)); 
		strncpy(saved_lc_time, locale, LC_NAMELEN);
		if (ostr != 0)	 /* free the previoulsy allocated local array*/
			free(ostr);
		ostr = str;
		(void) close(fd);
		return;
	}

out3:
	free(str);
out2: 
	(void) close(fd);
out1:
	/* update _catinfo to reflect reality -- _cftime
	 * wasn't updated for one reason or another 
	 */
	(void) strcpy(_catinfo[LC_TIME].locale, saved_lc_time);
	return;
}

static char *
itoa(i, ptr, dig)
register int	i;
register char	*ptr;
register int	dig;
{
	switch(dig) {
	case 4:
		*ptr++ = i / 1000 + '0';
		i = i - i / 1000 * 1000;
	case 3:
		*ptr++ = i / 100 + '0';
		i = i - i / 100 * 100;
	case 2:
		*ptr++ = i / 10 + '0';
	case 1:
		*ptr++ = i % 10 + '0';
	}

	return(ptr);
}
