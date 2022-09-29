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
#ident	"$Header: uptime.c,v 1.2.2.2 90/05/10 00:33:23 wje Exp $"

/*
 * Formattable uptime command.  The format consists of characters and
 * %-specifiers.  The available specifiers are:
 *
 *	%t	Current time (hours:minutes) on 12-hour clock
 *	%T	Current time (hours:minutes) on 24-hour clock
 *	%r	"am" or "pm"
 *	%R	"AM" or "PM"
 *	%u	Number of users with idle time less than an hour
 *	%U	Number of users on the system
 *
 *	%d	Days up
 *	%h	Hours up not including days
 *	%H	Hours up including days
 *	%m	Minutes up not including days and hours
 *	%M	Minutes up including days and hours
 *
 *	%D	Date sentence like BSD:
 *			hh:mm  (less than 1 day)
 *			d days, hh:mm
 *
 *	%p	Plurality -- 's' if previous % specifier was not 1,
 *			     empty otherwise
 *
 *	%a	Exponentially decaying load average values for 60, 300, and
 *		900 seconds, printed as: x.xx, x.xx, x.xx
 *	%A	Same, but with additional values for 1, 5, and 20 seconds
 *	%1a	Exponentially decaying load average over last 1 second
 *	%2a	Exponentially decaying load average over last 5 seconds
 *	%3a	Exponentially decaying load average over last 20 seconds
 *	%4a	Exponentially decaying load average over last 300 seconds
 *	%5a	Exponentially decaying load average over last 600 seconds
 *	%6a	Exponentially decaying load average over last 900 seconds
 *
 *	%s	Square decaying load average as: x.xx, x.xx, x.xx
 *	%1s	Square decaying load average over last 60 seconds
 *	%2s	Square decaying load average over last 300 seconds
 *	%3s	Square decaying load average over last 900 seconds
 *
 *	%%	%
 *
 * The specifiers %[456]s are printed as %s.
 * The sequences %[1-6][^as] are not printed.
 * All other sequences of % followed by a character are printed as-is.
 * \n, \t, \f, \b, \r, and \\ are printed as expected.  No other \ sequences
 * are handled.
 * A newline is printed after all characters in the format have been processed.
 */

#include <sys/param.h>
#include <sys/types.h>
#include <sys/fixpoint.h>
#include <sys/times.h>
#include <sys/fcntl.h>
#include <sys/stat.h>

#include <stdio.h>
#include <nlist.h>
#include <utmp.h>
#include <time.h>

struct	nlist nl[] = {
#define NL_LOADAVG	0
	{ "loadavg" },
#define NL_SQ_AVE	1
	{ "sq_avenrun" },
#define	NL_BOOTTIME	2
	{ "boottime" },
	0
};

double Loadavg[6];
double Sq_aven[3];

struct tm Cur_time;
time_t Up_time;

/*
 * Convert to 12-hour clock
 */

#define TWELVEHR(x)	(((x) > 12) ? ((x) - 12) : (x))

struct utmp *getutent();

int All_users;	/* all users */
int Act_users;	/* non-idle users */

#define	FMT_STD	" %t%r  up %D,  %U user%p,  load average: %a"
#define	FMT_LNG	" %t%r  up %D,  %U user%p,  load average: %A"
#define	FMT_SQ	" %t%r  up %D,  %U user%p,  load average: %s"

#define USAGE	"uptime: usage: uptime [-n] [-l] [-s] [-f format]"

main(argc, argv)
	int argc;
	char **argv;
{

	extern int optind;
	extern char *optarg;
	int c;
	char *format;

	format = (char *)getenv("UPTIME_FORMAT");
	if (format == NULL) {
		format = FMT_STD;
	}

	while ((c = getopt(argc, argv, "nlsf:")) != EOF) {
		switch (c) {

		case 'n':
			format = FMT_STD;
			break;

		case 'l':
			format = FMT_LNG;
			break;

		case 's':
			format = FMT_SQ;
			break;

		case 'f':
			format = optarg;
			break;

		default:
			fprintf(stderr, "%s\n", USAGE);
			exit(1);
			break;
		}
	}

	get_info();
	print_uptime(format);
}

/*
 * The subroutine get_info() gets the current time, the boot time,
 * all of the load average values, and the numbers of users.
 */

get_info()
{

	time_t now;		/* current time */
	struct tm *save;	/* current time tm structure */
	int kmemf;		/* /dev/kmem file descriptor */
	struct tms timbuf;	/* place holder */
	long btime;		/* temp for boot time */

	struct utmp *utmp;	/* utmp return value */
	char dpath[100];	/* path of tty device */
	struct stat statbuf;
	time_t last_idle;	/* one hour ago */

	fix t_loadavg[6];	/* temporaries for load averages */
	fix t_sq_aven[3];

	/*
	 * nlist the kernel to get the places for the symbols
	 */

	if (nlist("/unix", nl)) {
		fprintf(stderr, "uptime: Could not get data from /unix\n");
		exit(1);
	}
	kmemf = open("/dev/kmem", O_RDONLY);
	if (kmemf < 0) {
		perror("/dev/kmem");
		exit(1);
	}

	/*
	 * Get the current time and save it
	 */

	time(&now);
	save = localtime(&now);
	memcpy(&Cur_time, save, sizeof (Cur_time));

	/*
	 * Get the boot time.  If the offset is not 0, use that time. If it is,
	 * try getting the clicks from times().  As a last resort, say that we
	 * just came up.
	 */

	if (nl[NL_BOOTTIME].n_value) {
		(void)lseek(kmemf, nl[NL_BOOTTIME].n_value & 0x7fffffff, 0);
		(void)read(kmemf, (char *)&Up_time, sizeof (Up_time));
		Up_time = (now - Up_time);
	} else {
		btime = times(&timbuf);
		if (btime > 0) {
			Up_time = (btime / HZ);
		} else {
			Up_time = 0;
		}
	}

	/*
	 * Get the load averages.
	 */

	if (nl[NL_LOADAVG].n_value) {
		(void)lseek(kmemf, nl[NL_LOADAVG].n_value & 0x7fffffff, 0);
		(void)read(kmemf, (char *)t_loadavg, sizeof (t_loadavg));
		Loadavg[0] = FIX_TO_DBL(t_loadavg[0]);
		Loadavg[1] = FIX_TO_DBL(t_loadavg[1]);
		Loadavg[2] = FIX_TO_DBL(t_loadavg[2]);
		Loadavg[3] = FIX_TO_DBL(t_loadavg[3]);
		Loadavg[4] = FIX_TO_DBL(t_loadavg[4]);
		Loadavg[5] = FIX_TO_DBL(t_loadavg[5]);
	}

	if (nl[NL_SQ_AVE].n_value) {
		(void)lseek(kmemf, nl[NL_SQ_AVE].n_value & 0x7fffffff, 0);
		(void)read(kmemf, (char *)t_sq_aven, sizeof (t_sq_aven));
		Sq_aven[0] = FIX_TO_DBL(t_sq_aven[0]);
		Sq_aven[1] = FIX_TO_DBL(t_sq_aven[1]);
		Sq_aven[2] = FIX_TO_DBL(t_sq_aven[2]);
	}

	close(kmemf);

	/*
	 * Get the number of users.
	 */

	All_users = 0;
	Act_users = 0;

	last_idle = time(0) - (60 * 60);	/* one hour ago */
	setutent();
	while ((utmp = getutent()) != NULL) {
		if (utmp->ut_name[0] == '\0' ||
		    utmp->ut_type != USER_PROCESS) {
			continue;
		}
		All_users++;
		strcpy(dpath, "/dev/");
		strcat(dpath, utmp->ut_line);
		if (stat(dpath, &statbuf) >= 0) {
			if (statbuf.st_mtime > last_idle) {
				Act_users++;
			}
		}
	}
	endutent();
}

#define S_DAY	(24 * 60 * 60)
#define S_HOUR	(60 * 60)
#define S_MIN	(60)

print_uptime(fmt)
	char *fmt;
{
	int which;		/* array reference */
	int plural = 0;		/* was last item plural? */
	int tot_days_up;	/* %d */
	int left_hours_up;	/* %h */
	int tot_hours_up;	/* %H */
	int left_mins_up;	/* %m */
	int tot_mins_up;	/* %M */

	tot_days_up = Up_time / S_DAY;
	tot_hours_up = Up_time / S_HOUR;
	tot_mins_up = Up_time / S_MIN;

	Up_time = Up_time - (tot_days_up * S_DAY);
	left_hours_up = Up_time / S_HOUR;

	Up_time = Up_time - (left_hours_up * S_HOUR);
	left_mins_up = Up_time / S_MIN;

	while (*fmt) {
		switch (*fmt) {

		case '\\':
			fmt++;
			switch (*fmt) {

			case 'n':
				putchar('\n');
				break;

			case 'f':
				putchar('\f');
				break;

			case 'r':
				putchar('\r');
				break;

			case 'b':
				putchar('\b');
				break;

			case 't':
				putchar('\t');
				break;

			default:
				putchar(*fmt);
				break;

			}

			fmt++;
			continue;

		case '%':
			fmt++;
			break;

		default:
			putchar(*fmt);
			fmt++;
			continue;
		}

		which = 0;
		switch (*fmt) {

		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
			which = *fmt - '0';
			fmt++;
			break;
		}

		switch (*fmt) {

		case '%':
			putchar(*fmt);
			break;

		case 't':
			printf("%d:%.2d", TWELVEHR(Cur_time.tm_hour),
				Cur_time.tm_min);
			plural = 0;
			break;

		case 'T':
			printf("%d:%.2d", Cur_time.tm_hour, Cur_time.tm_min);
			plural = 0;
			break;

		case 'r':
			if (Cur_time.tm_hour >= 12) {
				printf("pm");
			} else {
				printf("am");
			}
			plural = 0;
			break;

		case 'R':
			if (Cur_time.tm_hour >= 12) {
				printf("PM");
			} else {
				printf("AM");
			}
			plural = 0;
			break;

		case 'u':
			printf("%d", Act_users);
			if (Act_users == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;

		case 'U':
			printf("%d", All_users);
			if (All_users == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;

		case 'D':
			if (tot_days_up > 0) {
				printf("%d day%s, %d:%.2d", tot_days_up,
					(tot_days_up == 1) ? "" : "s",
					left_hours_up, left_mins_up);
			} else {
				printf("%d:%.2d", left_hours_up, left_mins_up);
			}
			plural = 0;
			break;
		
		case 'd':
			printf("%d", tot_days_up);
			if (tot_days_up == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;
		
		case 'h':
			printf("%d", left_hours_up);
			if (left_hours_up == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;
		
		case 'H':
			printf("%d", tot_hours_up);
			if (tot_hours_up == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;
		
		case 'm':
			printf("%.2d", left_mins_up);
			if (left_mins_up == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;
		
		case 'M':
			printf("%.2d", tot_mins_up);
			if (tot_mins_up == 1) {
				plural = 0;
			} else {
				plural = 1;
			}
			break;

		case 'p':
			if (plural == 1) {
				putchar('s');
			}
			break;

		case 'a':
			plural = 0;
			if (which == 0) {
				printf("%4.2f, %4.2f, %4.2f",
					Loadavg[3], Loadavg[4], Loadavg[5]);
			} else {
				printf("%4.2f", Loadavg[which - 1]);
			}
			break;

		case 'A':
			plural = 0;
			printf("%4.2f, %4.2f, %4.2f, %4.2f, %4.2f, %4.2f",
				Loadavg[0], Loadavg[1], Loadavg[2],
				Loadavg[3], Loadavg[4], Loadavg[5]);
			break;

		case 's':
			plural = 0;
			if (which == 0 || which > 3) {
				printf("%4.2f, %4.2f, %4.2f",
					Sq_aven[0], Sq_aven[1], Sq_aven[2]);
			} else {
				printf("%4.2f", Sq_aven[which - 1]);
			}
			break;

		default:
			putchar('%');
			putchar(*fmt);
			break;
		}
		fmt++;
	}
	putchar('\n');
}
