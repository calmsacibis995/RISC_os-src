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
/* $Header: hd146818.h,v 1.1.4.2 90/05/10 10:52:56 wje Exp $ */

/*
 * General time definitions
 */
#define	SECMIN	((unsigned)60)			/* seconds per minute */
#define	SECHOUR	((unsigned)(60*SECMIN))		/* seconds per hour */
#define	SECDAY	((unsigned)(24*SECHOUR))	/* seconds per day */
#define	SECYR	((unsigned)(365*SECDAY))	/* sec per reg year */

#define	YRREF		1970
#define	LEAPYEAR(x)	(((x) % 4) == 0)
#define	TODRZERO	(1<<26)
#define BASEYEAR	72			/* Must be a leap year */

#ifndef FILL3
#define	FILL3(x)	char fill_/**/x[3]
#endif

/*
 * Definitions for use HD146818 real time clock
 */
#ifdef LANGUAGE_C
struct rt_clock {
	u_char	rt_secs;		/* current seconds */
	FILL3(0);
	u_char	rt_seca;		/* alarm seconds */
	FILL3(1);
	u_char	rt_mins;		/* current minutes */
	FILL3(2);
	u_char	rt_mina;		/* alarm minutes */
	FILL3(3);
	u_char	rt_hours;		/* current hours */
	FILL3(4);
	u_char	rt_houra;		/* alarm hours */
	FILL3(5);
	u_char	rt_dayw;		/* day of the week */
	FILL3(6);
	u_char	rt_daym;		/* day of the month */
	FILL3(7);
	u_char	rt_month;		/* month */
	FILL3(8);
	u_char	rt_year;		/* year */
	FILL3(9);
	u_char	rt_rega;		/* register a */
	FILL3(10);
	u_char	rt_regb;		/* register b */
	FILL3(11);
	u_char	rt_regc;		/* register c */
	FILL3(12);
	u_char	rt_regd;		/* register d */
	FILL3(13);
	u_char	rt_mem[50*4];		/* general purpose battery-bkup ram */
};
#endif

#define	RT_MEMX(x)	((x)<<2)
#define RT_MEM_OFFSET	0x38

/*
 * Register A bit definitions
 */
#define	RTA_UIP		0x80		/* update in progress */
#define	RTA_DV4M	(0<<4)		/* time base is 4.194304 MHz */
#define	RTA_DV1M	(1<<4)		/* time base is 1.048576 MHz */
#define	RTA_DV32K	(2<<4)		/* time base is 32.768 kHz */
#define	RTA_DVRESET	(7<<4)		/* reset divider chain */
#define	RTA_RSNONE	0		/* disable periodic intr and SQW */

/*
 * Register B bit definitions
 */
#define	RTB_SET		0x80		/* inhibit date update */
#define	RTB_PIE		0x40		/* enable periodic interrupt */
#define	RTB_AIE		0x20		/* enable alarm interrupt */
#define	RTB_UIE		0x10		/* enable update-ended interrupt */
#define	RTB_SQWE	0x08		/* square wave enable */
#define	RTB_DMBINARY	0x04		/* binary data (0 => bcd data) */
#define	RTB_24HR	0x02		/* 24 hour mode (0 => 12 hour) */
#define	RTB_DSE		0x01		/* daylight savings mode enable */

/*
 * Register C bit definitions
 */
#define	RTC_IRQF	0x80		/* interrupt request flag */
#define	RTC_PF		0x40		/* periodic interrupt flag */
#define	RTC_AF		0x20		/* alarm interrupt flag */
#define	RTC_UF		0x10		/* update-ended interrupt flag */

/*
 * Register D bit definitions
 */
#define	RTD_VRT		0x80		/* valid RAM and time flag */
