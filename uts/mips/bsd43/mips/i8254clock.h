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
/* $Header: i8254clock.h,v 1.7.1.2 90/05/10 04:41:51 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * General time definitions
 */
#define	BSD43_SECMIN	((unsigned)60)			/* seconds per minute */
#define	BSD43_SECHOUR	((unsigned)(60*BSD43_SECMIN))		/* seconds per hour */
#define	BSD43_SECDAY	((unsigned)(24*BSD43_SECHOUR))	/* seconds per day */
#define	BSD43_SECYR	((unsigned)(365*BSD43_SECDAY))	/* sec per reg year */

#define	BSD43_YRREF		1970
#define	BSD43_LEAPYEAR(x)	(((x) % 4) == 0)
#define	BSD43_TODRZERO	(1<<26)
#define	BSD43_BASEYEAR	72			/* MUST be a leap year */

#define	BSD43_RT_CLOCK_ADDR	(struct bsd43_(rt_clock) *)BSD43_PHYS_TO_K1(0x1e010003)

#define	BSD43_FILL3(x)	char fill_/**/x[3]
/*
 * Definitions for use HD146818 real time clock
 */
struct bsd43_(rt_clock) {
	u_char	rt_secs;		/* current seconds */
	BSD43_FILL3(0);
	u_char	rt_seca;		/* alarm seconds */
	BSD43_FILL3(1);
	u_char	rt_mins;		/* current minutes */
	BSD43_FILL3(2);
	u_char	rt_mina;		/* alarm minutes */
	BSD43_FILL3(3);
	u_char	rt_hours;		/* current hours */
	BSD43_FILL3(4);
	u_char	rt_houra;		/* alarm hours */
	BSD43_FILL3(5);
	u_char	rt_dayw;		/* day of the week */
	BSD43_FILL3(6);
	u_char	rt_daym;		/* day of the month */
	BSD43_FILL3(7);
	u_char	rt_month;		/* month */
	BSD43_FILL3(8);
	u_char	rt_year;		/* year */
	BSD43_FILL3(9);
	u_char	rt_rega;		/* register a */
	BSD43_FILL3(10);
	u_char	rt_regb;		/* register b */
	BSD43_FILL3(11);
	u_char	rt_regc;		/* register c */
	BSD43_FILL3(12);
	u_char	rt_regd;		/* register d */
	BSD43_FILL3(13);
	u_char	rt_mem[50*4];		/* general purpose battery-bkup ram */
};

#define	BSD43_RT_MEMX(x)	((x)<<2)

/*
 * Register A bit definitions
 */
#define	BSD43_RTA_UIP		0x80		/* update in progress */
#define	BSD43_RTA_DV4M	(0<<4)		/* time base is 4.194304 MHz */
#define	BSD43_RTA_DV1M	(1<<4)		/* time base is 1.048576 MHz */
#define	BSD43_RTA_DV32K	(2<<4)		/* time base is 32.768 kHz */
#define	BSD43_RTA_DVRESET	(7<<4)		/* reset divider chain */
#define	BSD43_RTA_RSNONE	0		/* disable periodic intr and SQW */

/*
 * Register B bit definitions
 */
#define	BSD43_RTB_SET		0x80		/* inhibit date update */
#define	BSD43_RTB_PIE		0x40		/* enable periodic interrupt */
#define	BSD43_RTB_AIE		0x20		/* enable alarm interrupt */
#define	BSD43_RTB_UIE		0x10		/* enable update-ended interrupt */
#define	BSD43_RTB_SQWE	0x08		/* square wave enable */
#define	BSD43_RTB_DMBINARY	0x04		/* binary data (0 => bcd data) */
#define	BSD43_RTB_24HR	0x02		/* 24 hour mode (0 => 12 hour) */
#define	BSD43_RTB_DSE		0x01		/* daylight savings mode enable */

/*
 * Register C bit definitions
 */
#define	BSD43_RTC_IRQF	0x80		/* interrupt request flag */
#define	BSD43_RTC_PF		0x40		/* periodic interrupt flag */
#define	BSD43_RTC_AF		0x20		/* alarm interrupt flag */
#define	BSD43_RTC_UF		0x10		/* update-ended interrupt flag */

/*
 * Register D bit definitions
 */
#define	BSD43_RTD_VRT		0x80		/* valid RAM and time flag */

/*
 * Definitions for 8254 programmable interval timer
 *
 * NOTE: counter2 is clocked at MASTER_FREQ (3.6864 MHz), the
 * output of counter2 is the clock for both counter0 and counter1.
 * Counter0 output is tied to Interrupt 2 to act as the scheduling
 * clock and the output of counter1 is tied to Interrupt 4 to act as
 * the profiling clock.
 */

#define	BSD43_PT_CLOCK_ADDR	(struct bsd43_(pt_clock) *)BSD43_PHYS_TO_K1(0x1e001003)
#define	BSD43_TIM0_ACK_ADDR	(char *)BSD43_PHYS_TO_K1(0x1e200003)
#define	BSD43_TIM1_ACK_ADDR	(char *)BSD43_PHYS_TO_K1(0x1e200007)

struct bsd43_(pt_clock) {
	u_char	pt_counter0;		/* counter 0 port */
	BSD43_FILL3(0);
	u_char	pt_counter1;		/* counter 1 port */
	BSD43_FILL3(1);
	u_char	pt_counter2;		/* counter 2 port */
	BSD43_FILL3(2);
	u_char	pt_control;		/* control word */
};

/*
 * control word definitions
 */
#define	BSD43_PTCW_SC(x)	((x)<<6)	/* select counter x */
#define	BSD43_PTCW_RBCMD	(3<<6)		/* read-back command */
#define	BSD43_PTCW_CLCMD	(0<<4)		/* counter latch command */
#define	BSD43_PTCW_LSB	(1<<4)		/* r/w least signif. byte only */
#define	BSD43_PTCW_MSB	(2<<4)		/* r/w most signif. byte only */
#define	BSD43_PTCW_16B	(3<<4)		/* r/w 16 bits, lsb then msb */
#define	BSD43_PTCW_MODE(x)	((x)<<1)	/* set mode to x */
#define	BSD43_PTCW_BCD	0x1		/* operate in BCD mode */

/*
 * Mode definitions
 */
#define	BSD43_MODE_ITC	0		/* interrupt on terminal count */
#define	BSD43_MODE_HROS	1		/* hw retriggerable one-shot */
#define	BSD43_MODE_RG		2		/* rate generator */
#define	BSD43_MODE_SQW	3		/* square wave generator */
#define	BSD43_MODE_STS	4		/* software triggered strobe */
#define	BSD43_MODE_HTS	5		/* hardware triggered strobe */

#define	BSD43_CRYSTAL_HZ	3686400		/* input clock to master divider */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BASEYEAR BSD43_BASEYEAR
#   define CRYSTAL_HZ BSD43_CRYSTAL_HZ
#   define FILL3 BSD43_FILL3
#   define LEAPYEAR BSD43_LEAPYEAR
#   define MODE_HROS BSD43_MODE_HROS
#   define MODE_HTS BSD43_MODE_HTS
#   define MODE_ITC BSD43_MODE_ITC
#   define MODE_RG BSD43_MODE_RG
#   define MODE_SQW BSD43_MODE_SQW
#   define MODE_STS BSD43_MODE_STS
#   define PTCW_16B BSD43_PTCW_16B
#   define PTCW_BCD BSD43_PTCW_BCD
#   define PTCW_CLCMD BSD43_PTCW_CLCMD
#   define PTCW_LSB BSD43_PTCW_LSB
#   define PTCW_MODE BSD43_PTCW_MODE
#   define PTCW_MSB BSD43_PTCW_MSB
#   define PTCW_RBCMD BSD43_PTCW_RBCMD
#   define PTCW_SC BSD43_PTCW_SC
#   define PT_CLOCK_ADDR BSD43_PT_CLOCK_ADDR
#   define RTA_DV1M BSD43_RTA_DV1M
#   define RTA_DV32K BSD43_RTA_DV32K
#   define RTA_DV4M BSD43_RTA_DV4M
#   define RTA_DVRESET BSD43_RTA_DVRESET
#   define RTA_RSNONE BSD43_RTA_RSNONE
#   define RTA_UIP BSD43_RTA_UIP
#   define RTB_24HR BSD43_RTB_24HR
#   define RTB_AIE BSD43_RTB_AIE
#   define RTB_DMBINARY BSD43_RTB_DMBINARY
#   define RTB_DSE BSD43_RTB_DSE
#   define RTB_PIE BSD43_RTB_PIE
#   define RTB_SET BSD43_RTB_SET
#   define RTB_SQWE BSD43_RTB_SQWE
#   define RTB_UIE BSD43_RTB_UIE
#   define RTC_AF BSD43_RTC_AF
#   define RTC_IRQF BSD43_RTC_IRQF
#   define RTC_PF BSD43_RTC_PF
#   define RTC_UF BSD43_RTC_UF
#   define RTD_VRT BSD43_RTD_VRT
#   define RT_CLOCK_ADDR BSD43_RT_CLOCK_ADDR
#   define RT_MEMX BSD43_RT_MEMX
#   define SECDAY BSD43_SECDAY
#   define SECHOUR BSD43_SECHOUR
#   define SECMIN BSD43_SECMIN
#   define SECYR BSD43_SECYR
#   define TIM0_ACK_ADDR BSD43_TIM0_ACK_ADDR
#   define TIM1_ACK_ADDR BSD43_TIM1_ACK_ADDR
#   define TODRZERO BSD43_TODRZERO
#   define YRREF BSD43_YRREF
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


