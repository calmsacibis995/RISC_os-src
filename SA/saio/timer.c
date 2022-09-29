#ident "$Header: timer.c,v 1.30.1.2 90/12/14 12:28:46 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
 * timer.c -- saio timer routines
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/signal.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/i8254clock.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "machine/bc.h"
#ifdef SABLE
#include "machine/sableclock.h"
#endif SABLE
#include "prom/prom.h"
#include "saio/saio.h"
#include "saio/setjmp.h"
#include "saio/debug.h"
#include "saio/ctype.h"

static unsigned alarm_time;
static alarm_on;

extern int RT_CLOCK_ADDR[];	/* See saio/machaddr.c for definition */
extern int TODC_CLOCK_ADDR[];
			
int *_timer_jmpbuf;

unsigned get_tod_rt();		/* Get time-of-day for HD146818 chip */
unsigned get_tod_todc();	/* Get time-of-day for MK4T02 chip */

unsigned (*get_tod[])() = {
	get_tod_rt,		/* M500 */
	get_tod_rt,		/* M800 */
	get_tod_rt,		/* M1000 */
	get_tod_todc,		/* Intrepid */
	get_tod_todc,		/* M2000 */
	get_tod_todc,		/* Excalibur */
	get_tod_todc,		/* M12 */
	get_tod_todc,		/* M12 Sable */
	get_tod_todc,		/* M180 */
	get_tod_todc,		/* M20 */
	get_tod_todc,		/* Genesis */
};				/* Indexed by machine_type */

int write_todclk();		/* Write time-of-day HD146818/MK48T02 chip */

extern int PT_CLOCK_ADDR[], TIM0_ACK_ADDR[], TIM1_ACK_ADDR[];
#define MASTER_PERIOD	3600
#define	HZ	100		/* 100 ticks/second of the clock */
#define	SCHED_PERIOD	MASTER_PERIOD/HZ
#define	PROF_PERIOD	36		/* 100 hertz for now */
 
_init_timer()
{
	alarm_on = 0;
	_timer_jmpbuf = 0;
}

/*
 * _set_timer -- arrange for alrmint() to be called in future
 * returns seconds left on previous timer
 * _set_timer(0) cancels current timer
 * (implemented via _scandevs())
 */
_set_timer(secs)
unsigned secs;
{
	int cur_time;
	int old_time;

	cur_time = (*MACHDEP(get_tod))();
	old_time = alarm_on ? _max(alarm_time - cur_time, 1) : 0;
#ifdef SABLE
	alarm_time = cur_time;		/* make it happen right now */
#else
	alarm_time = cur_time + secs;
#endif !SABLE
	alarm_on = secs;		/* _set_timer(0) turns off alarm */
	return(old_time);
}

/*
 * check_timer -- called from _scandevs() to implement alarm clock
 */
_check_timer()
{
	if (alarm_on && (*MACHDEP(get_tod))() >= alarm_time) {
		alarm_on = 0;
		alrmint();
	}
}

/*
 * alrmint() -- called when timer expires
 */
static
alrmint()
{
	int *jbp;

	if (_timer_jmpbuf) {
		jbp = _timer_jmpbuf;
		_timer_jmpbuf = NULL;
		longjmp(jbp, 1);
	}
	/*
	 * If nobody wants this anymore, just
	 * ignore it
	 */
}

/*
 * This is an attempt to use the brain-damaged 146818 real-time clock chip.
 * We always set the chip for a leap year (1972) and use the chip only to
 * calculate seconds from the first of the year.
 */
static int month_days[12] = {
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

/*
 * get_tod_rt -- get current time-of-day from the HD146818 chip
 */
unsigned
get_tod_rt()
{
	register volatile struct rt_clock *rt = 
	  (struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	u_int month, day, year, hours, mins, secs, tries;
	register int i;
	register char *cp;
	extern char *get_nvram();

	if ((rt->rt_regd & RTD_VRT) == 0) {
		_errputs("nv ram lost battery backup\n");
		set_nvram(NVADDR_STATE, NVLEN_STATE, "\0");
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
	tries = 0;
again:
	while (rt->rt_rega & RTA_UIP)
		__scandevs();
	secs = rt->rt_secs;
	mins = rt->rt_mins;
	hours = rt->rt_hours;
	day = rt->rt_daym;
	month = rt->rt_month;
	year = rt->rt_year;

	if ((year != BASEYEAR && year != BASEYEAR+1)
	    || month > 12 || day > 31 || hours > 23 || mins > 59 || secs > 59
	) {

		_errputs("tod clock invalid\n");
		printf("secs=%d mins=%d hours=%d day=%d month=%d year=%d\n",
		    secs, mins, hours, day, month, year);

		/*
		 * TOD isn't initialized, so drop a date into it
		 * so we have a base for relative
		 * time measurements.  Clear TODVALID so later OS kernels
		 * can tell that time is bogus.
		 */
		cp = get_nvram(NVADDR_STATE, NVLEN_STATE);
		*cp &= ~NVSTATE_TODVALID;
		set_nvram(NVADDR_STATE, NVLEN_STATE, cp);
		if (tries++) {
			extern int *nofault;

			/*
			 * can't call _io_abort here, because it calls
			 * get_tod eventually
			 */
			printf("\ncan't set tod clock\n");
#ifdef PROM
			return(0);
#else
			nofault = 0;
			exit(-1);
#endif
		}
		printf("\ninitializing tod clock\n");
		write_todclk(0);
		goto again;
	}

	secs = sum_seconds(secs, mins, hours, day, month, year);
	return(secs);
}

/*
 * Write time-of-day for the HD146818/MK48T02 chip.
 */

write_todclk(year_secs)
register long year_secs;
{
	register volatile struct rt_clock *rt = 
	  (struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register volatile struct todc_clock *todc = 
	  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	register month, day, hours, mins, secs;
	register char *cp;
#ifdef COMPILER_FIXED
	register int junk;
#else
	static int junk;
#endif

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
	
	secs = year_secs;
	/*
	 * 146818 is brain damaged and may not be initialized to
	 * certain dates, check for them and avoid them by backing
	 * up a couple of seconds.
	 */
	if (day >= 28 && day <= 30 && hours == 23 && mins == 59
	    && secs >= 58 && secs <= 59)
		secs = 57;
	
	XPR5(XPR_TOD,"setting year %d month %d day %d hours %d",BASEYEAR,month,
		day, hours);
	XPR3(XPR_TOD," mins %d secs %d\n",mins,secs);

	if (rt) {	/* IS_R2300 */
		junk = rt->rt_regd;	/* set VRT */
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
		wbflush();
	} else if (todc) {
			/* What follows is the kick start sequence
			   for the TOD chip */
		todc->todc_control |= TODC_CONT_WRITE;	/* Halt to set */
		wbflush();
		todc->todc_secs = 0; wbflush();			/* clear Stop */
		todc->todc_hours = TODC_HOUR_KICK; wbflush();	/* kick-start */
		todc->todc_control &= ~TODC_CONT_WRITE; wbflush();
		DELAY(2097152);	/* Chip claims it needs 2 secs here, phooey */
		todc->todc_control |= TODC_CONT_WRITE; wbflush();
		todc->todc_hours = 0; wbflush();		/* clear Kick */
		todc->todc_secs = dec_to_bcd(secs); wbflush();
		todc->todc_mins = dec_to_bcd(mins); wbflush();
		todc->todc_hours = dec_to_bcd(hours); wbflush();
		todc->todc_daym = dec_to_bcd(day); wbflush();
		todc->todc_dayw &= 3; wbflush();
		todc->todc_month = dec_to_bcd(month); wbflush();
		todc->todc_year = dec_to_bcd(BASEYEAR); wbflush();
		todc->todc_control &= ~TODC_CONT_WRITE;	/* Fire it up */
		wbflush();
	} else {
		printf("write_todclk: invalid machine_type %d\n",machine_type);
		return(-1);
	}
}

/*
 * get_tod_todc -- get current time-of-day from the MK48T02 clock chip
 */

unsigned
get_tod_todc()
{
	register volatile struct todc_clock *todc = 
	  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	u_int month, day, year, hours, mins, secs;
	register char *cp;


	todc->todc_control |= TODC_CONT_READ;	/* Halt chip */
	wbflush();
	if (todc->todc_secs & TODC_SECS_STOP) {
		/*
		 *  tod chip oscillator has been stopped to conserve
		 *  battery life, and it must now be kick-started
		 */
		printf("tod clock being kickstarted!\n");
		todc->todc_control &= ~TODC_CONT_READ; wbflush();
		write_todclk(0);			/* initialize time */
		todc->todc_control |= TODC_CONT_READ;	/* Halt chip for read */
		wbflush();
	}
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

#ifdef MDEBUG
	XPR5(XPR_TOD,"getting year %d month %d day %d hours %d ",year,month,
		day,hours);
	XPR3(XPR_TOD,"mins %d secs %d\n",mins,secs);
#endif

/* What type of sanity check to do? */

	if (month > 12 || day > 31 || hours > 23 || mins > 59 || secs > 59) {

		_errputs("todc clock invalid\n");
		printf("secs=%d mins=%d hours=%d day=%d month=%d year=%d\n",
		    secs, mins, hours, day, month, year);

		cp = get_nvram(NVADDR_STATE, NVLEN_STATE);
		*cp &= ~NVSTATE_TODVALID;
		set_nvram(NVADDR_STATE, NVLEN_STATE, cp);

		printf("initializing todc clock\n");

		write_todclk(0);
	}

	secs = sum_seconds(secs, mins, hours, day, month, year);
	return(secs);
}

sum_seconds(secs, mins, hours, day, month, year)
u_int secs, mins, hours, day, month, year;
{
	register int i;
	u_int totsecs;
	/*
	 * Sum up seconds from beginning of year
	 */
	totsecs = secs;
	totsecs += mins * SECMIN;
	totsecs += hours * SECHOUR;
	totsecs += (day-1) * SECDAY;
	for (i = 0; i < ((int)month)-1; i++)
		 totsecs += month_days[i] * SECDAY;
	i = year - BASEYEAR;
	totsecs += i * (SECYR + SECDAY);
	totsecs += TODRZERO;
	return(totsecs);

}

struct baud {
	long	code;
	char	*str;
	} baud [] = {
    {0, "75"},
    {1, "110"},
    {2, "134"},
    {3, "150"},
    {4, "300"},
    {5, "600"},
    {6, "1200"},
    {7, "1800"},
    {8, "2400"},
    {9, "4800"},
    {10, "9600"},
    {11, "19200"}
#define MAXBAUDCODE	11
};

/*
 * get_nvram -- read string from non-volatile memory
 */
char *
get_nvram(nv_addr, nv_len)
register int nv_addr;
register int nv_len;
{
	register volatile struct rt_clock *rt = 
	  (struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register volatile struct todc_clock *todc = 
	  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	register int i;
	static char buf[64];
	char temp [4];

	if (rt)				/* M500/M800/M1000 */
	    {
            if (nv_addr <= NVADDR_CHKSUM)
	    	{
		for (i = 0; i < nv_len; i++)
			buf[i] = rt->rt_mem[RT_MEMX(nv_addr + i)];
		}
	    else
		{
		i=0;
		}
	    }
	else if (todc) { 	/* M120/M2000//GENESISEXCALIBUR/R3030 */
		for (i = 0; i < nv_len; i++)
	        	buf[i] = todc->todc_mem[nv_addr + i].value;
	} else {
	    printf("get_nvram: invalid machine type %d\n",machine_type);
	    i = 0;
        }
	buf[i] = 0;

	/* decode stuff coming out of nvram so as to save space */
	switch(nv_addr) {
		case NVADDR_NETADDR:
			temp[0] = buf[0]; temp[1] = buf[1];
			temp[2] = buf[2]; temp[3] = buf[3];
			sprintf(buf, "%d.%d.%d.%d", temp[0], temp[1], temp[2],
				 temp[3]);
		    break;
		case NVADDR_LBAUD:
		case NVADDR_RBAUD:
		    for (i = 0; i <= MAXBAUDCODE; i++) {
			if (buf[0] == baud[i].code) {
			    strcpy(buf, baud[i].str);
			    break;
			} /* if */
		    } /* for */
		    if (i == MAXBAUDCODE  &&  !IS_R6300) /* XXX try 19200 */
			strcpy(buf, "invalid baud");
		    break;
		case NVADDR_RESETEPC:
		case NVADDR_RESETRA:
			temp[0] = buf[0]; temp[1] = buf[1];
			temp[2] = buf[2]; temp[3] = buf[3];
			sprintf(buf, "0x%08x", temp[0] | (temp[1]<<8) | 
				(temp[2]<<16) | (temp[3]<<24));
			break;
		case NVADDR_IOAPARM:
			/* Note: this address is used as use_bootparams
			   in Rx3230, so be very careful in using the 
			   else portion of the if statement */ 
			if (IS_R6300) {
			    temp[0] = buf[0];
			    sprintf(buf, "0x%02x", temp[0]);
			}
			break;
		default:;
	} /* switch */

	return(buf);
}

/*
 * set_nvram -- write string to non-volatile memory
 */
set_nvram(nv_addr, nv_len, string)
register int nv_addr;
register int nv_len;
register char *string;
{
	register volatile struct rt_clock *rt = 
	  (struct rt_clock *)MACHDEP(RT_CLOCK_ADDR);
	register volatile struct todc_clock *todc = 
	  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
	register int i;
	register char *p,*start;
	char temp[5];
	register int special;
	unsigned int	num;
	unsigned int	chksum;
	int calcchk();

	/* encode going into nvram to save space */
	switch(nv_addr) {

	case NVADDR_NETADDR:
	    temp[0] = temp[1] = temp[2] = temp[3] = temp[4] = 0;
	    /* convert each part of address to binary format */
	    for (start = p = string, i = 0; i < 4 && *p; p++) {
		if (*p != '.')
		    continue;
		if (start == p)
		    temp[i] = 0;
		else
		    temp[i] = atoi(start);
		start = p + 1;
		i++;
	    } /* for */
	    if (p != start && i < 4)
		temp[i] = atoi(start);

	    string = &temp[0];
	    special = 1;		/* So for loop won't terminate */
	    break;

	    case NVADDR_LBAUD:
	    case NVADDR_RBAUD:
		for (i = 0; i <= MAXBAUDCODE; i++) {
		    if (strcmp(string, baud[i].str) == 0) {
			temp[0] = baud[i].code;
			break;
		    } /* if */
		} /* for */
		if (i == MAXBAUDCODE)
		    temp[0] = i;		/* out of range value */
		string = &temp[0];
		special = 1;		/* So for loop won't terminate */
		break;

	    case NVADDR_RESETEPC:
	    case NVADDR_RESETRA:
		temp[0]=temp[1]=temp[2]=temp[3]=0;

		if (string[0] != '0' || string[1] != 'x')
		    break;
		/* convert ascii hex to binary */
		for(i = 2; i < 10; i++) {

#define IS_HEX(d) (((d>='0')&&(d<='9')) || ((d>='a')&&(d<='f')))
#define HEX_TO_BINARY(d) (((d>='0')&&(d<='9')) ? d-'0' : d-'a')

		    if (!IS_HEX(string[i]))
			break;
		    num = (num << 4) +  HEX_TO_BINARY(string[i]);
		};
		temp[0] = num & 0xff;
		temp[1] = (num>>8) & 0xff;
		temp[2] = (num>>16) & 0xff;
		temp[3] = (num>>24) & 0xff;

		string = &temp[0];
		special = 1;
		break;

	    case NVADDR_IOAPARM:
		/* Note: this address is used as use_bootparams in Rx3230, 
		   so be very careful in using the else portion of the if 
		   statement */ 
	    	if (IS_R6300) {
		  temp[0]=0;

		  if (string[0] != '0' || string[1] != 'x') {
		    string = &temp[0];
		    break;
		  }
		  /* convert ascii hex to binary */
		  for(i = 2; i < 4; i++) {
		    if (!IS_HEX(string[i]))
		      break;
		    num = (num << 4) +  HEX_TO_BINARY(string[i]);
		  };
		  temp[0] = num & 0xff;
		  
		  string = &temp[0];
		  special = 1;
		} 
	    	break;

	default:
	    special = 0;
	} /* switch */

    /* fill nvram */
	if (rt) {				/* M500/M800/M1000 */
            if (nv_addr <= NVADDR_CHKSUM)
	    	{
		for (i = 0; (*string || special) && i < nv_len; i++) {
			rt->rt_mem[RT_MEMX(nv_addr + i)] = *string++;
			wbflush();
		}
		for (; i < nv_len; i++) {
			rt->rt_mem[RT_MEMX(nv_addr + i)] = 0;
			wbflush();
		}

		/* Calculate checksum and write it out */
		chksum = calcchk();
		rt->rt_mem[RT_MEMX(NVADDR_CHKSUM)] = (char )(chksum & 0xff);
		wbflush();
		}
	    else
		{
		return;
		}

	} else if (todc) { 	/* M120/M2000/GENESIS/EXCALIBUR/R3030 */
		for (i = 0; (*string || special) && i < nv_len; i++) {
	        	todc->todc_mem[nv_addr + i].value = *string++;
			wbflush();
		}
		for (; i < nv_len; i++) {
	        	todc->todc_mem[nv_addr + i].value = 0;
			wbflush();
		}
		/* Calculate checksum and write it out */
		chksum = calcchk();
		todc->todc_mem[NVADDR_CHKSUM].value = (char )(chksum & 0xff);
		wbflush();
	} else {
	    printf("get_nvram: invalid machine type %d\n",machine_type);
	    i = 0;
	}
	return;
}

/* 
 * This structure is created so that the kernel does NOT have to know
 * where things are located in the NVRAM.  It just asks for an index
 * which is used to reference into this structure.
 */

struct {
	int addr, len;
} nvram_addrs[] = {
	{ NVADDR_NETADDR, 	NVLEN_NETADDR },	/* NVINDEX_NETADDR */
	{ NVADDR_LBAUD,		NVLEN_LBAUD },		/* NVINDEX_LBAUD */
	{ NVADDR_RBAUD,		NVLEN_RBAUD },		/* NVINDEX_RBAUD */
	{ NVADDR_BOOTFILE,	NVLEN_BOOTFILE },	/* NVINDEX_BOOTFILE */
	{ NVADDR_BOOTMODE,	NVLEN_BOOTMODE },	/* NVINDEX_BOOTMODE */
	{ NVADDR_CONSOLE,	NVLEN_CONSOLE },	/* NVINDEX_CONSOLE */
	{ NVADDR_STATE,		NVLEN_STATE },		/* NVINDEX_STATE */
	{ NVADDR_FAILCODE,	NVLEN_FAILCODE },	/* NVINDEX_FAILCODE */
	{ NVADDR_CPUID,		NVLEN_CPUID },		/* NVINDEX_CPUID */
	{ NVADDR_FLAG,		NVLEN_FLAG },		/* NVINDEX_FLAG */
	{ NVADDR_PONMASK,	NVLEN_PONMASK },	/* NVINDEX_PONMASK */
	{ NVADDR_RESETEPC,	NVLEN_RESETEPC },	/* NVINDEX_RESETEPC */
	{ NVADDR_RESETRA,	NVLEN_RESETRA },	/* NVINDEX_RESETRA */
	{ NVADDR_MEMPARITY,	NVLEN_MEMPARITY },	/* NVINDEX_MEMPARITY */
	{ NVADDR_UBOOTPARAMS,	NVLEN_UBOOTPARAMS },	/* NVINDEX_UBOOTPARAMS */
	{ NVADDR_CHKSUM,	NVLEN_CHKSUM },		/* NVINDEX_CHKSUM */
};

int nvram_cnt = sizeof(nvram_addrs)/sizeof(nvram_addrs[0]);

char *
nv_get(index)
int index; {

	if (index < 0 || index > (nvram_cnt-1))
		return(0);
	else
		return((char *)get_nvram(nvram_addrs[index].addr,
				 nvram_addrs[index].len));
}

nv_set(index, string)
char *string; {
	
	if (index < 0 || index > (nvram_cnt-1))
		return(0);
	else {
		set_nvram(nvram_addrs[index].addr,
				 nvram_addrs[index].len, string);
		return(1);
	}
}

int delay_mult;

#define	TEST_PERIOD	512

_config_delay_r6000()
{
#define CPU_MHZ_R6000	50
#define TEST_SPIN	16384
#define CLOCK_COUNT	*(volatile u_int *)CSR_COUNT
	unsigned long	start_time,
			end_time;
	unsigned long	have_good_value;

	delay_mult = 1;		/* init test value */
	have_good_value = 0;	/* compute things at least once */
	while (!have_good_value) {
		start_time = CLOCK_COUNT;
		DELAY(TEST_SPIN);
		end_time = CLOCK_COUNT;
		if (end_time > start_time) {
			/* no timer count rollover -- ok to compute delay */
			delay_mult
			 = TEST_SPIN		/* supposedly N microsec */
			    / ((end_time - start_time)	/* CPU cycles */
			       /CPU_MHZ_R6000);	/* cycles/usec */
			if (delay_mult <= 0)
				delay_mult = 1;	/* don't go too low! */
			have_good_value = 1;	/* all done */
		} /* if */
	} /* while */
#ifdef SABLE
	delay_mult = 0;		/* Sable is too slow already -- no delays! */
#endif SABLE
}


#define KILO    1000
#define MEGA    (1000*KILO)
#define CLOCK_GUESS_25     25      /* initial guess of system clock */
#define CLOCK_GUESS_33     33      /* initial guess of system clock */
#define MULT    4               /* RAMBO tick to CPU clock tick multiplier*/
#define DELAY_TEST_TIME 256
#define CLOCK_COUNT_3030     *(volatile u_long *)0xbc000c00 /* RAMBO count reg */
#define SECOND_DELAY	500000

char cpu_bd_rev;
static  u_long rambo_hz;     

#ifdef PROM 
int _diff_ticks;

#ifdef R3030
_count_rambo_r3030()
{
	register volatile struct todc_clock *tod = (struct todc_clock *)PHYS_TO_K1(TODC_CLOCK_ADDR_R3030);
	unsigned int secs;
	unsigned long start_tick;
	int i;

	i = SECOND_DELAY;
	secs = tod->todc_secs;
	while ((secs == tod->todc_secs) && (--i != 0)) {
		;			/* wait for seconds to change */
	}
        start_tick = CLOCK_COUNT_3030;
		
	i = SECOND_DELAY;
	secs = tod->todc_secs;
	while ((secs == tod->todc_secs) && (--i != 0)) {
		;			/* wait for seconds to change */
	}
        _diff_ticks = ((CLOCK_COUNT_3030 - start_tick + 50000) * 4)/MEGA;
}
#endif R3030
#endif PROM

_config_delay_r3030() {
        register s;
        unsigned long   start_tick,
                        diff_tick;   
	char *cp;
	extern char *get_nvram();
/*
 * we calculate the delay multiplier as follows:
 * it takes diff_tick rambo ticks to do a 'supposedly'
 * DELAY_TEST_TIME microseconds delay. we generate what
 * by:
 *      rambo_hz/MEGA (number of rambo ticks per second/1000000)
 *
 * we then divide the time it should have taken bu the time it
 * did take, and this is the number of times we would have to do
 * the loop to actually do a real delay of the requested time.
 *
 * we have rearranged the calculation to try and ensure that no overflows
 * occur, either overflow, or underflow. We have to be careful, as the
 * basic rambo_hz = 6250000 and this / MEGA is only 6, not 6.25, an error
 * of 4%
 */
#ifndef SABLE
	cp = (char *)PHYS_TO_K1(IDPROM_R3030+ID_REV_OFF_R3030);
	cpu_bd_rev = *cp;
	if (cpu_bd_rev & 0x20)
	    rambo_hz  = CLOCK_GUESS_33*MEGA/MULT; 
	else   
	    rambo_hz  = CLOCK_GUESS_25*MEGA/MULT;    
        delay_mult = 1;
        start_tick = CLOCK_COUNT_3030;
        DELAY(DELAY_TEST_TIME);
        diff_tick = CLOCK_COUNT_3030 - start_tick;
        delay_mult = ((DELAY_TEST_TIME/diff_tick)*rambo_hz)/MEGA;
	if (!delay_mult) delay_mult++;	/* make delay_mult at least 1 */
#else SABLE
        delay_mult = 0;         /* Sable is too slow already -- no delays! */
#endif SABLE
}

_config_delay()
{
	register volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);
	register unsigned cnt_lo, cnt_hi, cnt;

	if (IS_R6300) {
		_config_delay_r6000();
		return;
	}
	if (IS_R3030) {
		_config_delay_r3030();
		return;
	}
#ifndef SABLE
	delay_mult = 1;

	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_STS);
	pt->pt_counter2 = 0xffff; wbflush();
	pt->pt_counter2 = 0xffff>>8; wbflush();

	DELAY(TEST_PERIOD);

	pt->pt_control = PTCW_SC(2)|PTCW_CLCMD;
	cnt_lo = pt->pt_counter2;
	cnt_hi = pt->pt_counter2;

	stopclocks();

	cnt = 0xffff - ((cnt_hi << 8) | cnt_lo);
	XPR2(XPR_TOD,"8254 count = 0x%x\n",((cnt_hi << 8) | cnt_lo));
	delay_mult = ((unsigned)TEST_PERIOD*MASTER_FREQ);
	delay_mult = ((delay_mult/1000000) + cnt) / cnt;
	XPR3(XPR_TOD,"Delay multiplier = %d, cnt = %d\n", delay_mult, cnt);
	if (cnt_hi == 0 && cnt_lo == 0)
		printf("config_delay: WARNING: TEST_PERIOD too long\n");
	if (delay_mult < 1 || delay_mult > 12)
		printf("config_delay: WARNING: unusual DELAY value\n");
	if (delay_mult <= 0)
		delay_mult = 3;
#endif !SABLE
}

/*
 * Disable clocks 
 */
stopclocks_r6000()
{
	/* XXX add code */
}

stopclocks()
{
	register volatile struct pt_clock *pt = 
		(struct pt_clock *)MACHDEP(PT_CLOCK_ADDR);

	if (IS_R6300) {
		stopclocks_r6000();
		return;
	}
	pt->pt_control = PTCW_SC(0)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter0 = SCHED_PERIOD;
	wbflush();
	pt->pt_counter0 = SCHED_PERIOD>>8;
	pt->pt_control = PTCW_SC(1)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter1 = PROF_PERIOD;
	wbflush();
	pt->pt_counter1 = PROF_PERIOD>>8;
	pt->pt_control = PTCW_SC(2)|PTCW_16B|PTCW_MODE(MODE_HROS);
	pt->pt_counter2 = MASTER_PERIOD;
	wbflush();
	pt->pt_counter2 = MASTER_PERIOD>>8;

	ackrtclock();
	ackkgclock();
}

/*
 * Clear cpu board TIM0 acknowledge register
 */
ackrtclock()
{
	register volatile char *tim0ack = 
		(char *)MACHDEP(TIM0_ACK_ADDR);
	static char bucket;

	if (IS_R6300) {
		/* XXX add code */
	} else {
		bucket = *tim0ack;
	}
}
/*
 * Clear cpu board TIM1 acknowledge register
 */
ackkgclock()
{
	register volatile char *tim1ack = 
		(char *)MACHDEP(TIM1_ACK_ADDR);
	static char bucket;

	if (IS_R6300) {
		/* XXX add code */
	} else {
		bucket = *tim1ack;
	}
}


calcchk()
{
    register volatile struct todc_clock *todc = 
	(struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
    register int i,chksum;
    char *cp;
    char *get_nvram();

    chksum = 0;
        
    /*
     * Only use those fields that are only changed through set_nvram.
     * If it is just written out (e.g. bootmode gets set to 'e' if pons
     * fail.) 
     */
    cp = get_nvram(NVADDR_NETADDR,NVLEN_NETADDR);
    for( i = 0; i < NVLEN_NETADDR; i++)
	chksum = (chksum + *(cp+i)) + 0xff;
    
    cp = get_nvram(NVADDR_LBAUD,NVLEN_LBAUD);
    for( i = 0; i < NVLEN_LBAUD; i++)
	chksum = (chksum + *(cp+i)) + 0xff;
    
	
    cp = get_nvram(NVADDR_RBAUD,NVLEN_RBAUD);
    for( i = 0; i < NVLEN_RBAUD; i++)
	chksum = (chksum + *(cp+i)) + 0xff;
    
    cp = get_nvram(NVADDR_BOOTFILE,NVLEN_BOOTFILE);
    for( i = 0; i < NVLEN_BOOTFILE; i++)
	chksum = (chksum + *(cp+i)) + 0xff;
    
    cp = get_nvram(NVADDR_CONSOLE,NVLEN_CONSOLE);
    for( i = 0; i < NVLEN_CONSOLE; i++)
	chksum = (chksum + *(cp+i)) + 0xff;

    if ( todc )
	{
	/* continue to calculate checksum of all bytes in 
	** the new nvram area
	*/
    	for( i = NVADDR_NEW_AREA_START; i < NVLEN_NEW_AREA_TOTAL; i++)
	    {
	    chksum = (chksum + todc->todc_mem[i].value) + 0xff;
	    }
	}
   return(chksum & 0xff); 
}

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */
/*
** Constants defined for OEM NVRAM access routines
*/


#define		NO_ERR			0
#define		OEM_READ		0
#define		OEM_WRITE		1
#define		OEM_NVRAM_ACCESS_ERR	1

typedef		int		ERR_CODE;

/*
** structure/table defined for OEM nvram partition
*/ 
struct  oem_rw_boundary
    {
    int RO_start;
    int RO_end;
    int RW_start;
    int RW_end;
    };

static struct oem_rw_boundary oem_nvram_part [] = 
{
    { 0, 0, 0, 0 },		/* NULL - offset of BRDTYPE	*/

    { 0, 0, 0, 0 },		/* BRDTYPE_R2300 -- M500	*/

    { 0, 0, 0, 0 },		/* BRDTYPE_R2600 -- M800	*/

    { 0, 0, 0, 0 },		/* BRDTYPE_R2800 -- M1000	*/

    { 0, 0,			/* BRDTYPE_R2400 -- M120	*/
      NVADDR_BASE+NVSTART_OEM,
      NVADDR_BASE+NVSTART_OEM+NVLEN_OEM-1 },	

    { 0, 0, 			/* BRDTYPE_R3200 -- M2000	*/
      NVADDR_BASE+NVSTART_OEM,
      NVADDR_BASE+NVSTART_OEM+NVLEN_OEM-1 },

    { 0, 0, 			/* BRDTYPE_R6300 -- RC6280	*/
      NVADDR_BASE+NVSTART_OEM,
      NVADDR_BASE+NVSTART_OEM+NVLEN_OEM-1 },

    { 0, 0, 0, 0 },		/* BRDTYPE_I2000 -- Jupiter	*/

    { 0, 0, 0, 0 },		/* BRDTYPE_I2000S -- Jupiter	*/

    { 0, 0, 			/* BRDTYPE_M180  -- RC3240	*/
      NVADDR_BASE+NVSTART_OEM,
      NVADDR_BASE+NVSTART_OEM+NVLEN_OEM-1 },

    { NVADDR_BASE+512,		/* BRDTYPE_R3030 -- M20/Pizazz	*/
      NVADDR_BASE+1024-1,
      NVADDR_BASE+1024,
      NVADDR_BASE+1536-1 },

    { 0, 0, 			/* BRDTYPE_RB3125  -- Genesis	*/
      NVADDR_BASE+NVSTART_OEM,
      NVADDR_BASE+NVSTART_OEM+NVLEN_OEM-1 },
};


/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ERR_CODE oem_nvram_read_string
(
int	nv_addr,	/* start address to read string 	*/
int	nv_lens,	/* the string length to be read		*/
char	*string		/* ptr to save string 			*/
)
/*
** This routine is used to read the contents of OEM space of NVRAM.
*/
{
register volatile struct todc_clock *todc = 
  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
ERR_CODE error;
int	 Index;

if ((error = oem_nvram_check_bound(nv_addr, nv_lens, OEM_READ))
    == NO_ERR)
    {
    /* read one string from OEM nvram */
    for (Index = 0; Index < nv_lens; string++, Index++)
	{
	*string = todc->todc_mem[nv_addr + Index].value;
	}
    *string = NULL;
    }
return (error);
}

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ERR_CODE oem_nvram_write_string
(
int	nv_addr,	/* start address to write string	*/
char	*string		/* string to be writen			*/
)
/*
** This routine is used to write a string to the OEM space of NVRAM.
*/
{
register volatile struct todc_clock *todc = 
  (struct todc_clock *)MACHDEP(TODC_CLOCK_ADDR);
ERR_CODE error;

if ((error = oem_nvram_check_bound(nv_addr, strlen(string), OEM_WRITE)) 
    == NO_ERR)
    {
    /* write one string to OEM nvram */
    for (; *string != NULL; string++, nv_addr++)
	{
	todc->todc_mem[nv_addr].value = *string;
	wbflush();
	}
    }
return (error);
}

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

ERR_CODE oem_nvram_check_bound
(
int	start_addr,	/* start address of access rigion	*/
int	length,		/* size of access rigion		*/
int	rw_act		/* TRUE if write action			*/
)
/*
** This routine is used to check the OEM space boundary, if the address
** range exceed the machine dependent boundary, an error will be returned
** to the caller.
*/
{
ERR_CODE error;
int	 end_addr;

error = NO_ERR;
end_addr = start_addr + length - 1;

/* check OEM R/W partition boundary */
if ((start_addr < oem_nvram_part[machine_type].RW_start) ||
    (start_addr > oem_nvram_part[machine_type].RW_end) ||
    (end_addr < oem_nvram_part[machine_type].RW_start) ||
    (end_addr > oem_nvram_part[machine_type].RW_end))
    {
    error = OEM_NVRAM_ACCESS_ERR;
    }

/* if error, still need to check OEM R/O partition boundary */
if (error != NO_ERR)
    {
    if ((start_addr < oem_nvram_part[machine_type].RO_start) ||
	(start_addr > oem_nvram_part[machine_type].RO_end) ||
	(end_addr < oem_nvram_part[machine_type].RO_start) ||
	(end_addr > oem_nvram_part[machine_type].RO_end))
	{
	error = OEM_NVRAM_ACCESS_ERR;
	}
    else
	{
	/* the access rigion is in R/O partition, but if this is
	** write action, still not allowed.
	*/
	error = NO_ERR;
	if (rw_act == OEM_WRITE)
	    error = OEM_NVRAM_ACCESS_ERR;
	}
    }

return (error);
}
