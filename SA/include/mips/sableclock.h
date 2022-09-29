#ident "$Header: sableclock.h,v 1.2 90/01/23 14:19:44 huang Exp $"
/* $Copyright$ */

/*
 * Definition of the sable scheduling/time of day clock.
 */

struct sable_clock {
	unsigned long	sc_command;	/* stop/start scheduling ticks */
	unsigned long	sc_sched_hz;	/* rate of scheduling tick */
	unsigned long	sc_prof_hz;	/* rate of profiling tick */
	unsigned long	sc_todr;	/* time of day register. READONLY */
};

#define SC_SCHED_START	1
#define SC_SCHED_ACK	2
#define SC_PROF_START	4
#define SC_PROF_ACK	8
#define SABLE_CLOCK_BASE	(struct sable_clock *)(0x1f002000+K1BASE)
