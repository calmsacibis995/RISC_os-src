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
/* $Header: sableclock.h,v 1.6.3.2 90/05/10 04:43:13 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Definition of the sable scheduling/time of day clock.
 */

struct bsd43_(sable_clock) {
	unsigned long	sc_command;	/* stop/start scheduling ticks */
	unsigned long	sc_sched_hz;	/* rate of scheduling tick */
	unsigned long	sc_prof_hz;	/* rate of profiling tick */
	unsigned long	sc_todr;	/* time of day register. READONLY */
};

#define BSD43_SC_SCHED_START	1
#define BSD43_SC_SCHED_ACK	2
#define BSD43_SC_PROF_START	4
#define BSD43_SC_PROF_ACK	8
#define BSD43_SABLE_CLOCK_BASE	(struct bsd43_(sable_clock) *)(0x1f002000+BSD43_K1BASE)

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define SABLE_CLOCK_BASE BSD43_SABLE_CLOCK_BASE
#   define SC_PROF_ACK BSD43_SC_PROF_ACK
#   define SC_PROF_START BSD43_SC_PROF_START
#   define SC_SCHED_ACK BSD43_SC_SCHED_ACK
#   define SC_SCHED_START BSD43_SC_SCHED_START
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


