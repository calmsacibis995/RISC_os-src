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
#ident	"$Header: r6000_todc.c,v 1.2.4.2 90/05/10 05:27:42 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/types.h"
#include "sys/param.h"
/* #include "sys/i8254clock.h" */
#include "sys/clock.h"
#include "bsd/sys/time.h"
/* #include "sys/cmn_err.h" */
#include "sys/ctlspace.h"

extern struct timeval time;

#ifdef SABLE
int delay_mult = 0;	/* multiplier for DELAY macro, with initial value */
#else
int delay_mult = 6;	/* multiplier for DELAY macro, with initial value */
#endif !SABLE

#ifdef SABLE
#define R6000_CPU_HZ 	20000000	/* get more timer interrupts */
#else !SABLE				/*  for better u_utime stats */
#define R6000_CPU_HZ 	66666667
#endif !SABLE

static struct ctlspace_registers *ctl_regs
	= (struct ctlspace_registers *)(CTLSPACE_BASE + CTLSPACE_BRD_REGISTERS);


/*
 * Start the real-time clock (really the programable interval clock!).
 */
startrtclock()
{
	register s;
	int	mhz, khz100, khz10;

	mhz    = R6000_CPU_HZ / 1000000;
	khz100 = (R6000_CPU_HZ - (mhz * 1000000)) / 100000;
	khz10  = (R6000_CPU_HZ - (mhz * 1000000) - (khz100 * 100000)) / 10000;
	printf("[ Using %d.%d%d-MHz CPU ]\n", mhz, khz100, khz10 );
	s = spl7();
	/*
	 *  The R6000 clock ticks every cycle, and interrupts whenever the
	 *  COUNT value equals the COMPARE value.
	 */
	ctl_regs->ctl_timer_compare
		= ctl_regs->ctl_timer_count + R6000_CPU_HZ/HZ;
	ctl_regs->ctl_int_vec_clear = CTLSPACE_INT_VEC_TIMER;
	ctl_regs->ctl_int_mask |= CTLSPACE_INT_VEC_TIMER;
	splx( s );
}


/*
 * Disable clocks 
 */
stopclocks()
{
	ctl_regs->ctl_int_mask     &= ~CTLSPACE_INT_VEC_TIMER;
	ctl_regs->ctl_int_vec_clear =  CTLSPACE_INT_VEC_TIMER;
}


/*
 * Mommy, make that mean 'ol clock interrupt go away!
 */
ackrtclock()
{
	register	s;
	unsigned long	old_timer_compare,
			new_timer_compare,
			new_timer_count,
			timer_count_rollover;
	unsigned long	holdoff_microsec;

	s = spl7();
	/*
	 *  Always bump the Compare register by a fixed value, and clear
	 *  the external timer interrupt.
	 */
	old_timer_compare = ctl_regs->ctl_timer_compare;
	new_timer_compare = old_timer_compare + R6000_CPU_HZ/HZ;
	ctl_regs->ctl_timer_compare = new_timer_compare;

	/*
	 *  It is possible that timer interrupts have been held off so long
	 *  that the Count has moved beyond the new Compare.  In that case
	 *  we must turn the external timer interrupt back on again.  
	 *  (We could just avoid clearing the interrupt in this case, but I
	 *  think there is a timing window which this algorithm avoids.)
	 *  We need to take Count rollovers into account, so we must presume
	 *  that interrupts won't be held off longer than 30 bits of Count.
	 */

	new_timer_count = ctl_regs->ctl_timer_count;
	timer_count_rollover
	   = (old_timer_compare > 0xc0000000) && (new_timer_count < 0x40000000);

	if (new_timer_compare > old_timer_compare) {
	    /*
	     *  Compare has *not* rolled over
	     */
	    if (timer_count_rollover) {
		/*
		 *  Compare hasn't rolled over, but Count has, so Count has
		 *  clearly moved beyond Compare
		 */
		ctl_regs->ctl_int_vec_set = CTLSPACE_INT_VEC_TIMER;
	    } else {
		/*
		 *  neither Count nor Compare have rolled over -- simple case
		 */
		if (new_timer_count >= new_timer_compare)
		    ctl_regs->ctl_int_vec_set = CTLSPACE_INT_VEC_TIMER;
	    } /* else: neither Count nor Compare have rolled over */
	} else {
	    /*
	     *  Compare *has* rolled over
	     */
	    if (timer_count_rollover) {
		/*
		 *  both Count and Compare have rolled over -- simple case
		 */
		if (new_timer_count >= new_timer_compare)
		    ctl_regs->ctl_int_vec_set = CTLSPACE_INT_VEC_TIMER;
	    } 
		/* else:
		 *  Compare has rolled over, but Count has not, so clearly
		 *  Count hasn't moved beyond Compare
		 */
	} /* timer Compare has rolled over */
	splx( s );
}


config_delay()
{
#define	DELAY_TEST_TIME 500
	register s;
	unsigned long	start_time,
			end_time,
			loop_count,
			loop_max = 20;
	unsigned long	have_good_value;

	s = spl7();		/* disable interrupts */
	delay_mult = 1;		/* init test value */
	have_good_value = 0;	/* compute things at least once */
	while (!have_good_value) {
		start_time = ctl_regs->ctl_timer_count;
		for (loop_count = 1; loop_count <= loop_max; loop_count++) {
			DELAY( DELAY_TEST_TIME );
		} /* for */
		end_time = ctl_regs->ctl_timer_count;
		if (end_time > start_time) {
			/* no timer count rollover -- ok to compute delay */
			delay_mult
			 = DELAY_TEST_TIME	/* supposedly N microsec */
			    / (((end_time - start_time)	/* CPU cycles */
			       /(R6000_CPU_HZ/1000000)	/* cycles/usec */
			       )/loop_max);
			have_good_value = 1;	/* all done */
		} /* if */
	} /* while */
#ifdef SABLE
	delay_mult = 0;		/* Sable is too slow already -- no delays! */
#endif SABLE
	splx(s);		/* restore interrupt state */
}
