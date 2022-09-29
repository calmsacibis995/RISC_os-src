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
#ident	"$Header: leds.c,v 1.10.1.2.1.3 90/07/12 10:40:56 hawkes Exp $"

#include "sys/sbd.h"
#include "sys/cpu_board.h"

/* passes through idle loop between changes */
static int LED_PAUSE[] = {
	 8000,	/* M500 */
	 8000,	/* M800 */
	 8000,	/* M1000 */
	 8000,	/* M/120 */
	 8000,	/* M2000 */
	30000,	/* RC62x0 */
	80000,	/* Rx2030 */
	80000,	/* Rx2030 */
	 8000,	/* M/180 */
	 8000,	/* spare */
	 8000,	/* RB3125 */
	 8000,	/* spare */
	 8000 	/* spare */
    };

#define LEDS_ON	((IS_R2400) ? 0xff : 0x3f)
#define HIGH_LED ((IS_R2400) ? 0x80 : 0x20)

int _led_pause;
int _led_pattern = 1;
int _led_direction = 1;

reset_leds()
{
	_led_pattern = LEDS_ON;
	_led_pause = 0;
}

static v50_mem_problem;
/* Check to see whether the CPU-led display has changed since
 * the last call to this routine.  If not, change it by
 * toggling the top (least often lit) bit.
 * This function is called every now and then
 * (probably once a second) directly from the clock interrupt
 * handler -- even if the CPU is not at the base priority.
 * This way if there isn't enough CPU idle time to change the leds
 * they will change at least once each second in some way that indicates
 * a CPU bound system.  In particular, it is possible to see
 * the difference between a hung system and one "alive" at least
 * enough to let the clock interrupt.
 * When the idle loop changes the leds, it will "overwrite" this
 * special mark led with whatever the next pattern would have been.
 * This is, perhaps, mainly a system programmer assist.
 */

static int led_pat_last;	/* last led pattern seen by this routine */
static int xmask = 0;		/* xor mask to use - need to toggle	*/

cpu_bound_leds()
{
	if (led_pat_last == _led_pattern) {
		xmask ^= HIGH_LED;
		set_leds (_led_pattern ^ xmask);
	} else {
		/* pattern has changed, remember it and clear mask */
		led_pat_last = _led_pattern;
		xmask = 0;
	}
}


bump_leds()
{
	if (++_led_pause > MACHDEP(LED_PAUSE)) {
		_led_pause = 0;
		/*
		 * Jupiter only has one led to blink.  Therefore the pause
		 * delay needs to be increased by an order of magnitude so that
		 * the change will be seen.
		 */
		if (IS_I2000) {
# ifdef notdef
		    /*
		     * 27-Aug-1988.
		     * The V50 clock task has a local copy of the clocks command
		     * block.  For some unknown reason (software problem likely)
		     * the V50's version has changed disabling clock interrupts
		     * to the R2000.  This way every couple of seconds we'll 
		     * update the V50's copy.
		     */
		    if (v50_mem_problem++ > 100) {
			v50_mem_problem = 0;
			startrtclock();
		    }	
# endif /* notdef */
		    if (_led_direction == 1) {
			_led_direction = 0;
			set_leds(1);
		    }
		    else {
			_led_direction = 1;
			set_leds(0);
		    }
		} else {
		    if (_led_direction == 1 && _led_pattern == LEDS_ON) {
			    _led_direction = -1;
		    }
		    if (_led_direction == -1 && _led_pattern == 1) {
			    _led_direction = 1;
		    }
		    if (_led_direction == 1) {
			    _led_pattern <<= 1;
			    _led_pattern |= 1;
		    }
		    else {
			    _led_pattern >>= 1;
		    }
		    set_leds (_led_pattern);
		} /* if */
	} /* if */
}
