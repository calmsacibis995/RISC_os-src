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
#ident	"$Header: leds.c,v 1.5.1.2 90/05/10 10:51:38 wje Exp $"

#include "sys/sbd.h"
#include "sys/cpu_board.h"

extern int LED_REG[];			/* Lboot'able addresses */
			
/*
 * set_leds -- write pattern into cpu board leds
 */
set_leds(pattern)
{
	register char *led_address = (char *)PHYS_TO_K1(MACHDEP(LED_REG));

	if (led_address != (char *)PHYS_TO_K1(0)) {

		switch(machine_type) {
		case BRDTYPE_R3200:	/* 6 bits (2 ignored) - inverted */
		case BRDTYPE_R2400:	/* 8 bits - inverted */
		case BRDTYPE_M180:	/* 8 bits - inverted */
			*led_address = ~pattern;	/* 0=on 1=off */
			break;
		case BRDTYPE_R6300:
			*led_address = pattern & 0x3F;	/* 1=on 0=off */
			break;
		default:
			/*
			 * or in 0xC0 to avoid resetting local memory interface
			 * and coprocessor interface on R2[368]00 CPUs.
			 */
			*led_address = ~pattern | 0xC0;	/* 0=on 1=off */
			break;
		}

	} /* if non-null led_address */
}
