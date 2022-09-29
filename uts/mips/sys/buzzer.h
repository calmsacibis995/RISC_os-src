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
/* $Header: buzzer.h,v 1.2.2.2 90/05/10 06:06:29 wje Exp $ */
/*
 * $Header: buzzer.h,v 1.2.2.2 90/05/10 06:06:29 wje Exp $
 */
/*
 * interface for the V50 buzzer.
 */
struct buzzer {
	u_short buzzer_time;	/* duration of the buzzer in 10ms units */
	u_short buzzer_load;	/* pitch of the buzzer */
	u_char buzzer_flags;	/* various control flags */
};

/*
 * convert frequency in HZ of the resulting square wave to a 
 * load value for the clock. input clock is 5mhz
 */
#define HZ_TO_LOAD(h)	(5000000 / h)

/*
 * TCT#2 load value for a 440 HZ square wave for testing the buzzer.
 */
#define TCT2_LOAD	HZ_TO_LOAD(440)		/* load value for 440 hz */

/*
 * flag defines.
 */
#define BUZZER_IE		1	/* enable interrupt when done buzzing */
