#ident "$Header: pon_kbdtest.c,v 1.2.1.1 90/07/18 14:31:57 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
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

/********************************************************************
 *
 *	Name : pon_kbd_t2.c
 *
 *	Description :
 *		Award Keyboard Controller ( i8042 ) tests.
 *		The keyboard controller communicates with the system
 *		through 4 ports, input port, output port, status port
 *		and command port. command port and status port have the
 *		same address (CONTROL) .
 *		input and output port have the same address (DATA).
 *
 *      control ------|---> Command port
 *                    |
 *                    |<--- Status port
 *
 *         data ------|---> input buffer
 *                    |
 *                    |<--- output buffer
 *
 ********************************************************************/

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/i8042.h"
#include "prom/prom.h"
#include "pon.h"

#define NOT_READY	0x1299beef
#define READY		0
#define READY_DELAY	0x10
#define DELAY_K		10
#define KBD_ACK		0xfa
#define KBD_LED_CMD	0xed

#define IF_OK		0x00		/* interface test passed */
#define	CB_ENA_INTR	0x01		/* enable interrupt */

struct kbd {
	unsigned char pad0[3];
	unsigned char data;
	unsigned char pad1[3];
	unsigned char control;
};

extern char success[], failure[], skipped[];

static volatile struct	kbd *kbd = (struct kbd *)PHYS_TO_K1(KBD_BASE);
static volatile u_char kbd_status;


/*
 * Name : kbd_iface_t
 * Description : checks the interface between the keyboard and the
 *		keyboard controller (i8042). by issuing the
 *		KEYBOARD_INTERFACE_TEST command to the controller.
 */
static kbd_iface_t()
{
		/* enable keyboard Interface
		 */
	if( wrt_kbd_cmd( I8042EKI ) == FAIL ) {
	  return( FAIL );
	}

		/* clear output buffer
		 */
	if( ready_to_rd() == READY ) {
	  clear_kbd_buf();
	}

		/* write INTERFACE_TEST Command to command reg.
		 */
	if( wrt_kbd_cmd( I8042TKI ) == FAIL ) {
	  return( FAIL );
	}

		/* read test result
		 */
	if( ready_to_rd() != READY ) {
          return( FAIL );
	}
	else
	  kbd_status = kbd->data;

	if( kbd_status != IF_OK )
	  return( FAIL );

		/* disable keyboard interface
		 */
	if( wrt_kbd_cmd( I8042DKI ) == FAIL )
	  return( FAIL );

	return( PASS );
}


/*
 * Name : ready_to_wrt
 * Description : It checks the input buffer of the keyboard controller.
 *		 returns READY, if it is ready for an input. If it is not
 *		 ready, it returns NOT_READY.
 */
static ready_to_wrt()
{
	u_long i;
#ifdef DEBUG
	pon_puts("ready_to_wr " );
#endif DEBUG
		/* wait for controller to become ready
		 */
	for( i = 0 ; i < DELAY_K ; i++ ) {
	   if( (kbd->control & I8042IBF) ) {
	     kbd_delay( READY_DELAY*15 );
	   }
	   else
	     break;
	}

	if( i == DELAY_K ) {
	  return( NOT_READY );
	}
	else
	  return( READY );
}


/*
 * Name : ready_to_rd
 * Description : It checks the output buffer of the keyboard controller.
 *		 returns READY, if there is data in the output buffer.
 *		 If there is no data, it returns NOT_READY.
 */
static ready_to_rd()
{
	u_long i;
#ifdef DEBUG
	pon_puts("ready_to_rd " );
#endif DEBUG
		/* Wait for the controller to become ready
		 */
	for( i = 0 ; i < DELAY_K ; i++ ) {
	   if( !(kbd->control & I8042OBF) ) {
	     kbd_delay( READY_DELAY*15 );
	   }
	   else
	     break;
	}

	if( i == DELAY_K ) {
	  return( NOT_READY );
	}
	else {
	  return( READY );
	}
}


/*
 * Name : kbd_delay
 * Description :  delay loop for short timeouts.
 */
static kbd_delay(n)
u_long n;
{
    u_long i, j;

    for (i=0; i<n ; i++) {
	for ( j=0 ; j<n ; j++)
	    ;
    }
}


/*
 * Name : wrt_kbd_cmd
 * Description : write a command to the keyboard command port.
 *		 It checks the input buffer to make sure it is empty, before
 *		 it write the command.
 */
static wrt_kbd_cmd(cmd)
u_char cmd;
{
	u_long i;

#ifdef DEBUG
	pon_puts("wrt_kbd_cmd ");
#endif DEBUG

		/* wait a while
		 */
	for( i = 0 ; i < DELAY_K ; i++ ) {
	   if( (kbd->control & I8042IBF) || (kbd->control & I8042OBF) ) {
	     kbd_delay( READY_DELAY*15 );
	   }
	   else
	     break;
	}

	if( i == DELAY_K ) {
	  return( FAIL );
	}

	kbd->control = cmd;
	return( PASS );
}


static clear_kbd_buf()
{
	u_char r;

#ifdef DEBUG
	pon_puts( "clear_kbd_buf ");
#endif DEBUg

	while( kbd->control & I8042OBF ) {
	     r = kbd->data;
	     kbd_delay( READY_DELAY );
	}

	return( PASS );
}


#ifdef	USED
/*
 *	Name : kbd_led_t
 *	Description : makes the Status Indicator LEDs on the keyboard
 *			To Count in binary from 0-7
 */
static kbd_led_t()
{
	register u_char led;

#ifdef DEBUG
	pon_puts("led ");
#endif DEBUG

   for( led = 1 ; led <= 8 ; led <<= 1 ) {

		/* send LED command
		 */
	if( ready_to_wrt() != READY ) {
	  return( FAIL );
	}
	else
	  kbd->data = KBD_LED_CMD;

		/* receive the ACK
		 */
	if( ready_to_rd() != READY ) {
	  return( FAIL );
	}

	if( kbd->data != KBD_ACK ) {
	  return( FAIL );
	}

		/* send LED pattern
		 */
	if( ready_to_wrt() != READY ) {
	  return( FAIL );
	}
	else
	  kbd->data = ((led & 0x02) << 1) | ((led & 0x04) >> 1) | (led & 0x01);

		/* receive the ACK
		 */
	if( ready_to_rd() != READY ) {
	  return( FAIL );
	}

	if( kbd->data != KBD_ACK ) {
	  return( FAIL );
	}
   }

	return( PASS );
}
#endif	USED


/*
 * Name: pn_kbd_int
 * Description : Keyboard Controller Interrupt test
 */
static pn_kbd_int()
{
	register volatile u_char cmd, data;

		/* read Controller Command Byte.
		 * write READ_CMD_BYTE command, to control register
		 */
	if( wrt_kbd_cmd( I8042RCR ) == FAIL )
	  return( FAIL );

	if( ready_to_rd() != READY ) {
	  return( FAIL );
	}
	else
	  cmd = kbd->data;

#ifdef DEBUG
	printf("\nKeyboard Command Byte=%x", cmd );
#endif DEBUG

		/* Set Enable_interrupt_on_output_buffer_full
		 * and write the command byte
		 */
	data = (cmd | CB_ENA_INTR );

	if( wrt_kbd_cmd( I8042WCR ) == FAIL )
	  return( FAIL );

	if( ready_to_wrt() != READY )
	  return(FAIL );
        else {
	  kbd->data = data;
	}

#ifdef DEBUG
	printf("\nKeyboard Command Byte=%x", data );
	printf("\nBefore Interrupt, CAUSE=%x, IR=%x", GetCause(), GetIntR() );
#endif DEBUG

		/* verify there is no keyboard interrupt pending
		 */
	if( (GetCause() & CAUSE_IP3) ) {
	  return( FAIL );
	}
	if( !(GetIntR() & IR_KBD_INT_B) ) {
	  return( FAIL );
	}

	if( kbd->control & I8042OBF ) {
	  return( FAIL );
	}

	if( wrt_kbd_cmd( I8042RIP ) == FAIL ) {
	  return( FAIL );
	}

		/* verify the key that output buufer is full
		 */
	if( ready_to_rd() != READY ) {
	  return( FAIL );
	}

		/* verify interrupt is pending
		 */
	if( !(GetCause() & CAUSE_IP3) ) {
	  return( FAIL );
	}
	if( GetIntR() & IR_KBD_INT_B ) {
	  return( FAIL );
	}

#ifdef DEBUG
	printf("\nAfter Interrupt, CAUSE=%x, IR=%x", GetCause(), GetIntR() );
#endif DEBUG

	data = kbd->data;

#ifdef DEBUG
	printf("\nAfter Clearing Interrupt, CAUSE=%x, IR=%x", GetCause(), GetIntR() );
#endif DEBUG
		/* verify interrupt is cleared
		 */
	if( (GetCause() & CAUSE_IP3) ) {
	  return( FAIL );
	}
	if( !(GetIntR() & IR_KBD_INT_B) ) {
	  return( FAIL );
	}

		/* ReSet Enable_interrupt_on_output_buffer_full
		 * and write the command byte
		 */
	if( wrt_kbd_cmd( I8042WCR ) == FAIL )
	  return( FAIL );

	if( ready_to_wrt() != READY )
	  return(FAIL );
        else {
	  kbd->data = cmd;
	}

	return( PASS );
}


Pon_KbdTest()
{
	int error = 0;

	pon_puts("Keyboard Interface Tests...");

	if( GetDepend() & PON_FAULT_KEYBOARD ) {
	  pon_puts( skipped );
	  return( FAIL );
	}

			/* keyboard interface test
			 */
	if (!(GetPonEnviron() & PON_KBD_NOT_PRESENT)) {
		if( kbd_iface_t() == FAIL ) {
		  error = 1;
		  goto done;
		}

#ifdef	USED
			/* keyboard LED test
			 */
		if( kbd_led_t() == FAIL ) {
		  error = 1;
		  goto done;
		}
#endif	USED
	}

	if( pn_kbd_int() == FAIL ) {
	  error = 1;
	  goto done;
	}

done:
	if (error) {
		pon_puts( failure );
		FastFlash(0);
		SetDepend(PON_FAULT_KEYBOARD);
		return( FAIL );
	}

	pon_puts( success );
	return( PASS );
}
