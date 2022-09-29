#ident "$Header: pon_kbd.s,v 1.2.5.1 90/07/18 14:31:50 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restrictive Rights Legend                        |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#  $ */

#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/i8042.h"
#include "prom/prom.h"
#include "pon.h"

#define	KBD_INPUT_BUF		0
#define	KBD_OUTPUT_BUF		0
#define	KBD_CMD			4
#define	KBD_STATUS		4
#define	KBD_SELF_TST_OK		0x55
#define	KBD_BAT_OK		0xaa
#define	KBD_IF_TST_OK		0
#define	KBD_RESET		0xff
#define	KBD_ACK			0xfa

#define	KBD_DELAY		100

		.extern	failure
		.extern	success

		.text

 # ----------------------------------------------------------------------
 #
 # delay_1 : a delay routine
 #
 # ----------------------------------------------------------------------
LEAF( delay_1 )

		li	v0,0x10000
98:		nop
		subu	v0,1
		bne	v0,zero,98b
		nop
		j	ra

END( delay_1 )


 # -----------------------------------------------------------------------
 #
 # Pon_KbdST : Keyboard controller self test.
 #	This test sends the keyboard SLEFTEST COMMAND to
 #	the keyboard controller (i8042) and waits for the
 #	results. If the keyboard controller return good selftest
 #	status, this routine returns 0, otherwise it returns -1.
 #
 # -----------------------------------------------------------------------
LEAF(Pon_KbdST)

		move	a3, ra			# save return address

#ifdef	USED
		la	a0, kbdselfmsg		# print selftest message
		jal	pon_puts
#endif	USED

		li	s0,K1BASE|KBD_BASE	# keyboard base address

	/****************************************
	 * Wait for keyboard controller to become ready.
	 ***************************************/
		li	s1,KBD_DELAY		# initialize count
1:
		lw	s2, KBD_STATUS(s0)	# get status
		and	s2,(I8042IBF|I8042OBF)	#
		beq	s2,zero,2f		# ready for command
		jal	delay_1
		subu	s1,1
		bne	s1,zero,1b
		nop
		b	kbd_self_t_f		# timed out
2:
	/****************************************
	 * send selftest command to 8042
	 ***************************************/
		li	s1, I8042PST		# perform self test
		sw	s1, KBD_CMD(s0)		# write to command port

	/****************************************
	 * Wait for the result of selftest
	 ***************************************/
		li	s1,KBD_DELAY		# initialize count
1:
		lw	s2,KBD_STATUS(s0)	# get status
		and	s2,I8042OBF		# mask output buffer full bit
		bne	s2,zero,2f		#
		jal	delay_1
		subu	s1,1
		bne	s1,zero,1b
		nop
		b	kbd_self_t_f		# timed out
		nop
2:
		lw	s3,KBD_OUTPUT_BUF(s0)	# read the selftest status

		li	s4, KBD_SELF_TST_OK	# selftest passed ?
		andi	s3, 0xff		#
		bne	s3, s4, kbd_self_t_f	# No
		nop				#
						# Yes
#ifdef	USED
		la	a0, success		# print PASSED message
		jal	pon_puts
#endif	USED

#ifdef DEBUG
		move	a0, s3			# print selftest returned status
		jal	pon_puthex
#endif DEBUG
		li	v0, 0			# return 0
		j	a3

kbd_self_t_f:
#ifdef	USED
		la	a0, failure		# print FAILED message
		jal	pon_puts
#endif	USED

		li	a0,0			# just sound the buzzer
		jal	FastFlash

		li	a0,PON_FAULT_KEYBOARD
		jal	SetDepend

#ifdef DEBUG
		lw	a0, KBD_STATUS(s0)	# print i8042 status reg.
		jal	pon_puthex

		move	a0, s4			# print selftest returned status
		jal	pon_puthex
#endif DEBUG
		li	v0, 0xffffffff		# return non-zero
		j	a3

END(Pon_KbdST)


 # -----------------------------------------------------------------------
 #
 #	Pon_KbdBAT : Keyboard Basic Assurance Test.
 #	This test sends the keyboard RESET COMMAND to
 #	the keyboard and waits for the
 #	results. If the keyboard returns good
 #	status, this routine returns 0, otherwise it returns -1.
 #
 # -----------------------------------------------------------------------
LEAF(Pon_KbdBAT)

		move	t5, ra			# save return address

		jal	GetDepend
		and	v0,PON_FAULT_KEYBOARD
		bne	v0,zero,4f		# 8042 self-test must have failed so
						#   don't do anything

		li	t0,K1BASE|KBD_BASE	# keyboard base address

	/****************************************
	 * Wait for keyboard controller to become ready to receive a command.
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2, KBD_STATUS(t0)	# get status
		and	t2,(I8042IBF|I8042OBF)	#
		beq	t2,zero,2f		# ready for command
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
2:
	/****************************************
	 * send Enable command to 8042
	 ***************************************/
		li	t1, I8042EKI		# Enable keyboard interface
		sw	t1, KBD_CMD(t0)		# write to command port

		jal	delay_1
		jal	delay_1

	/****************************************
	 * Clear Output buffer
	 ***************************************/
		lw	t3,KBD_OUTPUT_BUF(t0)	# read Output Buffer

	/****************************************
	 * Wait for keyboard controller to become ready to receive a command.
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2, KBD_STATUS(t0)	# get status
		and	t2,(I8042IBF|I8042OBF)	#
		beq	t2,zero,2f		# ready for command
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
2:
	/****************************************
	 * send RESET command to keyboard
	 ***************************************/
		li	t1, KBD_RESET		# keyboard reset command
		sw	t1, KBD_INPUT_BUF(t0)	# write to input port

	/****************************************
	 * Wait for keyboard controller output buffer to become full.
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2, KBD_STATUS(t0)	# get status
		and	t2,(I8042OBF)		#
		beq	t2,zero,2f		# data in output port ?
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
2:
		jal delay_1

 #		move	a0, t2			# print selftest returned status
 #		jal	pon_puthex

	/****************************************
	 * Read the ACK from Keyboard
	 ***************************************/
		lw	t3,KBD_OUTPUT_BUF(t0)	# read Output Buffer

		li	t4, KBD_ACK		# Is this ACK ?
		andi	t3, 0xff		#
		beq	t3, t4, 3f		# Yes

	/****************************************
	 * Wait for keyboard controller to become ready to receive a command.
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2, KBD_STATUS(t0)	# get status
		and	t2,(I8042IBF|I8042OBF)	#
		beq	t2,zero,2f		# ready for command
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
2:
	/****************************************
	 * send disable command to 8042
	 ***************************************/
		li	t1, I8042DKI		# disable keyboard interface
		sw	t1, KBD_CMD(t0)		# write to command port

		jal	delay_1
		jal	delay_1

#ifdef	DEBUG
		la	a0, kbdwarn		# print keyboard warning message
		jal	pon_puts
#endif	DEBUG

		b	kbd_bat_f		# failed
3:
	/****************************************
	 * Wait for the result of BAT
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2,KBD_STATUS(t0)	# get status
		and	t2,I8042OBF		# mask output buffer full bit
		bne	t2,zero,2f		#
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
		nop
2:
		lw	t3,KBD_OUTPUT_BUF(t0)	# read the BAT status

	/****************************************
	 * Wait for keyboard controller to become ready to receive a command.
	 ***************************************/
		li	t1,KBD_DELAY		# initialize count
1:
		lw	t2, KBD_STATUS(t0)	# get status
		and	t2,(I8042IBF|I8042OBF)	#
		beq	t2,zero,2f		# ready for command
		jal	delay_1
		subu	t1,1
		bne	t1,zero,1b
		nop
		b	kbd_bat_f		# timed out
2:
	/****************************************
	 * send disable command to 8042
	 ***************************************/
		li	t1, I8042DKI		# disable keyboard interface
		sw	t1, KBD_CMD(t0)		# write to command port

		jal	delay_1
		jal	delay_1

	# ------------------
		li	t4, KBD_BAT_OK		# Basic Assurance test passed ?
		andi	t3, 0xff		#
		bne	t3, t4, kbd_bat_f	# No

#ifdef	DEBUG
		move	a0, t3			# print selftest returned status
		jal	pon_puthex
#endif	DEBUG

		b	4f
kbd_bat_f:
#ifdef	DEBUG
		lw	a0, KBD_STATUS(t0)	# print i8042 status reg.
		jal	pon_puthex

		move	a0, t4			# print selftest returned status
		jal	pon_puthex
#endif	DEBUG

		li	a0,PON_KBD_NOT_PRESENT
		jal	SetPonEnviron
4:
		move	v0,zero			# always return good status since a
						#   keyboard is optional
		j	t5

END( Pon_KbdBAT )

		.data

#ifdef	USED
kbdselfmsg:
		.asciiz "Keyboard Controller Self-Test..."
#endif	USED
