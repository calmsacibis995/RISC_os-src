#ident "$Header: pon_duart.s,v 1.11.3.1 90/07/18 14:30:34 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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

#include "machine/cp0.h"
#include "machine/cpu_board.h"
#include "machine/duart.h"
#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "pon.h"
#include "prom.h"

#ifdef SABLE
#define DELAYCNT	1
#else
#define	DELAYCNT	40000
#endif !SABLE

	.globl DUART0

	.text

LEAF(Pon_Duart)

	move	s0,ra			# save our return address
	move	gp,zero			# indicate NO error so far
	li	a0,PON_DUART1A
	jal	pon_set_leds

 # This DELAY IS NECESSARY to avoid funny character emission!
#ifdef SABLE
	li	s7,1
#else
	li	s7,0x3000
#endif !SABLE
1:
	addiu	s7,s7,-1		# decrement the count
	bne	s7,zero,1b		# spin till it be 0

	DUART0_BASE
	move	s5,v0
	li	s7,0x2A			# (reset receiver, disable tx & rx)
	sb	s7,DOFF_CR(s5)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x3A			# (reset transmitter, disable tx & rx)
	sb	s7,DOFF_CR(s5)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x4A			# (reset error status, disable tx & rx)
	sb	s7,DOFF_CR(s5)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x1A			# (reset MR pointer, disable tx & rx)
	sb	s7,DOFF_CR(s5)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x13			# (no parity, 8 bits/char)
	sb	s7,DOFF_MR(s5)		# (MR1A)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x87			# (local loopback,1 stop bit)
	sb	s7,DOFF_MR(s5)		# (MR2A)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0xBB			# (9600 baud only for now)
	sb	s7,DOFF_CSR(s5)		# (channel A St/Clk Sel Reg)
	jal	FlushWB			# Guarantee write occurs

	li	s7,0x5			# (enable TX,RX)
	sb	s7,DOFF_CR(s5)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s7,DELAYCNT
$34:
	lbu	s2,DOFF_SR(s5)		# grab status
	and	s6,s2,SRTXRDY		# and see if TX'er is ready to XMIT
	bne	s6,0,$35

	addiu	s7,s7,-1		# decrement the count
	beq	s7,zero,1f		# spin till it be 0

	b	$34			# spin till TX ready
1:
	b	failed
$35:
	move	a2,zero
$36:
	addu	s1,a2,TBYTE_DATA	# setup new character

 # Transmit character

	sb	s1,DOFF_RHRA(s5)	# RHRA/THRA
	jal	FlushWB			# Guarantee write occurs

	li	s7,DELAYCNT
$37:
	lbu	s2,DOFF_SR(s5)		# grab status
	and	s3,s2,SRRXRDY		# and see if a RX char is ready
	bne	s3,0,$38

	addiu	s7,s7,-1		# decrement the count
	beq	s7,zero,1f		# spin till it be 0

	b	$37			# just spin till ready
1:
	b	failed
$38:
 # Receive character

	lbu	s4,DOFF_RHRA(s5)	# RHRA/THRA
	addu	s1,a2,VBYTE_DATA

 # Verify the data

	beq	s4,s1,$39

	b	failed
$39:
	addu	a2,a2,1			# inc char count
	bltu	a2,BYTE_CNT,$36		# ship another character?

	b	13f
failed:
	li	gp,1			# indicate error for later on

 # No?  Re-initialize the DUART channel to allow debug prints
13:
	DUART0_BASE			# Setup DUART pointer
	la	a0,duart_table		# Point to DUART inialization table
	b	2f			# go forward and pickup 1st offset byte
1:
	addu	a2,s1,v0
	addu	a0,1			# bump to initial value of current register
	lbu	a1,0(a0)		# Pickup initialization dat
	addu	a0,1
	sb	a1,0(a2)		# store the data
	jal	FlushWB			# Guarantee write occurs
2:
	lbu	s1,0(a0)		# Pick up register
	bne	s1,0xcd,1b		# Done?

	li	a0,1			# argument to indicate lbaud rate
	jal	get_nvram_baud

	move	s1,v0			# save baud rate

	DUART0_BASE
	sb	s1,DOFF_CSR(v0)		# (channel A St/Clk Sel Reg)
	jal	FlushWB			# Guarantee write occurs

	li	s4,SEL_MR1		# point to mr1
	sb	s4,DOFF_CR(v0)		# (channel A CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s5,0x13			# (no parity, 8 bits/char)
	sb	s5,DOFF_MR(v0)		# (MR1A)
	jal	FlushWB			# Guarantee write occurs

	beq	s1,b110,1f		# see if nvram was set to 110 baud

	li	s6,stop1		# (1 stop bit)
	b	2f
1:
	li	s6,stop2		# (2 stop bits if 110 baud)
2:
	sb	s6,DOFF_MR(v0)		# (MR2A)
	jal	FlushWB			# Guarantee write occurs

 # Set rbaud for port B

	move	a0,zero			# argument to indicate rbaud rate
	jal	get_nvram_baud

	move	s1,v0			# save baud rate

	DUART0_BASE
	sb	v0,DOFF_CSRB(v0)		# (channel B St/Clk Sel Reg)
	jal	FlushWB			# Guarantee write occurs

	li	s4,SEL_MR1		# point to mr1
	sb	s4,DOFF_CRB(v0)		# (channel B CMD register)
	jal	FlushWB			# Guarantee write occurs

	li	s5,0x13			# (no parity, 8 bits/char)
	sb	s5,DOFF_MRB(v0)		# (MR1B)
	jal	FlushWB			# Guarantee write occurs

	beq	s1,b110,1f		# see if nvram was set to 110 baud

	li	s6,stop1		# (1 stop bit)
	b	2f
1:
	li	s6,stop2		# (2 stop bits if 110 baud)
2:
	sb	s6,DOFF_MRB(v0)		# (MR2B)
	jal	FlushWB			# Guarantee write occurs

	bne	gp,zero,1f		# test gp for error

#ifdef	DEBUG
	la	a0,success
	jal	pon_puts
#endif	DEBUG

	move	v0,zero			# indicate success
	b	2f
1:
	la	a0,failure
	jal	pon_puts

	li	a0,PON_DUART1A
	jal	FastFlash

	li	a0,PON_DUART1A
	jal	pon_set_leds

	li	a0,PON_FAULT_CONSOLE
	jal	SetDepend

	li	v0,1			# indicate failure
2:
	j	s0

END(Pon_Duart)

	.data

/*
 * Table for loading DUART control registers:
 *
 * In each entry of the table, the first byte is the offset of the register
 * from DUART_BASE and the second byte (low order byte) is the data to be
 * loaded into the register.
 */
	.align 1

EXPORT(duart_table)
	#
	#  Channel A Setup
	#
	.byte	DOFF_CRA, RES_RX	# cra - reset receiver
	.byte	DOFF_CRA, RES_TX	# cra - reset transmitter
	.byte	DOFF_CRA, RES_ERR	# cra - reset error status
	.byte	DOFF_CRA, RES_BRK	# cra - reset break interrupt
	.byte	DOFF_ACR, 0x80		# use baud rate set2
	.byte	DOFF_CRA, SEL_MR1	# cra - point to mr1
	.byte	DOFF_MRA, 0x13		# mr1a - no parity, 8 bits per char
	.byte	DOFF_MRA, 0x07		# mr2a - 1 stop bit
	.byte	DOFF_CRA, SEL_MR1	# cra - point to mode register 1
	.byte	DOFF_CSRA, b9600	# csra - RxC = 9600, TxC = 9600
	#
	#  Channel B Setup
	#
	.byte	DOFF_CRB, RES_RX	# crb - reset receiver
	.byte	DOFF_CRB, RES_TX	# crb - reset transmitter
	.byte	DOFF_CRB, RES_ERR	# crb - reset error status
	.byte	DOFF_CRB, RES_BRK	# crb - reset break interrupt
	.byte	DOFF_CRB, SEL_MR1	# crb - point to mr1
	.byte	DOFF_MRB, 0x13		# mr1b - no parity, 8 bits per char
	.byte	DOFF_MRB, 0x07		# mr2b - 1 stop bit
	.byte	DOFF_CRB, SEL_MR1	# crb - point to mode register 1
	.byte	DOFF_CSRB, b9600	# csrb - RxC = 9600, TxC = 9600
	#
	#  Interrupt Setup
	#
	.byte	DOFF_IMR, 0x00		# imr - Disable all interrupts
	#
	#  Output Port - Turn all bits on
	#
	.byte	DOFF_SOPBC, 0x03	# sopbc - Set output port bits command
	#
	# Turn on both channels
	#
	.byte	DOFF_OPCR, 0x00		# opcr - Complement of opr register
	.byte	DOFF_CRA, EN_TX | EN_RX	# cra - Enable transmitter and receiver
	.byte	DOFF_CRB, EN_TX | EN_RX	# crb - Enable transmitter and receiver
	.half	0xcdcd

failure:
	.asciiz "\n\rFAILED Duart 1, Channel A Test\r\n"

#ifdef	DEBUG
success:
	.asciiz "\n\rPASSED Duart 1, Channel A Test\r\n"
#endif	DEBUG
