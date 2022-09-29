#ident "$Header: pon_idprom.s,v 1.8.5.1 90/07/18 14:31:43 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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

#include "mips/asm.h"
#include "mips/regdef.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "pon.h"

#define	PROMSIZE		32


		.extern	success
		.extern	failure
		.extern	skipped
		.text

/*
 * Checks the checksum in the ID PROM.
 */
LEAF(Pon_IdProm)

		move	s4,ra			# save return address

#ifndef	R3030
		li	a0,PON_IDPROM
		jal	pon_set_leds
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

		li	s0,BRDTYPE_R2400
		beq	s0,sp,1f		# if M120

		li	s0,BRDTYPE_M180
		beq	s0,sp,1f		# if M180

		li	s0,BRDTYPE_R6300
#ifdef R6000_BUG_IDPROM
		beq	s0,sp,skip
#else
		beq	s0,sp,6f		# if R6000
#endif R6000_BUG_IDPROM

		li	s0,BRDTYPE_R3030
		beq	s0,sp,4f		# if M20

		li	v0,IDPROM_R2300|K1BASE+3
						# M-SERIES
		b	2f
6:
		li	v0,IDPROM_R6300|K1BASE+3-4
						# R6000 uses byte zero
		b	2f			#  for SBC init
1:
		li	v0,IDPROM_R2400|K1BASE+3# M120
		b	2f
4:
		li	v0,TODC_CLOCK_ADDR_R3030|K1BASE+3
2:
		li	s1,PROMSIZE-1
		move	s2,zero			# initial checksum
3:
		lbu	s3,(v0)			# get byte
		addu	s2,s3			# sum it

		addu	v0,4			# next byte location
		subu	s1,1			# decrement count
		bne	s1,zero,3b

		neg	s2,s2			# compute checksum byte
		and	s2,0xff

		lbu	s3,(v0)			# get checksum byte in ID PROM
		beq	s2,s3,pass

		la	a0,failure
		jal	pon_puts

#ifndef	R3030
		li	a0,PON_IDPROM
		jal	pon_set_leds
#endif	!R3030

		li	a0,PON_IDPROM
		jal	FastFlash

		li	a0,PON_FAULT_IDPROM
		jal	SetDepend

		li	v0,1
		j	s4
pass:
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s4

skip:
		la	a0,skipped
		jal	pon_puts

		move	v0,zero
		j	s4

END(Pon_IdProm)

		.data

begintest:
		.asciiz	"ID PROM Test..."
