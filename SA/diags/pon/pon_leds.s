#ident "$Header: pon_leds.s,v 1.6.1.1 90/07/18 14:32:05 huang Exp $"
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

#include "mips/dregdef.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "mips/standard.h"
#include "mips/delaymacs.h"
#include "mips/asm.h"
#include "pon.h"

#ifdef SABLE
#define WAIT		0
#else
#define	WAIT		0x30000
#endif !SABLE
#define DELAY \
		.set	noreorder; \
		.set	noat; \
		li	AT,WAIT; \
99:; \
		bne	AT,zero,99b; \
		subu	AT,1; \
		.set	at; \
		.set	reorder


		.text

/*
 * Just writes a walking 1's pattern to the LED register for a visual check.
 */
LEAF(Pon_Leds)

		move	s6,ra

		li	s0,BRDTYPE_R3030
		beq	s0,sp,4f		# no LEDs on the M20

		li	s0,BRDTYPE_R2400
		beq	s0,sp,1f		# if INTREPID

		li	s0,BRDTYPE_R6300
		beq	s0,sp,3f

		li	v0,LED_REG_R2300|K1BASE
		li	s1,6			# 6 bits
		li	s2,0x3f			# mask
		b	2f
1:
		li	v0,LED_REG_R2400|K1BASE
		li	s1,8			# 8 bits
		li	s2,0xff			# mask
		b	2f
3:
		li	v0,LED_REG_R6300|K1BASE
		li	s1,6			# 6 bits
		li	s2,0x3f			# mask
2:
		sb	s2,(v0)
		jal	FlushWB

		DELAY

		move	s3,s1
		li	v1,1			# starting pattern
3:
		not	s4,v1
		and	s4,s2
		sb	s4,(v0)
		jal	FlushWB

		DELAY

		sll	v1,1
		subu	s3,1
		bne	s3,zero,3b

		sb	s2,(v0)
		jal	FlushWB

		DELAY
4:
		move	v0,zero			# always passes
		j	s6

END(Pon_Leds)
