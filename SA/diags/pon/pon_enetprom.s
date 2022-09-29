#ident "$Header: pon_enetprom.s,v 1.4.7.1 90/07/18 14:31:07 huang Exp $"
/* $Copyright$ */

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
 * Checks the checksum in the Ethernet PROM.  Intrepid Only.
 */
LEAF(Pon_EnetProm)

		move	s4,ra			# save return address

		lw	s0,machine_type
		bne	s0,BRDTYPE_R2400,skipit	# if not INTREPID skip

		li	a0,PON_ENETPROM
		jal	pon_set_leds

		la	a0,begintest
		jal	pon_puts

		li	v0,ENETPROM_BASE|K1BASE+3
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

		li	a0,PON_ENETPROM
		jal	pon_set_leds

		li	a0,PON_ENETPROM
		jal	FastFlash

		li	a0,PON_FAULT_IDPROM
		jal	SetDepend

		li	v0,1
		j	s4
pass:
		la	a0,success
		jal	pon_puts
skipit:

		move	v0,zero
		j	s4

END(Pon_EnetProm)

		.data

begintest:
		.asciiz	"Ethernet ID PROM Test..."
