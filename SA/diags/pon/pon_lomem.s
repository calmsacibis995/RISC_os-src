#ident "$Header: pon_lomem.s,v 1.10.1.1 90/07/18 14:32:07 huang Exp $"
/* $Copyright
# |-----------------------------------------------------------|
# | Copyright (c) 1989, 1990 MIPS Computer Systems, Inc.      |
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
#include "machine/cpu_board.h"
#include "machine/cp0.h"
#include "machine/param.h"
#include "pon.h"
#include "prom.h"

#undef	TEST
#undef	DEBUG

#define	INCR		0x100000
#define FIVES		0x55555555
#define	ACES		0xaaaaaaaa

		.extern	crlf

#define PRINT(x, y, z) \
		la	a0,failure; \
		jal	pon_puts; \
		nop; \
		la	a0,paddress; \
		jal	pon_puts; \
		nop; \
		move	a0,x; \
		jal	pon_puthex; \
		nop; \
		la	a0,pexpect; \
		jal	pon_puts; \
		nop; \
		move	a0,y; \
		jal	pon_puthex; \
		nop; \
		la	a0,pactual; \
		jal	pon_puts; \
		nop; \
		move	a0,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,pxor; \
		jal	pon_puts; \
		nop; \
		xor	a0,y,z; \
		jal	pon_puthex; \
		nop; \
		la	a0,crlf; \
		jal	pon_puts; \
		nop

#undef	DELAY
#define	DELAY(x) \
		li	a2,+(x); \
1: ; \
		subu	a2,1; \
		bne	zero,a2,1b

		.extern failure
		.extern success
		.text

LEAF(Pon_LoMem)

		move	s7,ra			# save return address

#ifndef	R3030
		li	a0,PON_LOMEM		# just begun
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030

		la	a0,begintest
		jal	pon_puts

	/*
	 * The chicken and egg problem with write buffers and memory.  Can't
	 * test one without the other working.
	 */

		jal	SizeMemory

		bne	v0,zero,3f		# found memory

		la	a0,nomem
		jal	pon_puts

#ifndef	R3030
		li	a0,LMEM_NOMEM_PATTERN	# setup error code
		sb	a0,0(v0)		# and write the diag location
#endif	!R3030
2:
		li	a0,LMEM_NOMEM_PATTERN
		jal	FastFlash

		b	2b			# and just loop forever
3:
		li	s5,K1BASE		# uncached starting address
		li	s6,K1BASE + PON_LOMEMEND

#ifdef	DEBUG
		la	a0,memstart_msg		# display start address message
		jal	pon_puts

		move	a0,s5
		jal	pon_puthex		# display start address

		la	a0,memend_msg		# display end address message
		jal	pon_puts

		move	a0,s6			# end address found
		jal	pon_puthex		# display end address

		la	a0,crlf
		jal	pon_puts
#endif	DEBUG

		li	a0,FIVES
		li	a1,ACES
		move	s4,s5
1:
		sw	a0,0(s4)		#write fives from start to end
		add	s4,4
		bne	s4,s6,1b
		sw	a0,0(s4)
		move	s4,s5
2:
		lw	s1,0(s4)
		bne	a0,s1,error
		sw	a1,0(s4)
		add	s4,4
		bne	s4,s6,2b
		lw	s1,0(s4)
		bne	a0,s1,error
		sw	a1,0(s4)
		move	s4,s5
3:
		lw	s1,0(s4)
		bne	s1,a1,error
		add	s4,4
		bne	s4,s6,3b
 # Successful!
		la	a0,success
		jal	pon_puts

		move	v0,zero
		j	s7
 # Failed!
error:
                PRINT(s4, s3, s2)               # print address, expected,
                                                #   actual, and Xor
		li	a0,PON_LOMEM
		jal	FastFlash

#ifndef	R3030
		li	a0,PON_LOMEM
		jal	pon_set_leds		# write to the CPU LEDS
#endif	!R3030

		li	a0,PON_FAULT_MEM
		jal	SetDepend

		li	v0,1
		j	s7

END(Pon_LoMem)

		.data

begintest:
		.asciiz "Low Memory Test..."

#ifdef	DEBUG
memstart_msg:
		.asciiz "Start Address: "
memend_msg:
		.asciiz " End Address: "
#endif	DEBUG
