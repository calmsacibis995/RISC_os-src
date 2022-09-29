#ident "$Header: pon_banner_3030.s,v 1.2.1.1 90/07/18 14:29:27 huang Exp $"
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

#include "machine/asm.h"
#include "machine/regdef.h"
#include "pon.h"


		.text

 # The duart console test has successfully completed!, so tell the world
 # about it!

LEAF(Pon_Banner)

		move	s0,ra			# save our return address
		la	a0,starting
		jal	pon_puts

		jal	GetPonEnviron

		and	v0,PON_KBD_NOT_PRESENT
		beq	v0,zero,1f

		la	a0,kbdwarn
		jal	pon_puts
1:
		move	v0,zero
		j	s0

END(Pon_Banner)

		.data
starting:
		.asciiz "\r\n\nRunning Power-On Diagnostics...\r\n"
kbdwarn:
		.asciiz "*** Warning: Keyboard May Not Be Connected ***\r\n"
