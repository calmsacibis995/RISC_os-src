#ident "$Header: pon_banner.s,v 1.4.7.1 90/07/18 14:29:24 huang Exp $"
/* $Copyright$ */

#include "machine/asm.h"
#include "machine/regdef.h"


		.text

 # The duart console test has successfully completed!, so tell the world
 # about it!

LEAF(Pon_Banner)

		move	s0,ra			# save our return address
		la	a0,starting
		jal	pon_puts

		move	v0,zero
		j	s0

END(Pon_Banner)

		.data
starting:
		.asciiz "\r\n\nRunning Power-On Diagnostics...\r\n"
