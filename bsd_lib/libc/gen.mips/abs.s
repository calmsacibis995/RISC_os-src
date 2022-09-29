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
/* $Header: abs.s,v 1.1.2.4 90/05/10 20:11:58 wje Exp $ */
#include <regdef.h>

/* abs - absolute value */

/* The BSD abs() definition requires that if the
 * base is 2's complement and the argument is the
 * largest negative number (i.e. has no non-negative representation).
 * then the returnd result is the same bit pattern as the argument.
 *
 * This is more restrictive than the Sys-V definition which states that
 * the result is undefined and may or may not cause an error.
 * The natural implementation, with the "abs" assembler op,
 * produces an error for MIPS.
 * In this case, subtract unsigned to avoid the overflow trap.
 */

.globl abs
.ent abs
abs:
	.frame sp,0,ra
	move	v0,a0		# move into result reg
	bgez	a0,1f		# >=0 , just use result in v0
	subu	v0,zero,a0	# unsigned so 0x80000000 doesn't trap
1:
	# result is in v0 through a move or a subtract
	j ra
.end abs
