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
/* $Header: abort.s,v 1.2.2.3 90/05/07 20:28:56 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>

/* C library -- abort(3)
 *
 * Abort(3) is to execute an instruction which is illegal in user mode
 * and resulting in the process getting a SIGILL.
 */
LEAF(abort)
	.set noreorder
	c0	0	# should be illegal to do in user mode
	nop
	.set reorder
	li	v0,0
	RET
END(abort)
