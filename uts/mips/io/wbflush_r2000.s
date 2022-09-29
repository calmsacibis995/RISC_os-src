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
/* $Header: wbflush_r2000.s,v 1.2.3.2 90/05/10 05:37:53 wje Exp $ */

#include "sys/regdef.h"
#include "sys/asm.h"

LEAF(wbflush)
	.set	noreorder
	nop;nop;nop;nop
	.set	reorder
1:
	bc0f	1b
	j	ra
	END(wbflush)

