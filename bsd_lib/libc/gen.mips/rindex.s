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
/* $Header: rindex.s,v 1.1.2.2 90/05/07 20:32:20 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>

LEAF(rindex)
	move	v0,zero
1:	lb	a3,0(a0)
	addu	a0,1
	bne	a3,a1,2f
	subu	v0,a0,1
2:	bne	a3,zero,1b
	j	ra
.end	rindex
