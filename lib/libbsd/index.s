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
/* $Header: index.s,v 1.5.2.2 90/05/10 01:18:02 wje Exp $ */


#include <sys/asm.h>
#include <sys/regdef.h>

LEAF(index)
1:	lb	a2,0(a0)
	addu	a0,1
	beq	a2,a1,2f
	bne	a2,zero,1b
	move	v0,zero
	j	ra

2:	subu	v0,a0,1
	j	ra
.end	index
