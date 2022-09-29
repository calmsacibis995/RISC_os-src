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
/* $Header: strlen.s,v 1.6.2.2 90/05/10 01:27:09 wje Exp $ */

#include <sys/asm.h>
#include <sys/regdef.h>

LEAF(strlen)
	subu	v0,a0,1
1:	lbu	v1,1(v0)
	add	v0,1
	bne	v1,zero,1b
	subu	v0,v0,a0
	j	ra
	.end	strlen
