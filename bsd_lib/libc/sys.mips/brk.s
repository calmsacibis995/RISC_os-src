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
/* $Header: brk.s,v 1.1.2.2 90/05/07 21:11:27 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>

	.globl	_curbrk
	.globl	_minbrk

LEAF(_brk)
	b	1f
.end _brk

LEAF(brk)
	lw	v0,_minbrk
	bgeu	a0,v0,1f
	move	a0,v0
1:
	li	v0,SYS_brk
	syscall
	bne	a3,zero,err
	sw	a0,_curbrk
	move	v0,zero
	RET

err:
	j	_cerror
.end brk
