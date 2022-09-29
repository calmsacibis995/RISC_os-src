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
/* $Header: sbrk.s,v 1.1.2.2 90/05/07 21:23:13 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>

	.globl	end
	.globl	_minbrk
	.globl	_curbrk

.sdata
_minbrk:.word	end
_curbrk:.word	end

.text

LEAF(sbrk)
	lw	v1,_curbrk
	addu	a0,v1
	li	v0,SYS_brk
	syscall
	bne	a3,zero,err
	move	v0,v1			# return previous curbrk
	sw	a0,_curbrk		# update to new curbrk
	RET

err:
	j	_cerror
.end sbrk
