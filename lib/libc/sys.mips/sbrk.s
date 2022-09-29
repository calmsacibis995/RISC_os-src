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
/* $Header: sbrk.s,v 1.9.1.2 90/05/09 19:57:10 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>
#include "sys/syscall.h"


	.globl	end
	.globl	_minbrk
	.globl	_curbrk

	.data
_minbrk:.word	end
_curbrk:.word	end
	.text

#ifdef	SYSTYPE_POSIX
LEAF(_sbrk)
#else	/* !SYSTYPE_POSIX */
LEAF(sbrk)
#endif	/* !SYSTYPE_POSIX */
	lw	v1,_curbrk
	addu	a0,v1
	li	v0,SYS_brk
	syscall
	bne	a3,zero,err
	move	v0,v1			# return previous curbrk
	sw	a0,_curbrk		# update to new curbrk
#ifdef	SYSTYPE_POSIX
	RET(_sbrk)
#else	/* !SYSTYPE_POSIX */
	RET(sbrk)
#endif	/* !SYSTYPE_POSIX */

err:
	j	_cerror


#ifndef	SYSTYPE_POSIX
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
	RET(brk)
#endif	/* !SYSTYPE_POSIX */
