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
/* $Header: hwconf.s,v 1.1.2.2 90/05/07 21:18:23 wje Exp $ */
#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>
#include <sysmips.h>

/*
 * hwconf is implemented via the SYS_mips system call. The 2 arguments are
 * shuffled back so the MIPS_HWCONF vector can be put in the first argument.
 * The SYS_mips system call takes 5 arguments and unused arguments must
 * have zero values.
 */
LEAF(hwconf)
	subu	sp,8
	sw	zero,16(sp)
	move	a3,zero
	move	a2,a1
	move	a1,a0
	li	a0,MIPS_HWCONF
	li	v0,SYS_sysmips
	syscall
	addu	sp,8
	beq	a3,zero,9f
	j	_cerror
9:
	RET
.end hwconf
