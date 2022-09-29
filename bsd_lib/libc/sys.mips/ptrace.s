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
/* $Header: ptrace.s,v 1.1.2.2 90/05/07 21:21:43 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>

.extern	errno 4

LEAF(ptrace)
	sw	zero,errno
	li	v0,SYS_ptrace
	syscall
	bne	a3,zero,err
	RET

err:
	j	_cerror
.end ptrace
