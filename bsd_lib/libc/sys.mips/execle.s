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
/* $Header: execle.s,v 1.1.2.2 90/05/07 21:13:11 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>

FRMSIZE	=	20

NESTED(execle, FRMSIZE, zero)
	subu	sp,FRMSIZE
	sw	ra,FRMSIZE-4(sp)
	sw	a1,FRMSIZE+4(sp)
	sw	a2,FRMSIZE+8(sp)
	sw	a3,FRMSIZE+12(sp)
	move	a1,sp
	addu	a1,FRMSIZE+4
	move	a2,a1
1:	lw	v0,0(a2)
	addu	a2,4
	bne	v0,zero,1b
	lw	a2,0(a2)
	jal	execve
	lw	ra,FRMSIZE-4(sp)
	addu	sp,FRMSIZE
	RET		# execle(file, arg1, arg2, ..., 0, env);
.end execle
