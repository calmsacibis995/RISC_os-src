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
/* $Header: execl.s,v 1.1.2.2 90/05/07 21:13:05 wje Exp $ */


#include <mips/regdef.h>
#include <mips/asm.h>
#include <syscall.h>

FRMSIZE	=	20

NESTED(execl, FRMSIZE, zero)
	subu	sp,FRMSIZE
	sw	ra,FRMSIZE-4(sp)
	sw	a1,FRMSIZE+4(sp)
	sw	a2,FRMSIZE+8(sp)
	sw	a3,FRMSIZE+12(sp)
	move	a1,sp
	addu	a1,FRMSIZE+4
	jal	execv
	lw	ra,FRMSIZE-4(sp)
	addu	sp,FRMSIZE
	RET		# execl(file, arg1, arg2, ..., 0);
.end execl
