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
/* $Header: execv.s,v 1.8.2.2 90/05/10 01:51:32 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>
#include "sys/syscall.h"

	.globl	environ

FRMSIZE	=	20

NESTED(execv, FRMSIZE, zero)
	subu	sp,FRMSIZE
	sw	ra,FRMSIZE-4(sp)
	lw	a2,environ
	jal	execve
	lw	ra,FRMSIZE-4(sp)
	addu	sp,FRMSIZE
	RET(execv)			# execv(file, argv)
