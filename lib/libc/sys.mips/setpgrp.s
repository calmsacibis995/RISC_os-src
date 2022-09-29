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
/* $Header: setpgrp.s,v 1.8.2.2 90/05/09 19:57:32 wje Exp $ */

#include <sys/regdef.h>
#include <sys/asm.h>
#include <sys.s>
#include "sys/syscall.h"

LEAF(setpgrp)
	li	a0,1
	li	v0,SYS_setpgrp
	syscall
	bne	a3,zero,err
	RET(setpgrp)

LEAF(getpgrp)
	li	a0,0
	li	v0,SYS_setpgrp
	syscall
	bne	a3,zero,err
	RET(getpgrp)

err:
	j	_cerror
