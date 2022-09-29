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
/* $Header: flock.s,v 1.3.1.2 90/05/10 01:14:23 wje Exp $ */


#include <sys/regdef.h>
#define SYSTYPE_BSD43 1
#include <bsd43/mips/asm.h>
#include <bsd43/syscall.h>
#undef SYSTYPE_BSD43

SYSCALL(flock)
	RET
.end flock
