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
/* $Header: remque.s,v 1.2.2.2 90/05/07 20:32:15 wje Exp $ */


#ifndef SYSTYPE_SYSV
#include <mips/regdef.h>
#include <mips/asm.h>
#else
#include <sys/regdef.h>
#include <sys/asm.h>
#endif

/* remque(entry) */

LEAF(remque)
	lw	v0, 0(a0)	# ep->link
	lw	v1, 4(a0)	# ep->rlink
	sw	v0, 0(v1)	# ep->rlink->link = ep->link
	sw	v1, 4(v0)	# ep->link->rlink = ep->rlink
	j	ra
.end	remque
