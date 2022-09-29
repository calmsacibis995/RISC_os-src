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
/* $Header: insque.s,v 1.2.2.2 90/05/07 20:31:52 wje Exp $ */


#ifndef SYSTYPE_SYSV
#include <mips/regdef.h>
#include <mips/asm.h>
#else
#include <sys/regdef.h>
#include <sys/asm.h>
#endif

/*
 * insque(ep, pp)
 * ep is new element, pp is predecessor
 */
LEAF(insque)
	lw	v0, 0(a1)	# pp->link
	sw	v0, 0(a0)	# ep->link = pp->link
	sw	a1, 4(a0)	# ep->rlink = pp
	sw	a0, 4(v0)	# pp->link->rlink = ep
	sw	a0, 0(a1)	# pp->link = ep
	j	ra
.end	insque
