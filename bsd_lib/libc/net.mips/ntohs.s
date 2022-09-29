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
/* $Header: ntohs.s,v 1.2.2.2 90/05/07 20:50:42 wje Exp $ */


/* hostorder = ntohs(netorder) */

#ifndef SYSTYPE_SYSV
#include <mips/regdef.h>
#include <mips/asm.h>
#else
#include <sys/regdef.h>
#include <sys/asm.h>
#endif

LEAF(ntohs)
#ifdef MIPSEL
	# start with ab
	sll	v0,a0,8			# ?ab0
	srl	v1,a0,8			# 0??a
	and	v1,0xff			# 000a
	or	v0,v1			# ??ba
	and	v0,0xffff		# 00ba
#else
	and	v0,a0,0xffff
#endif
	j	ra
.end ntohs
