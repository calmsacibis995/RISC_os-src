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
/* $Header: htonl.s,v 1.2.2.2 90/05/07 20:50:25 wje Exp $ */


/*
 * htonl -- host to network byte order for longs
 */

#ifndef SYSTYPE_SYSV
#include <mips/regdef.h>
#include <mips/asm.h>
#else
#include <sys/regdef.h>
#include <sys/asm.h>
#endif

LEAF(htonl)
#ifdef MIPSEL
	# start with abcd
	sll	v0,a0,24	# d000
	srl	v1,a0,24	# 000a
	or	v0,v1		# d00a
	srl	t0,a0,8		# 0abc
	and	t1,t0,0xff00	# 00b0
	and	v1,t0,0xff	# 000c
	sll	v1,16		# 0c00
	or	v1,t1		# 0cb0
	or	v0,v1		# dcba
#else
	move	v0,a0
#endif
	j	ra
.end htonl
