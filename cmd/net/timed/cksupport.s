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
/* $Header: cksupport.s,v 1.1.2.2 90/05/09 17:43:22 wje Exp $ */

#include <mips/regdef.h>
#include <mips/asm.h>


/*
 * in_checksum(addr, len, prevcksum)
 *
 * Calculates a 16 bit ones-complement checksum.
 * Note that for a big-endian machine, this routine always adds even
 * address bytes to the high order 8 bits of the 16 bit checksum and
 * odd address bytes are added to the low order 8 bits of the 16 bit checksum.
 * For little-endian machines, this routine always adds even address bytes
 * to the low order 8 bits of the 16 bit checksum and the odd address bytes
 * to the high order 8 bits of the 16 bit checksum.
 */
LEAF(in_checksum)
	move	v0,a2		# copy previous checksum
	beq	a1,zero,4f	# count exhausted
	and	v1,a0,1
	beq	v1,zero,2f	# already on a halfword boundry
	lbu	t8,0(a0)
	addu	a0,1
#ifdef MIPSEL
	sll	t8,8
#endif MIPSEL
	addu	v0,t8
	subu	a1,1
	b	2f

1:	lhu	t8,0(a0)
	addu	a0,2
	addu	v0,t8
	subu	a1,2
2:	bge	a1,2,1b
	beq	a1,zero,3f	# no trailing byte
	lbu	t8,0(a0)
#ifdef MIPSEB
	sll	t8,8
#endif MIPSEB
	addu	v0,t8
3:	srl	v1,v0,16	# add in all previous wrap around carries
	and	v0,0xffff
	addu	v0,v1
	srl	v1,v0,16	# wrap-arounds could cause carry, also
	addu	v0,v1
	and	v0,0xffff
4:	j	ra
	END(in_checksum)


/*
 * nuxi_s - byte swap short
 */
LEAF(nuxi_s)			# a0 = ??ab
	srl	v0,a0,8		# v0 = 0??a
	and	v0,0xff		# v0 = 000a
	sll	v1,a0,8		# v1 = ?ab0
	or	v0,v1		# v0 = ?aba
	and	v0,0xffff	# v0 = 00ba
	j	ra
	END(nuxi_s)

