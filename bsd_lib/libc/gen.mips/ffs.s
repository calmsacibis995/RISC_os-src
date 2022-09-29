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
/* $Header: ffs.s,v 1.2.1.2 90/05/07 20:30:45 wje Exp $ */

#ifdef SYSTYPE_BSD43
#include <mips/regdef.h>
#include <mips/asm.h>
#endif
#ifdef SYSTYPE_SYSV
#include <sys/regdef.h>
#include <sys/asm.h>
#endif

/*
 * ffs(word)
 * find first bit set in word (a la VAX instruction)
 * looks at low order bits first, lowest order bit is 1, highest bit is 32
 * no bits returns 0
 */
LEAF(ffs)
	.set	noreorder
	move	v0,zero
	beq	a0,zero,2f		# no bits set, return zero
1:	and	v1,a0,1
	addu	v0,1
	beq	v1,zero,1b
	srl	a0,1			# BDSLOT: shift right to next bit
2:	j	ra
	nop
	.set	reorder
.end ffs

#ifdef notdef
LEAF(ffs)
	move	v1,zero			# initial table offset
	and	v0,a0,0xffff		# check lower halfword
	bne	v0,zero,1f		# bits in lower halfword
	addu	v1,64			# table offset for halfword
	srl	a0,16			# check upper halfword
1:	and	v0,a0,0xff		# check lower byte of halfword
	bne	v0,zero,2f		# bits in lower byte
	addu	v1,32			# table offset for byte
	srl	a0,8			# check upper byte of halfword
2:	and	v0,a0,0xf		# check lower nibble
	bne	v0,zero,3f		# bits in lower nibble
	addu	v1,16			# table offset for nibble
	srl	v0,a0,4			# check upper nibble
	and	v0,0xf
3:	addu	v1,v0			# total table offset
	lbu	v0,ffstbl(v1)		# load bit number from table
	j	ra
.end ffs

	.data
#define NIBBLE(x) \
	.byte	0,       1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	3+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	4+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4; \
	.byte	3+(x)*4, 1+(x)*4, 2+(x)*4, 1+(x)*4
ffstbl:
	NIBBLE(0)
	NIBBLE(1)
	NIBBLE(2)
	NIBBLE(3)
	NIBBLE(4)
	NIBBLE(5)
	NIBBLE(6)
	NIBBLE(7)
#endif notdef
