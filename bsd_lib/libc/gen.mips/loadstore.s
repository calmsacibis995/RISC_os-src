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
/* $Header: loadstore.s,v 1.1.2.2 90/05/07 20:32:03 wje Exp $ */
#include "regdef.h"
#include "asm.h"

LEAF(unaligned_load_word)
XLEAF(unaligned_load_word_)
	ulw	v0, 0(a0)
	j	ra
END(unaligned_load_word)

LEAF(unaligned_load_half)
XLEAF(unaligned_load_half_)
	ulh	v0, 0(a0)
	j	ra
END(unaligned_load_half)

LEAF(unaligned_load_uhalf)
XLEAF(unaligned_load_uhalf_)
	ulhu	v0, 0(a0)
	j	ra
END(unaligned_load_uhalf)

LEAF(unaligned_load_float)
XLEAF(unaligned_load_float_)
	ulw	v0, 0(a0)
	mtc1	v0, $f0
	j	ra
END(unaligned_load_float)

LEAF(unaligned_load_double)
XLEAF(unaligned_load_double_)
	ulw	v0, 0(a0)
	ulw	v1, 4(a0)
	mtc1	v0, $f0
	mtc1	v1, $f1
	j	ra
END(unaligned_load_double)

LEAF(unaligned_store_word)
XLEAF(unaligned_store_word_)
	usw	a1, 0(a0)
	j	ra
END(unaligned_store_word)

LEAF(unaligned_store_half)
XLEAF(unaligned_store_half_)
	ush	a1, 0(a0)
	j	ra
END(unaligned_store_half)

LEAF(unaligned_store_float)
XLEAF(unaligned_store_float_)
	usw	a1, 0(a0)
	j	ra
END(unaligned_store_float)

LEAF(unaligned_store_double)
XLEAF(unaligned_store_double_)
	usw	a2, 0(a0)
	usw	a3, 4(a0)
	j	ra
END(unaligned_store_double)
