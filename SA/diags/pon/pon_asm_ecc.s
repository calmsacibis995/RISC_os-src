#ident "$Header: pon_asm_ecc.s,v 1.10.1.1 90/07/18 14:29:11 huang Exp $"
/* $Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restrictive Rights Legend                        |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#  $ */

/********************************************************
 *							*
 *			ECCasm.s			*
 *							*
 ********************************************************/
#define	LOCORE
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "machine/mem_board.h"
#include "pon_ecc.h"
/*
 *	Externals.
 */
	.extern	ECCpon
	.extern machine_type
	.extern skipped
	.globl	Pon_Ecc

/*
 *	Invalidate word:
 *
 *	  The cache entry mapping to the passed address is invalidated. Only
 *	't0' is modified.
 */
 	.globl	InvalidateWord
LEAF(InvalidateWord)
	.set noreorder
	mfc0	t0,C0_SR		/* isolate the cache */
	nop
	.set reorder
	.set	noat
	li	$at,SR_ISC
	or	$at,t0
	.set noreorder
	mtc0	$at,C0_SR
	nop
	.set	at
	nop
	sh	$0,0(a0)		/* invalidate entry */
	nop
	mtc0	t0,C0_SR		/* reenable the cache */
	nop
	.set reorder
	j	ra
	END(InvalidateWord)
/*
 *	Clear block:
 *
 *	  This clears "a1" number of bytes starting at "a0". The byte
 *	count is rounded to the nearest data block, the address is block
 *	aligned and uncached. The loop is set up for that writes should
 *	stream.
 */
	.globl	ClearBlock
	LEAF(ClearBlock)
	/*
	 *	Clean up word count and address.
	 */
	addiu	a1,DBLOCKSIZE * 4 - 1
	srl	a1,LOG_DBLOCK
	.set	noat
	li	$at,0xa0000000
	or	a0,$at
	.set	at
	srl	a0,LOG_DBLOCK
	sll	a0,LOG_DBLOCK
	/*
	 *	Now generate a stream of writes.
	 */
1:
#if	(DBLOCKSIZE >= 1)
	sw	$0,0(a0)
#endif	(DBLOCKSIZE >= 1)
#if	(DBLOCKSIZE >= 2)
	sw	$0,4(a0)
#endif	(DBLOCKSIZE >= 2)
#if	(DBLOCKSIZE >= 4)
	sw	$0,8(a0)
	sw	$0,12(a0)
#endif	(DBLOCKSIZE >= 4)
#if	(DBLOCKSIZE >= 8)
	sw	$0,16(a0)
	sw	$0,20(a0)
	sw	$0,24(a0)
	sw	$0,28(a0)
#endif	(DBLOCKSIZE >= 8)
#if	(DBLOCKSIZE >= 16)
	sw	$0,32(a0)
	sw	$0,36(a0)
	sw	$0,40(a0)
	sw	$0,44(a0)
	sw	$0,48(a0)
	sw	$0,52(a0)
	sw	$0,56(a0)
	sw	$0,60(a0)
#endif	(DBLOCKSIZE >= 16)
	addiu	a1,-1
	addiu	a0,DBLOCKSIZE * 4
	bne	a1,$0,1b
	j	ra
	END(ClearBlock)

LEAF(Pon_Ecc)
	sub	sp,4
	sw	ra,(sp)
	lw	v0,machine_type
	beq	v0,BRDTYPE_M180,1f
	la	a0,ECCPon
	and	a0,0x9fffffff
	j	a0
	b	2f
1:
	move	v0,zero
2:
	lw	ra,(sp)
	add	sp,4
	j	ra
END(Pon_Ecc)
