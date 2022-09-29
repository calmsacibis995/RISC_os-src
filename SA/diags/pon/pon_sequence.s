#ident "$Header: pon_sequence.s,v 1.4.1.2 90/07/31 15:13:47 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/timer.h"
#include "machine/hd146818.h"
#include "machine/mk48t02.h"
#include "prom/prom.h"
#include "pon.h"

#define WBFLUSH	.set	noat; \
		lui	$at,0xbfc0; \
		lw	$0,($at); \
		.set	at; \
9:		bc0f	9b

	.extern	no_pon

LEAF(Pon_Init)

#ifdef	R3030
	move	s0,ra
	jal	ResetPonEnviron

	j	s0
#else	R3030
	j	ra
#endif	R3030

END(Pon_Init)


LEAF(Pon_Diags)

	jal	ResetDepend

#ifdef	R3030
	jal	Pon_Buzzer

	li	a0,PON_ENTERED
	jal	SetPonEnviron		# indicate that PON code has been entered
#endif	R3030

	la	fp,PonHandler
	li	v0,SR_BEV
	.set	noreorder
	mtc0	v0,C0_SR		# state unknown on reset
	mtc0	zero,C0_CAUSE		# clear software interrupts
	.set	reorder

	MACHINE_SPECIFIC
L_M2000: ; L_GENESIS: ; L_EXCALIBUR:
	jal	initmem			# initmem because of block refills
	b	1f
L_M120: ; L_M180:
	sh	zero,SCR|K1BASE		# clear SCR
	WBFLUSH

	li	a0,LANCE_BASE_R2400|K1BASE
	sh	zero,LANCE_RAP_OFFSET(a0)
	WBFLUSH

	li	a1,4
	sh	a1,LANCE_RDP_OFFSET(a0)	# stop the Lance chip
	WBFLUSH
	b	1f
L_M20:
	li	a0,SYS_CREG|K1BASE
	li	a1,CR_ENA_BUZZER_B
	sw	a1,(a0)			# clear system control register
	WBFLUSH

	li	a0,LANCE_BASE_R3030|K1BASE
	sh	zero,LANCE_RAP_OFFSET(a0)
					# select LANCE CSR0
	WBFLUSH

	li	a1,4
	sh	a1,LANCE_RDP_OFFSET(a0)	# stop LANCE
	WBFLUSH

	li	a0,0x84000000		# purge FIFO, disable channel,
					#   ... clear error
	sw	a0,DMA_MODE1|K1BASE	# disable RAMBO channel 1
	sw	a0,DMA_MODE2|K1BASE	# disable RAMBO channel 2
	b	1f
L_M500: ; L_M800: ; L_M1000:
	/* nothing special */
1:
	jal	get_machine_type
	move	sp,v0			# save machine type in SP for PON
					#   assembly routines
	move	k1,zero
        move    v0,zero

#if	!R3030
	jal	Pon_Leds
#endif  !R3030

#if     (!R3030 && !RB3125)
	jal	Pon_Duart
	or	k1,v0
#endif  (!R3030 && !RB3125)

#if     RB3125
        jal     Pon_Scc
        or      k1,v0
#endif  RB3125

#if	R3030
	jal	Pon_KbdST
	or	k1,v0

	jal	Pon_KbdBAT

	jal	Pon_Scc

	jal	Pon_Ttys
#endif	R3030

	jal	Pon_Banner

	jal	Pon_LoMem
	or	k1,v0

	jal	Pon_Cache1
	or	k1,v0

	jal	Pon_Cache2
	or	k1,v0

	jal	Pon_Cache3
	or	k1,v0

	jal	Pon_Cache4
	or	k1,v0

	jal	Pon_Dblk
	or	k1,v0

	jal	Pon_Iblk
	or	k1,v0

	jal	reset_cache		# invalidate entries

	jal	Pon_IdProm
	or	k1,v0

	jal	Pon_WB
	or	k1,v0

	jal	reset_cache		# invalidate entries

	la	fp,PonHandler
	li	v0,SR_BEV
	.set	noreorder
	mtc0	v0,C0_SR		# state unknown on reset
	mtc0	zero,C0_CAUSE		# clear software interrupts
	.set	reorder
	jal	Pon_Memory
	or	k1,v0

	beq	k1,zero,2f

	NVRAM(NVADDR_BOOTMODE)
	beq	v0,CHAR_d,2f		# diag mode set, don't change bootmode

	SETNVRAM(CHAR_e, NVADDR_BOOTMODE)
	WBFLUSH
2:
/*
 * Clear memory.
 */
	jal	GetDepend

	and	k0,v0,PON_FAULT_MEM
	beq	k0,zero,2f		# don't need to clear memory

#if    	!R3030
	li	a0,ZEROMEM_PATTERN
	jal	pon_set_leds
#endif	!R3030

	jal	SizeMemory
#ifdef SABLE
	li	v0,64*1024		# clear only first 64KB
#endif SABLE
	move	s0,v0			# save the Memory size...

	jal	GetDepend		# (k0 may be trashed -- do this again)
	li	a0,K1BASE
	or	a1,a0,s0		# ending address
	and	k0,v0,PON_FAULT_CACHE
	la	v1,2f
	bne	k0,zero,1f		# don't run cached I-fetches if had
					#  problems with caches
#if    	!R3030
	bne	sp,BRDTYPE_R6300,6f	# not R6000, so do cached i-fetches

	la	a0,1f			# for R6000, stuff Prom instructions
	la	a1,2f			#  into s-cache so cached
	jal	copy_r6000_to_cacheable	#   i-fetches will hit

	jal	SizeMemory		# recompute ending address again
#ifdef SABLE
	li	v0,64*1024		# clear only first 64KB
#endif SABLE
	li	a0,K1BASE		#  because copy_*() trashed regs
	or	a1,a0,v0
	la	v1,2f
	la	v0,1f-K0SIZE		# run cached I-fetches
	j	v0
6:
#endif	!R3030
	la	v0,1f
	la	v1,2f
	.set	noreorder
	mfc0	t0,C0_SR		# get status register
	li	t1,SR_SWC		# swap cache bit
	or	t1,t0
	mtc0	t1,C0_SR		# swap caches
	li	t1,K0SIZE		# size of cached addresses
	move	t2,a0			# get starting memory address
	subu	t2,t1			# convert to cached address
	move	t6,t2			# save address
	move	t3,v0			# get starting PROM address
5:
	lw	t4,(t3)			# get word from PROM
	addu	t3,4			# bump PROM pointer
	sw	t4,(t2)			# store in memory
	lw	t5,(t2)			# read from memory
	addu	t2,4			# bump memory pointer
	bne	t4,t5,4f		# compare failed - don't do cached
	bltu	t3,v1,5b		# do all prom words
	nop
	move	v0,t6			# get instruction address
4:
	mtc0	t0,C0_SR		# un-swap caches
	nop
	nop
	.set	reorder
	j	v0
1:
	sw	zero,(a0)
	sw	zero,4(a0)
	sw	zero,8(a0)
	sw	zero,12(a0)
	addu	a0,16
	bltu	a0,a1,1b

	j	v1
2:
	WBFLUSH

	jal	initmem
	jal	get_machine_type
	sw	v0,machine_type		# set machine type for PON
	move	k1,zero

	la	fp,PonHandler
	.set	noreorder
	li	a0,SR_BEV
	mtc0	a0,C0_SR
	.set	reorder

#if     (!R3030 && !R3200 && !RB3125)
	jal	Pon_Scr
	or	k1,v0
#endif  (!R3030 && !R3200 && !RB3125)

#if     (R3030 || RB3125)

	jal	Pon_SccTest
	or	k1,v0

#endif  (R3030 || RB3125)

#if     R3030
	jal	Pon_Timers
	or	k1,v0

	jal	Pon_Cfb
	or	k1,v0

	jal	Pon_Fdc
	or	k1,v0

	jal	Pon_Dma
	or	k1,v0

	jal	Pon_Ncr94
	or	k1,v0
#endif	R3030

	.set	noreorder
	mtc0	zero,C0_SR
	.set	reorder

	jal	config_cache		# determine cache sizes for flush
	jal	flush_cache
	jal	_hook_exceptions
	jal	Pon_VM
	or	k1,v0

	jal	config_cache		# determine cache sizes for flush
	jal	flush_cache
	jal	Pon_Allexc
	or	k1,v0

	jal	_hook_exceptions

#if     (!R3200 && !RB3125)
	jal	Pon_Parity
	or	k1,v0
#endif  (!3200 && RB3125)

#if	R3030
	jal	Pon_DmaParity
	or	k1,v0
#else	R3030
	jal	Pon_Vme
	or	k1,v0

	jal	Pon_Ecc
	or	k1,v0
#endif	R3030

	la	fp,PonHandler
	.set	noreorder
	li	a0,SR_BEV
	mtc0	a0,C0_SR
	.set	reorder

	jal	Pon_NVram
	or	k1,v0

#if    	!R3030
        jal     Pon_Timers
        or      k1,v0

	jal	Pon_Duarts
	or	k1,v0

#if     (!R3200 && !RB3125)
	jal	Pon_Imr
	or	k1,v0
#endif  (!R3200 && !RB3125)
#else	!R3030
	jal	Pon_KbdTest
	or	k1,v0
#endif	!R3030

	jal	Pon_Fp1
	or	k1,v0

	jal	Pon_Fp2
	or	k1,v0

#if     RB3125
        jal     Pon_Dma_Scsi
        or      k1,v0
#endif  RB3125

#if    	!R3030
#if     (!R3200 && !RB3125)
	jal	Pon_UdcSlave
	or	k1,v0

	jal	Pon_Chain1
	or	k1,v0

	jal	Pon_Chain2
	or	k1,v0

	jal	Pon_ScsiSlave
	or	k1,v0

	jal	Pon_ScsiMaster
	or	k1,v0

	jal	Pon_EnetProm
	or	k1,v0
#endif  (!R3200 && !RB3125)
#endif	!R3030

#if     RB3125
        jal     Pon_Lance_Buffer
        or      k1,v0

        jal     _hook_exceptions
        jal     Pon_Buffer_Parity
        or      k1,v0
#endif

	jal	Pon_LanceSlave
	or	k1,v0

	jal	Pon_LanceMaster
	or	k1,v0

#if     (!R3030 && !R3200 && !RB3125)
	jal	Pon_Atreg
	or	k1,v0
#endif  (!R3030 && !R3200 && !RB3125)

#if	R3030
	.set	noreorder
	mtc0	zero,C0_SR
	.set	reorder
	jal	config_cache		# determine cache sizes for flush
	jal	flush_cache
	jal	_hook_exceptions
	jal	Pon_AtDigi
	or	k1,v0
#endif	R3030

	and	k1,PON_DEPEND_MASK
					# mask only test bits
	beq	k1,zero,1f

	NVRAM(NVADDR_BOOTMODE)
	beq	v0,CHAR_d,1f		# diag mode set, don't change bootmode

XLEAF(PonError)

	SETNVRAM(CHAR_e, NVADDR_BOOTMODE)
1:
	j	no_pon

END(Pon_Diags)
