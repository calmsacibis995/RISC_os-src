#ident "$Header: pon_subr_2000.s,v 1.4.1.1 90/07/18 14:33:15 huang Exp $"
/* $Copyright$ */

#include "machine/asm.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "machine/cpu_board.h"
#include "machine/cpu.h"
#include "machine/cp0.h"

#define	REFIL_MSK	0xfffffff0
/*
 * Put cache related state to known values clear PE,
 *  insure all cache lines have correct parity.
 */
LEAF(init_cache)
XLEAF(reset_cache)

	.set noreorder
		mtc0	zero,C0_CAUSE

		/*
		 * Clear I and D-caches to known state.
		 */
		li	v0,SR_ISC|SR_BEV
		mtc0	v0,C0_SR
	.set reorder
		li	v0,MAXCACHE
1:
		sw	zero,K0BASE(v0)
		subu	v0,4
		bgez	v0,1b

		nop
		nop
		li	v0,SR_ISC|SR_SWC|SR_BEV
	.set noreorder
		mtc0	v0,C0_SR
	.set reorder
		nop
		nop
		li	v0,MAXCACHE
1:
		sw	zero,K0BASE(v0)
		subu	v0,4
		bgez	v0,1b

		nop
		nop
		li	v0,SR_PE|SR_BEV
	.set noreorder
		mtc0	v0,C0_SR		# clear PE bit and swap back
	.set reorder
		j	ra
	END(init_cache)


LEAF(invalidate_cache)
	.set noreorder
		mtc0	zero,C0_CAUSE

		/*
		 * Clear I and D-caches to known state.
		 */
		li	v0,SR_ISC|SR_BEV
		mtc0	v0,C0_SR
	.set reorder
		li	v0,MAXCACHE
1:
		sb	zero,K0BASE(v0)
		subu	v0,4
		bgez	v0,1b

		nop
		nop
		li	v0,SR_ISC|SR_SWC|SR_BEV
	.set noreorder
		mtc0	v0,C0_SR
	.set reorder
		nop
		nop
		li	v0,MAXCACHE
1:
		sb	zero,K0BASE(v0)
		subu	v0,4
		bgez	v0,1b

		nop
		nop
		li	v0,SR_PE|SR_BEV
	.set noreorder
		mtc0	v0,C0_SR		# clear PE bit and swap back
	.set reorder
		j	ra
	END(invalidate_cache)


/*
 *  flushall_tlb()
 *	Flush all tlb entries.
 */
LEAF(flushall_tlb)
	.set noreorder
	mtc0	zero,C0_TLBLO		# set to invalid
	li	v0,K0BASE
	mtc0	v0,C0_TLBHI		# set to invalid
	move	v0,zero			# initialize index
	li	v1,NTLBENTRIES<<TLBINX_INXSHIFT
					# loop limit
ILoop:
	mtc0	v0,C0_INX
	add	v0,1<<TLBINX_INXSHIFT	# increment index
	c0	C0_WRITEI		# invalidate entry
	.set reorder
	bne	v0,v1,ILoop		# march through all entries
	j	ra
	END(flushall_tlb)


LEAF(FlushWB)
	.set	noreorder
		nop
		nop
		nop
		nop
1:
		bc0f	1b
		nop
		j	ra
		nop
	.set	reorder
	END(FlushWB)


/*
*  SetBadPar_RB3125(addr,data)
*
*  SetBadPar_RB3125 reads the configuration register and inverts the sense of parity stored
*  into the lance buffer. Bad parity is written into the address specified. The config. 
*  register is written again restoring the polarity of the config. register to its original
*  state.
*/


LEAF(SetBadPar_RB3125)
		li	t0,CPU_CR_M2000 | 0xa0000000/* point to config reg. */
		lw	t1,0(t0) 		/* read the config. reg. */
		and	t2,t1,CR_BUFPARSEL 
		beq	t2,zero,1f 		/* check the parity used */
		and	t1,t1,~CR_BUFPARSEL
		sw	t1,0(t0) 		/* invert parity bit */
		b	2f
1:
		or	t1,t1,CR_BUFPARSEL 	/* invert parity bit */
		sw	t1,0(t0)
2:
		sb	a1,0(a0)		/* write bad parity */
		and	t2,t1,CR_BUFPARSEL	/* restore parity */
		beq	t2,zero,3f
		and	t1,t1,~CR_BUFPARSEL
		sw	t1,0(t0)
		b	4f
3:
		or	t1,t1,CR_BUFPARSEL
		sw	t1,0(t0)
4:
		j	ra	

END(SetBadPar_RB3125)
	
