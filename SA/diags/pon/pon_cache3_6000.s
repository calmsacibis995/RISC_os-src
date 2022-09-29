#ident "$Header: pon_cache3_6000.s,v 1.3.7.1 90/07/18 14:30:03 huang Exp $"
/* $Copyright$ */

/*
 *	Functional Description:
 *
 *	NOTE: This routine expects to be executed before a stack is
 *	      established.  Therefore, it uses the S registers freely.
 *
 *	RETURNS: A 0 if the test is successful, a 1 is unsuccessful.
 */

#define	Ra_save			s0
#define Dcache_Bsize		s1
#define Scache_Side_Bsize	s2
#define Subtest_Msg		s3

#define Tmp0			v0
#define Tmp1			v1

#define KILO_SHIFT		10	/* mult/divide by 1024 */


#include "machine/asm.h"
#include "machine/bc.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/regdef.h"
#include "machine/mach_ops.h"
#include "pon.h"

	.globl	reset_cache
	.extern	success
	.extern failure
	.extern skipped

	.extern	pexpect
	.extern	pactual
	.extern	pxor
	.extern	crlf

	.text

LEAF(Pon_Cache3)
	move	Ra_save,ra		# save our return address

	li	a0,PON_CACHE3		# just begun
	jal	pon_set_leds		# write to the CPU LEDS

  	la	a0,begintest
  	jal	pon_puts

	jal	GetDepend

	and	v0,PON_FAULT_CACHE
	beq	v0,zero,run

  	la	a0,skipped
  	jal	pon_puts

	b	norun
run:
	jal	reset_cache

	.set	noreorder
	mfc0	Tmp0,C0_SR		# set MM_MODE bit in status register
	li	Tmp1,SR_MM_MODE
	or	Tmp0,Tmp1
	mtc0	Tmp0,C0_SR
	.set	reorder

	/*
	 *  Determine the cache sizes from the IdProm
	 */

.set	noreorder
	li	Tmp0,CSR_IDPROM+4
	lb	Dcache_Bsize,ID_DCACHE_OFF(Tmp0)
	li	Tmp1,1					# (LDSLOT)
	lb	Dcache_Bsize,ID_DCACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Dcache_Bsize,4
/* XXX end hack */
	sll	Dcache_Bsize,Tmp1,Dcache_Bsize	# power of 2 shift for Kbytes
	sll	Dcache_Bsize,KILO_SHIFT		#  and for bytes

	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# power of 2 Kbytes
	li	Tmp1,1					# (LDSLOT)
	lb	Scache_Side_Bsize,ID_SCACHE_OFF(Tmp0)	# XXX SBC bug
	nop						# XXX SBC bug
/* XXX start hack */
	li	Scache_Side_Bsize,9
/* XXX end hack */
	sll	Scache_Side_Bsize,Tmp1,Scache_Side_Bsize
	sll	Scache_Side_Bsize,KILO_SHIFT-1		# and for bytes per side
	.set	reorder




 # Successful!
	.set	noreorder
	mfc0	Tmp0,C0_SR
	li	Tmp1,~SR_MM_MODE
	and	Tmp0,Tmp1		# remove MM_MODE
	mtc0	Tmp0,C0_SR		#  and restore SR
	.set	reorder

#ifdef XXX
  	la	a0,success
  	jal	pon_puts
#endif XXX

	move	v0,zero
	j	Ra_save


failed:
	.set	noreorder
	mfc0	Tmp0,C0_SR
	li	Tmp1,~SR_MM_MODE
	and	Tmp0,Tmp1		# remove MM_MODE
	mtc0	Tmp0,C0_SR		#  and restore SR
	.set	reorder

	beq	Subtest_Msg,zero,1f	# print any subtest msg?
	move	a0,Subtest_Msg
	jal	pon_puts
1:

	la	a0,failure
   	jal	pon_puts

	li	a0,PON_CACHE3
	jal	FastFlash

	li	a0,PON_CACHE3
	jal	pon_set_leds		# write to the CPU LEDS
norun:
	li	a0,PON_FAULT_CACHE
	jal	SetDepend

	li	v0,1
	j	Ra_save	

	END(Pon_Cache3)

	.data

begintest:	.asciiz	"Cache Test #3...TBD\r\n"
