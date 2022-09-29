#ident "$Header: clientasm.s,v 1.4 90/01/18 09:54:53 huang Exp $"
/* $Copyright$ */

/*	%Q%	%I%	%M%
 *
 * clientasm.s -- assembler routines for transfering to and from client code
 */

#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu.h"

/*
 * client_start(argc, argv, environ, client_pc, client_sp);
 *
 * WARNING: dbgmon expects to be transfered to as:
 *	dbgmon(argc, argv, environ, client_pc)
 * In this case, the prom doesn't know an appropriate client_pc, so
 * client_start is currently written to pass the entry pc of the dbgmon
 * itself to the dbgmon.  This suffices to allow the dbgmon to come up
 * where the user can change the initial pc by hand.  (The dbgmon receives
 * the correct pc when invoked normally via check_dbg() in the saio library.)
 * If you change this, make sure a3 contains a accessable word address so
 * that a manually loaded dbgmon won't crap out when attempting to disassemble
 * the entry pc.
 */
CLIENTFRM=(4*4)+(3*4)
NESTED(client_start, CLIENTFRM, zero)
	subu	sp,CLIENTFRM
	sw	ra,CLIENTFRM-4(sp)
	sw	a0,CLIENTFRM-8(sp)	# flush for r6000 changes a0/a1
	sw	a1,CLIENTFRM-12(sp)
	jal	flush_cache		# make sure i cache is consistent
	lw	a0,CLIENTFRM-8(sp)	# restore a0/a1
	lw	a1,CLIENTFRM-12(sp)
	lw	v0,CLIENTFRM+16(sp)	# client_sp
	bne	v0,zero,1f		# if client_sp is nonzero, then use
	move	v0,sp			#  that as the sp; otherwise, use the
1:					#   current stack pointer
	li	v1,0xe0000000		# mask for high-order address bits
	and	v1,a3			#  isolate those bits...
	bne	v1,K0BASE,2f		# if entrypoint is K0, then use
	sll	v0,3			#  a K0 stackpointer
	srl	v0,3			#   otherwise don't alter sp
	or	v0,K0BASE		#    from what caller has specified
2:	move	sp,v0			# switch stacks
	jal	a3			# call client
	li	a0,0
	jal	exit
	END(client_start)
