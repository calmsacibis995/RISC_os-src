#ident "$Header: csu.mips.s,v 1.8 90/06/13 11:22:04 chungc Exp $"
/*	%Q%	%I%	%M%
 * csu.s -- dbgmon startup code (independently loaded version)
 */
/* $Copyright$ */

#include "machine/regdef.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/asm.h"
#include "prom/entrypt.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "dbgmon/dbgmon.h"

	BSS(_fault_sp,4)
	BSS(environ, 4)
	BSS(_client_sp, 4)
	BSS(machine_type, 4)
	LBSS(argc, 4)
	LBSS(argv, 4)

	.text

/*
 * dbgmon entry point
 * dbgmon expects to be called:
 *	dbgmon(argc, argv, environ, entry_pc)
 * where argc, and argv are the arguments passed to the process
 * being debugged.  environ is a pointer to the current environment
 * and entry_pc is the initial pc for the process being debugged.
 * dbgmon assumes that the stack it is called on is the stack to use
 * for the process being debugged and that the gp register on entry
 * is the clients gp.  dbgmon itself runs on a stack above its bss.
 */
STARTFRM=	4*4+4
NESTED(start, STARTFRM, zero)
	move	v0,gp
	la	gp,_gp
	subu	sp,4*4			# guarantee space for argsaves
	sw	sp,_client_sp		# save for rdebug boot command
	sw	sp,_regs+R_SP*4		# save initial sp
	sw	v0,_regs+R_GP*4		# save client's gp
	sw	a0,_regs+R_A0*4		# save argc
	sw	a0,argc
	sw	a1,_regs+R_A1*4		# save argv
	sw	a1,argv
	sw	a2,_regs+R_A2*4		# save environ
	sw	a2,environ
	sw	a3,_regs+R_EPC*4	# save start pc
	/*
	 * switch stacks
	 */
	li	sp,DBGSTACKTOP-(4*4)
	sw	sp,_fault_sp		# small stack for fault handling
	subu	sp,EXSTKSZ
	li	v0,MODE_DBGMON
	sw	v0,_prom_mode
	subu	sp,STARTFRM		# fault stack can grow to here + 16
	sw	ra,STARTFRM-4(sp)
	jal	get_machine_type
	sw	v0,machine_type
	.set	noreorder
	bne	v0,BRDTYPE_R6300,1f
	nop				# (BDSLOT)
	mfc0	v1,C0_ERROR		# for R6000, save the Cp0 ERROR reg
	nop				# (LDSLOT) 
	sw	v1,_regs+R_ERROR*4
	sw	v1,_error_save		# save in case we want to display it
1:
	.set	reorder
	jal	flush_cache		# just to be sure
	jal	_hook_exceptions
	jal	_save_vectors
	lw	a0,argc
	lw	a1,argv
	lw	a2,environ
	jal	_dbgmon			# enter debug monitor
	/* shouldn't return, but just in case */
	j	_quit
	END(start)

/*
 * _quit -- real exit from dbgmon back to prom
 */
LEAF(_quit)
	li	ra,+PROM_RESTART
	j	ra
	END(_quit)


#define	CPU_IMP_R6000	3

LEAF(get_machine_type)
	/*
	 *	uses v0,v1,a0
	 *		returns machine_type in v0
	 */
	.set	noreorder
	mfc0	v0,C0_PRID		# what kind of CPU is this?
	nop				# (LDSLOT)
	.set	reorder
	srl	v0,8			# right-justify "implementation" field
	bne	v0,CPU_IMP_R6000,1f	# R6000 CPU?

	/*
	 * This is an R6000 CPU.  Use the explicit BRDTYPE.
	 */
	li	v0,BRDTYPE_R6300
	j	ra

1:
#ifndef SABLE
	/*
	 * There are no proms on the R2000 side of the Jupiter workstation.
	 * The first page of prom space is remapped to zero.  By writting
	 * a pattern to some offset in the first page and reading that
	 * pattern back in prom space we know that this system is a
	 * Jupiter workstation.
	 */
	li	v1,RESTART_ADDR-4
	li	v0,RESTART_MAGIC
	sw	v0,0(v1)
	or	v1,BOOTPROM
	lw	a0,0(v1)
	bne	a0,RESTART_MAGIC,3f
	li	v0,BRDTYPE_I2000
	j	ra
3:
	/*
	 * The idea here is that on Intrepid, BOOTPROM and BOOTPROM +
	 * BOOTPROM_SIZE point to mirror images of the boot prom.
	 * If it fails, then we are on either a M-series or M2000
	 */

	li	v0,ID_SN5_OFF-1			# max bytes in IDPROM
1:
	#
	#	check if data in IDPROM matches
	#
	lbu	v1,(K1BASE|BOOTPROM)(v0)		
	lbu	a0,K1BASE|(BOOTPROM+BOOTPROM_SIZE)(v0)
	bne	v1,a0,2f
	#
	#	check if passed max bytes in IDPROM
	sub	v0,1
	bgt	v0,zero,1b
	#
	# everything matched therefore it's intrepid or R3030
	#
	la	a0,K1BASE|IDPROM_R2400+3 # IDPROM of M120/M180
	li	v0,4			# check if boardtype = 4 
	lbu	v1,0(a0)
	bne	v0,v1,5f		# if not, then R3030
	#
	#	check if IDPROM checksum OK
	#
	li	v0,32			# max bytes in IDPROM
	li	v1,0			# initialize checksum
	.set	noat
1:
	lbu	AT,0(a0)
	addu	v1,AT
	#	check if passed max bytes in IDPROM
	addu	a0,4
	sub	v0,1
	bgt	v0,zero,1b
	.set	at

	and	v1,0xff
	beq	v1,zero,1f		# if checksum OK, then Intrepid
5:	li	v0,BRDTYPE_R3030
	j	ra
1:
        lhu     v0,SCR|K1BASE
        andi    v0,SCR_KEY0
        bne     v0,zero,4f
        li      v0,BRDTYPE_M180
        j       ra
4:	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra

2:		
	# something mismatched hence Mbox
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
#ifdef	R3030
	lw	v0,machine_type
	j	ra
#else
#ifdef INTREPID
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2400-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#else
	lw	v0,IDPROM_ADDR+((BRDTYPE_R2300-1)<<2)
	lb	v0,ID_BRDTYPE_OFF(v0)
	j	ra
#endif INTREPID
#endif	R3030
#endif SABLE
	END(get_machine_type)




