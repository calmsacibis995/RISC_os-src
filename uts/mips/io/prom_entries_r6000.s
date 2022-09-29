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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: prom_entries_r6000.s,v 1.6 90/07/24 10:32:19 hawkes Exp $ */
#include	"../ml/assym.s"

/*
 *  build_gbamap_rmw (rmw_addr, gbamap_base)
 *
 *  For the specified VME address space location (a0), resets its GbaMap entry
 *  to include the Lock bit and to change the OpCode to RMW.
 *  Returns the address of the GbaMap entry in v0, and the original contents
 *  in v1 (so the caller can restore the entry).
 *  The caller is expected to disable interrupts during the time the GbaMap
 *  entry is thus altered.
 *
 *  Uses v0, v1, t0
 */
LEAF(build_gbamap_rmw)
	and	v0,a0,GBA_SPAN-1	# form the addr of GbaMap entry
	srl	v0,PNUMSHFT		#  using the addr of the Map base
	sll	v0,2			#   and adding
	add	v0,a1			#    the page offset*4
	lw	v1,(v0)			# return original contents
	and	t0,v1,~GBA_OPCODE_MASK
	or	t0,(GBA_OPCODE_RMW | LOCK_IOA)
	sw	t0,(v0)			# add Lock and OpCode-RMW
	j	ra
	END(build_gbamap_rmw)

/*
 * Read-modify-write cycle routines for R6000
 * RMW_TOGGLE when read causes read-modify-write cycle with immediately
 * following read and write cycles
 *
 * a0	target address
 * a1	mask to AND or OR
 * a2	0 == AND   1 == OR
 * a3	1 == byte  2 == halfword  4 == word
 */
LEAF(generic_rmw_r6000)
	move	t9,ra			# save ra
	jal	get_ioa_ctlspace_addr	# what is CtlSpace for this IOA?
	beq	v0,zero,99f		# if not an IOA, then do in-line
	move	t8,v0			# save CtlSpace base address
	move	t7,a1			# save mask value
	li	t6,GBA0_MISC_BASE
	add	a1,v0,GBA0_MAP		# and pass GbaMap base as arg2
	beq	v1,zero,1f
	li	t6,GBA1_MISC_BASE
	add	a1,v0,GBA1_MAP
	.set	noreorder
1:	mfc0	t5,C0_SR		# save current SR
	jal	build_gbamap_rmw	# build an appropriate gbamap entry
	mtc0	zero,C0_SR		# (BDSLOT) disable interrupts
	/*
	 *  Read the target address using the appropriate width,
	 *  which also Locks the VMEbus.
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	t0,0(a0)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	t0,0(a0)		# (BDSLOT) Read 16-bit location
4:	lw	t0,0(a0)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,1f		# AND or OR?
	or	t2,t0,t7		# (BDSLOT)   Modify with OR
	and	t2,t0,t7		# no Branch: Modify with AND
1:
	/*
	 *  Write the target address using the appropriate width
	 */
	add	t6,t8			# form GBA_MISC address
	and	t4,a0,3			#  and merge the original low-order
	or	t6,t4			#   byte-offset-in-word bits
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t2,GBA_RMW_UNLOCK(t6)	# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t2,GBA_RMW_UNLOCK(t6)	# (BDSLOT) Write 16-bit location
4:	sw	t2,GBA_RMW_UNLOCK(t6)	# Write 32-bit location
9:
	/*
	 *  Restore original GbaMap entry, and clear IOC BusyReset
	 */
	sw	v1,(v0)			# restore original GbaMap entry
	li	t1,IOC_BUSY_RESET
	sw	t1,IOA_ERRORINFO_REG(t8)

	j	t9			# return
	mtc0	t5,C0_SR		# (BDSLOT) restore sr

99:
	/*
	 *  Not a GBA address, so perform the simple load/modify/store.
	 *
	 *  Read the target address using the appropriate width,
	 *  which also Locks the VMEbus.
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	t0,0(a0)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	t0,0(a0)		# (BDSLOT) Read 16-bit location
4:	lw	t0,0(a0)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,1f		# AND or OR?
	or	t2,t0,a1		# (BDSLOT)   Modify with OR
	and	t2,t0,a1		# no Branch: Modify with AND
1:
	/*
	 *  Write the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t2,(a0)			# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t2,(a0)			# (BDSLOT) Write 16-bit location
4:	sw	t2,(a0)			# Write 32-bit location
9:
	j	t9			# exit
	nop				# (BDSLOT)
	.set	reorder
	END(generic_rmw_r6000)

/*
 * define prom entrypoints of interest to kernel
 */

LEAF(andb_rmw)
	li	a2,0			# AND
	li	a3,1			# 1-byte location
	b	generic_rmw_r6000
	END(andb_rmw)

LEAF(andh_rmw)
	li	a2,0			# AND
	li	a3,2			# 2-byte location
	b	generic_rmw_r6000
	END(andh_rmw)

LEAF(andw_rmw)
	li	a2,0			# AND
	li	a3,4			# 4-byte location
	b	generic_rmw_r6000
	END(andw_rmw)

LEAF(orb_rmw)
	li	a2,1			# OR
	li	a3,1			# 1-byte location
	b	generic_rmw_r6000
	END(orb_rmw)

LEAF(orh_rmw)
	li	a2,1			# OR
	li	a3,2			# 2-byte location
	b	generic_rmw_r6000
	END(orh_rmw)

LEAF(orw_rmw)
	li	a2,1			# OR
	li	a3,4			# 4-byte location
	b	generic_rmw_r6000
	END(orw_rmw)

EXPORT(prom_reset)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_RESET
	j	v0

EXPORT(prom_exec)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_EXEC
	j	v0

EXPORT(prom_restart)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_RESTART
	j	v0

EXPORT(prom_reinit)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_REINIT
	j	v0

EXPORT(prom_reboot)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_REBOOT
	j	v0

EXPORT(prom_autoboot)
#ifdef R6000_BUG_PID
	.set	noreorder
	li	v0,C0_ERROR_IMASK
	mtc0	v0,C0_ERROR		# inhibit all parity errors
	.set	reorder
#endif
	li	v0,PROM_AUTOBOOT
	j	v0

EXPORT(prom_getchar)
	li	v0,PROM_GETCHAR
	j	v0

EXPORT(prom_putchar)
	li	v0,PROM_PUTCHAR
	j	v0

EXPORT(dprintf)
	li	v0,PROM_PRINTF
	j	v0

EXPORT(prom_open)
	li	v0,PROM_OPEN
	j	v0

EXPORT(prom_close)
	li	v0,PROM_CLOSE
	j	v0

EXPORT(prom_read)
	li	v0,PROM_READ
	j	v0

EXPORT(prom_ioctl)
	li	v0,PROM_IOCTL
	j	v0

EXPORT(prom_disablecmd)
	li	v0,PROM_DISABLECMD
	j	v0
	
EXPORT(prom_getenv)
	li	v0,PROM_GETENV
	j	v0

EXPORT(prom_setenv)
	li	v0,PROM_SETENV
	j	v0

EXPORT(prom_nv_get)
	li	v0,PROM_NV_GET
	j	v0

EXPORT(prom_nv_set)
	li	v0,PROM_NV_SET
	j	v0
