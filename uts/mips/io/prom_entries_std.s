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
/* $Header: prom_entries_std.s,v 1.2.3.4 90/05/10 05:25:26 wje Exp $ */
/* Copyright: |
# |-----------------------------------------------------------|
# | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
# | All Rights Reserved                                       |
# |-----------------------------------------------------------|
# |          Restricted Rights Legend                         |
# | Use, duplication, or disclosure by the Government is      |
# | subject to restrictions as set forth in                   |
# | subparagraph (c)(1)(ii) of the Rights in Technical        |
# | Data and Computer Software Clause of DFARS 252.227-7013.  |
# |         MIPS Computer Systems, Inc.                       |
# |         928 Arques Avenue                                 |
# |         Sunnyvale, CA 94086                               |
# |-----------------------------------------------------------|
#   */

#include	"../ml/assym.s"


/*
 * define prom entrypoints of interest to kernel
 */

EXPORT(prom_reset)
	li	v0,PROM_RESET
	j	v0

EXPORT(prom_exec)
	li	v0,PROM_EXEC
	j	v0

EXPORT(prom_restart)
	li	v0,PROM_RESTART
	j	v0

EXPORT(prom_reinit)
	li	v0,PROM_REINIT
	j	v0

EXPORT(prom_reboot)
	li	v0,PROM_REBOOT
	j	v0

EXPORT(prom_autoboot)
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

/*
 * Read-modify-write cycle routines for Mbox'es
 * RMW_TOGGLE when read causes read-modify-write cycle with immediately
 * following read and write cycles.
 *
 * NOTE:  After the RMW_TOGGLE is read and while the VMEbus is locked,
 *	  the official documentation claims that the CPU is free to access
 *	  main memory, floss teeth, etc.  This does not appear to be the case
 *	  for earlier Mbox'es, such as the M/500.  Therefore, we execute
 *	  the heart of the r-m-w algorithm twice:  the first time for a main
 *	  memory address, to pull all the instructions into the I-cache, and
 *	  the second time with the real RMW_TOGGLE and the real VMEbus address.
 *	  This has an advantageous side-effect of executing the second pass,
 *	  the real RMW, completely out of the I-cache and minimizing the length
 *	  of time the VMEbus is in this locked state.
 *
 * a0	target address
 * a1	mask to AND or OR
 * a2	0 == AND   1 == OR
 * a3	1 == byte  2 == halfword  4 == word
 */
#define RMW_TOGGLE	0xbe400003

BSS(rmw_tmp,4)

LEAF(generic_rmw_std)
	.set	noreorder
	mfc0	v1,C0_SR		# save sr
	li	t2,RMW_TOGGLE		# RMW_TOGGLE for Mbox/M2000
	mtc0	zero,C0_SR		# interrupts off
	la	t2,rmw_tmp		# first pass fake RMW_TOGGLE
	la	t3,rmw_tmp		# first pass fake VME address

1:	lb	v0,0(t2)		# hit RMW toggle
	/*
	 *  Read the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	lb	v0,0(t3)		# (BDSLOT) Read 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	lh	v0,0(t3)		# (BDSLOT) Read 16-bit location
4:	lw	v0,0(t3)		# Read 32-bit location
9:
	/*
	 *  Perform the appropriate action:  AND or OR
	 */
	bne	a2,zero,5f		# AND or OR?
	or	t0,a1,v0		# (BDSLOT)   Modify with OR
	and	t0,a1,v0		# no Branch: Modify with AND
5:
	/*
	 *  Write the target address using the appropriate width
	 */
	li	t1,1			# length == 1 ?
	bne	a3,t1,2f
	li	t1,2			# (BDSLOT)
	b	9f
	sb	t0,0(t3)		# (BDSLOT) Write 8-bit location
2:	bne	a3,t1,4f		# length == 2 ?
	nop				# (BDSLOT)
	b	9f
	sh	t0,0(t3)		# (BDSLOT) Write 16-bit location
4:	sw	t0,0(t3)		# Write 32-bit location
9:
	bc0f	9b			# make sure WriteBuffer is flushed
	nop				# (BDSLOT)
	beq	t3,a0,9f		# have we done the real VME address?
	li	t2,RMW_TOGGLE		# (BDSLOT) pass two uses real RMW_TOGGLE
	b	1b
	move	t3,a0			# (BDSLOT) pass two uses real VME addr
9:	j	ra
	mtc0	v1,C0_SR		# (BDSLOT) restore sr
	.set	reorder
	END(generic_rmw_std)

LEAF(orw_rmw)
	li	a2,1			# OR
	li	a3,4			# 4-byte location
	j	generic_rmw_std		# do it
	END(orw_rmw)

LEAF(orh_rmw)
	li	a2,1			# OR
	li	a3,2			# 2-byte location
	j	generic_rmw_std		# do it
	END(orh_rmw)

LEAF(orb_rmw)
	li	a2,1			# OR
	li	a3,1			# 1-byte location
	j	generic_rmw_std		# do it
	END(orb_rmw)

LEAF(andw_rmw)
	li	a2,0			# AND
	li	a3,4			# 4-byte location
	j	generic_rmw_std		# do it
	END(andw_rmw)

LEAF(andh_rmw)
	li	a2,0			# AND
	li	a3,2			# 2-byte location
	j	generic_rmw_std		# do it
	END(andh_rmw)

LEAF(andb_rmw)
	li	a2,0			# AND
	li	a3,1			# 1-byte location
	j	generic_rmw_std		# do it
	END(andb_rmw)
