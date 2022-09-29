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
/* $Header: prom_entries_i2000.s,v 1.2.3.2 90/05/10 05:24:40 wje Exp $ */
#include	"../ml/assym.s"


EXPORT(prom_reset)
	li	a0,PROM_RESET
	j	_prom_restart

EXPORT(prom_restart)
	li	a0,PROM_RESTART
	j	_prom_restart

EXPORT(prom_reboot)
	li	a0,PROM_REBOOT
	j	_prom_restart

EXPORT(prom_autoboot)	
	li	a0,PROM_AUTOBOOT
	j	_prom_restart

EXPORT(prom_reinit)
	li	a0,PROM_REINIT
	j	_prom_restart

EXPORT(dprintf)
	j	ra		# this routine can't be supported on the
				# current Jupiter hardware because the proms
				# are inaccessible to the R2000.
