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
/* $Header: reboot.h,v 1.7.1.2 90/05/10 04:54:57 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)reboot.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_

/*
 * Arguments to reboot system call.
 * These are passed to boot program in r11,
 * and on to init.
 */
#define	BSD43_RB_AUTOBOOT	0	/* flags for system auto-booting itself */

#define	BSD43_RB_ASKNAME	0x01	/* ask for file name to reboot from */
#define	BSD43_RB_SINGLE	0x02	/* reboot to single user only */
#define	BSD43_RB_NOSYNC	0x04	/* dont sync before reboot */
#define	BSD43_RB_HALT		0x08	/* don't reboot, just halt */
#define	BSD43_RB_INITNAME	0x10	/* name given for /etc/init */
#define	BSD43_RB_DFLTROOT	0x20	/* use compiled-in rootdev */
#define	BSD43_RB_WRITABLE	0x100	/* MOUNT FILESYSTEM WRITABLE */
#define	BSD43_RB_NOBOOTRC	0x40	/* don't run /etc/rc.boot */

#define	BSD43_RB_PANIC	0	/* reboot due to panic */
#define	BSD43_RB_BOOT		1	/* reboot due to boot() */

/*
 * Constants for converting boot-style device number to type,
 * adaptor (uba, mba, etc), unit number and partition number.
 * Type (== major device number) is in the low byte
 * for backward compatibility.  Except for that of the "magic
 * number", each mask applies to the shifted value.
 */
#define	BSD43_B_ADAPTORSHIFT	24
#define	BSD43_B_ADAPTORMASK	0x0f
#define BSD43_B_UNITSHIFT	16
#define BSD43_B_UNITMASK	0xff
#define BSD43_B_PARTITIONSHIFT 8
#define BSD43_B_PARTITIONMASK	0xff
#define	BSD43_B_TYPESHIFT	0
#define	BSD43_B_TYPEMASK	0xff
#define	BSD43_B_MAGICMASK	0xf0000000
#define	BSD43_B_DEVMAGIC	0xa0000000

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define B_ADAPTORMASK BSD43_B_ADAPTORMASK
#   define B_ADAPTORSHIFT BSD43_B_ADAPTORSHIFT
#   define B_DEVMAGIC BSD43_B_DEVMAGIC
#   define B_MAGICMASK BSD43_B_MAGICMASK
#   define B_PARTITIONMASK BSD43_B_PARTITIONMASK
#   define B_PARTITIONSHIFT BSD43_B_PARTITIONSHIFT
#   define B_TYPEMASK BSD43_B_TYPEMASK
#   define B_TYPESHIFT BSD43_B_TYPESHIFT
#   define B_UNITMASK BSD43_B_UNITMASK
#   define B_UNITSHIFT BSD43_B_UNITSHIFT
#   define RB_ASKNAME BSD43_RB_ASKNAME
#   define RB_AUTOBOOT BSD43_RB_AUTOBOOT
#   define RB_BOOT BSD43_RB_BOOT
#   define RB_DFLTROOT BSD43_RB_DFLTROOT
#   define RB_WRITABLE BSD43_RB_WRITABLE
#   define RB_HALT BSD43_RB_HALT
#   define RB_INITNAME BSD43_RB_INITNAME
#   define RB_NOBOOTRC BSD43_RB_NOBOOTRC
#   define RB_NOSYNC BSD43_RB_NOSYNC
#   define RB_PANIC BSD43_RB_PANIC
#   define RB_SINGLE BSD43_RB_SINGLE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


