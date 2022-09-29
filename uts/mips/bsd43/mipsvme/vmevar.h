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
/* $Header: vmevar.h,v 1.7.1.2 90/05/10 04:45:07 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * This file contains definitions related to the kernel structures
 * for dealing with vme bus devices.
 *
 * Each vme bus controller which is not a device has a vme_ctlr structure.
 * Each vme bus device has a vme_device structure.
 */

/*
 * Per-controller structure.
 * (E.g. one for each disk and tape controller, and other things
 * which use and release buffered data paths.)
 *
 * If a controller has devices attached, then there are
 * cross-referenced vme_drive structures.
 * The queue of devices waiting to transfer is attached here.
 */
struct bsd43_(vme_ctlr) {
	struct	bsd43_(vme_driver) *vm_driver;
	short	vm_ctlr;	/* controller index in driver */
	short	vm_alive;	/* controller exists */
	int	(**bsd43_(vm_intr))();	/* interrupt handler(s) */
	caddr_t	vm_vmeaddr;	/* address of device in vm_addr space */
	int	vm_am;		/* address modifier for vm_addr */
	caddr_t	vm_addr;	/* address of device in k1 space space */
	struct	bsd43_(buf) vm_tab;	/* queue of devices for this controller */
};

/*
 * Per ``device'' structure.
 * A controller has devices.  Everything else is a ``device''.
 *
 * If a controller has many drives attached, then there will
 * be several vme_device structures associated with a single vme_ctlr
 * structure.
 *
 * This structure contains all the information necessary to run
 * a vme bus device without slaves.  It also contains information
 * for slaves of vme bus controllers as to which device on the slave
 * this is.  A flags field here can also be given in the system specification
 * and is used to tell device specific parameters.
 */
struct bsd43_(vme_device) {
	struct	bsd43_(vme_driver) *vi_driver;
	short	vi_unit;	/* unit number on the system */
	short	vi_ctlr;	/* mass ctlr number; -1 if none */
	short	vi_slave;	/* slave on controller */
	int	(**bsd43_(vi_intr))();	/* interrupt handler(s) */
	caddr_t	vi_vmeaddr;	/* address of device in vi_am space */
	int	vi_am;		/* address modifier for vi_addr */
	short	vi_dk;		/* if init 1 set to number for iostat */
	int	vi_flags;	/* parameter from system specification */
	caddr_t	vi_addr;	/* address of device in k1 space space */
	short	vi_alive;	/* device exists */
	short	vi_type;	/* driver specific type information */
/* this is the forward link in a list of devices on a controller */
	struct	bsd43_(vme_device) *vi_forw;
/* if the device is connected to a controller, this is the controller */
	struct	bsd43_(vme_ctlr) *vi_mi;
};

/*
 * Per-driver structure.
 *
 * Each vme bus driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct bsd43_(vme_driver) {
	int	(*vd_probe)();		/* see if a driver is really there */
	int	(*vd_slave)();		/* see if a slave is there */
	int	(*vd_attach)();		/* setup driver for a slave */
	char	*vd_dname;		/* name of a device */
	struct	bsd43_(vme_device) **vd_dinfo;	/* backpointers to ubdinit structs */
	char	*vd_mname;		/* name of a controller */
	struct	bsd43_(vme_ctlr) **vd_minfo;	/* backpointers to ubminit structs */
};

#ifdef KERNEL
/*
 * vmminit and vmdinit initialize the mass storage controller and
 * device tables specifying possible devices.
 */
extern	struct	bsd43_(vme_ctlr) bsd43_(vmminit)[];
extern	struct	bsd43_(vme_device) bsd43_(vmdinit)[];
#endif KERNEL

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


