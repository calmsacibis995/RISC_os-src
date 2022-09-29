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
/* $Header: iopvar.h,v 1.2.3.2 90/05/10 06:25:05 wje Exp $ */
/*
 * $Header: iopvar.h,v 1.2.3.2 90/05/10 06:25:05 wje Exp $
 */
/*
 * This file contains definitions related to the kernel structures
 * for dealing with iop  devices.
 *
 * Each iop  controller which is not a device has a iop_ctlr structure.
 * Each iop  device has a iop_device structure.
 */

/*
 * Per-controller structure.
 * (E.g. one for each disk and tape controller, and other things
 * which use and release buffered data paths.)
 *
 * If a controller has devices attached, then there are
 * cross-referenced iop_drive structures.
 * The queue of devices waiting to transfer is attached here.
 */
struct iop_ctlr {
	struct	iop_driver *io_driver;
	short	io_ctlr;	/* controller index in driver */
	short	io_alive;	/* controller exists */
	int	(**io_intr)();	/* interrupt handler(s) */
	struct	buf io_tab;	/* queue of devices for this controller */
};

/*
 * Per ``device'' structure.
 * A controller has devices.  Everything else is a ``device''.
 *
 * If a controller has many drives attached, then there will
 * be several iop_device structures associated with a single iop_ctlr
 * structure.
 *
 * This structure contains all the information necessary to run
 * a iop  device without slaves.  It also contains information
 * for slaves of iop  controllers as to which device on the slave
 * this is.  A flags field here can also be given in the system specification
 * and is used to tell device specific parameters.
 */
struct iop_device {
	struct	iop_driver *ii_driver;
	short	ii_unit;	/* unit number on the system */
	short	ii_ctlr;	/* mass ctlr number; -1 if none */
	short	ii_slave;	/* slave on controller */
	int	(**ii_intr)();	/* interrupt handler(s) */
	short	ii_dk;		/* if init 1 set to number for iostat */
	int	ii_flags;	/* parameter from system specification */
	short	ii_alive;	/* device exists */
	short	ii_type;	/* driver specific type information */
/* this is the forward link in a list of devices on a controller */
	struct	iop_device *ii_forw;
/* if the device is connected to a controller, this is the controller */
	struct	iop_ctlr *ii_mi;
};

/*
 * Per-driver structure.
 *
 * Each iop  driver defines entries for a set of routines
 * as well as an array of types which are acceptable to it.
 * These are used at boot time by the configuration program.
 */
struct iop_driver {
	int	(*id_probe)();		/* see if a driver is really there */
	int	(*id_slave)();		/* see if a slave is there */
	int	(*id_attach)();		/* setup driver for a slave */
	char	*id_dname;		/* name of a device */
	struct	iop_device **id_dinfo;	/* backpointers to ubdinit structs */
	char	*id_mname;		/* name of a controller */
	struct	iop_ctlr **id_minfo;	/* backpointers to ubminit structs */
};
