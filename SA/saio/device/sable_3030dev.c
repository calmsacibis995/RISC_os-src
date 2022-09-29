#ident "$Header: sable_3030dev.c,v 1.7 90/03/08 18:11:36 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/* 
** TODO:
** 	1. review for really needed devices
**	3. review and update all devices and table for r3030 board.
**	   This r3030 device_table will be used by Rx3230, currently
**	   just borrow r2300 device_table to make things look easier.
*/

/*
 * Copyright 1989 by MIPS Computer Systems, Inc.
 */

/*
 * saio/device/sable_3030dev.c (SABLE version)
 * This file contains devices that are used by r3030 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _sccinit(), _sccopen(), _sccstrategy(), _sccioctl();

extern int _pdkinit(), _pdkopen(), _pdkstrategy(), _pdkioctl();
extern int _ptsinit(), _ptsopen(), _ptsstrategy(), _ptsioctl(), _ptsclose();

extern int _lainit(), _laopen(), _lastrategy(), _laioctl(), _laclose();

/*
 * Don't put _init_pcons() here, its special!
 */
extern int _pconsstrategy(), _pconsioctl();

extern int _nulldev();

#ifndef PROM
extern int _memopen(), _memstrategy();
#endif

/*
 * _device_table -- interface between standalone io system and device drivers
 * add new devices by adding entry to this table
 *
 * NEW: We now use the notion of separate _device_table's for each type
 *      of machine we will be running on.  We then have a array of
 *      pointers to _device_table's that contain the pointers to each
 *	of the _device_table's.  The index used to get the proper 
 *	_device_table is the cpu board type which is taken from the ID prom.
 *	Also the size variable is now an array to hold all the sizes.
 *
 * strategy routines should return number of characters transferred
 */

/*
** ??? new devices for r3030 
*/

struct device_table r3030_device_table[] = {
	{
		"tty",		_sccinit,	_sccopen,
		_sccstrategy, _nulldev,	_sccioctl,
		DTTYPE_CHAR|DTTYPE_CONS,
		DTFS_NONE,	"console uart"
	},
	{
		"console",	_nulldev,	_nulldev,
		_pconsstrategy, _nulldev,	_pconsioctl,
		DTTYPE_CHAR,
		DTFS_NONE,	"pseudo console"
	},
	{
		"dksd",		_pdkinit,	_pdkopen,
		_pdkstrategy,	_nulldev,	_pdkioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Internal SCSI disk controller"
	},
	/* bfs requires BSD network code */
	{
		"bfs",		_lainit,	_laopen,
		_lastrategy,	_laclose,	_laioctl,
		DTTYPE_CHAR,
		DTFS_BFS,	"boot server/LANCE ethernet"
	},
	/* bootp requires BSD network code */
	{
		"bootp",	_lainit,	_laopen,
		_lastrategy,	_laclose,	_laioctl,
		DTTYPE_CHAR,
		DTFS_BOOTP,	"Ethernet with BOOTP/TFTP protocols"
	},
	{
		"tqsd",		_ptsinit,	_ptsopen,
		_ptsstrategy,	_ptsclose,	_ptsioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Internal SCSI tape"
	},
#ifndef PROM
	{
		"mem",		_nulldev,	_memopen,
		_memstrategy,	_nulldev,	_nulldev,
		DTTYPE_CHAR,
		DTFS_NONE,	"memory pseudo-device"
	},
#ifdef NCP
	{
		"ncp",		_lainit,	_laopen,
		_lastrategy,	_laclose,	_laioctl,
		DTTYPE_CHAR|DTTYPE_CONS|DTTYPE_RAW,
		DTFS_NCP,	"Network Console Protocol"
	},
#endif NCP
#endif
};


#ifdef	PROM

struct device_table *_device_table[] = {
			NULL,				/* M500 */
			NULL,				/* M800 */
			NULL,				/* M1000 */
			NULL,				/* M120 */
			NULL,				/* M2000 */
			NULL,				/* RC6280 */
			NULL,				/* M12 */
			NULL,				/* M12 Sable */
			NULL,				/* M180 */
			r3030_device_table,		/* Rx3230 */
};

int _ndevices[] = {
	0,						/* M500 */
	0,						/* M800 */
	0,						/* M1000 */
	0,						/* M120 */
	0,						/* M2000 */
	0,						/* RC6280 */
	0,						/* M12 */
	0,						/* M12 Sable */
	0,						/* M180 */
	(sizeof(r3030_device_table)/sizeof(r3030_device_table[0])), /* Rx3230 */
};

#endif

