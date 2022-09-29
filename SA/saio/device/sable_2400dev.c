#ident "$Header: sable_2400dev.c,v 1.4 90/01/16 13:44:46 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/sable_2400dev.c (SABLE version)
 * This file contains devices that are used by r2400 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _sconsinit(), _sconsopen(), _sconsstrategy(), _sconsioctl();

extern int _dkinit(), _dkopen(), _dkstrategy(), _dkioctl();
extern int _tsinit(), _tsopen(), _tsstrategy(), _tsioctl(), _tsclose();
extern int _lainit(), _laopen(), _lastrategy(), _laioctl(),_laclose();


/*
 * Don't put _init_pcons() here, its special!
 */
extern int _pconsstrategy(), _pconsioctl();

extern int _nulldev();

#ifndef PROM
extern int _memopen(), _memstrategy();
#endif

extern int _sdinit(), _sdopen(), _sdstrategy(), _sdioctl();

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


struct device_table r2400_device_table[] = {
	{
		"tty",		_sconsinit,	_sconsopen,
		_sconsstrategy, _nulldev,	_sconsioctl,
		DTTYPE_CHAR|DTTYPE_CONS,
		DTFS_NONE,	"console uart"
	},
	{
		"console",	_nulldev,	_nulldev,
		_pconsstrategy, _nulldev,	_pconsioctl,
		DTTYPE_CHAR,
		DTFS_NONE,	"pseudo console"
	},
	{	"dkis",		_dkinit,	_dkopen,
		_dkstrategy,	_nulldev,	_dkioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"SCSI disk"
	},
	{
		"dksd",		_sdinit,	_sdopen,
		_sdstrategy,	_nulldev,	_sdioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"sable disk"
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
		"tqis",		_tsinit,	_tsopen,
		_tsstrategy,	_tsclose,	_tsioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"SCSI tape"
	},
#ifndef PROM
	{
		"mem",		_nulldev,	_memopen,
		_memstrategy,	_nulldev,	_nulldev,
		DTTYPE_CHAR,
		DTFS_NONE,	"memory pseudo-device"
	},
#endif
};


#ifdef	PROM

struct device_table *_device_table[] = {
			NULL,				/* M500 */
			NULL,				/* M800 */
			NULL,				/* M1000 */
			r2400_device_table,		/* M120 */
			NULL,				/* M2000 */
			NULL,				/* RC6280 */
			NULL,				/* M12 */
			NULL,				/* M12 Sable */
			r2400_device_table,		/* M180 */
			NULL,				/* Rx3230 */
};

int _ndevices[] = {
	0,						/* M500 */
	0,						/* M800 */
	0,						/* M1000 */
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])), /* M120 */
	0,						/* M2000 */
	0,						/* RC6280 */
	0,						/* M12 */
	0,						/* M12 Sable */
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])), /* M180 */
	0,						/* Rx3230 */
};

#endif
