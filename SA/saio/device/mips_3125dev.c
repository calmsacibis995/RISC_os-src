#ident "$Header: mips_3125dev.c,v 1.1 90/05/23 15:50:34 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/mips_3125dev.c (MIPS version)
 * This file contains devices that are used by RB3125 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _sccinit(), _sccopen(), _sccstrategy(), _sccioctl();

extern int _dkipinit(), _dkipopen(), _dkipstrategy(), _dkipioctl();
extern int _dkvjinit(), _dkvjopen(), _dkvjstrategy(), _dkvjioctl();
extern int _tpvjinit(), _tpvjopen(), _tpvjstrategy(), _tpvjioctl();
extern int _tpvjclose();

extern int _pdkinit(), _pdkopen(), _pdkstrategy(), _pdkioctl();
extern int _ptsinit(), _ptsopen(), _ptsstrategy(), _ptsioctl(), _ptsclose();
extern int _cmcinit(), _cmcopen(), _cmcstrategy(), _cmcioctl(), _cmcclose();
extern int _eglinit(), _eglopen(), _eglstrategy(), _eglioctl(), _eglclose();
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


struct device_table r3125_device_table[] = {
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
		"dkip",		_dkipinit,	_dkipopen,
		_dkipstrategy,	_nulldev,	_dkipioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Interphase SMD disk controller"
	},
	{
		"dkij",		_dkvjinit,	_dkvjopen,
		_dkvjstrategy,	_nulldev,	_dkvjioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Interphase Jaguar SCSI disk"
	},
	{
		"tqij",		_tpvjinit,	_tpvjopen,
		_tpvjstrategy,	_tpvjclose,	_tpvjioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Interphase Jaguar SCSI tape"
	},
	/* bfs requires BSD network code */
	{
		"bfs",		_lainit,	_laopen,
		_lastrategy,	_laclose,	
		_laioctl,
		DTTYPE_CHAR,
		DTFS_BFS,	"boot server/LANCE ethernet"
	},
	/* bootp requires BSD network code */
	{
		"bootp",	_lainit,	_laopen,
		_lastrategy,	_laclose,	
		_laioctl,
		DTTYPE_CHAR,
		DTFS_BOOTP,	"Ethernet with BOOTP/TFTP protocols"
	},
	/* cmc ethernet controller */
	{
		"cmc",		_cmcinit,	_cmcopen,
		_cmcstrategy,	_cmcclose,	_cmcioctl,
		DTTYPE_CHAR,
		DTFS_NONE,	"CMC ethernet controller"
	},
	/* eagle ethernet controller */
	{
		"egl",		_eglinit,	_eglopen,
		_eglstrategy,	_eglclose,	_eglioctl,
		DTTYPE_CHAR,
		DTFS_NONE,	"EAGLE ethernet controller"
	},
	/* lance ethernet controller */
	{
		"la",		_lainit,	_laopen,
		_lastrategy,	_laclose,	_laioctl,
		DTTYPE_CHAR,
		DTFS_BFS,	"LANCE ethernet controller"
	},
	{
		"dksd",		_pdkinit,	_pdkopen,
		_pdkstrategy,	_nulldev,	_pdkioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Internal SCSI disk controller"
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
		"ncp",		_cmcinit,	_cmcopen,
		_cmcstrategy,	_cmcclose,	_cmcioctl,
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
			NULL,				/* Rx3230 */
			r3125_device_table		/* Genesis */
};

int _ndevices[] = {
	0,						/* M500 */
	0,						/* M800 */
	0,						/* M1000 */
	0,						/* M120 */
	0,						 /* M2000 */
	0,						/* RC6280 */
	0,						/* M12 */
	0,						/* M12 Sable */
	0,						/* M180 */
	0,						/* Rx3230 */
	(sizeof(r3125_device_table)/sizeof(r3125_device_table[0])) /* Genesis */
};

#endif

