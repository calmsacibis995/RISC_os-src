#ident "$Header: sable_3125dev.c,v 1.1 90/05/30 14:19:00 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/sable_3125dev.c (SABLE version)
 * This file contains devices that are used by RB3125 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _sconsinit(), _sconsopen(), _sconsstrategy(), _sconsioctl();

extern int _dkipinit(), _dkipopen(), _dkipstrategy(), _dkipioctl();
extern int _tpqicinit(), _tpqicopen(), _tpqicstrategy(), _tpqicioctl(),
	_tpqicclose();
extern int _tpqicopen11(), _tpqicopen24();
extern int _dkvjinit(), _dkvjopen(), _dkvjstrategy(), _dkvjioctl();

extern int _cmcinit(), _cmcopen(), _cmcstrategy(), _cmcioctl(), _cmcclose();
extern int _eglinit(), _eglopen(), _eglstrategy(), _eglioctl(), _eglclose();


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

struct device_table r3125_device_table[] = {
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
	{
		"dksd",		_sdinit,	_sdopen,
		_sdstrategy,	_nulldev,	_sdioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"sable disk"
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
	/* bfs requires BSD network code */
	{
		"bfs",		_cmcinit,	_cmcopen,
		_cmcstrategy,	_cmcclose,	_cmcioctl,
		DTTYPE_CHAR,
		DTFS_BFS,	"boot server/CMC ethernet"
	},
	/* bootp requires BSD network code */
	{
		"bootp",	_cmcinit,	_cmcopen,
		_cmcstrategy,	_cmcclose,	_cmcioctl,
		DTTYPE_CHAR,
		DTFS_BOOTP,	"Ethernet with BOOTP/TFTP protocols"
	},
	{
		"tpqic",	_tpqicinit,	_tpqicopen,
		_tpqicstrategy,	_tpqicclose,	_tpqicioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"Integrated Solutions QIC-02 tape"
	},
	{
		"tpqic11",	_tpqicinit,	_tpqicopen11,
		_tpqicstrategy,	_tpqicclose,	_tpqicioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"ISI QIC-02 tape with QIC-11 format"
	},
	{
		"tpqic24",	_tpqicinit,	_tpqicopen24,
		_tpqicstrategy,	_tpqicclose,	_tpqicioctl,
		DTTYPE_BLOCK,
		DTFS_AUTO,	"ISI QIC-02 tape with QIC-24 format"
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
			r3125_device_table,		/* RB3125 */
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
	0,						/* Rx3230 */
	(sizeof(r3125_device_table)/sizeof(r3125_device_table[0])), /* RB3125 */
};

#endif
