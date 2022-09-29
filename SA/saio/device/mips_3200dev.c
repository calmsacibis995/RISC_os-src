#ident "$Header: mips_3200dev.c,v 1.6 90/05/23 15:46:56 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/* 
** TODO:
**	2. review for NCP switch
**	3. review and update all devices and table for r3200 board.
**	   This new r3200 hardware will be used by M2000, currently
**	   M2000 use the r2300 hardware which sahred by M500, M800, N1000.
*/

/*
 * saio/device/mips_3200dev.c (MIPS version)
 * This file contains devices that are used by r3200 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _s2681init(), _s2681open(), _s2681strategy(), _s2681ioctl();

extern int _dkipinit(), _dkipopen(), _dkipstrategy(), _dkipioctl();
extern int _dkvjinit(), _dkvjopen(), _dkvjstrategy(), _dkvjioctl();
extern int _tpvjinit(), _tpvjopen(), _tpvjstrategy(), _tpvjioctl();
extern int _tpvjclose();

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
** ??? new devices for r3200 
*/

struct device_table r3200_device_table[] = {
	{
		"tty",		_s2681init,	_s2681open,
		_s2681strategy, _nulldev,	_s2681ioctl,
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
			r3200_device_table,		/* M2000 */
			NULL,				/* RC6280 */
			NULL,				/* M12 */
			NULL,				/* M12 Sable */
			NULL,				/* M180 */
			NULL,				/* Rx3230 */
			NULL				/* Genesis */
};

int _ndevices[] = {
	0,						/* M500 */
	0,						/* M800 */
	0,						/* M1000 */
	0,						/* M120 */
	(sizeof(r3200_device_table)/sizeof(r3200_device_table[0])), /* M2000 */
	0,						/* RC6280 */
	0,						/* M12 */
	0,						/* M12 Sable */
	0,						/* M180 */
	0,						/* Rx3230 */
	0						/* Genesis */
};

#endif

