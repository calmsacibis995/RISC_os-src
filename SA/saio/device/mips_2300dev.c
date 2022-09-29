#ident "$Header: mips_2300dev.c,v 1.4 90/01/16 13:10:46 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/mips_2300dev.c (MIPS version)
 * This file contains devices that are used by r2300 hardware.
 */

#ifdef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

extern int _s2681init(), _s2681open(), _s2681strategy(), _s2681ioctl();

extern int _dkipinit(), _dkipopen(), _dkipstrategy(), _dkipioctl();
extern int _tpqicinit(), _tpqicopen(), _tpqicstrategy(), _tpqicioctl(),
	_tpqicclose();
extern int _tpqicopen11(), _tpqicopen24();

extern int _cmcinit(), _cmcopen(), _cmcstrategy(), _cmcioctl(), _cmcclose();

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

struct device_table r2300_device_table[] = {
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
			r2300_device_table,		/* M500 */
			r2300_device_table,		/* M800 */
			r2300_device_table,		/* M1000 */
			NULL,				/* M120 */
			NULL,				/* M2000 */
			NULL,				/* RC6280 */
			NULL,				/* M12 */
			NULL,				/* M12 Sable */
			NULL,				/* M180 */
			NULL,				/* Rx3230 */
};

int _ndevices[] = {
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	0,						/* M120 */
	0,						/* M2000 */
	0,						/* RC6280 */
	0,						/* M12 */
	0,						/* M12 Sable */
	0,						/* M180 */
	0,						/* Rx3230 */
};

#endif

