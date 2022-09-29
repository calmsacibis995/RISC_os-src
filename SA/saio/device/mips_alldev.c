#ident "$Header: mips_alldev.c,v 1.5 90/05/23 15:48:37 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/mips_alldev.c -- standalone device
 * configuration tables (MIPS version)
 * This file includes the device tables that used by all the machines.
 */

#ifndef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

#include "mips_2300dev.c"
#include "mips_2400dev.c"
#include "mips_3200dev.c"
#include "mips_6300dev.c"
#include "mips_3030dev.c"
#include "mips_3125dev.c"

#ifndef	PROM

struct device_table *_device_table[] = {
			r2300_device_table,		/* M500 */
			r2300_device_table,		/* M800 */
			r2300_device_table,		/* M1000 */
			r2400_device_table,		/* M120 */
			r3200_device_table,		/* M2000 */
			r6300_device_table,		/* RC6280 */
			NULL,				/* M12 */
			NULL,				/* M12 Sable */
			r2400_device_table,		/* M180 */
			r3030_device_table,		/* Rx3230 */
			r3125_device_table		/* Genesis */
};

int _ndevices[] = {
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])),
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])),
	(sizeof(r3200_device_table)/sizeof(r3200_device_table[0])),
	(sizeof(r6300_device_table)/sizeof(r6300_device_table[0])),
	0,
	0,
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])),
	(sizeof(r3030_device_table)/sizeof(r3030_device_table[0])),
	(sizeof(r3125_device_table)/sizeof(r3125_device_table[0]))
};

#endif


