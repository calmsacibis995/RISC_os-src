#ident "$Header: sable_alldev.c,v 1.5 90/05/30 14:27:58 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * saio/device/sable_alldev.c -- standalone device
 * configuration tables (SABLE version)
 * This file includes the device tables that used by all the machines.
 */

#ifndef	PROM
#include "sys/param.h"
#include "sys/errno.h"
#include "saio/saio.h"
#endif

#include "sable_2300dev.c"
#include "sable_2400dev.c"
#include "sable_3200dev.c"
#include "sable_6300dev.c"
#include "sable_3030dev.c"
#include "sable_3125dev.c"

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
			r3125_device_table,		/* RB3125 */
};

int _ndevices[] = {
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])), /* M500 */
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])), /* M800 */
	(sizeof(r2300_device_table)/sizeof(r2300_device_table[0])), /* M1000 */
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])), /* M120 */
	(sizeof(r3200_device_table)/sizeof(r3200_device_table[0])), /* M2000 */
	(sizeof(r6300_device_table)/sizeof(r6300_device_table[0])), /* RC6280 */
	0,							/* M12 */
	0,							/* M12 Sable */
	(sizeof(r2400_device_table)/sizeof(r2400_device_table[0])), /* M180 */
	(sizeof(r3030_device_table)/sizeof(r3030_device_table[0])), /* Rx3230 */
	(sizeof(r3125_device_table)/sizeof(r3125_device_table[0])), /* RB3125 */
};

#endif

