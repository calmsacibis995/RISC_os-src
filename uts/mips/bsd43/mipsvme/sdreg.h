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
/* $Header: sdreg.h,v 1.4.3.2 90/05/10 04:44:05 wje Exp $ */
#define SD_NMAP		64
#define SD_PGOFFSET	0xfff
#define SD_PGSHIFT	12

struct sddevice {
	unsigned long	sd_status;
	unsigned long	sd_command;
	unsigned long	sd_blocknumber;
	unsigned long	sd_byteoffset;
	unsigned long	sd_bytecount;
	unsigned long	sd_statistics;
	unsigned long	sd_map[SD_NMAP];
};

#define	SD_ERR		1

#define	SD_READ		1		/* read data */
#define	SD_WRITE	2		/* write data */
#define	SD_IE		3		/* interrupt enable */

#define SDISK_BASE	(0x1f001000+K1BASE)
