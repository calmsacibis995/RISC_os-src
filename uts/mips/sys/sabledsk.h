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
/* $Header: sabledsk.h,v 1.7.3.2 90/05/10 06:35:26 wje Exp $ */

#ifndef	_SYS_SABLEDSK_
#define	_SYS_SABLEDSK_	1



#define NSD	1
#define SD_NMAP		64
#define SD_PGOFFSET	0xfff
#define SD_PGSHIFT	12

struct sddevice {
	volatile unsigned long	sd_status;
	volatile unsigned long	sd_command;
	volatile unsigned long	sd_blocknumber;
	volatile unsigned long	sd_byteoffset;
	volatile unsigned long	sd_bytecount;
	volatile unsigned long	sd_statistics;
	volatile unsigned long	sd_map[SD_NMAP];
};

#define	SD_ERR		1

#define	SD_READ		1		/* read data */
#define	SD_WRITE	2		/* write data */
#define	SD_IE		3		/* interrupt enable */

#define SDISK_BASE	(0x1f001000+K1BASE)

#endif	_SYS_SABLEDSK_
