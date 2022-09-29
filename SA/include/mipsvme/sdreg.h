#ident "$Header: sdreg.h,v 1.2 90/01/23 13:29:22 huang Exp $"
/* $Copyright$ */

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
