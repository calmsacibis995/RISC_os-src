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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: xyl712reg.h,v 1.4.4.2.1.3 90/07/18 17:51:01 hawkes Exp $ */

#ifndef	_SYS_XYL712REG_
#define	_SYS_XYL712REG_	1


/*
 * Register definitions for the Xylogics 712 disk controller.
 */

/*
 * Stuff to handle possible byte orderings
 */
#ifdef	MIPSEL
#define	SwapBytes(s)	((((s) & 0xFF) << 8) | (((s) >> 8) & 0xFF))
#define	SetShort(i, s)	(i) = SwapBytes(s)
#define	SetLong(i, l) \
	(i) = ((SwapBytes(l) << 16) | SwapBytes(l >> 16))
#else
#define	SetShort(i, s)	(i) = (s)
#define	SetLong(i, l)	(i) = (l)
#endif

/* command iopb */
struct	xylCmdIopb {
#ifdef	MIPSEB
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_intLevel;		/* 06: interrupt level */
	unchar	xi_intVector;		/* 07: interrupt vector */
	ushort	xi_count;		/* 08,09: transfer count high */
	ushort	xi_cyl;			/* 0A,0B: cylinder address */
	unchar	xi_head;		/* 0C: head */
	unchar	xi_sector;		/* 0D: sector */
	unchar	xi_addrMod;		/* 0E: addr/link address modifier */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	ulong	xi_addr;		/* 10-13: data address H, MH, ML, L */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
#ifdef	MIPSEL
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_intLevel;		/* 06: interrupt level */
	ushort	xi_count;		/* 08,09: transfer count high */
	ushort	xi_cyl;			/* 0A,0B: cylinder address */
	unchar	xi_sector;		/* 0D: sector */
	unchar	xi_head;		/* 0C: head */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_addrMod;		/* 0E: addr/link address modifier */
	ulong	xi_addr;		/* 10-13: data address H, MH, ML, L */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
};

/* Iopb used for controller initialization */
struct	xylCtlrIopb {
#ifdef	MIPSEB
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_intLevel;		/* 06: interrupt level */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_cparamA;		/* 08: controller parameters A */
	unchar	xi_cparamB;		/* 09: controller parameters B */
	unchar	xi_cparamC;		/* 0A: controller parameters C */
	unchar	xi_throttle;		/* 0B: dma throttle */
	ushort	xi_mbz;			/* 0C,0D: must be zero */
	unchar	xi_type;		/* 0E: controller type */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_epromHigh;		/* 10: eprom part number, high byte */
	unchar	xi_epromLow;		/* 11: eprom part number, low byte */
	unchar	xi_rev;			/* 12: eprom revision */
	unchar	xi_subRev;		/* 13: eprom sub-revision */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
#ifdef	MIPSEL
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_intLevel;		/* 06: interrupt level */
	unchar	xi_cparamB;		/* 09: controller parameters B */
	unchar	xi_cparamA;		/* 08: controller parameters A */
	unchar	xi_throttle;		/* 0B: dma throttle */
	unchar	xi_cparamC;		/* 0A: controller parameters C */
	ushort	xi_mbz;			/* 0C,0D: must be zero */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_type;		/* 0E: controller type */
	unchar	xi_epromLow;		/* 11: eprom part number, low byte */
	unchar	xi_epromHigh;		/* 10: eprom part number, high byte */
	unchar	xi_subRev;		/* 13: eprom sub-revision */
	unchar	xi_rev;			/* 12: eprom revision */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
};

/* xi_cparamA bits */
#define	CPA_AUD		0x80		/* auto-update iopb */
#define	CPA_TMOD	0x40		/* transfer mode */
#define	CPA_ICS		0x10		/* iopb checksum */
#define	CPA_EDT		0x08		/* enable dma timeout */
#define	CPA_NPRM	0x04		/* non-privledged reg mode */
#define	CPA_AIOR	0x03		/* aio response time */

/* xi_cparamB bits */
#define	CPB_TDT		0xC0		/* throttle dead time */
#define	CPB_ROR		0x10		/* release on request */

/* xi_cparamC bits */
#define	CPC_OVS		0x80		/* overlap seek */
#define	CPC_COP		0x40		/* command optimization */
#define	CPC_IEC		0x20		/* interrupt at end-of-chain */
#define	CPC_ASR		0x10		/* automatic seek retry */
#define	CPC_ZLR		0x08		/* zero latency */
#define	CPC_RBC		0x04		/* retry before correction */
#define	CPC_ECCM	0x03		/* ecc mode */
#define	CPC_ECCM2	0x02		/* ecc mode 2 */

/* Iopb used for drive initialization */
struct	xylDriveIopb {
#ifdef	MIPSEB
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_driveParam;		/* 06: drive parameters */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_maxSpareSector;	/* 08: max sector for spare cyl */
	unchar	xi_headOffset;		/* 09: head offset for removable drv */
	ushort	xi_maxCyl;		/* 0A,0B: maximum cylinder */
	unchar	xi_maxHead;		/* 0C: maximum head */
	unchar	xi_maxSector;		/* 0D: maximum sector */
	unchar	xi_sectorsPerTrack;	/* 0E: sectors per track */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	ulong	xi_mbz;			/* 10-13: must be zero */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
#ifdef	MIPSEL
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_driveParam;		/* 06: drive parameters */
	unchar	xi_headOffset;		/* 09: head offset for removable drv */
	unchar	xi_maxSpareSector;	/* 08: max sector for spare cyl */
	ushort	xi_maxCyl;		/* 0A,0B: maximum cylinder */
	unchar	xi_maxSector;		/* 0D: maximum sector */
	unchar	xi_maxHead;		/* 0C: maximum head */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_sectorsPerTrack;	/* 0E: sectors per track */
	ulong	xi_mbz;			/* 10-13: must be zero */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
};

/* Iopb used for formatting */
struct	xylFormatIopb {
#ifdef	MIPSEB
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_interleave;		/* 06: interleave factor */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_field1;		/* 08: size of field 1 */
	unchar	xi_field2;		/* 09: size of field 2 */
	unchar	xi_field3;		/* 0A: size of field 3 */
	unchar	xi_field4;		/* 0B: size of field 4 */
	ushort	xi_sectorSize;		/* 0C,0D: size of sector, in bytes */
	unchar	xi_field12;		/* 0E: size of field 12 */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_field6;		/* 10: size of field 6 */
	unchar	xi_field7;		/* 11: size of field 7 */
	ushort	xi_otherSectorSize;	/* 12,13: other size of sector (AFE) */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
#ifdef	MIPSEL
	unchar	xi_status1;		/* 01: first status byte */
	unchar	xi_cmd;			/* 00: command */
	unchar	xi_status3;		/* 03: third status byte */
	unchar	xi_status2;		/* 02: second status byte */
	unchar	xi_unit;		/* 05: unit */
	unchar	xi_subCmd;		/* 04: subfunction for command */
	unchar	xi_intVector;		/* 07: interrupt vector */
	unchar	xi_interleave;		/* 06: interleave factor */
	unchar	xi_field2;		/* 09: size of field 2 */
	unchar	xi_field1;		/* 08: size of field 1 */
	unchar	xi_field4;		/* 0B: size of field 4 */
	unchar	xi_field3;		/* 0A: size of field 3 */
	ushort	xi_sectorSize;		/* 0C,0D: size of sector, in bytes */
	unchar	xi_nextIopbMod;		/* 0F: next iopb address modifier */
	unchar	xi_field12;		/* 0E: size of field 12 */
	unchar	xi_field7;		/* 11: size of field 7 */
	unchar	xi_field6;		/* 10: size of field 6 */
	ushort	xi_otherSectorSize;	/* 12,13: other size of sector (AFE) */
	ulong	xi_nextIopb;		/* 14-17: next iopb address */
	ushort	xi_iopbChecksum;	/* 18,19: iopb checksum */
	ushort	xi_eccPattern;		/* 1A,1B: ecc pattern word */
	ushort	xi_eccOffset;		/* 1C,1D: ecc offset word */
#endif
};

union	xyliopb {
	struct	xylCmdIopb xi_cmd;
	struct	xylCtlrIopb xi_ctlr;
	struct	xylDriveIopb xi_drive;
	struct	xylFormatIopb xi_format;
};

/* get the address of an iopb, in kseg 1 (non-cached) */
#define	iopb_cmd(ui, iopbnum) \
	((volatile struct xylCmdIopb *)K0_TO_K1(&((ui)->ui_iopb[iopbnum])))
#define	iopb_drv(ui, iopbnum) \
	((volatile struct xylDriveIopb *)K0_TO_K1(&((ui)->ui_iopb[iopbnum])))
#define	iopb_ctlr(ui, iopbnum) \
	((volatile struct xylCtlrIopb *)K0_TO_K1(&((ui)->ui_iopb[iopbnum])))
#define	iopb_format(ui, iopbnum) \
	((volatile struct xylFormatIopb *)K0_TO_K1(&((ui)->ui_iopb[iopbnum])))
#define	iopb_u(ui, iopbnum) \
	((volatile union xyliopb *)K0_TO_K1(&((ui)->ui_iopb[iopbnum])))

/* convenience macros */
#define	CMDIOPB(xi)	((volatile struct xylCmdIopb *)(xi))
#define	CTLRIOPB(xi)	((volatile struct xylCtlrIopb *)(xi))
#define	DRVIOPB(xi)	((volatile struct xylDriveIopb *)(xi))
#define	FMTIOPB(xi)	((volatile struct xylFormatIopb *)(xi))

/*
 * Command codes (xi_cmd), and their subfunctions
 */
#define	CMD_NOP		0x00
#define	CMD_WRITE	0x01
#define	CMD_READ	0x02
#define	CMD_SEEK	0x03
#define		CMD_SEEK_REPORT		0x00
#define		CMD_SEEK_SEEKREPORT	0x01
#define		CMD_SEEK_START		0x02
#define	CMD_RESET	0x04
#define		CMD_RESET_DRIVE		0x00
#define		CMD_RESET_FAULT		0x80
#define	CMD_WPARAMS	0x05
#define		CMD_WPARAMS_CTLR	0x00
#define		CMD_WPARAMS_DRIVE	0x80
#define		CMD_WPARAMS_FORMAT	0x81
#define		CMD_WPARAMS_STOREFORMAT	0xB0
#define	CMD_RPARAMS	0x06
#define		CMD_RPARAMS_CTLR	0x00
#define		CMD_RPARAMS_DRIVE	0x80
#define		CMD_RPARAMS_FORMAT	0x81
#define		CMD_RPARAMS_STATUS	0xB0
#define		CMD_RPARAMS_CONFIG	0xB1
#define	CMD_EWRITE	0x07
#define		CMD_EWRITE_HEADERS	0x80
#define		CMD_EWRITE_FORMAT	0x81
#define		CMD_EWRITE_GUNK	0x82
#define		CMD_EWRITE_DEFECTMAP	0xB0
#define	CMD_EREAD	0x08
#define		CMD_EREAD_HEADERS	0x80
#define		CMD_EREAD_READVERIFY	0x81
#define		CMD_EREAD_GUNK		0x82
#define		CMD_EREAD_DEFECTMAP	0xB0
#define	CMD_DIAG	0x09
#define	CMD_SEND	0x0C
#define		CMD_SEND_NOSTATUS	0xB0
#define		CMD_SEND_EXPECTSTATUS	0xB1

/* other bits in xi_cmd code field */
#define	CMD_SGM		0x10		/* use scatter gather */
#define	CMD_CHEN	0x20		/* chain enable */
#define	CMD_DONE	0x40		/* done bit (ctlr sets this) */
#define	CMD_ERRS	0x80		/* err bit (ctlr sets this) */

/* bits in the xi_status2 field */
#define	ST2_DRDY	0x01		/* drive is ready */
#define	ST2_CMPL	0x02		/* last command is complete */
#define	ST2_ATTN	0x08		/* drive has faulted */
#define	ST2_SR		0x40		/* slipped revolution */

/* bits in the xi_unit field */
#define	UNIT_FIXD	0x80
#define	UNIT_BHT	0x10

/* bits in the xi_intLevel field */
#define	ILEVEL_LLL_SHIFT 3		/* shift to put into bits 3-7 */

/* scatter gather structure */
struct	xylsg {
#ifdef	MIPSEB
	ushort	sg_count;
	unchar	sg_reserved;
	unchar	sg_mod;
	ulong	sg_addr;
#endif
#ifdef	MIPSEL
	ushort	sg_count;
	unchar	sg_mod;
	unchar	sg_reserved;
	ulong	sg_addr;
#endif
};

/* device registers */
struct	xyldevice {
#ifdef	MIPSEB
	unchar	d_unused0;		/* 00: unused */
	unchar	d_iopbAddr0;		/* 01: iopb address byte 0 */
	unchar	d_unused1;		/* 02: unused */
	unchar	d_iopbAddr1;		/* 03: iopb address byte 1 */
	unchar	d_unused2;		/* 04: unused */
	unchar	d_iopbAddr2;		/* 05: iopb address byte 2 */
	unchar	d_unused3;		/* 06: unused */
	unchar	d_iopbAddr3;		/* 07: iopb address byte 3 */
	unchar	d_unused4;		/* 08: unused */
	unchar	d_iopbMod;		/* 09: iopb address modifier */
	unchar	d_unused5;		/* 0A: unused */
	unchar	d_csr;			/* 0B: control and status register */
	unchar	d_unused6;		/* 0C: unused */
	unchar	d_err;			/* 0D: fatal error register */
#endif
#ifdef	MIPSEL
	unchar	d_iopbAddr0;		/* 01: iopb address byte 0 */
	unchar	d_unused0;		/* 00: unused */
	unchar	d_iopbAddr1;		/* 03: iopb address byte 1 */
	unchar	d_unused1;		/* 02: unused */
	unchar	d_iopbAddr2;		/* 05: iopb address byte 2 */
	unchar	d_unused2;		/* 04: unused */
	unchar	d_iopbAddr3;		/* 07: iopb address byte 3 */
	unchar	d_unused3;		/* 06: unused */
	unchar	d_iopbMod;		/* 09: iopb address modifier */
	unchar	d_unused4;		/* 08: unused */
	unchar	d_csr;			/* 0B: control and status register */
	unchar	d_unused5;		/* 0A: unused */
	unchar	d_err;			/* 0D: fatal error register */
	unchar	d_unused6;		/* 0C: unused */
#endif
};

/* csr bits */
#define	CSR_RBS		0x01		/* register busy semaphore */
#define	CSR_RIO		0x02		/* remove iopb */
#define	CSR_AIO		0x04		/* add iopb */
#define	CSR_RESET	0x08		/* controller reset */
#define	CSR_RESERVED	0x10		/* reserved */
#define	CSR_MAINTMODE	0x20		/* maintenance mode */
#define	CSR_FATAL	0x40		/* fatal error */
#define	CSR_BUSY	0x80		/* busy */

/* modifier values */
#define	IOPB_MOD	0x2D		/* standard address modifier */

/*
 * Device parameters
 */
#define	XYLUPC	4			/* units per controller */
#define	UIOPB	2			/* iopbs per unit */
#define	NSCAT	32			/* maximum scatter gather entries */

/* macros to extract info out of a dev_t */
#define	CTLR(dev)	(((dev) >> 6) & 3)
#define	UNIT(dev)	(((dev) >> 4) & 3)
#define	SLICE(dev)	((dev) & 0xF)

/*
 * Software data structure for maintaining the driver state
 */

/* per unit structure */
struct	xylunitinfo {
	char	ui_attached;		/* non-zero if drive attached */
	char	ui_nextIopb;		/* next free iopb to use */
	char	ui_unit;		/* which unit this is */
	union	xyliopb *ui_current;	/* pointer to active iopb */
	union	xyliopb	ui_iopb[NSCAT];	/* two for double buffering */
	struct	xylsg ui_sg[NSCAT];	/* scatter gather records */
	struct	volume_header ui_vh;	/* drive volume header */
	int	ui_spc;			/* sectors per cylinder */
	daddr_t	ui_bn;			/* current physical bn thats active */
	struct	buf ui_tab;		/* unit activity queue */
	struct	xylctlrinfo *ui_ci;	/* back pointer to controller */
};

/* per controller structure */
struct	xylctlrinfo {
	char	ci_probed;		/* non-zero if controller probed */
	char	ci_ctlr;		/* controller # */
	struct	xylunitinfo ci_unit[XYLUPC];
	struct	xyldevice *ci_device;	/* pointer to vme i/o space regs */
	unchar	ci_ipl;			/* interrupt priority level */
	unchar	ci_vec;			/* interrupt vector */
	struct	buf ci_tab;		/* controller activity queue */
};

extern	struct	xylctlrinfo xylctlrinfo[];

#endif	_SYS_XYL712REG_
