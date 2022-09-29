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
/* $Header: flreg.h,v 1.2.3.2 90/05/10 06:14:18 wje Exp $ */
/*
 * $Header: flreg.h,v 1.2.3.2 90/05/10 06:14:18 wje Exp $
 */
/*
 * Integrated Solutions IOP-PSEUDO-SCSI floppy driver defines
 */

struct	flunitinfo {	       		/* pseudoctlr status block */
	u_char		ui_open;	/* number of open references */
	u_char		ui_dev;		/* type of disk first opened as */
	u_char		ui_sense;	/* sense was requested */
	u_char		ui_errcnt;
	u_int		ui_lba;		/* block number */
	int		ui_bleft;	/* bytes left to transfer */
	int		ui_bpart;	/* bytes transferred */
	caddr_t		ui_addr;	/* address of transfer */
	struct iobuf	*ui_utab;	/* pointer to drive queue */
	struct iotime	*ui_iotime;	/* pointer to drive queue */
	struct flop_msel ui_msel;
	struct buf	ui_cflbuf;	/* buffer header for special commands */
	struct shadevice	ui_dcb;	/* out of desperation */
	struct volume_header	ui_vh;	/* disk volume header for filesystems */
	union scsi_sns	ui_sns;		/* for sense on normal r/w */
};

#ifdef	INKERNEL
struct iotime fliotime[NFLTARG];	/* io statistics */
struct flunitinfo flunitinfo[NFLTARG];
struct iobuf flutab[NFLTARG];
extern	int flmajor;				/* internal major # */
extern  struct flop_msel fldisktypes[];
extern  int nfltypes;
#endif
