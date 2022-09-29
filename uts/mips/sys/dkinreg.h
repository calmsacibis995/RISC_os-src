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
/* $Header: dkinreg.h,v 1.2.4.2 90/05/10 06:09:33 wje Exp $ */
/*
**
*/
#define NDKIN 		7		/* max number of drives allowed */
#define NDKINC 		2		/* max number of controllers */

#define HOST_A_RDY	0x80
#define INTR_OK		0x80		/* Interrupt OK */
#define INT_RETRY	3		/* Number of S/W Retries */
#define INT_POLLED	0		/* Polled mode of Controller */
#define INT_INTERRUPT	1		/* Interrupt mode of Controller */
#define FILES 		1		/* for SPACE command */
#define BLOCKS 		0		/* for SPACE command */
#define CHECK_CONDITION	0x2
/* these two are used for the mode select command
 */
#define PAGE_FORMAT	0x10		/* indicates page data complies with
					 * the page format */
#define SAVE_PARAMS	0x1		/* indicates that the page parameter
					 * data being set should be save by
					 * the target permanently */

/* Class Code for Group 1 Commands */
#define CD10BYTE	0x20		/* SCSI - Group 1 Command */

#define NOREWIND(x)	(x & 1)		/* Tape No Rewind */

#define XB(x)		(((unsigned long)x>>24)&0xff)
#define HB(x)		(((unsigned long)x>>16)&0xff)
#define MB(x)		(((unsigned long)x>>8)&0xff)
#define LB(x)		((unsigned long)x&0xff)
/*
 * Scsi Check Condition defines
 */
#define FM		0x80	/* Indicates that a File Mark has been read */
#define EOM		0x40	/* EOM or BOT has been reached */
#define VALID		0x80	/* Indicates that the residual length field */
/* Sense Keys			 * is defined (request sense command) */
#define NO_SENSE	0x00	/* FM, EOM or status unavailavble */
#define REC_ERR		0x01	/* command completed with recovery actions */
#define NOT_RDY		0x02	/* Tape drive can't be accessed */
#define MEDIUM_ERR	0x03	/* Non-recoverable data error */
#define HW_ERR		0x04	/* non-recoverable hdw failure (parity, etc) */
#define ILL_REQ		0x05	/* cmd block contains illegal parameter */
#define UNIT_ATN	0x06	/* cartridge change or tape drive was reset */
#define DATA_PROT	0x07	/* cartridge is write-protected */
#define BLANK_CHK	0x08	/* no-data condition encountered on tape */
#define CMD_ABORT	0x0b	/* tape drive aborted the command */
#define VOL_OVFLOW	0x0d	/* physical EOM with data still in buffer */
#define KEYMASK		0x0f
/*
**	SIX-BYTE COMMAND DESCRIPTOR BLOCK
**
**	    7    6    5    4    3    2    1    0
**	0   -          Operation Code	       -
**	1   -   LUN   -       LBA (MSB)        -
**      2   -             LBA                  -
**      3   -             LBA (LSB)            -
**      4   -          XFER Length             -
**      5   -Ven -    0    0    0    - Fl - Lk -
**
**	TEN-BYTE COMMAND DESCRIPTOR BLOCK
**
**	    7    6    5    4    3    2    1    0
**	0   -          Operation Code	       -
**	1   -   LUN   -    0    0    0    -Rel -
**      2   -             LBA (MSB)            -
**      3   -             LBA                  -
**      4   -             LBA                  -
**      5   -             LBA (LSB)            -
**      6   -              0                   -
**      7   -         XFER Length (MSB)        -
**      8   -         XFER Length (LSB)        -
**      9   -Ven -    0    0    0    - Fl - Lk -
*/
struct cdblk {
	unsigned char	cd_b0;	/* 0 */
	unsigned char	cd_b1;	/* 1 */
	unsigned char	cd_b2;	/* 2 */
	unsigned char	cd_b3;	/* 3 */
	unsigned char	cd_b4;	/* 4 */
	unsigned char	cd_b5;	/* 5 */
	unsigned char	cd_b6;	/* 6 */
	unsigned char	cd_b7;	/* 7 */
	unsigned char	cd_b8;	/* 8 */
	unsigned char	cd_b9;	/* 9 */
	unsigned char	cd_b10;	/* 10 */
	unsigned char	cd_b11;	/* 11 */
};
#define cd_opcode	cd_b0

struct int_iopb {
	unsigned char	in_task;	/* 0 */	/* Task ID */
	unsigned char	in_adctl;	/* 1 */ /* Adaptor Control */
	unsigned char	in_intlv;	/* 2 */	/* Interrupt level */
	unsigned char	in_statid;	/* 3 */ /* Status ID */
	unsigned char	in_burst;	/* 4 */	/* Data Throttle burst */
	unsigned char	in_break;	/* 5 */	/* Data Throttle break */
	unsigned short 	in_bufh;	/* 6 */	/* Buffer address */
	unsigned short 	in_bufl;	/* 8 */	/* Buffer address */
	unsigned char	in_addmod;	/* 10 */ /* Address Modifier */
	unsigned char	in_cnth;	/* 11 */ /* Transfer Count */
	unsigned char	in_cntm;	/* 12 */ /* Transfer Count */
	unsigned char	in_cntl;	/* 13 */ /* Transfer Count */
	unsigned char	in_adstatus;	/* 14 */ /* Adapter status */
	unsigned char	in_scstatus;	/* 15 */ /* SCSI status */
	unsigned char	in_target;	/* 16 */ /* Target ID */
	unsigned char	in_ext_ctl;	/* 17 */ /* To align */
		/* The Command Descriptor Structure 12 Bytes */
	unsigned char	incd_b0;	/* 18 */
	unsigned char	incd_b1;	/* 19 */
	unsigned char	incd_b2;	/* 20 */
	unsigned char	incd_b3;	/* 21 */
	unsigned char	incd_b4;	/* 22 */
	unsigned char	incd_b5;	/* 23 */
	unsigned char	incd_b6;	/* 24 */
	unsigned char	incd_b7;	/* 25 */
	unsigned char	incd_b8;	/* 26 */
	unsigned char	incd_b9;	/* 27 */
	unsigned char	incd_b10;	/* 28 */
	unsigned char	incd_b11;	/* 29 */

	unsigned char	in_resv1;	/* 30 */
	unsigned char	in_resv2;	/* 31 */
};

#define MAX_SGENTRY	31	/* max scat/gath entries for Rev 4 board */
#define NSGENTRY	30 	/* due to tandem-found bug */
/*
 * scatter/gather entry.
 */
struct insge {
	u_long	insg_addr;		/* datablock/entry address */
	u_char	insg_addrmod;		/* VME address modifier */
	u_char	insg_cnth;		/* Transfer Count */
	u_char	insg_cntm;	 	/* Transfer Count */
	u_char	insg_cntl;	 	/* Transfer Count */
};

/* Adaptor Control Byte in the IOPB
 */
#define	AC_IOPBPRI	0x80	/* IOPB Priority - Set for unix */
#define	AC_WIDTH16	0x40	/* 16 bit width if on */
#define	AC_WIDTH32	0x00	/* off => 32 bit mode */
#define	AC_BLKXFER	0x20	/* Block Burst Mode if used */
/* Data burst mode - Bit on for Transaction burst value is number of words */
/* Bit off = time on the VME Bus = burst value * 125 nseconds */
#define AC_BURST	0x10	/* Burst Mode */
#define AC_SCAT_GATH	0x08	/* Enable scatter/gather */
#define AC_PRIOVER	0x04	/* Priority Override Function */
#define AC_BYTE_XFER_1	0x02	/* LSB of 16-bit word mode xfer count */
#define AC_BYTE_XFER_0	0x01	/* LSB of 32-bit word mode xfer count */

#define BURSTVALUE	0xff	/* Burst Value for long words or words (256)*/
#define BURSTBREAK	0x10	/* (TIME*125ns) required for being off BUS */
/* Exteded control byte in the IOPB
 */
#define SCSIBUSY_EN	0x4	/* enabled causes adapter to return SCSI
				 * Busy status to the host. disabled (0)
				 * causes the adapter to retry the cmd 
				 * until its accepted by the target */
#define ARB_DISABLE	0x2	/* set to disable arbitration */
#define RESET_TARGET	0x1	/* set to issue a reset to a SCSI target */

/* SCSI Status
 */
#define SCSI_OK		0x00
#define SCSI_CHECK	0x02	/* Check condition - use request sense cmd */
#define SCSI_MET	0x04
#define SCSI_INTER	0x10
#define SCSI_COMBO	0x14
#define SCSI_RESV	0x18

/* Host Adaptor Status
 */
#define AS_NOERROR	0x00		/* No Error */
#define AS_SELTMO	0x81		/* Selection Timeout */
#define AS_PHASERR	0x82		/* SCSI Phase Error */
#define AS_PBERROR	0x83		/* I/O Parameter Block Negated */
#define AS_PARITY	0x84		/* SCSI Parity Error Unrecoverable */
#define AS_HWERROR	0x85		/* Hardware Failure */
#define AS_SCSITMO	0x86		/* SCSI Timeout Error */
#define AS_VMEBUSERR    0x87		/* VME bus error */

/* Group 0 Commands - All device Types
 */
#define C0_TESTRDY	0x00		/* Test Unit Ready */
#define C0_REQSENSE	0x03		/* Request Sense */
#define C0_INQUIRY	0x12		/* Inquiry */
#define C0_COPY		0x18		/* Copy */
#define C0_RECDIAG	0x1C		/* Receive Diagnostic Results */
#define C0_SENDDIAG	0x1D		/* Send Diagnostic Results */

/* Group 1 Commands - All device Types
 */
#define C1_COMPARE	0x39		/* Compare */
#define C1_COPYVERIFY	0x3A		/* Copy and Verify */

/* Group 0 Commands - Direct Access Devices
 */
#define C0_REZERO	0x01		/* Re Zero Unit */
#define C0_FORMAT	0x04		/* Format Unit */
#define C0_REASSIGN	0x07		/* Reassign Blocks */
#define C0_READ		0x08		/* Read */
#define C0_WRITE	0x0A		/* Write */
#define C0_SEEK		0x0B		/* Seek */
#define C0_MODESEL	0x15		/* Mode Select */
#define C0_RESERVE	0x16		/* Reserve */
#define C0_RELEASE	0x17		/* Release */
#define C0_SENSEMODE	0x1A		/* Mode Sense */
#define C0_STARTSTOP	0x1B		/* Start / Stop */
#define C0_MEDREMOVE	0x1E		/* Prevent/Allow Medium Removal */

/* Group 1 Commands - Direct Access Devices
 */
#define C1_READCAP	0x25		/* Read Capacity */
#define C1_READ		0x28		/* Read */
#define C1_WRITE	0x2A		/* Write */
#define C1_SEEK		0x2B		/* Seek */
#define C1_WRVERIFY	0x2E		/* Write and Verify */
#define C1_VERIFY	0x2F		/* Verify */
#define C1_DAHIGH	0x30		/* Search Data High */
#define C1_DAEQUAL	0x31		/* Search Data Equal */
#define C1_DALOW	0x32		/* Search Data Low */
#define C1_SETLIMITS	0x33		/* Set Limits */

/* Group 0 Commands - Sequential Access Devices
 */
#define	C0_REWIND	0x01		/* Rewind */
#define C0_RDBLOCK	0x05		/* Read Block Limits */
#define	C0_TRKSEL	0x0B		/* Select Track */
#define	C0_RDREV	0x0F		/* Read Reverse */
#define	C0_WRFM		0x10		/* Write Filemark */
#define	C0_SPACE	0x11		/* Space */
#define	C0_RECBUF	0x14		/* Recover Buffered Data */
#define	C0_ERASE	0x19		/* Erase */
#define	C0_LOAD		0x1B		/* Load / Unload */

/* Group 1 Commands - Sequential Access Devices - None */
/* Group 0 Commands - WORM Devices - None Different */
/* Group 1 Commands - WORM Devices - None Different */

/* defines for the Inquiry command
 */
#define TYPE_DISK		0x00
#define TYPE_TAPE		0x01
#define TYPE_PRINTER		0x02
#define TYPE_CPU		0x03
#define TYPE_WORM		0x04	/* optical */
#define TYPE_RONLY_DISK		0x05	/* optical */
#define TYPE_LUN_GONE		0x7f
#define REMOVE_MEDIA 		0x80	/* bit set indicates removable media */

/* C_FLAGS & UN_FLAGS Flags
 */
#define INT_ALIVE	0x000001	/* Device is Alive and Available */
#define INT_READY	0x000002	/* Device is Ready */
#define INT_BUSY	0x000004	/* Device is busy */
#define INT_WRITTEN	0x000008	/* Written to */
#define INT_TAPE	0x000010	/* A Tape Device */
#define INT_DISK	0x000020	/* A Disk Device */
#define INT_WORM	0x000040
#define INT_REWINDING	0x000080	/* rewinding */
#define INT_SELECTED	0x000100	/* Drive Selected */
#define INT_FORMATTED	0x000200	/* Drive Formatted - Do I need this */
#define INT_WAITING	0x000400	/* Waiting for Interrupt */
#define INT_WANTED	0x000800	/* Wanted after Interrupt */
#define INT_OPEN	0x001000	/* Driver Open Routine */
#define INT_READ	0x002000	/* device has been read */
#define INT_EOM		0x004000	/* EOM or BOT reached */
#define INT_FM		0x008000	/* indicates a file mark was read */
#define INT_PRINTER	0x010000	/* Printer Device */
#define INT_RMV_MEDIA	0x020000	/* Floppy Device or ? */
#define INT_ARB_DISABLE	0x040000	/* Disable arbitration for this device*/
#define INT_READONLY	0x080000	/* read-only device */
#define INT_ATN		0x100000	/* tape unit attention occurred */

struct inquiry {
	char	inq_type;		/* 0 */
	char	inq_qual;		/* 1 */
	char	inq_ansi;		/* 2 */
	char	inq_r0;			/* 3 Reserved */
	char	inq_addlen;		/* 4 */
	char	inq_r1;			/* 5 Reserved */
	char	inq_r2;			/* 6 Reserved */
	char	inq_r3;			/* 7 Reserved */
	char	inq_vendor[8];		/* 8-15 */
	char	inq_product[16];	/* 16-31 */
	char	inq_rev[4];		/* 32-35 */
};
#define PER	0x4	/* report ecc error (correction) to initiator */
#define DTE	0x2	/* disable data transmission on recovered error */
#define DCR	0x1	/* disable ecc application */
/* error recovery page struct for mode select command */
struct err_page {
	u_char	mode_sel[4];		/* mode select header */
/*	u_char	block_desc[8];		/* block descripter */
	u_char	err_pagecode;		/* error recovery page code */
	u_char	err_pagelength;		/* error recovery page length */
	u_char	err_flags;		/* error recovery page flags */
	u_char	err_retry_count;	/* recovery retry count */
	u_char	err_reserved0;		/* error recovery parameters */
	u_char	err_reserved1;		/* error recovery parameters */
	u_char	err_reserved2;		/* error recovery parameters */
	u_char	err_reserved3;		/* error recovery parameters */
};
