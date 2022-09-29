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
/* $Header: tthreg.h,v 1.8.4.2 90/05/10 06:42:21 wje Exp $ */

#ifndef	_SYS_TTHREG_
#define	_SYS_TTHREG_	1


/*******************************************************************************
********************************************************************************
****									    ****
****		Copyright (C) Tech-Source Laboratories, Inc.                ****
****                         A Division of Ciprico Inc. 1984.	            ****
****									    ****
****		              Tech-Source Laboratories                      ****
****                          1175 Spring Centre South                      ****
****                        Altamonte Springs, FL 32714                     ****
****                              (305) 788-9100                            ****
****									    ****
****				Ciprico Incorporated			    ****
****				 2405 Annapolis Lane			    ****
****				 Plymouth, MN 55441			    ****
****				   (612) 559-2034			    ****
****									    ****
****	Module Number:	xxxxxxxx                                            ****
****	Module Name:	tthreg.h					    ****
****	Package:	Tapemaster 3000 Driver for UNIX System 4.2  	    ****
****	Date:		(05/20/86)				            ****
****	Module Rev:	(%R%.%L%)					    ****
****									    ****
****	Subroutines:	Macros for SWAB, PTOAI, AITOP, SPLTT.		    ****
****									    ****
****	Description:							    ****
****		This is the include file for the Tapemaster 3000 Driver.    ****
****		It defines the software interface to the board, defining    ****
****		the various bit level patterns used by the TM 3000.	    ****
****		Also defined are the various structures used by the driver  ****
****		and the board.  No actual data allocation is done, only     ****
****		definition.  Several macros are also defined, mostly having ****
****		to do with byte ordering issues between Motorola & Intel.   ****
****									    ****
********************************************************************************
*******************************************************************************/

/*******************************************************************************
*									       *
*				Revision History			       *
*									       *
********************************************************************************

    Revision	Date		Author
	Description of Change

    1.1		11/1/85		Tim Beres
	Initial Release.  Driver based on SYS III/V driver by D. A. Dickey
    1.2		05/20/86	M. Tobias
	Corrected ioctl interface for 4.2 (was like System V)
    1.3		08/29/86	J. Fisher
	Ported to MIPS 4.2 system.

*******************************************************************************/

/*
	The definitions within this file are as follows:

	- Type definitions for 8, 16, and 32 bit quantities.
	- Macros for reordering bytes within 16 and 32 bit quantities.
	- Macros for busting the minor device number.
	- System dependent defines that must be edited to suit your system.
	- Defines for the Number of IOPB's to use.
	- Ioctl defines that should be moved to <sys/ioctl.h>.
	- Defines for the TM 3000 I/O Registers and how to interpret the
		various bit definitions therein.
	- The IOPB used by the controller (TTPB) and how to interpret the
		fields within.
	- The structure used for configuring the TM 3000 and its fields.
	- Driver internal structures (TTDEVICE and IOPB).
*/

#ifndef lint
static  char Sccshid[] = "%W%  %D%  Copyright Ciprico Inc";
#endif

#define MOTOROLA	/* byte ordering */

/****** First off, a few typedefs to make life a little easier. ******/
typedef unsigned char byte;		/* Should be an 8 bit quantity. */
typedef unsigned short word;		/* Should be a 16 bit quantity. */
typedef unsigned long Iptr;		/* Should be a 32 bit quantity. */


/*
 *	These macros are used for moving bytes around within a word.  The
 *  SWAB macro can be used for swapping bytes.  On this machine for the TM 3000
 *  the SWAB macro should leave the bytes along and just return a word.
 *  The PTOAI macro is used to convert a Pointer TO Absolute Intel pointer.
 *  This amounts to merely swapping the words within a 32 bit long.  The AITOP
 *  macro is used to reverse the PTOAI macro.  Since the PTOAI macro can reverse
 *  itself, simply call it.
 */
#undef	SWAB(x)		((x) & 0xFFFF)
#define	SWAB(x)		(((((x) >> 8) & 0xFF) | (((x) << 8) & 0xFF00)))
#define	PTOAI(x)	((Iptr) (((SWAB((Iptr) (x))) << 16) | (SWAB(((Iptr) (x)) >> 16))))
#define	AITOP(x)	((caddr_t) PTOAI(x))


#define SPLTT(x)	splx(pritospl(x))


#define S2_MBI_ADDR(x)	((int)((x)&0x000fffff))	/* the def in mbvar.h is incorrect */

/*
 *	The minor device number is broken down as follows:
 *		Bit:	76543210	(a one in the bit corresponds to on)
 *			^^^^^^^^
 *			||  ||\/
 *			||  || > Unit Number.
 *			||  |>>> No Rewind at close.
 *			||  >>>> Density bit, on selects high density.
 *			|>>>>>>> Speed bit, on selects high speed.
 *                      >>>>>>>> Extend bit, on selects extended gap.
 *
 *	The Unit Number tells the controller what physical drive the
 *	operations are for.  The No Rewind at close bit, when set, will
 *	prevent the driver from rewinding the Unit on the last close.
 *
 *	And now the macros to bust the minor device number:
 */
#define UNIT(dev)	(minor(dev) & 0x03)	/* Unit number from device # */
#define NOREWIND(dev)	(minor(dev) & 0x04)	/* No Rewind unit */
#define	DENSITY(dev)	(minor(dev) & 0x08)	/* Density select */
#define	SPEED(dev)	(minor(dev) & 0x40)	/* Speed select */
#define	EXTEND(dev)	(minor(dev) & 0x80)	/* Extended gap select */

/* VME things: */
#define	TTH_VECTOR	0x10	/* was 0xfb JF */
#define	TTH_LEVEL	3
/* 
 * The address modifier on the board should be jumpered to 0x2d. 
 * This is for 16bit address and 16bit data I/O transfers.
 * The address modifier in this code is for 32bit address 32bit data transfers.
 */
#define	TTH_ADDMOD	0x09	/* 32 bit addresses */
#define	IOPBAM		0x09	/* 32 bit addresses */

/********* Miscellaneous Defines. ********/

#ifndef TRUE
#define	TRUE	1		/* Boolean TRUE in the C language. */
#define	FALSE	0		/* Boolean FALSE in the C language. */
#endif TRUE

/********* Miscellaneous Macros. ***********/
#ifndef DELAY
#define DELAY(x) { register int _N_; _N_ = x; while (_N_--); }
#endif


/*
 *  The following defines are the sizes of arrays within the actual driver
 *  code.  They can safely be adjusted within the limits provided for your
 *  system.  NIOPB tells the driver how many IOPB's it has available to use.
 *  This is only useful when the driver gets a number of requests before the
 *  last one is done.  This only happens under UNIX when using the Buffered I/O
 *  device.  If you will use soley the Raw I/O interface to the driver, you
 *  can safely set this value to one as no more than one IOPB will ever be in
 *  use at a given moment.
 *  CAUTION:  Don't make this less than four!!!
 */
#define	NIOPB	6	/* Number of IOPB's to use. */


/*
 *  The following defines should probably be moved to sys/mtio.h and have that 
 *  file included instead of doing is this way.  In addition, also make sure
 *  that the defines are unique among all the operations in mtio.h.  These
 *  defines are used when a program uses the ioctl interface to the driver for
 *  doing various functions that aren't normally performed.
 */

#define MT_ISTM3000	9		/* identify ourself */
#define	MTIOCGCON	_IOR(m, 3, struct TTH_CBLOCK)	/* get configuration */
#define	MTIOCSCON	_IOW(m, 4, struct TTH_CBLOCK)	/* set configuration */


/*
 *	I/O space registers - word accesses only
 * This structure defines what the TM 3000 looks like to the driver:
 */

struct TTREG {
	word    addr0;		/* pba lsb */
	word	addr2;		/* pba msb */
	word	resv1;		/* reserved */
	word	addr4;		/* address modifier */
	word	resv[12];	/* reserved */
	word	reset;
};

#define	attention	addr0		/* Read causes channel attention */

/* 
 * The following additional defines are returned in the lower byte of the
 * Interface Status Block when the Drive Status command is used.  The above
 * DS_ defines are returned in the upper byte with a slight twist.  Actually,
 * the sense of the bit is negated.  Also, the order of the two bytes is
 * determined by the high bit in the IOPB control byte.
 */

#define	DS_UNITSEL	0x0007		/* Mask for UNIT Select */
#define	DS_DBY		0x0008		/* Data Busy Active */
#define	DS_FMK		0x0010		/* File Mark Detected */
#define DS_EOT		0x0020		/* at End Of Tape */
#define	DS_CER		0x0040		/* Corrected Error */
#define	DS_HER		0x0080		/* Hard Error */
#define	DS_BOT		0x0100		/* at Beginning Of Tape */
#define	DS_FPT		0x0200		/* Drive is Write Protected */
#define	DS_RWD		0x0400		/* drive is Rewinding */
#define	DS_ONL		0x0800		/* drive is Online */
#define	DS_RDY		0x1000		/* Drive Ready */
#define	DS_SPD		0x2000		/* Speed (Interface P2, pin 40) */
#define	DS_DEN		0x4000		/* Density (Interface P2, pin 26) */
#define	DS_FBY		0x8000		/* Formatter Busy */


/*
 *	TTPB is the actual IOPB used by the controller at execution time.
 */

struct TTPB {
	byte	cmd;		/* Command */
	byte	control;	/* Control */
	byte	levela;
	byte	vectora;
	byte	levelb;
	byte	vectorb;
	byte	sdptram;		/* Src/Dest Ptr AM */
	byte	lkptram;		/* Link Ptr AM */

	Iptr	sdptr;			/* Source/Destination Pointer */
	Iptr	pblink;			/* Linked Parameter Block Pointer */
	unsigned long	count;		/* Byte/Record Count */
	unsigned long	transfer;	/* Record Count */
	word	dsr;			/* Driver Status Register */
	byte	errco;			/* Error Code */
	byte	gate;			/* Gate */
};

/* Indices for vmeint: */
#define	NORMAL	0
#define	ERROR	1

/*
 * cmd - Command Codes	(cmd in struct TTPB)
 */
#define CONFIGURE	0x00	/* Configure */
#define GIVECONF	0x01	/* Return Configuration */
#define	RESET		0x10	/* Reset */
#define	REWIND		0x11	/* Rewind */
#define	SECURE		0x12	/* Security Erase */
#define	ERASE		0x13	/* Fixed Erase */
#define	ONLINE		0x14	/* Online */
#define	OFFLINE		0x15	/* Offline */
#define	DSTATUS		0x17	/* Get Drive Status */
#define	VARERASE	0x18	/* Variable Erase */
#define	WFM		0x20	/* Write File Mark */
#define	SFM		0x21	/* Space File Mark */
#define	SPACE		0x22	/* Space Record(s) */
#define	NORMREAD	0x30	/* Normal Read */
#define	RBREAD		0x31	/* Ring Buffer Read */
#define	NORMWRITE	0x32	/* Normal Write */
#define	RBWRITE		0x33	/* Ring Buffer Write */
#define	WUR		0x35	/* Ring Buffer Write Unlimited Record Size */
#define	RUR		0x36	/* Ring Buffer Read Unlimited Record Size */
#define	DIAG1		0x40	/* Diagnostic Test 1 */
#define	DIAG2		0x41	/* Diagnostic Test 2 */
#define	NSC		0x50	/* Non-Standard Command */
#define	NSCP		0x51	/* Non-Standard Command with Parameters */
#define	NSRS		0x52	/* Non-Standard Read Status Command */

/*
 * control - Control	(control in struct TTPB)
 */
#define	C_UNITSEL	0x07	/* Bits for Unit Select */
#define	C_REVERSE	0x08	/* Tape Direction */
#define	C_SKP		0x10	/* Skip EOT marker */
#define	C_LNK		0x20	/* Linked parameter block follows */

/*
 * gate - Command Gate	(gate in struct TTPB)
 */
#define G_ENT		0x01	/* Command Entered */
#define G_CMP		0x02	/* Command Done */
#define G_ERR		0x04	/* Error in command execution */

/*
 * errco - Error Codes	(errco in struct TTPB)
 */
		/* Recoverable errors: */
#define	BUSTOUT		0x01	/* Bus Timeout */
#define	NOREVBOT	0x02	/* Can't Reverse at BOT */
#define	HITEOT		0x03	/* End Of Tape */
#define	HITFM		0x04	/* Filemark detected */
#define	FBUSY		0x05	/* Formatter Busy */
#define	HARDTERR	0x06	/* Hard Tape Error */
#define	SWPADDR		0x07	/* Invalid Address to start swap (must be even) */
#define	SWPCOUNT	0x09	/* Invalid Byte Count to swap (must be even) */
#define	NOCMD		0x0A	/* Invalid Command */
#define	COUNTERR	0x0B	/* Invalid Byte/Record Count */
#define	RETRYERR	0x0C	/* Invalid retry count */
#define	THRTLERR	0x0D	/* Invalid Throttle size */
#define	LARGER		0x0E	/* Actual Record was Larger than requested */
#define	PREVPB		0x0F	/* Linked parameter block error (in prev. PB) */
#define	ONL_US		0x10	/* Online command unsupported */
#define	PARITY		0x11	/* Parity error */
#define	RDOVERRUN	0x12	/* Read Overrun */
#define	REVBOT		0x13	/* Reversed onto BOT */
#define	SMALLER		0x14	/* Actual Record was Smaller than requested */
#define	SOFTTERR	0x15	/* Soft Tape Error */
#define	PROTECTED	0x16	/* Tape File Protected */
#define	DRVBUSY		0x17	/* Drive busy (data busy) */
#define	NOTONL		0x18	/* Drive not Online */
#define	NOTRDY		0x19	/* Drive not Ready */
#define	WFMERR		0x1A	/* Write Filemark Error */
#define	WTUNDERRUN	0x1B	/* Write Underrun */
#define	SIGNALSERR	0x1C	/* Invalid signals parameter in configuration */
#define	BLANK		0x1D	/* Blank tape encountered */
#define	NOGO		0x1E	/* No Go error */
#define	NSCLENGTH	0x1F	/* Invalid non-standard block command length */
#define	NSCBCOUNT	0x20	/* Invalid non-standard block byte count */
		/* Unrecoverable Errors: */
#define	LOCALMEM	0x80	/* Local memory error */
#define	PROMCHK		0x83	/* PROM check error */
#define	PFIFO		0x84	/* Parameter/FIFO test error */
#define	FIFORAM		0x85	/* FIFO Ram test error */
#define	TAPESIDE	0x86	/* Tape Side test error */
#define	UTEA		0x87	/* Underrun Test Error A */
#define	UTEB		0x88	/* Underrun Test Error B */
#define	OTEA		0x89	/* Overrun Test Error A */
#define	OTEB		0x8A	/* Overrun Test Error B */
#define	TPTEA		0x8B	/* Tape Prescale Test Error A */
#define	TPTEB		0x8C	/* Tape Prescale Test Error B */
#define	TPTEC		0x8D	/* Tape Prescale Test Error C */

/*
 * CONFIGURE - Drive Configuration information
 *	This block is used for the CONFIGURE and GIVECONF commands
 */

struct CONF_INFO {
	struct TTH_CBLOCK {
		byte	buscntrl;	/* Bus Control */
		byte	tapecn1;	/* Tape Control 1 */
		byte	signals;	/* Signals */
		byte	throttle;	/* Throttle */
		byte	retries;	/* Retry Value */
		byte	tapecn2;	/* Tape Control 2 */
	} fmtr[2];
};
/*
 * buscntrl (Bus Control)	(buscntrl in struct TTH_CBLOCK)
 */
#define	WIDTH32		0x01	/* to select 32-Bit Bus Width transfers */
#define	SWP		0x02	/* to select Byte Swap during data transfer */
/*
 * tapecn[12] (Tape Control)	(tapecn[12] in struct TTH_CBLOCK)
 */
#define	HIGHSPD		0x01	/* Tape Speed */
#define	HIGHDEN		0x02	/* Tape Density */
#define	EXTGAP		0x04	/* Inter-Record Gap Lenth */
/*
 * throttle - At power up, the board starts with 32 bytes for the throttle.
 *	Let's retain this for the CONFIGURE within ttopen.
 */
#define	DEF_THROTTLE	0x20
/*
 * retries - At power up, the board starts with one retry.  Let's retain this
 *	for the CONFIGURE within tthopen.
 *	Select 2 erase retries and 5 retries.
 */
#define	DEF_RETRIES	((2 << 4) | 5)


/*
 * signals - Defines for creating signal values:
 */
#define	P1_36_GAP	0x01
#define	P1_36_DEN	0x02
#define	P1_44_THR	0x04
#define	P1_44_GAP	0x08
#define	P1_16_LOL	0x10
#define	P2_50_SPD	0x20
#define	P2_50_DEN	0x40
/*
 * signals - Here are some signal values for known drives:
 */
#define	CIPHER	(P2_50_SPD)
#define	TELEX	(P2_50_SPD | P1_16_LOL | P1_44_THR | P1_36_GAP)

/*
 * Ring Buffer Record - Used for the RBREAD, RBWRITE, WUR, and RUR commands.
 *	This is the block used by the controller at execution time.
 *	The WUR and RUR commands are as yet unsupported in this driver.
 */

struct RBBLOCK {
	byte		gate;		/* Gate */
	byte		reserved;	/* Reserved */
	byte		sdptram;	/* Src/Dest Ptr AM */
	byte		nxptram;	/* Next Ptr AM */
	unsigned long 	bcount;		/* Byte Count */
	Iptr		sdptr;		/* Source/Destination Pointer */
	Iptr		next;		/* Next Ring Buffer Record Pointer */
};

/*
 * gate - (Gate of Ring Buffer)	(gate in struct RBBLOCK)
 */
#define	RB_RDY	0x01	/* Buffer Ready */
#define	RB_BSY	0x02	/* Buffer Busy */
#define	RB_CMP	0x04	/* Buffer Complete */
#define	RB_ERR	0x08	/* Error */
#define	RB_ABT	0x10	/* Abort Ring Buffer Operation */
#define	RB_LST	0x20	/* Last Ring Buffer */
#define	RB_INT	0x40	/* Ring Buffer Interrupt */

/*
 * TTDEVICE - driver's per unit struct.
 *	This is used internally to the driver to keep track of information
 *	needed about the particular device.
 */

struct TTDEVICE {
	word	dsr;		/* Drive Status Register (copy from TTDSR) */
	word	status;		/* Bits used as defined below */
	daddr_t	blkno;		/* As close as we can tell, the drive is at this block */
	daddr_t	lastblkno;	/* As close as we can tell, this is the last block number on the tape. */
};
/*
 * TTDEVICE - Status bits	(status of struct TTDEVICE)
 */
#define	USED		0x01		/* I/O was performed on unit */
#define	RAWDEV	0x02		/* Tape is being used as a character device */
#define	INITIALIZED 0x04	/* Set after first open of unit. */


/*
 * IOPB - Drivers internal structure for keeping track of what operations are
 *	presently going on.  These structures can be on the ttaiopb list, in
 *	which case it is queued up to the controller and will be executed.
 *	They can be on the ttclist in which case they will be queue for
 *	execution as soon as the current ttaiopb list is finished.  And, they
 *	can be allocated but not yet queued; this would mean that a process
 *	is building up an operation.
 */

struct IOPB {
	struct IOPB *next;	/* Pointer to next IOPB in list */
	struct TTPB pb;		/* The actual PB used by the controller */
	int status;		/* Status bits as defined below */
	int mbinfo;		/* Bus info for this iopb (not used for MIPS)*/
	daddr_t end;		/* As close as we can tell, the tape */
				/* will be at this block number if */
				/* everything goes as planned. */
	struct buf *bp;		/* Pointer to the buffer for this operation */
	struct RBBLOCK rb[9];	/* Ring buffer blocks for splitting up large
					xfers into 4k pages. */
};
/*
 * IOPB status bits	(status of struct IOPB)
 */
#define	PBINUSE		0x01	/* IOPB is being used */
#define	PBWANTED	0x02	/* IOPB wanted (only appears in IOPB[0].status) */
#define	CMD		0x04	/* IOPB is a command */
#define	TRANSFER	0x08	/* IOPB is a data transfer */
#define	TSEEK		0x10	/* IOPB is a tape movement command */
#define	TREAD		0x20	/* IOPB is a tape read command */
#define	TWRITE		0x40	/* IOPB is a tape write command */

/*
 * LIST is a list of IOPB's, this is what a holding list is made up from.
 */
struct LIST {
	struct	IOPB	*head;		/* The head of the list */
	struct	IOPB	*tail;		/* The tail of the list */
};

#endif	_SYS_TTHREG_
