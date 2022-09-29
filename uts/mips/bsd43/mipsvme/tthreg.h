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
/* $Header: tthreg.h,v 1.8.1.2 90/05/10 04:44:19 wje Exp $ */
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
--------------------------------------------------------------------------------

    1.1		11/1/85		Tim Beres
	Initial Release.  Driver based on SYS III/V driver by D. A. Dickey
    1.2		05/20/86	M. Tobias
	Corrected ioctl interface for 4.2 (was like System V)
    1.3		08/29/86	J. Fisher
	Ported to MIPS 4.2 system.

*******************************************************************************/

/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


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



#define BSD43_MOTOROLA	/* byte ordering */

/****** First off, a few typedefs to make life a little easier. ******/
typedef unsigned char bsd43_(byte);		/* Should be an 8 bit quantity. */
typedef unsigned short bsd43_(word);		/* Should be a 16 bit quantity. */
typedef unsigned long BSD43_(Iptr);		/* Should be a 32 bit quantity. */


/*
 *	These macros are used for moving bytes around within a word.  The
 *  SWAB macro can be used for swapping bytes.  On this machine for the TM 3000
 *  the SWAB macro should leave the bytes along and just return a word.
 *  The PTOAI macro is used to convert a Pointer TO Absolute Intel pointer.
 *  This amounts to merely swapping the words within a 32 bit long.  The AITOP
 *  macro is used to reverse the PTOAI macro.  Since the PTOAI macro can reverse
 *  itself, simply call it.
 */
#undef	BSD43_SWAB(x)		((x) & 0xFFFF)
#define	BSD43_SWAB(x)		(((((x) >> 8) & 0xFF) | (((x) << 8) & 0xFF00)))
#define	BSD43_PTOAI(x)	((BSD43_(Iptr)) (((BSD43_SWAB((BSD43_(Iptr)) (x))) << 16) | (BSD43_SWAB(((BSD43_(Iptr)) (x)) >> 16))))
#define	BSD43_AITOP(x)	((caddr_t) BSD43_PTOAI(x))


#define BSD43_SPLTT(x)	bsd43_(splx)(pritospl(x))


#define BSD43_S2_MBI_ADDR(x)	((int)((x)&0x000fffff))	/* the def in mbvar.h is incorrect */

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
#define BSD43_UNIT(dev)	(bsd43_minor(dev) & 0x03)	/* Unit number from device # */
#define BSD43_NOREWIND(dev)	(bsd43_minor(dev) & 0x04)	/* No Rewind unit */
#define	BSD43_DENSITY(dev)	(bsd43_minor(dev) & 0x08)	/* Density select */
#define	BSD43_SPEED(dev)	(bsd43_minor(dev) & 0x40)	/* Speed select */
#define	BSD43_EXTEND(dev)	(bsd43_minor(dev) & 0x80)	/* Extended gap select */

/* VME things: */
#define	BSD43_TTH_VECTOR	0x10	/* was 0xfb JF */
#define	BSD43_TTH_LEVEL	3
/* 
 * The address modifier on the board should be jumpered to 0x2d. 
 * This is for 16bit address and 16bit data I/O transfers.
 * The address modifier in this code is for 32bit address 32bit data transfers.
 */
#define	BSD43_TTH_ADDMOD	0x09	/* 32 bit addresses */
#define	BSD43_IOPBAM		0x09	/* 32 bit addresses */

/********* Miscellaneous Defines. ********/

#define	BSD43_TRUE	1		/* Boolean TRUE in the C language. */
#define	BSD43_FALSE	0		/* Boolean FALSE in the C language. */

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
#define	BSD43_NIOPB	6	/* Number of IOPB's to use. */


/*
 *  The following defines should probably be moved to sys/mtio.h and have that 
 *  file included instead of doing is this way.  In addition, also make sure
 *  that the defines are unique among all the operations in mtio.h.  These
 *  defines are used when a program uses the ioctl interface to the driver for
 *  doing various functions that aren't normally performed.
 */

#define BSD43_MT_ISTM3000	9		/* identify ourself */
#define	BSD43_MTIOCGCON	BSD43__IOR(m, 3, struct BSD43_(TTH_CBLOCK))	/* get configuration */
#define	BSD43_MTIOCSCON	BSD43__IOW(m, 4, struct BSD43_(TTH_CBLOCK))	/* set configuration */


/*
 *	I/O space registers - word accesses only
 * This structure defines what the TM 3000 looks like to the driver:
 */

struct BSD43_(TTREG) {
	bsd43_(word)    addr0;		/* pba lsb */
	bsd43_(word)	addr2;		/* pba msb */
	bsd43_(word)	resv1;		/* reserved */
	bsd43_(word)	addr4;		/* address modifier */
	bsd43_(word)	resv[12];	/* reserved */
	bsd43_(word)	reset;
};

#define	bsd43_attention	addr0		/* Read causes channel attention */

/* 
 * The following additional defines are returned in the lower byte of the
 * Interface Status Block when the Drive Status command is used.  The above
 * DS_ defines are returned in the upper byte with a slight twist.  Actually,
 * the sense of the bit is negated.  Also, the order of the two bytes is
 * determined by the high bit in the IOPB control byte.
 */

#define	BSD43_DS_UNITSEL	0x0007		/* Mask for UNIT Select */
#define	BSD43_DS_DBY		0x0008		/* Data Busy Active */
#define	BSD43_DS_FMK		0x0010		/* File Mark Detected */
#define BSD43_DS_EOT		0x0020		/* at End Of Tape */
#define	BSD43_DS_CER		0x0040		/* Corrected Error */
#define	BSD43_DS_HER		0x0080		/* Hard Error */
#define	BSD43_DS_BOT		0x0100		/* at Beginning Of Tape */
#define	BSD43_DS_FPT		0x0200		/* Drive is Write Protected */
#define	BSD43_DS_RWD		0x0400		/* drive is Rewinding */
#define	BSD43_DS_ONL		0x0800		/* drive is Online */
#define	BSD43_DS_RDY		0x1000		/* Drive Ready */
#define	BSD43_DS_SPD		0x2000		/* Speed (Interface P2, pin 40) */
#define	BSD43_DS_DEN		0x4000		/* Density (Interface P2, pin 26) */
#define	BSD43_DS_FBY		0x8000		/* Formatter Busy */


/*
 *	TTPB is the actual IOPB used by the controller at execution time.
 */

struct BSD43_(TTPB) {
	bsd43_(byte)	cmd;		/* Command */
	bsd43_(byte)	control;	/* Control */
	bsd43_(byte)	levela;
	bsd43_(byte)	vectora;
	bsd43_(byte)	levelb;
	bsd43_(byte)	vectorb;
	bsd43_(byte)	sdptram;		/* Src/Dest Ptr AM */
	bsd43_(byte)	lkptram;		/* Link Ptr AM */

	BSD43_(Iptr)	sdptr;			/* Source/Destination Pointer */
	BSD43_(Iptr)	pblink;			/* Linked Parameter Block Pointer */
	unsigned long	count;		/* Byte/Record Count */
	unsigned long	transfer;	/* Record Count */
	bsd43_(word)	dsr;			/* Driver Status Register */
	bsd43_(byte)	errco;			/* Error Code */
	bsd43_(byte)	gate;			/* Gate */
};

/* Indices for vmeint: */
#define	BSD43_NORMAL	0
#define	BSD43_ERROR	1

/*
 * cmd - Command Codes	(cmd in struct TTPB)
 */
#define BSD43_CONFIGURE	0x00	/* Configure */
#define BSD43_GIVECONF	0x01	/* Return Configuration */
#define	BSD43_RESET		0x10	/* Reset */
#define	BSD43_REWIND		0x11	/* Rewind */
#define	BSD43_SECURE		0x12	/* Security Erase */
#define	BSD43_ERASE		0x13	/* Fixed Erase */
#define	BSD43_ONLINE		0x14	/* Online */
#define	BSD43_OFFLINE		0x15	/* Offline */
#define	BSD43_DSTATUS		0x17	/* Get Drive Status */
#define	BSD43_VARERASE	0x18	/* Variable Erase */
#define	BSD43_WFM		0x20	/* Write File Mark */
#define	BSD43_SFM		0x21	/* Space File Mark */
#define	BSD43_SPACE		0x22	/* Space Record(s) */
#define	BSD43_NORMREAD	0x30	/* Normal Read */
#define	BSD43_RBREAD		0x31	/* Ring Buffer Read */
#define	BSD43_NORMWRITE	0x32	/* Normal Write */
#define	BSD43_RBWRITE		0x33	/* Ring Buffer Write */
#define	BSD43_WUR		0x35	/* Ring Buffer Write Unlimited Record Size */
#define	BSD43_RUR		0x36	/* Ring Buffer Read Unlimited Record Size */
#define	BSD43_DIAG1		0x40	/* Diagnostic Test 1 */
#define	BSD43_DIAG2		0x41	/* Diagnostic Test 2 */
#define	BSD43_NSC		0x50	/* Non-Standard Command */
#define	BSD43_NSCP		0x51	/* Non-Standard Command with Parameters */
#define	BSD43_NSRS		0x52	/* Non-Standard Read Status Command */

/*
 * control - Control	(control in struct TTPB)
 */
#define	BSD43_C_UNITSEL	0x07	/* Bits for Unit Select */
#define	BSD43_C_REVERSE	0x08	/* Tape Direction */
#define	BSD43_C_SKP		0x10	/* Skip EOT marker */
#define	BSD43_C_LNK		0x20	/* Linked parameter block follows */

/*
 * gate - Command Gate	(gate in struct TTPB)
 */
#define BSD43_G_ENT		0x01	/* Command Entered */
#define BSD43_G_CMP		0x02	/* Command Done */
#define BSD43_G_ERR		0x04	/* Error in command execution */

/*
 * errco - Error Codes	(errco in struct TTPB)
 */
		/* Recoverable errors: */
#define	BSD43_BUSTOUT		0x01	/* Bus Timeout */
#define	BSD43_NOREVBOT	0x02	/* Can't Reverse at BOT */
#define	BSD43_HITEOT		0x03	/* End Of Tape */
#define	BSD43_HITFM		0x04	/* Filemark detected */
#define	BSD43_FBUSY		0x05	/* Formatter Busy */
#define	BSD43_HARDTERR	0x06	/* Hard Tape Error */
#define	BSD43_SWPADDR		0x07	/* Invalid Address to start swap (must be even) */
#define	BSD43_SWPCOUNT	0x09	/* Invalid Byte Count to swap (must be even) */
#define	BSD43_NOCMD		0x0A	/* Invalid Command */
#define	BSD43_COUNTERR	0x0B	/* Invalid Byte/Record Count */
#define	BSD43_RETRYERR	0x0C	/* Invalid retry count */
#define	BSD43_THRTLERR	0x0D	/* Invalid Throttle size */
#define	BSD43_LARGER		0x0E	/* Actual Record was Larger than requested */
#define	BSD43_PREVPB		0x0F	/* Linked parameter block error (in prev. PB) */
#define	BSD43_ONL_US		0x10	/* Online command unsupported */
#define	BSD43_PARITY		0x11	/* Parity error */
#define	BSD43_RDOVERRUN	0x12	/* Read Overrun */
#define	BSD43_REVBOT		0x13	/* Reversed onto BOT */
#define	BSD43_SMALLER		0x14	/* Actual Record was Smaller than requested */
#define	BSD43_SOFTTERR	0x15	/* Soft Tape Error */
#define	BSD43_PROTECTED	0x16	/* Tape File Protected */
#define	BSD43_DRVBUSY		0x17	/* Drive busy (data busy) */
#define	BSD43_NOTONL		0x18	/* Drive not Online */
#define	BSD43_NOTRDY		0x19	/* Drive not Ready */
#define	BSD43_WFMERR		0x1A	/* Write Filemark Error */
#define	BSD43_WTUNDERRUN	0x1B	/* Write Underrun */
#define	BSD43_SIGNALSERR	0x1C	/* Invalid signals parameter in configuration */
#define	BSD43_BLANK		0x1D	/* Blank tape encountered */
#define	BSD43_NOGO		0x1E	/* No Go error */
#define	BSD43_NSCLENGTH	0x1F	/* Invalid non-standard block command length */
#define	BSD43_NSCBCOUNT	0x20	/* Invalid non-standard block byte count */
		/* Unrecoverable Errors: */
#define	BSD43_LOCALMEM	0x80	/* Local memory error */
#define	BSD43_PROMCHK		0x83	/* PROM check error */
#define	BSD43_PFIFO		0x84	/* Parameter/FIFO test error */
#define	BSD43_FIFORAM		0x85	/* FIFO Ram test error */
#define	BSD43_TAPESIDE	0x86	/* Tape Side test error */
#define	BSD43_UTEA		0x87	/* Underrun Test Error A */
#define	BSD43_UTEB		0x88	/* Underrun Test Error B */
#define	BSD43_OTEA		0x89	/* Overrun Test Error A */
#define	BSD43_OTEB		0x8A	/* Overrun Test Error B */
#define	BSD43_TPTEA		0x8B	/* Tape Prescale Test Error A */
#define	BSD43_TPTEB		0x8C	/* Tape Prescale Test Error B */
#define	BSD43_TPTEC		0x8D	/* Tape Prescale Test Error C */

/*
 * CONFIGURE - Drive Configuration information
 *	This block is used for the CONFIGURE and GIVECONF commands
 */

struct BSD43_(CONF_INFO) {
	struct BSD43_(TTH_CBLOCK) {
		bsd43_(byte)	buscntrl;	/* Bus Control */
		bsd43_(byte)	tapecn1;	/* Tape Control 1 */
		bsd43_(byte)	signals;	/* Signals */
		bsd43_(byte)	throttle;	/* Throttle */
		bsd43_(byte)	retries;	/* Retry Value */
		bsd43_(byte)	tapecn2;	/* Tape Control 2 */
	} fmtr[2];
};
/*
 * buscntrl (Bus Control)	(buscntrl in struct TTH_CBLOCK)
 */
#define	BSD43_WIDTH32		0x01	/* to select 32-Bit Bus Width transfers */
#define	BSD43_SWP		0x02	/* to select Byte Swap during data transfer */
/*
 * tapecn[12] (Tape Control)	(tapecn[12] in struct TTH_CBLOCK)
 */
#define	BSD43_HIGHSPD		0x01	/* Tape Speed */
#define	BSD43_HIGHDEN		0x02	/* Tape Density */
#define	BSD43_EXTGAP		0x04	/* Inter-Record Gap Lenth */
/*
 * throttle - At power up, the board starts with 32 bytes for the throttle.
 *	Let's retain this for the CONFIGURE within ttopen.
 */
#define	BSD43_DEF_THROTTLE	0x20
/*
 * retries - At power up, the board starts with one retry.  Let's retain this
 *	for the CONFIGURE within tthopen.
 *	Select 2 erase retries and 5 retries.
 */
#define	BSD43_DEF_RETRIES	((2 << 4) | 5)


/*
 * signals - Defines for creating signal values:
 */
#define	BSD43_P1_36_GAP	0x01
#define	BSD43_P1_36_DEN	0x02
#define	BSD43_P1_44_THR	0x04
#define	BSD43_P1_44_GAP	0x08
#define	BSD43_P1_16_LOL	0x10
#define	BSD43_P2_50_SPD	0x20
#define	BSD43_P2_50_DEN	0x40
/*
 * signals - Here are some signal values for known drives:
 */
#define	BSD43_CIPHER	(BSD43_P2_50_SPD)
#define	BSD43_TELEX	(BSD43_P2_50_SPD | BSD43_P1_16_LOL | BSD43_P1_44_THR | BSD43_P1_36_GAP)

/*
 * Ring Buffer Record - Used for the RBREAD, RBWRITE, WUR, and RUR commands.
 *	This is the block used by the controller at execution time.
 *	The WUR and RUR commands are as yet unsupported in this driver.
 */

struct BSD43_(RBBLOCK) {
	bsd43_(byte)		gate;		/* Gate */
	bsd43_(byte)		reserved;	/* Reserved */
	bsd43_(byte)		sdptram;	/* Src/Dest Ptr AM */
	bsd43_(byte)		nxptram;	/* Next Ptr AM */
	unsigned long 	bcount;		/* Byte Count */
	BSD43_(Iptr)		sdptr;		/* Source/Destination Pointer */
	BSD43_(Iptr)		next;		/* Next Ring Buffer Record Pointer */
};

/*
 * gate - (Gate of Ring Buffer)	(gate in struct RBBLOCK)
 */
#define	BSD43_RB_RDY	0x01	/* Buffer Ready */
#define	BSD43_RB_BSY	0x02	/* Buffer Busy */
#define	BSD43_RB_CMP	0x04	/* Buffer Complete */
#define	BSD43_RB_ERR	0x08	/* Error */
#define	BSD43_RB_ABT	0x10	/* Abort Ring Buffer Operation */
#define	BSD43_RB_LST	0x20	/* Last Ring Buffer */
#define	BSD43_RB_INT	0x40	/* Ring Buffer Interrupt */

/*
 * TTDEVICE - driver's per unit struct.
 *	This is used internally to the driver to keep track of information
 *	needed about the particular device.
 */

struct BSD43_(TTDEVICE) {
	bsd43_(word)	dsr;		/* Drive Status Register (copy from TTDSR) */
	bsd43_(word)	bsd43_(status);		/* Bits used as defined below */
	daddr_t	blkno;		/* As close as we can tell, the drive is at this block */
	daddr_t	lastblkno;	/* As close as we can tell, this is the last block number on the tape. */
};
/*
 * TTDEVICE - Status bits	(status of struct TTDEVICE)
 */
#define	BSD43_USED	0x01		/* I/O was performed on unit */
#define	BSD43_RAWDEV	0x02		/* Tape is being used as a character device */
#define	BSD43_INITIALIZED 0x04	/* Set after first open of unit. */


/*
 * IOPB - Drivers internal structure for keeping track of what operations are
 *	presently going on.  These structures can be on the ttaiopb list, in
 *	which case it is queued up to the controller and will be executed.
 *	They can be on the ttclist in which case they will be queue for
 *	execution as soon as the current ttaiopb list is finished.  And, they
 *	can be allocated but not yet queued; this would mean that a process
 *	is building up an operation.
 */

struct BSD43_(IOPB) {
	struct BSD43_(IOPB) *next;	/* Pointer to next IOPB in list */
	struct BSD43_(TTPB) pb;		/* The actual PB used by the controller */
	int bsd43_(status);		/* Status bits as defined below */
	int mbinfo;		/* Bus info for this iopb (not used for MIPS)*/
	daddr_t end;		/* As close as we can tell, the tape */
				/* will be at this block number if */
				/* everything goes as planned. */
	struct bsd43_(buf) *bp;		/* Pointer to the buffer for this operation */
	struct BSD43_(RBBLOCK) rb[9];	/* Ring buffer blocks for splitting up large
					xfers into 4k pages. */
};
/*
 * IOPB status bits	(status of struct IOPB)
 */
#define	BSD43_PBINUSE		0x01	/* IOPB is being used */
#define	BSD43_PBWANTED	0x02	/* IOPB wanted (only appears in IOPB[0].status) */
#define	BSD43_CMD		0x04	/* IOPB is a command */
#define	BSD43_TRANSFER	0x08	/* IOPB is a data transfer */
#define	BSD43_TSEEK		0x10	/* IOPB is a tape movement command */
#define	BSD43_TREAD		0x20	/* IOPB is a tape read command */
#define	BSD43_TWRITE		0x40	/* IOPB is a tape write command */

/*
 * LIST is a list of IOPB's, this is what a holding list is made up from.
 */
struct BSD43_(LIST) {
	struct	BSD43_(IOPB)	*head;		/* The head of the list */
	struct	BSD43_(IOPB)	*tail;		/* The tail of the list */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define AITOP BSD43_AITOP
#   define BLANK BSD43_BLANK
#   define BUSTOUT BSD43_BUSTOUT
#   define CIPHER BSD43_CIPHER
#   define CMD BSD43_CMD
#   define CONFIGURE BSD43_CONFIGURE
#   define COUNTERR BSD43_COUNTERR
#   define C_LNK BSD43_C_LNK
#   define C_REVERSE BSD43_C_REVERSE
#   define C_SKP BSD43_C_SKP
#   define C_UNITSEL BSD43_C_UNITSEL
#   define DEF_RETRIES BSD43_DEF_RETRIES
#   define DEF_THROTTLE BSD43_DEF_THROTTLE
#   define DENSITY BSD43_DENSITY
#   define DIAG1 BSD43_DIAG1
#   define DIAG2 BSD43_DIAG2
#   define DRVBUSY BSD43_DRVBUSY
#   define DSTATUS BSD43_DSTATUS
#   define DS_BOT BSD43_DS_BOT
#   define DS_CER BSD43_DS_CER
#   define DS_DBY BSD43_DS_DBY
#   define DS_DEN BSD43_DS_DEN
#   define DS_EOT BSD43_DS_EOT
#   define DS_FBY BSD43_DS_FBY
#   define DS_FMK BSD43_DS_FMK
#   define DS_FPT BSD43_DS_FPT
#   define DS_HER BSD43_DS_HER
#   define DS_ONL BSD43_DS_ONL
#   define DS_RDY BSD43_DS_RDY
#   define DS_RWD BSD43_DS_RWD
#   define DS_SPD BSD43_DS_SPD
#   define DS_UNITSEL BSD43_DS_UNITSEL
#   define ERASE BSD43_ERASE
#   define ERROR BSD43_ERROR
#   define EXTEND BSD43_EXTEND
#   define EXTGAP BSD43_EXTGAP
#   define FALSE BSD43_FALSE
#   define FBUSY BSD43_FBUSY
#   define FIFORAM BSD43_FIFORAM
#   define GIVECONF BSD43_GIVECONF
#   define G_CMP BSD43_G_CMP
#   define G_ENT BSD43_G_ENT
#   define G_ERR BSD43_G_ERR
#   define HARDTERR BSD43_HARDTERR
#   define HIGHDEN BSD43_HIGHDEN
#   define HIGHSPD BSD43_HIGHSPD
#   define HITEOT BSD43_HITEOT
#   define HITFM BSD43_HITFM
#   define INITIALIZED BSD43_INITIALIZED
#   define IOPBAM BSD43_IOPBAM
#   define LARGER BSD43_LARGER
#   define LOCALMEM BSD43_LOCALMEM
#   define MOTOROLA BSD43_MOTOROLA
#   define MTIOCGCON BSD43_MTIOCGCON
#   define MTIOCSCON BSD43_MTIOCSCON
#   define MT_ISTM3000 BSD43_MT_ISTM3000
#   define NIOPB BSD43_NIOPB
#   define NOCMD BSD43_NOCMD
#   define NOGO BSD43_NOGO
#   define NOREVBOT BSD43_NOREVBOT
#   define NOREWIND BSD43_NOREWIND
#   define NORMAL BSD43_NORMAL
#   define NORMREAD BSD43_NORMREAD
#   define NORMWRITE BSD43_NORMWRITE
#   define NOTONL BSD43_NOTONL
#   define NOTRDY BSD43_NOTRDY
#   define NSC BSD43_NSC
#   define NSCBCOUNT BSD43_NSCBCOUNT
#   define NSCLENGTH BSD43_NSCLENGTH
#   define NSCP BSD43_NSCP
#   define NSRS BSD43_NSRS
#   define OFFLINE BSD43_OFFLINE
#   define ONLINE BSD43_ONLINE
#   define ONL_US BSD43_ONL_US
#   define OTEA BSD43_OTEA
#   define OTEB BSD43_OTEB
#   define P1_16_LOL BSD43_P1_16_LOL
#   define P1_36_DEN BSD43_P1_36_DEN
#   define P1_36_GAP BSD43_P1_36_GAP
#   define P1_44_GAP BSD43_P1_44_GAP
#   define P1_44_THR BSD43_P1_44_THR
#   define P2_50_DEN BSD43_P2_50_DEN
#   define P2_50_SPD BSD43_P2_50_SPD
#   define PARITY BSD43_PARITY
#   define PBINUSE BSD43_PBINUSE
#   define PBWANTED BSD43_PBWANTED
#   define PFIFO BSD43_PFIFO
#   define PREVPB BSD43_PREVPB
#   define PROMCHK BSD43_PROMCHK
#   define PROTECTED BSD43_PROTECTED
#   define PTOAI BSD43_PTOAI
#   define RAWDEV BSD43_RAWDEV
#   define RBREAD BSD43_RBREAD
#   define RBWRITE BSD43_RBWRITE
#   define RB_ABT BSD43_RB_ABT
#   define RB_BSY BSD43_RB_BSY
#   define RB_CMP BSD43_RB_CMP
#   define RB_ERR BSD43_RB_ERR
#   define RB_INT BSD43_RB_INT
#   define RB_LST BSD43_RB_LST
#   define RB_RDY BSD43_RB_RDY
#   define RDOVERRUN BSD43_RDOVERRUN
#   define RESET BSD43_RESET
#   define RETRYERR BSD43_RETRYERR
#   define REVBOT BSD43_REVBOT
#   define REWIND BSD43_REWIND
#   define RUR BSD43_RUR
#   define S2_MBI_ADDR BSD43_S2_MBI_ADDR
#   define SECURE BSD43_SECURE
#   define SFM BSD43_SFM
#   define SIGNALSERR BSD43_SIGNALSERR
#   define SMALLER BSD43_SMALLER
#   define SOFTTERR BSD43_SOFTTERR
#   define SPACE BSD43_SPACE
#   define SPEED BSD43_SPEED
#   define SPLTT BSD43_SPLTT
#   define SWAB BSD43_SWAB
#   define SWP BSD43_SWP
#   define SWPADDR BSD43_SWPADDR
#   define SWPCOUNT BSD43_SWPCOUNT
#   define TAPESIDE BSD43_TAPESIDE
#   define TELEX BSD43_TELEX
#   define THRTLERR BSD43_THRTLERR
#   define TPTEA BSD43_TPTEA
#   define TPTEB BSD43_TPTEB
#   define TPTEC BSD43_TPTEC
#   define TRANSFER BSD43_TRANSFER
#   define TREAD BSD43_TREAD
#   define TRUE BSD43_TRUE
#   define TSEEK BSD43_TSEEK
#   define TTH_ADDMOD BSD43_TTH_ADDMOD
#   define TTH_LEVEL BSD43_TTH_LEVEL
#   define TTH_VECTOR BSD43_TTH_VECTOR
#   define TWRITE BSD43_TWRITE
#   define UNIT BSD43_UNIT
#   define USED BSD43_USED
#   define UTEA BSD43_UTEA
#   define UTEB BSD43_UTEB
#   define VARERASE BSD43_VARERASE
#   define WFM BSD43_WFM
#   define WFMERR BSD43_WFMERR
#   define WIDTH32 BSD43_WIDTH32
#   define WTUNDERRUN BSD43_WTUNDERRUN
#   define WUR BSD43_WUR
#   define attention bsd43_attention
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


