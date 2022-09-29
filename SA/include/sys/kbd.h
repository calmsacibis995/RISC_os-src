#ident "$Header: kbd.h,v 1.1 90/02/28 11:32:59 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/* here is the preliminary keyboard control structure. JMP *

/* Jupiter keyboard */

/* commands is interrupt flag | command */

#define	KBDNULL		1	/* pay attention to interrupt bit	*/
#define	KBDWRITE	2	/* write to keyboard			*/
#define KBDDEBUGOFF	3	/* turn V50 debugging off 		*/
#define KBDDEBUGON	4	/* turn V50 debugging on		*/
#define KBDWRITELEDS	5	/* atomic write for leds		*/

/* interrupt flag */
#define	KBDINTR		0x8000
#define	KBDNOINTR	0x0000

#define KBDBUFSZ	16

struct _kbdiopb {
	unsigned short	command;	 /* read/write/interrupt	*/
	unsigned short	status;		 /* device specific		*/

	unsigned char	obuf;		/* output queue(single char)	*/
	unsigned char	pad;		/* output queue(single char)	*/

	short	bufsz;			/* sizeof(ibuf)			*/
	short	in,out;			/* input queue management	*/
	unsigned char ibuf[KBDBUFSZ]; /* input queue KBDBUFSZ chars	*/
};




#define KYDATA	0xC0			/* data port			*/
#define	KYCNTL	0xC4			/* control/status port		*/

/* commands */
#define	KYC_RCB			0x20	/* read command byte		*/
#define	KYC_WCB			0x60	/* write command byte		*/
#define	KYC_STEST		0xAA	/* execute self-test		*/
#define	KYC_ITEST		0xAB	/* execute interface test	*/
#define	KYC_DUMP		0xAC	/* diagnostic dump		*/
#define	KYC_DISABLE		0xAD	/* disable keyboard		*/
#define	KYC_ENABLE		0xAE	/* enable keyboard		*/
#define	KYC_INPUT		0xC0	/* read input port		*/
#define	KYC_OUTPUT		0xD0	/* read "output" port		*/
#define	KYC_WOUT		0xD1	/* write "output" port		*/
#define	KYC_TEST		0xE0	/* read test inputs		*/
#define	KYC_PULSE		0xF0	/* pulse output port		*/
#define KYC_RESET		0xFE	/* reset pulse			*/

/* status bits */
#define	KYS_OBF			0x01	/* output buffer full		*/
#define	KYS_IBF			0x02	/* input buffer full		*/
#define	KYS_SYSTEM		0x04	/* system flag			*/
#define	KYS_CD			0x08	/* command/data			*/
#define	KYS_ENABLED		0x10	/* keyboard enabled bit		*/
#define	KYS_TTO			0x20	/* transmit timeout		*/
#define	KYS_RTO			0x40	/* receive timeout		*/
#define	KYS_PARITY		0x80	/* parity error			*/

/* interlocked status bits (reset by any command) */
#define ILS_BADCMD		0x0800	/* illegal command		*/
#define ILS_DATALOST		0x1000	/* keyboard buffer overflow	*/
#define	ILS_TTO			0x2000	/* transmit timeout		*/
#define	ILS_RTO			0x4000	/* receive timeout		*/
#define	ILS_PARITY		0x8000	/* parity error			*/

/* command byte */
#define	CB_ENINTR		0x01	/* enable interrupt		*/
#define	CB_SYSTEM		0x04	/* system flag			*/
#define	CB_INH_OVERRIDE		0x08	/* inhibit override		*/
#define	CB_DISABLE		0x10	/* disable keyboard		*/
#define	CB_MODE0		0x20	/* one of two mode bits		*/
#define	CB_MODE1		0x40	/* one of two mode bits		*/

#define CONFIG	( CB_SYSTEM | CB_MODE1 )

/*
 *  Defines for setting lights on keyboard.
 */

#define KTCLED	0xed		/* command for setting lights */
#define KTCLEDMASK	0x7	/* mask for light bits */

/*
 *  Defines for RX3030 functions provided by the keyboard "output" port bits
 */
#define	KYB_SOFT		0x01	/* soft reset bits (ALWAYS 1)	*/
#define	KYB_BADP		0x02	/* force bad parity		*/
#define	KYB_MEMTYPE		0x04	/* simm type (1 = 4Mb, 0 = 1Mb)	*/
#define	KYB_ENDIAN		0x08	/* big/little endian (1 = big)	*/
#define	KYB_INT			0x10	/* enable keyboard interrupt	*/
#define	KYB_SLOTCLR		0x20	/* clear expansion interrupt	*/
