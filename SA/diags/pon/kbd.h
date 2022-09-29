#ident "$Header: kbd.h,v 1.1.7.1 90/07/18 14:28:50 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/* Award Keyboard Controller (i8042) */

/* commands is interrupt flag | command */

#define	KBDNULL		1	/* pay attention to interrupt bit	*/
#define	KBDWRITE	2	/* write to keyboard			*/
#define KBDDEBUGOFF	3	/* turn V50 debugging off 		*/
#define KBDDEBUGON	4	/* turn V50 debugging on		*/
#define	KBDWRITELEDS	5	/* atomic write for leds		*/

/* interrupt flag */
#define	KBDINTR		0x8000
#define	KBDNOINTR	0x0000

#define KBDBUFSZ	16

#define KYDATA	KBD_BASE+3		/* data port			*/
#define	KYCNTL	KBD_BASE+7		/* control/status port		*/

/* commands */
#define	KYC_RCB			0x20	/* read command byte		*/
#define	KYC_WCB			0x60	/* write command byte		*/
#define	KYC_STEST		0xAA	/* execute self-test		*/
#define	KYC_ITEST		0xAB	/* execute interface test	*/
#define	KYC_DUMP		0xAC	/* diagnostic dump		*/
#define	KYC_DISABLE		0xAD	/* disable keyboard		*/
#define	KYC_ENABLE		0xAE	/* enable keyboard		*/
#define	KYC_INPUT		0xC0	/* read input port		*/
#define	KYC_OUTPUT		0xD0	/* write output port		*/
#define	KYC_TEST		0xE0	/* read test inputs		*/
#define	KYC_PULSE		0xF0	/* pulse output port		*/
#define KYC_RESET		0xFE	/* reset pulse			*/

#define KYC_READ_INPUT		0xc0	/* read input port 		*/
#define KYC_READ_OUTPUT		0xd0	/* read output port 		*/
#define KYC_WRITE_OUTPUT	0xd1 	/* write output port 		*/
#define KYC_SELF_TEST		0xaa 	/* execute selftest 		*/

/* Selftest error codes
 */
#define SELF_TST_OK		0x55 	/* selftest passed, status code	*/
#define SELF_TST_FLD		0xff 	/* selftest failed, status code	*/

/* interface test error codes
 */
#define IF_OK			0x00	/* interface test passed	*/
#define IF_CLK_L		0x01	/* clock stuck low 		*/
#define IF_CLK_H		0x02	/* clock stuck high 		*/
#define IF_DATA_L		0x03	/* data stuck low 		*/
#define IF_DATA_H		0x04	/* data stuck high 		*/


/* status byte (SB)
 */
#define	SB_OBF			0x01	/* output buffer full		*/
#define	SB_IBF			0x02	/* input buffer full		*/
#define	SB_SYSTEM_FLAG		0x04	/* system flag			*/
#define	SB_CMD_DATA		0x08	/* command/data			*/
#define	SB_INH_SWITCH		0x10	/* keyboard inhibit switch	*/
#define	SB_XMIT_TIMEOUT		0x20	/* transmit timeout		*/
#define	SB_RECV_TIMEOUT		0x40	/* receive timeout		*/
#define	SB_PARITY_ERR		0x80	/* parity error			*/

/* interlocked status bits (reset by any command) */
#define ILS_BADCMD		0x0800	/* illegal command		*/
#define ILS_DATALOST		0x1000	/* keyboard buffer overflow	*/
#define	ILS_TTO			0x2000	/* transmit timeout		*/
#define	ILS_RTO			0x4000	/* receive timeout		*/
#define	ILS_PARITY		0x8000	/* parity error			*/

/* command byte (CB)
 */
#define	CB_ENA_INTR		0x01	/* enable interrupt		*/
#define	CB_RSRVD_1		0x02	/* always 0			*/
#define	CB_SYSTEM_FLAG		0x04	/* system flag			*/
#define	CB_INH_OVERRIDE		0x08	/* inhibit override		*/
#define	CB_DIS_KBD		0x10	/* disable keyboard		*/
#define	CB_MODE0		0x20	/* one of two mode bits		*/
#define	CB_MODE1		0x40	/* one of two mode bits		*/
#define	CB_RSRVD_7		0x80	/* always 0			*/

#define CONFIG	( CB_SYSTEM | CB_MODE1 )

/*
 *  Defines for setting lights on keyboard.
 */
#define KTCLED		0xed	/* command for setting lights */
#define KTCLEDMASK	0x7	/* mask for light bits */




/*
 * SYSCON register bits are contained in port 1 and port 2 of the
 * keyboard controller(i8042). here are  the definitions of the bits.
 */

/*
 * P1, read only register
 */
#define P1_FPU_PRSNT_B	0x01		/* 0 = FPU present in the system		*/
#define P1_RSRV1	0x02		/* */ 
#define P1_RSRV2	0x04		/* */
#define P1_RSRV3	0x08		/* */
#define P1_RSRV4	0x10		/* */
#define P1_RSRV5	0x20		/* */
#define P1_RSRV6	0x40		/* */
#define P1_RSRV7	0x80		/* */

/*
 * P2, read/write register
 */
#define P2_SOFT_RESET_B	0x01		/* 0 = reset to CPU		*/
#define P2_FORCE_BADP_B	0x02		/* 0 = forces writing bad parity to memory		*/
#define P2_4MB_RAM	0x04		/* 1 = indicates to memory decoder 4Mb RAM chips are in use */
#define P2_L_ENDIAN_B	0x08		/* 0 = change to little endian byte ordering	*/
#define P2_KBD_ENA_INT	0x10		/* 1 = enable interrupt from keyboard on buffer full */
#define P2_CLR_SLOT_INT_B 0x20		/* 0 = Clear interrupt from expansion slot(RAT/VIDEO) */
#define P2_KBD_CLK	0x40		/* clock to keyboard */
#define P2_KBD_DATA	0x80		/* data to keyboard */

#ifdef LANGUAGE_C
struct _kbdiopb {
	unsigned short	command;	 /* read/write/interrupt	*/
	unsigned short	status;		 /* device specific		*/

	unsigned char	obuf;		/* output queue(single char)	*/
	unsigned char	pad;		/* output queue(single char)	*/

	short	bufsz;			/* sizeof(ibuf)			*/
	short	in,out;			/* input queue management	*/
	unsigned char ibuf[KBDBUFSZ]; /* input queue KBDBUFSZ chars	*/
};


struct kbd {
	unsigned char pad0[3];
	unsigned char data;
	unsigned char pad1[3];
	unsigned char control;
	   };

#endif LANGUAGE_C


