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
/* $Header: tsreg.h,v 1.15.4.2 90/05/10 06:42:13 wje Exp $ */

#ifndef	_SYS_TSREG_
#define	_SYS_TSREG_	1


/*
 * Device registers for the ISI VME-QIC2/X cartridge tape controller.
 *
 * $Source: /MIPSNET/dish/root/usr4/co-rel4.50/root/usr/src/uts/mips/sys/RCS/tsreg.h,v $
 * $Revision: 1.15.4.2 $
 * $Date: 90/05/10 06:42:13 $
 */

/*
 * Controller registers
 */
struct	tsdevice {
	ushort	tcpr;		/* write: low word of data buffer address */
	ushort	txcpr;		/* write: high word of data buf addr */
	ushort	clkreg;		/* read/write: clock register (not used) */
	ushort	atxcpr;		/* write: alternate dba */
};
#define	tssr	txcpr		/* read: status register */

#define	TXCPR_RESET	0x8000	/* subsystem clear */
#define	TXCPR_RETENSION	0xC000	/* tape retention */

/* bits in tssr status register */
#define	TSSR_SC		0x8000		/* special condition (error) */
#define	TSSR_RMR	0x1000		/* register modification refused */
#define	TSSR_NXM	0x0800		/* nonexistant memory */
#define	TSSR_NBA	0x0400		/* need buffer address */
#define	TSSR_SSR	0x0080		/* subsytem ready */
#define	TSSR_OFL	0x0040		/* off-line */
#define	TSSR_TC		0x000E		/* termination codes */

/* termination codes */
#define	TSTC_OK		0x0000	/* TC0: normal termination */
#define	TSTC_ATTN	0x0002	/* TC1: attention */
#define	TSTC_ALERT	0x0004	/* TC2: tape status alert */
#define	TSTC_REJECT	0x0006	/* TC3: function reject */
#define	TSTC_UNREC0	0x0008	/* TC4: unrecoverable, tape position lost */
#define	TSTC_UNREC1	0x000A	/* TC5: unrecoverable, tape position lost */
#define	TSTC_UNREC2	0x000C	/* TC6: unrecoverable, tape position lost */
#define	TSTC_FATAL	0x000E	/* TC7: fatal error */

/* status message */
struct	tsstatus {
	ushort	header;		/* packet header */
	ushort	len;		/* packet length */
	ushort	resid;		/* residual frame count */
	ushort	xs0;		/* see bits defined below */
	ushort	xs1;		/* see bits defined below */
	ushort	xs2;		/* always zero */
	ushort	xs3;		/* see bits defined below */
	ushort	pad;		/* not used */
};
/* bits header */
#define	TSX_ACK	0x8000		/* acknowledge */

/* Error codes in xstat 0 */
#define	TSXS0_BITS	\
"\20\20TMK\17RLS\16LET\15RLL\14WLE\13NEF\12ILC\11ILA\10MOT\7ONL\6IES\5VCK\4PED\3WLK\2BOT\1EOT"
#define	XS0_TMK	0x8000		/* tape mark detected */
#define	XS0_RLS	0x4000		/* record length short */
#define	XS0_LET	0x2000		/* logical end of tape */
#define	XS0_RLL	0x1000		/* record length long */
#define	XS0_WLE	0x0800		/* write lock error */
#define	XS0_NEF	0x0400		/* non-executable function */
#define	XS0_ILC	0x0200		/* illegal command */
#define	XS0_ILA	0x0100		/* illegal address */
#define	XS0_ONL	0x0040		/* on-line */
#define	XS0_IE	0x0020		/* interrupt enable status */
#define	XS0_WLK	0x0004		/* write locked */
#define	XS0_BOT	0x0002		/* beginning of tape */
#define	XS0_EOT	0x0001		/* end of tape */

/* Error codes in xstat 1 */
#define	TSXS1_BITS	\
"\20\20DLT\17-\16COR\15CRS\14TIG\13DBF\12SCK\11RBP\10IPR\7SYN\6IPO\5IED\4POS\3POL\2UNC\1MTE"
#define	XS1_UNC	0x0002		/* uncorrectable data */

/* Error codes in xstat 3 */
#define	XS3_REV	0x0020		/* tape motion is reverse */
#define	XS3_RIB	0x0001		/* reverse into BOT */

/* command message */
struct tscmd {
	ushort	cmd;		/* command */
	ushort	balo;		/* low order buffer address */
	ushort	bahi;		/* high order buffer address */
	ushort	bytecount;
};

/* commands and command bits */
#define	TSCMD_ACK	0x8000		/* ack - release command packet */
#define	TSCMD_MUSTBEONE	0x4000		/* must be one */
#define TSCMD_SWB_	0x1000		/* don't swap bytes (active low) */
#define	TSCMD_IE	0x0080		/* interrupt enable */

#define	TSCMD_READ	0x0001		/* read data */
#define	TSCMD_WRITECHAR	0x0004		/* write characteristics */
#define	TSCMD_WRITE	0x0005		/* write data */
#define	TSCMD_FSR	0x0008		/* forward space record */
#define	TSCMD_FSF	0x0208		/* forward space file */
#define	TSCMD_REWIND	0x0408		/* rewind */
#define	TSCMD_WFMK	0x0009		/* write file mark */
#define	TSCMD_RESET	0x000B		/* initialize controller */
#define	TSCMD_SENSE	0x000F		/* get status */
#define	TSCMD_CMD	0x0F1F		/* bits that commands use */
#define	TSCMD_RETENSION	0x7FFF		/* retension pseudo-command */

/* characteristics data */
struct tschar {
	ushort	balo;			/* low address of status packet */
	ushort	bahi;			/* high address of status packet */
	ushort	bytecount;		/* its size */
	ushort	mode;			/* characteristics */
};

/* bits in characteristics mode */
#define	TSCHAR_ESS	0x0080		/* enable skip tape marks stop */
#define	TSCHAR_ENB	0x0040		/* enable tape mark stop off BOT */
#define	TSCHAR_RAW	0x0008		/* enable non-block mode */
#define	TSCHAR_Q11	0x0004		/* use qic-11 mode */
#define	TSCHAR_FMT	0x0002		/* enable format */

/*
 * Software state per tape drive.
 */
#define	TSBUFSIZE	16384
struct	tssoftc {
	short	sc_flags;		/* state flags */
	char	sc_ctlr;		/* controller # */
	int	sc_id;			/* timeout id */
	volatile struct tsdevice *sc_device;
	struct	tscmd sc_cmd;		/* the command packet */
	struct	tschar sc_char;		/* characteristics packet */
	struct	tsstatus sc_status;	/* status block */
};

/* software state of the drive */
#define	SC_PROBED	0x0001	/* controller probed */
#define	SC_INITIALIZED	0x0002	/* has been initialized */
#define	SC_OPEN		0x0004	/* device is open and in use */
#define	SC_BUSY		0x0008	/* busy */
#define	SC_WAITING	0x0010	/* somebody want's a wakeup */
#define	SC_WRITTEN	0x0020	/* tape has been written */
#define	SC_READ		0x0040	/* tape has been read */
#define	SC_BUSTED	0x0080	/* things are busted/hard error */
#define	SC_REWINDING	0x0100	/* tape is rewinding */
#define	SC_ERROR	0x0200	/* error on last command */
#define	SC_FILEMARK	0x0400	/* file mark seen */
#define SC_FORMAT	0x0800	/* This bit will indicate if the drive */
				/* is a raw (no header) tape.  We need */
				/* to reset the drive if someone opens */
				/* the drive for the other format */
#define SC_FORMAT_SHFT	11	/* Need to compare SC_FORMAT with the RAWDEV */
				/* bit to see if thier the same.  To do this */
				/* the bits must be lsb location */
#define SC_Q24		0x1000  /* Indicates we are currently doing qic24 */
#define SC_Q24_SHFT	12	/* Shifts SC_Q24 bit to lsb */
#define SC_Q11		0x2000  /* Indicates we are currently doing qic11 */
#define SC_Q11_SHFT	13	/* Shifts SC_Q11 bit to lsb */
#define	SC_EOT		0x4000	/* Tells us we have hit EOT, return ENXIO */

#ifdef	INKERNEL
extern	struct tssoftc tssoftc[];
extern	int tsctlrs;
extern	char tsbuf[][TSBUFSIZE];
#endif

#endif	_SYS_TSREG_
