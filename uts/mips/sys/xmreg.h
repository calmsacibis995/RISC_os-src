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
/* $Header: xmreg.h,v 1.8.4.2 90/05/10 06:45:24 wje Exp $ */

#ifndef	_SYS_XMREG_
#define	_SYS_XMREG_	1


/*
 * definitions for xm.c
 */

# define XM_MAXCTLR	2		/* # of max ctlr */
# define XM_MAXDRIVES	8		/* max # of drives per ctlr */


/* xm size limits */
# define XM_RWLIM	(63*1024)	/* limit for normal r/w */
# define XM_IOLIM	XM_RWLIM	/* limit for extended r/w */
# define XM_MAXSG	32		/* max # of sg entries per req */

/* XXX Hack for compiler bug */
#ifdef mips	/* was sgi */
#undef btoc
#define	btoc(x)	(((x)+(NBPC-1))>>BPCSHIFT)
#endif sgi

# define XM_NSG		(btoc(XM_IOLIM)+1) /* # of sg entries for XM_IOLIM */
# define XM_ICNT	8		/* # iopb's per drive */
# define XM_ISIZE	0x1A		/* # bytes of real iopb */


/*
 * device scatter/gather entry
 */
struct xm_sg {
	unchar sg_bchi;			/* byte count, hi */
	unchar sg_bclo;			/* byte count, lo */
	unchar sg_junk;			/* - */
	unchar sg_mod;			/* i/o address modifier */
	unchar sg_ba0;			/* i/o address, byte 0 */
	unchar sg_ba1;			/* i/o address, byte 1 */
	unchar sg_ba2;			/* i/o address, byte 2 */
	unchar sg_ba3;			/* i/o address, byte 3 */
};


/*
 * xm i/o parameter block.
 */
struct xm_iopb {
        unchar i[XM_ISIZE];		/* real iopb */
	ushort drv;			/* associated drive */
	struct xm_iopb *next;		/* free chain */
};


/*
 * per-drive status information.
 */
struct xm_drive {
	int d_lock;			/* lock for private variables */
	struct xm_iopb *d_rawipb;	/* private ptr to raw iopb */
	struct buf d_rawbuf;		/* private buf header */
	struct xm_sg d_rawsg[XM_NSG];	/* private sg entries */

        ushort d_status;		/* open, present, etc */
	ushort d_iostate;		/* active, retrying, etc */
	ushort d_retries;		/* # of retries on current buf */
	daddr_t d_rec;			/* current r/w rec */
	daddr_t d_wrec;			/* last written offset */
	daddr_t d_fileno;		/* current fileno */

	struct buf d_tab;		/* i/o queue per drive */
	struct xm_iopb *d_curipb;	/* ptr to current iopb */
	dev_t d_drv;			/* drive # */
	dev_t d_curdev;			/* current file semantics */

	unchar d_wprot;			/* is currently w protected */
	unchar d_bot;			/* is at bot */
	unchar d_online;		/* is currently online */
	unchar d_dens;			/* current density */
	unchar d_eot;			/* is at eot */
	unchar d_atfmark;		/* is at fmark */
	struct xm_iopb d_params;	/* current param iopb */

	struct xm_softc *d_ci;		/* ptr to ctlr info */
	struct xm_iopb d_iopbs[XM_ICNT]; /* iopbs */
	struct xm_iopb *d_free;		/* free iopbs */
	struct xm_sg d_sg[XM_NSG];	/* normal sg entries */
	struct xm_drive *d_cchain;	/* chain of drives on this ctlr */
};


/*
 * per-ctlr status information
 */
struct xm_softc {
	int c_lock;			/* lock for ctlr variables */
        ushort c_status;		/* ctlr status flags */
	ushort c_opens;			/* open bits */
	struct xm_iopb c_params;	/* current param iopb */
	ushort c_unit;			/* ctlr # */
	unchar *c_base;			/* registers */
	struct xm_drive *c_drives;	/* drives on this ctlr */
};


# ifdef XM_DRIVER

/* system definitions */
# define VOLATILE	volatile
# define WBFLUSH	wbflush


/* covert bytes to / from short */
# define BCHI(x)	((unchar)((x)>>8))
# define BCLO(x)	((unchar)((x)>>0))
# define BCCONS(h, l)	((h)<<8|(l))

/* convert bytes to / from addr */
# define ADDR0(x)	((unchar)((unsigned long)(x)>>(3*8)))
# define ADDR1(x)	((unchar)((unsigned long)(x)>>(2*8)))
# define ADDR2(x)	((unchar)((unsigned long)(x)>>(1*8)))
# define ADDR3(x)	((unchar)((unsigned long)(x)>>(0*8)))
# define ADDRCONS(a, b, c, d) \
			((((a)<<8|(b))<<8|(c))<<8|(d))


/*
 * the minor device number is decoded as follows:
 *	no-rewind		7:7
 *	density			4:6
 *	ctlr			3:3
 *	drive			0:2
 *
 * density 3 is a raw device for unhanging the driver.
 */
# define DEVNOREWIND(dev)	((minor(dev)>>7)&01)
# define DEVDENS(dev)		((minor(dev)>>4)&07)
# define DEVCTLR(dev)		((minor(dev)>>3)&01)
# define DEVDRIVE(dev)		((minor(dev)>>0)&017)
# define DEVSLAVE(dev)		((minor(dev)>>0)&07)
# define DEBUGSLAVE(dev)	(DEVDENS(dev)==7)

/* definitions for locking private variables */
# define XMPRI		PUSER		/* sleep priority, killable */
# define LOCKBUSY	(1<<0)		/* lock busy flag */
# define LOCKWANTED	(1<<1)		/* lock wanted */

/* interrupt priority */
# define RAISE		(s = spl5())
# define USEPRI		register int s
# define LOWER		splx(s)
# define PHYSPACE	VME_A32NPAMOD


/*
 * xm register offsets
 */
# define XM_AR3		0x1		/* iopb address, byte 3 (lsb) */
# define XM_AR2		0x3		/* iopb address, byte 2 */
# define XM_AR1		0x5		/* iopb address, byte 1 */
# define XM_AR0		0x7		/* iopb address, byte 0 (msb) */
# define XM_AMOD	0x9		/* iopb address modifier */
# define XM_SR		0xB		/* status */
# define XM_CR		0xB		/* control */
# define XM_FERR	0xD		/* fatal error */

# define XM_OUTREG(ci, r, v)	(((VOLATILE unchar *)(ci)->c_base)[r]=(v))
# define XM_INREG(ci, r)	(((VOLATILE unchar *)(ci)->c_base)[r])

/* XM_AMOD register, bit fields */
# define AMOD_PRIO	(0x1<<7)	/* priority flag */
# define AMOD_AMOD	(0x7F<<0)	/* address modifier mask */

/* XM_CR register, bit fields */
# define CR_RMM		(1<<7)		/* register maint mode */
# define CR_MM		(1<<5)		/* enable maint mode */
# define CR_RST		(1<<3)		/* ctlr reset */
# define CR_AIO		(1<<2)		/* add iopb */
# define CR_CRIO	(1<<1)		/* clear rio */
# define CR_CRBS	(1<<0)		/* clear register busy semaphore */

/* XM_SR register, bit fields */
# define SR_BUSY	(1<<7)		/* ctlr busy */
# define SR_FERR	(1<<6)		/* fatal h/w error */
# define SR_MMA		(1<<5)		/* maint mode active */
# define SR_RSTA	(1<<3)		/* ctlr reset active */
# define SR_AIOP	(1<<2)		/* AIO pending */
# define SR_RIO		(1<<1)		/* remove IOPB */
# define SR_RBS		(1<<0)		/* register busy semaphore */

/* XM_FERR register, values */
# define FERR_CSF	0xE0		/* IRAM checksum failure */
# define FERR_PEST1	0xE1		/* powerup selftest 1 */
# define FERR_PESTF	0xEF		/* powerup selftest F */
# define FERR_IOCF	0xF0		/* IOPB checksum failure */
# define FERR_IODMAF	0xF1		/* IOPB DMA failure */
# define FERR_IOAAF	0xF2		/* IOPB addr alignment failure */
# define FERR_FWF	0xF3		/* firmware failure */
# define FERR_CTF	0xF4		/* cable test failure */
# define FERR_IMM	0xF5		/* illegal maint mode test # */
# define FERR_ACFAIL	0xF6		/* ACFAIL asserted */


/*
 * offsets and values for the "standard" iopb
 */
# define IO_CMD			0x00		/* cmd byte */
#	define CMD_MASK		(0xF<<0)	/* cmd mask */
#	define CMD_SGM		(1<<4)		/* scatter / gather enable */
#	define CMD_CHEN		(1<<5)		/* iopb chain enable */
#	define CMD_DONE		(1<<6)		/* (r) iopb done */
#	define CMD_ERRS		(1<<7)		/* (r) iopb error flag */
# define IO_ERRNO		0x01		/* completion code */
#	define E_OK			0x00
#	define E_DENS_CHANGE		0x10
#	define E_ILL_PARAM		0x11
#	define E_ODD			0x12
#	define E_ZERO			0x13
#	define E_ILL_CMD		0x14
#	define E_ILL_SGL		0x1C
#	define E_IOPB_ALIGN		0x1E
#	define E_BHT			0x21
#	define E_CORRECTED		0x30
#	define E_UNCORR_TAPE		0x40
#	define E_EOT			0x41
#	define E_WFMK			0x42
#	define E_OPTIMO			0x43
#	define E_DMACTIMO		0x44
#	define E_TAPE_PARITY		0x45
#	define E_FIFO_PARITY		0x46
#	define E_REC_LONG		0x47
#	define E_REC_SHORT		0x48
#	define E_DATA_LATE		0x49
#	define E_FATAL_DMAC		0x4A
#	define E_VMEBUS			0x4B
#	define E_CONTINUE_RELOAD	0x4C
#	define E_DFAULT			0x60
#	define E_DNOTREADY		0x61
#	define E_FIRMWARE		0x71
#	define E_IRAM_CHECKSUM		0x81
#	define E_IOPB_ABORT		0x82
#	define E_IOPB_ERROR		0x83
#	define E_WPROT			0x90
#	define E_OFFLINE		0x91
#	define E_BOT			0xA0
#	define E_RFMK			0xA1
# define IO_STAT2		0x02		/* status byte 1 */
#	define STAT2_HER	(1<<7)
#	define STAT2_CER	(1<<6)
#	define STAT2_RLS	(1<<5)
#	define STAT2_RLL	(1<<4)
#	define STAT2_FMK	(1<<3)
#	define STAT2_PEID	(1<<2)
#	define STAT2_EOT	(1<<1)
#	define STAT2_WPT	(1<<0)

# define IO_STAT3		0x03		/* status byte 2 */
#	define STAT3_GCNRZ	(1<<7)
#	define STAT3_HISPD	(1<<6)
#	define STAT3_BOT	(1<<5)
#	define STAT3_REW	(1<<4)
#	define STAT3_DBSY	(1<<3)
#	define STAT3_FBSY	(1<<2)
#	define STAT3_DRRDY	(1<<1)
#	define STAT3_ONLINE	(1<<0)

# define IO_FUNC		0x04		/* subfunction */

# define IO_UNIT		0x05		/* unit etc */
#	define UNIT_MASK	(0x7<<0)
#	define BHT		(1<<4)

# define IO_LEVEL		0x06
#	define LEVEL_MASK	(0x7<<0)
#	define LEVEL_NSG_SHIFT	3
#	define LEVEL_NSG_MASK	(0x1F<<LEVEL_NSG_SHIFT)

# define IO_IVEC		0x07		/* interrupt vector */

# define IO_BCHI		0x08		/* byte count, hi */
# define IO_BCLO		0x09		/* byte count, lo */

# define IO_IGNERRNO		0x0A		/* ignored error errno */
# define IO_NIGN		0x0B		/* # of ignored errors */

# define IO_ACHI		0x0C		/* actual count, hi */
# define IO_ACLO		0x0D		/* actual count, lo */

# define IO_DATAMOD		0x0E		/* data address modifier */
# define IO_LINKMOD		0x0F		/* link address modifier */

# define IO_DATA0		0x10		/* data address, byte 0 */
# define IO_DATA1		0x11		/* data address, byte 1 */
# define IO_DATA2		0x12		/* data address, byte 2 */
# define IO_DATA3		0x13		/* data address, byte 3 */

# define IO_LINK0		0x14		/* link address, byte 0 */
# define IO_LINK1		0x15		/* link address, byte 1 */
# define IO_LINK2		0x16		/* link address, byte 2 */
# define IO_LINK3		0x17		/* link address, byte 3 */

# define IO_CHI			0x18		/* checksum, hi */
# define IO_CLO			0x19		/* checksum, lo */


/*
 * offsets and values for the "controller parameters" iopb
 */
# define IO_CPARA		0x08		/* ctlr parameters a */
#	define CPARA_AUD	(1<<7)
#	define CPARA_TMOD	(1<<6)
#	define CPARA_ICS	(1<<4)
#	define CPARA_EDT	(1<<3)
#	define CPARA_NPRM	(1<<2)
#	define CPARA_AIOR_SHIFT	0
#	define CPARA_AIOR_MASK	(03<<CPARA_AIOR_SHIFT)

# define IO_CPARB		0x09
#	define CPARB_TDT_SHIFT	6
#	define CPARB_TDT_MASK	(03<<CPARB_TDT_SHIFT)
#	define CPARB_ROR	(1<<4)

# define IO_CPARC		0x0A
#	define CPARC_OVR	(1<<7)
#	define CPARC_WWD	(1<<6)
#	define CPARC_IEC	(1<<5)

# define IO_CTHROTTLE		0x0B

# define IO_CTYPE		0x0E

# define IO_CEPROM0		0x10
# define IO_CEPROM1		0x11

# define IO_CFWREV		0x12
# define IO_CSUBREV		0x13


/*
 * offsets and values for the "drive parameters" iopb
 */
# define IO_DPARA		0x08
#	define DPARA_IRER	(1<<7)
#	define DPARA_IWER	(1<<6)
#	define DPARA_RRTY	(1<<5)
#	define DPARA_WRTY	(1<<4)
#	define DPARA_GRTY	(1<<3)
#	define DPARA_DSB	(1<<2)
#	define DPARA_SWWD	(1<<1)
#	define DPARA_SWBY	(1<<0)

# define IO_DPARB		0x09
#	define DPARB_LGAP	(1<<7)
#	define DPARB_IPOW	(1<<6)
#	define DPARB_SPD	(1<<5)
#	define DPARB_ASS	(1<<4)
#	define DPARB_DENS_SHIFT	0
#	define DPARB_DENS_MASK	(03<<DPARB_DENS_SHIFT)
#	define DPARB_DENS_BASE	4
#		define DENS_X0	(0<<DPARB_DENS_SHIFT)
#		define DENS_X1	(1<<DPARB_DENS_SHIFT)
#		define DENS_LO	(2<<DPARB_DENS_SHIFT)
#		define DENS_HI	(3<<DPARB_DENS_SHIFT)
#		define DENS_800	 (4<<DPARB_DENS_SHIFT)
#		define DENS_1600 (5<<DPARB_DENS_SHIFT)
#		define DENS_3200 (6<<DPARB_DENS_SHIFT)
#		define DENS_6250 (7<<DPARB_DENS_SHIFT)

# define IO_D800		0x0A
# define IO_D1600		0x0B
# define IO_D3200		0x0C
# define IO_D6250		0x0D
# define IO_DENS		0x0A
#	define DENS_LGAP	(1<<7)
#	define DENS_THR1	(1<<6)
#	define DENS_HSP		(1<<5)
#	define DENS_ERASE	(1<<4)
#	define DENS_WFM		(1<<3)
#	define DENS_EDIT	(1<<2)
#	define DENS_WRITE	(1<<1)
#	define DENS_REVERSE	(1<<0)

# define IO_DIRG		0x0E
# define IO_DBUSYTIMER		0x10

/*
 * iopb command codes and subfunctions
 */
# define CMD_NOP	0x00

# define CMD_WRITE	0x01

# define CMD_READ	0x02

# define CMD_DRESET	0x04
#	define DRESET_HALT	0x00
#	define DRESET_LOAD	0x20
#	define DRESET_REWIND	0x21
#	define DRESET_UNLOAD	0x22

# define CMD_WPAR	0x05
#	define WPAR_CTLR	0x00
#	define WPAR_DRIVE	0x01

# define CMD_RPAR	0x06
#	define RPAR_CTLR	0x00
#	define RPAR_DRIVE	0x01
#	define RPAR_SENSE	0x40
#	define RPAR_EXT		0x41
#	define RPAR_DENS	0x42

# define CMD_XWRITE	0x07
#	define XWRITE_FMK	0x20
#	define XWRITE_GAP	0x21
#	define XWRITE_CONT	0x22

# define CMD_XREAD	0x08
#	define XREAD_CONT	0x40

# define CMD_ABORT	0x0A

# define CMD_SPAR		0x0B
#	define SPAR_NOP		0x40
#	define SPAR_LODENS	0x42
#	define SPAR_HIDENS	0x43
#	define SPAR_D800	0x44
#	define SPAR_D1600	0x45
#	define SPAR_D3200	0x46
#	define SPAR_D6250	0x47
#	define SPAR_LONORM	0x48
#	define SPAR_HINORM	0x49
#	define SPAR_LOLONG	0x4A
#	define SPAR_HILONG	0x4B

# define CMD_PASSTHRU	0x0C
#	define PASSTHRU_CODE	0x40

# define CMD_SEEK		0x03
#	define SEEK_RECFORW	0x20
#	define SEEK_RECBACK	0x21
#	define SEEK_FILEFORW	0x40
#	define SEEK_FILEBACK	0x41
#	define SEEK_MULTI	0x42


# define RAWBUF(dp)	(&(dp)->d_rawbuf)
# define SETRAWBUF(bp)	((bp)->b_flags=(bp)->b_error=(bp)->b_resid=0)
# define ISRAWBUF(bp)	((bp) == RAWBUF(dp))

/* drive status flags */
# define DRV_OPEN	(1<<0)		/* is open for autorew */
# define DRV_INITED	(1<<1)		/* was inited */
# define DRV_WRITTEN	(1<<2)		/* was written */
# define DRV_ACTIVE	(1<<3)		/* is doing a command */

/* drive iostate values */
# define DRV_IO		0		/* normal i/o */
# define DRV_CMD	1		/* doing a cmd */
# define DRV_PRESEEK	2		/* preseek for cooked i/o */
# define DRV_RETRYSEEK	3		/* seek for retry cooked i/o */

/* ctlr status flags */
# define CTLR_INITED	(1<<1)
# define CTLR_ERROR	(1<<2)
# define CTLR_GONG	(1<<3)

/* internal driver command codes */
#define C_LOAD		1	/* load tape */
#define C_REWIND	2	/* rewind tape */
#define C_UNLOAD	3	/* unload tape */
#define C_DRESET	4	/* ordinary RESET */
#define C_DENS		6	/* set density */
#define C_WFMARK	7	/* write file mark */
#define C_CINIT		9	/* init ctlr params */
#define C_DINIT		10	/* init drive params */
#define C_FWDFILE	12	/* space forward file */
#define C_REVFILE	13	/* space reverse file */
#define C_FWDREC	14	/* space forward record */
#define C_REVREC	15	/* space reverse record */
#define C_NOP		16	/* no op */
#define C_READPARAMS	17	/* read ctlr parameters */


typedef struct xm_iopb IOPB;
typedef struct xm_drive DRINFO;
typedef struct xm_softc CINFO;

# endif XM_DRIVER

#endif	_SYS_XMREG_
