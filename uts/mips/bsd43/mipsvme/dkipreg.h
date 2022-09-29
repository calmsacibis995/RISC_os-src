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
/* $Header: dkipreg.h,v 1.7.1.2 90/05/10 04:43:57 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * dkipreg.h -- register definitions for Interphase VME 3200 SMD controller
 */

/*
 * format of Interphase VME 3200 i/o parameter block
 * NOTE: assumed to be accessed via a D16 data path
 */
struct bsd43_(ipiopb) {
#ifdef MIPSEL
	u_char	ippb_cmdopt;		/* command options */
	u_char	ippb_cmdcode;		/* command code */
	u_char	ippb_errcode;		/* error code */
	u_char	ippb_statcode;		/* status code */
	union {
		struct {
			u_short	idp_cyl;	/* cylinder */
			u_char	idp_sec;	/* sector */
			u_char	idp_trk;	/* track */
		} id_pda;		/* physical disk address */
		struct {
			u_short idl_lbnhi;	/* logical block high */
			u_short idl_lbnlo;	/* logical block low */
		} id_lda;		/* logical disk address */
	} ippb_diskaddr;		/* disk address */
	u_short	ippb_scnt;		/* sector count */
	u_short	ippb_bahi;		/* buffer address high */
	u_short	ippb_balo;		/* buffer address low */
	u_char	ippb_addrmod;		/* VME address modifier */
	u_char	ippb_memtype;		/* memory type */
	u_char	ippb_normvec;		/* normal completion intr vector */
	u_char	ippb_ipl   :4;		/* interrupt priority level */
	u_char	ippb_drive :4;		/* drive number if 4 unit operation */
	u_char	ippb_errvec;		/* error completion intr vector */
	u_char	ippb_dmaburst;		/* dma burst length */
	u_short	ippb_iopbhi;		/* next iopb address high */
	u_short	ippb_iopblo;		/* next iopb address low */
	u_char	ippb_pbaddrmod;		/* iopb VME address modifier */
	u_char	ippb_pbmemtype;		/* iopb memory type */
	u_char	ippb_sgentries;		/* number of scatter/gather entries
					 * in the first scatter/gather list
					 */
	u_char	ippb_skew;		/* spiral skew */
#endif MIPSEL
#ifdef MIPSEB
	u_char	ippb_cmdcode;		/* command code */
	u_char	ippb_cmdopt;		/* command options */
	u_char	ippb_statcode;		/* status code */
	u_char	ippb_errcode;		/* error code */
	union {
		struct {
			u_short	idp_cyl;	/* cylinder */
			u_char	idp_trk;	/* track */
			u_char	idp_sec;	/* sector */
		} id_pda;		/* physical disk address */
		struct {
			u_short idl_lbnhi;	/* logical block high */
			u_short idl_lbnlo;	/* logical block low */
		} id_lda;		/* logical disk address */
	} ippb_diskaddr;		/* disk address */
	u_short	ippb_scnt;		/* sector count */
	u_short	ippb_bahi;		/* buffer address high */
	u_short	ippb_balo;		/* buffer address low */
	u_char	ippb_memtype;		/* memory type */
	u_char	ippb_addrmod;		/* VME address modifier */
	u_char	ippb_drive :4;		/* drive number if 4 unit operation */
	u_char	ippb_ipl   :4;		/* interrupt priority level */
	u_char	ippb_normvec;		/* normal completion intr vector */
	u_char	ippb_dmaburst;		/* dma burst length */
	u_char	ippb_errvec;		/* error completion intr vector */
	u_short	ippb_iopbhi;		/* next iopb address high */
	u_short	ippb_iopblo;		/* next iopb address low */
	u_char	ippb_pbmemtype;		/* iopb memory type */
	u_char	ippb_pbaddrmod;		/* iopb VME address modifier */
	u_char	ippb_skew;		/* spiral skew */
	u_char	ippb_sgentries;		/* number of scatter/gather entries 
					 * in the first scatter/gather list
					 */
#endif MIPSEB
};

/*
 * shorthand for disk address
 */
#define	bsd43_ippb_lbnhi	ippb_diskaddr.id_lda.idl_lbnhi
#define	bsd43_ippb_lbnlo	ippb_diskaddr.id_lda.idl_lbnlo

#define	bsd43_ippb_cyl	ippb_diskaddr.id_pda.idp_cyl
#define	bsd43_ippb_trk	ippb_diskaddr.id_pda.idp_trk
#define	bsd43_ippb_sec	ippb_diskaddr.id_pda.idp_sec

/*
 * maximum number of scatter/gather entries which can fit 
 * into the on-board memory. On-board memory size is 512;
 * 32 bytes are taken up by the iopb and status registers
 * leaving 480 bytes for scatter/gather entries. Each entry 
 * is 8 bytes, thus allowing for 60 entries onboard. However,
 * we must leave room for the 4 unit operation status register
 * and the optional status change register, thus allowing 59
 * scatter/gather entries.
 * NOTE: Due to performance considerations, it is desirable to use
 * the onboard memory for the scatter/gather information.
 * 
 */
#define BSD43_MAX_SGENTRY	59

/*
 * scatter/gather entry.
 * An entry can describe the memory area being used for scatter/gather,
 * or it can be a link entry describing the next scatter/gather list.
 * If the entry is a link, ipsg_link==1, ipsg_count is the number of
 * scatter/gather entries in the next list, and the rest of the fields
 * describe the memory location of the next list.
 *
 * A few guidelines must be observed when using scatter/gather:
 *	- the scatter/gather list must be less than or equal to
 *		the number of bytes per sector
 *	- all elements within a list must contain the same memory
 *		type and address modifier
 *	- the byte count must be a multiple of the bytes/sector
 */
struct bsd43_(ipsge) {
#ifdef MIPSEL
	u_short	ipsg_count;		/* byte/entry count */
	u_short	ipsg_addrhi;		/* datablock/entry address high */
	u_short	ipsg_addrlo;		/* datablock/entry address low */
	u_char	ipsg_addrmod;		/* VME address modifier */
	u_char	ipsg_memtype :3;	/* memory type */
	u_char	ipsg_mbz     :4;	/* RESERVED */
	u_char	ipsg_link    :1;	/* link specifier */
#endif MIPSEL
#ifdef MIPSEB
	u_short	ipsg_count;		/* byte/entry count */
	u_short	ipsg_addrhi;		/* datablock/entry address high */
	u_short	ipsg_addrlo;		/* datablock/entry address low */
	u_char	ipsg_link    :1;	/* link specifier */
	u_char	ipsg_mbz     :4;	/* RESERVED */
	u_char	ipsg_memtype :3;	/* memory type */
	u_char	ipsg_addrmod;		/* VME address modifier */
#endif MIPSEB
};

/*
 * command codes
 * (ippb_cmdcode)
 */
#define	BSD43_IP_DIAG		0x70		/* run power-up diagnostics */
#define	BSD43_IP_READLONG	0x71		/* read data and ecc */
#define	BSD43_IP_WRITELONG	0x72		/* write data and ecc */
#define	BSD43_IP_READHDR	0x74		/* read first encountered hdr */
#define BSD43_IP_READRAW	0x75		/* read raw data */
#define BSD43_IP_READCDCFLAW	0x76		/* read cdc flaw map */
#define	BSD43_IP_GETCONFIG	0x77		/* get uib configuration */
#define	BSD43_IP_WRITESBUF	0x78		/* write sector buffer */
#define	BSD43_IP_READSBUF	0x79		/* read sector buffer */
#define	BSD43_IP_READ		0x81		/* read sectors */
#define	BSD43_IP_WRITE	0x82		/* write sectors */
#define	BSD43_IP_VERIFY	0x83		/* verify sectors */
#define	BSD43_IP_FORMATTRK	0x84		/* format track */
#define	BSD43_IP_MAPTRK	0x85		/* forward track */
#define	BSD43_IP_HANDSHAKE	0x86		/* get firmware id */
#define	BSD43_IP_INITIALIZE	0x87		/* set uib configuration */
#define	BSD43_IP_RECAL	0x89		/* rtz and recalibrate drive */
#define	BSD43_IP_SEEK		0x8A		/* seek to cylinder */
#define	BSD43_IP_REFORMAT	0x8B		/* format preserving track forwards */
#define	BSD43_IP_FMTDATA	0x8C		/* format and write data */
#define	BSD43_IP_MAPSEC	0x90		/* forward sector */
#define BSD43_IP_READSEQ	0x91		/* read sectors sequential */
#define BSD43_IP_WRITESEQ	0x92		/* write sectors sequential */
#define BSD43_IP_VERIFYSEQ	0x93		/* verify sectors sequential */
#define	BSD43_IP_READNC	0x94		/* read bypassing controller cache */
#define BSD43_IP_READSEQDA	0x95		/* read sequential, disable addr */
#define BSD43_IP_WRITESEQDA	0x96		/* write sequential, disable addr */
#define BSD43_IP_CLRFAULT	0x97		/* clear drive fault */
#define	BSD43_IP_VERIFYTRK	0x99		/* verify trk readable w/o ecc errs */
#define	BSD43_IP_TRACKID	0x9A		/* return sec hdrs in order encntrd */
#define	BSD43_IP_EXECIOPB	0x9B		/* execute linked iopb's */
#define BSD43_IP_VERIFYTRKSEQ	0x9C		/* verify track sequential */
#define BSD43_IP_EDIAGS	0x9E		/* extended diagnostics */
#define BSD43_IP_DPPRISEL	0x9F		/* dual port priority select */
#define BSD43_IP_SCATTER	0xA1		/* read sectors & scatter to memory */
#define BSD43_IP_GATHER	0xA2		/* gather from memory & write sectors */

/*
 * command options
 * (ippb_cmdopt)
 */
#define	BSD43_IPO_ECCEN	0x01		/* enable ecc correction */
#define	BSD43_IPO_INTEN	0x02		/* enable cmd completion interrupt */
#define	BSD43_IPO_DISERR	0x04		/* disable ecc error detection */
#define	BSD43_IPO_RSVPORT	0x08		/* reserve dual ported drive */
#define	BSD43_IPO_LOGICAL	0x10		/* enable logical block addressing */
#define	BSD43_IPO_LINKPG	0x20		/* another iopb is linked */
#define	BSD43_IPO_VOLUME	0x40		/* volume number */
#define	BSD43_IPO_UNIT	0x80		/* unit number */

/*
 * command status codes
 * (ippb_statcode)
 */
#define	BSD43_IPS_OK		0x80		/* completed without errors */
#define	BSD43_IPS_INPROG	0x81		/* in progress */
#define	BSD43_IPS_ERROR	0x82		/* completed with error */
#define	BSD43_IPS_EXCEPT	0x83		/* completed with exception */

/*
 * command exception codes (valid if ippb_statcode == IPS_EXCEPT)
 * (ippb_errcode)
 */
#define	BSD43_IPEX_RETRYMASK	0x0F		/* mask for retry count */
#define	BSD43_IPEX_RECAL	0x10		/* recalibrate done */
#define	BSD43_IPEX_BADDATA	0x40		/* data may be bad */
#define	BSD43_IPEX_ECC	0x80		/* ecc correction done */

/*
 * command error code (valid if ippb_statcode == IPS_ERROR)
 * (ippb_errcode)
 */
#define	BSD43_IPER_DNOTRDY	0x10		/* drive not ready */
#define	BSD43_IPER_SEEKERR	0x12		/* seek error, cyl not found */
#define	BSD43_IPER_ECCERR	0x13		/* ecc error, correctn not attempted */
#define	BSD43_IPER_INVCMD	0x14		/* invalid command */
#define	BSD43_IPER_ILLEXEC	0x15		/* illegal execute iopb */
#define	BSD43_IPER_INVSEC	0x16		/* invalid sector address */
#define	BSD43_IPER_ILLMEM	0x17		/* illegal memory type */
#define	BSD43_IPER_BUSTMOUT	0x18		/* bus timeout */
#define	BSD43_IPER_HDRCSUM	0x19		/* header checksum */
#define	BSD43_IPER_WPROT	0x1A		/* write protect */
#define	BSD43_IPER_NOUNIT	0x1B		/* unit select error */
#define	BSD43_IPER_SKERRTMOUT	0x1C		/* seek error timeout */
#define	BSD43_IPER_FAULTTMOUT	0x1D		/* fault timeout */
#define	BSD43_IPER_DFAULT	0x1E		/* drive fault */
#define	BSD43_IPER_RDYTMOUT	0x1F		/* ready timeout */
#define	BSD43_IPER_EOM	0x20		/* end of media */
#define	BSD43_IPER_VOLFAULT	0x21		/* volume uib not initialized */
#define	BSD43_IPER_INVHDRPAD	0x22		/* invalid header pad */
#define	BSD43_IPER_HARDECC	0x23		/* uncorrectable ecc error */
#define	BSD43_IPER_LGCLCYL	0x24		/* logical address error on cylinder */
#define	BSD43_IPER_LGCLTRK	0x25		/* logical address error on trk */
#define	BSD43_IPER_LGCLSEC	0x26		/* logical address error on sec */
#define	BSD43_IPER_DOVERRUN	0x27		/* data overrun */
#define	BSD43_IPER_INDEXTMOUT	0x28		/* index pulse timeout */
#define	BSD43_IPER_SECNFND	0x29		/* sector not found */
#define	BSD43_IPER_IDERR	0x2A		/* head number wrong in header */
#define	BSD43_IPER_INVSYNCD	0x2B		/* invalid sync char in data */
#define BSD43_IPER_HDRINVAL	0x2C		/* no valid header was found */
#define	BSD43_IPER_SEEKTMOUT	0x2D		/* seek timeout */
#define	BSD43_IPER_BUSYTMOUT	0x2E		/* busy timeout */
#define	BSD43_IPER_NOTONCYL	0x2F		/* not on cylinder */
#define	BSD43_IPER_RTZTMOUT	0x30		/* rtz timeout */
#define	BSD43_IPER_INVSYNCH	0x31		/* invalid sync char in header */
#define	BSD43_IPER_UNITINIT	0x40		/* unit not initialized */
#define	BSD43_IPER_GAPERR	0x42		/* gap specification error */
#define	BSD43_IPER_DSEEKERR	0x4B		/* drive reported seek error */
#define	BSD43_IPER_UIBSECTRK	0x50		/* uib sectors/trk spec incorrect */
#define	BSD43_IPER_UIBBYTSEC	0x51		/* uib bytes/sector spec incorrect */
#define	BSD43_IPER_UIBINTRLV	0x52		/* uib intrlv spec incorrect */
#define	BSD43_IPER_INVTRK	0x53		/* trk address mismatch with uib */
#define	BSD43_IPER_INVCYL	0x54		/* cyl address mismatch with uib */
#define	BSD43_IPER_ODDDMACNT	0x5D		/* odd dma count */
#define	BSD43_IPER_PBBUSERR	0x60		/* bus error referencing iopb */
#define	BSD43_IPER_DMABUSERR	0x61		/* bus error on data transfer */
#define	BSD43_IPER_ADDRERR	0x62		/* unaligned address */
#define BSD43_IPER_BADHDR	0x6A		/* unrecognized header field */
#define BSD43_IPER_BADMAPHDR	0x6B		/* mapped header error */
#define	BSD43_IPER_NOSPARE	0x6F		/* spare sector not spec'ed in uib */
#define BSD43_IPER_CMDABORT	0x77		/* command aborted */
#define BSD43_IPER_ACFAIL	0x78		/* acfail detected */
#define	BSD43_IPER_CMDNIMP	0xFF		/* command not implemented */

/*
 * memory types
 * (ippb_memtype and ippb_pbmemtype)
 */
#define	BSD43_IPMT_8BIT	0		/* 8 bit data transfers */
#define	BSD43_IPMT_16BITI	1		/* 16 bit internal memory */
#define	BSD43_IPMT_16BIT	2		/* 16 bit data transfers */
#define	BSD43_IPMT_32BIT	3		/* 32 bit data transfers */

/*
 * format of Interphase VME 3200 device registers
 * NOTE: this is accessed via a D16 (not D32!) data path
 */
struct bsd43_(ipdevice) {
#ifdef MIPSEL
	u_char	ipds0;		/* drive status unit 0 */
	u_char	ipds1;		/* drive status unit 1 */
#endif MIPSEL
#ifdef MIPSEB
	u_char	ipds1;		/* drive status unit 1 */
	u_char	ipds0;		/* drive status unit 0 */
#endif MIPSEB
	u_short	ipcsr;		/* command/status register */
	struct bsd43_(ipiopb) bsd43_(ipiopb);	/* resident iopb */
	struct bsd43_(ipsge) ipsg[BSD43_MAX_SGENTRY]; /* resident scatter/gather list */
	u_short	unused;
#ifdef MIPSEL
	u_char	ipdsr0;		/* drive status unit 0 - 4 unit op only */
	u_char	ipdsr1;		/* drive status unit 1 - 4 unit op only */
	u_char	ipdsr2;		/* drive status unit 2 - 4 unit op only */
	u_char	ipdsr3;		/* drive status unit 3 - 4 unit op only */
#endif MIPSEL
#ifdef MIPSEB
	u_char	ipdsr3;		/* drive status unit 3 - 4 unit op only */
	u_char	ipdsr2;		/* drive status unit 2 - 4 unit op only */
	u_char	ipdsr1;		/* drive status unit 1 - 4 unit op only */
	u_char	ipdsr0;		/* drive status unit 0 - 4 unit op only */
#endif MIPSEB
	u_short	ipscr;		/* optional status change register */
};

/*
 * drive status bits
 * (ipds0 and ipds1)
 */
#define	BSD43_IPDS_DRDY	0x01	/* drive ready */
#define	BSD43_IPDS_WPROT	0x02	/* drive write protected */
#define	BSD43_IPDS_DBUSY	0x04	/* drive busy, dual ported drives only */
#define	BSD43_IPDS_FAULT	0x08	/* drive fault */
#define	BSD43_IPDS_ONCYL	0x10	/* drive on cylinder */
#define	BSD43_IPDS_SKERR	0x20	/* drive seek error */
#define	BSD43_IPDS_UALIVE	0x40	/* controller has detected drive */
#define	BSD43_IPDS_URDY	0x80	/* ctrlr sees DRDY & ONCYL & !FAULT & !SKERR */

/*
 * command and status bits
 * (ipcsr)
 * NOTE: if the optional status change register is selected then the bits
 * 	IPCS_SCSRC and IPCS_SC will not be used in this register but will
 *	be reflected in the optional register, ipscr.
 */
#define	BSD43_IPCS_SCSRC	0x0008	/* status change source, drv 0 or 1 */
#define	BSD43_IPCS_ERLST	0x0010	/* error detected on last command */
#define	BSD43_IPCS_SC		0x0020	/* status change (DRDY,FAULT,ONCYL,SKERR) */
#define	BSD43_IPCS_OPDONE	0x0040	/* operation done */
#define	BSD43_IPCS_GOBUSY	0x0080	/* go/busy bit */
#define	BSD43_IPCS_BERR	0x0100	/* bus error detected */
#define BSD43_IPCS_ABORT	0x0800	/* abort current operation */
#define	BSD43_IPCS_BDCLR	0x1000	/* board clear */
#define	BSD43_IPCS_SFEN	0x2000	/* VME sysfail enable */
#define	BSD43_IPCS_BOK	0x4000	/* board ok */
#define	BSD43_IPCS_SLED	0x8000	/* status led, 0=>show activity, 1=>green */

/*
 * uib (unit initialization block) format
 */
struct bsd43_(ipuib) {
	u_char	ipu_v0sh;	/* vol 0 starting head */
	u_char	ipu_v0nh;	/* vol 0 number of heads */
	u_char	ipu_v1sh;	/* vol 1 starting head */
	u_char	ipu_v1nh;	/* vol 1 number of heads */
	u_char	ipu_sectrk;	/* sectors per track */
	u_char	ipu_skew;	/* spiral skew factor */
	u_char	ipu_bytsechi;	/* bytes per sector high */
	u_char	ipu_bytseclo;	/* bytes per sector low */
	u_char	ipu_gap1;	/* words in gap1 */
	u_char	ipu_gap2;	/* words in gap2 */
	u_char	ipu_intrlv;	/* sector interleave factor */
	u_char	ipu_retries;	/* max number of retries on data error */
	u_char	ipu_cylhi;	/* number of cylinders, high */
	u_char	ipu_cyllo;	/* number of cylinders, low */
	u_char	ipu_attrib;	/* attribute flags */
	u_char	ipu_options;	/* allows enabling of optional features */
	u_char	ipu_statipl;	/* status change interrupt level */
	u_char	ipu_statvec;	/* status change interrupt vector */
};

/*
 * optional features 
 * (ipu_options)
 */
#define BSD43_IPOP_EXTADDR	0x80	/* enable smd-e extended addressing */
#define BSD43_IPOP_4UNITSEL	0x40	/* enable four unit operation */

/*
 * status change interrupt level
 * (ipu_statipl)
 */
#define BSD43_IPIPL_USEOSCR	0x80	/* use optional status change register */

/*
 * attribute flags
 * (ipu_attrib)
 */
#define	BSD43_IPAT_RESEEK	0x01	/* enable recalibrate as last resort */
#define	BSD43_IPAT_MVBADDATA	0x02	/* enable transfer of bad data */
#define	BSD43_IPAT_INCBYHEAD	0x04	/* increment by head at track boundry */
#define	BSD43_IPAT_DUALPORT	0x08	/* observe dual port protocol */
#define	BSD43_IPAT_STCINTR	0x10	/* enable status change interrupts */
#define	BSD43_IPAT_CACHEEN	0x20	/* enable sector caching */
#define	BSD43_IPAT_SPSECEN	0x40	/* enable spare sectoring */
#define BSD43_IPAT_RUNTSECEN	0x80	/* runt(short) sector enable */

/*
 * misc macros
 */
#define	BSD43_HI16(x)	((unsigned)(x) >> 16)
#define	BSD43_LO16(x)	((unsigned)(x))
#define	BSD43_HI8(x)	((unsigned)(x) >> 8)
#define	BSD43_LO8(x)	((unsigned)(x))

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define HI16 BSD43_HI16
#   define HI8 BSD43_HI8
#   define IPAT_CACHEEN BSD43_IPAT_CACHEEN
#   define IPAT_DUALPORT BSD43_IPAT_DUALPORT
#   define IPAT_INCBYHEAD BSD43_IPAT_INCBYHEAD
#   define IPAT_MVBADDATA BSD43_IPAT_MVBADDATA
#   define IPAT_RESEEK BSD43_IPAT_RESEEK
#   define IPAT_RUNTSECEN BSD43_IPAT_RUNTSECEN
#   define IPAT_SPSECEN BSD43_IPAT_SPSECEN
#   define IPAT_STCINTR BSD43_IPAT_STCINTR
#   define IPCS_ABORT BSD43_IPCS_ABORT
#   define IPCS_BDCLR BSD43_IPCS_BDCLR
#   define IPCS_BERR BSD43_IPCS_BERR
#   define IPCS_BOK BSD43_IPCS_BOK
#   define IPCS_ERLST BSD43_IPCS_ERLST
#   define IPCS_GOBUSY BSD43_IPCS_GOBUSY
#   define IPCS_OPDONE BSD43_IPCS_OPDONE
#   define IPCS_SC BSD43_IPCS_SC
#   define IPCS_SCSRC BSD43_IPCS_SCSRC
#   define IPCS_SFEN BSD43_IPCS_SFEN
#   define IPCS_SLED BSD43_IPCS_SLED
#   define IPDS_DBUSY BSD43_IPDS_DBUSY
#   define IPDS_DRDY BSD43_IPDS_DRDY
#   define IPDS_FAULT BSD43_IPDS_FAULT
#   define IPDS_ONCYL BSD43_IPDS_ONCYL
#   define IPDS_SKERR BSD43_IPDS_SKERR
#   define IPDS_UALIVE BSD43_IPDS_UALIVE
#   define IPDS_URDY BSD43_IPDS_URDY
#   define IPDS_WPROT BSD43_IPDS_WPROT
#   define IPER_ACFAIL BSD43_IPER_ACFAIL
#   define IPER_ADDRERR BSD43_IPER_ADDRERR
#   define IPER_BADHDR BSD43_IPER_BADHDR
#   define IPER_BADMAPHDR BSD43_IPER_BADMAPHDR
#   define IPER_BUSTMOUT BSD43_IPER_BUSTMOUT
#   define IPER_BUSYTMOUT BSD43_IPER_BUSYTMOUT
#   define IPER_CMDABORT BSD43_IPER_CMDABORT
#   define IPER_CMDNIMP BSD43_IPER_CMDNIMP
#   define IPER_DFAULT BSD43_IPER_DFAULT
#   define IPER_DMABUSERR BSD43_IPER_DMABUSERR
#   define IPER_DNOTRDY BSD43_IPER_DNOTRDY
#   define IPER_DOVERRUN BSD43_IPER_DOVERRUN
#   define IPER_DSEEKERR BSD43_IPER_DSEEKERR
#   define IPER_ECCERR BSD43_IPER_ECCERR
#   define IPER_EOM BSD43_IPER_EOM
#   define IPER_FAULTTMOUT BSD43_IPER_FAULTTMOUT
#   define IPER_GAPERR BSD43_IPER_GAPERR
#   define IPER_HARDECC BSD43_IPER_HARDECC
#   define IPER_HDRCSUM BSD43_IPER_HDRCSUM
#   define IPER_HDRINVAL BSD43_IPER_HDRINVAL
#   define IPER_IDERR BSD43_IPER_IDERR
#   define IPER_ILLEXEC BSD43_IPER_ILLEXEC
#   define IPER_ILLMEM BSD43_IPER_ILLMEM
#   define IPER_INDEXTMOUT BSD43_IPER_INDEXTMOUT
#   define IPER_INVCMD BSD43_IPER_INVCMD
#   define IPER_INVCYL BSD43_IPER_INVCYL
#   define IPER_INVHDRPAD BSD43_IPER_INVHDRPAD
#   define IPER_INVSEC BSD43_IPER_INVSEC
#   define IPER_INVSYNCD BSD43_IPER_INVSYNCD
#   define IPER_INVSYNCH BSD43_IPER_INVSYNCH
#   define IPER_INVTRK BSD43_IPER_INVTRK
#   define IPER_LGCLCYL BSD43_IPER_LGCLCYL
#   define IPER_LGCLSEC BSD43_IPER_LGCLSEC
#   define IPER_LGCLTRK BSD43_IPER_LGCLTRK
#   define IPER_NOSPARE BSD43_IPER_NOSPARE
#   define IPER_NOTONCYL BSD43_IPER_NOTONCYL
#   define IPER_NOUNIT BSD43_IPER_NOUNIT
#   define IPER_ODDDMACNT BSD43_IPER_ODDDMACNT
#   define IPER_PBBUSERR BSD43_IPER_PBBUSERR
#   define IPER_RDYTMOUT BSD43_IPER_RDYTMOUT
#   define IPER_RTZTMOUT BSD43_IPER_RTZTMOUT
#   define IPER_SECNFND BSD43_IPER_SECNFND
#   define IPER_SEEKERR BSD43_IPER_SEEKERR
#   define IPER_SEEKTMOUT BSD43_IPER_SEEKTMOUT
#   define IPER_SKERRTMOUT BSD43_IPER_SKERRTMOUT
#   define IPER_UIBBYTSEC BSD43_IPER_UIBBYTSEC
#   define IPER_UIBINTRLV BSD43_IPER_UIBINTRLV
#   define IPER_UIBSECTRK BSD43_IPER_UIBSECTRK
#   define IPER_UNITINIT BSD43_IPER_UNITINIT
#   define IPER_VOLFAULT BSD43_IPER_VOLFAULT
#   define IPER_WPROT BSD43_IPER_WPROT
#   define IPEX_BADDATA BSD43_IPEX_BADDATA
#   define IPEX_ECC BSD43_IPEX_ECC
#   define IPEX_RECAL BSD43_IPEX_RECAL
#   define IPEX_RETRYMASK BSD43_IPEX_RETRYMASK
#   define IPIPL_USEOSCR BSD43_IPIPL_USEOSCR
#   define IPMT_16BIT BSD43_IPMT_16BIT
#   define IPMT_16BITI BSD43_IPMT_16BITI
#   define IPMT_32BIT BSD43_IPMT_32BIT
#   define IPMT_8BIT BSD43_IPMT_8BIT
#   define IPOP_4UNITSEL BSD43_IPOP_4UNITSEL
#   define IPOP_EXTADDR BSD43_IPOP_EXTADDR
#   define IPO_DISERR BSD43_IPO_DISERR
#   define IPO_ECCEN BSD43_IPO_ECCEN
#   define IPO_INTEN BSD43_IPO_INTEN
#   define IPO_LINKPG BSD43_IPO_LINKPG
#   define IPO_LOGICAL BSD43_IPO_LOGICAL
#   define IPO_RSVPORT BSD43_IPO_RSVPORT
#   define IPO_UNIT BSD43_IPO_UNIT
#   define IPO_VOLUME BSD43_IPO_VOLUME
#   define IPS_ERROR BSD43_IPS_ERROR
#   define IPS_EXCEPT BSD43_IPS_EXCEPT
#   define IPS_INPROG BSD43_IPS_INPROG
#   define IPS_OK BSD43_IPS_OK
#   define IP_CLRFAULT BSD43_IP_CLRFAULT
#   define IP_DIAG BSD43_IP_DIAG
#   define IP_DPPRISEL BSD43_IP_DPPRISEL
#   define IP_EDIAGS BSD43_IP_EDIAGS
#   define IP_EXECIOPB BSD43_IP_EXECIOPB
#   define IP_FMTDATA BSD43_IP_FMTDATA
#   define IP_FORMATTRK BSD43_IP_FORMATTRK
#   define IP_GATHER BSD43_IP_GATHER
#   define IP_GETCONFIG BSD43_IP_GETCONFIG
#   define IP_HANDSHAKE BSD43_IP_HANDSHAKE
#   define IP_INITIALIZE BSD43_IP_INITIALIZE
#   define IP_MAPSEC BSD43_IP_MAPSEC
#   define IP_MAPTRK BSD43_IP_MAPTRK
#   define IP_READ BSD43_IP_READ
#   define IP_READCDCFLAW BSD43_IP_READCDCFLAW
#   define IP_READHDR BSD43_IP_READHDR
#   define IP_READLONG BSD43_IP_READLONG
#   define IP_READNC BSD43_IP_READNC
#   define IP_READRAW BSD43_IP_READRAW
#   define IP_READSBUF BSD43_IP_READSBUF
#   define IP_READSEQ BSD43_IP_READSEQ
#   define IP_READSEQDA BSD43_IP_READSEQDA
#   define IP_RECAL BSD43_IP_RECAL
#   define IP_REFORMAT BSD43_IP_REFORMAT
#   define IP_SCATTER BSD43_IP_SCATTER
#   define IP_SEEK BSD43_IP_SEEK
#   define IP_TRACKID BSD43_IP_TRACKID
#   define IP_VERIFY BSD43_IP_VERIFY
#   define IP_VERIFYSEQ BSD43_IP_VERIFYSEQ
#   define IP_VERIFYTRK BSD43_IP_VERIFYTRK
#   define IP_VERIFYTRKSEQ BSD43_IP_VERIFYTRKSEQ
#   define IP_WRITE BSD43_IP_WRITE
#   define IP_WRITELONG BSD43_IP_WRITELONG
#   define IP_WRITESBUF BSD43_IP_WRITESBUF
#   define IP_WRITESEQ BSD43_IP_WRITESEQ
#   define IP_WRITESEQDA BSD43_IP_WRITESEQDA
#   define LO16 BSD43_LO16
#   define LO8 BSD43_LO8
#   define MAX_SGENTRY BSD43_MAX_SGENTRY
#   define ippb_cyl bsd43_ippb_cyl
#   define ippb_lbnhi bsd43_ippb_lbnhi
#   define ippb_lbnlo bsd43_ippb_lbnlo
#   define ippb_sec bsd43_ippb_sec
#   define ippb_trk bsd43_ippb_trk
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


