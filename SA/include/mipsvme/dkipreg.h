#ident "$Header: dkipreg.h,v 1.4 90/01/23 13:16:49 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * dkipreg.h -- register definitions for Interphase VME 3200 SMD controller
 */

/*
 * format of Interphase VME 3200 i/o parameter block
 * NOTE: assumed to be accessed via a D16 data path
 */
struct ipiopb {
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
	u_char	ippb_ipl;		/* interrupt priority level */
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
	u_char	ippb_ipl;		/* interrupt priority level */
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
#define	ippb_lbnhi	ippb_diskaddr.id_lda.idl_lbnhi
#define	ippb_lbnlo	ippb_diskaddr.id_lda.idl_lbnlo

#define	ippb_cyl	ippb_diskaddr.id_pda.idp_cyl
#define	ippb_trk	ippb_diskaddr.id_pda.idp_trk
#define	ippb_sec	ippb_diskaddr.id_pda.idp_sec

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
#define MAX_SGENTRY	59

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
struct ipsge {
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
#define	IP_DIAG		0x70		/* run power-up diagnostics */
#define	IP_READLONG	0x71		/* read data and ecc */
#define	IP_WRITELONG	0x72		/* write data and ecc */
#define	IP_READHDR	0x74		/* read first encountered hdr */
#define IP_READRAW	0x75		/* read raw data */
#define IP_READCDCFLAW	0x76		/* read cdc flaw map */
#define	IP_GETCONFIG	0x77		/* get uib configuration */
#define	IP_WRITESBUF	0x78		/* write sector buffer */
#define	IP_READSBUF	0x79		/* read sector buffer */
#define	IP_READ		0x81		/* read sectors */
#define	IP_WRITE	0x82		/* write sectors */
#define	IP_VERIFY	0x83		/* verify sectors */
#define	IP_FORMATTRK	0x84		/* format track */
#define	IP_MAPTRK	0x85		/* forward track */
#define	IP_HANDSHAKE	0x86		/* get firmware id */
#define	IP_INITIALIZE	0x87		/* set uib configuration */
#define	IP_RECAL	0x89		/* rtz and recalibrate drive */
#define	IP_SEEK		0x8A		/* seek to cylinder */
#define	IP_REFORMAT	0x8B		/* format preserving track forwards */
#define	IP_FMTDATA	0x8C		/* format and write data */
#define	IP_MAPSEC	0x90		/* forward sector */
#define IP_READSEQ	0x91		/* read sectors sequential */
#define IP_WRITESEQ	0x92		/* write sectors sequential */
#define IP_VERIFYSEQ	0x93		/* verify sectors sequential */
#define	IP_READNC	0x94		/* read bypassing controller cache */
#define IP_READSEQDA	0x95		/* read sequential, disable addr */
#define IP_WRITESEQDA	0x96		/* write sequential, disable addr */
#define IP_CLRFAULT	0x97		/* clear drive fault */
#define	IP_VERIFYTRK	0x99		/* verify trk readable w/o ecc errs */
#define	IP_TRACKID	0x9A		/* return sec hdrs in order encntrd */
#define	IP_EXECIOPB	0x9B		/* execute linked iopb's */
#define IP_VERIFYTRKSEQ	0x9C		/* verify track sequential */
#define IP_EDIAGS	0x9E		/* extended diagnostics */
#define IP_DPPRISEL	0x9F		/* dual port priority select */
#define IP_SCATTER	0xA1		/* read sectors & scatter to memory */
#define IP_GATHER	0xA2		/* gather from memory & write sectors */

/*
 * command options
 * (ippb_cmdopt)
 */
#define	IPO_ECCEN	0x01		/* enable ecc correction */
#define	IPO_INTEN	0x02		/* enable cmd completion interrupt */
#define	IPO_DISERR	0x04		/* disable ecc error detection */
#define	IPO_RSVPORT	0x08		/* reserve dual ported drive */
#define	IPO_LOGICAL	0x10		/* enable logical block addressing */
#define	IPO_LINKPG	0x20		/* another iopb is linked */
#define	IPO_VOLUME	0x40		/* volume number */
#define	IPO_UNIT	0x80		/* unit number */

/*
 * command status codes
 * (ippb_statcode)
 */
#define	IPS_OK		0x80		/* completed without errors */
#define	IPS_INPROG	0x81		/* in progress */
#define	IPS_ERROR	0x82		/* completed with error */
#define	IPS_EXCEPT	0x83		/* completed with exception */

/*
 * command exception codes (valid if ippb_statcode == IPS_EXCEPT)
 * (ippb_errcode)
 */
#define	IPEX_RETRYMASK	0x0F		/* mask for retry count */
#define	IPEX_RECAL	0x10		/* recalibrate done */
#define	IPEX_BADDATA	0x40		/* data may be bad */
#define	IPEX_ECC	0x80		/* ecc correction done */

/*
 * command error code (valid if ippb_statcode == IPS_ERROR)
 * (ippb_errcode)
 */
#define	IPER_DNOTRDY	0x10		/* drive not ready */
#define	IPER_SEEKERR	0x12		/* seek error, cyl not found */
#define	IPER_ECCERR	0x13		/* ecc error, correctn not attempted */
#define	IPER_INVCMD	0x14		/* invalid command */
#define	IPER_ILLEXEC	0x15		/* illegal execute iopb */
#define	IPER_INVSEC	0x16		/* invalid sector address */
#define	IPER_ILLMEM	0x17		/* illegal memory type */
#define	IPER_BUSTMOUT	0x18		/* bus timeout */
#define	IPER_HDRCSUM	0x19		/* header checksum */
#define	IPER_WPROT	0x1A		/* write protect */
#define	IPER_NOUNIT	0x1B		/* unit select error */
#define	IPER_SKERRTMOUT	0x1C		/* seek error timeout */
#define	IPER_FAULTTMOUT	0x1D		/* fault timeout */
#define	IPER_DFAULT	0x1E		/* drive fault */
#define	IPER_RDYTMOUT	0x1F		/* ready timeout */
#define	IPER_EOM	0x20		/* end of media */
#define	IPER_VOLFAULT	0x21		/* volume uib not initialized */
#define	IPER_INVHDRPAD	0x22		/* invalid header pad */
#define	IPER_HARDECC	0x23		/* uncorrectable ecc error */
#define	IPER_LGCLCYL	0x24		/* logical address error on cylinder */
#define	IPER_LGCLTRK	0x25		/* logical address error on trk */
#define	IPER_LGCLSEC	0x26		/* logical address error on sec */
#define	IPER_DOVERRUN	0x27		/* data overrun */
#define	IPER_INDEXTMOUT	0x28		/* index pulse timeout */
#define	IPER_SECNFND	0x29		/* sector not found */
#define	IPER_IDERR	0x2A		/* head number wrong in header */
#define	IPER_INVSYNCD	0x2B		/* invalid sync char in data */
#define IPER_HDRINVAL	0x2C		/* no valid header was found */
#define	IPER_SEEKTMOUT	0x2D		/* seek timeout */
#define	IPER_BUSYTMOUT	0x2E		/* busy timeout */
#define	IPER_NOTONCYL	0x2F		/* not on cylinder */
#define	IPER_RTZTMOUT	0x30		/* rtz timeout */
#define	IPER_INVSYNCH	0x31		/* invalid sync char in header */
#define	IPER_UNITINIT	0x40		/* unit not initialized */
#define	IPER_GAPERR	0x42		/* gap specification error */
#define	IPER_DSEEKERR	0x4B		/* drive reported seek error */
#define	IPER_UIBSECTRK	0x50		/* uib sectors/trk spec incorrect */
#define	IPER_UIBBYTSEC	0x51		/* uib bytes/sector spec incorrect */
#define	IPER_UIBINTRLV	0x52		/* uib intrlv spec incorrect */
#define	IPER_INVTRK	0x53		/* trk address mismatch with uib */
#define	IPER_INVCYL	0x54		/* cyl address mismatch with uib */
#define	IPER_ODDDMACNT	0x5D		/* odd dma count */
#define	IPER_PBBUSERR	0x60		/* bus error referencing iopb */
#define	IPER_DMABUSERR	0x61		/* bus error on data transfer */
#define	IPER_ADDRERR	0x62		/* unaligned address */
#define IPER_BADHDR	0x6A		/* unrecognized header field */
#define IPER_BADMAPHDR	0x6B		/* mapped header error */
#define	IPER_NOSPARE	0x6F		/* spare sector not spec'ed in uib */
#define IPER_CMDABORT	0x77		/* command aborted */
#define IPER_ACFAIL	0x78		/* acfail detected */
#define	IPER_CMDNIMP	0xFF		/* command not implemented */

/*
 * memory types
 * (ippb_memtype and ippb_pbmemtype)
 */
#define	IPMT_8BIT	0		/* 8 bit data transfers */
#define	IPMT_16BITI	1		/* 16 bit internal memory */
#define	IPMT_16BIT	2		/* 16 bit data transfers */
#define	IPMT_32BIT	3		/* 32 bit data transfers */

/*
 * format of Interphase VME 3200 device registers
 * NOTE: this is accessed via a D16 (not D32!) data path
 */
struct ipdevice {
#ifdef MIPSEL
	u_char	ipds0;		/* drive status unit 0 */
	u_char	ipds1;		/* drive status unit 1 */
#endif MIPSEL
#ifdef MIPSEB
	u_char	ipds1;		/* drive status unit 1 */
	u_char	ipds0;		/* drive status unit 0 */
#endif MIPSEB
	u_short	ipcsr;		/* command/status register */
	struct ipiopb ipiopb;	/* resident iopb */
	struct ipsge ipsg[MAX_SGENTRY]; /* resident scatter/gather list */
	u_short	unused;
	u_char	ipdsr[4];	/* For SEL, unit 0 is in ipdsr[0]	*/
				/* For SEB, unit 0 is in ipdsr[3]	*/
	u_short	ipscr;		/* optional status change register */
};

/*
 * drive status bits
 * (ipds0 and ipds1)
 */
#define	IPDS_DRDY	0x01	/* drive ready */
#define	IPDS_WPROT	0x02	/* drive write protected */
#define	IPDS_DBUSY	0x04	/* drive busy, dual ported drives only */
#define	IPDS_FAULT	0x08	/* drive fault */
#define	IPDS_ONCYL	0x10	/* drive on cylinder */
#define	IPDS_SKERR	0x20	/* drive seek error */
#define	IPDS_UALIVE	0x40	/* controller has detected drive */
#define	IPDS_URDY	0x80	/* ctrlr sees DRDY & ONCYL & !FAULT & !SKERR */

/*
 * command and status bits
 * (ipcsr)
 * NOTE: if the optional status change register is selected then the bits
 * 	IPCS_SCSRC and IPCS_SC will not be used in this register but will
 *	be reflected in the optional register, ipscr.
 */
#define	IPCS_SCSRC	0x0008	/* status change source, drv 0 or 1 */
#define	IPCS_ERLST	0x0010	/* error detected on last command */
#define	IPCS_SC		0x0020	/* status change (DRDY,FAULT,ONCYL,SKERR) */
#define	IPCS_OPDONE	0x0040	/* operation done */
#define	IPCS_GOBUSY	0x0080	/* go/busy bit */
#define	IPCS_BERR	0x0100	/* bus error detected */
#define IPCS_ABORT	0x0800	/* abort current operation */
#define	IPCS_BDCLR	0x1000	/* board clear */
#define	IPCS_SFEN	0x2000	/* VME sysfail enable */
#define	IPCS_BOK	0x4000	/* board ok */
#define	IPCS_SLED	0x8000	/* status led, 0=>show activity, 1=>green */

/*
 * uib (unit initialization block) format
 */
struct ipuib {
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
#define IPOP_EXTADDR	0x80	/* enable smd-e extended addressing */
#define IPOP_4UNITSEL	0x40	/* enable four unit operation */

/*
 * status change interrupt level
 * (ipu_statipl)
 */
#define IPIPL_USEOSCR	0x80	/* use optional status change register */

/*
 * attribute flags
 * (ipu_attrib)
 */
#define	IPAT_RESEEK	0x01	/* enable recalibrate as last resort */
#define	IPAT_MVBADDATA	0x02	/* enable transfer of bad data */
#define	IPAT_INCBYHEAD	0x04	/* increment by head at track boundry */
#define	IPAT_DUALPORT	0x08	/* observe dual port protocol */
#define	IPAT_STCINTR	0x10	/* enable status change interrupts */
#define	IPAT_CACHEEN	0x20	/* enable sector caching */
#define	IPAT_SPSECEN	0x40	/* enable spare sectoring */
#define IPAT_RUNTSECEN	0x80	/* runt(short) sector enable */

/*
 * misc macros
 */
#define	HI16(x)	((unsigned)(x) >> 16)
#define	LO16(x)	((unsigned)(x))
#define	HI8(x)	((unsigned)(x) >> 8)
#define	LO8(x)	((unsigned)(x))
