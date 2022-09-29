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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: dkipreg.h,v 1.22.1.8.1.1.1.4 90/12/05 17:14:40 beacker Exp $ */

#define PART_GATS

/*
 * dkipreg.h -- register definitions for Interphase VME 3200 SMD controller
 *		define NOMACSI to disable the 4201/4200 from using the MACSI
 *		interface. MACSI allows command Queing, sorting, overlapped
 *		seeks and will automatically clear drive faults.
 */

/*
 * Scatter gather structure.  Using the scatter gather structure, the host
 * can make virtually contiguous dma appear physically contiguous to the
 * controller.  The controller can handle a maximum of a sector size,
 * in bytes, of scatter gather entries.  All the entries must use the
 * same address modifier and memory space.
 */
typedef struct ipsg {
	ushort	sg_count;		/* byte/entry count */
	ushort	sg_addrhi;		/* datablock/entry address high */
	ushort	sg_addrlo;		/* datablock/entry address low */
#ifdef	MIPSEL
	unchar	sg_addrmod;		/* VME address modifier */
	unchar	sg_meminfo;		/* bits about memory location */
#endif
#ifdef	MIPSEB
	unchar	sg_meminfo;		/* bits about memory location */
	unchar	sg_addrmod;		/* VME address modifier */
#endif
}DKIP_IPSG;

/*
 * format of Interphase VME 3200 i/o parameter block
 * NOTE: assumed to be accessed via a D16 data path
 */

typedef struct ipiopb {
#ifdef	MIPSEL
	unchar	ip_cmdopt;		/* 01: command options */
	unchar	ip_cmdcode;		/* 00: command code */
	unchar	ip_errcode;		/* 03: error code */
	unchar	ip_statcode;		/* 02: status code */
	union {
		struct {
			ushort	idp_cyl;	/* 04,05: cylinder */
			unchar	idp_sec;	/* 07: sector */
			unchar	idp_head;	/* 06: head */
		} id_pda;			/* physical disk address */
		struct {
			ushort idl_lbnhi;	/* 04,05: logical block high */
			ushort idl_lbnlo;	/* 06,07: logical block low */
		} id_lda;			/* logical disk address */
	} ip_diskaddr;		/* disk address */
	ushort	ip_scnt;		/* 08,09: sector count */
	ushort	ip_bahi;		/* 0A,0B: buffer address high */
	ushort	ip_balo;		/* 0C,0D: buffer address low */
	unchar	ip_addrmod;		/* 0F: VME address modifier */
	unchar	ip_memtype;		/* 0E: memory type */
	unchar	ip_normvec;		/* 11: normal completion intr vector */
	unchar	ip_ipl;			/* 10: interrupt priority level */
	unchar	ip_errvec;		/* 13: error completion intr vector */
	unchar	ip_dmaburst;		/* 12: dma burst length */
	ushort	ip_iopbhi;		/* 14,15: next iopb address high */
	ushort	ip_iopblo;		/* 16,17: next iopb address low */
	unchar	ip_pbaddrmod;		/* 19: iopb VME address modifier */
	unchar	ip_pbmemtype;		/* 18: iopb memory type */
	unchar	ip_sgentries;		/* 1B: number of scatter/gather entries */
	unchar	ip_skew;		/* 1A: spiral skew */
					/* in the first scatter/gather list */
#endif  MIPSEL
#ifdef	MIPSEB
	unchar	ip_cmdcode;		/* 00: command code */
	unchar	ip_cmdopt;		/* 01: command options */
	unchar	ip_statcode;		/* 02: status code */
	unchar	ip_errcode;		/* 03: error code */
	union {
		struct {
			ushort	idp_cyl;	/* 04,05: cylinder */
			unchar	idp_head;	/* 06: head */
			unchar	idp_sec;	/* 07: sector */
		} id_pda;			/* physical disk address */
		struct {
			ushort idl_lbnhi;	/* 04,05: logical block high */
			ushort idl_lbnlo;	/* 06,07: logical block low */
		} id_lda;			/* logical disk address */
	} ip_diskaddr;		/* disk address */
	ushort	ip_scnt;		/* 08,09: sector count */
	ushort	ip_bahi;		/* 0A,0B: buffer address high */
	ushort	ip_balo;		/* 0C,0D: buffer address low */
	unchar	ip_memtype;		/* 0E: memory type */
	unchar	ip_addrmod;		/* 0F: VME address modifier */
	unchar	ip_ipl;			/* 10: interrupt priority level */
	unchar	ip_normvec;		/* 11: normal completion intr vector */
	unchar	ip_dmaburst;		/* 12: dma burst length */
	unchar	ip_errvec;		/* 13: error completion intr vector */
	ushort	ip_iopbhi;		/* 14,15: next iopb address high */
	ushort	ip_iopblo;		/* 16,17: next iopb address low */
	unchar	ip_pbmemtype;		/* 18: iopb memory type */
	unchar	ip_pbaddrmod;		/* 19: iopb VME address modifier */
	unchar	ip_skew;		/* 1A: spiral skew */
	unchar	ip_sgentries;		/* 1B: number of scatter/gather entries 
					 * in the first scatter/gather list */
#endif
}IPIOPB;

/*
 * Following defines let us load multiple fields at the same time.  This is a
 * big win for machines like the RC6280 as it avoids multiple accesses to the
 * VME bus.
 */

#ifdef	MIPSEL
#define IP_CMD_CODEOPT(iopb,code,opt) {iopb->ip_cmdcode = code; wbflush(); iopb->ip_cmdopt = opt;}
#define IP_MEMINFO_AM(iopb,memtype,am) {iopb->ip_memtype = memtype; wbflush(); iopb->ip_addrmod = am;}
#define IP_HEADSEC(iopb,head,sec) {iopb->ip_head = head; wbflush(); iopb->ip_sec = sec;}
#endif	MIPSEL
  
#ifdef	MIPSEB
#define IP_CMD_CODEOPT(iopb,code,opt) {*(ushort *)&iopb->ip_cmdcode = (code << 8) | opt;}
#define IP_MEMINFO_AM(iopb,memtype,am) {*(ushort *)&iopb->ip_memtype = (memtype << 8) | am;}
#define IP_HEADSEC(iopb,head,sec) {*(ushort *)&iopb->ip_head = (head << 8) | sec;}
#endif	MIPSEB

/*
 * shorthand for disk address
 */
#define	ip_lbnhi	ip_diskaddr.id_lda.idl_lbnhi
#define	ip_lbnlo	ip_diskaddr.id_lda.idl_lbnlo

#define	ip_cyl	ip_diskaddr.id_pda.idp_cyl
#define	ip_head	ip_diskaddr.id_pda.idp_head
#define	ip_sec	ip_diskaddr.id_pda.idp_sec

/*
 * command codes
 * (ip_cmdcode)
 */
#define	IP_DIAG		0x70		/* run power-up diagnostics */
#define	IP_READLONG	0x71		/* read data and ecc */
#define	IP_WRITELONG	0x72		/* write data and ecc */
#define	IP_READHDR	0x74		/* read first encountered hdr */
#define IP_READRAW	0x75		/* read raw data */
#define IP_READFLAWS	0x76		/* read cdc flaw map */
#define	IP_GETCONFIG	0x77		/* get uib configuration */
#define	IP_WRITESBUF	0x78		/* write sector buffer */
#define	IP_READSBUF	0x79		/* read sector buffer */
#define IP_GMACSI	0x7f		/* put controller in MACSI mode	*/
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
#define IP_WSCATTER	0xA3		/* word wide scatter (no zero latency */
#define IP_WGATHER	0xA4		/* word wide gather (no zero latency */

/*
 * command options
 * (ip_cmdopt)
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
 * (ip_statcode)
 */
#define	IPS_OK		0x80		/* completed without errors */
#define	IPS_INPROG	0x81		/* in progress */
#define	IPS_ERROR	0x82		/* completed with error */
#define	IPS_EXCEPT	0x83		/* completed with exception */

/*
 * command exception codes (valid if ip_statcode == IPS_EXCEPT)
 * (ip_errcode)
 */
#define	IPEX_RETRYMASK	0x0F		/* mask for retry count */
#define	IPEX_RECAL	0x10		/* recalibrate done */
#define	IPEX_BADDATA	0x40		/* data may be bad */
#define	IPEX_ECC	0x80		/* ecc correction done */
#define	IPEX_ICKY	(IPEX_ECC|IPEX_BADDATA|IPEX_RECAL)

/*
 * command error code (valid if ip_statcode == IPS_ERROR)
 * (ip_errcode)
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
 * (ip_memtype and ip_pbmemtype)
 */
#define	IPMT_8BIT	0		/* 8 bit data transfers */
#define	IPMT_16BITI	1		/* 16 bit internal memory */
#define	IPMT_16BIT	2		/* 16 bit data transfers */
#define	IPMT_32BIT	3		/* 32 bit data transfers */

/*
 * format of Interphase VME 3200 device registers
 * NOTE: this is accessed via a D16 (not D32!) data path
 * For normal transfers, an iopb in this io space is used, as well as its
 * scatter gather entries.  If the transfer can't take place under such
 * restrictions, then the scatter gather info is stored in host memory
 * instead.
 */
#define IP_MAXSG	59		/* max # of sg entries that fit */
typedef struct ipdevice {
#ifdef	MIPSEL
	unchar	ipds0;			/* 001: drive status unit 0 */
	unchar	ipds1;			/* 000: drive status unit 1 */
	ushort	ipcsr;			/* 002,003: command/status register */
	struct	ipiopb ipiopb;		/* 004-023: iopb */
	struct	ipsg ipsg[IP_MAXSG];	/* 024-1F7: some sg entries */
	char	ipreserved[2];		/* 1F8-1F9: not used */
	/*
	 * The remaining are only defined for the 3201, and then only if
	 * the four drive adapter is in place.  In any case, on the 3201
	 * you can't use these i/o registers for anything else.
	 */
#ifdef OLD_WAY
	unchar	ipds2;			/* 1FB: drive status unit 2 */
	unchar	ipds3;			/* 1FA: drive status unit 3 */
	unchar	ipds0a;			/* 1FD: drive status unit 0 */
	unchar	ipds1a;			/* 1FC: drive status unit 1 */
#else OLD_WAY
	unchar	ipds4[4];		/* 4 unit drive statuses	*/
#endif OLD_WAY
	ushort	ipdscr;			/* 1FE,1FF: status change register */
#endif MIPSEL
#ifdef	MIPSEB
	unchar	ipds1;			/* 000: drive status unit 1 */
	unchar	ipds0;			/* 001: drive status unit 0 */
	ushort	ipcsr;			/* 002,003: command/status register */
	struct	ipiopb ipiopb;		/* 004-023: iopb */
	struct	ipsg ipsg[IP_MAXSG];	/* 024-1F7: some sg entries */
	char	ipreserved[2];		/* 1F8-1F9: not used */
	/*
	 * The remaining are only defined for the 3201, and then only if
	 * the four drive adapter is in place.  In any case, on the 3201
	 * you can't use these i/o registers for anything else.
	 */
#ifdef OLD_WAY
	unchar	ipds3;			/* 1FA: drive status unit 3 */
	unchar	ipds2;			/* 1FB: drive status unit 2 */
	unchar	ipds1a;			/* 1FC: drive status unit 1 */
	unchar	ipds0a;			/* 1FD: drive status unit 0 */
#else OLD_WAY
	unchar	ipds4[4];		/* 4 unit drive statuses	*/
#endif OLD_WAY
	ushort	ipdscr;			/* 1FE,1FF: status change register */
#endif MIPSEB
}IPDEVICE;

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

/*----------------------------------------------------------------------*/
/* NEW NON-MACSI structures						*
/*----------------------------------------------------------------------*/

/*
 * Product codes returned from the handshake command
 */
#define IP3201		0x65		 /* Guerrila product code   	   */
#define IP4201		0x74		 /* Panther product code	   */
#define IP3200		0x49		 /* V/SMD product code	    	   */
#define IP4200		0x66		 /* Cheetah product code    	   */
#define IP4400		0x82		 /* phonix product code    	   */

#ifdef MIPSEL
typedef struct
{
	unchar	p_var;
	unchar	p_code;
	unchar	p_revl;
	unchar	p_revh;
	unchar	p_idl;
	unchar	p_idh;
	unchar	p_day;		/* day stored as BCD	*/
	unchar	p_month;	/* month stored as BCD	*/
	short	p_year;		/* year stored  as BCD	*/
}HNDSHK;
#endif MIPSEL
#ifdef MIPSEB
typedef struct
{
	unchar	p_code;
	unchar	p_var;
	unchar	p_revh;
	unchar	p_revl;
	unchar	p_idh;
	unchar	p_idl;
	unchar	p_month;	/* month stored as BCD	*/
	unchar	p_day;		/* day stored as BCD	*/
	short	p_year;		/* year stored  as BCD	*/
}HNDSHK;
#endif

#define PL_MSPARE	BIT(0)	/* supports multiple spares	*/
#define	PL_SOFT		BIT(1)	/* can run soft sector drives	*/
#define PL_DUAL		BIT(2)	/* supports dual porting	*/
#define PL_MACSI	BIT(3)	/* supports MACSI		*/

typedef struct
{
	unchar	pl_pcode;	/* product  code		*/
	char	*pl_pname;	/* product name			*/
	int	pl_flags;	/* poduct capabilities flags	*/
}PLIST;


/*----------------------------------------------------------------------*/
/* MACSI (Multiply Active Command System Interface) structures		*
/*----------------------------------------------------------------------*/
#ifndef NOMACSI

#ifdef MIPSEL
typedef struct dkip_cqe	{			/* command queue entry	*/
			USHORT control;
			USHORT iopbaddr;	/* short i/o offset of iopb */
			USHORT cmdtag_h;
			USHORT cmdtag_l;
			BYTE queue;		/* unused for 4201/4200	*/
			BYTE length;		/* Lword size of iopb	*/
			USHORT rsrvd;
			}CQE;

typedef struct dkip_mcsb	{		/* Master status block	*/
			USHORT msr;
			USHORT mcr;
			USHORT iqar;
			USHORT qhead;		/* for use by host	*/
			USHORT rsrvd[4];
			}MCSB;

typedef struct dkip_crb      {			/* command response block */
			USHORT crsw;
			USHORT rsrvd;
			USHORT   cmdtag_h;
			USHORT   cmdtag_l;
			BYTE   queue;
			BYTE   length;
			USHORT rsrvd1;
			IPIOPB   copyiopb;
			}CRB;

typedef struct dkip_cib      {			/*MACSI initialization*/
			USHORT cib_num_cqes;
			USHORT cib_crb_off;
			USHORT cib_m_flags;	/* MACSI FLAGS	*/
			USHORT cib_num_free;	/* NUM free entries */
			USHORT cib_ce_vectr;
			USHORT cib_cn_vectr;
			USHORT cib_num_back;
			USHORT cib_r_robin;
			USHORT cib_res2;	/* Future use set to 0	*/
			USHORT cib_res3;	/* Future use set to 0	*/
			USHORT cib_res4;	/* Future use set to 0	*/
			USHORT cib_res5;	/* Future use set to 0	*/
			USHORT cib_res6;	/* Future use set to 0	*/
			USHORT cib_res7;	/* Future use set to 0	*/
			USHORT cib_res8;	/* Future use set to 0	*/
			USHORT cib_res9;	/* Future use set to 0	*/
			}VSMD_CIB;
#endif MIPSEL
#ifdef MIPSEB
typedef struct dkip_cqe	{			/* command queue entry	*/
			USHORT control;
			USHORT iopbaddr;	/* short i/o offset of iopb */
			USHORT cmdtag_h;
			USHORT cmdtag_l;
			BYTE length;		/* Lword size of iopb	*/
			BYTE queue;		/* unused for 4201/4200	*/
			USHORT rsrvd;
			}CQE;

typedef struct dkip_mcsb	{		/* Master status block	*/
			USHORT msr;
			USHORT mcr;
			USHORT iqar;
			USHORT qhead;		/* for use by host	*/
			USHORT rsrvd[4];
			}MCSB;

typedef struct dkip_crb      {			/* command response block */
			USHORT crsw;
			USHORT rsrvd;
			USHORT   cmdtag_h;
			USHORT   cmdtag_l;
			BYTE   length;
			BYTE   queue;
			USHORT rsrvd1;
			IPIOPB   copyiopb;
			}CRB;

typedef struct dkip_cib      {			/*MACSI initialization*/
			USHORT cib_num_cqes;
			USHORT cib_crb_off;
			USHORT cib_m_flags;	/* MACSI FLAGS	*/
			USHORT cib_num_free;	/* NUM free entries */
			USHORT cib_ce_vectr;
			USHORT cib_cn_vectr;
			USHORT cib_num_back;
			USHORT cib_r_robin;
			USHORT cib_res2;	/* Future use set to 0	*/
			USHORT cib_res3;	/* Future use set to 0	*/
			USHORT cib_res4;	/* Future use set to 0	*/
			USHORT cib_res5;	/* Future use set to 0	*/
			USHORT cib_res6;	/* Future use set to 0	*/
			USHORT cib_res7;	/* Future use set to 0	*/
			USHORT cib_res8;	/* Future use set to 0	*/
			USHORT cib_res9;	/* Future use set to 0	*/
			}VSMD_CIB;
#endif

#define NUM_CQES 	8		/* Command Q entries in short i/o */
#define NUM_MACSI	20		/* number of commands to q	*/
#define NUM_DKIP_SG 	8
#define NUM_R_ROBIN	8
#define LBPMAX		8		/* number of BPs to link	*/
#define MACSI_DKIP_SG	64		/* number of MACSI scat/gat entries */
					/* this times 8 MUST be < 512	*/

/* When Running MACSI, Scatter/gather tables MUST be in external memory
 * We will declare a free pool of scatter/gather tables in each ctlr struct
 * NUM_DKIP_SG defines the number of entries in the free pool per controller.
 * This number should be:
 *	NUM_MACSI to ensure that all the Q entries can be filled
 * However, The driver can deal with this being smaller.
 * NOTE:
 *	It is inavisable to make NUM_DKIP_SG smaller than DKIPUPC.
 *	This will possibly prevent overlapped seeks from happening.
 *	2 * DKIPUPC is the smallest recommended number
 *
 *	The driver will add & remove from the top of the free list.
 * ALSO:
 *	The free list had to be in each ctlr struct instead of 1 common
 *	pool because if the controller start routine gets called at interrupt
 *	level, and there are no more free entries available, sleep cannot
 *	be called and there might not be any more requests comming in for
 *	that controller.
 */

typedef struct dkip_ipsg_free {
	struct dkip_ipsg_free *nxt;	/* pointer to next IPSG_FREE	*/
	DKIP_IPSG ipsg[MACSI_DKIP_SG];
} IPSG_DKIP_FREE;

/* When running on an M6000, we need to keep track of the GBA map register
 * descriptors so the GBA cache can be flushed and the GBA map registers
 * released when the I/O operation completes.  During "normal" operation,
 * each outstanding I/O request needs a single area (i.e. page) mapped, so
 * we need a single descriptor per outstanding I/O or NUM_CQES + NUM_MACSI
 * (total 28).  However, if multiple writes occur to consecutive location,
 * a SG write can span LBPMAX buffers, so we need LBPMAX * NUM_DKIP_SG
 * (total 64) to handle this case.
 *
 * There is currently no spare space in the "bp" to store this information.
 * For this reason, we will use the av_back field which used to contain
 * the SG head.  The av_back field will now point to a new data structure,
 * the DKIP_IOINFO structure which will actually contain both the map register
 * descriptor and the SG head pointer.
 */

#define NUM_DKIP_IOINFO 64

typedef struct dkip_ioinfo_t {
	struct dkip_ioinfo_t	*ioinfo_nxt;
	struct buf	*bp;
	IPSG_DKIP_FREE *dkip_sg_list;
	ulong		sph;
      } IOINFO_DKIP_T;


#define IOPB_BUF_SIZ	sizeof(IPIOPB)

typedef struct
{
	char iopb_buf[IOPB_BUF_SIZ];
} IOPB_BUFF;

/*
 *	The sructure definition below is for the:
 *
 *		 VSMD, 3201, 4200 & 4201 short/io memory space
 *		IN MACSI mode
 *	NOTE: the CRB can actually be anywhere in short i/o 1a6 was just
 *	      picked out of the 'air'.
 */
#define U4STATBASE	(0x1fa)		/* address of 4 drive status	*/
#define CRB_OFFSET	(0x1a0)		/* address of Command RES. block */
#define CSLOP		(CRB_OFFSET - sizeof(MCSB) - (NUM_CQES * (sizeof(CQE) + sizeof(IOPB_BUFF))) - sizeof(VSMD_CIB))
#define MSLOP	(U4STATBASE  - CRB_OFFSET - sizeof(CRB))
/*
 * SHORT i/o definition for 4200/4201 in MACSI mode
 *
 */

typedef struct {
	MCSB		vsmd_mcsb;		/* this cannot move */
	CQE		vsmd_cqe[NUM_CQES];	/* this cannot move */
	IOPB_BUFF	vsmd_iopb[NUM_CQES];
	VSMD_CIB	init_info;
	BYTE		bs_slop[CSLOP];
	CRB		vsmd_crb;
	BYTE		bs_slop2[MSLOP];
	BYTE		vsmd_dstatus[4];	/* this cannot move */
	ushort	ipdscr;			/* 1FE,1FF: status change register */
}VSMD_MSHIO;
/* 
 * IOPB TYPES
 */

#define VSCSI_IOPB_TYPE	0		/* for Jaguar 4210 SCSI controller */
#define DISK_IOPB_TYPE	1		/* for 4201/4200 DISK iopbs	   */
					/* MUST use 4-UNIT mode		   */
#define VTAPE_IOPB_TYPE	2		/* for VTAPE emulation on 4201	   */

/*
 * MSR values
 */

#define MSR_CNA		BIT(0)		/* Controller not available	*/
#define MSR_BOK		BIT(1)		/* MACSI Board OK		*/
/*
 * MCR values
 */
#define MCR_SLED	BIT(15)		/* disable status led 		*/
#define MCR_SFEN	BIT(13)

/*
 * CRSW values
 */
#define CRSW_CRBV	BIT(0)		/* Command response block valid	*/
#define CRSW_CC		BIT(1)		/* Command commplete		*/
#define CRSW_ER		BIT(2)		/* Error on last command	*/
#define CRSW_EX		BIT(3)		/* Exception on last command	*/
#define CRSW_FQ		BIT(4)		/* Flush Q complete		*/
#define CRSW_QMS	BIT(5)		/* Q mode started (4210 ONLY)	*/
#define CRSW_CQA	BIT(6)		/* CQE entry available		*/

/*
 * IQAR values
 */
#define IQAR_IQEA	BIT(15)

/*
 * CQE command values
 */
#define CQE_DKIP_GO	BIT(0)
#define CQE_HPC		BIT(2)		/* run command next	*/
#define CQE_SORT	BIT(7)		/* sort entry into Q	*/

/*
 * MACSI init flags
 */
				/* BIT 0 not used		*/
#define M_OVL_SEEK	BIT(1)	/* enable overlapped seeks	*/
#define M_RESTORE	BIT(2)	/* enable auto-restore on drive faults */
#define M_RROBIN	BIT(3)	/* Force Round-Robin Disk selection */

/*
 * MACSI MACROS
 *
 */

#define VSMD_WSIZ (sizeof(VSMD_IOPB)/sizeof(USHORT))  /* rnds 2 16 bit wrds*/

#define VSMD_CSR(c)     c->ci_device->ipcsr
#define VSMD_DONE_GO(c) \
        VSMD_CSR(c) = (CS_GO | (~(CS_DONE | CS_ERR_LC) & VSMD_CSR(c)))
#define VSMD_DONE(c)    VSMD_CSR(c) &= ~(CS_DONE | CS_ERR_LC)

#define MACSI_CRSW(c)    ((VSMD_MSHIO*)(c->ci_device))->vsmd_crb.crsw
#define MACSI_MSR(c)    ((VSMD_MSHIO*)(c->ci_device))->vsmd_mcsb.msr
#define MACSI_IQAR(c)	 ((VSMD_MSHIO*)(c->ci_device))->vsmd_mcsb.iqar
#define MACSI_CQE_INDX(c) \
	(c->ci_cqe_index)
#define MACSI_CQE(c) \
	((VSMD_MSHIO*)(c->ci_device))->vsmd_cqe[MACSI_CQE_INDX(c)]
#define MACSI_QECR(c) \
	MACSI_CQE(c).control
#define MACSI_IOPB_ADDR(c) \
&(((VSMD_MSHIO*)(c->ci_device))->vsmd_iopb[MACSI_CQE_INDX(c)])
#define MACSI_RIOPB_ADDR(c) \
	&(((VSMD_MSHIO*)(c->ci_device))->vsmd_crb.copyiopb)
#define MACSI_CMDTAG(c) \
	 (VSMD_XIOPB*)(((VSMD_MSHIO*)(c->ci_device))->vsmd_crb.cmdtag_l + \
	 (((VSMD_MSHIO*)(c->ci_device))->vsmd_crb.cmdtag_h << 16))

/*----------------------------------------------------------------------*
 *			END of MACSI definitions			*
 *----------------------------------------------------------------------*/
#endif

/*
 * uib (unit initialization block) format
 */
typedef struct ipuib {
#ifdef	MIPSEL
	unchar	ipu_v0nh;	/* vol 0 number of heads */
	unchar	ipu_v0sh;	/* vol 0 starting head */
	unchar	ipu_v1nh;	/* vol 1 number of heads */
	unchar	ipu_v1sh;	/* vol 1 starting head */
	unchar	ipu_skew;	/* spiral skew factor */
	unchar	ipu_sectrk;	/* sectors per track */
	unchar	ipu_bytseclo;	/* bytes per sector low */
	unchar	ipu_bytsechi;	/* bytes per sector high */
	unchar	ipu_gap2;	/* words in gap2 */
	unchar	ipu_gap1;	/* words in gap1 */
	unchar	ipu_retries;	/* max number of retries on data error */
	unchar	ipu_intrlv;	/* sector interleave factor */
	unchar	ipu_cyllo;	/* number of cylinders, low */
	unchar	ipu_cylhi;	/* number of cylinders, high */
	unchar	ipu_mbz;	/* reserved */
	unchar	ipu_attrib;	/* attribute flags */
	unchar	ipu_statvec;	/* status change interrupt vector */
	unchar	ipu_statipl;	/* status change interrupt level */
#endif
#ifdef	MIPSEB
	unchar	ipu_v0sh;	/* vol 0 starting head */
	unchar	ipu_v0nh;	/* vol 0 number of heads */
	unchar	ipu_v1sh;	/* vol 1 starting head */
	unchar	ipu_v1nh;	/* vol 1 number of heads */
	unchar	ipu_sectrk;	/* sectors per track */
	unchar	ipu_skew;	/* spiral skew factor */
	unchar	ipu_bytsechi;	/* bytes per sector high */
	unchar	ipu_bytseclo;	/* bytes per sector low */
	unchar	ipu_gap1;	/* words in gap1 */
	unchar	ipu_gap2;	/* words in gap2 */
	unchar	ipu_intrlv;	/* sector interleave factor */
	unchar	ipu_retries;	/* max number of retries on data error */
	unchar	ipu_cylhi;	/* number of cylinders, high */
	unchar	ipu_cyllo;	/* number of cylinders, low */
	unchar	ipu_attrib;	/* attribute flags */
	unchar	ipu_mbz;	/* reserved */
	unchar	ipu_statipl;	/* status change interrupt level */
	unchar	ipu_statvec;	/* status change interrupt vector */
#endif
}IPUIB;

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
 * reserved attribute flags
 * (ipu_mbz)
 */

#define	RA_MSPARE	BIT(0)	/* multiple spares (uses init long command) */
#define RA_4UNIT	BIT(6)	/* init controller for 4 unit mode	*/
#define RA_EXTADDR	BIT(7)	/* Extended addressing for > 1023 cylinders */
#define ESDI_HARD	0x00	/* ESDI with hard sectors		*/
#define ESDI_SERVO	0x01	/* ESDI with embedded servo		*/
#define ESDI_SOFT	0x10	/* ESDI with soft sectoring		*/

#ifndef HI16
/*
 * misc macros
 */
#define	HI16(x)	((unsigned)(x) >> 16)
#define	LO16(x)	((unsigned)(x))
#define	HI8(x)	((unsigned)(x) >> 8)
#define	LO8(x)	((unsigned)(x))
#endif


#define	IP_MAXIOPBS	32
#define MAX_BADBLOCKS	600
#define MAX_ERRBLOCKS	85
#define DKIPUPC		4			/* 4 drives per controller */


/* per unit information */
typedef struct	dkipunitinfo {
	daddr_t	ui_bn;			/* block being transfered */
	unchar	ui_command;		/* command in progress */
	unchar	ui_attached;		/* non-zero if it attached */
	unchar	ui_vhvalid;		/* non-zero if vh is valid */
	unchar	ui_unit;		/* which unit this is */
	struct	volume_header ui_vh;	/* volume header */
	struct	bst_table *ui_bsttab;	/* bad sector table */
	struct	error_table *ui_errtab;	/* error table */
	ushort	ui_nretries;		/* # of retries for current command */
 	ushort	ui_maxretries;		/* max number of retries	*/
	uint	ui_spc;			/* sectors per cyl */
	struct	iobuf *ui_tab;		/* pointer to unit table */
	struct	iotime *ui_iotime;	/* pointer to io statistics */
	struct	buf ui_ctab;		/* command buffer for special cmds */
	IPIOPB	ui_iopb;		/* iopb for special commands	*/
	IPUIB	ui_uib;			/* uib area for init & special cmds */
	struct	dkipctlrinfo *ui_ci;	/* back pointer to controller info */
	struct	buf *ui_bp;		/* buffer in progress */
	unchar 	*ui_dstatus;		/* pointer to drive status */
	uint	ui_lastcyl;		/* current location for sort index  */
 	ulong	ui_softcnt;		/* Count of soft errors		 */
 	ulong	ui_hardcnt;		/* Count of hard errors		 */
	u_char  ui_open[16];		/* # of open calls per partition */
	unchar	ui_ilv_drv;		/* interrupt lvl & 4 unit drive num */
 	unchar  ui_ecc;			/* 1 if ecc is enabled, 0 if not */
	struct fmt_map_info ui_fmi;	/* Need one structure per unit*/
	char  ui_md[DEV_BSIZE];		/* ditto.  However this had to be */
					/* at least a sector size	*/
#ifdef PART_GATS
	unchar ui_partial;		/* partial command in prog	*/
#endif
}DKIPUNITINFO;

/*
 * Controller BIT flags
 *
 */

#define	CI_SG		BIT(0)		/* do Scatter/Gather		*/
#define CI_WSG		BIT(1)		/* do Word Wide Scatter/Gather	*/
#define	CI_CACHE	BIT(2)		/* Is running Read-Ahead	*/
#define	CI_4UNIT	BIT(3)		/* Is running in 4 Unit mode	*/
#define	CI_MACSI	BIT(4)		/* Is running in MACI mode	*/
#define CI_SORT		BIT(5)		/* Is using Sorting in MACSI	*/
#define CI_OVLSEEK	BIT(6)		/* OVERLAPPED SEEKS in MACSI	*/
#define CI_AUTOREST	BIT(7)		/* AUTO drive clear in MACSI	*/

/* per controller information */
typedef struct	dkipctlrinfo {
	ulong	ci_sintr;		/* # of spurious interrupts	*/
	ulong	ci_istart;		/* cmds started from interrupt	*/
	ulong	ci_pending;		/* command pending completion	*/
	ulong	ci_timeouts;		/* # of commands that timed out */
	unchar	ci_sgOK;		/* non-zero if can scat/gath	*/
	unchar	ci_cacheOK;		/* non-zero if can use cache	*/
	unchar	ci_ctlr;		/* controller #			*/
	unchar	ci_addrmod;		/* Addr modifier for usual i/o	*/
	struct	dkipunitinfo ci_unit[DKIPUPC];	/* unit stuff		*/
	struct	ipiopb ci_iopb[IP_MAXIOPBS];	/* iopbs for big jobs	*/
	struct	ipdevice *ci_device;	/* pointer to i/o registers	*/
	struct	iobuf *ci_tab;		/* pointer to controller table	*/
	int	ci_timeid;		/* timeout id			*/

	/* static buffer info */
	int	ci_bufinuse;		/* bp containing temp buffer	*/
	unchar	ci_pflgs;		/* wanting polled i/o		*/
	unchar	ci_isinit;		/* board has been initialized	*/
	ushort	ci_bufdata;		/* count of bytes in ui_buf	*/
	caddr_t	ci_bufva;		/* kernel vaddr to copy ui_buf	*/
	long	ci_buf[NBPSCTR/sizeof(long)];	/* static buffer	*/

	ushort	ci_mode;		/* for NEW style option settings*/
	HNDSHK	ci_hndshk;		/* handshake info		*/
	ulong	ci_csh;			/* Logical Cache Section descriptor */
	ulong	ci_sph;			/* Descriptor for the ci's IO map*/
	ulong	ci_iodata_sph;		/* Descriptor for data in IO request,
					 * usually used in NON-MACSI mode */
	ulong   ci_cmdtime;		/* lbolt when last command completed */
#ifndef NOMACSI
	ushort	ci_cqe_index;		/* index of next cqe to use */
	IPSG_DKIP_FREE *dkipsg_hd;	/* points to next free entry	*/
	IPSG_DKIP_FREE dkipsg_fentry[NUM_DKIP_SG]; /* scat/gath free list	*/
	IOINFO_DKIP_T *dkip_ioinfo_hd;	/* points to free ioinfo entry */
	IOINFO_DKIP_T dkip_ioinfo[NUM_DKIP_IOINFO]; /* ioinfo entries */
#endif
}DKIPCTLRINFO;

#ifdef	INKERNEL
/*
 * lboot defines the bounds of these data structures
 */
extern	struct iotime dkipiotime[][DKIPUPC];	/* io statistics */
extern	struct dkipctlrinfo *dkipctlrptr[];	/* controller software state */
extern	int dkipmajors;				/* internal major # */
extern	int dkiparray[];			/* internal major # */
extern	int dkipctlrs;				/* number of controllers */
extern	char dkipecc[][DKIPUPC];		/* ecc disabled/enabled */
#endif

