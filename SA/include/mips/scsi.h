#ident "$Header: scsi.h,v 1.16 90/11/11 10:00:36 hawkes Exp $"
/* $Copyright: $ */

/*
** Common SCSI related defines
**
** Must have included the following files
**     sys/types.h
**     sys/param.h
**     sys/buf.h
**     sys/dvh.h
**
*/

#ifndef _SYS_SCSI_
#define _SYS_SCSI_

#ifdef STANDALONE

#define HZ                      100
#define NBPP                    4096
#define NBPSCTR                 512
#define SCTRSHFT                9

/* K2 addresses are not used in standalone instead they are K0 or K1 */
#define K2_TO_PHYS(x)   ((unsigned)K1_TO_PHYS(x))
#define K2_TO_K1(x)     ((unsigned)K0_TO_K1(x))
#define K2_TO_K0(x)     ((unsigned)K1_TO_K0(x))

#define emajor(x)       (int)(((unsigned)(x)>>8)&0xff)
#define poff(x)         ((unsigned)(x) & (NBPP-1))

#define	IO_TO_DEV(i)	(makedev(i->i_ctlr,(((i->i_unit) << 4) | i->i_part)))
#define SP_WAIT		POLLED
#define SP_NOWAIT	POLLED
#define TP_BLKS_PER_REC	32
#define TP_BLKSIZ	(TP_BLKS_PER_REC * NBPSCTR)
#define TP_SHIFT	5
#define WAITREADY	100

#else STANDALONE

#define K2_TO_PHYS(x)	(ctob(kvtokptbl((unsigned)x)->pgm.pg_pfn) |\
			(((unsigned)x) & POFFMASK))
#define K2_TO_K1(x)	((unsigned)K2_TO_PHYS(x) | 0xa0000000)
#define K2_TO_K0(x)	((unsigned)K2_TO_PHYS(x) | 0x80000000)

#define SP_WAIT		INTERRUPT|WAIT
#define SP_NOWAIT	INTERRUPT

#endif STANDALONE

#define TARGET(dev)		((minor(dev) >> 4) & 0x0f)
#define TAR_LUN(target,lun,Nlun)		((target)*(Nlun) + (lun))
#define TARMAJOR_DEV(target,maj)	(makedev((maj),((target)<<4)))
#define FS(dev)		((dev) & 0xf)

#define MAXEOM		10
#define ERROR		0x80		/* set error condition */
#define INT_RETRY	3		/* Number of S/W Retries */
#define INT_POLLED	0		/* Polled mode of Controller */
#define INT_INTERRUPT	1		/* Interrupt mode of Controller */
#define	PHYSEOM		3		/* for SPACE command */
#define	SEQFILES	2		/* for SPACE command */
#define FILES 		1		/* for SPACE command */
#define BLOCKS 		0		/* for SPACE command */

#define MAXTRK		1		/* tracks to check for volume header */
#define NRETRIES	3	 /* # retries on a hard error */
#define SG		1	 /* use scatter/gather */
#define NOSG		0	 /* don't use scatter/gather */
#define DMA_READ	1	 /* dma read operation */
#define DMA_WRITE	0	 /* dma write operation */
#define WAIT		1	 /* wait for io to complete */
#define WAIT_NO		0	 /* don't wait for io to complete */
#define MAXLBA		0x200000 /* 2 meg is max lba for 6-byte read/write */
#define POLL_DELAY	80000000 /* delay used by low-level code for POLLED 
				    commands */
#define POLL_LOOP_FACTOR	10000
#define POLL_LOOP_DELAY		128

/*
 * Time constants.  These were measured for a 450 foot tape, then fudged
 * up, then normalized for a 650 foot tape.  Thus, they should be plenty
 * long enough.  The units are seconds.
 */
#define	TIME_FORMAT	(60 * 45)		/* no more than 45 minutes */
#define	TIME_FSF	(60 * 17)		/* no more than 17 minutes */
#define	TIME_REWIND	(60 * 4)		/* no more than 4 minutes */
#define	TIME_RDWR	(60 * 2)		/* no more than 2 minutes */
#define	TIME_RESET	(TIME_FSF + TIME_REWIND)
#define	TIME_RETEN	(60 * 4)		/* no more than 4 minutes */
#define	TIME_STARTSTOP	(60 * 6)		/* no more than 4 minutes */

/* these two are used for the mode select command
 */
#define PAGE_FORMAT	0x10		/* indicates page data complies with
					 * the page format */
#define SAVE_PARAMS	0x1		/* indicates that the page parameter
					 * data being set should be save by
					 * the target permanently */
/* defines for mode sense/select pagecode */
#define		  PD_ERROR	0x01	/*	Error Recovery Parameters */
#define		  PD_DISREC	0x02	/*	Disconnect Reconnect Params */
#define		  PD_FORMAT	0x03	/*	Format Parameters Parameters */
#define		  PD_GEOM	0x04	/*	Drive Geometry Parameters */
#define		  PD_RDAHD	0x08	/*	Read-ahead control Parameters */
#define		  PD_FLOP	0x21	/*	Floppy Specific Parameters */
#define		  PD_CACHE	0x38	/*	Cache Control Parameters */
#define		  PD_ALL	0x3F	/*	All Pages */

/*
 * SCSI Message system defines
 */
#define MSG_CMDCMP	0	/* command complete message */
#define MSG_EXT		1	/* extended message */
#define MSG_SAVEPTR	2	/* save pointers message */
#define MSG_RSTRPTR	3	/* restore pointers message */
#define MSG_DISCON	4	/* disconnect message */
#define MSG_ABORT	6	/* abort message */
#define MSG_REJECT	7	/* rejected message */
#define MSG_NOOP	8	/* no operation message */
#define MSG_PARERR	9	/* parity error message */
#define MSG_DEVRST	10	/* device reset message */
#define MSG_ID		0x80	/* bit set indicates an ID message */

#define MSG_SYNC_CODE	1	/* extended message code for syncronous */
#define EXT_MSGLEN	3	/* syncronous message length */
#define SYNC_OFFSET	8	/* maximum sync offset supported */
#define SYNC_XFER_RATE	0x3f	/* maximum sync tranfer rate supported
				 * 0x3e X 4ns = 248ns minimum between ack
				 * pulses is as fast as we go (4.0 mb/s) */
#define	IDENTIFY	0x80	/* mandatory bit says this is an IDENTIFY MSG */
#define DIS_REC_YES	0x40	/* discon/recon is supported by INITIATOR */
#define DIS_REC_NO	0x00	/* discon/recon is NOT supported by INITIATOR */
#define LUN_MASK	0x7

/* Class Code for Group 0 Commands */
#define CD6BYTE		0x0		/* SCSI - Group 0 Command */
/* Class Code for Group 1 Commands */
#define CD10BYTE	0x20		/* SCSI - Group 1 Command */
/* Class Code for Group 5 Commands */
#define CD12BYTE	0xa0		/* SCSI - Group 5 Command */
#define GROUP_MASK	0xe0		/* mask to isolate group code */
#define CMD_MASK	0x1f		/* mask to isolate command code */

#ifdef STANDALONE
#define NOREWIND(x)	(0)
#else STANDALONE
#define NOREWIND(x)	(x & 1)		/* Tape No Rewind */
#endif STANDALONE

#define XB(x)		(((unsigned long)x>>24)&0xff)
#define HB(x)		(((unsigned long)x>>16)&0xff)
#define MB(x)		(((unsigned long)x>>8)&0xff)
#define LB(x)		((unsigned long)x&0xff)
#define HS(x)		(((unsigned long)x>>16)&0xffff)
#define LS(x)		(((unsigned long)x)&0xffff)

/*
 * Scsi Check Condition defines
 */
#define FM		0x80	/* Indicates that a File Mark has been read */
#define EOM		0x40	/* EOM or BOT has been reached */
#define VALID		0x80	/* Indicates that the residual length field */
				/* is defined (request sense command) */
/* 
 * Sense Keys
 */
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

struct    cdb_0 {		    	/* COMMAND GROUP 0 */
	u_char	cdb_0_cmd;		/* command code */
	u_char	cdb_0_lun	: 3;	/* logical unit number */
	u_char	cdb_0_lba_h	: 5;	/* high part of address */
	u_char	cdb_0_lba_m;		/* middle part of address */
	u_char	cdb_0_lba_l;		/* low part of address */
	u_char	cdb_0_len;		/* block count */
	u_char	cdb_0_rsvd	: 6;	/* reserved */
	u_char  cdb_0_fr	: 1;	/* flag request */
	u_char  cdb_0_link	: 1;	/* link */
};

struct    cdb_1 {		    /* COMMAND GROUP 1 */
	u_char	cdb_1_cmd;		/* command code */
	u_char	cdb_1_lun	: 3;	/* logical unit number */
	u_char	cdb_1_rsvd1	: 4;	/* reserved */
	u_char	cdb_1_reladr	: 1;	/* relative address */
	u_char	cdb_1_lba_h;		/*				   */
	u_char	cdb_1_lba_mh;		/* (4 bytes) logical block address */
	u_char	cdb_1_lba_ml;		/*				   */
	u_char	cdb_1_lba_l;		/*				   */
	u_char	cdb_1_rsvd2;		/* reserved */
	u_char	cdb_1_len_h;		/* length (high) */
	u_char	cdb_1_len_l;		/* length (low) */
	u_char	cdb_1_rsvd3	: 6;	/* reserved */
	u_char  cdb_1_fr	: 1;	/* flag request */
	u_char  cdb_1_link	: 1;	/* link */
};

union	scsi_cdb {		/* SCSI command block */
	struct	cdb_0 cdb_0;
	struct	cdb_1 cdb_1;
    	u_int	cdb_raw[4];	 /* pad to 16 bytes */
};
struct scsi_iopb {
	u_short 	scsi_flags;	/* bits to indicate various options */
	u_short		scsi_taskid;	/* Task ID */
	u_char		scsi_hwstatus;	/* hardware status */
	u_char		scsi_status;	/* SCSI status */
	u_char		scsi_target;	/* Target ID */
	u_char		scsi_lun;	/* logical unit number */
	struct scsi_unit *scsi_un;	/* pointer to unit structure */
	u_int 		scsi_bufaddr;	/* Buffer address when dma */
	u_int 		scsi_bufaddr0;	/* Buffer address when non-dma */
	u_int 		scsi_bufaddr1;	/* Buffer address when non-dma */
	u_int		scsi_count;	/* dma Transfer Count (in bytes) */
	u_int		scsi_count0;	/* special case of bytes to manually
					 * transfer before dma */
	u_int		scsi_count1;	/* special case of bytes to manually
					 * transfer after dma */
	u_int		scsi_count_all;	/* dma Transfer Count (in bytes)
					 * save total count for diag purposes */
#ifdef	DIAG
	u_int 		scsi_physaddr2;
	u_int		scsi_count2;
	u_int 		scsi_physaddr3;
	u_int		scsi_count3;
	u_int 		scsi_physaddr4;
	u_int		scsi_count4;
#endif	DIAG
	u_int		scsi_timeid;	/* timeout id */
	u_int		scsi_time;	/* seconds to wait */
	u_char		scsi_syncoff;	/* syncronous offset value */
	u_char		scsi_syncxfer;	/* syncronous transfer rate */
	u_int		scsi_extra; 	/* pointer to extra information */
	union scsi_cdb	cmd_blk;	/* Command Descriptor bytes */
};

/* scsi_flags bit definitions
 */
#define	XFER_MASK	0x000f	/* mask for transfer modes */
#define	MAN_XFER	0x0001	/* use manual transfer mode (Req/Ack) */
#define	PTM_XFER	0x0002	/* use program transfer mode(hardware)*/
#define	DMA_XFER	0x0004	/* use DMA transfer mode (hardware) */
#define	NO_XFER		0x0008	/* No data xfer for this command */
#define POLLED		0x0010  /* this is a POLLED xfer */
#define INTERRUPT	0x0020  /* this is an Interrupt driven transfer */
#define DISCON_RECON	0x0040  /* disconnect/reconnect supported */
#define MESSAGES	0x0080  /* full scsi message system supported */

/* Extended control
 */
#define ARB_DISABLE	0x2	/* set to disable arbitration */
#define RESET_TARGET	0x1	/* set to issue a reset to a SCSI target */

/* SCSI Status (defined in the SCSI standard)
 */
#define SCSI_OK		0x00
#define SCSI_CHECK	0x02	/* Check condition - use request sense cmd */
#define CHECK_CONDITION	0x02	/* Check condition - use request sense cmd */
#define SCSI_MET	0x04	/* condition met/good */
#define SCSI_BUSY	0x08    /* target is BUSY */
#define SCSI_INTER	0x10	/* intermediate/good */
#define SCSI_COMBO	0x14	/* intermediate/condition met/good */
#define SCSI_RESV	0x18	/* reservation conflict */
#define SCSI_STATUS_BITS "\20\5INTER\4BUSY\3CM\2CHECK\1PARITY"

/* NON scsi device Status (those errors that are not scsi device status)
 * the high-order bit is set to differentiate these errors with scsi
 * status.
 */
#define NOERROR		0x00	/* No Error */
#define SELTMO		0x81	/* Selection Timeout */
#define INVTARGET	0x82	/* Invalid Target ID */
#define INVUNIT		0x83	/* Invalid Unit Number */
#define PHASE_ERROR	0x84	/* SCSI Phase Error */
#define PARITY		0x85	/* SCSI Parity Error Unrecoverable */
#define HWERROR		0x86	/* Hardware Failure */
#define SCSITMO		0x87	/* SCSI Timeout Error */

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
#define	C1_READDEF	0x37		/* Read Defect Data */

/* Group 0 Commands - Sequential Access Devices
 */
#define	C0_REWIND	0x01		/* Rewind */
#define C0_RDBLOCK	0x05		/* Read Block Limits */
#define	C0_TRKSEL	0x0B		/* Select Track */
#define	C0_RDREV	0x0F		/* Read Reverse */
#define	C0_WRFM		0x10		/* Write Filemark */
#define	C0_SPACE	0x11		/* Space */
#define	C0_RECBUF	0x14		/* Recover Buffered Data */
#define	C0_MSELECT	0x15		/* Mode select */
#define	C0_ERASE	0x19		/* Erase */
#define	C0_MSENSE	0x1A		/* Mode sense */
#define	C0_LOAD		0x1B		/* Load / Unload */

/* Group 1 Commands - Sequential Access Devices - None */
/* Group 0 Commands - WORM Devices - None Different */
/* Group 1 Commands - WORM Devices - None Different */

/*
 * Special Command - Raw mode
 */
#define	CX_RAW		0x80

/* defines for the Inquiry command
 */
#define TYPE_DISK		0x00
#define TYPE_TAPE		0x01
#define TYPE_PRINTER		0x02
#define TYPE_CPU		0x03
#define TYPE_WORM		0x04	/* optical */
#define TYPE_RONLY_DISK		0x05	/* optical */
#define TYPE_SCANNER		0x06	/* scanner device */
#define TYPE_OPTICAL		0x07	/* optical */
#define TYPE_CHANGER		0x08	/* Medium changer */
#define TYPE_COMM		0x09	/* communication device */
#define TYPE_LUN_GONE		0x7f	/* unknown device */
#define TYPE_M12_FLOPPY		0x81

/* C_FLAGS & UN_FLAGS Flags
 */
#define INT_ALIVE	0x00000001	/* Device is Alive and Available */
#define INT_READY	0x00000002	/* Device is Ready */
#define INT_BUSY	0x00000004	/* Device is busy */
#define INT_WRITTEN	0x00000008	/* Written to */
#define INT_TAPE	0x00000010	/* A Tape Device */
#define INT_DISK	0x00000020	/* A Disk Device */
#define INT_WORM	0x00000040	/* A WORM Device */
#define INT_REWINDING	0x00000080	/* rewinding */
#define INT_SELECTED	0x00000100	/* Drive Selected */
#define INT_FLOPPY	0x00000200	/* M12 Floppy */
#define INT_WAITING	0x00000400	/* Waiting for Interrupt */
#define INT_WANTED	0x00000800	/* Wanted after Interrupt */
#define INT_OPEN	0x00001000	/* Driver Open Routine */
#define INT_READ	0x00002000	/* device has been read */
#define INT_EOM		0x00004000	/* EOM or BOT reached */
#define INT_FM		0x00008000	/* indicates a file mark was read */
#define INT_PRINTER	0x00010000	/* Printer Device */
#define INT_RMV_MEDIA	0x00020000	/* Floppy Device or ? */
#define INT_ARB_DISABLE	0x00040000	/* Disable arbitration for this device*/
#define INT_READONLY	0x00080000	/* read-only device */
#define INT_ATN		0x00100000	/* tape unit attention occurred */
#define INT_NOT_RDY	0x00200000	/* tape not ready */
#define INT_VARIABLE	0x00400000	/* tape has variable block size */
#define INT_RFM		0x00800000	/* file mark needs reporting */
#define INT_COMM	0x01000000	/* communications device */
#define INT_WRTVFY	0x02000000	/* write verify mode */
#define INT_BYTVFY	0x04000000	/* write byte compare verify mode */
#define INT_CLOBBER	0x08000000	/* setup for removal */
#define INT_PC8		0x10000000	/* use page 8 for cache enable */

struct	flop_msel {		/* data sent by CMD_MSELECT */
	u_long	msel_hdr;		/* medium type, blk_desc length all 0 */
	struct	{
	    u_char	desc_code;	/*   page code */
	    u_char	desc_len;	/*   page length */
	    struct pg_flop {		/* FLOPPY VU PARAMETERS */
		u_short	flop_ncyl;	/* number of cylinders per disk */
		u_short	flop_nbyte;	/* number of bytes per sector */
		u_short	flop_nsec;	/* number of sectors per track */
		u_char	flop_nhead;	/* number of drive heads (surfaces) */
		u_char	flop_xfer;	/* transfer rate */
		u_char	flop_norm_gap;	/* normal gap length */
		u_char	flop_fmt_gap;	/* format gap length */
		u_char	flop_mon;	/* motor on delay */
		u_char	flop_moff;	/* motor off delay */
		u_char	flop_hsd;	/* head settle delay in ms */
		u_char	flop_step_rate;	/* head step rate in ms */
		u_char	flop_hlt;	/* head load time,2ms units (1-127) */
		u_char	flop_hut;	/* head unload time,16ms units (1-15) */
		u_char	flop_mfm;	/* MFM encoding if non-zero */
		u_char	flop_hcap;	/* high capacity drive if non-zero */
	    }	desc_pg;		/* page descriptor */
	} msel_desc;
};

typedef struct mode_sense_hdr
{
	u_char	sense_data_len;		/* sense data length */
	u_char	medium_type;		/* medium type */
	u_char	WP	: 1;		/* write protected */
	u_char	BFM	: 3;		/* buffered mode */
	u_char	speed	: 4;		/* speed */
	u_char	blk_desc_len;		/* block descriptor length */
} SCSI_MS_HDR;

typedef struct mode_sense_blk_desc
{
	u_long	density_code	: 8;	/* density code */
	u_long	nrblks		: 24;	/* buffered mode */
	u_long	resv		: 8;	/* reserved */
	u_long	blk_len		: 24;	/* block length */
} SCSI_MS_BLK_DESC;

typedef struct mode_sense_page_hdr
{
	u_char	ps		: 1;	/* parameters savable */
	u_char	resv		: 1;	/* reserved */
	u_char	page_code	: 6;	/* page code */
	u_char  page_length;
} SCSI_MS_PAGE_HDR;

typedef struct mode_sense
{
	SCSI_MS_HDR		hdr;
	SCSI_MS_BLK_DESC	blk_desc;
} SCSI_MODE_SENSE;

typedef struct mode_sense_page_1	/* error recovery page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR error_hdr;	/* page header */
	u_char	awre		: 1;	/* auto write reallocation */
	u_char	arre		: 1;	/* auto read reallocation */
	u_char	tb		: 1;	/* transfer failing block */
	u_char	rc		: 1;	/* read continuous */
	u_char	eec		: 1;	/* enable early correction */
	u_char	per		: 1;	/* post error */
	u_char	dte		: 1;	/* disable transfer on error */
	u_char	dcr		: 1;	/* disable correction */
	u_char	retry_cnt;		/* retry count */
	u_char	corr_span;		/* correction span */
	u_char	head_off;		/* head offset count */
	u_char	dstrobe_off;		/* data strobe offset count */
	u_char	rtime;			/* recovery time limit */
} SCSI_MS_ERROR;

typedef struct mode_sense_page_2	/* disconnect/reconnect control page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR disrec_hdr;	/* page header */
	u_char	bfr;			/* buffer full ratio  */
	u_char	ber;			/* buffer empty ratio  */
	u_short	bil;			/* bus inactivity limit */
	u_short	dtl;			/* disconnect time limit */
	u_short	ctl;			/* connect time limit */
	u_short	resv;			/* reserved */
} SCSI_MS_DISREC;

typedef struct mode_sense_page_3	/* format parameters page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR format_hdr;	/* page header */
	u_short	tpz;			/* tracks per zone  */
	u_short	aspz;			/* alternate sectors per zone  */
	u_short	atpz;			/* alternate tracks per zone  */
	u_short	atpv;			/* alternate tracks per volume  */
	u_short	spt;			/* sectors per track */
	u_short	bps;			/* data bytes per physical sector */
	u_short	ilv;			/* interleave */
	u_short	tsf;			/* track skew factor */
	u_short	csf;			/* cylinder skew factor */
	u_char	ssec		: 1;	/* soft sectoring */
	u_char	hsec		: 1;	/* hard sectoring */
	u_char	rmb		: 1;	/* removable media */
	u_char	surf		: 1;	/* surface map */
	u_char	ins		: 1;	/* inhibit save */
	u_char	resv0		: 3;	/* reserved */
	u_char	resv1[3];		/* reserved */
} SCSI_MS_FORMAT;

typedef struct mode_sense_page_4	/* drive geometry parameters page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR geom_hdr;	/* page header */
	u_char	cyls_hb;		/* number of cylinders MSB */
	u_char	cyls_mb;		/* number of cylinders MID */
	u_char	cyls_lb;		/* number of cylinders LSB */
	u_char	heads;			/* number of heads */
	u_char	wpc_hb;			/* write precomp start MSB */
	u_char	wpc_mb;			/* write precomp start MID */
	u_char	wpc_lb;			/* write precomp start LSB */
	u_char	rwc_hb;			/* reduced write current start MSB */
	u_char	rwc_mb;			/* reduced write current start MID */
	u_char	rwc_lb;			/* reduced write current start LSB */
	u_short	dsr;			/* drive step rate */
	u_char	lz_hb;			/* loading zone cylinder MSB */
	u_char	lz_mb;			/* loading zone cylinder MID */
	u_char	lz_lb;			/* loading zone cylinder LSB */
	u_char	resv1[3];		/* reserved */
} SCSI_MS_GEOM;

typedef struct mode_sense_page_8	/* Read-ahead control parameters page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR rdahd_hdr;	/* page header */
	u_char	rsvd0		: 5;	/* reserved */
	u_char	wce		: 1;	/* write cache enable */
	u_char	ms		: 1;	/* multiple selection */
	u_char	rcd		: 1;	/* read cache disable */
	u_char	rrp		: 4;	/* read retention priority */
	u_char	wrp		: 4;	/* write retention priority */
	u_short	dptl;			/* disable prefetch transfer length */
	u_short	minpf;			/* minimum prefetch */
	u_short	maxpf;			/* maximum prefetch */
	u_short	maxpfc;			/* maximum prefetch ceiling */
} SCSI_MS_RDAHD;

typedef struct mode_sense_page_56	/* CDC cache control parameters page */
{
	SCSI_MODE_SENSE msense;
	SCSI_MS_PAGE_HDR cache_hdr;	/* page header */
	u_char	rsvd0		: 1;	/* reserved */
	u_char	wie		: 1;	/* write index enable */
	u_char	rsvd1		: 1;	/* reserved */
	u_char	ce		: 1;	/* cache enable */
	u_char	cts		: 4;	/* cache table size */
	u_char	pft;			/* prefetch threshold */
	u_char	maxpf;			/* maximum prefetch */
	u_char	maxpfm;			/* maximum prefetch multiplier */
	u_char	minpf;			/* minimum prefetch */
	u_char	minpfm;			/* minimum prefetch multiplier */
	u_char	resv2[8];		/* reserved */
} SCSI_MS_CACHE;

typedef struct read_limit_desc
{
	u_long	res0	: 8;		/* reserved */
	u_long	maxlen	: 24;		/* max block length */
	u_short	minlen;			/* min block length */
} SCSI_RDBLKLIM;

typedef struct  scsi_ext_sense {/* scsi extended sense for error class 7  */
    u_char   valid   : 1;        /* sense data is valid        (byte 0)    */
    u_char   class   : 3;        /* alway at binary 111        (byte 0)    */
    u_char           : 4;        /* alway at binary 0000       (byte 0)    */
    u_char   segment;            /* segment number             (byte 1)    */
    u_char   filmrk  : 1;        /* file mark                  (byte 2)    */
    u_char   eom     : 1;        /* end of media               (byte 2)    */
    u_char   ilength : 1;        /* incorrect length indicator (byte 2)    */
    u_char           : 1;        /* reserved                   (byte 2)    */
    u_char   key     : 4;        /* sense key                  (byte 2)    */
    u_char   info1;              /* information                (byte 3)    */
    u_char   info2;              /* information                (byte 4)    */
    u_char   info3;              /* information                (byte 5)    */
    u_char   info4;              /* information                (byte 6)    */
    u_char   add_len;            /* number of additional bytes (byte 7)    */
    u_char   copy_cmp1;          /* Reserved for Copy/Compare Command      */
    u_char   copy_cmp2;          /* Reserved for Copy/Compare Command      */
    u_char   res0;               /* Reserved byte                          */
    u_char   res1;               /* Reserved byte                          */
    u_char   sense_code;         /* Additional Sense Code                  */
    u_char   res2;               /* Reserved byte                          */
    u_char   fru;                /* Field Replacement Unit                 */
    u_char   fpv    : 1;         /* Field pointer valid                    */
    u_char   c_d    : 1;         /* Command/Data                           */
    u_char          : 2;         /* Reserved                               */
    u_char   bpv    : 1;         /* Bit pointer vaild                      */
    u_char   bitp   : 3;         /* Bit pointer                            */
    u_char   fp1;                /* Field pointer 1                        */
    u_char   fp2;                /* Field pointer 2                        */
    u_char   extra[10];          /* additional bytes follow, if any        */
} SCSI_EXT_SENSE;

typedef struct inquiry {
    u_char   device_type;        /* Peripheral Device Type                 */
    u_char   rm          : 1;    /* Removeable Media                       */
    u_char   dtq         : 7;    /* Device Type Qualifier                  */
    u_char               : 2;    /* Reserved                               */
    u_char   ecma        : 3;    /* ECMA Version                           */
    u_char   ansi        : 3;    /* ANSI Version                           */
    u_char   res0;               /* Reserved                               */
    u_char   length;             /* Additional Length                      */
    u_char   res1;               /* Reserved                               */
    u_char   res2;               /* Reserved                               */
    u_char   res3;               /* Reserved                               */
    u_char   vendor_id[8];       /* Vendor ID  (and null)                  */
    u_char   product_id[16];     /* Product ID  (and null)                 */
    u_char   revision_level[4];  /* Product Revision Level (and null)      */
    u_char   mcode_level[8];	 /* Microcode Revision Level */
    u_char   serial_nr[12];	 /* Drive serial number */
} SCSI_INQUIRY;

typedef struct readcap {
    int	     maxblk;        	 /* Max block address                      */
    int	     blklen;        	 /* block length                           */
} SCSI_READCAP;

/*
 * SCSI unit.  One allocated per SCSI TARGET/LUN pair.
 */
struct scsi_unit {
	u_int	 	un_flags;	/* target/lun Flags */
	struct scsi_unit *un_motorstart;/* must remain here */
	u_int		un_command;
	struct scsi_iopb *un_iopbp; 	/* IOPB pointer */
	struct iobuf	*un_dp;		/* pointer to head of queue */
	u_char		un_target;
	u_char		un_lun;
	u_char		un_vhvalid;
	u_char		un_nretries;
	u_char	 	un_eomcount;	/* count of ops past eom */
	u_char	 	un_weomcount;	/* count of write ops past eom */
	u_char		un_maxsg;	/* # of scatter/gather entries */
	u_char		un_sgentries;	/* # of scatter/gather entries */
	u_long		un_softcount;	/* count of soft errors */
	u_long		un_hardcount;	/* count of hard errors */
	u_long		un_dmaalign;	/* dma align size (must be power of 2 */
	u_long		un_dmastartmask;
	u_long		un_dmaaddmask;
	u_long		un_dmacntmask;
	u_long		un_sectsize;
	daddr_t		un_bn;
	daddr_t		un_prev_bn;
	long		un_prev_b_resid;
	int		un_xfer;	/* transfer count */
	int		un_resid;	/* resid from request sense */
	u_int  		un_prev_secs;  	/* sectors xfer'ed last */
	u_int		un_lastcyl;
	SCSI_INQUIRY	un_inq;		/* From Inquiry Command */
	SCSI_RDBLKLIM 	un_blklim;	/* Block limits info */
	SCSI_READCAP 	un_readcap;	/* Read capacity info */
	SCSI_MS_ERROR	un_mserror;	/* Mode sense page 1 */
	SCSI_MS_DISREC	un_msdisrec;	/* Mode sense page 2 */
	SCSI_MS_FORMAT	un_msformat;	/* Mode sense page 3 */
	SCSI_MS_GEOM	un_msgeom;	/* Mode sense page 4 */
	SCSI_MS_RDAHD	un_msrdahd;	/* Mode sense page 8 */
	SCSI_MS_CACHE	un_mscache;	/* Mode sense page 56 */
	SCSI_EXT_SENSE 	un_sense;	/* Last Request Sense bytes */
	struct volume_header *un_vh;	/* MIPS volumne header */
	struct volume_header *un_vh_k1ptr;/* MIPS volumne header */
	struct iotime 	*un_iotime;	/* io statistics */
	struct buf	un_buf;		/* buffer for raw i/o */
	struct scsi_iopb un_iopb;  	/* IOPB */
	u_int		un_extra;	/* pointer to extra information */
	u_char		*un_tmpbuf;	/* pointer to malloced temp buffer */
	u_char		*un_buf_64;	/* pointer to 16-word alligned 64 byte
					 * buffer needed for pizazz */
#ifdef STANDALONE
	int	un_tpxfer;		/* bytes xfered by last tape read */
	int	un_tpnxtrec;		/* next record on tape to read/write */
#endif
}; 

/*
 *	each scsi controller must supply this structure pointed to by 
 *	scsitab[TAR_LUN(target,lun)].io_s1.
 */
struct low_scsi {
	void	(*low_scsi_init)();	/* init controller dependent device */
	void	(*low_scsi_setupdma)();	/* init dma controller */
	int	(*low_scsi_startop)();	/* have controller start op */
	struct scsi_unit *low_scsi_un;	/* ptr to unit info structure */
	int	low_scsi_Ntarget;	/* max targets */
	int	low_scsi_Nlun;		/* max logical units / target */
};

/* The following macro allows you to specify the dp and the last component
 *	of the desired function to get at the function pointer.
 */
#define LOW_SCSI(dev,y) ((common_scsi_map[emajor(dev)])->low_scsi_/**/y)

/* the following routines are supplied by common_scsi and are called by
 *	your driver
 */
extern void	common_scsi_registerme();/* registers major # map to low_scsi */
extern void	common_scsi_read();
extern void	common_scsi_write();
extern void	common_scsi_attach();	/* get volume header */
extern void	common_scsi_intr();
extern int	common_scsi_slave();	/* performs inquiry command */
extern int	common_scsi_timeval();	/* timeout value for different ops */
extern int	common_scsi_size();
extern int	common_scsi_dump();
extern int	common_scsi_spcmd();
extern char*	common_scsi_printerr();	/* prints scsi error codes */
extern int	common_scsi_sense_status();
extern struct scsi_unit *common_scsi_getun();
#ifdef STANDALONE
extern int	common_scsi_open();	/* opens tape or disk */
extern int	common_scsi_close();
extern int	common_scsi_ioctl();
extern int	common_scsi_strategy();
#else
extern void	common_scsi_open();	/* opens tape or disk */
extern void	common_scsi_close();
extern void	common_scsi_ioctl();
extern void	common_scsi_strategy();
#endif

#endif _SYS_SCSI_
