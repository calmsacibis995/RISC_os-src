#ident "$Header: scsi_3030sa.h,v 1.1 90/02/28 11:30:23 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
** Pizazz SCSI related defines for SPP
*/
#define ALIGNED_BUF_SIZE	4096    /* size of our 64-byte aligned buffer */
#define HIGHLEVEL 	1
#ifdef STANDALONE
#define TAPEUNIT	6	/* tape streamer will have this scsi id */
#endif STANDALONE

#define ERROR		0x80		/* set error condition */
#define INT_RETRY	3		/* Number of S/W Retries */
#define INT_POLLED	0		/* Polled mode of Controller */
#define INT_INTERRUPT	1		/* Interrupt mode of Controller */
#define	PHYSEOM		3		/* for SPACE command */
#define	SEQFILES	2		/* for SPACE command */
#define FILES 		1		/* for SPACE command */
#define BLOCKS 		0		/* for SPACE command */

/* these two are used for the mode select command
 */
#define PAGE_FORMAT	0x10		/* indicates page data complies with
					 * the page format */
#define SAVE_PARAMS	0x1		/* indicates that the page parameter
					 * data being set should be save by
					 * the target permanently */

/* Class Code for Group 0 Commands */
#define CD6BYTE		0x0		/* SCSI - Group 0 Command */
/* Class Code for Group 1 Commands */
#define CD10BYTE	0x20		/* SCSI - Group 1 Command */
/* Class Code for Group 5 Commands */
#define CD12BYTE	0xa0		/* SCSI - Group 5 Command */
#define GROUP_MASK	0xe0		/* mask to isolate group code */
#define CMD_MASK	0x1f		/* mask to isolate command code */

#define NOREWIND(x)	(x & 1)		/* Tape No Rewind */

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
/* Sense Keys			 * is defined (request sense command) */
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

struct scsi_iopb {
	u_char		scsi_taskid;	/* Task ID */
	u_int 		scsi_bufaddr;	/* Buffer address when dma */
	u_int 		scsi_bufaddr0;	/* Buffer address when non-dma */
	u_int 		scsi_bufaddr1;	/* Buffer address when non-dma */
	u_int		scsi_count;	/* dma Transfer Count (in bytes) */
	u_int		scsi_count0;	/* special case of bytes to manually
					 * transfer before or instead of dma */
	u_int		scsi_count1;	/* special case of bytes to manually
					 * transfer after dma */
#ifdef	DIAG
	u_int 		scsi_physaddr2;
	u_int		scsi_count2;
	u_int 		scsi_physaddr3;
	u_int		scsi_count3;
	u_int 		scsi_physaddr4;
	u_int		scsi_count4;
#endif	DIAG
	u_char		scsi_hwstatus;	/* hardware status */
	u_char		scsi_status;	/* SCSI status */
	u_char		scsi_target;	/* Target ID */
	u_char		scsi_lun;	/* logical unit number */
	u_int		scsi_timeid;	/* timeout id */
	u_int		scsi_time;	/* seconds to wait */
	u_short 	scsi_flags;	/* bits to indicate various options */
	u_char		scsi_syncoff;	/* syncronous offset value */
	u_char		scsi_syncxfer;	/* syncronous transfer rate */
	u_char		scsi_sgentries;	/* number of scatter/gather entries 
					 * in the scatter/gather list */
	u_char		scsi_residual;	/* special case of bytes to manually
					 * transfer before dma */
	u_int 		scsi_manaddr;	/* special case manual xfer addr */
	struct scsisge* dma_blk_ptr; 	/* pointer to chain of scat/gather
					   * entries */
	struct scsi_unit *scsi_un;	/* pointer to unit structure */
	u_int		scsi_extra; 	/* pointer to extra information */
	/* Command Descriptor bytes */
	u_char	cmd_b0;
	u_char	cmd_b1;
	u_char	cmd_b2;
	u_char	cmd_b3;
	u_char	cmd_b4;
	u_char	cmd_b5;
	u_char	cmd_b6;
	u_char	cmd_b7;
	u_char	cmd_b8;
	u_char	cmd_b9;
	u_char	cmd_b10;
	u_char	cmd_b11;
};

struct scsisge {
	u_int	mem_ptr;	/* src or dest of data to xfer */ 
	u_short count;		/* '16-word' block count for this xfer */
	u_int	chanmode;	/* channel initialization info */
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

#define MAX_SGENTRY	65	/* max X 4k => 256k byte transfer + 1 */

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
#define	C0_ERASE	0x19		/* Erase */
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
#define TYPE_LUN_GONE		0x7f
#define REMOVE_MEDIA 		0x80	/* bit set indicates removable media */

/* C_FLAGS & UN_FLAGS Flags
 */
#define INT_ALIVE	0x000001	/* Device is Alive and Available */
#define INT_READY	0x000002	/* Device is Ready */
#define INT_BUSY	0x000004	/* Device is busy */
#define INT_WRITTEN	0x000008	/* Written to */
#define INT_TAPE	0x000010	/* A Tape Device */
#define INT_DISK	0x000020	/* A Disk Device */
#define INT_WORM	0x000040
#define INT_REWINDING	0x000080	/* rewinding */
#define INT_SELECTED	0x000100	/* Drive Selected */
#define INT_FORMATTED	0x000200	/* Drive Formatted - Do I need this */
#define INT_WAITING	0x000400	/* Waiting for Interrupt */
#define INT_WANTED	0x000800	/* Wanted after Interrupt */
#define INT_OPEN	0x001000	/* Driver Open Routine */
#define INT_READ	0x002000	/* device has been read */
#define INT_EOM		0x004000	/* EOM or BOT reached */
#define INT_FM		0x008000	/* indicates a file mark was read */
#define INT_PRINTER	0x010000	/* Printer Device */
#define INT_RMV_MEDIA	0x020000	/* Floppy Device or ? */
#define INT_ARB_DISABLE	0x040000	/* Disable arbitration for this device*/
#define INT_READONLY	0x080000	/* read-only device */
#define INT_ATN		0x100000	/* tape unit attention occurred */
#define INT_NOT_RDY	0x200000	/* tape not ready */

/* generic scsi inquiry data
 */
struct inquiry {
	char	inq_type;		/* 0 */
	char	inq_qual;		/* 1 */
	char	inq_ansi;		/* 2 */
	char	inq_r0;			/* 3 Reserved */
	char	inq_addlen;		/* 4 */
	char	inq_r1;			/* 5 Reserved */
	char	inq_r2;			/* 6 Reserved */
	char	inq_r3;			/* 7 Reserved */
	char	inq_vendor[8];		/* 8-15 */
	char	inq_product[16];	/* 16-31 */
	char	inq_rev[4];		/* 32-35 */
};
#define PER	0x4	/* report ecc error (correction) to initiator */
#define DTE	0x2	/* disable data transmission on recovered error */
#define DCR	0x1	/* disable ecc application */
/* error recovery page struct for mode select command */
struct err_page {
	u_char	mode_sel[4];		/* mode select header */
/*	u_char	block_desc[8];		/* block descripter */
	u_char	err_pagecode;		/* error recovery page code */
	u_char	err_pagelength;		/* error recovery page length */
	u_char	err_flags;		/* error recovery page flags */
	u_char	err_retry_count;	/* recovery retry count */
	u_char	err_reserved0;		/* error recovery parameters */
	u_char	err_reserved1;		/* error recovery parameters */
	u_char	err_reserved2;		/* error recovery parameters */
	u_char	err_reserved3;		/* error recovery parameters */
};
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

/*
 * SCSI unit.  One allocated per SCSI TARGET/LUN pair.
 */
struct scsi_unit {
	/* this is all we need! */
	u_char		*un_buf_64;	/* pointer to 16-word alligned 64 byte
					 * buffer needed for pizazz */
	u_char		*un_vh;		/* MIPS volumne header */
	u_long		un_dmastartmask;
	u_long		un_dmaaddmask;
	u_long		un_dmacntmask;
}; 


#ifndef STANDALONE
/*
 * lboot defines the bounds of these data structures
 */
extern	struct iotime scsiiotime[][7];	/* io statistics */
#endif STANDALONE


typedef struct new_inquiry {
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
} SCSI_INQUIRY;


/* defines for the Inquiry command
 */
#define TYPE_DISK		0x00
#define TYPE_TAPE		0x01
#define TYPE_PRINTER		0x02
#define TYPE_CPU		0x03
#define TYPE_WORM		0x04	/* optical */
#define TYPE_RONLY_DISK		0x05	/* optical */
#define TYPE_COMM		0x09	/* communication device */
#define TYPE_LUN_GONE		0x7f
#define TYPE_M12_FLOPPY		0x81
#define REMOVE_MEDIA 		0x80	/* bit set indicates removable media */
