#ident "$Header: dscsi.h,v 1.5 90/01/23 14:11:59 huang Exp $"
/* $Copyright$ */
/*	%Q%	%I%	%M%	*/

#define FILES 		1	/* SPACE command will space FILES */
#define BLOCKS 		0	/* SPACE command will space BLOCKS */

#define TAPEUNIT	6	/* tape streamer will have this scsi id */
/*
 * SCSI Identify Message codes
 * these bits or'ed together with IDENTIFY make up an identify message. 
 */
#define	IDENTIFY	0x80	/* mandatory bit says this is an IDENTIFY MSG */
#define DIS_REC_YES	0x40	/* discon/recon is supported by INITIATOR */
#define DIS_REC_NO	0x00	/* discon/recon is NOT supported by INITIATOR */
#define UNIT_0		0x00	/* logical Unit 0 for a target */
#define UNIT_1		0x01	/* logical Unit 1 for a target */
#define UNIT_2		0x02	/* logical Unit 2 for a target */
#define UNIT_3		0x03	/* logical Unit 3 for a target */
#define UNIT_4		0x04	/* logical Unit 4 for a target */
#define UNIT_5		0x05	/* logical Unit 5 for a target */
#define UNIT_6		0x06	/* logical Unit 6 for a target */
#define UNIT_7		0x07	/* logical Unit 7 for a target */

/* Group 0 Commands - All device Types
 */
#define C0_TESTRDY	0x00		/* Test Unit Ready */
#define C0_REQSENSE	0x03		/* Request Sense */
#define C0_INQUIRY	0x12		/* Inquiry */
#define C0_COPY		0x18		/* Copy */
#define C0_RECDIAG	0x1C		/* Receive Diagnostic Results */
#define C0_SENDDIAG	0x1D		/* Send Diagnostic Results */

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


/* SCSI Status
 */
#define SCSI_OK		0x00
#define CHECK_CONDITION	0x02
#define SCSI_CHECK	0x02	/* Check condition - use request sense cmd */
#define SCSI_MET	0x04	/* result of successful search data cmd */
#define SCSI_INTER	0x10	/* intermediate success of linked cmd */
#define SCSI_COMBO	0x14	/* combination of sucess for a linked series */
#define SCSI_RESV	0x18	/* reservation conflict (reserved device) */

/* NON scsi device Status (those errors that are not scsi device status)
 * the high-order bit is set to differentiate these errors with scsi
 * status.
 */
#define NOERROR		0x00	/* No Error */
#define SELTMO		0x81	/* Selection Timeout */
#define SEL_ERROR	0x81	/* Selection Error */
#define INVTARGET	0x82	/* Invalid Target ID */
#define INVUNIT		0x83	/* Invalid Unit Number */
#define PHASE_ERROR	0x84	/* SCSI Phase Error */
#define PBERROR		0x85	/* I/O Parameter Block Negated */
#define PARITY		0x86	/* SCSI Parity Error Unrecoverable */
#define HWERROR		0x87	/* Hardware Failure */
#define SCSITMO		0x88	/* SCSI Timeout Error */
#define REQ_ASRT_TOUT 	0x89	/* timed out waiting for target to assert REQ */
#define REQ_DEASRT_TOUT	0x8a	/* timed out waiting for tar to de-assert REQ */
#define PTM_IN_TOUT	0x8b	/* timed out during ptm mode 'data-in' op */
#define PTM_OUT_TOUT	0x8c	/* timed out during ptm mode 'data-in' op */
#define PTM_XFER_ST	0x8d
#define PTM_ST_TOUT	0x8e
#define PTM_ST_NOINT	0x8f
#define DMA_TOUT 	0x90
#define DMA_XFER_ST	0x91 
/*
 * format of generic scsi i/o parameter block
 */
struct scsiiopb {
	u_char	scsi_status;		/* status byte from target */
	u_char	hw_status;		/* status byte from target */
	u_short flags;			/* bits to indicate various options */
	u_char	scsi_sgentries;		/* number of scatter/gather entries 
					 * in the scatter/gather list */
	struct scsi_dma_blk* dma_blk_ptr; /* pointer to chain of scat/gather
					   * entries */
	u_char scsi_id;			/* needed since tape io->i_unit will
				 	 * be 0 but unit scsi id will be 6? 
					 */
	/* The Command Descriptor Bytes
	 */
	unsigned char	cmd_b0;
	unsigned char	cmd_b1;
	unsigned char	cmd_b2;
	unsigned char	cmd_b3;
	unsigned char	cmd_b4;
	unsigned char	cmd_b5;
	unsigned char	cmd_b6;
	unsigned char	cmd_b7;
	unsigned char	cmd_b8;
	unsigned char	cmd_b9;
	unsigned char	cmd_b10;
	unsigned char	cmd_b11;
};
/*
 * required data structures and register defines for the AMD
 * Am9516 Universal DMA Controller Chip
 */
struct scsi_dma_blk {
	u_short reload_word;	/* MUST be hard-wired for scsi dma! */
	u_short	mem_ptr[2];	/* src or dest of data to xfer */ 
	u_short count;		/* '16-bit' word count for this xfer */
	u_short	chanmode[2];	/* channel initialization info */
	struct scsi_dma_blk *next_blk; /* pointer to next blk or NULL */
};
/* flags bit definitions
 */
#define	XFER_MASK	0x000f	/* mask for transfer modes */
#define	MAN_XFER	0x0001	/* use manual transfer mode (Req/Ack) */
#define	PTM_XFER	0x0002	/* use program transfer mode(hardware)*/
#define	DMA_XFER	0x0004	/* use DMA transfer mode (hardware) */
#define	NO_XFER		0x0008	/* No data xfer for this command */
#define	ID_MSG		0x0010	/* identification message request */

