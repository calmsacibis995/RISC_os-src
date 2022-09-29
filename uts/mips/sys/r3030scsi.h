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
/* $Header: r3030scsi.h,v 1.1.1.2.1.3.1.3 90/11/02 18:04:50 beacker Exp $ */
/*
** NCR SCSI related defines
*  even though this file is named 'r3030scsi' it contains some stuff for
   the 'rb3125' Genesis system. This is done for convenience.
*/
#define LOWLEVEL 	0
#define HIGHLEVEL 	1
#ifdef STANDALONE
#define TAPEUNIT	6	/* tape streamer will have this scsi id */
#endif STANDALONE

#define NTARGET		8
#define MAX_SGENTRY	17	/* max X 4k => 64k byte transfer + 1 extra
				 * due to 16 bit limitation of ncr scsi chip */

/* Genesis discrete dma register defines
 */
struct dma {			/* all registers are R/W */
	u_int	cmd_addr;	/* DMA Command Address Register */
	u_int	mem_ptr;	/* DMA Memory Address Register */
	u_int	control;	/* DMA Control Register (count invalid) */
	u_int	count_control;	/* DMA Control/Count Register */
};
/* dma reg bit defines 
 */
#define	DMA_BYTE_SHIFT    15 	/* bits to shift right or left for half-word
				 * to byte conversion and vice-versa */
#define NO_DMA_ERROR	0x40	/* this active low bit indicates a dma error */
#define NO_DMA_RESET	0x20	/* this bit is SET to not reset dma engine */
#define NO_DMA_PENDING	0x20	/* this bit is READ to see if dma pending  */
#define NO_CLR_DMA	0x10	/* this bit is set to not dewedge dma engine */
#define NO_FLUSH_PIPE	0x08	/* this bit is set to not flush dma (wr out) */
#define TO_MEMORYB	0x04	/* this bit is set for a scsi write (mem rd) */
#define NO_CHAIN_ENABLE 0x02	/* this bit is set to disable auto chaining  */
#define NO_RUN_ENABLE 	0x01	/* this bit is set to disable running */

#define G_BASE		(*(struct dma *)PHYS_TO_K1(G_DMA_BASE))
#define DMA_ADDR	(&G_BASE)
#define DMA_REG		struct dma

#define RAMBO_REG	struct rambo
#define NCR_REG		struct ncr53c94

struct ncr_manager {
	u_int channel_flags; /* scsi channel state */
	u_int unit_flags[NTARGET]; /* per target state */
	struct scsi_iopb* iopbp[NTARGET]; /* one for each possible target */
	char active_id; /* our current active target */
};

struct ncr_target {
	u_int nt_state,nt_next;	/* this is a state machine */
#define NT_BUSFREE 0
#define NT_CMD_PHASE	1
#define NT_TERMINATE_PHASE 2
#define NT_DISCONNECT_PHASE 3
#define NT_MSGOUT_PHASE 4
#define NT_MSGIN_PHASE 5
#define NT_DATA_PHASE 6
#define NT_NEW_PHASE 7
	char nt_buf[12];
	char nt_blen;
	char nt_sensekey;
};

struct scsisge {                /* Pizazz scatter/gather element struct */
	u_int	mem_ptr;	/* src or dest of data to xfer */ 
	u_short count;		/* '16-word' block count for this xfer */
	u_int	chanmode;	/* channel initialization info */
};
struct scsisge_g {              /* Genesis scatter/gather element struct */
	u_int	mem_ptr;	/* src or dest of data to xfer */
	u_int	control_count;	/* control and 'half-word' count info */ 
};
extern int scsiexterr, scsi_Ntarget, scsi_Nlun;
extern struct scsi_unit scsi_un[];
extern struct iotime isd_iotime[];
extern struct iobuf scsi_tab[];
extern struct scsisge scsi_sge[][2*MAX_SGENTRY];
extern int scsi_majors[];
