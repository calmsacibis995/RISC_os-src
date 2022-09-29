#ident "$Header: r3030scsi.h,v 1.2 90/05/23 14:26:39 huang Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/*
** NCR SCSI related defines
*  even though this file is named 'r3030scsi' it contains some stuff for
   the 'rb3125' Genesis system. This is done since most driver driver files
   are for both systems.
*/
#define LOWLEVEL 	0
#define HIGHLEVEL 	1
#ifdef STANDALONE
#define TAPEUNIT	6	/* tape streamer will have this scsi id */
#endif STANDALONE

#define NTARGET		7
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
#define NO_DMA_RESET	0x20	/* this bit is set to not reset dma engine */
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
	u_int unit_flags[7]; /* per target state */
	struct scsi_iopb* iopbp[7]; /* one for each possible target */
	char active_id; /* our current active target */
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
