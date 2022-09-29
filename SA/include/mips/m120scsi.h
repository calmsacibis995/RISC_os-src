#ident "$Header: m120scsi.h,v 1.1 90/03/20 07:22:45 hal Exp $"
/* $Copyright: $ */

/*
** Intrepid SCSI related defines
*/
#define LOWLEVEL 	0
#define HIGHLEVEL 	1
#ifdef STANDALONE
#define TAPEUNIT	6	/* tape streamer will have this scsi id */
#endif STANDALONE

#define NTARGET		7
#define MAX_SGENTRY	65	/* max X 4k => 256k byte transfer + 1 */

struct fuji_manager {
	u_int channel_flags; /* scsi channel state */
	u_int unit_flags[7]; /* per target state */
	struct scsi_iopb* iopbp[7]; /* one for each possible target */
	char active_id; /* our current active target */
};

struct scsisge {
	u_short reload_word;	/* MUST be hard-wired for scsi dma! */
	u_short	mem_ptr[2];	/* src or dest of data to xfer */ 
	u_short count;		/* '16-bit' word count for this xfer */
	u_short	chanmode[2];	/* channel initialization info */
	u_short next_blk[2];	/* pointer to next blk or NULL */
};

extern int scsiexterr, scsi_Ntarget, scsi_Nlun;
extern struct scsi_unit scsi_un[];
extern struct iotime scsiiotime[];
extern struct iobuf scsi_tab[];
extern struct scsisge scsi_sge[][MAX_SGENTRY];
extern int scsi_majors[];
