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
/* $Header: adaptec_scsi.h,v 1.2.3.2 90/05/10 06:04:27 wje Exp $ */
/*
 * $Header: adaptec_scsi.h,v 1.2.3.2 90/05/10 06:04:27 wje Exp $
 */
/* GENERIC SCSI DEFINITIONS */

#define NSCLUN		8		/* number of luns per target */

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

/* GROUP 0 commands */
#define		  CMD_TESTREADY	0x00	/*   test unit ready */
#define		  CMD_REZERO	0x01	/*   rezero unit */
#define		  CMD_SENSE	0x03	/*   request sense */
#define		  CMD_FORMAT	0x04	/*   format unit */
#define           CMD_BLKLIM    0x05    /*   read block limits */
#define		  CMD_REASSIGN	0x07	/*   re-assign blocks */
#define		  CMD_READ 	0x08	/*   read data */
#define		  CMD_WRITE	0x0A	/*   write data */
#define		  CMD_SEEK 	0x0B	/*   seek */
#define           CMD_FILEMARK  0x10    /*   write file mark */
#define           CMD_SPACE     0x11    /*   space */
#define		  CMD_INQUIRY	0x12	/*   inquiry */
#define		  CMD_MSELECT	0x15	/*   mode select */
#define		  CMD_RESERVE	0x16	/*   reserve */
#define		  CMD_RELEASE	0x17	/*   release */
#define           CMD_ERASE     0x19    /*   erase tape */
#define		  CMD_S_DIAG	0x1C	/*   send diagnostic */
#define		  CMD_R_DIAG	0x1D	/*   receive diagnostic rslt */
#define		  CMD_MSENSE	0x1A	/*   mode sense */
#define		  CMD_STARTSTOP	0x1B	/*   start/stop unit */
#define           CMD_PREVENT   0x1E    /*   prevent medium release */
#define           CMD_LOADCART  0xCF    /*   load/unload cartridge */

#define	SET_CDB_0(cdb, cmd, lun, lba, len, fr, link)		{	\
		bzero(cdb, sizeof (struct cdb_0));			\
		(cdb)->cdb_0.cdb_0_cmd = cmd;				\
		(cdb)->cdb_0.cdb_0_lun = lun;				\
		SCSI_HML_SET((cdb)->cdb_0.cdb_0_lba, lba);		\
		(cdb)->cdb_0.cdb_0_len = len;				\
		(cdb)->cdb_0.cdb_0_fr = fr;				\
		(cdb)->cdb_0.cdb_0_link = link;				\
		wbflush();						}

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

/* GROUP 1 commands */
#define		  CMD_RCAPAC	0x25	/*   read capacity */
#define		  CMD_XREAD	0x28	/*   extended read data */
#define		  CMD_XWRITE	0x2A	/*   extended write data */
#define		  CMD_XSEEK	0x2B	/*   extended seek */
#define           CMD_VERIFY    0x2F    /*   verify data */
#define           CMD_RDDEFECT  0x37    /*   read defect list */

#define	SET_CDB_1(cdb, cmd, lun, reladr, lba, len, fr, link)	{	\
		bzero(cdb, sizeof (struct cdb_1));			\
		(cdb)->cdb_1.cdb_1_cmd = cmd;				\
		(cdb)->cdb_1.cdb_1_lun = lun;				\
		(cdb)->cdb_1.cdb_1_reladr = reladr;			\
		SCSI_HMML_SET((cdb)->cdb_1.cdb_1_lba, lba);		\
		SCSI_HL_SET((cdb)->cdb_1.cdb_1_len, len);		\
		(cdb)->cdb_1.cdb_1_fr = fr;				\
		(cdb)->cdb_1.cdb_1_link = link;				\
		wbflush();						}

struct    cdb_7 {		    /* COMMAND GROUP 7 */
	u_char	cdb_7_cmd;		/* command code */
	u_char	cdb_7_rsvd	: 6;	/* reserved */
	u_char  cdb_7_fr	: 1;	/* flag request */
	u_char  cdb_7_link	: 1;	/* link */
};

/* GROUP 7 commands */
#define		CMD_RESET	0xF0	/*   ISI reset device or ctlr */
#define		CMD_INIT 	0xF1	/*   ISI reinit controller */
#define		CMD_STATUS	0xF2	/*   ISI return status */
#define		CMD_VERSION	0xF3	/*   ISI return firmware vers */
#define		CMD_DUMPREG	0xF4	/*   ISI dump reg states */


union	scsi_cdb {		/* SCSI command block */
	struct cdb_0 cdb_0;
	struct cdb_1 cdb_1;
	struct cdb_7 cdb_7;
    	u_char	cdb_raw[16];		/* pad to 16 bytes for screg.h */
};

/* defines for scsi completion status byte */
#define	SCSI_STATUS_PARITY	0x01
#define	SCSI_STATUS_CHECK	0x02
#define	SCSI_STATUS_CM		0x04
#define	SCSI_STATUS_BUSY	0x08
#define	SCSI_STATUS_INTER	0x10
#define	SCSI_STATUS_BITS	"\20\5INTER\4BUSY\3CM\2CHECK\1PARITY"

struct sns_06 {		/* non-extended sense data format */
	u_char sns_06_lbav	: 1;	/* logical block is valid */
	u_char sns_06_class	: 3;	/* error class (0-6) */
	u_char sns_06_code	: 4;	/* error code */
	u_char sns_06_lba_h;		/* high logical block address */
	u_char sns_06_lba_m;		/* middle logical block address */
	u_char sns_06_lba_l;		/* low logical block address */
};

struct sns_7 {		/* extended sense data format */
	u_char sns_7_valid	: 1;	/* sense data is valid */
	u_char sns_7_class	: 3;	/* error class (7) */
	u_char	 		: 4;	/* not used */
	u_char sns_7_seg_num;		/* segment number */
	u_char sns_7_fil_mk	: 1;	/* file mark on device */
	u_char sns_7_eom	: 1;	/* end of media */
	u_char sns_7_ili	: 1;	/* incorrect length indicator */
	u_char		 	: 1;	/* reserved */
	u_char sns_7_key	: 4;	/* sense key */
	u_char sns_7_info_h;		/* information */
	u_char sns_7_info_mh;		/* information */
	u_char sns_7_info_ml;		/* information */
	u_char sns_7_info_l;		/* information */
	u_char sns_7_add_len;		/* number of additional bytes */
	u_char sns_7_xxx[4];		/* reserverd for copy search cmd */
	u_char sns_7_err;		/* extended sense error codes */
	u_char sns_7_pad;		/* make it an even # of bytes */
};

/* defines for sns_7_key */
#define		SNS_7_KEY_NO_SENSE	0x0
#define		SNS_7_KEY_RECOVERED	0x1
#define		SNS_7_KEY_NOT_READY	0x2
#define		SNS_7_KEY_MEDIUM_ERROR	0x3
#define		SNS_7_KEY_HW_ERROR	0x4
#define		SNS_7_KEY_ILL_REQUEST	0x5
#define		SNS_7_KEY_UNIT_ATTN	0x6
#define		SNS_7_KEY_DATA_PROTECT	0x7
#define		SNS_7_KEY_BLANK_CHECK	0x8
#define		SNS_7_KEY_VU		0x9
#define		SNS_7_KEY_COPY_ABORTED	0xa
#define		SNS_7_KEY_ABORT_CMD	0xb
#define		SNS_7_KEY_EQUAL		0xc
#define		SNS_7_KEY_VOL_OVERFLOW	0xd
#define		SNS_7_KEY_MISCOMPARE	0xe
#define		SNS_7_KEY_RESERVED	0xf
#define		SNS_7_KEY_MAX (SNS_7_KEY_RESERVED + 1)

#define		SNS_7_ERR_

union	scsi_sns {		/* data returned by CMD_SENSE */
	struct sns_06 sns_06;
	struct sns_7  sns_7;
};

struct	scsi_inq {		/* data returned by CMD_INQUIRY */
	u_char	inq_pdtype;		/* Peripheral Device Type */
	u_char	inq_removable	: 1;	/* removable */
	u_char	inq_dtypeq	: 7;	/* device type qualifier */
	u_char	inq_isov	: 2;	/* ISO version */
	u_char	inq_emcav	: 3;	/* EMCA version */
	u_char	inq_ansiiv	: 3;	/* ANSII version */
	u_char	inq_rdf;		/* response data format */
	u_char	inq_addlen;		/* additional length */
	u_char	inq_vu[3];		/* pad vender unique data */
	u_char	inq_vendor[8];		/* vendor name ASCII */
	u_char	inq_product[16];	/* vendor product name ASCII */
	u_char	inq_rev[4];		/* vendor product revision ASCII */
};

/* defines for inq_pdtype */
#define		  INQ_PDT_DISK	0x00	/*   Direct-access device */
#define		  INQ_PDT_TAPE	0x01	/*   Sequential-access device */
#define		  INQ_PDT_PRINT	0x02	/*   PRINTer device */
#define		  INQ_PDT_PROC	0x03	/*   PROCessor device */
#define		  INQ_PDT_WORM	0x04	/*   Write-Once Read-Multiple device */
#define		  INQ_PDT_RO	0x04	/*   Read Only device */
#define		  INQ_PDT_RAT	0x10	/*   Random Access Tape device */
#define		  INQ_PDT_FLOP	0x81	/*   Jupiter Floppy device */
#define		  INQ_PDT_NOLUN	0x7f	/*   Logical unit not present */


struct	blk_desc {		/* block descriptor: CMD_MSENSE, CMD_MSELECT */
	u_char	bd_density;	/*   block descriptor density */
	u_char	bd_nb_h;	/*   high number of blocks */
	u_char	bd_nb_m;	/*   mid number of blocks */
	u_char	bd_nb_l;	/*   low number of blocks */
	u_char		: 8;	/*   reserved */
	u_char	bd_bl_h;	/*   high block length */
	u_char	bd_bl_m;	/*   mid block length */
	u_char	bd_bl_l;	/*   low block length */
};

struct	pag_desc {		/* page descriptor: CMD_MSENSE, CMD_MSELECT */
    u_char	pd_save		: 1;	/*   parameter savable */
    u_char			: 1;	/*   reserved */
    u_char	pd_code		: 6;	/*   page code */
    u_char	pd_len;			/*   page length */
    union	pd_pg {
	struct pg_err {			/*   ERROR PAGE */
	    u_char	err_awre	: 1;	/* auto write reallocation */
	    u_char	err_arre	: 1;	/* auto read reallocation */
	    u_char	err_tb		: 1;	/* transfer block on error */
	    u_char	err_rc		: 1;	/* read continuous */
	    u_char	err_ecc		: 1;	/* enable early correction */
	    u_char	err_per		: 1;	/* post errors (report soft)*/  
	    u_char	err_dte		: 1;	/* disable transfer on error */
	    u_char	err_dcr		: 1;	/* disable correction */
	    u_char	err_retries;		/* retry count */
	    u_char	err_cspan;		/* correction span */
	    u_char	err_hoffset;		/* head offset */
	    u_char	err_dsoffset;		/* data strobe offset */
	    u_char	err_tlimit;		/* recovery time limit */
	}	pg_err;
	struct pg_dis {			/*   DISCONNECT RECONNECT PAGE */
	    u_char	dis_bfulr;		/* buffer full ratio */
	    u_char	dis_beulr;		/* buffer empty ratio */
	    u_short	dis_bil;		/* bus inactivity limit */
	    u_short	dis_distime;		/* disconnect time limit */
	    u_short	dis_retime;		/* reconnect time limit */
	    u_short	dis_xxx;		/* reserved */
	}	pg_dis;
	struct pg_fmt {			/*   FORMAT PAGE */
	    u_short	fmt_tpz;		/* tracks/zone */
	    u_short	fmt_alt_spz;		/* alternate sectors/zone */
	    u_short	fmt_alt_tpz;		/* alternate tracks/zone */
	    u_short	fmt_alt_tpv;		/* alternate tracks/volume */
	    u_short	fmt_spt;		/* sectors/track */
	    u_short	fmt_bps;		/* bytes/sector */
	    u_short	fmt_interleave;		/* interleave factor */
	    u_short	fmt_t_skew;		/* track skew factor */
	    u_short	fmt_c_skew;		/* cylinder skew factor */
	    u_char	fmt_ssec	: 1;	/* soft sector formatting */
	    u_char	fmt_hsec	: 1;	/* hard sector formatting */
	    u_char	fmt_rmb		: 1;	/* removable */
	    u_char	fmt_surf	: 1;	/* surface lba allocation */
	    u_char	fmt_ins		: 1;	/* inhibit save */
	    u_char			: 3;
	}	pg_fmt;
	struct	pd_geo {		/*   GEOMETRY PAGE */
	    u_char	geo_cyl_h;		/* number of cylinders */
	    u_char	geo_cyl_m;
	    u_char	geo_cyl_l;
	    u_char	geo_heads;		/* number of heads */
	    u_char	geo_cyl_wp_h;		/* start write precomp cyl */
	    u_char	geo_cyl_wp_m;
	    u_char	geo_cyl_wp_l;
	    u_char	geo_cyl_rw_h;		/* start reduced write cyl */
	    u_char	geo_cyl_rw_m;
	    u_char	geo_cyl_rw_l;
	    u_short	geo_step_rate;		/* step rate */
	    u_char	geo_cyl_ld_h;		/* landing zone cylinder */
	    u_char	geo_cyl_ld_m;
	    u_char	geo_cyl_ld_l;
	    u_char	geo_rsvd[3];
	}	pg_geo;
	struct pg_flop {			/* FLOPPY VU PARAMETERS */
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
	}	pg_flop;
    }	pd_pg;
};

/* defines for pd_code */
#define		  PD_ERROR	0x01	/*	Error Recovery Parameters */
#define		  PD_DISRE	0x02	/*	Disconnect Reconnect Params */
#define		  PD_FORMAT	0x03	/*	Format Parameters Parameters */
#define		  PD_GEOMETRY	0x04	/*	Drive Geometry Parameters */
#define		  PD_FLOP	0x21	/*	Floppy Specific Parameters */
#define		  PD_ALL	0x3F	/*	All Pages */


struct	scsi_msen {		/* data returned by CMD_MSENSE */
	u_char	msen_len;		/* sense data length */
	u_char	msen_mtype;		/* medium type */
	u_char	msen_wprot	: 1;	/* write protected if 1 */
	u_char	msen_rsvd	: 7;	/* reserved */
	u_char	msen_bdl;		/* block descriptor length */
	struct	blk_desc msen_bd;	/* block descriptor */
	struct	pag_desc msen_pd;	/* page descriptor */
};

struct	scsi_msel {		/* data sent by CMD_MSELECT */
	u_char			: 8;	/* reserved */
	u_char	msel_mtype;		/* medium type */
	u_char			: 8;	/* reserved */
	u_char	msel_bdl;		/* block descriptor length */
	struct	blk_desc msel_bd;	/* block descriptor */
	struct	pag_desc msel_pd;	/* page descriptor */
};

struct	flop_msel {		/* data sent by CMD_MSELECT */
	u_long	msel_hdr;		/* medium type, blk_desc length all 0 */
	struct	{
    		u_char	desc_code;	/*   page code */
    		u_char	desc_len;		/*   page length */
		struct pg_flop desc_pg;	/* page descriptor */
	} msel_desc;
};

struct	scsi_rcap {		/* data returned by CMD_RCAPAC */
	u_long	rcap_lba;		/* logical block address */
	u_long	rcap_bl;		/* block length */
};

struct  scsi_blklim {           /* data returned by CMD_BLKLIM */
        u_long  blklim_max;
        u_short blklim_min;
};

/* pick up (set) 2 byte "_h _l" field in (from) an int */
#define	SCSI_HL(x)		((x/**/_h<<8)|(x/**/_l))
#define	SCSI_HL_SET(x, v)	{ x/**/_h=v>>8; x/**/_l=v;}

/* pick up (set) 3 byte "_h _m _l" field in (from) an int */
#define	SCSI_HML(x)		((x/**/_h<<16)|(x/**/_m<<8)|(x/**/_l))
#define	SCSI_HML_SET(x, v)	{ x/**/_h=v>>16; x/**/_m=v>>8; x/**/_l=v;}

/* pick up (set) 4 byte "_h _mh _ml _l" field in (from) an int */
#define	SCSI_HMML(x)		\
		    ((x/**/_h<<24)|(x/**/_mh<<16)|(x/**/_ml<<8)|(x/**/_l))
#define	SCSI_HMML_SET(x, v)	\
		    { x/**/_h=v>>24; x/**/_mh=v>>16; x/**/_ml=v>>8; x/**/_l=v;}
/* __EOF__ */

