#ident "$Header: dkvj_reg.h,v 1.10 90/03/22 13:24:28 srinath Exp $"
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1988, 1990 MIPS Computer Systems, Inc.      |
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

/* ********************************************************************** */
/*      vjreg.h for NEW Jaguar 4210 V/SCSI support                        */
/* ********************************************************************** */

#ifndef HI16
/*
 * misc macros
 */
#define HI16(x)		((unsigned)(x) >> 16)
#define LO16(x)		((unsigned)(x))
#define HI8(x)		((unsigned)(x) >> 8)
#define LO8(x)		((unsigned)(x))
#endif

typedef unsigned long	ulong;

#define VJ_WSIZ (sizeof(VJ_IOPB)/sizeof(UWORD))  /* rnds 2 16 bit wrds*/

#define	NDKMAP			16
#define NLPART			NDKMAP		/* # of logical partitions */
#define LPART(dev)		(dev & 0xf)
#define VJUNIT(x)		( ((x)>>4) & 0xF)

#define VJCTLR(ctlr,x)		ctlr = 0;\
				while (ctlr < Nvjctlr) { \
				    if ((((x)>>8) & 0xff) == dkvjmajors[ctlr])\
					break; \
				    ++ctlr; \
				}

#define BPTOVJN(bp)		VJUNIT(bp->b_dev)
#define BPTOVJC(ctlr,bp)	VJCTLR(ctlr,bp->b_dev)
#define MKDEV(un)		((dkvjmajors[(un->un_ctlr)] << 8) | \
				((un->un_slave) << 4))
#define SECSIZE			512
#define SECTORSIZE		SECSIZE
#define VJ_Q_SIZ		NUM_CQE		/* number of CQE entries  */
#define VJ_DISK_PRIORITY	1
#define VJ_DISK_SLOTS		VJ_Q_SIZ
#define MAX_WORK_QUEUES		15
#define DKVJUPC			16

#define VJ_SELECTION_TIMEOUT	1000		/* 1 Second		*/
#define VJ_RESELECTION_TIMEOUT	1000		/* 1 Second		*/
#define VJ_INFINITE_TIMEOUT	0

#define VJIOSOPEN		_IO(d, 18)      /* Special open		*/
#define VJIONOPEN		_IO(d, 19)      /* Normal open		*/
#define VJIOFORMAT		_IO(d, 20)      /* Format unit		*/

#define ADRM_STD_S_P		0x3E	/*    Standard Supervisory Program  */
#define ADRM_STD_S_D		0x3D	/*    Standard Supervisory Data     */
#define ADRM_STD_N_P		0x3A	/*    Standard Normal Program       */
#define ADRM_STD_N_D		0x39	/*    Standard Normal Data          */
#define ADRM_SHT_S_IO		0x2D	/*    Short Supervisory IO          */
#define ADRM_SHT_N_IO		0x29	/*    Short Normal IO               */
#define ADRM_EXT_S_P		0x0E	/*    Extended Supervisory Program  */
#define ADRM_EXT_S_D		0x0D	/*    Extended Supervisory Data     */
#define ADRM_EXT_N_P		0x0A	/*    Extended Normal Program       */
#define ADRM_EXT_N_D		0x09	/*    Extended Normal Data          */
#define ADRM_BLK_N_D		0x0B	/*    Block Mode          	    */

/* When Running MACSI, Scatter/gather tables MUST be in external memory
 * We will declare a free pool of scatter/gather tables in each ctlr struct
 * NUM_M_SG defines the number of entries in the free pool per controller.
 * This number should be:
 *  NUM_MACSI to ensure that all the Q entries can be filled
 * However, The driver can deal with this being smaller.
 * NOTE:
 *  It is inavisable to make NUM_M_SG smaller than DKIPUPC.
 *  This will possibly prevent overlapped seeks from happening.
 *  2 * DKIPUPC is the smallest recommended number
 *
 *  The driver will add & remove from the top of the free list.
 * ALSO:
 *  The free list had to be in each ctlr struct instead of 1 common
 *  pool because if the controller start routine gets called at interrupt
 *  level, and there are no more free entries available, sleep cannot
 *  be called and there might not be any more requests comming in for
 *  that controller.
 */

typedef struct vjalign {
    ULONG	al_uaddr;		/* user address			*/
    ULONG	al_taddr;		/* temp buffer address		*/
    ULONG	al_faddr;		/* kern_free address		*/
    ULONG	al_size;		/* number of bytes		*/
} VJ_ALIGN;

#define MACSI_SG        64
#define NUM_M_SG        28

typedef struct ipsg_free {
    struct ipsg_free *nxt;      /* pointer to next IPSG_FREE    */
    VJ_ALIGN	align;
    IPSG	ipsg[MACSI_SG];
} IPSG_FREE;

typedef struct buf           BUF;
typedef struct buf           VJ_DISK_HD;
typedef struct volume_header DVH;

/* ********************************************************************** */
/*                                                                        */
/* Data per controller                                                    */
/*                                                                        */
/* ********************************************************************** */
typedef struct  ctlr {
    VJ_SHIO     *c_io;              /* ptr to I/O space                   */
    int		c_ctlr;		    /* controller number 		  */
    struct u_n_i_t *c_unit[16];	    /* pointers to unit structures	  */
    BYTE	c_maxunit;	    /* Max number of units on controller  */
    BYTE	c_firsttime;        /* First time for CTLR INIT           */
    BYTE        c_present;          /* ctlr is present                    */
    ulong       c_csh;             /* Logical Cache Section descriptor   */
    ulong       c_sph;             /* Descriptor for the c's IO map     */
    ulong       c_iodata_sph[2];   /* descriptor for c's iomap */
    BYTE        c_nintvec;          /* normal int vector                  */
    BYTE        c_eintvec;          /* error int vector                   */
    BYTE        c_qintvec;          /* Queue Entry Available              */
    BYTE        c_level;            /* interrupt level                    */
    BYTE        c_wait_cqe;         /* waiting on room in CQE's           */
    BYTE	c_pid;              /* primary bus id                     */
    BYTE	c_sid;              /* secondary bus id                   */
    VJ_IOPB    *c_mce_iopb;         /* Address of MCE IOPB                */
    VJ_CQE     *c_cqe_top;          /* Top pointer to CQE list            */
    VJ_CQE     *c_cqe_end;          /* End pointer to CQE list            */
    VJ_CQE     *c_cqe_QHDP;         /* CQE head pointer                   */
    VJ_DISK_HD  c_disk_hd;          /* Disk head for bp queueing          */
    VJ_CQE      c_mce;              /* MCE structure                      */
    VJ_IOPB     c_miopb;            /* MCE IOPB structure                 */
    VJ_CIB      c_cib;              /* CIB structure                      */
    VJ_WQCF     c_wqcf;             /* WQCF structure                     */
    IPSG_FREE  *dkvjsg_hd;          /* points to next free entry          */
    IPSG_FREE   dkvjsg_fentry[NUM_M_SG]; /* scat/gath free list           */
    OFFBD_IOPB  c_offbd[NUM_CQE_OFF]; /* offboard cqe/iopb pairs          */
    OFFBD_IOPB *c_off_top;          /* Top pointer to ECB list            */
    OFFBD_IOPB *c_off_QHDP;         /* ECB head pointer                   */
    VJ_CRB      c_off_crb;          /* offboard CRB                       */
    VJ_IOPB     c_off_iopb;         /* offboard returned IOPB             */
    ushort      c_mode;             /* for NEW style option settings      */
    ulong       c_istart;           /* cmds started from interrupt        */
    ulong       c_sstart;           /* cmds started from strategy         */
    struct      iobuf *c_tab;       /* pointer to controller table        */
    int         c_max_to_que;       /* max on-board iopbs/unia for sort   */
} VJ_CTLR;

/* c_mode defines */
#define C_OFF_BOARD     0x0001      /* enable off-board iopb's            */
#define C_MACSI_SORT    0x0002      /* MACSI firmware provides sorting    */


/* ********************************************************************** */
/*                                                                        */
/*   Definition of a unit or disk drive.                                  */
/*                                                                        */
/* ********************************************************************** */
typedef struct  u_n_i_t {
    ULONG               un_command;     /* saved current command        */
    UBYTE		un_ctlr;	/* controller number		*/
    UBYTE		un_slave;	/* for two unit support		*/
    UBYTE		un_retries;	/* Max retries on unit		*/
    UBYTE		un_eomcount;	/* Count of commands after EOM	*/
    UBYTE               un_weomcount;   /* count of write ops past eom */
    ULONG		un_flags;	/* Special  Flags		*/
    ULONG		un_qcount;	/* number of q entries used	*/
    ULONG		un_timeid;	/* timeout id			*/
    ULONG		un_timeout;	/* timeout count		*/
    ULONG		un_scsibusy;	/* busy retry count		*/
    UBYTE		un_aborta;	/* Abort Acknowledge flag	*/
    UBYTE		un_workq;	/* Unit work Queue		*/
    UBYTE		un_burst;	/* Unit Burst Count		*/
    UBYTE		un_vhvalid;	/* Volume header flag		*/
    sah_type            un_sph;         /* syst pg handle for Gba map   */
    BUF			*un_savebp;	/* Saved bp for request sense	*/
    SCSI_EXT_SENSE	un_sense;	/* Request sense data		*/
    VJ_MODE_SENSE	un_msense;	/* Mode sense data		*/
    VJ_RDBLKLIM		un_blklim;	/* Block limits info		*/
    VJ_UADR		un_unit;	/* SCSI Unit Address		*/
    VJ_CQE		un_cqe;		/* CQE structure		*/
    VJ_IOPB		un_iopb;	/* IOPB structure		*/
    VJ_CRB		un_crb;		/* CRB structure		*/
    DVH			un_vh;		/* Volume header		*/
    BUF			un_sbuf;	/* Special BUF.			*/
    VJ_IOPB		un_siopb;	/* Special IOPB.		*/
    VJ_INQUIRY		un_inquiry;	/* Inquiry command		*/
    int			un_xfer;	/* transfer size for vjcmd	*/
    int			un_resid;	/* resid from vjsense_request	*/
    struct iotime      *un_iotime;      /* pointer to io statistics     */
    struct iobuf       *un_tab;         /* pointer to unit table        */
    unsigned int        un_lastcyl;     /* current location for sort index */
    VJ_CTLR            *un_c;           /* back pointer to controller info */
    int                 un_queue_size;  /* max work queue entries allowed  */
#ifdef STANDALONE
    int			un_curfile;
    int			un_nxtrec;
    UBYTE		un_lastiow;
#endif STANDALONE
} VJ_UNIT;


/*  SCSI IOPB definitions  */
#define SCSI_PASS_THRU      0x20    /* SCSI Pass Through commands         */
#define SCSI_PASS_THRU_EXT  0x21    /* SCSI Pass Through Extended commands*/
#define SCSI_RESET          0x22    /* SCSI Reset bus                     */

/*  SCSI Control IOPB's                                                   */
#define CNTR_DIAG           0x40    /* Perform Diagnostics                */
#define CNTR_INIT           0x41    /* Initialize Controller              */
#define CNTR_INIT_WORKQ     0x42    /* Initialize Work Queue              */
#define CNTR_DUMP_INIT      0x43    /* Dump Initialization Parameters     */
#define CNTR_DUMP_WORDQ     0x44    /* Dump work Queue Parameters         */
#define CNTR_FLUSH_WORKQ    0x49    /* Flush Work Queue                   */

/* ERROR return codes */
#define BUS_RESET_STATUS	0x11
#define NO_SECOND_PORT		0x12
#define SELECT_TIMEOUT		0x30
#define TRANSFER_COUNT_ERROR	0x34
#define WORK_QUEUE_ABORT    	0x80

/*  Memory types                                                         */
#define MEMT_16BIT              1   /* 16 Bit Memory type                */
#define MEMT_32BIT              2   /* 32 Bit Memory type                */
#define MEMT_SHIO               3   /* Short I/O Memory type             */

/*  Transfer types                                                       */
#define TT_NORMAL               0   /* Normal Mode Tranfers              */
#define TT_BLOCK                1   /* Block  Mode Tranfers              */
#define TT_DISABLE_INC_ADDR     2   /* Disable Incrementing Addresses    */

/*  VME Direction                                                        */
#define WRITE_TO_VME            0   /* Data is Written to VMEbus         */
#define READ_FROM_VME           1   /* Data is Read from VMEbus          */

/*
 * Scsi Check Condition defines
 */
#define CHECK_CONDITION	0x2

#define SCSI_BUSY	0x08

/*
 * Special flags for unit structure.  This was extracted from the 
 * Intrepid header, and not all items have been implemented.
 *
 */
#define IVJ_ALIVE	0x000001	/* Device is Alive and Available */
#define IVJ_READY	0x000002	/* Device is Ready */
#define IVJ_BUSY	0x000004	/* Device is busy */
#define IVJ_WRITTEN	0x000008	/* Written to */
#define IVJ_TAPE	0x000010	/* A Tape Device */
#define IVJ_DISK	0x000020	/* A Disk Device */
#define IVJ_WORM	0x000040
#define IVJ_REWINDING	0x000080	/* rewinding */
#define IVJ_SELECTED	0x000100	/* Drive Selected */
#define IVJ_FORMATTED	0x000200	/* Drive Formatted - Do I need this */
#define IVJ_WAITING	0x000400	/* Waiting for Interrupt */
#define IVJ_WANTED	0x000800	/* Wanted after Interrupt */
#define IVJ_OPEN	0x001000	/* Driver Open Routine */
#define IVJ_READ	0x002000	/* device has been read */
#define IVJ_EOM		0x004000	/* EOM or BOT reached */
#define IVJ_FM		0x008000	/* indicates a file mark was read */
#define IVJ_PRINTER	0x010000	/* Printer Device */
#define IVJ_RMV_MEDIA	0x020000	/* Floppy Device or ? */
#define IVJ_ARB_DISABLE	0x040000	/* Disable arbitration for this device*/
#define IVJ_READONLY	0x080000	/* read-only device */
#define IVJ_ATN		0x100000	/* tape unit attention occurred */
#define IVJ_NOT_RDY	0x200000	/* tape not ready */
#define IVJ_VARIABLE	0x400000	/* tape has variable block size */
#define IVJ_RFM		0x800000	/* file mark needs reporting */

/* Misc defines and typedefs removed from main program */

#define CTLR(dev) ((dev >> 7 ) & 1)
#define MAGIC_DEV	10
#define JAGUAR_OVERLAP_SG_MAX    32768
#define B_SPL	B_FORMAT
#define MAXEOM        5

#define TRUE		1
#define FALSE		0
#define WAIT		1
#define NO_WAIT		0
#define SENSE_LENGTH	0x1a

#define VEC(c,vec)  (((c)->c_level << 8) + (vec))

#define CQE_GO( qecr )          W( qecr ) |= M_QECR_GO; wbflush()
#define CQE_AA_GO( qecr )       W( qecr ) |= (M_QECR_GO + M_QECR_AA);wbflush()
#define CRB_CLR_DONE( crsw )    W( crsw ) = 0; wbflush()

#define VJ_BURST_COUNT  0

#define MEMTYPE     MEMT_32BIT          /* do 32-bit transfers */

#define ADDR_MOD    ( (TT_NORMAL << 10) | (MEMTYPE << 8) | VME_A32NPAMOD )
#define ADDR_MOD_B  ( (TT_BLOCK << 10) | (MEMTYPE << 8) | VME_A32NPBMOD )
#define SHIO_MOD    ( (TT_NORMAL << 10) | (MEMT_SHIO << 8) | ADRM_SHT_N_IO )
#define DEFAULT_SCSI_ID     M_PSID_DFT
#define NO_INTERRUPT        0
#define WANT_INTERRUPT       1
#define SPECIAL_DEV_MASK    0x80
#define SPECIAL( dev )  ( (dev) & SPECIAL_DEV_MASK )

#define VJ_UNIT_READY		1
#define VJ_RESTORE		2
#define VJ_SENSE		3
#define VJ_READ			4
#define VJ_WRITE		5
#define VJ_SEEK			6
#define VJ_FORMAT		7
#define VJ_REWIND		8
#define VJ_W_FM			9
#define VJ_SPACE		10
#define VJ_INQUIRY_CMD		11
#define VJ_MODE_SENSE_CMD	12
#define VJ_MODE_SELECT_CMD	13
#define VJ_RDBLKLIM_CMD		14
#define VJ_REASSIGN		15
#define VJ_LOAD			16
#define VJ_SCSI_RESET		17
#define VJ_CNTR_INIT		18
#define VJ_INIT_WORKQ		19
#define VJ_FLUSH_WORKQ		20

typedef struct scsi_errors {
    UWORD   code;
    char    *msg;
} SCSI_ERRORS;

typedef struct vj_errors {
    UWORD   code;
    char    *msg;
} VJ_ERRORS;

#define MAX_RETRIES         	1
#define TIME_RDWR		(HZ * 60 * 2)
#define TIME_REWIND		(HZ * 60 * 5)
#define TIME_FSF		(HZ * 60 * 17)

#define	NOREWIND(dev)		((dev) & 0x01)	
	
/* Types of items to space over */
#define SP_BLOCK		0
#define SP_FILEMARK		1
#define SP_SEQFILEMARK		2
#define SP_ENDOFDATA		3

#ifndef STANDALONE
#define printf dri_printf
#endif STANDALONE
#define vjdelay( n )	DELAY( n )

typedef struct sense_key {
	UBYTE	key;
	BYTE	*key_msg;
} SENSE_KEY_DEFS;

#ifdef STANDALONE
#define TP_SHIFT	5
#define TP_MASK		0x1f
#define TP_BLKS_PER_REC	128
#define TP_BLKSIZ	(TP_BLKS_PER_REC * 512)
#endif STANDALONE

/*
 * lboot defines the bounds of these data structures
 */
extern  struct iotime dkvjiotime[][DKVJUPC];    /* io statistics */
extern  struct iobuf dkvjctab[];                /* controller queues */
extern  struct iobuf dkvjutab[][DKVJUPC];       /* drive queues */
extern  VJ_CTLR dkvjctlr[];                     /* controller structs */
extern  VJ_UNIT dkvjunit[][DKVJUPC];            /* unit structs */

