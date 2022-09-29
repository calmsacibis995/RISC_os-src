#ident "$Header: dkvj_scsi.h,v 1.7 90/01/23 13:27:29 huang Exp $"
/* $Copyright$ */

/*
 *   scsi.h : SCSI command blocks
 *
 *   Contains all of the structs and types for SCSI
 *
 */

/****************     SCSI Control Description Block     ******************/

typedef struct  scsi_cdb {      /* SCSI command description block         */
    UBYTE   cmd;                /* command code                           */
    UBYTE   lun       : 3;      /* logical unit number                    */
    UBYTE   high_addr : 5;      /* High address MSB                       */
    UBYTE   mid_addr;           /* Logical block address                  */
    UBYTE   low_addr;           /* Logical block address                  */
    UBYTE   count;              /* block count lsb                        */
    UBYTE   vu        : 2;      /* vendor unique (byte 5 bit 7)           */
    UBYTE   res0      : 4;      /* reserved                               */
    UBYTE   flag      : 1;      /* flag request (interrupt at completion) */
    UBYTE   link      : 1;      /* link (another command follows)         */
} SCSI_CDB;

/**************** END SCSI Control Description Block     ******************/

/****************     SCSI Extended Control Description Block     *********/

typedef struct  scsi_ecdb {     /* SCSI extended command description block*/
    UBYTE   cmd;                /* command code                           */
    UBYTE   lun       : 3;      /* logical unit number                    */
    UBYTE   res0      : 5;      /* reserved                               */
    UBYTE   lba_b0;	        /* Logical block address byte 0           */
    UBYTE   lba_b1;	        /* Logical block address byte 1           */
    UBYTE   lba_b2;	        /* Logical block address byte 2           */
    UBYTE   lba_b3;	        /* Logical block address byte 3           */
    UBYTE   res1;               /* reserved				  */
    UBYTE   count_hi;           /* block count msb                        */
    UBYTE   count_lo;           /* block count lsb                        */
    UBYTE   vu        : 2;      /* vendor unique (byte 5 bit 7)           */
    UBYTE   res2      : 4;      /* reserved                               */
    UBYTE   flag      : 1;      /* flag request (interrupt at completion) */
    UBYTE   link      : 1;      /* link (another command follows)         */
} SCSI_ECDB;

/**************** END SCSI Extended Control Description Block     *********/

typedef struct scsi_space {     /* SCSI space command description block   */
    UBYTE   cmd;                /* command code                           */
    UBYTE   res0      : 6;      /* reserved				  */
    UBYTE   code      : 2;      /* Code					  */
    UBYTE   high_count;         /* Count of items to space over		  */
    UBYTE   mid_count;          /* Count of items to space over		  */
    UBYTE   low_count;          /* Count of items to space over		  */
    UBYTE   vu        : 2;      /* vendor unique (byte 5 bit 7)           */
    UBYTE   res1      : 4;      /* reserved                               */
    UBYTE   flag      : 1;      /* flag request (interrupt at completion) */
    UBYTE   link      : 1;      /* link (another command follows)         */
} SCSI_SPACE;

typedef struct  scsi_sense {    /* scsi sense for error classes 0-6       */
    UBYTE   adr_val : 1;        /* sense data is valid                    */
    UBYTE   class   : 3;        /* error class (0-6)                      */
    UBYTE   code    : 4;        /* error code                             */
    UBYTE   high_addr;          /* high byte of block addr                */
    UBYTE   mid_addr;           /* middle byte of block addr              */
    UBYTE   low_addr;           /* low byte of block addr                 */
    UBYTE   extra[12];          /* pad this struct so it can hold max num */
                                /* of sense bytes we may receive          */
} SCSI_SENSE;

typedef struct  scsi_ext_sense {/* scsi extended sense for error class 7  */
    UBYTE   valid   : 1;        /* sense data is valid        (byte 0)    */
    UBYTE           : 7;        /* alway at binary 1110000    (byte 0)    */
    UBYTE   segment;            /* segment number             (byte 1)    */
    UBYTE   filmrk  : 1;        /* file mark                  (byte 2)    */
    UBYTE   eom     : 1;        /* end of media               (byte 2)    */
    UBYTE   ilength : 1;        /* incorrect length indicator (byte 2)    */
    UBYTE           : 1;        /* reserved                   (byte 2)    */
    UBYTE   key     : 4;        /* sense key                  (byte 2)    */
    UBYTE   info1;              /* information                (byte 3)    */
    UBYTE   info2;              /* information                (byte 4)    */
    UBYTE   info3;              /* information                (byte 5)    */
    UBYTE   info4;              /* information                (byte 6)    */
    UBYTE   add_len;            /* number of additional bytes (byte 7)    */
    UBYTE   copy_cmp1;          /* Reserved for Copy/Compare Command      */
    UBYTE   copy_cmp2;          /* Reserved for Copy/Compare Command      */
    UBYTE   res0;               /* Reserved byte                          */
    UBYTE   res1;               /* Reserved byte                          */
    UBYTE   sense_code;         /* Additional Sense Code                  */
    UBYTE   res2;               /* Reserved byte                          */
    UBYTE   fru;                /* Field Replacement Unit                 */
    UBYTE   fpv    : 1;         /* Field pointer valid                    */
    UBYTE   c_d    : 1;         /* Command/Data                           */
    UBYTE          : 2;         /* Reserved                               */
    UBYTE   bpv    : 1;         /* Bit pointer vaild                      */
    UBYTE   bitp   : 3;         /* Bit pointer                            */
    UBYTE   fp1;                /* Field pointer 1                        */
    UBYTE   fp2;                /* Field pointer 2                        */
    UBYTE   extra[10];          /* additional bytes follow, if any        */
} SCSI_EXT_SENSE;

typedef struct Inquiry {
    UBYTE   device_type;        /* Peripheral Device Type                 */
    UBYTE   rm          : 1;    /* Removeable Media                       */
    UBYTE   dtq         : 7;    /* Device Type Qualifier                  */
    UBYTE               : 2;    /* Reserved                               */
    UBYTE   ecma        : 2;    /* ECMA Version                           */
    UBYTE   ansi        : 2;    /* ANSI Version                           */
    UBYTE   res0;               /* Reserved                               */
    UBYTE   length;             /* Additional Length                      */
    UBYTE   res1;               /* Reserved                               */
    UBYTE   res2;               /* Reserved                               */
    UBYTE   res3;               /* Reserved                               */
    UBYTE   vendor_id[8];       /* Vendor ID  (and null)                  */
    UBYTE   product_id[16];     /* Product ID  (and null)                 */
    UBYTE   revision_level[4];  /* Product Revision Level (and null)      */
} VJ_INQUIRY;

typedef struct read_limit_desc
{
    ULONG   res0	: 8;	/* reserved				*/
    ULONG   maxlen	: 24;	/* max block length 			*/
    UWORD   minlen;		/* min block length			*/
} VJ_RDBLKLIM;

typedef struct mode_sense_hdr
{
    UBYTE   sense_data_len;     /* sense data length */
    UBYTE   medium_type;        /* medium_type */
    UBYTE   WP          : 1;
    UBYTE   BFM         : 3;
    UBYTE   speed       : 4;
    UBYTE   blk_desc_len;       /* block descriptor length */
} VJ_MS_HDR;

typedef struct mode_sense_blk_desc
{
    ULONG   density_code	: 8;
    ULONG   nrblks		: 24;   /* no. of blocks */
    ULONG   resv		: 8;
    ULONG   blk_len		: 24;   /* block length */
} VJ_MS_BLK_DESC;

typedef struct mode_sense_page_1 {
    UBYTE   res0		: 2;
	UBYTE	page_code	: 6;
    UBYTE   page_length;
	UBYTE	awre		: 1;
	UBYTE	arre		: 1;
	UBYTE	tb  		: 1;
	UBYTE	rc  		: 1;
	UBYTE	ecc			: 1;
	UBYTE	per 		: 1;
	UBYTE	dte 		: 1;
	UBYTE	dcr			: 1;
    UBYTE   retry_count;
    UBYTE   correction_span;
    UBYTE   head_offset;
    UBYTE   strobe_offset;
    UBYTE   recovery_time;
} VJ_MS_PAGE1;

typedef struct mode_sense_page_2 {
    UBYTE   res0;
    UBYTE   page_length;
    UBYTE   buffer_full_ratio;
    UBYTE   buffer_empty_ratio;
    UBYTE   res1[8];
} VJ_MS_PAGE2;

typedef struct mode_sense_1
{
    VJ_MS_HDR       hdr;
    VJ_MS_BLK_DESC  blk_desc;
    VJ_MS_PAGE1     pg1;
} VJ_MODE_SENSE1;

typedef struct mode_sense_2
{
    VJ_MS_HDR       hdr;
    VJ_MS_BLK_DESC  blk_desc;
    VJ_MS_PAGE2     pg2;
} VJ_MODE_SENSE2;

typedef struct mode_sense
{
    VJ_MS_HDR       hdr;
    VJ_MS_BLK_DESC  blk_desc;
} VJ_MODE_SENSE;

/*
 * Sense key values for extended sense.
 */
#define SCSI_NO_SENSE             0x0
#define SCSI_RECOVERABLE_ERROR    0x1
#define SCSI_NOT_READY            0x2
#define SCSI_MEDIUM_ERROR         0x3
#define SCSI_HARDWARE_ERROR       0x4
#define SCSI_ILLEGAL_REQUEST      0x5
#define SCSI_UNIT_ATTENTION       0x6
#define SCSI_DATA_PROTECT         0x7
#define SCSI_BLANK_CHECK          0x8
#define SCSI_VENDOR_UNIQUE        0x9
#define SCSI_COPY_ABORTED         0xa
#define SCSI_ABORT_COMMAND        0xb
#define SCSI_EQUAL                0xc
#define SCSI_VOLUME_OVERFLOW      0xd
#define SCSI_MISCOMPARE           0xe
#define SCSI_RESERVED             0xf

/*
 * SCSI device types
 */
#define SCSI_TYPE_DISK		0x00
#define SCSI_TYPE_TAPE		0x01
#define SCSI_TYPE_PRINTER	0x02
#define SCSI_TYPE_CPU		0x03
#define SCSI_TYPE_WORM		0x04
#define SCSI_TYPE_RONLY_DISK	0x05
#define SCSI_TYPE_LUN_GONE	0x7f

/*
 * SCSI Operation codes.
 */
#define SCSI_UNLOAD_FLAG	0x00
#define SCSI_LOAD_FLAG		0x01
#define SCSI_RETEN		0x02

/*
 * SCSI Operation codes.
 */
#define SCSI_TEST_UNIT_READY		0x00
#define SCSI_REZERO_UNIT		0x01
#define SCSI_REWIND			0x01
#define SCSI_REQUEST_SENSE		0x03
#define SCSI_FORMAT_UNIT		0x04
#define SCSI_READ_BLOCK_LIMITS		0x05
#define SCSI_REASSIGN_BLOCK		0x07
#define SCSI_READ			0x08
#define SCSI_WRITE			0x0A
#define SCSI_SEEK			0x0B
#define SCSI_WRITE_FILE_MARKS		0x10
#define SCSI_SPACE_FM			0x11
#define SCSI_INQUIRY			0x12
#define SCSI_MODE_SELECT		0x15
#define SCSI_RESERVE_UNIT		0x16
#define SCSI_RELEASE_UNIT		0x17
#define SCSI_ERASE			0x19
#define SCSI_MODE_SENSE			0x1A
#define SCSI_RECEIVE_DIAG_RESULTS	0x1C
#define SCSI_SEND_DIAG			0x1D
#define SCSI_LOAD			0x1B
#define SCSI_READ_CAPACITY		0x25
#define SCSI_READ_EXTENDED		0x28
#define SCSI_WRITE_EXTENDED		0x2A
#define SCSI_SEEK_EXTENDED		0x2B
#define SCSI_WRITE_VERIFY		0x2E
#define SCSI_VERIFY			0x2F
#define SCSI_READ_DEFECT_DATA		0x37
#define SCSI_COMPARE			0x39
#define SCSI_COPY_VERIFY		0x3A
#define SCSI_WRITE_BUFFER		0x3B
#define SCSI_READ_BUFFER		0x3C
#define SCSI_FORMAT_TRACK		0xE4
#define SCSI_READ_LONG			0xE8
#define SCSI_WRITE_LONG			0xEA

/*
 * Messages that SCSI can send.
 */
#define SCSI_COMMAND_COMPLETE     0x00
#define SCSI_SAVE_DATA_PTR        0x02
#define SCSI_RESTORE_PTRS         0x03
#define SCSI_DISCONNECT           0x04
#define SCSI_IDENTIFY             0x80
#define SCSI_DR_IDENTIFY          0xc0

#define MORE_STATUS 0x80        /* More status flag */
#define STATUS_LEN  3           /* Max status len for SCSI */

#define CDB_ADDR(cdb, addr)     \
    { \
        (cdb)->high_addr = ((addr) >> 16) & 0xFF;\
        (cdb)->mid_addr = ((addr) >> 8) & 0xFF;\
        (cdb)->low_addr = (addr) & 0xFF; \
    }

#define SPACE_COUNT(space, count)     \
    { \
        (space)->high_count = ((count) >> 16) & 0xFF;\
        (space)->mid_count = ((count) >> 8) & 0xFF;\
        (space)->low_count = (count) & 0xFF; \
    }

#define ADDR_CDB(cdb)     \
    ( ((cdb)->high_addr << 16 ) + ((cdb)->mid_addr << 8) + (cdb)->low_addr)

#define CDB_XFER_LEN(cdb, length)     \
    { \
        (cdb)->mid_addr = ((length) >> 16) & 0xFF;\
        (cdb)->low_addr = ((length) >> 8) & 0xFF;\
        (cdb)->count = (length) & 0xFF; \
    }

#define CDB_EADDR(cdb, addr)     \
    { \
        ((SCSI_ECDB *)(cdb))->lba_b0 = ((addr) >> 24) & 0xFF;\
        ((SCSI_ECDB *)(cdb))->lba_b1 = ((addr) >> 16) & 0xFF;\
        ((SCSI_ECDB *)(cdb))->lba_b2 = ((addr) >> 8) & 0xFF;\
        ((SCSI_ECDB *)(cdb))->lba_b3 = (addr) & 0xFF; \
    }

#define CDB_EXFER_LEN(cdb, length)     \
    { \
        ((SCSI_ECDB *)(cdb))->count_hi = (UBYTE)(((length) >> 8) & 0xFF);\
        ((SCSI_ECDB *)(cdb))->count_lo = (UBYTE)((length) & 0xFF); \
    }
