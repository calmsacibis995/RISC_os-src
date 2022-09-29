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
/* $Header: dkvj_scsi.h,v 1.4.3.3.1.3 90/07/12 10:44:48 hawkes Exp $ */
/*
 *   scsi.h : SCSI command blocks
 *
 *   Contains all of the structs and types for SCSI
 *
 */

/****************     SCSI Control Description Block     ******************/

typedef struct  scsi_cdbi {     /* SCSI command description block         */
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
#define SCSI_TYPE_SCANNER	0x06
#define SCSI_TYPE_OPTICAL	0x07
#define SCSI_TYPE_CHANGER	0x08
#define SCSI_TYPE_COMM		0x09
#define SCSI_TYPE_LUN_GONE	0x7f
#define SCSI_TYPE_M12_FLOPPY	0x81

/*
 * SCSI Operation codes.
 */
#define SCSI_UNLOAD_FLAG	0x00
#define SCSI_LOAD_FLAG		0x01
#define SCSI_RETEN		0x02

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
