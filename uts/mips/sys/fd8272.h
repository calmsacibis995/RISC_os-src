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
/* $Header: fd8272.h,v 1.2.1.2.1.2.1.3 90/12/20 19:31:20 beacker Exp $ */
/* hardware address */

#define FD_STATUS_REG 0xbe000003
#define FD_DATA_REG 0xbe000007
#define FD_TERMINAL_COUNT 0xbe800003

#define FD_DOR	0xbe00001b

#define FD_SECTSIZE	512

struct fd8272 {
	unsigned char *fd_status_reg; 
	unsigned char *fd_data_reg;
	unsigned char *fd_terminal_count;
	unsigned char *fd_dor;

	int fd_state;
#define FDS_READY	0
#define FDS_CMD_PHASE	1
#define FDS_DATA_PHASE  2
#define FDS_RES_PHASE 3
#define FDS_WAIT_PHASE	4
#define FDS_WAIT_MOTOR	5

	unsigned fd_sflags;
#define FDS_TIMEOUT	0x1
#define FDS_MOTOR_ON	0x2
#define FDS_MOTOR	0x4
	
#define FDS_TOTAL 5
	unsigned char *fd_command;
	int fd_clen;
	unsigned char *fd_data;
	int fd_dlen;
	unsigned char *fd_result;
	int fd_rlen;
	int (*fd_reply)();
	struct sfd8272  *fd_arg;

	int cur_cylinder;
	int err;
	int fd_timeout;
	int fd_motor_timeout;
	unsigned fd_flags;
	int fd_82077;
	unsigned fd_pad;
};


struct fd_cmd {
	unsigned short fc_cmd;
	unsigned char fc_clen;
	unsigned char fc_rlen;
	unsigned fc_flags;
#define FC_IO_TO_FD 0x1
#define FC_IO_FROM_FD 0x2
#define FC_DATA_PHASE 0x3
#define FC_RES_PHASE 0x4
#define FC_WAIT_PHASE 0x8
#define FC_EXEC_PHASE 0x10
#define FC_REG0	0x20
#define FC_REG1 0x40
#define FC_REG2 0x80
#define FC_REG3 0x100
#define FC_REGS 0x1e0
#define FC_REG_NOOP 0x200
#define FC_TERMINATE	0x400
#define FC_SAVE	0x800
#define FC_FORMAT 0x1000
#define FC_WAIT_RES_PHASE 0x2000
#define FC_TURN_MOTOR_ON 0x4000
#define FC_PAD	0x8000
};

/* commands for 8272 (only those that are supported) */
#define FD_CMD_MASK 0x1f
#define FD_READ_DATA 0x6
#define FD_READ_DEL_DATA 0xc
#define FD_WRITE_DATA 0x5
#define FD_WRITE_DEL_DATA 0x9
#define FD_FORMAT_TRACK 0xd
#define FD_RECALIBRATE 0x7
#define FD_SENSE_INTR 0x8
#define FD_SPECIFY 0x3
#define FD_SENSE_DRIVE 0x4
#define FD_SEEK 0xf
#define FD_NOOP 0x1f
#define FD_READ_ID 0xa
#define FD_READ_TRACK 0x2
#define FD_SCAN_EQ 0x11
#define FD_SCAN_LOW 0x19
#define FD_SCAN_HIGH 0x1d
#define FD_READ_TRACK 0x2
#define FD_CONFIGURE 0x13
#define FD_MOTOR 0xb
#define FD_REL_SEEK 0x8f
#define FD_DUMP_REG 0xf

#define FD_MT	0x80
#define FD_MFM  0x40
#define FD_SK   0x20

#define FD_MOTOR_ON 0x80
#define FD_HEAD_OUT 0x8
#define FD_HEAD_IN 0xc

#define FD_ENABLE_HSDA 0x80
#define FD_ENABLE_IS 0x40
#define FD_DISABLE_FIFO 0x20
#define FD_DISABLE_POLL 0x10
#define FD_FIFO_DEPTH 0xf

#define FL_READ_WRITE	9
#define FL_READ_ID 2
#define FL_FORMAT_TRACK 6
#define FL_SCAN 9
#define FL_RECALIBRATE 2
#define FL_SENSE_INTR 1
#define FL_SPECIFY 3
#define FL_SENSE_DRIVE 2
#define FL_SEEK 3
#define FL_NOOP 1
#define FL_CONFIGURE 4
#define FL_MOTOR 1
#define FL_REL_SEEK 3
#define FL_DUMP_REG 1


#define FR_READ_WRITE 7
#define FR_READ_ID 7
#define FR_FORMAT_TRACK 7
#define FR_SCAN 7
#define FR_RECALIBRATE 2
#define FR_SPECIFY 0
#define FR_SENSE_DRIVE 1
#define FR_SENSE_INTR 2
#define FR_SEEK 2
#define FR_NOOP 1
#define FR_CONFIGURE 0
#define FR_MOTOR 0
#define FR_REL_SEEK 0
#define FR_DUMP_REG 10


/* main status register */
#define FD_UNIT0_BUSY 0x1
#define FD_UNIT1_BUSY 0x2
#define FD_UNIT2_BUSY 0x4
#define FD_UNIT3_BUSY 0x8
#define FD_BUSY 0x10
#define FD_NON_DMA 0x20	/* set during exec phase in non-dma mode */
#define FD_TO_CPU 0x40
#define FD_READY 0x80

/* data rate select register */

#define FD_DR_RESET 0x80
#define FD_DR_POWER_DOWN 0x40
#define FD_DR_DISABLE_PLL 0x20
#define FD_DR_PRECOMP 0x0
#define FD_DR_DATA_RATE 0x0

/* status registers */
#define FD_R0_UNIT_MASK 0x3
#define FD_R0_HEAD_ADDR 0x4
#define FD_R0_NOT_READY 0x8
#define FD_R0_CHECK 0x10
#define FD_R0_SEEK_END 0x20
#define FD_R0_INTR_MASK 0xc0
#define FD_R0_NORMAL 0x0
#define FD_R0_ABNORMAL 0x40
#define FD_R0_INVALID 0x80
#define FD_R0_STATE_CHANGED 0xc0

#define FD_R1_MISS_ADDR 0x1
#define FD_R1_NOT_WRITABLE 0x2
#define FD_R1_NO_DATA 0x4
#define FD_R1_OVERRUN 0x10
#define FD_R1_CRC 0x20
#define FD_R1_END_CYL 0x80

#define FD_R2_MISS_ADDR 0x1
#define FD_R2_BAD_CYL 0x2
#define FD_R2_SCAN_FAIL 0x4
#define FD_R2_SCAN_EQUAL 0x8
#define FD_R2_WRONG_CYL 0x10
#define FD_R2_CRC 0x20
#define FD_R2_DEL_MARK 0x40

#define FD_R3_UNIT_MASK 0x3
#define FD_R3_HEAD_ADDR 0x40
#define FD_R3_TWO_SIDE 0x80
#define FD_R3_TRACK0 0x10
#define FD_R3_READY 0x20
#define FD_R3_WRITE_PROT 0x40
#define FD_R3_FAULT 0x80

struct sfd8272 {
	int sf_state;
#define SF_READY 0
#define SF_EXEC_CMD 1
#define SF_IO 2
#define SF_GET_VHB	3
#define SF_FORMAT_TRACK 4
#define SF_RECAL	5
#define SF_DO_SEEK	6
#define SF_SEEKING	7
#define SF_READ_ID	8
#define SF_MODE_SELECT	9
#define SF_RW		10
#define SF_SETUP_DONE 11
#define SF_CONFIGURE_DONE 12
#define SF_MODE_SELECT_DONE	13
#define SF_SENSE_DRIVE	14
#define SF_SENSE_DONE	15
#define SF_CLOSE	16
#define SF_NOOP	17
#define SF_LIGHT 18

	unsigned sf_flags;
#define SF_TIMEOUT	0x1
#define SF_MOTOR_ON	0x2
#define SF_MOTOR	0x4
	int sf_timeout_id;

	struct buf *sf_bp;	/* active bp */

	struct iobuf sf_iobuf;	/* queue head for all requests */
	struct fd8272 *sf_fd;	/* points to hardware structure*/
	int sf_busy;		/* set if scheduler is running */

	u_int sf_open;		/* number of open clients */
	u_int sf_dev;		/* device type */
	u_int sf_cylinder;	/* current cylinder number */
	int   sf_need_recal;
	u_int sf_block_no;	/* starting logical block requested */
	u_int sf_new_cylinder;	/* cylinder for sf_block_no */
	u_int sf_new_head;
	u_int sf_new_sector;

	u_int sf_format_cylinder;
	u_int sf_format_head;
	u_int sf_interleave;

	u_char *sf_addr;
	u_int sf_xlen;
	u_int sf_nxfr;
	u_int sf_nleft;
	short sf_io;		/* 0 for read, 1 for write */
	short sf_retries,sf_rw_retries;
	

	u_char sf_cmd[9],sf_res[10];
	u_char *sf_data;
	int sf_clen,sf_dlen,sf_rlen;

	
	struct buf sf_ioctl; /* internal cmd bufs */
	int sf_req;	/* if sf_ioctl is used, this should be set to the ioctl req */
#define FDR_LOW_LEVEL	0	/* low level controller level cmd */
#define FDR_MODE_SELECT	1	/* mode select on open */
#define FDR_GET_VHB	2	/* get vhb */
#define FDR_FORMAT_TRACK 3
#define FDR_RECAL	4
#define FDR_SENSE_DRIVE 5
#define FDR_CLOSE	6


	u_char sf_ioctl_cmd[9];
	u_char *sf_ioctl_data;
	u_char sf_ioctl_res[7];

	int sf_ioctl_clen;
	int sf_ioctl_dlen;
	int sf_ioctl_rlen;


	int sf_bs;	/* block size */
	struct flop_msel sf_flop;	/* geometry about the drive */
	struct volume_header sf_vhb; /* volume home block */
	int sf_82077;	/* set to 1 if the chip is a 82077 */
};
