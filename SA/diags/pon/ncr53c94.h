#ident "$Header: ncr53c94.h,v 1.2.1.1 90/07/18 14:28:53 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/***********************************************************************
 *
 * 	Name : ncr53c94.h
 *
 * 	Description : Defines for the SCSI Protocol Controller.
 *		NCR 53c94.
 *
 *
 ***********************************************************************/



#ifdef LANGUAGE_C

struct scsi_g    {
		unsigned char buf0[3];
		unsigned char lo_count;
		unsigned char buf1[3];
		unsigned char hi_count;
		unsigned char buf2[3];
		unsigned char fifo;
		unsigned char buf3[3];
		unsigned char cmd;
		unsigned char buf4[3];
		unsigned char status_id;
		unsigned char buf5[3];
		unsigned char intr_timeout;
		unsigned char buf6[3];
		unsigned char seqstep_syncperiod;
		unsigned char buf7[3];
		unsigned char fifoflag_syncoffset;
		unsigned char buf8[3];
		unsigned char config1;
		unsigned char buf9[3];
		unsigned char clockfactor;
		unsigned char buf10[3];
		unsigned char test;
		unsigned char buf11[3];
		unsigned char config2;
		unsigned char buf12[3];
		unsigned char config3;
		 };

struct scsi_r    {
		unsigned char lo_count;
		unsigned char hi_count;
		unsigned char fifo[16];
		unsigned char cmd;
		unsigned char status;
		unsigned char destination_id;
		unsigned char interrupt;
		unsigned char timeout;
		unsigned char seqstep;
		unsigned char syncperiod;
		unsigned char fifoflag;
		unsigned char syncoffset;
		unsigned char config1;
		unsigned char clockfactor;
		unsigned char test;
		unsigned char config2;
		unsigned char config3;
		};

#endif LANGUAGE_C


/*
 * Command Register
 */
#define NCR_CMD_DMA		0x80		/* DMA bit */
#define NCR_CMD_NOP		0x00		/* NOP command */
#define NCR_CMD_FLUSH_FIFO	0x01		/* Flush FIFO  command */
#define NCR_CMD_CHIP_RESET	0x02		/* Reset Chip  command */
#define NCR_CMD_SCSI_RESET	0x03		/* Reset SCSI Bus command */

#define NCR_CMD_RESLCT		0x40		/* reselection Command */
#define NCR_CMD_SLCT		0x41		/* selection Command */
#define NCR_CMD_SLCT_ATN	0x42		/* selection with attention Command */
#define NCR_CMD_SLCT_STOP	0x43		/* selection with attention and stop Command */
#define NCR_CMD_ENA_SLCT	0x44		/* enable selection/reselection Command */
#define NCR_CMD_DIS_SLCT	0x45		/* disable selection/reselection Command */
#define NCR_CMD_SLCT_ATN3	0x46		/* selection with attention 3  Command */

#define NCR_CMD_XFR_INFO	0x10		/* Transfer Info Command */
#define NCR_CMD_INI_CMPLT	0x11		/* command complete sequence Command */
#define NCR_CMD_MSG_ACPTD	0x12		/* Message accpted Command */
#define NCR_CMD_XFR_PAD		0x18		/* transfer pad Command */
#define NCR_CMD_SET_ATN		0x1a		/* Set attention Command */

/*
 * Configuration_1 register
 */
#define NCR_CFG1_IDMSK		0x07		/* My Bus ID bits */
#define NCR_CFG1_TEST		0x08		/* Test Mode */

/*
 * Test register
 */
#define NCR_TEST_T		0x01		/* Target role */
#define NCR_TEST_I		0x02		/* Initiator role */
#define NCR_TEST_Z		0x04		/* High Impedance outputs */
#define NCR_TEST_MSK		0x07		/* Test Regsiter Mask*/

/*
 * FIFO FLAG register
 */
#define NCR_FIFO_CNT_MSK	0x1f		/* Five bits of FIFO count */
#define NCR_FIFO_HSIZE		0x08		/* size of NCR FIFO in Halfwords */
#define NCR_FIFO_BSIZE		0x10		/* size of NCR FIFO in bytes */
#define NCR_FIFO_SEQ_STEP	0x70		/* 3 bits of sequence step */

/*
 * Status register
 */
#define NCR_STA_IO		0x01		/* I/O Phase bit */
#define NCR_STA_CD		0x02		/* Command/Data Phase bit */
#define NCR_STA_MSG		0x04		/* message Phase bit */
#define NCR_STA_VGC		0x08		/* Valid Group Code bit */
#define NCR_STA_TC		0x10		/* Terminal Count bit */
#define NCR_STA_PE		0x20		/* Parity Error  bit */
#define NCR_STA_GE		0x40		/* Gross Error  bit */
#define NCR_STA_INT		0x80		/* interrupt bit */

#define PHASE_MASK		0x07		/* phase bits mask */
#define MSG_IN_F		0x07		/* message in Phase */
#define MSG_OUT_F		0x06		/* message out Phase */
#define DATA_IN_F		0x01		/* data in Phase */
#define DATA_OUT_F		0x00		/* data out Phase */
#define CMD_F			0x02		/* command Phase */
#define STATUS_F		0x03		/* status Phase */

/*
 * interrupt register
 */
#define NCR_INT_BUS_RST		0x80		/* scsi bus reset bit */
#define NCR_INT_ILL		0x40		/* illegal command bit */
#define NCR_INT_DIS		0x20		/* disconnect bit */
#define NCR_INT_BS		0x10		/* Bus service bit */
#define NCR_INT_FC		0x08		/* function complete  bit */
#define NCR_INT_RESEL		0x04		/* reselect bit */
#define NCR_INT_SEL_ATN		0x02		/* select with attention bit */
#define NCR_INT_SEL		0x01		/* select bit */


