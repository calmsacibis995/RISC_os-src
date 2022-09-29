#ident "$Header: ncr53c94.h,v 1.2 90/03/20 07:20:17 hal Exp $"
/* $Copyright: $ */

/*
 * required data structures and register defines for the NCR
 * 53c9X Family of Advanced SCSI Controller chips
 */

struct ncr53c94 {
	u_char pad[3];
	u_char count_lo;	    /*  3 transfer counter (lsb) (R/W) */
	u_char pad1[3];
	u_char count_hi;	    /*  7 transfer counter (msb) (R/W) */
	u_char pad2[3];
	u_char fifo;		    /*  b fifo (R/W) */
	u_char pad3[3];
	u_char command;		    /*  f command (R/W) */
	u_char pad4[3];
	union {
		u_char status;	    /* 13 ASC status (R) */
		u_char dest_id;	    /* 13 destination scsi bus id (W) */
	} s_d;
	u_char pad5[3];
	union {
		u_char interrupts;  /* 17 interrupt sense/int reset (R) */
		u_char timeout;	    /* 17 sel/resel timeout value (W) */
	} i_t;
	u_char pad6[3];
	union {
		u_char seqn_step;   /* 1b sequence step register (R) */
		u_char sync_period; /* 1b syncronous transfer period (W) */
	} s_p;
	u_char pad7[3];
	union {
		u_char fifo_flags;  /* 1f fifo byte count (R) */
		u_char sync_offset; /* 1f syncronous offset (W) */
	} f_o;
	u_char pad8[3];
	u_char config1;	    	    /* 23 configuration 1 register (R/W) */
	u_char pad9[3];
	u_char clk_conv;	    /* 27 clock conversion (W) */
	u_char pad10[3];
	u_char test;		    /* 2b test register (W) */
	u_char pad11[3];
	u_char config2;	    	    /* 33 configuration 2 register (R/W) */
	u_char pad12[3];
	u_char config3;	    	    /* 37 configuration 3 register (R/W) */
};

/* miscellaneous defines */
#define		MAX_FIFO_CNT	0x10	/* max asc fifo depth */
#define		FIFO_MASK	0x1f	/* fifo flags mask */
/* sel/resel timeout register defines
 */
#define		MS250		0x99	/* 250ms timeout value for sel
					 * using 25Mhz clock (pizazz) */
/* sequence step register defines
 */
#define		SEQN_STEP_MSK	  0x7	/* mask out sequence bits */
#define		SEQN_SEL_ATN	  0x4	/* select sequence complete */
#define		SEQN_SEL_NO_ATN	  0x4	/* select sequence complete */
#define		SEQN_SEL_ATN_STOP 0x1	/* select sequence complete */

/* clock conversion register defines
 */
#define 	FACTOR_10MHZ	0x2 	/* 10 Mhz */
#define 	FACTOR_15MHZ	0x3 	/* 10.01 to 15 Mhz */
#define 	FACTOR_20MHZ	0x4 	/* 15.01 to 20 Mhz */
#define 	FACTOR_25MHZ	0x5 	/* 20.01 to 25 Mhz */

/* configuration register 1 defines
 */
#define		PARITY_ENABLE	0x10    /* enable parity checking */
#define		NO_SCSI_RST_INT 0x40    /* disable scsi reset interrupt */
/* bus id defines for writing (set up initiator) lower 3 bits
 */
#define 	BUSID_OUT7	0x7 	/* highest priority scsi dev */
#define 	BUSID_OUT6	0x6
#define 	BUSID_OUT5	0x5
#define 	BUSID_OUT4	0x4
#define 	BUSID_OUT3	0x3
#define 	BUSID_OUT2	0x2
#define 	BUSID_OUT1	0x1
#define 	BUSID_OUT0	0x0	/* lowest priority scsi dev */

/* bus id defines for reading
 */
#define		BUSID_IN7	0x80
#define		BUSID_IN6	0x40
#define		BUSID_IN5	0x20
#define		BUSID_IN4	0x10
#define		BUSID_IN3	0x08
#define		BUSID_IN2	0x04
#define		BUSID_IN1	0x02
#define		BUSID_IN0	0x01

/* Command register
 * 
 *		command		code     */

/* MISCELLANEOUS GROUP			 */
#define		NOP		0x00	/* No operation */
#define 	FLUSH_NCR_FIFO	0x01	/* reset fifo flags and zero last byte*/
#define 	RESET_NCR	0x02	/* soft reset the ncr */
#define 	RESET_SCSI	0x03	/* send RST signal to SCSI bus */

/* DISCONNECTED STATE GROUP		 */
#define		SELECT_NO_ATN	0x41
#define		SELECT_ATN	0x42
#define		SELECT_ATN_STOP	0x43
#define		EN_RESEL	0x44	/* command itself will not yield int */
#define		DISABLE_RESEL	0x45
#define		SELECT_ATN3	0x46	/* SCSI II command tagging */

/* INITIATOR STATE GROUP		 */
#define		TRANSFER	0x10
#define		CMD_CMP		0x11
#define		MSG_OK		0x12
#define		TRANSFER_PAD	0x18
#define		SET_ATN		0x1a

#define 	DMA		0x80	/* OR in for DMA based op */

/* Transfer Mode
 */
#define 	SYNC_XFER	0x80	/* Syncronous transfer */
#define		ASYNC		0	/* use asyncronous data transfers */
#define		OFFSET_MASK	0x70	/* pick out sync offset bits */
#define		OFFSET_SHIFT	4	/* bits to move offset value */
#define		OFFSET_8	0	/* use "8" req offset as max value */
#define		PERIOD_MASK	0x0C	/* pick out syncronous period value */
#define		PERIOD_SHIFT	2	/* bits to move period value */
#define		PERIOD_0	0	/* 4.0  mbytes/s at 8.0 Mhz (125ns) */
#define		PERIOD_1	1	/* 2.67 mbytes/s at 8.0 Mhz (125ns)  */
#define		PERIOD_2	2	/* 2.0  mbytes/s at 8.0 Mhz (125ns) */
#define		PERIOD_3	3	/* 1.6  mbytes/s at 8.0 Mhz (125ns) */
#define 	SYNC_OFFSET_ASC	15	/* maximum sync offset supported */
#define 	SYNC_XFER_RATE_ASC 0x32	/* maximum sync tranfer rate supported
				 	 * 0x32 X 4ns = 200 minimum between ack
				 	 * pulses is max sync xfer (5.0 mb/s) */
/* Interrupt Sense - active interrupts.
 * Reading clears this register along with the status and
 * sequence step registers.
 */
#define		SCSI_RESET	0x80
#define		ILL_CMD		0x40
#define		DISCONECT	0x20
#define		SER_REQ		0x10
#define		CMD_CMPLT	0x08
#define		RESELECT	0x04
#define		SEL_W_ATN	0x02	/* for asc as target only */
#define		SLECT		0x01	/* for asc as target only */
/* this form needed for ncrintr() */
#define		INT_SELECT	0x0	/* for asc as target only */
#define		INT_SELECT_ATN	0x1	/* for asc as target only */
#define		INT_RESELECT	0x2
#define		INT_CMD_CMPLT	0x3
#define		INT_SER_REQ	0x4
#define		INT_DISCONECT	0x5
#define		INT_ILL_CMD	0x6
#define		INT_SCSI_RESET	0x7

/* ASC Status
 */
#define		INTR		0x80	/* ASC is driving INT output */
#define		GROSS_ERR	0x40	/* a GROSS error has occurred */
#define		ASC_PAR_ERR	0x20	/* ASC detected a parity error */
#define		TC0		0x10	/* terminal count is 0 */
#define		VAL_GRP		0x08	/* target only */
#define		MSG		0x04	/* Asserted by TARGET during MSG phase*/
#define		C_D		0x02	/* TRUE indicates control */
#define		I_O		0x01	/* TRUE indicates input to initiator */
#define 	XFER_PHSE_MSK	0x07	/* mask to isolate scsi phase on bus */
#define		BUS_FREE	0x0	/* the best we can do for bus_free */
#define		DATA_OUT	0x0
#define		DATA_IN		I_O
#define		COMMAND		C_D
#define		STATUS		(C_D|I_O)
#define		MSG_OUT		(MSG|C_D)
#define		MSG_IN		(MSG|C_D|I_O)

