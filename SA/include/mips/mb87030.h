#ident "$Header: mb87030.h,v 1.7 90/01/23 14:17:05 huang Exp $"
/* $Copyright$ */

/*
 * required data structures and register defines for the Fujitsu
 * MB87030 SCSI Protocol Controller Chip
 */

struct mb87030 {
	u_char pad[3];
	u_char scsi_id;		/*  3 scsi bus id */
	u_char pad1[3];
	u_char control;		/*  7 SPC control */
	u_char pad2[3];
	u_char command;		/*  b command */
	u_char pad3[3];
	u_char xfer_mode;	/*  f transfer mode */
	u_char pad4[3];
	u_char interupts;	/* 13 interrupt sense/interrupt reset */
	u_char pad5[3];
	union {
		u_char phase_s;	/* 17 scsi bus phase sense */
		u_char diag_c;	/* 17 scsi bus phase diag control */
	} phse_diag;
	u_char pad6[3];
	u_char status;		/* 1b SPC status */
	u_char pad7[3];
	u_char err_stat;	/* 1f SPC error status */
	u_char pad8[3];
	u_char phase_c;		/* 23 scsi bus phase control */
	u_char pad9[3];
	u_char byte_cnt;	/* 27 modified byte counter */
	u_char pad10[3];
	u_char data;		/* 2b data register (8 byte FIFO) */
	u_char pad11[3];
	u_char temp;		/* 2f temporary register */
	u_char pad12[3];
	u_char count_hi;	/* 33 transfer counter (high) */
	u_char pad13[3];
	u_char count_mid;	/* 37 transfer counter (middle) */
	u_char pad14[3];
	u_char count_lo;	/* 3b transfer counter (low) */
	u_char pad15[3];
	u_char ext_buff;	/* 3f external buffer */
};
/* MISCELLANEOUS useful defines for SPC control
 */
#define		TWAIT	 0x4	/* count_lo value during SEL yields 1250ns
				 * delay after BUS FREE before ARB/SEL starts */
#define		TSEL_HI  0x20	/* these two values are used to provide an */
#define		TSEL_LO  0x00	/* ~1 sec target response supervisory time,
				 * ie SELECT command TIME-OUT */
#define		CNTRL_DEFLT	0x80	/* control reg power-on defaults */
#define		INTS_DEFLT	0x00	/* interrupt reg power-on default */
#define		SERR_DEFLT	0x00	/* spc error reg power-on default */
#define		HIN_READ	0x0
#define		HIN_WRITE	0x1

/* bus id defines for writing (set up initiator)
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

/* SPC control byte definitions 
 */
#define		RESET_DISABLE	0x80	/* reset SPC internally */
#define		CNTRL_RESET	0x40	/* reset SPC data xfer circuitry */
#define		DIAG_MODE	0x20	/* Diagnostic mode(disabled from scsi)*/
#define		ARBIT_EN	0x10	/* Arbitratration enabled */
#define		ARBIT_NO	0x00	/* Arbitratration disabled */
#define		PARITY_EN	0x08	/* Parity enabled (data from scsi) */
#define		PARITY_NO	0x00	/* Parity disabled (data from scsi) */
#define		SEL_EN		0x04	/* spc respnds like target during SEL */
#define		SEL_NO		0x00	/* spc doesn't respond to SEL phase */
#define		RESEL_EN	0x02	/* spc responds as Initiator to RESEL */
#define		RESEL_NO	0x00	/* spc doesn't respond to RESEL phase */
#define		INTS_EN		0x01	/* Interrupts enabled */
					/* NOTE: reset int can't be masked, int
					 * EVENTS are always seen in int reg */
#define		INTS_NO		0x00	/* Interrupts disabled */
	
/* Command register
 */
#define		SCSI_CMD_MASK	0xe0
#define		SCSI_CMD_SHIFT	5

/*		command		code     */

#define		BUS_REL		0x00
#define		SELECT		0x20
#define		RST_ATN		0x40
#define		SET_ATN		0x60
#define		TRANSFER	0x80
#define		XFER_PAUSE	0xa0
#define		RST_REQ		0xc0	/* SPC is TARGET */
#define		SET_REQ		0xe0	/* SPC is TARGET */
#define		RST_ACK		0xc0	/* SPC is INITIATOR */
#define		SET_ACK		0xe0	/* SPC is INITIATOR */

#define 	RST_OUT		0x10	/* send RST signal to SCSI */
#define		INTCPT_XFER	0x08	/* special data transfer mode */
#define		PRG_XFER	0x04	/* Program transfer mode, or */
#define		CPU_XFER	0x04   	/* data xfer between CPU and data reg */
#define		DMA_XFER_MODE	0     	/* data xfer in dma mode ->dreq to ext*/
#define		TERM_MODE	0x01	/* transfer command termination mode */
#define		PAD_XFER	1	/* "padding" xfer mode */
#define		TC0_TERM	0     	/* xfer cmd terminates when TC = 0 */

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

/* Phase Control Bit Definitions
 */
#define		BFREE_INT_EN	0x80	/* enable an int on BUS FREE detected */
#define		I_O_SEL		0x00	/* tell SELECT cmd we're SELECT'ing */
#define		I_O_RESEL	0x01	/* tell SELECT cmd we're RESELECT'ing */
#define		P_DATAOUT	0x0	/* "data out" data transfer phase */
#define		P_DATAIN	0x1	/* "data in" data transfer phase */
#define		P_CMD		0x2	/* "command" data transfer phase */
#define		P_STATUS	0x3	/* "status" data transfer phase */
#define		NO_PHASE	0x4
#define		P_BOGUS		0x5	/* bogus, undefined phase */
#define		P_MSGOUT	0x6	/* "message out" data transfer phase */
#define		P_MSGIN		0x7	/* "message in" data transfer phase */

/* Phase Sense Bit Definitions
 */
#define		REQ		0x80	/* Driven by a target to indicate
					 * a request for a req/ack handshake */
#define		ACK		0x40	/* Driven by initiator for req/ack 
					 * data transfer handshake */
#define		ATN		0x20	/* Asserted by initiator for ATN cond */
#define		SEL		0x10	/* used by initiator during SEL phase
					 * or by target during RESEL phase */
#define		BSY		0x08	/* Asserted indicates scsi bus in use */
#define		MSG		0x04	/* Asserted by TARGET during MSG phase*/
#define		C_D		0x02	/* TRUE indicates control */
#define		I_O		0x01	/* TRUE indicates input to initiator */
#define		BUS_NOT_FREE	0x18	/* busy or sel asserted => not free */
#define 	XFER_PHSE_MSK	0x07	/* mask to isolate scsi phase on bus */
#define		DATA_OUT	0x0
#define		DATA_IN		I_O
#define		COMMAND		C_D
#define		STATUS		(C_D|I_O)
#define		MSG_OUT		(MSG|C_D)
#define		MSG_IN		(MSG|C_D|I_O)
/* Interrupt Sense - active interrupts.
 * A write to this register with the given bit set, reset's that particular
 * interrupt. Multiple ints can be reset at the same time.
 */
#define		SLECT		0x80
#define		RESELECT	0x40
#define		DISCONECT	0x20
#define		CMD_CMPLT	0x10
#define		SER_REQ		0x08
#define		TIME_OUT	0x04
#define		HDW_ERROR	0x02
#define		SCSI_RESET	0x01
/* this form needed for fujiintr() */
#define		INT_SCSI_RESET	0x0
#define		INT_HDW_ERROR	0x1
#define		INT_TIME_OUT	0x2
#define		INT_SER_REQ	0x3
#define		INT_CMD_CMPLT	0x4
#define		INT_DISCONECT	0x5
#define		INT_RESELECT	0x6
#define		INT_SELECT	0x7

/* SPC Status
 */
#define		SCSI_STAT_MASK	0xf0
#define		SCSI_STAT_SHIFT	4
#define		NO_CONNECT	0x0	/* SPC not connected with SCSI */
#define		NO_CONNECT_SEL	0x2	/* No connect, select command waiting
					 * for Bus Free phase or arbit phase */
#define		TARG_NOP_MAN	0x4	/* SPC is TARGET, no op on SCSI or 
					 * manual transfer being executed */
#define		RESELECTION	0x6	/* SPC executing RESELECT on SCSI */
#define		TARG_XFER	0x7	/* SPC is TARGET, executing Hardware
					 * transfer operation (xfer cmd) */
#define		INIT_NOP_MAN	0x8	/* SPC is INITIATOR, no op on SCSI or
					 * manual transfer being executed */
#define		INIT_NO_XFER	0x9	/* Initiator rx'ed REQ but no transfer
					 * command OR transfer phase mismatch */
#define		SELECTION	0xa	/* executing SELECTION phase on SCSI */
#define		INIT_XFER	0xb	/* SPC is INITIATOR, executing Hardware 
					 * transfer operation (xfer cmd) */

#define		INIT		0x80	/* these two are connected status */
#define		TARG		0x40
#define		BUSY		0x20
#define		XFER		0x10
#define		RESET_ST	0x08
#define		TC0		0x04
#define		DFULL		0x02	/* these two are data register status */
#define		DEMPTY		0x01
#define	 	SEL_MASK	0xe0	/* isolate bits associated with sel */
#define	 	XFER_CMP_ST	(INIT|TC0|DEMPTY)

/* SPC Error
 */
#define		SCSI_PAR_ERR	0x80	/* these two are data errors */
#define		SPC_PAR_ERR	0x40
#define		TC_PAR_ERR	0x08
#define		PHASE_ERR	0x04
#define		SHRT_XFER_PER	0x02
#define		XFER_OFFSET     0x01

/* SDGC register defines  (allows CPU to generate fake SCSI input signals)
 */
#define		DIAG_REQ	0x80
#define		DIAG_ACK	0x40
#define		DIAG_BSY	0x08
#define		DIAG_MSG	0x04
#define		DIAG_C_D	0x02
#define		DIAG_I_O	0x01 

#ifndef	NOREGS
/*
 * SPC Control register description.
 */
static struct reg_desc spcc_desc[] = {
	/* mask			shift	name		format	values */
	{  RESET_DISABLE,	0,	"RstDis",	0,	0	},
	{  CNTRL_RESET,		0,	"CtlRst",	0,	0	},
	{  DIAG_MODE,		0,	"DiagMod",	0,	0	},
	{  ARBIT_EN,		0,	"ARBITEn",	0,	0	},
	{  PARITY_EN,		0,	"ParEn",	0,	0	},
	{  SEL_EN,		0,	"SelEn",	0,	0	},
	{  RESEL_EN,		0,	"RselEn",	0,	0	},
	{  INTS_EN,		0,	"IntEn",	0,	0	},
	{  0,			0,	0,		0,	0	}
};

/*
 * SPC Command register description.
 */
static struct reg_values spccmd_values[] = {
	/* value				name */
	{  BUS_REL >> SCSI_CMD_SHIFT,		"BusRel"	},
	{  SELECT >> SCSI_CMD_SHIFT,		"Sel"		},
	{  RST_ATN >> SCSI_CMD_SHIFT,		"RstATN"	},
	{  SET_ATN >> SCSI_CMD_SHIFT,		"SetATN"	},
	{  TRANSFER >> SCSI_CMD_SHIFT,		"Xfer"		},
	{  XFER_PAUSE >> SCSI_CMD_SHIFT,	"XferPaus"	},
	{  RST_ACK >> SCSI_CMD_SHIFT,		"RstACK"	},	/* SPC is initiator only */
	{  SET_ACK >> SCSI_CMD_SHIFT,		"SetACK"	},	/* SPC is initiator only */
	{  0,					0		}
};

static struct reg_desc spccmd_desc[] = {
	/* mask			shift			name		format	values */
	{  SCSI_CMD_MASK,	-SCSI_CMD_SHIFT,	"Cmd",		"%d",	spccmd_values	},
	{  RST_OUT,		0,			"RSTOut",	0,	0		},
	{  INTCPT_XFER,		0,			"Intrcp",	0,	0		},
	{  PRG_XFER,		0,			"PRGXfer",	0,	0		},
	{  TERM_MODE,		0,			"TermMod",	0,	0		},
	{  0,			0,			0,		0,	0		}
};

/*
 * SPC Interrupt Sense register description.
 */
static struct reg_desc spcints_desc[] = {
	/* mask			shift	name		format	values */
	{  SLECT,		0,	"Sel",		0,	0	},
	{  RESELECT,		0,	"Resel",	0,	0	},
	{  DISCONECT,		0,	"DisConn",	0,	0	},
	{  CMD_CMPLT,		0,	"Cmplt",	0,	0	},
	{  SER_REQ,		0,	"ServRq",	0,	0	},
	{  TIME_OUT,		0,	"TimOut",	0,	0	},
	{  HDW_ERROR,		0,	"HardEr",	0,	0	},
	{  SCSI_RESET,		0,	"RstCond",	0,	0	},
	{  0,			0,	0,		0,	0	}
};

/*
 * SPC Phase Sense register description.
 */
static struct reg_desc spcps_desc[] = {
	/* mask		shift	name	format	values */
	{  REQ,		0,	"REQ",	0,	0	},
	{  ACK,		0,	"ACK",	0,	0	},
	{  ATN,		0,	"ATN",	0,	0	},
	{  SEL,		0,	"SEL",	0,	0	},
	{  BSY,		0,	"BSY",	0,	0	},
	{  MSG,		0,	"MSG",	0,	0	},
	{  C_D,		0,	"C/D",	0,	0	},
	{  I_O,		0,	"I/O",	0,	0	},
	{  0,		0,	0,	0,	0	}
};

/*
 * SPC Status register description.
 */
static struct reg_values spcstat_values[] = {
	/* value		name */
	{  NO_CONNECT,		"NoConn"		},
	{  0x1,			"Unused"		},
	{  NO_CONNECT_SEL,	"NoConnSel"		},
	{  0x3,			"Unused"		},
	{  TARG_NOP_MAN,	"TarNoOP/Man"		},
	{  0x5,			"Unused"		},
	{  RESELECTION,		"ReSel"			},
	{  TARG_XFER,		"TarXfer"		},
	{  INIT_NOP_MAN,	"InitNoOP/Man"		},
	{  INIT_NO_XFER,	"InitNoXfer/Mis"	},
	{  SELECTION,		"Sel"			},
	{  INIT_XFER,		"InitXfer"		},
	{  0xc,			"Unused"		},
	{  0xd,			"Unused"		},
	{  0xe,			"Unused"		},
	{  0xf,			"Unused"		},
	{  0,			0			}
};

static struct reg_desc spcst_desc[] = {
	/* mask			shift			name		format	values */
	{  SCSI_STAT_MASK,	-SCSI_STAT_SHIFT,	"OpStat",	"%d",	spcstat_values	},
	{  RESET_ST,		0,			"Rst",		0,	0		},
	{  TC0,			0,			"TC0",		0,	0		},
	{  DFULL,		0,			"DREGFul",	0,	0		},
	{  DEMPTY,		0,			"DREGEmp",	0,	0		},
	{  0,			0,			0,		0,	0		}
};

/*
 * SPC Error Status register description,
 */
static struct reg_desc spcerr_desc[] = {
	/* mask			shift	name		format  values */
	{  SCSI_PAR_ERR,	0,	"SCSIPar",	0,	0	},
	{  SPC_PAR_ERR,		0,	"SPCPar",	0,	0	},
	{  TC_PAR_ERR,		0,	"TCParEr",	0,	0	},
	{  PHASE_ERR,		0,	"PhasEr",	0,	0	},
	{  SHRT_XFER_PER,	0,	"ShrtPrd",	0,	0	},
	{  XFER_OFFSET,		0,	"OffEr",	0,	0	},
	{  0,			0,	0,		0,	0	}
};

/*
 * SPC Phase Control register description.
 */
static struct reg_values spcxfer_values[] = {
	/* value	name */
	{  P_DATAOUT,	"DatOut"	},
	{  P_DATAIN,	"DatIn"		},
	{  P_CMD,	"Cmd"		},
	{  P_STATUS,	"Stat"		},
	{  NO_PHASE,	"Unused"	},
	{  NO_PHASE,	"Unused"	},
	{  P_MSGOUT,	"MsgOut"	},
	{  P_MSGIN,	"MsgIn"		},
	{  0,		0		}
};

static struct reg_desc spcpc_desc[] = {
	/* mask			shift	name		format	values */
	{  BFREE_INT_EN,	0,	"BFreIntEn",	0,	0		},
	{  XFER_PHSE_MSK,	0,	"XferPhas",	"%d",	spcxfer_values	},
	{  0,			0,	0,		0,	0		}
};
#endif	NOREGS
