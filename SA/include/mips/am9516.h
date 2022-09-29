#ident "$Header: am9516.h,v 1.9 90/01/23 14:08:29 huang Exp $"
/* $Copyright$ */

/*
 * required data structures and register defines for the AMD
 * Am9516 Universal DMA Controller Chip
 */
struct am9516 {
	u_short pad0;		/* 0 */
	u_short base;		/* 2  udc base register */
	u_short pad1;		/* 4 */
	u_short ptr;		/* 6  udc register pointer */
};
/* Channel Address Register Tag Field defines (Bits 1-2)
 * Chain   Address Register Tag Field defines (Bits 1-2)
 */
#define NOWAIT		0x0
#define ONEWAIT		0x2
#define TWOWAIT		0x4
#define FOURWAIT	0x6
#define UDC_WAIT_MASK	0x6
#define UDC_WAIT_SHIFT	1
/* Channel Address Register Tag Field defines (Bits 3-4) */
#define	INCR		0x00
#define	DECR		0x08
#define HOLD		0x10
#define UDC_CTRL_MASK	0x18
#define UDC_CTRL_SHIFT	3
/* Channel Address Register Tag Field defines (Bits 6-7) */
#define SYS_IO		0x00
#define SYS_MEM		0x40
#define NORM_IO		0x80
#define NORM_MEM	0xc0
#define UDC_REF_MASK	0xc0
#define UDC_REF_SHIFT	6

/* Channel address Tag values oriented as to their placement in a
 * 32-bit word as used by the disk and tape drivers
 */
#define WAIT0		0x000000
#define WAIT1		0x020000
#define WAIT2		0x040000
#define WAIT4		0x060000
#define INC_ADDR	0x000000
#define DEC_ADDR	0x080000
#define HOLD_ADDR	0x100000
#define UDC_A25		0x400000	/* address bit 25 enable */
#define UDC_A24		0x800000	/* address bit 24 enable */
#define UDC_A25_P	0x40		/* address bit 25 enable (for prints) */
#define UDC_A24_P	0x80		/* address bit 24 enable (for prints) */
#define UDC_A24_A25	0xc00000	/* address bit 24 and 25 enable */
#define UDC_ADDR(x)	((x & 0xffff) | ((x & 0xffff0000) << 8))
#define ADDR_24		0x1000000
#define ADDR_25		0x2000000

/* Chain Control Register defines (reload word)
 */
#define LD_CUR_ARA		0x200
#define LD_CUR_ARB		0x100
#define LD_CUR_OPCNT		0x080
#define LD_BASE_ARA		0x040
#define LD_BASE_ARB		0x020
#define LD_BASE_OPCNT		0x010
#define LD_PAT_MASK		0x008
#define LD_VECTOR		0x004
#define LD_CHAN_MODE		0x002
#define LD_CHAIN_ADDR		0x001
/*
 * RELOAD words for use with the SCSI DMA blk struct
 */
#define LD_SCSI_R	(LD_CUR_ARB|LD_CUR_OPCNT|LD_CHAN_MODE|LD_CHAIN_ADDR)
#define LD_SCSI_W	(LD_CUR_ARA|LD_CUR_OPCNT|LD_CHAN_MODE|LD_CHAIN_ADDR)
/*
 * UDC control register offsets
 */
#define CARB2_LO	0x00
#define CARB1_LO	0x02
#define BARB2_LO	0x04
#define BARB1_LO	0x06
#define CARA2_LO	0x08
#define CARA1_LO	0x0a
#define BARA2_LO	0x0c
#define BARA1_LO	0x0e
#define CARB2_HI	0x10
#define CARB1_HI	0x12
#define BARB2_HI	0x14
#define BARB1_HI	0x16
#define CARA2_HI	0x18
#define CARA1_HI	0x1a
#define BARA2_HI	0x1c
#define BARA1_HI	0x1e
#define CAR2_LO		0x20
#define CAR1_LO		0x22
#define CAR2_HI		0x24
#define CAR1_HI		0x26
#define	INTRSAVE2	0x28
#define	INTRSAVE1	0x2a
#define COMMAND2	0x2c
#define	STATUS2		0x2c
#define COMMAND1	0x2e
#define	STATUS1		0x2e
#define COC2		0x30
#define COC1		0x32
#define	BOC2		0x34
#define	BOC1		0x36
#define MASTER_MODE	0x38
#define	PATTERN2	0x48
#define	PATTERN1	0x4a
#define	MASK2		0x4c
#define	MASK1		0x4e
#define	CMR2_LO		0x50
#define	CMR1_LO		0x52
#define	CMR2_HI		0x54
#define	CMR1_HI		0x56
#define	INTRVEC2	0x58
#define	INTRVEC1	0x5a
	
/* UDC Master Mode Register Bits
 */
#define CHIP_ENABLE	0x1	/* enable UDC to request the bus */
#define CHIP_INTLV_EN	0x2	/* enable interleaving CPU<->UDC */
#define CHIP_WAIT_EN	0x4	/* enable sampling of the WAIT line */
#define CHIP_NOVEC	0x8	/* no vector driven during int ack */

/* UDC Command Register defines
 */
#define	RESET_CMD	0x0
#define START_CHAIN1	0xa0
#define START_CHAIN2	0xa1
#define SET_SFT_REQ1	0x42
#define SET_SFT_REQ2	0x43
#define CLR_SFT_REQ1	0x40
#define CLR_SFT_REQ2	0x41
#define SET_HDW_MSK1	0x82
#define SET_HDW_MSK2	0x83
#define CLR_HDW_MSK1	0x80
#define CLR_HDW_MSK2	0x81
#define SET_CIE_IP1	0x36
#define SET_CIE_IP2	0x37
#define CLR_CIE_IP1	0x34
#define CLR_CIE_IP2	0x35

/* UDC Channel Status bits
 */
#define	TC	0x0001
#define	EOP	0x0002
#define	MC	0x0004
#define	MCL	0x0008
#define	MCH	0x0010
#define	HRQ	0x0020
#define	HM	0x0040
#define	SIP	0x0200
#define	WFB	0x0400
#define	NAC	0x0800
#define	CA	0x1000
#define	IP	0x2000
#define	CIE	0x8000

#define RESET_STATUS	(CA|NAC)	/* after reset these bits s/b set */

/* Interrupt Save register bit defines
 */
#define VEC_MASK	0xff
#define	ISR_CH		0x100	/* channel mask */
#define ISR_TC		0x200
#define ISR_EOP		0x400
#define ISR_MC		0x800
#define ISR_CH_ABORT	0x1000
#define ISR_MCL		0x2000
#define ISR_MCH		0x4000
#define ISR_HDW_REQ	0x8000

/* Chain Control Register Bits
 */
#define CARA		0x200
#define CARB		0x100
#define COPCNT		0x80
#define BARA		0x200
#define BARB		0x100

/* Reload Word bit definitions
 */
#define COUNT		0x1
#define SRC		0x2
#define DEST		0x4
#define CHANMODE	0x8
#define CHAINADR	0x10
#define ENDOFCHAIN	0

/* Channel Mode Register Operation Field defines (Bits 0-3)
 */
#define WORD_FLOWTHRU	0x0 /*FlowThru Mode both src,dest. are word wide*/
#define BYTE_FLOWTHRU	0x1 /*FlowThru Mode both src,dest. are byte wide*/
#define	WORD_FLYBY	0x2 /*FlyBy mode both src and dest. are word wide*/
#define BYTE_FLYBY	0x3 /*FlyBy mode both src and dest. are byte wide*/
#define BW_FLOWTHRU	0x8 /*FlowThru mode src byte dest word(16 bits) */
#define UDC_OP_MASK	0xf

#define FLIP		0x10
#define NOFLIP		0x0

/* Channel Mode Register Transfer Type Field defines (Bits 5-6)
 */
#define SINGLE_XFER	0x00
#define DD_BUSHOLD	0x20
#define DD_BUSREL	0x40
#define DEMAND_INTLV	0x60
#define UDC_XFER_MASK	0x60
#define UDC_XFER_SHIFT	5

/* Channel Mode Register Completion Field defines (Bits 5-6)
 */
#define	IE_EOP	0x80	/* EOP for interrupt enable */
#define	IE_MC	0x100	/* MC for interrupt enable */
#define	IE_TC	0x200	/* TC for interrupt enable */
#define	RE_EOP	0x400	/* EOP for reload enable */
#define	RE_MC	0x800	/* MC for reload enable */
#define	RE_TC	0x1000	/* TC for reload enable */
#define	CE_EOP	0x2000	/* EOP for chain enable */
#define	CE_MC	0x4000	/* MC for chain enable */
#define	CE_TC	0x8000	/* TC for chain enable */

/* Channel Mode Register Match Control Field defines (Bits 16-17)
 */
#define	NOMATCH0	0x0
#define	NOMATCH1	0x10000
#define	MATCHWORD	0x20000
#define	MATCHBYTE	0x30000


/* Channel Mode Register HIGH
 * 
 * these three are needed for register decode printf's
 */
#define	DACK_R		0x4
#define HWMASK_R	0x8
#define	SOFTREQ_R	0x10

#define	DACK		0x40000  /* if set used pulsed dack output flyby */
#define HWMASK		0x80000  /* if set don't listen to dreq input */
#define	SOFTREQ		0x100000 /* if set start the dma op */ 

/* Constant channel mode values
 */
#define CHMODE_R	(WORD_FLYBY|FLIP|DD_BUSREL|DACK)
#define CHMODE_W	(WORD_FLYBY|DD_BUSREL|DACK)
#define CHMODE_R_SIM 	(WORD_FLYBY|FLIP|DD_BUSREL|DACK|SOFTREQ|HWMASK)
#define CHMODE_W_SIM 	(WORD_FLYBY|DD_BUSREL|DACK|SOFTREQ|HWMASK)

#ifndef	NOREGS
/*
 * UDC Master Mode register description.
 */
static struct reg_desc udcmm_desc[] = {
	/* mask			shift	name		format	values */
	{  CHIP_NOVEC,		0,	"NoVec",	0,	0	},
	{  CHIP_WAIT_EN,	0,	"WaitEn",	0,	0	},
	{  CHIP_INTLV_EN,	0,	"IntlvEn",	0,	0	},
	{  CHIP_ENABLE,		0,	"ChipEn",	0,	0	},
	{  0,			0,	0,		0,	0	}
};

/*
 * UDC Chain Address register high description.
 */
static struct reg_values udcwait_values[] = {
	/* value			name */
	{  NOWAIT >> UDC_WAIT_SHIFT,	"NoWait"	},
	{  ONEWAIT >> UDC_WAIT_SHIFT,	"1Wait"		},
	{  TWOWAIT >> UDC_WAIT_SHIFT,	"2Wait"		},
	{  FOURWAIT >> UDC_WAIT_SHIFT,	"4Wait"		},
	{  0,				0		}
};

static struct reg_desc udc_chainhi_desc[] = {
	/* mask			shift			name		format	values */
	{  0xff00,		-8,			"Adr23-16",	"%x",	0		},
	{  UDC_WAIT_MASK,	-UDC_WAIT_SHIFT,	"Wait",		"%d",	udcwait_values	},
	{  0,			0,			0,		0,	0		}
};

/*
 * UDC Base/Current Address register high description.
 */
static struct reg_values udcctrl_values[] = {
	/* value				name */
	{  INCR >> UDC_CTRL_SHIFT,		"Inc"	},
	{  DECR >> UDC_CTRL_SHIFT,		"Dec"	},
	{  HOLD >> UDC_CTRL_SHIFT,		"Hold"	},
	{  (HOLD >> UDC_CTRL_SHIFT) | 1,	"Hold"	},	/* Hold is 1X (10 and 11) */
	{  0,					0	}
};

static struct reg_values udcref_values[] = {
	/* value			name */
	{  SYS_IO >> UDC_REF_SHIFT,	"SysIO"		},
	{  SYS_MEM >> UDC_REF_SHIFT,	"SysMem"	},
	{  NORM_IO >> UDC_REF_SHIFT,	"NormIO"	},
	{  NORM_MEM >> UDC_REF_SHIFT,	"NormMem"	},
	{  0,				0		}
};

static struct reg_desc udc_car_desc[] = {
	/* mask			shift			name		format	values */
	{  0xff00,		-8,			"Adr23-16",	"%x",	0		},
	{  UDC_REF_MASK,	-UDC_REF_SHIFT,		"Ref",		"%d",	udcref_values	},
	{  UDC_CTRL_MASK,	-UDC_CTRL_SHIFT,	"Ctl",		"%d",	udcctrl_values	},
	{  UDC_WAIT_MASK,	-UDC_WAIT_SHIFT,	"Wait",		"%d",	udcwait_values	},
	{  0,			0,			0,		0,	0		}
};

/*
 * UDC Channel Status register description.
 */
static struct reg_desc udc_stat_desc[] = {
	/* mask		shift	name	format	values */
	{  CIE,		0,	"CIE",	0,	0	},
	{  IP,		0,	"IP",	0,	0	},
	{  CA,		0,	"CA",	0,	0	},
	{  NAC,		0,	"NAC",	0,	0	},
	{  WFB,		0,	"WFB",	0,	0	},
	{  SIP,		0,	"SIP",	0,	0	},
	{  HM,		0,	"HM",	0,	0	},
	{  HRQ,		0,	"HRq",	0,	0	},
	{  MCH,		0,	"MCH",	0,	0	},
	{  MCL,		0,	"MCL",	0,	0	},
	{  MC,		0,	"MC",	0,	0	},
	{  EOP,		0,	"EOP",	0,	0	},
	{  TC,		0,	"TC",	0,	0	},
	{  0,		0,	0,	0,	0	}
};

/*
 * UDC Channel Mode register high description.
 */
static struct reg_values udcmatch_values[] = {
	/* value	name */
	{  0,		"StpNoMatch"	},
	{  1,		"StpNoMatch"	},
	{  2,		"StpWrdMatch"	},
	{  3,		"StpBytMatch"	},
	{  0,		0		}
};

static struct reg_desc udc_modehi_desc[] = {
	/* mask		shift	name		format	values */
	{  SOFTREQ_R,	0,	"SoftRq",	0,	0		},
	{  HWMASK_R,	0,	"HWMask",	0,	0		},
	{  DACK_R,	0,	"DAck",		0,	0		},
	{  0x3,		0,	"MatchCtl",	"%d",	udcmatch_values	},
	{  0,		0,	0,		0,	0		}
};

/*
 * UDC Channel Mode register low description.
 */
static struct reg_values udcop_values[] = {
	/* value	name */
	{  0x0,		"W->W_XferFlo"		},
	{  0x1,		"B->B_XferFlo"		},
	{  0x2,		"W->W_XferFly"		},
	{  0x3,		"B->B_XferFly"		},
	{  0x4,		"W->W_XferSrchFlo"	},
	{  0x5,		"B->B_XferSrchFlo"	},
	{  0x6,		"W->W_XferSrchFly"	},
	{  0x7,		"B->B_XferSrchFly"	},
	{  0x8,		"B->W_XferFlo"		},
	{  0x9,		"B->W_XferFlo"		},
	{  0xa,		"IllOp"			},
	{  0xb,		"IllOp"			},
	{  0xc,		"B->W_XferShFlo"	},
	{  0xd,		"B->W_XferShFlo"	},
	{  0xe,		"W->W_Srch"		},
	{  0xf,		"B->B_Srch"		},
	{  0,		0			}
};

static struct reg_values udcxfer_values[] = {
	/* value				name */
	{  SINGLE_XFER >> UDC_XFER_SHIFT,	"Single"	},
	{  DD_BUSHOLD >> UDC_XFER_SHIFT,	"DDBusHld"	},
	{  DD_BUSREL >> UDC_XFER_SHIFT,		"DDBusRel"	},
	{  DEMAND_INTLV >> UDC_XFER_SHIFT,	"Intlv"		},
	{  0,					0		}
};

static struct reg_desc udc_modelo_desc[] = {
	/* mask			shift			name		format	values */
	{  CE_TC,		0,			"CE_TC",	0,	0		},
	{  CE_MC,		0,			"CE_MC",	0,	0		},
	{  CE_EOP,		0,			"CE_EOP",	0,	0		},
	{  RE_TC,		0,			"RE_TC",	0,	0		},
	{  RE_MC,		0,			"RE_MC",	0,	0		},
	{  RE_EOP,		0,			"RE_EOP",	0,	0		},
	{  IE_TC,		0,			"IE_TC",	0,	0		},
	{  IE_MC,		0,			"IE_MC",	0,	0		},
	{  IE_EOP,		0,			"IE_EOP",	0,	0		},
	{  UDC_XFER_MASK,	-UDC_XFER_SHIFT,	"Xfer",		"%d",	udcxfer_values	},
	{  FLIP,		0,			"Flip",		0,	0		},
	{  UDC_OP_MASK,		0,			"OP",		"%x",	udcop_values	},
	{  0,			0,			0,		0,	0		}
};

/*
 * UDC Interrupt Save register description.
 */
static struct reg_values udcchan_values[] = {
	/* value	name */
	{  0,		"1"	},
	{  1,		"2"	},
	{  0,		0	}
};

static struct reg_desc udc_intsav_desc[] = {
	/* mask			shift	name		format	values */
	{  ISR_HDW_REQ,		0,	"HWReq",	0,	0		},
	{  ISR_MCH,		0,	"MCH",		0,	0		},
	{  ISR_MCL,		0,	"MCL",		0,	0		},
	{  ISR_CH_ABORT,	0,	"ChainAbt",	0,	0		},
	{  ISR_MC,		0,	"MC",		0,	0		},
	{  ISR_EOP,		0,	"EOP",		0,	0		},
	{  ISR_TC,		0,	"TC",		0,	0		},
	{  ISR_CH,		-8,	"Chan",		0,	udcchan_values	},
	{  VEC_MASK,		0,	"Vec",		"%d",	0		},
	{  0,			0,	0,		0,	0		}
};
#endif	NOREGS
