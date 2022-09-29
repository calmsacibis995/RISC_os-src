#ident "$Header: ecc.h,v 1.3 90/01/23 14:12:57 huang Exp $"
/* $Copyright$ */

#define YES 1
#define NO  0

#define MEG256_VALUE    0x40     /* memory address register value for 256Meg */
#define MEG256_OFFSET 0x10000000 /* address offset to switch access port */
#define BITSPERWORD	  32
#define MAXMEMBRDS	   8	/* maximum MIPS memory boards allowed */
#define LIMIT	         100	/* time-out value */
#define MEM_4MEG_OFFSET    4    /* 4 meg per increment */
#define MEM_SHIFT	  20    /* bits to shift to convert to real address */
#define SYNDROMEBITS  	  39    /* number of possible single-bit syndrome
				   errors */
#define EVEN_LOW_ADDR 0x100000  /* even array/low bank test address */
#define ODD_LOW_ADDR  0x100004  /* odd array/low bank test address */
#define INTLV_EVEN_LOW_ADDR 0x100008  /* even array/low bank test address */
#define INTLV_ODD_LOW_ADDR  0x10000c  /* odd array/low bank test address */
#define EVEN_HI_ADDR  0x300000  /* even array/high bank test address */
#define ODD_HI_ADDR   0x300004  /* odd array/high bank test address */
#define INTLV0_EVEN_HI_ADDR  0x500000  /* even array/high bank test address */
#define INTLV0_ODD_HI_ADDR   0x500004  /* odd array/high bank test address */
#define INTLV1_EVEN_HI_ADDR  0x500008  /* even array/high bank test address */
#define INTLV1_ODD_HI_ADDR   0x50000c  /* odd array/high bank test address */
#define HIGH_BANK_4M_MASK  0x600000  /* addressing HI bank of 4M board */
#define HIGH_BANK_16M_MASK 0x1800000 /* addressing HI bank of 16M board */
#define ODD_BANK_MASK 0x4	/* determine if we're addressing ODD bank */
#define MAGIC_PASS	0x93846153 /* shows that everything went well */
#define DBE_MAGIC_PASS	0x61539384 /* shows that everything went well */
#define MAGIC_FAIL	0x17623548 /* show that something went wrong */

#define	ARMED_FOR_INT1  0xceeeface /* we're expecting this interrupt level */
#define	ARMED_FOR_INT3  0xfacebeef /* we're expecting this interrupt level */
#define	ARMED_FOR_INT7  0xcaefbaea /* we're expecting this interrupt level */
#define	ARMED_FOR_DBE   0xbabababa /* we're expecting this exception */

#define ODD_STAT	0x76
#define EVEN_STAT	0xD6
#define ODD_STAT_PRIV	0x32
#define EVEN_STAT_PRIV	0xC2
#define ODD_STAT_VME	0x36 /* NOTE: this should be 37, error on board */
#define EVEN_STAT_VME	0xC6 /* NOTE: this should be C7, error on board */
	
#define INTLEVEL1	0	/* test interrupt level 1 */
#define INTLEVEL3	1	/* test interrupt level 3 */
#define INTLEVEL7	2	/* test interrupt level 7 */
/* 
 * Syndrome decode information
 */
struct syndrome {
	u_short	code_low;	/* low bank syndrome */
	u_short	code_high;	/* high bank syndrome */
	char *bit;		/* user string indicating which bit is bad */
	char *odd_low_loc;	/* board location of faulty chip
				   odd array/low bank */
	char *odd_high_loc;	/* board location of faulty chip
				   odd array/high bank */
	char *even_low_loc;	/* board location of faulty chip
				   even array/low bank */
	char *even_high_loc;	/* board location of faulty chip
				   even array/high bank */
};
