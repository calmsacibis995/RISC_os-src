/* $Header: ioa.h,v 1.5 90/01/23 14:14:46 huang Exp $ */
/* $Copyright$ */

#ifndef	_SYS_IOA_
#define	_SYS_IOA_	1

/*
 *                I O A   D E F I N I T I O N S
 */

/*
 *   IOA base addresses on the system bus.
 *
 *        size  phys addr       36-bit physical addresses on the Excalibur.
 *        ---- -----------
 *   IOA1 64MB 0x01c000000      IOA1 is 64M and contains PROM beginning at
 *   IOA2 32MB 0x01a000000      physical address 0x01f000000.  IOA2 and IOA3
 *   IOA3 32MB 0x018000000      are 32M each and have no PROM space.
 */

/*
 *   BusChip BoardAddress register values for IOA boards.
 */
#define IOA1_BRD 0xaaa95000
#define IOA2_BRD 0xaaa96400
#define IOA3_BRD 0xaaa96800

/*
 *   IOA virtual addresses in kseg1 (0xa0000000 - 0xbfffffff).
 *   (uncached, hardmapped to 0x00000000 - 0x1fffffff)
 */
#define IOA1 0xbc000000
#define IOA2 0xba000000
#define IOA3 0xb8000000

/*
 *   GBA0 and GBA1 occupy 16MB of system address space each.
 */
#define GBA_SPAN	0x01000000		/* each GBA spans 16MB */
#define GBA0		0x00000000
#define GBA1		0x01000000

/*
 *   PROM space is the upper 16MB of a 64MB IOA.
 */
#define IOA_PROM 0x03000000

/* -------------------------------------------------------------------- *
 *                                                                      *
 *               I O C   L O C A L   M E M O R Y                        *
 *                                                                      *
 * -------------------------------------------------------------------- */

/*
 *   GBA Maps: mapping of system addresses to GBA addresses.
 *
 *                        31   25 24       14 13      0
 *                        -----------------------------
 *   original address:    |      |XXXXXXXXXXX|        |    note: system address
 *                        -----------------------------    bit 24 is 0 for GBA0
 *                                |||||||||||              and 1 for GBA1.
 *                           ---------------------
 *   local memory address:   |0 0|XXXXXXXXXXX|0 0|
 *                           ---------------------
 *                          14 13 12        2 1  0
 *
 *   Bits 14:31 of the local memory location thus addressed contain bits 14:31
 *   of the GBA address; bits 11:13 contain the GBA OpCode; if bit 10 is set
 *   to 1, the IOA will not accept any non-control space accesses until it is
 *   unlocked by writing the IOA_UNLOCK register;  bits 0:9 are passed directly
 *   to the GBA, and their meaning is dependant on the type of GBA.
 */
#define GBA0_MAP 0x0000
#define GBA1_MAP 0x1000
#define LOCK_IOA 0x0400
#define GMAP_SHIFT 14
#define GMAP_MASK 0xffffc000
#define GOPC_SHIFT 11
#define GBA_CTL_BITS 0x000003ff
#define GMAP(GBA) (GBA0_MAP | ((GBA) << 12))
#define GMAP_VME_AM_SHIFT 4		/* l-justify Addr Modifier in field */

/*
 *   GBA OpCodes  (bits 11:13 in the control word)
 */

#define GBA_OPCODE_MASK	0x3800		/* bits 13:11 */
#define GBA_OPCODE_RW	0x0000		/* normal read/write */
#define GBA_OPCODE_IACK	0x0800		/* Interrupt Ack, level in bits 4:2 */
#define GBA_OPCODE_RMW	0x1000		/* read portion of read-modify-write */


/*
 *   System Maps: mapping of GBA addresses to system addresses.
 *
 *                       31    24 23      14 13      0
 *                       -----------------------------
 *   original address:   |       |XXXXXXXXXX|        |
 *                       -----------------------------
 *                                ||||||||||
 *   local memory        ------------------------
 *            address:   |0 0|GBA|XXXXXXXXXX|0 0|
 *                       ------------------------
 *                      14 13 12 11        2 1 0
 *
 *   Bits 10:31 of the the local memory location thus addressed contain
 *   the physical page number (system address bits 14:35):
 *
 *                       31                10 9      0
 *                       -----------------------------
 *          map entry:   | physical page num |   0   | 
 *                       -----------------------------
 *
 */
#define SYS0_MAP 0x2000
#define SYS1_MAP 0x3000
#define SMAP_SHIFT 10
#define IOA_PAGE_SHIFT 14
#define SMAP_MASK 0xfffffc00

#define SMAP(GBA) (SYS0_MAP | ((GBA) << 12))
#define VME_TO_SECT(x) (((x) & 0x00f80000) >> 19)
#define VME_TO_GRPNUM(x) (((x) & 0x0007ff00) >> 8)
#define VME_TO_SMAP(x) (((x) & 0x00ffc000) >> 12)
#define PHYS_TO_SMAP(x) (((x) & ~0x3fff) >> (IOA_PAGE_SHIFT - SMAP_SHIFT))
#define SMAP_TO_VME(x) (((x) & 0x00000ffc) << 12)
#define SMAP_TO_SECT(x) (((x) & 0x00000f80) >> 5)

/*
 *   Gba0 LongOp registers and Interrupt registers.
 */
#define LOP0 0x4000
#define LOP0_INT 0x4400
#define GBA0_INT 0x4C00

/*
 *   Gba1 LongOp registers and Interrupt registers.
 */
#define LOP1 0x5000
#define LOP1_INT 0x5400
#define GBA1_INT 0x5C00

/*
 *   LongOp register ops.  (Low nybble of address.)
 */
#define LOP_WRT (0 * 4)
#define LOP_AND (1 * 4)
#define LOP_OR  (2 * 4)

/*
 *   Macro to select a particular register (0-15).
 *   Works for GbaInt, LongOpInt, and LongOp registers.
 */
#define IOC_REG(NUMBER) ((NUMBER) << 4)

/*
 *   VME-GBA LongOps
 */
#define GBA_FLUSH_OP 0
#define GBA_FETCH_OP 1
#define GBA_BLOCK_MOVE_OP 2

/*
 *   Macros for selecting the register set based on which GBA.
 */
#define GBA_LOP(GBA) (0x4000 | ((GBA) << 12))
#define LOP_INT(GBA) (0x4400 | ((GBA) << 12))
#define GBA_INT(GBA) (0x4C00 | ((GBA) << 12))

/*
 *   Constants.  Must be initialized by software.  Used by the IOC to decode
 *   a bit number into a data word with the appropriate bit set.  32 words,
 *   where word N contains a value with the Nth bit set.
 *   Example:  word 16 contains 0x10000.
 */
#define IOA_CONSTANTS 0x7000

/*
 *   DUART Interrupt registers.
 */
#define IOA_DUART_INT_CMD1 0x7800	/* system bus command word 1 */
#define IOA_DUART_INT_CMD2 0x7804	/* system bus command word 2 */
#define IOA_DUART_INT_DATA 0x7808	/* system bus data word */

/* -------------------------------------------------------------------- *
 *                                                                      *
 *      M I S C E L L A N E O U S   I O C   R E G I S T E R S           *
 *                                                                      *
 * -------------------------------------------------------------------- */

#define IOA_TOD_BASE	0xc000
#define IOA_LED_REGISTER 0xe000
#define IOA_DUART_BASE	0xe200
#define IOA_STATUS_REG1 0xe400
#define IOA_STATUS_REG0 0xe600
#define IOA_LOP_SUM	0xe800
#define IOA_ERRORINFO_REG 0xea00
#define IOA_ERROR_ADDR0	0xec00
#define IOA_ERROR_ADDR1	0xee00


/* -------------------------------------------------------------------- *
 *                                                                      *
 *   I O C   S t a t u s   R e g i s t e r   F o r m a t                *
 *                                                                      *
 * -------------------------------------------------------------------- */

#define IOC_SR_SEC_ERR (1 << 31)	/* bit31 : second error detected */
#define IOC_SR_ADMUX_PTY_ERR (1 << 30)	/* bit30 : ADMux parity error */
#define IOC_SR_DATAOUT_PTY_ERR (1 << 29)/* bit29 : DataOut parity error */
#define IOC_SR_SYS_BUS_ERR (1 << 28)	/* bit28 : system bus error */
#define IOC_SR_GBA_ERR (1 << 27)	/* bit27 : GBA internal error */
					/* bit26-24 : reserved */
#define IOC_SR_STATE (0xF << 19)	/* bit23-19 : state */
					/* bit18 : reserved */
#define IOC_SR_ORIGINATOR (1 << 17)	/* bit17 : orignator (gba/ext) */
#define IOC_SR_QUEUE_USED (1 << 16)	/* bit16 : queue used */
					/* bit15-12 : reserved */
#define IOC_SR_PART_DETECT (1 << 11)	/* bit11 - partial word detected */
#define IOC_SR_PART_SENT (1 << 10)	/* bit10 - partial words sent */
#define IOC_SR_LAST_WAS_BLK (1 << 9)	/* bit9 : last op was a Block op */
#define IOC_SR_XMITOK_RCVD (1 << 8)	/* bit8 - XmitOK rcv'd for last op */
					/* bit7-2 : reserved */
#define IOC_SR_BUS_FREE (1 << 1)	/* bit1 - GBA bus not busy */
#define IOC_SR_BRD_FREE 1		/* bit0 - GBA board not busy */


/* -------------------------------------------------------------------- *
 *                                                                      *
 *             E R R O R I N F O   R E G I S T E R                      *
 *                                                                      *
 * -------------------------------------------------------------------- */

#define G0_SEC_ERR (1 << 31)		/* bit31: GBA0 status reg bit 31 */
#define G0_ADMUX_PTY_ERR (1 << 30)	/* bit30: GBA0 status reg bit 30 */
#define G0_DOUT_PTY_ERR (1 << 29)	/* bit29: GBA0 status reg bit 29 */
#define G0_BUS_ERR (1 << 28)		/* bit28: GBA0 status reg bit 28 */
#define G0_ERR (1 << 27)		/* bit27: GBA0 status reg bit 27 */
					/* bit26: reserved */
#define G0_BUS_FREE (1 << 25)		/* bit25: GBA0 status reg bit 1 */
#define G0_BRD_FREE (1 << 24)		/* bit24: GBA0 status reg bit 0 */
#define G1_SEC_ERR (1 << 23)		/* bit23: GBA1 status reg bit 31 */
#define G1_ADMUX_PTY_ERR (1 << 22)	/* bit22: GBA1 status reg bit 30 */
#define G1_DOUT_PTY_ERR (1 << 21)	/* bit21: GBA1 status reg bit 29 */
#define G1_BUS_ERR (1 << 20)		/* bit20: GBA1 status reg bit 28 */
#define G1_ERR (1 << 19)		/* bit19: GBA1 status reg bit 27 */
					/* bit18: reserved */
#define G1_BUS_FREE (1 << 17)		/* bit17: GBA1 status reg bit 1 */
#define G1_BRD_FREE (1 << 16)		/* bit16: GBA1 status reg bit 0 */
#define IOC_DUART_INT (1 << 15)		/* bit15: DUART asserting interrupt */
					/* bit14: reserved */
#define GBA0_EXISTS (1 << 13)		/* bit13: GBA0 existence */
#define GBA1_EXISTS (1 << 12)		/* bit12: GBA1 existence */
#define IOC_ENA_FORCE_BUSY (1 << 11)	/* bit11: enable ForceBusy */
#define IOC_BUSY_RESET (1 << 10)	/* bit10: reset force busy to SBC */
#define GBA0_RESET (1 << 9)		/* bit09: reset Gba0 */
#define GBA1_RESET (1 << 8)		/* bit08: reset Gba1 */
#define ADMUX_FRC_PERR_MASK (0xF << 4)	/* bit7-4 : force ADmux parity error */
#define FORCE_ADMUX_PERR(x) ((x) << 4)
#define DATAOUT_FRC_PERR_MASK 0xF	/* bit3-0 : force DataOut pty error */
#define FORCE_DATAOUT_PERR(x) (x)



/* -------------------------------------------------------------------- *
 *                                                                      *
 *      M I S C E L L A N E O U S   G B A   R E G I S T E R S           *
 *                                                                      *
 * -------------------------------------------------------------------- */

/*
 *  Base addresses for miscellaneous Gba Registers
 */
#define GBA0_MISC_BASE 0x8000
#define GBA1_MISC_BASE 0x9000
#define GBACTL(GBA) (GBA0_MISC_BASE | ((GBA) << 12))

/*
 *  Offsets for individual miscellaneous Gba Registers
 */
#define GBA_TYPE 0x000
#define GBA_STATUS 0x100
#define GBA_DATA_ADDR 0x200
#define GBA_TAGS 0x400
#define GBA_SECT_INFO 0x500
#define GBA_DIRTY_BITS 0x600
#define GBA_CACHE 0x700
#define GBA_VME_INT_PEND 0x800
#define GBA_VME_ADR_CMP 0xA00
#define GBA_RMW_UNLOCK 0xB00
#define GBA_TOUCH 0xC00
#define GBA_FLUSH 0xD00
#define GBA_BLOCK_START 0xE00
#define GBA_BLOCK_INFO 0xF00

/* -------------------------------------------------------------------- *
 *                                                                      *
 *             G B A  S T A T U S  R E G I S T E R                      *
 *                                                                      *
 * -------------------------------------------------------------------- */

#define GBA_TIMEOUT		(1 << 7)	/* bit7: timeout */
#define GBA_MASTER_BUS_ERR	(1 << 6)	/* bit6: buserr while master */
#define GBA_PARITY_ERR_MASK	(7 << 3)	/* bit5-3: defines pty err */
#define GBA_PARITY_RCVD_OP	(4 << 3)	/*  on received op */
#define GBA_PARITY_RD_MISS	(5 << 3)	/*  during a RdMiss */
#define GBA_PARITY_PREFETCH	(6 << 3)	/*  during a Prefetch */
#define GBA_PARITY_TOUCH	(7 << 3)	/*  during a Touch */
#define GBA_DC_FAIL		(1 << 2)	/* bit2: DC failure */
#define GBA_AC_FAIL		(1 << 1)	/* bit1: AC failure */
#define GBA_RESET_MODE		(1)		/* bit0: doing reset */

/*
 *  GBA cache tag format
 */
#define GT_RD_VALID_WRT (1 << 4)
#define GT_WRT_VALID_WRT (1 << 3)
#define GT_DIRTY_WRT (1 << 2)
#define GT_FETCHED_WRT (1 << 1)
#define GT_BUSY_WRT 1
#define GT_RD_VALID_RD (1 << 31)		
#define GT_WRT_VALID_RD (1 << 30)		
#define GT_DIRTY_RD (1 << 29)		
#define GT_FETCHED_RD (1 << 28)		/* they're in different places */   
#define GT_BUSY_RD (1 << 27)		/* depending on if you're reading */
#define GT_GRPNUM_RD 0x7ff0000		/* or writing */

/*
 *  GBA SectionInfo format
 */
#define GBA_SECTION_SPAN 0x80000	/* bytes spanned by each section */
#define GBA_LINES_PER_SECT 2		/* num cache lines per section */
#define GS_SECT_SHIFT 19
#define GS_UNCACHED_WRT (1 << 3)
#define GS_LOCAL_WRT (1 << 2)
#define GS_RANDOM_WRT (1 << 1)
#define GS_NO_PARTIAL_WRT 1
#define GS_SECTIONINFO_MASK_WRT \
	 (GS_UNCACHED_WRT|GS_LOCAL_WRT|GS_RANDOM_WRT|GS_NO_PARTIAL_WRT)
#define GS_UNCACHED_RD (1 << 11)	/* they're in different places */   
#define GS_LOCAL_RD (1 << 10)		/* depending on if you're reading */
#define GS_RANDOM_RD (1 << 9)		/* or writing */
#define GS_NO_PARTIAL_RD (1 << 8)

/*
 *  Gba cache line size in bytes.
 */
#define GBA_LINE_SIZE 128

#ifndef LOCORE
/****************************************************************************
 * Definition of GBA/IOA hardware usable by kernel procedures written in C.
 *
 * Many of these definitions are alternate definitions of the above.
 ****************************************************************************/

typedef unsigned long Bit;

/****************************************************************************
 * GBA map
 *
 *   GBA0 map entries:  0x0000-0x0FFF in IOC Control Space
 *   GBA1 map entries:  0x1000-0x1FFF in IOC Control Space
 *
 *   For each GBA, the IOC has 1024 map entries which hold the information
 *   for translating M6000 system bus addresses to GBA addresses.  Each
 *   map entry covers one page (16K).
 *
 *   Simple address mapping of 16K pages in IOA's "Standard Space" (i.e.,
 *   not "Control Space") to GBA (e.g., VME) addresses.
 *                                                                             
 *                        31   25 24       14 13      0                        
 *                        -----------------------------                        
 *   original address:    |      |XXXXXXXXXXX|        |    note: system address
 *                        -----------------------------    bit 24 is 0 for GBA0
 *                                |||||||||||              and 1 for GBA1.     
 *                           ---------------------                             
 *   local memory address:   |0 0|XXXXXXXXXXX|0 0|                             
 *                           ---------------------                             
 *                           14 13 12        2 1 0
 *                                                                             
 *   Bits 14:31 of the local memory location thus addressed contain bits 14:31 
 *   of the GBA address; bits 11:13 contain the GBA OpCode; if bit 10 is set   
 *   to 1, the IOA will not accept any non-control space accesses until it is  
 *   unlocked by writing the IOA_UNLOCK register;  bits 0:9 are passed directly
 *   to the GBA, and their meaning is dependant on the type of GBA.            
 *
 *  *) Map entry
 *
 *     31               14 13    11  10 9       0
 *    --------------------------------------------
 *    |  To GBA address   | OpCode | L | Control |       map entry
 *    --------------------------------------------
 *     |||||||||||||||||||  Opcode: passed to GBA
 *     |||||||||||||||||||  L: If on, "Lock" the IOA; respond to CtlSpc only
 *     |||||||||||||||||||  Control: passed to GBA
 *    ----------------------------------------------
 *    | From GBA-map entry | From original address |     Gba address
 *    ----------------------------------------------
 *     31                14 13                    0
 *
 *
 ****************************************************************************/

typedef union {
  struct {
    Bit gba_address	:18;	/* MSB of address on GBA I/O bus */
    Bit op_code	:3;	/* op code to GBA */
    Bit lock		:1;	/* "lock" IOA for read-modify-write */
    Bit control	:10;	/* control bits to GBA (i.e. AM) */
  } bits;
  
  unsigned long word;
} gba_map_entry_type;

/****************************************************************************
 * System map
 *
 *    GBA0 System map entries:  0x2000-0x2FFF in IOC Control Space
 *    GBA1 System map entries:  0x3000-0x3FFF in IOC Control Space
 *
 *    Simple address mapping of GBA addresses to 16K pages on system bus.
 *
 *                        31    24 23      14 13      0
 *                        -----------------------------
 *    original address:   |       |XXXXXXXXXX|        |
 *                        -----------------------------
 *                                 ||||||||||
 *    local memory        ------------------------
 *             address:   |0 0|GBA|XXXXXXXXXX|0 0|
 *                        ------------------------
 *                        14 13 12 11       2 1 0
 * 
 *    Bits 10:31 of the the local memory location thus addressed contain
 *    the physical page number (system address bits 14:35):
 * 
 *                        31                10 9      0
 *                        -----------------------------
 *           map entry:   | physical page num |   0   | 
 *                        -----------------------------
 *
 ****************************************************************************/

typedef union {
  struct {
    Bit pp_num	:22;	/* physical page number on system bus */
    Bit reserved	:10;	/* must be ZERO */
  } bits;
    
  unsigned long word;
} system_map_entry_type;


/****************************************************************************
 * GBA interrupts
 *
 *     These are usually interrupts that come from the standard bus.  Exact
 *     use will be dependant on GBA implementation.
 *
 *     SBC command word 1:  0xjCi0 in IOC Control Space
 *     SBC command word 2:  0xjCi4 in IOC Control Space
 *     SBC data word:       0xjCi8 in IOC Control Space
 *                          Where i == interrupt number (0-f).
 *                                j == 4 (GBA0) or 5 (GBA1)
 *
 *     When GBA0 generates interrupt <i>, a single word write operation is
 *     performed on the Excalibur bus using the three words starting at
 *     0x4ci0 in IOC local memory.  For example, GBA0 interrupt 3 would
 *     perform a write using the three words at 0x4c30, 0x4c34, and 0x4c38.
 *     GBA1 interrupt words start at 0x5C00, and a GBA1 interrupt 3 would use
 *     the three words at 0x5c30, 0x5c34, and 0x5c38.  Normally, these three
 *     words will be the two bus chip (SBC) command words and one data word
 *     necessary to do a single word write into a CPU's interrupt register.
 *
 * LongOp interrupts
 *
 *     There are 16 LongOp interrupts, which are similar to the GBA
 *     interrupts.  These are usually interrupts from the GBA itself,
 *     resulting from the completion of some previously started non-trivial
 *     GBA operation.  A LongOp interrupt from a GBA sets a specified bit in
 *     the appropriate LongOp register and performs a single word write
 *     operation on the system bus using the two SBC command words and one
 *     data word contained in the appropriate LongOpInterrupt register.
 *
 *     Exact use and meaning will be dependant on GBA implementation.
 *
 *   a) LongOpInterrupt registers
 *
 *      SBC command word 1:  0xj4i0 in IOC Control Space
 *      SBC command word 2:  0xj4i4 in IOC Control Space
 *      SBC data word:       0xj4i8 in IOC Control Space
 *                           Where i == interrupt number (0-f).
 *                                 j == 4 (GBA0) or 5 (GBA1)
 *
 *      A set of 3 words for each of 16 LongOp interrupts.  The 3 words
 *      should be the SBC command and data words necessary to perform a
 *      single-word write on the system bus, normally to a CPU's interrupt
 *      register.
 *
 *      For example, GBA0 LongOp interrupt 9 would perform an Excalibur bus
 *      operation using the three words beginning at 0x4490 in IOC control
 *      space.  GBA1 LongOp interrupt d (13 decimal) would use the three
 *      words beginning at 0x54d0.
 *
 *   b) LongOp registers
 *
 *      write/read:  0xj0i0 in IOC Control Space
 *      AND (write): 0xj0i4 in IOC Control Space
 *      OR (write):  0xj0i8 in IOC Control Space
 *                   Where i == register number (0-f)
 *                         j == 4 (GBA0) or 5 (GBA1)
 *
 *      Sixteen registers, each 32 bits wide, for 16 different LongOp
 *      interrupts.  When a GBA gives a LongOpInterrupt to the IOC, it
 *      specifies which bit to set in the LongOp register; thus, each class
 *      of LongOps can have 32 sub-operations.
 *
 *      Software can manipulate these registers in three ways (direct write,
 *      AND, OR) depending upon which address is used for the write.  For
 *      example, to write the entire GBA0 LongOp 4 register, use offset
 *      0x4040 in IOC control space as the address.  To turn on bit 7 in GBA1
 *      LongOp register 3, write 0x80 to offset 0x5038 (current contents of
 *      register is ORed with 0x80).  To turn off bit 0 in GBA1 LongOp
 *      register 7, write 0xfffffffe to offset 0x5074 (current contents of
 *      register is ANDed with 0xfffffffe).
 *
 *      Reads from the OR & AND addresses will simply return the contents of
 *      the register.
 *
 *****************************************************************************/

typedef struct {
  struct {
    unsigned long wr_reg;	/* normal write/read of register */
    unsigned long and_reg;	/* write data is ANDed with register */
    unsigned long or_reg;	/* write data is ORed with register */
    unsigned long lop_reserved;
  }				long_op[16];
  unsigned long	reserved1[192];
  
  struct {
    unsigned long lop_done_sbc_cmd1;
    unsigned long lop_done_sbc_cmd2;
    unsigned long lop_done_sbc_data;
    unsigned long lop_done_reserved;
  }				long_op_done[16];
  unsigned long	reserved2[448];
  
  struct {
    unsigned long int_sbc_cmd1;
    unsigned long int_sbc_cmd2;
    unsigned long int_sbc_data;
    unsigned long int_reserved;
  }				interrupt[16];
  unsigned long	reserved3[192];
  
} gba_int_ctl_type;

/****************************************************************************
 * DUART interrupt
 *
 *     SBC command word 1:  0x7000 in IOC Control Space
 *     SBC command word 2:  0x7004 in IOC Control Space
 *     SBC data word:       0x7008 in IOC Control Space
 *
 *     When DUART chip asserts interrupt, a bus operation is performed using
 *     the three words starting at 0x7000 in IOC local memory.  Normally,
 *     these three words will be the two bus chip (SBC) command words and one
 *     data word to do a single word write into a CPU's interruptVectorSet
 *     register.
 *
 *   a) SBC command word formats
 *
 *       31                 14 13             0
 *      ----------------------------------------
 *      |      reserved       | PhysAdr[13..0] |    SBC command word 1
 *      ----------------------------------------
 *
 *       31                 14 13             0
 *      ----------------------------------------
 *      |   PhysAdr[35..14]   |    reserved    |    SBC command word 1
 *      ----------------------------------------
 *
 *       31                                   0
 *      ----------------------------------------
 *      |            Data[31..0]               |    SBC data word
 *      ----------------------------------------
 *
 ****************************************************************************/

typedef struct duart_int_type {
  unsigned long sbc_cmd1;
  unsigned long sbc_cmd2;
  unsigned long sbc_data;
  unsigned long	reserved[1021];
} duart_int_type;

/****************************************************************************
 ****************************************************************************/

typedef struct tod_type {		/* UNDEFINED */
  unsigned long	reserved[1024];
} tod_type;

/************************************************************************
 * The dc_line_info is returned whenever you READ one of the following
 * ioa registers: dirty_bits_reg, tag_format_reg, sect_control_reg.
 *
 * These registers have different formats when WRITTEN.
 *
 * This info is a concatenation of the bits which can be written from the
 * above named registers.
 *
 *   31   30   29  28  27  26     16 15 12 11  10   9   8   7   4 3     0
 * -----------------------------------------------------------------------
 * | WV | RV | D | PF | B | group # | ... | U | L | R | NP | ... | dirty |
 * -----------------------------------------------------------------------
 *
 ************************************************************************/
  
typedef union {
  struct {
    /* tag info */
    Bit read_valid	:1;	/* Cache line contains valid read data */
    Bit write_valid	:1;	/* Cache line contains valid write data */
    Bit dirty		:1;	/* Line has bytes written by GBA device */
    Bit already_pf	:1;	/* GBA has prefetched next cache line */
    Bit busy		:1;	/* GBA busy */
    /* tag */    
    Bit group_num	:11;	/* Bits 18 thru 8 of data_address_reg */
    Bit 		:4;
    /* cache section info */
    Bit uncached	:1;	/* Cache section has caching disabled */
    Bit local		:1;	/* Section VME address maps to IOC memory */
    Bit random		:1;	/* Section fetch-ahead, map-behind disabled */
    Bit no_partial	:1;	/* Section writes word, not just dirty bytes*/
    Bit 		:4;
    /* cache word info */    
    Bit dirty_byte_mask	:4;	/* Indicates dirty bytes at VME address */
  } bits;
  
  struct {
    Bit tag_info	:5;	/* All tag info fields as a group */
    Bit group_num	:11;	/* Group Number of GBA address */
    Bit			:4;
    Bit sect_info	:4;	/* All Cache Section info as a group */
    Bit			:4;
    Bit dirty_bits	:4;	/* All Dirty Bits as a group */
  } bit_groups;
  
  unsigned long word;		/* Alternate definition as 32 bit word */
} dc_line_info_type;

/************************************************************************
 * Writing the dirty_bits_reg will write the four dirty bits for the
 * four bytes in the GBA data cache corresponding to the VME address
 * last written into the data_address_reg.
 *
 * Reading the dirty_bits_reg returns the dc_line_info for the cache
 * line corresponding to the VME address in the data_address_reg.
 ************************************************************************/

typedef union {
  struct {
    Bit			:28;
    Bit dirty_bits	:4;
  } write_reg;

  dc_line_info_type read_reg;

  unsigned long word;
} dirty_bits_reg;
   
/************************************************************************
 * Writing the tag_format_reg will write the four tag bits for the
 * cache line in the GBA data cache corresponding to the VME address
 * last written into the data_address_reg.  Mainly used for diagnostic
 * purposes.
 *
 * Reading the tag_format_reg returns the dc_line_info for the cache
 * line corresponding to the VME address in the data_address_reg.
 ************************************************************************/

typedef union {
  struct {
    Bit			:27;
    Bit read_valid	:1;	/* Cache line contains valid read data */
    Bit write_valid	:1;	/* Cache line contains valid write data */
    Bit dirty		:1;	/* Line has bytes written by GBA device */
    Bit already_pf	:1;	/* GBA has prefetched next cache line */
    Bit busy		:1;	/* GBA busy -- DO NOT MODIFY THIS BIT! */
  } write_reg;

  dc_line_info_type read_reg;

  unsigned long word;
} tag_format_reg;
   
/************************************************************************
 * Writing the sect_control_reg will write the four section bits for
 * the cache section in the GBA data cache corresponding to the VME
 * address last written into the data_address_reg.
 *
 * Reading the sect_control_reg returns the dc_line_info for the cache
 * line corresponding to the VME address in the data_address_reg.
 *
 * Cacheable - If on, enables the cache.  Any attempt by a controller
 * to read a main memory location will cause a 128-byte block to be
 * read.  Writes by a controller will not be written to main memory
 * until the last byte of the Gba-cache line is written, at which point
 * the entire block will be written.  If the Cacheable bit is off, all
 * read and writes by a controller will generate single-word operations
 * on the system bus, bypassing the Gba-cache completely.
 *
 * Random - If on, disables fetch-ahead and write-behind.  Useful when
 * a controller is known to perform non-sequential accesses.  Disables
 * writing back to main memory when the last byte of a Gba-cache line
 * is written by VMEbus controller.
 *
 * Local - If on, forces all accesses from the VMEbus to go to IOC
 * local memory instead of being mapped to main memory.  This could be
 * used by a processor on the VMEbus to write the Gba and system maps,
 * for example.  Reset turns on the Local bit for section 0.  Note that
 * the Cacheable bit should be off for any section having the Local bit
 * set.
 *
 * NoPartial (aka AllowPartial) - If on, partial words in a block are
 * written back as if the entire word is dirty.  If off, partially
 * dirty words are disabled from being written to main memory in block
 * write operations.  After the block write, the IOC will write all the
 * partial words in the block using single word writes and the bytemask
 * to write only the dirty bytes.
 *
 ************************************************************************/

typedef union {
  struct {
    Bit			:28;
    Bit non_cacheable	:1;
    Bit random		:1;
    Bit local_to_vme	:1;
    Bit no_partial_wrt	:1;
  } write_reg;

  dc_line_info_type read_reg;

  unsigned long word;
} sect_control_reg;

/************************************************************************
 *   VME Address Compare Register:  0xA00 in GBA Control Space (read/write)
 *
 *   This register, written as a full word, contains four 8-bit registers
 *   used to define the VMEbus addresses to which the GBA will respond.
 *   These byte-wide registers are loaded with bits 31:24 of the VMEaddress.
 *   Bits 23:22 of the VMEbus address are fixed for each of the four 4MB
 *   sections.
 *
 *    31  24 23  16 15   8 7    0
 *   -----------------------------
 *   | vvvv | xxxx | yyyy | zzzz |   VME Address Compare Register
 *   -----------------------------
 *    |||||| |||||| ||||||  |||||
 *    |||||| |||||| |||||| ----------------------------
 *    |||||| |||||| |||||| | zzzz | 1 1 | nnnnnnnnnnn |  VMEbus address
 *    |||||| |||||| |||||| ----------------------------
 *    |||||| |||||| |||||| 31   24 23 22 21          0
 *    |||||| |||||| ||||||
 *    |||||| |||||| ----------------------------
 *    |||||| |||||| | yyyy | 1 0 | nnnnnnnnnnn |  VMEbus address
 *    |||||| |||||| ----------------------------
 *    |||||| |||||| 31   24 23 22 21          0
 *    |||||| ||||||
 *    |||||| ----------------------------
 *    |||||| | xxxx | 0 1 | nnnnnnnnnnn |  VMEbus address
 *    |||||| ----------------------------
 *    |||||| 31   24 23 22 21          0
 *    ||||||
 *    ----------------------------
 *    | vvvv | 0 0 | nnnnnnnnnnn |  VMEbus address
 *    ----------------------------
 *    31   24 23 22 21          0
 *
 ************************************************************************/

typedef union {
  struct {
    Bit vme_addr1	:8; /* 8 MSB of xxxx xxxx 00nn (+20 more bits) */
    Bit vme_addr2	:8; /* 8 MSB of xxxx xxxx 01nn (+20 more...) */
    Bit vme_addr3	:8; /* 8 MSB of xxxx xxxx 10nn (+20 more...) */
    Bit vme_addr4	:8; /* 8 MSB of xxxx xxxx 11nn (+20 more...) */
  } bits;

  unsigned long word;
} vme_addr_comp_reg;

/************************************************************************/
/************************************************************************/


typedef struct gba_misc_ctl_type {
    unsigned long		gba_type;	/* UNDEFINED */
    unsigned long reserved1[63];
    unsigned long		gba_status;	/* UNDEFINED */
    unsigned long reserved2[63];
    /******************************************************************
     * A 24 bit VME address is written into data_address_reg.  Then
     * information relating to that word/cache-line/cache-section can
     * be read/written to tag_format, sect_control, dirty_bits, and
     * and dc_data registers.
     ******************************************************************/
    
    unsigned long		data_address_reg;	/* WRITE-ONLY */
    unsigned long reserved3[127];
    tag_format_reg		tag_format;
    unsigned long reserved4[63];
    sect_control_reg		sect_control;
    unsigned long reserved5[63];
    dirty_bits_reg		dirty_bits;
    unsigned long reserved6[63];
    unsigned long		dc_data;	/* Access data in GBA cache */
    						/* at data_address_reg^	    */
    unsigned long reserved7[63];
    
    /******************************************************************
     * vme_int_pending has one bit for each interrupt level on VME bus.
     * Bit 1 for level IRQ1, bit 2 for IRQ2, ..., bit 7 for IRQ 7.
     ******************************************************************/
    
    unsigned long		vme_int_pending;
    unsigned long reserved8[63];
    
    /******************************************************************
     * vme_ack contains a word location for each interrupt level.  A
     * READ from that level's ack location will cause an interrupt
     * acknowledge on the VME bus and return the interrupt vector number.
     ******************************************************************/
    
    unsigned long		vme_ack[8];	/* 0, 1=IRQ1,...,7=IRQ7 ack */
    unsigned long reserved9[56];
    
    /******************************************************************
     * vme_addr_comp defines which addresses on the GBA bus the GBA will
     * respond to.  See definition of vme_addr_comp_reg for details.
     ******************************************************************/
    
    vme_addr_comp_reg		vme_addr_comp;
    unsigned long reserved10[63];
    
    /******************************************************************
     ******************************************************************/
    unsigned long		vme_rmw_unlock;	/* write the rmw write data */
    unsigned long reserved11[63];
    
    /******************************************************************
     * Write the "Prefetch Register" with the low-order 24 bits of the VME
     * address corresponding to the line to be prefetched.  This address is
     * passed to the IOC and mapped to an Excalibur system address in the
     * usual fashion.  When the prefetch operation is complete, LongOp
     * interrupt 1 will be generated to the IOC, specifying that the bit
     * corresponding to the section number that contained the line be set in
     * LongOp register 1.  Note that prefetching operates on a single
     * GBA-cache line, not a 2-line section.
     ******************************************************************/
    
    unsigned long		prefetch_reg;	/* write VME addr to fetch */
    unsigned long reserved12[63];
    
    /******************************************************************
     * Write "Flush Register" with the VMEbus address corresponding to the
     * section to be flushed.  When the flush operation is complete, LongOp
     * interrupt 0 will be generated to the IOC, specifying that the bit
     * corresponding to the section number be set in LongOp register 0.
     ******************************************************************/
    
    unsigned long		flush_reg;	/* write VME addr to flush */
    unsigned long reserved13[63];
    
    /******************************************************************
     ******************************************************************/
    unsigned long		block_start;	/* UNDEFINED */
    unsigned long reserved14[63];
    /******************************************************************
     ******************************************************************/
    unsigned long		block_info;	/* UNDEFINED */
    unsigned long reserved15[63];
} gba_misc_ctl_type;


typedef union  {
  struct {
    gba_map_entry_type		gba_maps[2][1024];		/* 0x0000 */
    system_map_entry_type	gba_smaps[2][1024];		/* 0x2000 */
    gba_int_ctl_type		gba_ints[2];			/* 0x4000 */
    unsigned long		reserved12[1024];		/* 0x6000 */
    duart_int_type		duart_int;			/* 0x7000 */
    gba_misc_ctl_type		gba_misc_ctl[2];		/* 0x8000 */
    unsigned long		reserved[1024];			/* 0xA000 */
    unsigned long		reserved2[1024];		/* 0xB000 */
    tod_type			tod;				/* 0xC000 */
    unsigned long		reserved3[1024];		/* 0xD000 */

    /* ioc_misc_ctl */
    unsigned long		led_reg;			/* 0xE000 */
    unsigned long reserved4[127];
    unsigned long 		duart_reg[1];	/* UNDEFINED  0xE200 */
    unsigned long reserved5[127];
    unsigned long		status_reg0;	/* status reg, GBA0 0xE400 */
    unsigned long reserved6[127];
    unsigned long		status_reg1;	/* status reg, GBA1 0xE600 */
    unsigned long reserved7[127];
    unsigned long reserved8[128];			/* (0xE800) */
    unsigned long		error_info;	/* error info reg (0xEA00) */
    unsigned long reserved9[127];
    unsigned long	 	shadow_addr0;	/* shadow que adr GBA0 0xEC00*/
    unsigned long reserved10[127];
    unsigned long	 	shadow_addr1;	/* shadow que adr GBA1 0xEE00*/
    unsigned long reserved11[127];
    
    /* sbc_misc_ctl */					/* 0xF000 */
    unsigned long		sbc_int_vec_set;
    unsigned long		sbc_int_vec_clr;
    unsigned long		sbc_int_mask;
    unsigned long		sbc_board_addr;
    unsigned long		sbc_ctl_misc;
    unsigned long		sbc_scan_data;
    unsigned long		sbc_mem_ecc;

    /*********************************************************************
     * The following FATAL INTERRUPT is used for IOA internal parity
     * errors, deadlocks, etc.  IOC asserts SendInt pin on SBC, causing
     * SBC to send the interrupt defined in its MemCtl register.
     *********************************************************************/
    union {
      struct {
	Bit 		:16;
	Bit int_enable	:1;		/* interrupt enable */
	Bit int_slot	:8;		/* slot number for interrupt */
	Bit int_bit	:2;		/* bit number for interrupt */
	Bit 		:5;
      } bits;
      
      unsigned long word;		/* Alternate define as 32 bit word */
    } sbc_mem_ctl;

    unsigned long	reserved13[1016];
  } mem;

unsigned long word[16384];	/* Alternate definition of ioa_dev_type */

} ioa_dev_type;

#endif LOCORE

#endif	_SYS_IOA_
