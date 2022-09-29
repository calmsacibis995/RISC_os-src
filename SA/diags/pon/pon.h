#ident "$Header: pon.h,v 1.11.1.1 90/07/18 14:28:59 huang Exp $"
/* $Copyright$ */

#define	PASS			0
#define	FAIL			1

#ifndef	TRUE
#define	TRUE			1
#endif	TRUE

#ifndef	FALSE
#define	FALSE			0
#endif	FALSE

#define	BAD			-1
#define	OK			0

#define	CHAR_d			0x64
#define	CHAR_e			0x65

#define	CHAR_0			0x30
#define	CHAR_1			0x31
#define	CHAR_t			0x74
#define	CHAR_m			0x6d
#define	CHAR_c			0x63
#define	CHAR_l			0x6c
#define	CHAR_g			0x67
#define	CHAR_r			0x72
#define	CHAR_a			0x61
#define	CHAR_v			0x76

/*
 * LED defines for PON tests
 */
#define	PON_DUART1A		0x03
#define	PON_CACHE1		0x05
#define	PON_CACHE2		0x06
#define	PON_CACHE3		0x07
#define	PON_CACHE4		0x09
#define	PON_IDPROM		0x0a
#define	PON_WB			0x0b
#define	PON_MEMTEST		0x0c
#define	PON_SCR			0x41
#define	PON_TLB			0x0d
#define	PON_ALLEXC		0x0e
#define	PON_PARITY		0x42		/* M120 only */
#define	PON_NVRAM		0x0f
#define	PON_TIMER		0x11
#define	PON_TOD			0x12
#define	PON_DUART1B		0x13
#define	PON_DUART2A		0x14
#define	PON_DUART2B		0x15
#define	PON_IMR			0x43		/* M120 only */
#define	PON_FP1			0x16
#define	PON_FP2			0x17
#define	PON_ECC			0x18		/* M2000 only */
#define	PON_DBLK		0x19		/* M2000 only */
#define	PON_IBLK		0x1a		/* M2000 only */
#define	PON_VME			0x1b		/* M2000 only */
#define	PON_UDCSLAVE		0x44		/* M120 only */
#define	PON_CHAIN1		0x45		/* M120 only */
#define	PON_CHAIN2		0x46		/* M120 only */
#define	PON_SCSISLAVE		0x47		/* M120 only */
#define	PON_SCSIMASTER		0x48		/* M120 only */
#define	PON_ENETPROM		0x49		/* M120 only */
#define	PON_LANCESLAVE		0x4a		/* M120 only */
#define	PON_LANCEMASTER		0x4b		/* M120 only */
#define	PON_ATREG		0x4c		/* M120 only */
#define	PON_LOMEM		0x4d		/* Low memory test */
#define PON_LANCE_BUFF		0x4e		/* Lance buffer test, RB3125 only */
#define PON_LANCE_BUFF_PARITY	0x4f		/* Lance buffer parity test, RB3125 only */

#define	PON_FAULT_PATTERN	0x1f		/* unexpected exception occurred */

/*
 * Memory scratch area
 */
#define	PON_SCRATCHMEM		0x200000
#define	PON_MEMTESTED		0x500
#define	PON_LOMEMEND		0x8000

/*
 * NVRAM codes:
 */
/*
 * First group is common to all systems.
 */
#define	PON_FAULT_AT		(1 << 0)	/* AT register or board is bad */
#define	PON_FAULT_DCACHE	(1 << 1)	/* data cache is bad */
#define	PON_FAULT_ICACHE	(1 << 2)	/* instruction cache is bad */
#define	PON_FAULT_CACHE		(PON_FAULT_DCACHE | PON_FAULT_ICACHE)
#define	PON_FAULT_CONSOLE	(1 << 3)	/* console is bad */
						/* not much can be done here */
#define	PON_FAULT_DUARTS	(1 << 4)	/* DUART (other than console) is bad */
#define	PON_FAULT_EXCEPT	(1 << 5)	/* exceptions is bad */
#define	PON_FAULT_FAULT		(1 << 6)	/* Fault ID and/or Fault Address Registers are bad */
#define	PON_FAULT_FP		(1 << 7)	/* FP is bad */
#define	PON_FAULT_IDPROM	(1 << 8)	/* ID PROM is bad */
						/*   not much can be done here */
#define	PON_FAULT_MEM		(1 << 9)	/* memory is bad */
#define	PON_FAULT_NVRAM		(1 << 10)	/* NVRAM is bad */
#define	PON_FAULT_SCR		(1 << 11)	/* System Configuration Register is bad */
#define	PON_FAULT_TIMER		(1 << 12)	/* timer is bad */
#define	PON_FAULT_TLB		(1 << 13)	/* TLB is bad */
#define	PON_FAULT_TOD		(1 << 14)	/* TOD clock is bad */

#ifndef	R3030
/*
 * Common to M120, M180 and Genesis systems.
 */
#define	PON_FAULT_ENETPROM	(1 << 15)	/* Ethernet PROM is bad */
#define	PON_FAULT_LANCE		(1 << 16)	/* LANCE is bad */
#define	PON_FAULT_SCSI		(1 << 17)	/* SCSI controller is bad */
#define	PON_FAULT_WB		(1 << 18)	/* write buffers are bad */
						/*   not much can be done here */

/*
 * Common to M120 and M180 systems.
 */
#define	PON_FAULT_IMR		(1 << 19)	/* Interrupt Mask Register is bad */
#define	PON_FAULT_UDC		(1 << 20)	/* UDC is bad */

/*
 * M2000 systems.
 */
#define	PON_FAULT_ECC		(1 << 21)	/* ECC checking is bad */
#define	PON_FAULT_VME		(1 << 22)	/* VME is bad */

/* Genesis systems.
*
*/
#define PON_FAULT_LNC_BUFF	(1 << 23)	/* Lance Buffer is bad */
#define PON_FAULT_LNC_BUFF_PAR	(1 << 24)	/* Lance Buffer Parity is bad */

#define	PON_DEPEND_MASK		~(0xffffffff << 23)
#else	!R3030

/*
 * M20 system.
 */
#define	PON_FAULT_COLORVIDEO	(1 << 15)	/* color frame buffer is bad */
#define	PON_FAULT_DMA_CHAN1	(1 << 16)	/* RAMBO DMA channel 1 is bad */
#define	PON_FAULT_DMA_CHAN2	(1 << 17)	/* RAMBO DMA channel 2 is bad */
#define	PON_FAULT_DMA_PARITY	(1 << 18)	/* RAMBO DMA parity checking is bad */
#define	PON_FAULT_FDC		(1 << 19)	/* 82072 FDC is bad */
#define	PON_FAULT_KEYBOARD	(1 << 20)	/* 8042 keyboard chip is bad */
#define	PON_FAULT_LANCE		(1 << 21)	/* LANCE is bad */
#define	PON_FAULT_SCSI		(1 << 22)	/* SCSI controller is bad */
#define	PON_FAULT_WB		(1 << 23)	/* write buffers are bad */

#define	PON_ENTERED		(1 << 0)	/* PON code entered */
#define	PON_EXCEPTION		(1 << 1)	/* unexpected exception occurred during PON */

#define	PON_TTY0		(1 << 2)	/* write to tty0 */
#define	PON_TTY1		(1 << 3)	/* write to tty1 */
#define	PON_KBD_NOT_PRESENT	(1 << 4)	/* keyboard is not detected */

#define	PON_DEPEND_MASK		~(0xffffffff << 24)
#endif	!R3030

/*
 * DUART test dependent
 */
#define	TBYTE_DATA		0x31
#define	VBYTE_DATA		0x31
#define	BYTE_CNT		20

/*
 * DUART registers offset independent of machine type
 */
#define	DOFF_MR			0x0		/* generic channel mode register (1 or 2) */
#define	DOFF_MRA		DOFF_MR		/* channel mode register (1 or 2) */
#define	DOFF_SR			0x04		/* generic channel status register (read) */
#define	DOFF_SRA		DOFF_SR		/* channel status register (read) */
#define	DOFF_CSR		DOFF_SR		/* generic Clock Select Register (A/B) */
#define	DOFF_CSRA		DOFF_SR		/* channel A Clock Select Register */
#define	DOFF_CR			0x08		/* generic channel command register */
#define	DOFF_CRA		DOFF_CR		/* channel command register */
#define	DOFF_RHR		0x0c		/* generic channel receiver holding register (read) */
#define	DOFF_RHRA		DOFF_RHR	/* channel receiver holding register (read) */
#define	DOFF_THR		DOFF_RHR	/* generic tX Holding Register (A/B) */
#define	DOFF_THRA		DOFF_RHR	/* generic tX Holding Register (A/B) */
#define	DOFF_IPCR		0x10		/* input port change register */
#define	DOFF_ACR		DOFF_IPCR	/* Aux Control Register */
#define	DOFF_ISR		0x14		/* interrupt status register */
#define	DOFF_IMR		DOFF_ISR	/* Interrupt Mask Register */
#define	DOFF_CTU		0x18		/* counter/timer upper */
#define	DOFF_CTUR		DOFF_CTU	/* C/T Upper Register */
#define	DOFF_CTL		0x1c		/* counter/timer lower */
#define	DOFF_CTLR		DOFF_CTL	/* C/T Lower Register */
#define	DOFF_MRB		0x20		/* channel B mode register (1 or 2) */
#define	DOFF_SRB		0x24		/* channel B status register (read) */
#define	DOFF_CSRB		DOFF_SRB	/* channel B Clock Select Register */
#define	DOFF_CRB		0x28		/* channel B command register */
#define	DOFF_RHRB		0x2c		/* channel B receiver holding register (read) */
#define	DOFF_THRB		DOFF_RHRB	/* channel B receiver holding register (read) */
#define	DOFF_IPORT		0x34		/* input port */
#define	DOFF_OPCR		DOFF_IPORT	/* Output Port Configuration Reg */
#define	DOFF_CCGO		0x38		/* start counter command */
#define	DOFF_SOPBC		DOFF_CCGO	/* Set Output Port Bits Command */
#define	DOFF_CCSTP		0x3c		/* stop counter command */
#define	DOFF_ROPBC		DOFF_CCSTP	/* Reset Output Port Bits Command */

/*
 * to determine the time constant:
 *      TC = ( (MHZ {the clock rate} / (16 * 2 * BR)) - 2 )
 *              for a 16x clock
 */
#define	MHZ     10000000        /* base pclock for the scc part */

#define	BCONST  16              /* use this if you wish to change the divisor*/

#define	B_CONST(speed)  ( (MHZ/(2*BCONST*speed)) - 2 )

/*
 * Pizazz (M20) defines.
 */
/*
 * This is the physical address map of M20
 */
#define	RAT_IO_BASE		0x10000000	/* base address of Restricted AT I/O Space */
#define	RAT_MEM_BASE		0x14000000	/* base address of Restricted AT Memory Space */

#define	NCR_BASE		0x18000000	/* base address of SCSI Protocol Controller */
#define	KBD_BASE		0x19000000	/* base address of Keyboard Controller */
#define	INT_REG			0x19800003	/* address of interrupt register */
#define	SCC_BASE		0x1b000000	/* base address of Serial Communication Controller */
#define	RAMBO_BASE		0x1c000000	/* base address of RAMBO DMA Controller */
#define	FDC_BASE		0x1e000000	/* base address of Floppy Disk Controller */

#define	SYS_EREG		RAMBO_BASE+0xe00	/* system error register */
#define	SYS_CREG		RAMBO_BASE+0xf00	/* system control register */

/*
 * RAMBI registers
 */

/*
 * Channel 1, with 24 word FIFO - 96 bytes
 */
#define	DMA1_BASE		RAMBO_BASE+0x000	/* DMA Channel 1 base address */
#define	DMA_LADDR1		RAMBO_BASE+0x000	/* DMA Channel 1 Load address register */
#define	DMA_DIAG1		RAMBO_BASE+0x100	/* DMA Channel 1 Diagnostic register */
#define	DMA_FIFO1		RAMBO_BASE+0x200	/* DMA Channel 1 FIFO Data register */
#define	DMA_MODE1		RAMBO_BASE+0x300	/* DMA Channel 1 Mode register */
#define	DMA_BLKCNT1		RAMBO_BASE+0x400	/* DMA Channel 1 Block Count register */
#define	DMA_CADDR1		RAMBO_BASE+0x500	/* DMA Channel 1 Current Address register */

/*
 * Channel 2, with 48 word FIFO - 192 bytes
 */
#define	DMA2_BASE		RAMBO_BASE+0x600	/* DMA Channel 2 base address */
#define	DMA_LADDR2		RAMBO_BASE+0x600	/* DMA Channel 2 Load address register */
#define	DMA_DIAG2		RAMBO_BASE+0x700	/* DMA Channel 2 Diagnostic register */
#define	DMA_FIFO2		RAMBO_BASE+0x800	/* DMA Channel 2 FIFO Data register */
#define	DMA_MODE2		RAMBO_BASE+0x900	/* DMA Channel 2 Mode register */
#define	DMA_BLKCNT2		RAMBO_BASE+0xa00	/* DMA Channel 2 Block Count register */
#define	DMA_CADDR2		RAMBO_BASE+0xb00	/* DMA Channel 2 Current Address register */

#ifdef	LANGUAGE_C
/*
 * DMA registers
 */
struct dma {
	volatile unsigned long laddr;
	unsigned char buf0[0x100-4];
	volatile unsigned long diag;
	unsigned char buf1[0x100-4];
	volatile unsigned long fifo;
	unsigned char buf2[0x100-4];
	volatile unsigned long mode;
	unsigned char buf3[0x100-4];
	volatile unsigned long blkcnt;
	unsigned char buf4[0x100-4];
	volatile unsigned long caddr;
};

struct system {
	volatile unsigned long ereg;
	unsigned char buf0[0x100-4];
	volatile unsigned long creg;
};
#endif	LANGUAGE_C

#define	TIMER_BASE		RAMBO_BASE+0xc00	/* timer/counter register base address */

#ifdef	LANGUAGE_C
struct cnt_timer {
	volatile unsigned long tcount;
	unsigned char buf0[0x100-4];
	volatile unsigned long tbreak;
};

/*
 * NCR 53c94 registers.
 */
struct scsi {
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
#endif	LANGUAGE_C

/*
 * SCC offsets.
 */
#define	SCC_PTRB		0
#define	SCC_DATAB		4
#define	SCC_PTRA		8
#define	SCC_DATAA		12

/*
 * System error register bit definition
 */
#define	ER_ADDR_MASK		0x0fffffc0	/* address of the location which caused parity error */
#define	ER_PAR_ERR_MASK		0x0000000f	/* parity error byte lane indentifier bits */
#define	ER_PARERR3		0x0001		/* identifies byte 3, bits(7-0) */
#define	ER_PARERR2		0x0002		/* identifies byte 2, bits(15-8) */
#define	ER_PARERR1		0x0004		/* identifies byte 1, bits(23-16) */
#define	ER_PARERR0		0x0008		/* identifies byte 0, bits(31-24) */

/*
 * System control register bit definition
 */
#define	CR_CLR_PAR_INT_B	0x0001		/* clear parity interrupt */
#define	CR_CLR_ERR_REG		0x0002		/* clear the byte identifier bit sin error reg.
						   this bit should be set to 0 after it is set to 1 */
#define	CR_ENA_PAR_CHK		0x0004		/* enables parity checking */
#define	CR_ENA_BUZZER_B		0x0008		/* enable buzzer bit */
#define	CR_BUZZ_L		0x0010		/* buzzer frequency control bit */
#define	CR_BUZZ_H		0x0020		/* buzzer frequency control bit */

/*
 * System interrupt register bit definition
 */
#define	IR_SLOT_INT_B		0x01		/* 0 = interrupt from RAT or video frame buffer	*/
#define	IR_KBD_INT_B		0x02		/* 0 = interrupt from Keyboard controller	*/
#define	IR_SCC_INT_B		0x04		/* 0 = interrupt from Serial Communication controller */
#define	IR_NCR_INT_B		0x08		/* 0 = interrupt from SCSI controller */
#define	IR_NET_INT_B		0x10		/* 0 = interrupt from LANCE */
#define	IR_MASK			0x1f		/* mask for valid bits of interrupt register*/

/*
 * RAMDAC registers.
 */
#define	RAMDAC_ADRLO		0x14000000	/* address low, R/W word, only bits 7-0 are valid */
#define	RAMDAC_ADRHI		0x14080000	/* address high, R/W word, only bits 7-0 are valid */
#define	RAMDAC_CNTRL		0x14100000	/* control register, R/W word, only bits 7-0 are valid */
#define	RAMDAC_PALET		0x14180000	/* palette RAM, R/W word, only bits 7-0 are valid */
