/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: rambo.h,v 1.3 90/03/22 18:29:50 chungc Exp $"

/*
 * required data structures and register defines for the Pizazz
 * RAMBO dma controller/timer asic
 */

union ram_short_st {
	u_long	rambo_long;
	struct	rambo_s {
		u_short	pad_short;
		u_short	rambo_short;
	} rambo_s;
};

struct rambo_chan {
	u_long 	dma_laddr;	/* 0 load memory address register (R/W) */
	u_char 	pad0[0xfc];
	u_long	dma_diag;	/* 100 hardware register (R) */
	u_char 	pad1[0xfc];
	union ram_short_st dma_fifo_st;	/* 202 fifo (R/W) */
	u_char 	pad2[0xfc];
	u_long	dma_mode;	/* 300 mode register (R/W) */
	u_char 	pad3[0xfc];
	union ram_short_st dma_block_st; /* 402 block count register (R/W) */
	u_char 	pad4[0xfc];
	u_long	dma_caddr;	/* 500 current address register (R) */
	u_char 	pad5[0xfc];
};

#define	dma_fifo_s	dma_fifo_st.rambo_s.rambo_short
#define	dma_fifo_l	dma_fifo_st.rambo_long

#define	dma_block_s	dma_block_st.rambo_s.rambo_short
#define	dma_block_l	dma_block_st.rambo_long

/*
 * offsets - the structure below implemets these offsets
 */

#define	RAMBO_TCOUNT	0xc00
#define	RAMBO_TBREAK	0xd00
#define	RAMBO_EREG	0xe00
#define	RAMBO_CREG	0xf00

/*
 * The monochrome buffer that the prom uses
 */

#define	PROM_RAMBO_BUFFER	(prom_rambo_buffer)
extern	unsigned long		*prom_rambo_buffer;

struct rambo {
	struct rambo_chan	rambo_ch[2];
	u_long	rambo_tcount;	/* current clock value */
	u_char	pad0[0xfc];
	u_long	rambo_tbreak;	/* next target clock value */
	u_char	pad1[0xfc];
	u_long	rambo_ereg;	/* error register */
	u_char	pad2[0xfc];
	u_long	rambo_creg;	/* control register */
};

#define	dma_laddr_1	rambo_ch[0].dma_laddr
#define	dma_diag_1	rambo_ch[0].dma_diag
#define	dma_fifo_1	rambo_ch[0].dma_fifo_s
#define	dma_mode_1	rambo_ch[0].dma_mode
#define	dma_block_1	rambo_ch[0].dma_block_s
#define	dma_caddr_1	rambo_ch[0].dma_caddr

/*
 * Mode register defines
 * these bits are read/write
 */
#define FLUSH_RAMBO_FIFO 0x80000000	/* clears fifo */
#define CHANNEL_EN	0x40000000	/* enable dma channel */
#define AUTO_RELOAD	0x20000000	/* reload src/dest address */
#define INTR_EN		0x10000000	/* enable dma interrupt */
#define TO_MEMORY	0x08000000	/* direction (0=>scsi write) */
#define CLR_DMA_ERR	0x04000000	/* clear dma error */
/*
 * these bits are read only
 */
#define FIFO_FULL	0x00000800	/* fifo full state */
#define FIFO_EMPTY	0x00000400	/* fifo empty state */
#define DMA_ERROR	0x00000200	/* parity error during transfer */
#define DMA_INTR	0x00000100	/* channel interrupt pending */
#define COUNT7		0x00000080	/* halfword count bit 7 */
#define COUNT6		0x00000040	/* halfword count bit 6 */
#define COUNT5		0x00000020	/* halfword count bit 5 */
#define COUNT4		0x00000010	/* halfword count bit 4 */
#define COUNT3		0x00000008	/* halfword count bit 3 */
#define COUNT2		0x00000004	/* halfword count bit 2 */
#define COUNT1		0x00000002	/* halfword count bit 1 */
#define COUNT0		0x00000001	/* halfword count bit 0 */

#define COUNT_MSK	0xff		/* mask off count bits  */
#define BLOCK_SHIFT	6		/* bits to shift for bytes<->blocks */
#define BLOCK_HW_CNT 	32		/* half-word count for block dma */

#define	BUZZMASK	0x30		/* mask of buzzer bits */
#define	BUZZ0		0x00		/* 1524Hz tone */
#define	BUZZ1		0x10		/*  726Hz tone */
#define	BUZZ2		0x20		/*  381Hz tone */
#define	BUZZ3		0x30		/*  190Hz tone */
#define BUZZON		0x08		/* buzzer on bit */
#define	ENABLEPARITY	0x04		/* enable parity checking */
#define	RESETERROR	0x02		/* reset error latch of EReg */
#define	CLRERRINT	0x01		/* clear error interrupt */
