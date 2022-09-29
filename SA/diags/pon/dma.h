#ident "$Header: dma.h,v 1.1.7.1 90/07/18 14:28:41 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/***********************************************************************
 *
 *	Name : dma.h
 *
 *	Description : Defines for the M20 DMA Controller
 *
 *
 ***********************************************************************/


/*
 * DMA registers
 */

/* Channel 1, with 24 word FIFO - 96 bytes
 */
#define DMA_LADDR1	RAMBO_BASE+0x000	/* DMA Channel 1 Load address register	 */
#define DMA_DIAG1	RAMBO_BASE+0x100	/* DMA Channel 1 Diagnostic register	 */
#define DMA_FIFO1	RAMBO_BASE+0x200	/* DMA Channel 1 FIFO Data register	 */
#define DMA_MODE1	RAMBO_BASE+0x300	/* DMA Channel 1 Mode register		 */
#define DMA_BLKCNT1	RAMBO_BASE+0x400	/* DMA Channel 1 Block Count register	 */
#define DMA_CADDR1	RAMBO_BASE+0x500	/* DMA Channel 1 Current Address register*/
#define DMA1_BASE	RAMBO_BASE+0x000	/* DMA Channel 1 base address		 */


/* Channel 2, with 48 word FIFO - 192 bytes
 */
#define DMA_LADDR2	RAMBO_BASE+0x600	/* DMA Channel 2 Load address register	 */
#define DMA_DIAG2	RAMBO_BASE+0x700	/* DMA Channel 2 Diagnostic register	 */
#define DMA_FIFO2	RAMBO_BASE+0x800	/* DMA Channel 2 FIFO Data register	 */
#define DMA_MODE2	RAMBO_BASE+0x900	/* DMA Channel 2 Mode register		 */
#define DMA_BLKCNT2	RAMBO_BASE+0xa00	/* DMA Channel 2 Block Count register	 */
#define DMA_CADDR2	RAMBO_BASE+0xb00	/* DMA Channel 2 Current Address register*/
#define DMA2_BASE	RAMBO_BASE+0x600	/* DMA Channel 2 base address		 */


/* Counter/Timer - clock interrupt
 */
#define TIMER_BASE	RAMBO_BASE+0xc00	/* Time/Counter register base address	 */


/* DMA MODE register
 */
#define DMA_CNTRL_MASK	0xffff0000	/* control portion of mode register	 */
#define DMA_STATUS_MASK	0x0000ffff	/* status portion of mode register	 */
#define DMA_FIFO_CNT	0x000000ff	/* # of halfwords in the FIFO		 */
#define DMA_INTR	0x00000100	/* interrupt pending bit		 */
#define DMA_PAR_ERR	0x00000200	/* DMA parity Error bit			 */
#define DMA_FIFO_EMPTY	0x00000400	/* DMA FIFO EMPTY bit			 */
#define DMA_FIFO_FULL	0x00000800	/* DMA FIFO FULL bit			 */
#define DMA_PURGE_FIFO	0x80000000	/* purge( flush ) FIFO bit		 */
#define DMA_ENA_CHNL	0x40000000	/* Enable channel bit			 */
#define DMA_AUTO_RELOAD	0x20000000	/* Auto-reload bit			 */
#define DMA_ENA_INTR	0x10000000	/* Enable interrupt bit			 */
#define DMA_TO_MEMORY	0x08000000	/* transfer direction bit		 */
#define DMA_CLR_ERR	0x04000000	/* clear DMA parity error bit		 */

/* LA_ADDR register
 */
#define	DMA_LA_MASK	0x0fffffc0	/* DMA controller can only address data	 */
					/* 16-word boundry			 */

/* BLKCNT register
 */
#define	DMA_BLKCNT_MASK	0x1fff		/* valid bits in block count register	 */

#define DMA_BLK_HSIZE	0x00000020	/* number of halfwords in a DMA Burst	  */
#define DMA_BLK_WSIZE	0x00000010	/* number of words in a DMA Burst	  */
#define DMA_BLK_BSIZE	0x00000040	/* number of bytes in a DMA Burst	  */
#define MAX_CNT_CH1	48		/* size of channel 1 FIFO (# of halfwords )*/
#define MAX_CNT_CH2	96		/* size of channel 2 FIFO (# of halfwords )*/


#ifdef	LANGUAGE_C
/*
 * DMA registers
 */
struct	dma {
		volatile unsigned long	laddr;
		unsigned char	buf0[0x100-4];
		volatile unsigned long	diag;
		unsigned char	buf1[0x100-4];
		volatile unsigned long	fifo;
		unsigned char	buf2[0x100-4];
		volatile unsigned long	mode;
		unsigned char	buf3[0x100-4];
		volatile unsigned long	blkcnt;
		unsigned char	buf4[0x100-4];
		volatile unsigned long	caddr;
	    };

/*
 * system registers
 */

struct system {
		volatile unsigned long	ereg;
		unsigned char	buf0[0x100-4];
		volatile unsigned long	creg;
		 };

struct cnt_timer {
		volatile unsigned long	tcount;
		unsigned char	buf0[0x100-4];
		volatile unsigned long	tbreak;
		 };
#endif	LANGUAGE_C
