#ident "$Header: dma_2000.h,v 1.2.1.1 90/07/18 14:28:43 huang Exp $"

#define  SG_BASE	0x00540000
#define  CDB_ADDR	0x00530000

#define ISR_NCR 	0x01000		/* 0 = interrupt from SCSI controller */
#define ISR_DMA_ERR	0x00100		/*    intr from dma error */
#define ISR_DMA_ZERO	0x00200	 	/*    intr from dma zero halfword     */	




/* MODE register bit definition
 */
#define DMA_FIFO_CNT	0x000000ff	/* # of halfwords in the stalled FIFO	  */
#define DMA_ZERO_HALFB  0x00000b40      /* success of transfer without any err    */
#define DMA_INTR	0x00000100	/* interrupt pending bit		  */
#define DMA_PAR_ERR	0x00000200	/* DMA parity bit	         	  */


#define DMA_RESETB	0x0000ffdf	/* DMA reset pulse bit (darryl,3-19-90)*/
#define NOT_DMA_RESETB  0x20            /* Disable reset after read back */
#define DMA_FLUSH_PIPEB	0xfff7  	/* purge( flush ) FIFO bit         	  */
#define DMA_RUNB 	0xfffe  	/* Enable channel bit	         	  */
#define DMA_ENA_ERRINTB	0xffdf		/* Enable error intr
  */
#define DMA_ENA_CHAINB	0xfffd
#define DMA_ENA_INTR	0x10000000	/* Enable interrupt bit	         	  */
#define DMA_TO_MEMORYB	0xfffb	/* transfer direction bit        	  */
#define RUNB_MSK	0x00000001

#define RUN_READ	0x00000001
					/* RAMBO can only address on 16 word	*/
					/* boundry 			     	*/
/* for genesis standalone */
#define CNT_MSK		0xffff0000      /*Halfwork count mask  */
#define CNT_SHIFT	16
#define CTL_MSK		0xffff		/*halfword ctl mask */

/* BLKCNT register bit definition
 */
#define	DMA_BLKCNT_MASK		0x1fff		/* valid bits in block count register   */

#define DMA_BLK_HSIZE	0x00000020	/* number of halfwords in a DMA Burst	  */
#define DMA_BLK_WSIZE	0x00000010	/* number of words in a DMA Burst	  */
#define DMA_BLK_BSIZE	0x00000040	/* number of bytes in a DMA Burst	  */
#define MAX_CNT_CH1	48		/* size of channel 1 FIFO (# of halfwords )   */
#define MAX_CNT_CH2	96		/* size of channel 2 FIFO (# of halfwords )   */

struct	dma_g  {
		unsigned long	caddr;
		unsigned long	maddr;
		unsigned long	ctl_cnt;
		unsigned long	ctl_cnt1;
	      };

struct ncrsg{
	u_int	maddr;
	u_int	ctl_cnt;
};

