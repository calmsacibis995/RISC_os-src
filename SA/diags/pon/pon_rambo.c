#ident "$Header: pon_rambo.c,v 1.3.1.1 90/07/18 14:32:41 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

/********************************************************************
 *
 *	Name : pon_rambo.c
 *
 *	Description : These tests check the DMA controller( RAMBO).
 *
 ********************************************************************/

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/rambo.h"
#include "prom/prom.h"
#include "pon.h"

#define	MAX_T_D			0x10
#define	DMA_BLKCNT_MASK		0x1fff		/* valid bits in block count register */
#define	DMA_CNTRL_MASK		0xffff0000	/* control portion of mode register */
#define	DMA_LA_MASK		0x0fffffc0	/* DMA controller can only address data */
#define	MAX_CNT_CH1		48		/* size of channel 1 FIFO (# of halfwords)*/
#define	MAX_CNT_CH2		96		/* size of channel 2 FIFO (# of halfwords)*/
#define	DMA_BLK_WSIZE		0x00000010	/* number of words in a DMA Burst */
#define	DMA_BLK_HSIZE		0x00000020	/* number of halfwords in a DMA Burst */
#define	DMA_BLK_BSIZE		0x00000040	/* number of bytes in a DMA Burst */

#define	FAULT_PAT		0x10367e5f

extern char failure[], success[], skipped[];
extern u_int pon_tmp;

static struct dma *ch[2] = {
	(struct dma *)PHYS_TO_K1(DMA1_BASE),
	(struct dma *)PHYS_TO_K1(DMA2_BASE)
};

static volatile struct system *sys = (struct system *)PHYS_TO_K1(SYS_EREG);

static u_long ch_number;
static u_long cause_reg;
static u_char *dma_fault_ptr;


static CheckSCR()

{
	register volatile u_int *screg = (u_int *)PHYS_TO_K1(SYS_CREG);
	register u_int i;
	register u_int r;
	register u_int save;
	register u_int w;
	register error = PASS;

		/* walk a 1 through 7 bits of the system control register
		*/
	save = *screg;
	for( i = 0, w = 1 ; i < 7 ; i++, w <<= 1 ) {
	   if (w == CR_ENA_PAR_CHK) {
		continue;
	   }

	   *screg = w;
	   pon_tmp = 0;
	   if( (r = *screg) != w ) {
		error = FAIL;
	   }
	}

	*screg = save;
	return(error);
}


Pon_Dma()
{
	register volatile u_long w, r;
	register u_long i, cn ;
	register u_long any_dma_errors;
	u_long volatile f_code[3];

	pon_puts("DMA Controller Chip Test...");

	any_dma_errors = 0;
	f_code[0] = PON_FAULT_SCR;
	f_code[1] = PON_FAULT_DMA_CHAN1;
	f_code[2] = PON_FAULT_DMA_CHAN2;

	any_dma_errors |= CheckSCR() ? PON_FAULT_SCR : 0;

	for( cn = 0 ; cn < 2 ; cn++ ) { /* --------------------------------- */

	   ch_number = cn+1; /* channel number */

	   ch[cn]->mode = 0;

		/* walk a 1 through bits 27-6 of LADDR register
		 * 6 lowest and 4 highest bits are don't care.
		 */
	   for( i = 0, w = 0x40 ; i < (32-6-4) ; i++, w <<= 1 ) {
	      ch[cn]->laddr = w;
	      pon_tmp = 0;
	      if( (r = ch[cn]->laddr) != w ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      pon_tmp = 0;
	      if( (r = ch[cn]->caddr) != w ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      ch[cn]->laddr = ~w;
	      pon_tmp = 0;
	      if( (r = ch[cn]->laddr) != (~w & DMA_LA_MASK) ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      pon_tmp = 0;
	      if( (r = ch[cn]->caddr) != (~w & DMA_LA_MASK) ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	   }

		/* walk a 1 through all the bits of block count register
		 */
	   for( i = 0, w = 1 ; i < 13 ; i++, w <<= 1 ) {
	      ch[cn]->blkcnt = w;
	      pon_tmp = 0;
	      if( (r = (ch[cn]->blkcnt & DMA_BLKCNT_MASK)) != w ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      ch[cn]->blkcnt = ~w;
	      pon_tmp = 0;
	      if( (r = (ch[cn]->blkcnt & DMA_BLKCNT_MASK)) != (~w & DMA_BLKCNT_MASK) ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	   }

		/* walk a 1 through all the bits of mode register
		 */
	   ch[cn]->mode = 0;
	   ch[cn]->blkcnt = 0;
	   for( i = 0, w = CLR_DMA_ERR ; i < 6 ; i++, w <<= 1 ) {
	      ch[cn]->mode = w;
	      pon_tmp = 0;
	      if( (r = (ch[cn]->mode & DMA_CNTRL_MASK)) != w ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      ch[cn]->mode = ~w;
	      pon_tmp = 0;
	      if( (r = (ch[cn]->mode & DMA_CNTRL_MASK)) != (~w & DMA_CNTRL_MASK) ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	   }
	   ch[cn]->mode = 0;

		/* walk a 1 through all the bits of FIFO register
		 */
	   ch[cn]->mode = (u_long)FLUSH_RAMBO_FIFO;
	   ch[cn]->mode = 0;
	   for( i = 0, w = 0x1 ; i < 16 ; i++, w <<= 1 ) {
	      ch[cn]->mode = (u_long )(TO_MEMORY);
	      ch[cn]->fifo = w;
	      ch[cn]->mode &= (u_long )(~TO_MEMORY);
	      if( (r = ch[cn]->fifo) != w ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	      ch[cn]->mode = (u_long )(TO_MEMORY);
	      ch[cn]->fifo = ~w;
	      ch[cn]->mode &= (u_long )(~TO_MEMORY);
	      if( (r = ch[cn]->fifo) != (~w & 0xffff) ) {
		any_dma_errors |= f_code[ch_number]; break;
	      }
	   }

		/* FIFO Test
		 */
	   if( dma_fifo_f( ch[cn], ch_number, MAX_CNT_CH1*ch_number ) == FAIL ) {
		any_dma_errors |= f_code[ch_number]; continue;
	   }

		/* DMA READ
		 */
	   if( pn_dmaint_rd( ch[cn], PON_SCRATCHMEM, ch_number ) == FAIL ) {
		any_dma_errors |= f_code[ch_number]; continue;
	   }

		/* DMA WRITE
		 */
	   if( pn_dmaint_wrt( ch[cn], PON_SCRATCHMEM, ch_number ) == FAIL ) {
		any_dma_errors |= f_code[ch_number]; continue;
	   }

	}  /* ------------------------------------------------ */


	if( any_dma_errors ) {
	  pon_puts( failure );
	  FastFlash(0);
	  SetDepend( any_dma_errors);
	  return( FAIL );
	}
	else {
	  pon_puts( success );
	  return(PASS);
	}
}


/*
 * Name : dma_fifo_f
 *	Description :
 *		This test checks the operation of FIFO for The
 *		DMA controller channel 1 and 2.
 *		It verifies FIFO count, FIFO Full and Empty bits,
 *		and Purge FIFO bit.
 *
 */
static dma_fifo_f( ch, ch_num, max_cnt)
register struct dma *ch;
register u_long ch_num, max_cnt;
{
	register u_long w, r, n, m;

#ifdef DEBUG
	pon_puts("FIFO test channel ");
	pon_puthex(ch_number);
	pon_puts("\r\n");
#endif DEBUG

		/* purge FIFO
		 */
	ch->mode = FLUSH_RAMBO_FIFO;

		/* Fill up the FIFO with incrementing data
		 */
	ch->mode = (u_long )(TO_MEMORY);
	for( m = 0, w = 0x00000001 ; m < max_cnt ; m++, w+=0x00000202 ) {
	   ch->fifo = w;
	   pon_tmp = 0;
	   if( (r = (ch->mode & COUNT_MSK)) != (m+1) ) {
	     return(FAIL);
	   }
	}

		/* check FIFO Full bit
		 */
	if( !(ch->mode & FIFO_FULL) ) {
	  return(FAIL);
	}

		/* empty the Channel FIFO
		 */
	ch->mode &= (u_long )(~TO_MEMORY);
	for( m = 0, w = 0x00000001 ; m < max_cnt ; m++, w+=0x00000202 ) {
	   if( (r = (ch->fifo)) != w ) {
	     return(FAIL);
	   }
	   if( (r = (ch->mode & COUNT_MSK)) != ((max_cnt-1)-m) ) {
	     return(FAIL);
	   }
	}

		/* check FIFO Empty bit
		 */
	if( !(ch->mode & FIFO_EMPTY) ) {
	  return(FAIL);
	}

		/* Fill up Chennel FIFO
		 */
	ch->mode = (u_long )(TO_MEMORY);
	for( m = 0, w = 0x00000001 ; m < max_cnt ; m++, w+=0x00000202 ) {
	   ch->fifo = w;
	}

		/* check fifo count
		 */
	if( (r = (ch->mode & COUNT_MSK)) != max_cnt ) {
	  return(FAIL);
	}

		/* purge the FIFO
		 */
	ch->mode |= (u_long )(FLUSH_RAMBO_FIFO);
	pon_tmp = 0;
	if( !(ch->mode & FIFO_EMPTY) ) {
	  return(FAIL);
	}

		/* verify FIFO count
		 */
	if( (ch->mode & COUNT_MSK) ) {
	  return(FAIL);
	}

	for( n = 0, w = 1 ; n < 16 ; n++, w <<= 1 ) {

		/* Fill up the FIFO
		 */
	   ch->mode = (u_long )(TO_MEMORY);
	   for( m = 0 ; m < max_cnt/2 ; m++ ) {
	      ch->fifo = w;
	      ch->fifo = ~w;
	   }

		/* verify the FIFO
		 */
	   ch->mode &= (u_long )(~TO_MEMORY);
	   for( m = 0 ; m < max_cnt/2 ; m++ ) {
	      if( (r = (ch->fifo)) != w )
		return(FAIL);

	      if( (r = (ch->fifo & 0xffff)) != ((~w) & 0xffff) )
		return(FAIL);
	   }

	}

		/* verify FIFO count
		 */
	if( (ch->mode & COUNT_MSK) ) {
	  return(FAIL);
	}

	return(PASS);
}


/*
 * Name : pn_dmaint_rd
 * Description :  programs the DMA controller to transfer 1 block of data
 *			from memory to FIFO. Then it verifies the interrupt
 *		at the completion of DMA transfer. It also verifies the data
 *		in the FIFO, and clearing of DMA interrupt .
 *
 */
static pn_dmaint_rd( ch, mem_ptr, ch_num)
register struct dma *ch;
register u_char *mem_ptr;
register u_long ch_num;
{
	register u_long m, n, r;
	register u_long *rd_ptr;
	u_long volatile start_pattern, exp_fifo_cnt;

#ifdef DEBUG
	pon_puts("Read test channel ");
	pon_puthex(ch_number);
	pon_puts("\r\n");
#endif DEBUG
		/* a work around for channel two DMA.
		 * It takes 1 byte from the FIFO before we can disable the channel
		 */
	if( ch_num == 2 ) {
	  start_pattern = 0x00000203;
	  exp_fifo_cnt = DMA_BLK_HSIZE - 1;
	}
	else {
	  start_pattern = 0x00000001;
	  exp_fifo_cnt = DMA_BLK_HSIZE ;
	}

		/* purge FIFO
		 */
	ch->mode = (u_long)FLUSH_RAMBO_FIFO;

		/* init. memory
		 * pattern  = 0, 1, 2, 3, 4, ....
		 */
	rd_ptr = (u_long *)PHYS_TO_K1( mem_ptr );
	for( m = 0, n = 0x00010203 ; m < DMA_BLK_WSIZE ; m++, n+=0x04040404 )
	   *rd_ptr++ = n ;

		/* Program DMA to transfer
		 * 1 block of data(16 words) from Memory to FIFO.
		 */
	ch->mode = (u_long)( INTR_EN );
	ch->laddr = (u_long )mem_ptr;
	ch->blkcnt = (u_long )1;

		/* enable channel
		 */
	ch->mode |= (u_long )(CHANNEL_EN );

		/* wait for the interrupt
		 */
	for( m = 0 ; m < MAX_T_D ; m++ ) {
	   if( GetCause() & CAUSE_IP4 ) {
	     break;
	   }
	}
		/* disable channel 1
		 */
	ch->mode  &= (u_long )( ~CHANNEL_EN );
#ifdef DEBUG
	pon_puthex(m);
#endif DEBUG
		/* verify Interrupt was Received.
		 */
	if( m == MAX_T_D ) {
	  return( FAIL );
	}

		/* check the Current Address reg.
		 */
	if( ch->caddr != (u_long)(mem_ptr+DMA_BLK_BSIZE) ) {
	  return( FAIL );
	}

		/* block count register should be 0
		 */
	if( ch->blkcnt ) {
	  return( FAIL );
	}

		/* check for parity error during DMA
		 */
	if( ch->mode & DMA_ERROR ) {
	  return( FAIL );
	}

		/* verify FIFO count
		 */
	if( (ch->mode & COUNT_MSK) != exp_fifo_cnt ) {
	  return( FAIL );
	}

		/* Verify The Data In the FIFO
		 */
	if( ch_num == 1 )
	for( m = 0, n = start_pattern ; m < exp_fifo_cnt ; m++, n+=0x00000202 ) {
	   if( ch->fifo != n ) {
	     return( FAIL );
	   }
	}
		/* clear RAMBO interrupt by writing to block count register
		 */
	ch->blkcnt = (u_long )0;

		/* verify interrupt pending bit is cleared
		 */
	if(  (ch->mode & DMA_INTR)  ) {
	  return( FAIL );
	}

		/* verify the interrupt is off
		 */
	if( GetCause() & CAUSE_IP4 ) {
	  return( FAIL );
	}

	ch->blkcnt = 0;
	ch->mode = 0;

	return( PASS );
}


/*
 * Name : pn_dmaint_wrt
 * Description :  programs the DMA controller to transfer 1 block of data
 *		from FIFO to memory . Then it verifies the interrupt
 *		at the completion of DMA transfer. It also verifies the data
 *		in Memory after the transfer, and clearing of DMA interrupt .
 *
 */
static pn_dmaint_wrt( ch, mem_ptr, ch_num )
register struct dma *ch;
register u_char *mem_ptr;
register u_long ch_num;
{
	register u_long m, n, r, w;
	register u_long *wrt_ptr;

#ifdef DEBUG
	pon_puts("Write test channel ");
	pon_puthex(ch_number);
	pon_puts("\r\n");
#endif DEBUG

		/* purge FIFO
		 */
	ch->mode = (u_long)FLUSH_RAMBO_FIFO;

		/* init. Memory
		 */
	wrt_ptr = (u_long *)PHYS_TO_K1( mem_ptr );
	for( m = 0 ; m < DMA_BLK_WSIZE ; m++ )
	   *wrt_ptr++ = 0xffffffff;

		/* put 1 block of data into the FIFO
		 */
	ch->mode = (u_long )(TO_MEMORY);
	for( m = 0, w = 0x00000001 ; m < DMA_BLK_HSIZE ; m++, w+=0x00000202 ) {
	   ch->fifo = w;
	}

		/* Program DMA to transfer
		 * 1 block of data(16 words) from FIFO To Memory .
		 */
	ch->mode = (u_long )( INTR_EN | TO_MEMORY );
	ch->laddr = (u_long )mem_ptr;
	ch->blkcnt = (u_long )1;

	if( GetCause() & CAUSE_IP4 ) {
	  return( FAIL );
	}

		/* enable channel
		 */
	ch->mode |= (u_long )(CHANNEL_EN );

		/* wait for the interrupt
		 */
	for( m = 0 ; m < MAX_T_D ; m++ ) {
	   if( GetCause() & CAUSE_IP4 ) {
	     break;
	   }
	}

		/* disable channel 1
		 */
	ch->mode  &= (u_long )( ~CHANNEL_EN );
#ifdef DEBUG
	pon_puthex(m);
#endif DEBUG

		/* verify Interrupt was Received.
		 */
	if( m == MAX_T_D ) {
	  return( FAIL );
	}

		/* check the Current Address reg.
		 */
	if( ch->caddr != (u_long)(mem_ptr+DMA_BLK_BSIZE) ) {
	  return( FAIL );
	}

		/* block count register should be 0
		 */
	if( ch->blkcnt ) {
	  return( FAIL );
	}

		/* verify data in memory
		 */
	wrt_ptr = (u_long *)PHYS_TO_K1( mem_ptr );
	for( m = 0, n = 0x00010203 ; m < DMA_BLK_WSIZE ; m++, n+=0x04040404 ) {
	   if( *wrt_ptr++ != n ) {
	     return( FAIL );
	   }
	}

		/* No Data should be in FIFO
		 */
	if( (ch_num == 1 ) && (ch->mode & COUNT_MSK) ) {
	  return( FAIL );
	}

		/* clear RAMBO interrupt by writing to interrupt enable bit
		 */
	ch->mode &= (u_long )(~INTR_EN );
	pon_tmp = 0;

		/* verify interrupt pending bit is cleared
		 */
	if(  (ch->mode & DMA_INTR)  ) {
	  return( FAIL );
	}

		/* verify the interrupt is off
		 */
	if( GetCause() & CAUSE_IP4 ) {
	  return( FAIL );
	}

	ch->blkcnt = 0;
	ch->mode = 0;

	return( PASS );
}


/********************************************************************
 *	Name : Pon_dma_parity
 *	Description :
 *		This test checks the parity error detection during
 *		DMA reads of Main memory.
 *
 ********************************************************************/
Pon_DmaParity()
{
	register exp_ereg;

	pon_puts("DMA Parity Test...");

		/* if a bad keyboard controller or DMA controller, skip the test
		 */
	if( GetDepend() & (PON_FAULT_KEYBOARD | PON_FAULT_DMA_CHAN1 | PON_FAULT_DMA_CHAN2 | PON_FAULT_SCR | PON_FAULT_FAULT) ) {
	  pon_puts(skipped);
	  return( FAIL );
	}

	ch[0]->blkcnt = ch[1]->blkcnt = 0;

		/* enable parity checking and capturing ( also de-assert clear
		 * error interrupt.
		 */
	sys->creg |= (CR_ENA_PAR_CHK | CR_CLR_PAR_INT_B ) ;

		/* test channel 1
		 */
	exp_ereg = (ER_PARERR0 | ER_PARERR1 | ER_PARERR2 | ER_PARERR3);
	dma_fault_ptr = (u_char *)PHYS_TO_K1( PON_SCRATCHMEM );
	exp_ereg |= PON_SCRATCHMEM ;
	if( pn_dma_par( ch[0], 1, exp_ereg, PON_SCRATCHMEM ) == FAIL )
	  goto parity_fail;

	sys->creg |= ( CR_CLR_PAR_INT_B ) ;

		/* test channel 2
		 */
	exp_ereg = (ER_PARERR0 | ER_PARERR1 | ER_PARERR2 | ER_PARERR3);
	dma_fault_ptr = (u_char *)PHYS_TO_K1( PON_SCRATCHMEM+0x100+8 );
	exp_ereg |= (PON_SCRATCHMEM+0x100+8) ;
	if( pn_dma_par( ch[1], 2, exp_ereg, PON_SCRATCHMEM+0x100 ) == FAIL )
	  goto parity_fail;

	sys->creg = CR_ENA_BUZZER_B | CR_CLR_ERR_REG;
	sys->creg = CR_ENA_BUZZER_B;
	ch[0]->mode = 0; ch[1]->mode = 0;
	pon_puts( success );
	return(PASS);

parity_fail:
	sys->creg = CR_ENA_BUZZER_B | CR_CLR_ERR_REG;
	sys->creg = CR_ENA_BUZZER_B;
	ch[0]->mode = 0; ch[1]->mode = 0;
	pon_puts( failure );
	FastFlash(0);
	SetDepend( PON_FAULT_DMA_PARITY);
	return( FAIL );
}


/*
 * dma_par( a_pointer_to_channel_registers, channel_number,
 *		exp_err_reg, a_pointer_to_dma_area );
 */
static pn_dma_par( ch, ch_num, exp_err_reg, dma_mem_ptr )
register struct dma *ch;
u_long ch_num, exp_err_reg;
register u_char *dma_mem_ptr;
{
	register u_long m, r, w;
	register u_long *rd_ptr;
	register volatile u_long err_reg;

#ifdef DEBUG
	pon_puts("DMA parity test channel ");
	pon_puthex(ch_num);
	pon_puts("\r\n");
#endif DEBUG

		/* init. memory.  Write an incrementing
		 * pattern 0, 1, 2, 3, 4, ....
		 */
	rd_ptr = (u_long *)PHYS_TO_K1( dma_mem_ptr );
	for( m = 0, w = 0x00000001 ; m < DMA_BLK_WSIZE*2 ; m++, w-=0x00020002 )
	   *rd_ptr++ = w ;

		/* Interrupts should not be pending now.
		 */
	if( GetCause() & (CAUSE_IP8 | CAUSE_IP4) ) {
	  return( FAIL );
	}

		/* set Force_Bad_Parity bit in P2 register
		 */
	if( SetBadParity() != PASS ) {
	  return( FAIL );
	}

	*(u_long *)dma_fault_ptr = FAULT_PAT;

		/* clear Force_Bad_Parity bit in P2 register
		 */
	if( UnsetBadParity() != PASS ) {
	  return( FAIL );
	}

		/* program DMA to transfer 2 blocks of data from Memory to FIFO
		 */
	ch->mode = (u_long )(FLUSH_RAMBO_FIFO);
	ch->mode = (u_long )0;
	ch->laddr = (u_long )dma_mem_ptr;
	ch->blkcnt = 2;

		/* enable channel
		 */
	ch->mode = (u_long )( INTR_EN | CHANNEL_EN );

		/* wait for the DMA to complete
		 */
	for( m = 0 ; m < MAX_T_D ; m++ ) {
	   if( (cause_reg = GetCause()) & (CAUSE_IP8 | CAUSE_IP4) ) {
	     break;
	   }
	}

		/* disable channel
		 */
	ch->mode &= (u_long )( ~CHANNEL_EN);

		/* correct parity
		 */
	*(u_long *)dma_fault_ptr = 0;

	sys->creg &= ~CR_CLR_PAR_INT_B ;

		/* verify DMA completion
		 */
	if( m == MAX_T_D ) {
	  return(FAIL);
	}

		/* verify, cause register Interrupt pending bit
		 */
	if( !(cause_reg & CAUSE_IP8)  )  {
	  return( FAIL );
	}

		/* verify, cause register Interrupt pending bit
		 */
	if( (cause_reg & CAUSE_IP4)  )  {
	  return( FAIL );
	}

		/* verify block count register
		 */
	if( ch->blkcnt != 1 ) {
	  return( FAIL );
	}

		/* check the Current Address reg.
		 */
	if( ch->caddr != (u_long)(dma_mem_ptr+DMA_BLK_BSIZE) ) {
	  return( FAIL );
	}

		/* check for parity error during DMA
		 */
	if( !(ch->mode & DMA_ERROR) ) {
	  return( FAIL );
	}

		/* DMA interrupt should not be pending
		 */
	if( (ch->mode & DMA_INTR) ) {
	  return( FAIL );
	}

		/* Verify FIFO Count for channel 1 only
		 */
	if( ch_num == 1 )
	  if( (r = (ch->mode & COUNT_MSK)) != DMA_BLK_HSIZE ) {
	    return( FAIL );
	  }

		/* read system error register
		 */
	err_reg = sys->ereg;

		/* verify parity error byte lane identifier
		 */
	if( (err_reg & ER_PAR_ERR_MASK) != (exp_err_reg & ER_PAR_ERR_MASK) ) {
	  return( FAIL );
	}

		/* verify parity error address
		 */
	if( (err_reg & ER_ADDR_MASK) != (exp_err_reg & ER_ADDR_MASK) ) {
	  return( FAIL );
	}

		/* clear parity error bit in DMA mode register
		 */
	ch->mode |= CLR_DMA_ERR;

		/* verify parity error bit in mode reg. is cleared.
		 */
	if( ch->mode & DMA_ERROR ) {
	  return( FAIL );
	}

		/* verify, cause register Interrupt pending bit
		 */
	if( (GetCause() & CAUSE_IP8)  )  {
	  return( FAIL );
	}

		/* clear parity error byte identifier bits
		 * in system error register
		 */
	sys->creg |= CR_CLR_ERR_REG;
	sys->creg &= ~CR_CLR_ERR_REG;
	pon_tmp = 0;

		/* verify parity error byte identifier bits
		 */
	if( (sys->ereg & ER_PAR_ERR_MASK) != 0 ) {
	  return( FAIL );
	}

	return(PASS);
}
