#ident "$Header: pon_ncr94.c,v 1.3.1.1 90/07/18 14:32:19 huang Exp $"
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
 *	Name : pon_ncr94.c
 *
 *	Description : This test verifies the Functions of
 *		the SCSI Protocol Controller ( NCR 53C94 ).
 *
 ********************************************************************/

#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/ncr53c94.h"
#include "prom/prom.h"
#include "pon.h"

#define	NCR_FIFO_CNT_MSK	0x1f		/* five bits of FIFO count */
#define MAX_T_DELAY		0x100

extern char failure[], success[];

static volatile struct scsi *ncr = (struct scsi *)PHYS_TO_K1(NCR_BASE);


Pon_Ncr94()
{
	register volatile u_char w, r;
	register u_long i, fail_cnt ;

	pon_puts("SCSI Controller Chip Test...");

	ncr = (struct scsi *)PHYS_TO_K1(NCR_BASE);
		/* reset NCR chip
		 */
	scsi_c_reset();

	fail_cnt = 0;

		/* verifiy registers
		 */
	for( i = 0, w = 1 ; i < 8 ; i++, w <<= 1 ) {
	   if( wrv_reg( &ncr->lo_count, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->hi_count, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config1, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config2, (w & 0x1f) ) == FAIL )
	     fail_cnt++;
	}

		/* write and read zeros and ones to all register
		 */
	for( i = 0, w = 0 ; i < 2 ; i++, w = ~w ) {
	   if( wrv_reg( &ncr->lo_count, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->hi_count, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config1, w ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config2, (w & 0x1f) ) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->cmd, w ) == FAIL )
	     fail_cnt++;
	}

	scsi_c_reset();

		/* test NCR FIFO
		 */
	if( scsi_fifo_t() == FAIL )
	  fail_cnt++;

		/* test NCR interrupt, and reset scsi bus
		 */
	if( scsi_bus_reset() == FAIL )
	  fail_cnt++;

	if( fail_cnt ) {
	  pon_puts( failure );
	  FastFlash(0);
	  SetDepend(PON_FAULT_SCSI);
	  return( FAIL );
	}
	else {
	  pon_puts( success );
	  return(PASS);
	}
}


/*
 * Name : scsi_c_reset
 * Description : Resets the SCSI protocol Controller chip.
 */
static scsi_c_reset()
{
	register u_long i, j;

		/* reset NCR chip
		 */
	ncr->cmd = RESET_NCR;

	for (i=0; i < 0x1000 ; i++)
	    ;

	ncr->cmd = NOP;

	return(PASS);
}


/*
 * Name : wrv_reg
 * Synopsis : wrv_reg( register_address, data_to_write )
 * Description : Writes Reads and verifies a register.
 */
static wrv_reg( reg_address, w )
u_char *reg_address;
u_char w;
{
	register volatile u_char r;

		/* write to NCR register
		 */
	*reg_address = w;

		/* if any of COUNT registers, load them by writing to command reg.
		 */
	if( (reg_address == &ncr->lo_count) || ( reg_address == &ncr->hi_count) )
	  ncr->cmd = DMA;

		/* read the NCR register back and verify it
		 */
	if( (r = *reg_address) != w ) {
	  return( FAIL );
	}

		/* zero-out the register
		 */
	*reg_address = 0;

		/* if any of COUNT registers, load them by writing to command reg.
		 */
	if( (reg_address == &ncr->lo_count) || (reg_address == &ncr->hi_count) )
	  ncr->cmd = DMA;

	return( PASS );
}


/*
 * Name : scsi_bus_reset
 * Description : resets the SCSI bus
 */
static scsi_bus_reset()
{
	register u_char st;
	register u_long i;

		/* reset scsi chip
		 */
	scsi_c_reset();

		/* check interrupts from NCR
		 */
	if( ncr->status_id & INTR ) {
	  goto f_scsi_bus_reset;
	}

		/* check interrupt register
		 */
	if( !(GetIntR() & IR_NCR_INT_B) ) {
	  goto f_scsi_bus_reset;
	}

		/* reset SCSI Bus
		 */
	ncr->cmd = RESET_SCSI;

		/* wait for interrupt from SCSI
		 */
	for( i = 0 ; i < MAX_T_DELAY ; i++ )  {
	    if( (st=ncr->status_id) & INTR )
	      break;
	}

		/* verify If timeout occurred
		 */
	if( i == MAX_T_DELAY ) {
	  goto f_scsi_bus_reset;
	}

		/* interrupt should be pending
		 */
	if( !(GetCause() & CAUSE_IP4) ) {
	  goto f_scsi_bus_reset;
	}

		/* check interrupt register
		 */
	if( GetIntR() & IR_NCR_INT_B ) {
	  goto f_scsi_bus_reset;
	}

		/* read NCR interrupt register, this clears all status, and interrupt reg. bits
		 */
	st = ncr->intr_timeout;

		/* verify SCSI Bus Reset bit In Interrupt Reg.
		 */
	if( !(st & SCSI_RESET) ) {
	  goto f_scsi_bus_reset;
	}

		/* verify interrupt is cleared
		 */
	if( !(GetIntR() & IR_NCR_INT_B) ) {
	  goto f_scsi_bus_reset;
	}

		/* interrupt should be clear
		 */
	if( (GetCause() & CAUSE_IP4) ) {
	  goto f_scsi_bus_reset;
	}

	return( PASS );

f_scsi_bus_reset:
	return( FAIL );
}


/*
 * Name : scsi_fifo_t
 * Description : Checks the 16 byte FIFO of NCR-53C94
 */
static scsi_fifo_t()
{

	register volatile u_char r,w,i;

		/* reset SCSI chip
		 */
	scsi_c_reset();

		/* Verify NCR FIFO
		 */
	for( i = 0, w = 1 ; i < MAX_FIFO_CNT ; i++, w += 1 ) {
	   ncr->fifo = w;
	   if( (ncr->fifoflag_syncoffset & NCR_FIFO_CNT_MSK) != (i+1) ) {
	     return( FAIL );
	   }
	}

	for( i = MAX_FIFO_CNT, w = 1 ; i != 0 ; i--, w += 1 ) {
	   if( (r=ncr->fifo) != w ) {
	     return( FAIL );
	   }
	   if( (ncr->fifoflag_syncoffset & NCR_FIFO_CNT_MSK) != (i-1) ) {
	     return( FAIL );
	   }
	}

	scsi_c_reset();

	return( PASS );
}
