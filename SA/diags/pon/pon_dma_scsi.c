#ident "$Header: pon_dma_scsi.c,v 1.2.1.2 90/07/31 15:14:24 huang Exp $"

/********************************************************************
 *
 *	Name : dma_subr.c
 *
 *	Description : Tests The interface between SCSI and the DMA
 *		using interrupt driven  .
 *
 *
 ********************************************************************/
#include	<sys/types.h>
#include	"sys/param.h"
#include	"sys/buf.h"
#include	"machine/cpu.h"
#include	"ncr53c94.h"
#include	"dma_2000.h"
#include	"machine/cpu_board.h"
#include	"machine/vmereg.h"
#include 	"mips/scsi.h"
#include	"pon.h"
	

#define MAX_T_DELAY	20

extern  char success[], failure[],skipped[], clrf[];
static 	volatile struct	scsi_g	*ncr = (struct scsi_g *)PHYS_TO_K1(NCR_BASE_RB3125);
static	volatile struct	scsi_r	ncr_r; 
volatile u_char scsi_status_byte, scsi_msg, *cdb/*,buf[1024]*/; 

volatile struct	dma_g	*ch = (struct dma_g*)PHYS_TO_K1(DMA_BASE_RB3125);
volatile u_char from_status_phase, from_msg_phase;
volatile u_char cmd_group;
volatile u_int  t_length;
volatile u_char  *r_ptr ;
volatile u_int  *w_ptr;
u_char	 scsi_write = 0;
u_char   sg_chain,target_id = 4;
u_int	 page_size = 4096;
struct   scsiip{
 	 u_char    scsi_hwstatus;
	 u_char    scsi_status;
	 u_char    rez1;
	 u_char	   rez2;
}   scsiip;	

struct scsiip  *ip;

struct  u_sense {
	u_char  class;
	u_char  segment;
	u_char  key;
	u_char  info1;
} *sense;

struct  u_inq {
	u_char  device_type;
	u_char  dtq;
	u_char  version;
	u_char  res0;
}  *inq;

dma_ncr_drvr()

{
	u_char   status_reg,int_reg,seq_step;
	u_char   in_phase;


	   in_phase = 0;
	   if (send_cmd() == FAIL) {
		return( FAIL);
	   }

	   for(;;) {
	   switch( (ncr_r.status & PHASE_MASK) ) {

		case DATA_OUT_F :
				if (in_phase > 2) {
				    pon_puts(" error: hang on data out phase\r\n");
				    return(FAIL);
				}
#ifdef  DEBUG
				printf("DATA_OUT_F");
#endif  DEBUG
				dsply_r();
				in_phase++;
				if (sg_chain == 1) {
			            if (send_to_ncrsg(t_length,w_ptr) == FAIL)
					return(FAIL);
				}
				else  if(send_to_scsi (t_length, w_ptr) == FAIL)
				        return(FAIL); 
				break;
		case DATA_IN_F :
				in_phase = 0;
#ifdef  DEBUG
				printf("DATA_IN_F");
#endif  DEBUG
				dsply_r();
				if (sg_chain == 1) {
				    if (rcv_from_ncrsg(t_length,r_ptr) == FAIL) 
					return(FAIL);
				}
				else if( rcv_from_scsi( t_length, r_ptr ) == FAIL )
				        return(FAIL); 
				break;
		case MSG_IN_F :
#ifdef  DEBUG
					printf("MSG_IN_F");
#endif  DEBUG
					dsply_r();
	  				rcv_msg_byte();

					break;
		case MSG_OUT_F :
#ifdef  DEBUG
					printf("MSG_OUT_F");
#endif  DEBUG
					dsply_r();

					break;
		case STATUS_F :
					in_phase = 0;
#ifdef  DEBUG
					printf("STATUS_F");
#endif  DEBUG
					dsply_r();
				        read_fifo();	
					if( rcv_status_byte() == FAIL )
					  goto end_bus;
					
					scsi_status_byte = ncr->fifo ;
					scsi_msg = ncr->fifo;
#ifdef  DEBUG
					printf("\nReturned Status = %x..", 
						scsi_status_byte );
					printf("\nReturned MSG = %x", ncr->fifo
						);
#endif  DEBUG
					ncr->cmd = NCR_CMD_MSG_ACPTD ;
					wbflush();
					wait_for_ncr_int( 0x80000 ); 
				        
					goto end_bus;
					break;

		default:
					pon_puts("UNKnown PHASE");
					dsply_r();
					goto end_bus;

					break;

	}
	}

end_bus:

	if( ip -> scsi_status = scsi_status_byte )
	  return( FAIL );

	return( PASS );
	  
}


   




/*
 *
 * Name : send_cmd
 * Description : sends the CDB to the NCR53c94
 *
 */
send_cmd()
{
	u_int	cnt;
	u_char  i;
		/* set transfer counter for NCR 
		 */

	ncr->hi_count = 0;
	wbflush();
	if (cmd_group = 1)
		ncr->lo_count = 12;
	else    ncr->lo_count = 8;
	wbflush();
		/* flush the FIFO
		 */
	ncr->cmd = NCR_CMD_FLUSH_FIFO | NCR_CMD_DMA ;
	wbflush();

		/* set destination ID
		 */
	ncr->status_id = target_id;
	wbflush();

	dsply_r();


		/* program timeout period
		 */
	ncr->intr_timeout = 153*2;
	wbflush();


	if (cmd_group == 1)
		cnt = ~(0x06);
	else    cnt = ~(0x4);
	ch->maddr = (u_long)CDB_ADDR;
	wbflush();
	ch->ctl_cnt1 = (u_long)((  DMA_RUNB) | (cnt << 16));
	wbflush();

		/* do the selection without attention
		 */
	ncr->cmd = NCR_CMD_SLCT | NCR_CMD_DMA;
	wbflush();

		/* wait for interrupt
		 */
	if (wait_for_ncr_int(80000) == FAIL){
#ifdef  DEBUG
	  printf("\nError: Timeout, Waiting For Target To Respond To Selection."		);
#endif  DEBUG
	  ip->scsi_hwstatus = SELTMO;
	  ch->ctl_cnt1 = (u_long)(  DMA_RESETB) ;
	  return( FAIL );
	}

	
	ch->ctl_cnt1 = (u_long)(  DMA_RESETB) ;
	return( PASS );
}


c0_test_ready()
{
	register u_int *cr_p;
		/*reset DMA & SCSI */
	cr_p = (u_int *)PHYS_TO_K1(CPU_CR_M2000);
	*cr_p = (u_int)( 0<<12 | 0<<14 | 0xffafe0);
	*cr_p = (u_int)( 1<<12 | 1<<14 | 0xffafe0);
	ch->ctl_cnt1 = (u_long)(  DMA_RESETB) ;
	wbflush();

	cdb = (u_char *)PHYS_TO_K1(CDB_ADDR);
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	
	cmd_group = 0;
	if( dma_ncr_drvr() == FAIL)
	    return(FAIL);

}

req_sense(sense)
struct  u_sense  *sense;
{
	cdb = (u_char *)PHYS_TO_K1(CDB_ADDR);
	*cdb++ = 0x03;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	
	r_ptr = (u_char *)sense ;
	t_length = 4;
	if( dma_ncr_drvr() == FAIL)
	    return(FAIL);
}	






c0_inq(inq)
struct u_inq *inq;
{
	cdb = (u_char *)PHYS_TO_K1(CDB_ADDR);
	*cdb++ = 0x12;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x00;
	*cdb++ = 0x04;
	*cdb++ = 0x00;
	
	r_ptr = (u_char *)inq ;
	t_length = 4;
	if( dma_ncr_drvr() == FAIL)
	    return(FAIL);
}	

/*
 *
 * Name : wait_for_ncr_int
 * Description : waits for interrupts from NCR53c94
 *
 *
 */
wait_for_ncr_int(time)
u_long time;
{
	u_long i;


	for( i = 0 ; i < time ; i++ ) {
	   /* if( GetIntR() & IR_NCR_INT_B ) */
	   if( !(ncr->status_id & NCR_STA_INT) ){
	     continue;
	   }
	   else {
	     ncr_r.fifoflag = ncr->fifoflag_syncoffset;
	     ncr_r.status = ncr->status_id;
	     ncr_r.interrupt = ncr->intr_timeout;
	     if (ncr_r.interrupt & NCR_INT_DIS) {
		return(FAIL);
	     }
#ifdef   DEBUG
	     printf("\n-INT-\n");
#endif   DEBUG
	     dsply_r(); 
	     return( PASS ); 
	   }
	}
#ifdef   DEBUG
	printf("\nTimed Out\n");
#endif   DEBUG

	return( FAIL );
}


rcv_from_scsi( count, read_buf )
u_char count;
u_long *read_buf;
{
	u_int	cnt;
	volatile  u_long  i;
	u_int   j =0;
		/* set transfer count to 512
		 */
	ncr->lo_count = count % 256;
	wbflush();
	ncr->hi_count = count >> 8;
	wbflush();

		/* flush the FIFO
		 */
	ncr->cmd = NCR_CMD_FLUSH_FIFO | NCR_CMD_DMA;
	wbflush();

	cnt = ~(count/2);
	ch->ctl_cnt1 = (u_long)(DMA_RESETB);
	ch->maddr = (u_long)K1_TO_PHYS(read_buf);
	ch->ctl_cnt1 = (u_long)(( DMA_TO_MEMORYB & DMA_RUNB)
           | (cnt << 16));

		/* enable NCR
		 */
	ncr->cmd = NCR_CMD_XFR_INFO | NCR_CMD_DMA ;

	
	wbflush();
	if( wait_for_ncr_int( 0xc0000 ) == FAIL )
	     return( FAIL );


	return(PASS);

}









/*
 *
 *
 *
 */
rcv_status_byte()
{

	ncr->cmd = NCR_CMD_FLUSH_FIFO;
		
		/* initiator Command Complete sequence
		 */
	ncr->cmd = NCR_CMD_INI_CMPLT;

	if( wait_for_ncr_int( 0x80000) == FAIL ) {
#ifdef  DEBUG
	  printf("\nError: Timeout, Waiting For Target To Send The Status Byte.");
#endif  DEBUG
	  return( FAIL );
	}

	return( PASS );

}


dsply_r()
{
#ifdef  DEBUG
	printf("\nFL=%x,ST=%x,INT=%x,CNL=%x,CNH=%x,CMD=%x",
	ncr->fifoflag_syncoffset, ncr->status_id, ncr_r.interrupt, ncr->lo_count	, ncr->hi_count, ncr->cmd );
#endif  DEBUG
}


read_fifo()
{
	u_char i;
	volatile u_char fifo_cnt;

#ifdef  DEBUG
	printf("\nFIFO=");
	fifo_cnt = (ncr->fifoflag_syncoffset & NCR_FIFO_CNT_MSK );
	if( !fifo_cnt )
	  return(PASS);

	for( i = 0 ; i < fifo_cnt ; i++ ) {
	   printf("%x,", ncr->fifo );
	}
#endif  DEBUG
}


rcv_msg_byte()
{

	u_char phase ;
	u_long i, rcv_count;

		/* 
		 */
	ncr->cmd = NCR_CMD_XFR_INFO;

		/* wait for interrupt
		 */
	if( wait_for_ncr_int( 0x80000) == FAIL ) {
#ifdef  DEBUG
	  printf("\nError: Timeout, Waiting For Target To Send The Status Byte.\n");
#endif  DEBUG
	  return( FAIL );
	}
#ifdef  DEBUG
	printf("MSG=%x", ncr->fifo );
#endif  DEBUG
	return( PASS );

}



send_to_scsi( count, write_buf )
u_long count;
u_long *write_buf;
{
	u_int   cnt;
		/* set transfer count to 512
		 */
	ncr->lo_count = count % 256;
	wbflush();
	ncr->hi_count = count >> 8;
	wbflush();

		/* flush the FIFO
		 */
	ncr->cmd = NCR_CMD_FLUSH_FIFO;
	wbflush();

	cnt = ~(count/2);
	ch->ctl_cnt1 = (u_long)(DMA_RESETB);
	wbflush();
	ch->maddr = (u_long)K1_TO_PHYS(write_buf);
	wbflush();
	ch->ctl_cnt1 = (u_long)(( DMA_RUNB) | (cnt << 16));
	wbflush();

		/* enable NCR
		 */
	ncr->cmd = NCR_CMD_XFR_INFO | NCR_CMD_DMA ;
	if( wait_for_ncr_int( 0x80000 ) == FAIL )
	     return( FAIL );


	return( PASS );


}


rcv_from_ncrsg( count, read_buf )
u_long count;
u_char *read_buf;
{
	register volatile struct	ncrsg	*sg;
	u_int	cnt,cnt_remain;
	volatile  u_long *rd_ptr,  i;
	u_long  j = 0;

		/* set transfer count to 512
		 */
	ncr->lo_count = count % 256;
	wbflush();
	ncr->hi_count =count >> 8;
	wbflush();

		/* flush the FIFO
		 */
	ncr->cmd = NCR_CMD_FLUSH_FIFO | NCR_CMD_DMA;
	wbflush();

		/* enable NCR
		 */
	ncr->cmd = NCR_CMD_XFR_INFO | NCR_CMD_DMA ;
	wbflush();

	rd_ptr = (u_long *)read_buf;
	cnt_remain = count >> 1;
	/* Setup scatter gather structs in memory */
	cnt = ~(page_size >> 1);
	ch->ctl_cnt1 = (u_long)(DMA_RESETB);
	sg = (struct ncrsg *)PHYS_TO_K1(SG_BASE);
	sg->maddr = (u_long)K1_TO_PHYS(read_buf);
	sg->ctl_cnt = (u_long)(( DMA_TO_MEMORYB & DMA_RUNB 
          & DMA_ENA_CHAINB) | (cnt << 16));
	cnt_remain -= (page_size >> 1);

	while (cnt_remain > 0) {
		sg++;
		j++;
		sg->maddr = (u_long)K1_TO_PHYS(read_buf+page_size*j);
		sg->ctl_cnt = (u_long)(( DMA_TO_MEMORYB & DMA_RUNB 
          	  & DMA_ENA_CHAINB) | (cnt << 16));
		cnt_remain -= (page_size >> 1);
	}

	ch->caddr = (u_long)SG_BASE;
	if(wait_for_ncr_int(0xc0000) == FAIL)
		return(FAIL);
	return(PASS);


}



send_to_ncrsg( count, read_buf )
u_long count;
u_char *read_buf;
{
	register volatile struct	ncrsg	*sg;
	u_int	cnt,cnt_remain,j=0;

		/* set transfer count to 512
		 */
	ncr->lo_count = count % 256;
	wbflush();
	ncr->hi_count = count >> 8;
	wbflush();

		/* flush the FIFO
		 */
	ncr->cmd = NCR_CMD_FLUSH_FIFO | NCR_CMD_DMA;
	wbflush();

		/* enable NCR
		 */
	ncr->cmd = NCR_CMD_XFR_INFO | NCR_CMD_DMA ;
	wbflush();

	cnt_remain = count >> 1;
	/* Setup scatter gather structs in memory */
	cnt = ~(page_size >> 1);
	ch->ctl_cnt1 = (u_long)(DMA_RESETB);
	sg = (struct ncrsg *)PHYS_TO_K1(SG_BASE);
	sg->maddr = (u_long)K1_TO_PHYS(read_buf);
	sg->ctl_cnt = (u_long)((  DMA_RUNB 
          & DMA_ENA_CHAINB) | (cnt << 16));
	cnt_remain -= (page_size >> 1);

	while (cnt_remain > 0){	
		sg++;
		j++;
		sg->maddr = (u_long)K1_TO_PHYS(read_buf+page_size*j);
	  	sg->ctl_cnt = (u_long)((  DMA_RUNB 
          	  & DMA_ENA_CHAINB) | (cnt << 16));
		cnt_remain -= (page_size >> 1);
	}

	ch->caddr = (u_long)SG_BASE;
	if (wait_for_ncr_int( 0xc0000) == FAIL)
		return(FAIL);

	return( PASS );

}
 

struct all {
	struct scsiip iopb;
	struct u_inq un_inquiry;
	struct u_sense un_sense;
};

static struct all *all = (struct all *)PHYS_TO_K1(PON_SCRATCHMEM);

/*
 * This test verifies the Functions of the SCSI Protocol Controller.
 */
Pon_Dma_Scsi()

{
	register u_int error;

	if (machine_type != BRDTYPE_RB3125) {
		pon_puts(skipped);
	}

	pon_set_leds(PON_SCSISLAVE);
	pon_puts("SCSI Controller Chip Test...");
	error = scsi_reg();

	if(error) {
	  pon_puts( failure );
	  FastFlash(0);
	  SetDepend(PON_FAULT_SCSI);
	  return( FAIL );	
	}
	else {
	    pon_puts(success);
	    return(PASS);
	}
}








scsi_reg()
{
	
	register volatile u_char w, r;
	register u_int i, fail_cnt ;
	register u_int  *cr_value;


	/* reset scsi       */
	cr_value = (u_int *)PHYS_TO_K1(CPU_CR_M2000);
	(u_int)*cr_value = (0<<12)|0xffefe0;
	(u_int)*cr_value = (1<<12)|0xffefe0;
        

		/* reset NCR chip
		 */
	scsi_reset();
	
	fail_cnt = 0;

		/* verifiy registers
		 */
	for( i = 0, w = 1 ; i < 8 ; i++, w <<= 1 ) {
	   if( wrv_reg( &ncr->lo_count, w) == FAIL )
	     {
#ifdef	DEBUG
	     printf("\nncr->lo_count failed !");
#endif
	     fail_cnt++;
	     }
	   if( wrv_reg( &ncr->hi_count, w) == FAIL )
	     {
#ifdef  DEBUG
	     printf("\n ncr->hi_count failed !");
#endif
	     fail_cnt++;
	     }
	   if( wrv_reg( &ncr->config1, w) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config2, (w & 0x1f)) == FAIL )
	     fail_cnt++;
	}

	for( i = 0, w = 0 ; i < 2 ; i++, w = ~w ) {
	   if( wrv_reg( &ncr->lo_count, w) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->hi_count, w) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config1, w) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->config2, (w & 0x1f)) == FAIL )
	     fail_cnt++;
	   if( wrv_reg( &ncr->cmd, w) == FAIL )
	     fail_cnt++;
	}

	scsi_reset();

	if(scsi_fifo_t() == FAIL)
	  {
#ifdef  DEBUG
	     printf("\n scsi_fifo_t failed !");
#endif
	   fail_cnt++;
	  }
	if( scsi_bus_reset() == FAIL)
	  {
#ifdef  DEBUG
	     printf("\n scsi_bus_reset failed !");
#endif
	  fail_cnt++;
	  }

	return (fail_cnt);
}



/*
 *
 * Name : scsi_reset
 * Description : Resets the SCSI protocol Controller chip.
 *
 */

scsi_reset()
{
		/* reset NCR chip
		 */
	ncr->cmd = NCR_CMD_CHIP_RESET;
	scsi_delay( 0x200 );
	ncr->cmd = NCR_CMD_NOP;

	return(PASS);
}


/*
 *
 * Name : wrv_reg
 * Synopsis : wrv_reg( register_address, data_to_write, register_name )
 * Description : Writes Reads and verifies a register.
 *
 */


wrv_reg( reg_address, w, reg_name )
u_char *reg_address;
u_char w;
char *reg_name;
{
	register volatile u_char r;

	*reg_address = w;
	if( (reg_address == &ncr->lo_count) || ( reg_address == &ncr->hi_count) )
	  ncr->cmd = NCR_CMD_DMA;

	r = *reg_address ;

	if( r != w ) {
	  printf("\nError: Incorrect Data Read From %s register,\n        Exp= 0x%02x, Actual= 0x%02x\n",
		      reg_name, w, r );
	  return( FAIL );
	}

	*reg_address = 0;
	if( (reg_address == &ncr->lo_count) || (reg_address == &ncr->hi_count) )
	  ncr->cmd = NCR_CMD_DMA;

	return( PASS );
}




/*
 *
 * Name : scsi_bus_reset
 * Description : resets the SCSI bus
 */
scsi_bus_reset()
{
	register u_char i;
	register u_short isr_value,imr_value;
	register u_int  *cr_p;



		/* disable CPU interrupts
		 */
	/*DisableInt();*/

		/* check interrupts from NCR
		 */
	if( ncr->status_id & NCR_STA_INT ) {
	  goto f_scsi_bus_reset; 
	}

		/* check vme interrupt register
		 */
	(u_short)isr_value = *(u_short *)PHYS_TO_K1(VME_ISR_RB3125);
	if(isr_value  & ISR_SCSIINTB  )  {
	  goto f_scsi_bus_reset;
	}

		/* enable interrupts from SCSI
		 */
	*(u_short *)PHYS_TO_K1(VME_IMR_RB3125) = 0x1000;
	/* Don't enable cpu stauts reg ,otherwise will go to int handling
	   routine
	 */
	/*SetSR(GetSR() | SR_IBIT3 | SR_IEC);*/
		/* hardware reset SCSI
		 */
	cr_p = (u_int *)PHYS_TO_K1(CPU_CR_M2000);
	(u_int) *cr_p = (0<<12) | 0xffefe0;
	(u_int) *cr_p = (1<<12) | 0xffefe0;
 
		/* reset SCSI Bus
		 */
	ncr->cmd = NCR_CMD_SCSI_RESET;

		/* wait for interrupt from SCSI
		 */
	for( i = 0 ; i < MAX_T_DELAY ; i++ )  {
	    if( ncr->status_id & NCR_STA_INT )
	      break;
	}

		/* verify If timeout occurred
		 */
	if( i == MAX_T_DELAY ) {
	  goto f_scsi_bus_reset;
	}

	/*if( !(GetCause() & CAUSE_IP3) ) {
	  goto f_scsi_bus_reset;
	}*/

		/* check interrupt register
		 */
	(u_short)isr_value = *(u_short *)PHYS_TO_K1(VME_ISR_RB3125);
	if(!( isr_value & ISR_SCSIINTB) ) {
	  goto f_scsi_bus_reset;
	}
		/* read NCR interrupt register, this clears all status, and interrupt reg. bits
		 */
	i = ncr->intr_timeout;

		/* verify SCSI Bus Reset bit In Interrupt Reg.
		 NCR_INT_BUS_RST
		 */
	if( !(i & NCR_INT_BUS_RST) ) {
	  goto f_scsi_bus_reset;
	}

		/* verify interrupt is cleared
		 */
	(u_short)isr_value = *(u_short *)PHYS_TO_K1(VME_ISR_RB3125);
	if(isr_value & ISR_SCSIINTB ) {
	  goto f_scsi_bus_reset;
	}
	return( PASS );

f_scsi_bus_reset:
	ncr->intr_timeout;
	return( FAIL );

}



/*
 *
 * Name : scsiCleanUp
 *
 */
scsiCleanUp()
{
	ncr->cmd = NCR_CMD_NOP;
}




/*
 *
 * Name : scsi_fifo_t
 * Description : Checks the 16 byte FIFO of NCR-53C94
 */
scsi_fifo_t()
{

	register volatile u_char r,w,i;
	register u_int 	  *cr_p;


		/* hardware reset SCSI
		 */
	cr_p = (u_int *)PHYS_TO_K1(CPU_CR_M2000);
	(u_int) *cr_p = (0<<12) | 0xffefe0;
	(u_int) *cr_p = (1<<12) | 0xffefe0;
		/* reset SCSI chip
		 */
	scsi_reset();
		
		/* Verify NCR FIFO
		 */
	for( i = 0, w = 1 ; i < NCR_FIFO_BSIZE ; i++, w += 1 ) {
	   ncr->fifo = w;
	   if( (ncr->fifoflag_syncoffset & NCR_FIFO_CNT_MSK) != (i+1) ) {
	     return( FAIL );
	   }
	}

	for( i = NCR_FIFO_BSIZE, w = 1 ; i != 0 ; i--, w += 1 ) {
	   if( (r=ncr->fifo) != w ) {
	     return( FAIL );
	   }
	   if( (ncr->fifoflag_syncoffset & NCR_FIFO_CNT_MSK) != (i-1) ) {
	     return( FAIL );
	   }
	}

	scsi_reset();

	return( PASS );
}



/*
 * delay routine
 *
 */
scsi_delay(n)
u_long n;
{
    u_long i, j;
   
    for (i=0; i<n ; i++) {
	for ( j=0 ; j<n ; j++)
	    ;
    }
    
}




init_ncr()
{

		/* reset the chip
		 */
	scsi_reset();

		/* program clock conversion factor
		 */
	ncr->clockfactor = 5;

		/* set the self Bus ID
		 */
	ncr->config1 = 0x07;


	return( PASS );
}









