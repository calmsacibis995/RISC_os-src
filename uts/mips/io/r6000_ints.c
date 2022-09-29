/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990 MIPS Computer Systems, Inc.            |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restricted Rights Legend                         |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: r6000_ints.c,v 1.5.1.8.1.7.1.3 90/10/17 18:05:46 beacker Exp $"

#include "sys/param.h"
#include "sys/cpu_board.h"
#include "sys/ctlspace.h"
#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/bc.h"
#include "sys/ioa.h"
#include "sys/immu.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/reg.h"
#include "sys/systm.h"

extern du_poll(), ioa_error_intr(), mem_error_intr();

extern int (*ctl_vec_tbl[])();

static CtlMisc *ctl_misc = (CtlMisc *)(CSR_CTLMISC);

extern int ioa_ctlspace_vaddr[];
extern int memory_ctlspace_vaddr[];	/* virt addr of Memorys' ctlspace */
extern uint memory_base_paddr[];	/* phys addr of each Memory board */
extern uint memory_board_mbsize[];	/* size in MBytes of each board */
int ioa_count;
int memory_count;
int ioa_gba_vaddr[SBC_SLOT_UPB][2];	/* virt addr of the 16MB Gba space */
					/*  indexed by ioa_number-1 and    */
					/*  gba_number; !=0 if exists      */

int vme_berr_ok = 0;			/* Set when we're in iobadaddr */

static uint renableMask[32] = {
     0x00000000,	/* highest pri event renables nothing */
     0x00000001, 0x00000003, 0x00000007, 0x0000000F,
     0x0000001F, 0x0000003F, 0x0000007F, 0x000000FF,
     0x000001FF, 0x000003FF, 0x000007FF, 0x00000FFF,
     0x00001FFF, 0x00003FFF, 0x00007FFF, 0x0000FFFF,
     0x0001FFFF, 0x0003FFFF, 0x0007FFFF, 0x000FFFFF,
     0x001FFFFF, 0x003FFFFF, 0x007FFFFF, 0x00FFFFFF,
     0x01FFFFFF, 0x03FFFFFF, 0x07FFFFFF, 0x0FFFFFFF,
     0x1FFFFFFF, 0x3FFFFFFF,
     0x7FFFFFFF		/* lowest pri int renables all but itself */
};

/*
 * Handle an External Interrupt
 *
 * The CPU SR has been setup to disallow other External Interrups on entry
 * to this routine.  Interrupts of higher priority (i.e. fp_intr) have
 * already been re-enabled in the SR.
 *
 * This routine will determine which external interrupt has occurred
 * and will disable this interrupt and all those of lower priority
 * using the CSR_IVECTMASK. It will then re-enable the External
 * interrupt in the SR so higher priority interrupts can be taken
 * during the processing of the current interrupt. 
 */
void
r6000_intr(ep)
uint *ep;
{
    uint origMask;
    int  bit;

    /*
     *  Disable all interrupts by clearing the SBC IntVect mask.
     */
    origMask = splall();	/* disable ints, and return IntVect mask */
    /*
     *  Now ensure that the SR gets reset so that "external" interrupts
     *  are recognized -- if the SBC IntVect mask allows it!
     *  We are presuming that splx() resets the SR this way.
     */
    splx( splall() );

    while (bit = ffs( *(volatile uint *)CSR_IVECTSET & origMask )) {
    	/*
    	 * It looks universal in the interrupt handlers to clear the
    	 * Bus Chip InterruptVector bit at the beginning, before
    	 * calling the specific interrupt handler.
    	 */


#ifdef R6000_BUG_SBC_TIMER
	if (bit > 1)
	  r6000_bug_ivectclr( 1 << (bit-1) );
	else
#endif R6000_BUG_SBC_TIMER	
	  *(volatile uint *)CSR_IVECTCLR = 1 << (bit-1);

	*(volatile uint *)CSR_IVECTMASK =
	   origMask  & renableMask[bit-1];  /* Enable high pri ints */
	
        /*************************************************************/
        /* An array of masks offers us flexibility to turn off	     */
	/* multiple sources if we need to.  Otherwise,               */
        /* above line could be replaced by:                          */
        /*  origMask & (0x7fffffff >> (32-bit));                     */
        /*************************************************************/

    	(*ctl_vec_tbl[ bit-1 ])(ep);

    }
    *(volatile uint *)CSR_IVECTMASK = origMask;
}


/*
 *  Interrupt handler for ignoring an interrupt for which the real
 *  interrupt handler isn't yet prepared to handle.
 */
null_intr(ep)
int *ep;
{
    return(0);
}


/*
 *  Interrupt handler for IOA Error interrupts
 */
ioa_error_intr(ep)
int *ep;
{
    int		ioa;
    u_int	errorinfo_reg, gba0_status_reg, gba1_status_reg;
    u_int	stat_reg0, stat_reg1;
    int		printed_general_msg;
    extern int	nofault;
    extern struct reg_desc ioc_errorinfo_desc[];
    extern struct reg_values gba_pty_err_mask_values[];
    extern struct reg_desc gba_status_desc[];

    printed_general_msg = 0;	/* not yet */

    for (ioa = 0; ioa < ioa_count; ioa++) {

	gba0_status_reg = gba1_status_reg = 0;  /* no error */

	errorinfo_reg = *(u_int *)(ioa_ctlspace_vaddr[ioa] | IOA_ERRORINFO_REG);
	stat_reg0 = *(u_int *)(ioa_ctlspace_vaddr[ioa] | IOA_STATUS_REG0);
	stat_reg1 = *(u_int *)(ioa_ctlspace_vaddr[ioa] | IOA_STATUS_REG1);

	if (errorinfo_reg & GBA0_EXISTS) {
	    gba0_status_reg = *(u_int *)(ioa_ctlspace_vaddr[ioa]
					 | GBA0_MISC_BASE | GBA_STATUS);
	    if (vme_berr_ok && (gba0_status_reg & GBA_TIMEOUT)) {
		/* ignore Timeouts resulting from iobadaddr() */
		errorinfo_reg   &= ~(G0_SEC_ERR  | G0_ERR);
	    }
	    vme_level0_intr(ioa, 0, 0);	/* Handle GBA error conditions */
	    *(u_int *)(ioa_ctlspace_vaddr[ioa] | IOA_STATUS_REG0) = 0;
	}
	if (errorinfo_reg & GBA1_EXISTS) {
	    gba1_status_reg = *(u_int *)(ioa_ctlspace_vaddr[ioa]
					 | GBA1_MISC_BASE | GBA_STATUS);
	    if (vme_berr_ok && (gba1_status_reg & GBA_TIMEOUT)) {
		/* ignore Timeouts resulting from iobadaddr() */
		errorinfo_reg   &= ~(G1_SEC_ERR  | G1_ERR);
	    }
	    vme_level0_intr(ioa, 1, 0);	/* Handle GBA error conditions */
	    *(u_int *)(ioa_ctlspace_vaddr[ioa] | IOA_STATUS_REG1) = 0;
	}

	if (errorinfo_reg & (G0_SEC_ERR		| G1_SEC_ERR
			    |G0_ADMUX_PTY_ERR	| G1_ADMUX_PTY_ERR
			    |G0_DOUT_PTY_ERR	| G1_DOUT_PTY_ERR
			    |G0_BUS_ERR | G1_BUS_ERR |G0_ERR | G1_ERR)) {

	    if (printed_general_msg == 0) {
		cmn_err(CE_WARN,"IOA Error interrupt");
		printed_general_msg = 1;
	    }
	    cmn_err( CE_CONT,"IOA%d ErrorInfo: %R \n"
		    ,(ioa+1), errorinfo_reg, ioc_errorinfo_desc );

	    cmn_err( CE_CONT,"     StatReg0: 0x%x ", stat_reg0 );
	    cmn_err( CE_CONT," State 0x%x ", (stat_reg0 >> 19) & 0x1f );
	    cmn_err( CE_CONT," Orig 0x%x ", (stat_reg0 >> 17) & 0x01);
	    cmn_err( CE_CONT," BlkOp 0x%x ", (stat_reg0 >> 10) &0x01);
	    cmn_err( CE_CONT," XmitOKRcvd 0x%x ", (stat_reg0 >> 9) &0x01);
	    cmn_err( CE_CONT," RespBack 0x%x\n", (stat_reg0 >> 8) &0x01);

	    cmn_err( CE_CONT,"     StatReg1: 0x%x ", stat_reg1 );
	    cmn_err( CE_CONT," State 0x%x ", (stat_reg1 >> 19) & 0x1f );
	    cmn_err( CE_CONT," Orig 0x%x ", (stat_reg1 >> 17) & 0x01);
	    cmn_err( CE_CONT," BlkOp 0x%x ", (stat_reg1 >> 10) &0x01);
	    cmn_err( CE_CONT," XmitOKRcvd 0x%x ", (stat_reg1 >> 9) &0x01);
	    cmn_err( CE_CONT," RespBack 0x%x\n", (stat_reg1 >> 8) &0x01);
	}

    }

    return(0);
}


sbc_rev_level( ctlspace_addr )
int ctlspace_addr;
{
    int slot, chip_rev, maj_rev, min_rev;
    
    slot = (ctlspace_addr >>16) & 0x0f;	/* extract the slot number */
    chip_rev = (*(u_int *)(ctlspace_addr + SBC_ERRREG) >> 6) & 0x0f;

    /* Currently defined chip revisions:
     * chip_rec   SBC revision
     *   0  	<= 2.0
     *   1         2.2
     *   2         2.3
     *   3         3.0
     *   4-15      Undefined
     */
    if (chip_rev <= 2) {
      maj_rev = 2;
      min_rev = chip_rev + 1;
    } else {
      maj_rev = 3;
      min_rev = chip_rev - 3; 
    }
    if (showconfig)
      cmn_err(CE_CONT, "SBC: MIPS R6020 System Bus Chip %d.%d in slot %d\n",
	      maj_rev, min_rev, slot);
    else
      cmn_err(CE_CONT, "!SBC: MIPS R6020 System Bus Chip %d.%d in slot %d\n",
	      maj_rev, min_rev, slot);
}

showconfig_r6000_boards()
{
    int		slot;
    int		ctlspace_addr;

    /*
     * locate the CtlSpace addresses for all SBC
     */

    ctlspace_addr = CTL_SPACE + CTL_SPACE_UNIT_BSIZE;	/* slot #1 */

    slot = 1;
    while (slot <= SBC_SLOT_UPB  &&
	   !badaddr( ctlspace_addr + SBC_BRDADDR, 4 )) {
      /*  there is something in this slot  */
      
      sbc_rev_level( ctlspace_addr );
      ctlspace_addr += CTL_SPACE_UNIT_BSIZE;	/* next one */
      slot++;
    }
}

find_r6000_boards()
{
    int		slot;
    u_int	brdtype;
    int		idprom_brdtype_addr,
		ctlspace_addr;
    register 	MemCtl mem_memctl;	/* SBC mem control reg */

    /*
     * locate the CtlSpace addresses for the Memory and IOC boards
     */

    for (slot = 0; slot < SBC_SLOT_UPB; slot++) {	/* start fresh */
	memory_ctlspace_vaddr[slot] = 0;
	ioa_ctlspace_vaddr[slot]    = 0;
    }

    ctlspace_addr = CTL_SPACE + CTL_SPACE_UNIT_BSIZE;	/* slot #1 */
    ioa_count     = 0;
    memory_count  = 0;

    slot = 1;
    while (slot <= SBC_SLOT_UPB  &&
	   !badaddr( ctlspace_addr + SBC_BRDADDR, 4 )) {
	/*  there is something in this slot  */
#ifdef R6000_BUG_IDPROM
	{
	    int i;
	    i = *(u_int *)(ctlspace_addr + SBC_CTLMISC);
	    if (i & BrdIsMem)
		brdtype = BRDTYPE_R6350;
	    else
	    if (i & BrdHasBootProm)		/* require all IOCs to  */
		brdtype = BRDTYPE_R6360;	/*  have BootProm       */
	    else
		brdtype = BRDTYPE_R6300;
	}
#else
	brdtype = *(u_char *)(ctlspace_addr + SBC_IDPROM+4+ID_BRDTYPE_OFF);
#endif R6000_BUG_IDPROM

	switch (brdtype) {

	    case BRDTYPE_R6350:
		/* MEMORY */
		memory_ctlspace_vaddr[ memory_count ] = ctlspace_addr;
		if (!sbc_addr32( *(volatile uint *)(ctlspace_addr + SBC_BRDADDR)
				,&memory_base_paddr[ memory_count ]
				,&memory_board_mbsize[ memory_count ] )) {
		    cmn_err( CE_PANIC,
			     "Memory in slot %d has invalid BoardAddress",
			     slot );
		}
		memory_count++;

		/*
		 *  Initialize the "Memory Error" interrupt vector in the SBC,
		 *  and prepare to handle "Memory Error" interrupts
		 */
		*(volatile u_int *)CSR_IVECTCLR = CSR_IVECTSET_MEM_ERR;
		ctl_vec_tbl[ ffs(CSR_IVECTSET_MEM_ERR) - 1 ] = mem_error_intr;
		*(volatile u_int *)(ctlspace_addr + SBC_MEMECC) = BankMask;
		mem_memctl.wd	  = 0;			/* start fresh */
		mem_memctl.f.islt = ctl_misc->f.sltno;	/* CPU's slot number */
		mem_memctl.f.ibit = CSR_IVECTSET_MEM_ERR_BIT;
		mem_memctl.wd |= EnaSErrLocCmp | EnaSErrCorrect | EnaSErrInt |
				  EnaMErrDet	| MemIntEna;	/* enable int */
		*(MemCtl *)(ctlspace_addr + SBC_MEMCTL) = mem_memctl;
		break;

	    case BRDTYPE_R6300:
		/* CPU */
		break;

	    case BRDTYPE_R6360:
		/* IOC */
		ioa_ctlspace_vaddr[ ioa_count ] = ctlspace_addr;
		ioa_gba_vaddr[ ioa_count ][0] = 0;
		ioa_gba_vaddr[ ioa_count ][1] = 0;
		ioa_count++;
		break;

	    default:
		cmn_err(CE_WARN,"unknown BRDTYPE %d at slot %d \n"
			,brdtype,slot);

	} /* switch */

	ctlspace_addr += CTL_SPACE_UNIT_BSIZE;	/* next one */
	slot++;
    }
}


/*
 * Form an interrupt vector triplet for a unit to generate an interrupt to
 * this CPU.  The triplet consists of two words containing the target physical
 * address, and one word of data.  The target physical address is this CPU's
 * CtlSpace InterruptVectorSet word.
 */
ioa_deposit_intr_vector( intr_vector_addr, target_data )
uint intr_vector_addr;
uint target_data;
{
    uint target_addr;

    /*
     *  Form the physical target_addr: the target of the singleword write.
     */
    target_addr = CTL_SPACE_PHYS
    		+ (ctl_misc->f.sltno * CTL_SPACE_UNIT_BSIZE)
    		+ SBC_REGISTERS;

    *(volatile uint *)intr_vector_addr = target_addr & (NBPP-1);
    *(volatile uint *)(intr_vector_addr + 4)
		 = (target_addr >> PNUMSHFT) << PTE_PNUMSHFT;
    *(volatile uint *)(intr_vector_addr + 8) = target_data;
} 

static
du_init_r6000( ctlspace, duart_int )
int	ctlspace;
u_int	duart_int;
{
    ioa_deposit_intr_vector( ctlspace | IOA_DUART_INT_CMD1, duart_int );

    ctl_vec_tbl[ ffs(CSR_IVECTSET_DUART) - 1 ] = du_poll;

    *(volatile uint *)CSR_IVECTCLR   = duart_int;
    *(volatile uint *)CSR_IVECTMASK |= duart_int;

    *(u_int *)(ctlspace | IOA_ERRORINFO_REG) = IOC_DUART_INT; /* clear */
}


static
ioa_init( ctlspace, ioa_number, gba0_int, gba0_lop_int, gba1_int, gba1_lop_int )
int	ctlspace;
int	ioa_number;
u_int	gba0_int;
u_int	gba0_lop_int;
u_int	gba1_int;
u_int	gba1_lop_int;
{
    int	bit;
    register MemCtl *ioc_memctl;	/* pointer to SBC mem control reg */

    /*
     *  Initialize the interrupt Constants
     */
    for (bit = 0; bit < 32; bit++) {
	*(volatile u_int *)(ctlspace + IOA_CONSTANTS + bit*sizeof(u_int))
	    = 1 << bit;
    }

    /*
     *  Initialize the "IOA Error" interrupt vector in the SBC,
     *  and prepare to handle "IOA Error" interrupts
     */
    *(volatile u_int *)CSR_IVECTCLR = CSR_IVECTSET_IOA_ERR;	/* clear */
    ioc_memctl = (MemCtl *)(ctlspace + SBC_MEMCTL);
    ioc_memctl->wd     = 0;			/* start fresh */
    ioc_memctl->f.islt = ctl_misc->f.sltno;	/* CPU's slot number */
    ioc_memctl->f.ibit = CSR_IVECTSET_IOA_ERR_BIT;
    ctl_vec_tbl[ ffs(CSR_IVECTSET_IOA_ERR) - 1 ] = ioa_error_intr;
    ioc_memctl->f.iena = 1;			/* enable interrupts */

    /*
     *  Initialize GBA operations.
     */
    if (*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG) & GBA0_EXISTS) {
	gba_init_r6000( ctlspace, ioa_number, 0, gba0_int, gba0_lop_int );
    }
    if (*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG) & GBA1_EXISTS) {
	gba_init_r6000( ctlspace, ioa_number, 1, gba1_int, gba1_lop_int );
    }

    /*
     *  Clear any pending external interrupts from the GBAs
     */
    *(volatile u_int *)CSR_IVECTCLR
	  = (gba0_int | gba1_int | gba0_lop_int | gba1_lop_int);
}


io_init_r6000()
{
    find_r6000_boards();

    if (ioa_ctlspace_vaddr[0]) {
	ioa_init( ioa_ctlspace_vaddr[0], 1
		 ,CSR_IVECTSET_IOA1_GBA0_OP, CSR_IVECTSET_IOA1_GBA0_LOP
		 ,CSR_IVECTSET_IOA1_GBA1_OP, CSR_IVECTSET_IOA1_GBA1_LOP );

	du_init_r6000( ioa_ctlspace_vaddr[0], CSR_IVECTSET_DUART );
    }
    if (ioa_ctlspace_vaddr[1]) {
	ioa_init( ioa_ctlspace_vaddr[1], 2
		 ,CSR_IVECTSET_IOA2_GBA0_OP, CSR_IVECTSET_IOA2_GBA0_LOP
		 ,CSR_IVECTSET_IOA2_GBA1_OP, CSR_IVECTSET_IOA2_GBA1_LOP );
    }
    if (ioa_ctlspace_vaddr[2]) {
	ioa_init( ioa_ctlspace_vaddr[2], 3
		 ,CSR_IVECTSET_IOA3_GBA0_OP, CSR_IVECTSET_IOA3_GBA0_LOP
		 ,CSR_IVECTSET_IOA3_GBA1_OP, CSR_IVECTSET_IOA3_GBA1_LOP );
    }
}



/****************************************************************************
 *  sbc_addr32 (brd_addr, base_mbyte_address, mbyte_size)
 *
 *  Takes a Bus Chip Board Address value and returns a 32 bit physical
 *  base address and a 32 bit size (in bytes).  Address is returned as a
 *  K1 seg address.
 *
 *  Function returns 1 for valid board address, 0 for invalid.
 *
 *  Routine callable externally, has no dependence on any data structures
 *  or hardware.
 ***************************************************************************/

int
sbc_addr32 (brd_addr, phys_addr, mbyte_size)

uint brd_addr;
uint *phys_addr;
uint *mbyte_size;

{
  int i;

  *phys_addr = 0;

  for (i=0; i<16; i++ ) {
    if (brd_addr & 0x80000000) {
      if (brd_addr & 0x40000000)
	return(0);		/* 11 == illegal combination */
      else
	*phys_addr = *phys_addr << 1;		/* 10 == zero in address */
    }
    else
      if (brd_addr & 0x40000000)
	*phys_addr = (*phys_addr << 1) + 1;	/* 01 == one in address */
      else {			/* 00 == don't care in address */
	if (brd_addr != 0)	/* All remaining bits should be dont care */
	  return(0);		/*  or address is illegal */
	
	*phys_addr = *phys_addr << (36-i);
	*mbyte_size = 1 << (16-i);
	return( 1 );
      }
    brd_addr = brd_addr << 2;
  }

  *phys_addr = *phys_addr << 20;	/* convert Mbyte- to byte-address */
  *mbyte_size = 1<<20;			/* convert Mbytes to bytes */
  return( 1 );
}


/****************************************************************************
 *
 * Check if I/O address is valid/
 *
 * Should be invoked by device drivers to determine if controller is
 * present, rather than invoking badaddr directly.  This avoids an attempt
 * to access a non-existent GBA rather than requiring that such an access
 * cause a bus error.  If GBA is present, let badaddr attempt the actual
 * data access.
 *
 ***************************************************************************/

iobadaddr( dev_addr, size )
     uint dev_addr;	/* address of IOA on system bus */
     uint size;	/* size of access (in bytes: 1,2, or 4)	*/

{
    int ioa, gba, retval;
  
    /*
     *  Is the dev_addr in the range of any known GBA?
     */
    for (ioa=0; ioa < ioa_count; ioa++ ) {

	for (gba = 0; gba < 2; gba++) {

	    if (dev_addr >= ioa_gba_vaddr[ioa][gba]		&&
		dev_addr <  ioa_gba_vaddr[ioa][gba] + GBA_SPAN) {

	      		vme_berr_ok = 1;
	      		retval = badaddr(dev_addr, size);
			vme_berr_ok = 0;
			return(retval);
	    }
	} /* for each gba */

    } /* for each ioa */
  
    /*
     *  No, dev_addr is not a known GBA address.
     *  Don't access a nonexistent GBA!
     */

    return(1);	/* I/O address is bad -- no GBA present specified dev_addr */
}
