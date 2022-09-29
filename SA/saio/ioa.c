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
#ident "$Header: ioa.c,v 1.16.4.2 90/12/19 16:09:42 chungc Exp $"

/*
 * ioa.c -- R6360 IOC and GBA management
 */

#include "sys/types.h"
#include "mips/bc.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "mips/ctlspace.h"
#include "mips/ioa.h"
#include "mips/param.h"
#include "mips/vmereg.h"
#include "prom/prom.h"  

#define NBPP_R6000		16384

u_int	ioa_ctlspace_vaddr[SBC_SLOT_UPB];	/* #slots max */
static int ioa_gba_vaddr[SBC_SLOT_UPB][2];	/* virt addr of the 16MB Gba */
						/*  space, indexed by ioa-1  */
						/*  and gba; !=0 if exists */
int	gba_count;

static CtlMisc *ctl_misc = (CtlMisc *)(CSR_CTLMISC);

find_r6000_ioa_boards()
{
    int		slot;
    int		ioa_count;
    int		idprom_brdtype_addr,
		ctlspace_addr;
    volatile u_int sbc_brdtype;

    /*
     * locate the CtlSpace addresses for the IOC boards
     */
    for (ioa_count = 0; ioa_count < SBC_SLOT_UPB; ioa_count++) {
	ioa_ctlspace_vaddr[ ioa_count ] = 0;	/* initialize to zero */
	ioa_gba_vaddr[ ioa_count ][ 0 ] = 0;
	ioa_gba_vaddr[ ioa_count ][ 1 ] = 0;
    }

    ctlspace_addr = CTL_SPACE + CTL_SPACE_UNIT_BSIZE;	/* slot #1 */
    ioa_count     = 0;
    slot	  = 1;
    while ( slot <= SBC_SLOT_UPB  &&
	    !badaddr( ctlspace_addr + SBC_BRDADDR, 4 ) ) {
#ifdef R6000_BUG_IDPROM
	    {
		u_int i;
		i = *(u_int *)(ctlspace_addr + SBC_CTLMISC);
		if (i & BrdHasBootProm)			/* require all IOCs  */
		    sbc_brdtype = BRDTYPE_R6360;	/*  to have BootProm */
		else
		if (i & BrdIsMem)
		    sbc_brdtype = BRDTYPE_R6350;
		else
		    sbc_brdtype = BRDTYPE_R6300;
	    }
#else
	    sbc_brdtype = *(volatile u_int *)(ctlspace_addr + SBC_IDPROM+4);
#endif R6000_BUG_IDPROM

	    switch (sbc_brdtype) {

	    case BRDTYPE_R6350:
		/* MEMORY */
		break;

	    case BRDTYPE_R6300:
		/* CPU */
		break;

	    case BRDTYPE_R6360:
		/* IOC */
		ioa_ctlspace_vaddr[ ioa_count ] = ctlspace_addr;
		ioa_count++;
		break;

	    default:
		printf("slot %d unknown BRDTYPE: %d (0x%x)\n"
		       ,slot, sbc_brdtype, sbc_brdtype);

	    } /* switch */

	slot++;
	ctlspace_addr += CTL_SPACE_UNIT_BSIZE;	/* next one */
    } /* while still finding Boards */
}


/*
 * Form an interrupt vector triplet for a unit to generate an interrupt to
 * this CPU.  The triplet consists of two words containing the target physical
 * address, and one word of data.  The target physical address is this CPU's
 * CtlSpace InterruptVectorSet word.
 */
ioa_deposit_intr_vector( intr_vector_addr, target_data )
u_int intr_vector_addr;
u_int target_data;
{
    u_int target_addr;

    /*
     *  Form the physical target_addr: the target of the singleword write.
     */
    target_addr = CTL_SPACE_PHYS
    		+ (ctl_misc->f.sltno * CTL_SPACE_UNIT_BSIZE)
    		+ SBC_REGISTERS;

    *(volatile u_int *)intr_vector_addr = target_addr & (NBPP_R6000-1);
    *(volatile u_int *)(intr_vector_addr + 4)
		 = (target_addr >> VPNSHIFT_R6000) << TLB_PFNSHIFT_R6000;
    *(volatile u_int *)(intr_vector_addr + 8) = target_data;
} 


gba_init_r6000( ctlspace, ioa_number, gba_number, gba_int, gba_lop_int )
int	ctlspace;
int	ioa_number;
int	gba_number;
u_int	gba_int;
u_int	gba_lop_int;
{
    int			intr_number,
			word;
    u_int		pfn;
    u_int		vme_addr;
    u_int		section_info_value;
    volatile u_int	*sysmap,
    			*gbamap;
    volatile u_int	*cache_addr,
			*cache_data,
			*cache_tag,
			*section_info,
			*cache_dirty;


    /*
     *  Turn off Reset Mode
     */
    *(volatile u_int *)(ctlspace + GBACTL(gba_number) + GBA_STATUS) = 0;

    /*
     *  Initialize all 16 LongOp registers.
     */
    for (intr_number = 0; intr_number < 16; intr_number++) {
        *(volatile u_int *)(ctlspace + GBA_LOP(gba_number)
				     + IOC_REG(intr_number)) = 0;
    }

    /*
     *  Initialize all 16 LongOp Interrupt Vector locations.
     */
    for (intr_number = 0; intr_number < 16; intr_number++) {
        ioa_deposit_intr_vector( ctlspace + LOP_INT(gba_number)
					  + IOC_REG(intr_number)
				,gba_lop_int );
    }

    /*
     *  Initialize all GbaInt Interrupt Vector locations.
     *  The GBA reports problems through a level-0 interrupt, so we'll
     *  handle that through the same handler as the "IOA Error" interrupts.
     */
    ioa_deposit_intr_vector( ctlspace + GBA_INT(gba_number) + IOC_REG(0)
			    ,CSR_IVECTSET_IOA_ERR );

    for (intr_number = VME_MINIPL; intr_number <= VME_MAXIPL; intr_number++) {
	ioa_deposit_intr_vector( ctlspace + GBA_INT(gba_number)
					  + IOC_REG(intr_number)
				,gba_int );
    }

    /*
     *  Clear the GBA cache:  reset the tags to zero (not-valid and not-dirty),
     *  reset the per-line dirty bits, and initialize the per-section Control
     *  to Cacheable, Allow-Partials.
     */
    cache_addr   = (u_int *)(ctlspace + GBACTL(gba_number) + GBA_DATA_ADDR);
    cache_data   = (u_int *)(ctlspace + GBACTL(gba_number) + GBA_CACHE);
    cache_tag    = (u_int *)(ctlspace + GBACTL(gba_number) + GBA_TAGS);
    cache_dirty  = (u_int *)(ctlspace + GBACTL(gba_number) + GBA_DIRTY_BITS);
    section_info = (u_int *)(ctlspace + GBACTL(gba_number) + GBA_SECT_INFO);

    section_info_value = ~(GS_UNCACHED_WRT | GS_LOCAL_WRT |
			   GS_RANDOM_WRT | GS_NO_PARTIAL_WRT)
			& GS_SECTIONINFO_MASK_WRT;

    for (vme_addr = 0; vme_addr < GBA_SPAN; vme_addr += GBA_SECTION_SPAN) {
	/*
	 *  Initialize both cache lines of each section.
	 */
	*section_info = section_info_value;
	for (word = 0; word < GBA_LINES_PER_SECT*GBA_LINE_SIZE; word += 4) {
	    *cache_addr  = vme_addr + word;	/* per word of cache line */
	    if ((word & (GBA_LINE_SIZE-1)) == 0) {	/* 1st word of line? */
		*cache_tag  = 0;		/* not-Valid, not-Dirty */
	    }
	    *cache_dirty = 0;			/* all four bytes not-dirty */
	    *cache_data  = 0;			/* nice, clean data, too! */
	} /* for each word of the section */
    } /* for each cache section */

    /*
     *  Initialize the GBA Address Compare register.
     */
    *(volatile u_int *)(ctlspace + GBACTL(gba_number) + GBA_VME_ADR_CMP)
	       = 0x0000FFFF;   /* XXX  GBA BUG HARDCODES THIS VALUE */

    /*
     *  Initialize the GBA Maps and System Maps for the most trivial
     *  configuration:  a direct mapping of VME byte N to System
     *  Physical byte N, and byte offset-from-GBA-base N to VME byte N.
     *  Think of this as the degenerate case.
     */
    for (pfn = 0, sysmap = (u_int *)(ctlspace + SMAP(gba_number)),
		  gbamap = (u_int *)(ctlspace + GMAP(gba_number));
         pfn < (GBA1-GBA0)/NBPP_R6000;
         pfn++, sysmap++, gbamap++) {
		*sysmap = pfn << SMAP_SHIFT;
		*gbamap = (pfn << GMAP_SHIFT) |
			  GBA_OPCODE_RW	      |
			  ( ((pfn*NBPP_R6000) <= 0xffff ? VME_A16SAMOD
						        : VME_A24SAMOD)
			    << GMAP_VME_AM_SHIFT );
    }

    switch (ioa_number) {	/* XXX should decode SBC BrdAddress */
	case 1:	ioa_gba_vaddr[0][gba_number] = IOA1 + (gba_number * GBA_SPAN);
		break;
	case 2:	ioa_gba_vaddr[1][gba_number] = IOA2 + (gba_number * GBA_SPAN);
		break;
	case 3:	ioa_gba_vaddr[2][gba_number] = IOA3 + (gba_number * GBA_SPAN);
		break;
    } /* switch */

    gba_count++;
}


/*
 *  find_r6000_controller( vme_address )
 *
 *  Searches each Gba for a controller at the specified VME address:
 *	vme_address + badaddr_offset
 *  beginning with IOA1-Gba1.
 *  Returns the K1 address of that controller, or the vme_address if not found.
 *  (The calling driver is expected to pass the returned address through
 *  badaddr(), which is expected to reject a Kuseg-like vme_address, and the
 *  driver can produce an error message with a meaningful address.)
 */
int
find_r6000_controller( vme_address, badaddr_offset, size )
int	vme_address;
int	badaddr_offset;
int	size;
{
    int	ioa;
    int	gba;
    int	system_address;

    for (ioa = 0; ioa < 3; ioa++) {
	for (gba = 0; gba <= 1; gba++) {
	    if (ioa_gba_vaddr[ioa][1-gba]) {
		system_address = ioa_gba_vaddr[ioa][1-gba] + vme_address;
		if (!badaddr( (system_address + badaddr_offset), size )) {
		    return( system_address );	/* found it! */
		}
	    } /* Gba exists */
	} /* for each GBA in an IOA */
    } /* for each IOA */

    return( vme_address );		/* no controller found */
}


_ioainit( ctlspace, ioa_number )
int	ctlspace;
int	ioa_number;
{
	int		bit;
	u_int		gba0_int, gba0_lop_int,
			gba1_int, gba1_lop_int;



	*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG)
			= IOC_BUSY_RESET | GBA0_RESET | GBA1_RESET;

	*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG)
			= 0;	/* deassert GBA reset */

	/* initialize the interrupt constants */
	for (bit = 0; bit < 32; bit++) {
	    *(volatile u_int *)(ctlspace + IOA_CONSTANTS + bit*sizeof(u_int))
		= 1 << bit;
	}
	/*
	 *  The hardware weenies tell us that we can't access the VMEbus
	 *  after Reset for 400 msec.  If this Reset is being done in the
	 *  saio initialization as a prelude to a program which will, in fact,
	 *  access the VMEbus, then we'd better wait right now to be safe.
	 *  If this is PROM initialization, then we might be using bootmode=m
	 *  which would hit the VMEbus fast, too.  Wait, wait.
	 */
	DELAY(524288);

	*(volatile u_int *)(ctlspace + IOA_STATUS_REG0)
			=  IOC_SR_SEC_ERR 	  | IOC_SR_ADMUX_PTY_ERR
			 | IOC_SR_DATAOUT_PTY_ERR | IOC_SR_SYS_BUS_ERR
			 | IOC_SR_GBA_ERR;

	*(volatile u_int *)(ctlspace + IOA_STATUS_REG1)
			=  IOC_SR_SEC_ERR 	  | IOC_SR_ADMUX_PTY_ERR
			 | IOC_SR_DATAOUT_PTY_ERR | IOC_SR_SYS_BUS_ERR
			 | IOC_SR_GBA_ERR;

	/*
	 *  Initialize the Duart interrupt vector
	 */
        ioa_deposit_intr_vector( ctlspace + IOA_DUART_INT_CMD1
				,CSR_IVECTSET_DUART );

	/*
	 *  Initialize GBA operations.
	 */
	switch (ioa_number) {
		case 1:
			gba0_int     = CSR_IVECTSET_IOA1_GBA0_OP;
			gba1_int     = CSR_IVECTSET_IOA1_GBA1_OP;
			gba0_lop_int = CSR_IVECTSET_IOA1_GBA0_LOP;
			gba1_lop_int = CSR_IVECTSET_IOA1_GBA1_LOP;
			break;
		case 2:
			gba0_int     = CSR_IVECTSET_IOA2_GBA0_OP;
			gba1_int     = CSR_IVECTSET_IOA2_GBA1_OP;
			gba0_lop_int = CSR_IVECTSET_IOA2_GBA0_LOP;
			gba1_lop_int = CSR_IVECTSET_IOA2_GBA1_LOP;
			break;
		case 3:
			gba0_int     = CSR_IVECTSET_IOA3_GBA0_OP;
			gba1_int     = CSR_IVECTSET_IOA3_GBA1_OP;
			gba0_lop_int = CSR_IVECTSET_IOA3_GBA0_LOP;
			gba1_lop_int = CSR_IVECTSET_IOA3_GBA1_LOP;
			break;
	} /* switch */

	if ((*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG) & GBA0_EXISTS)) {
	    gba_init_r6000( ctlspace, ioa_number, 0, gba0_int, gba0_lop_int );
	    gba_sw_init( ctlspace, 0 );

	}
	if ((*(volatile u_int *)(ctlspace + IOA_ERRORINFO_REG) & GBA1_EXISTS)) {
	    gba_init_r6000( ctlspace, ioa_number, 1, gba1_int, gba1_lop_int );
	    gba_sw_init( ctlspace, 1 );
	}
}

_ioainit_all()
{
	int	ioa;
	long	ioa_flags;
	char	*ioa_param;
	extern	char *get_nvram();


	find_r6000_ioa_boards();	/* init ctlspace addresses for IOAs */

	/* XXX start hack */
	ioa_param = get_nvram( NVADDR_IOAPARM, NVLEN_IOAPARM);
	ioa_flags = atoi(&ioa_param[2]);	/* skip leading "0x" */
	if (ioa_flags & 0x01) {
		printf("NO IOA INITIALIZATION PERFORMED!  RESET ioa_param  TO RE-ENABLE \n");
		return(0);
	}
	/* XXX end hack */

	/*
	 * Initialize/reset various IOC and GBA registers
	 */
	gba_count = 0;

	for (ioa=1; ioa<=3; ioa++) {
	    if (ioa_ctlspace_vaddr[ioa-1]) {
		_ioainit( ioa_ctlspace_vaddr[ioa-1], ioa );
	    } /* if specific IOA exists */
	} /* for each possible IOA */

	if (gba_count == 0) {
	    printf("No GBA found. \n");
	}
}


/*
 * Scan for any IOA errors and report them.
 * If ioaddr is non-zero, then it is the address which caused an
 * anticipated VME bus error.  In that case, don't report the error.
 */

_ioa_error_scan( ioaddr )
int	ioaddr;
{
  int	ioa;
  int	gba;
  int	ctlspace;
  u_int	ioc_status, gba_status;

  /*
   * Check the master IOA error bit in the CPU interrupt register.
   * If an IOA error has occurred, then poll the existing IOAs.
   */
  
  if (*(volatile u_int *)CSR_IVECTSET & CSR_IVECTSET_IOA_ERR) {
    *(volatile u_int *)CSR_IVECTCLR = CSR_IVECTSET_IOA_ERR;	/* Reset */

    for (ioa = 0; ioa < 3; ioa++) {
      if (ctlspace = ioa_ctlspace_vaddr[ioa]) {
	
	/*
	 * Check GBA0 status register and IOA STATUS REG for errors.
	 */
	  
	if (ioa_gba_vaddr[ioa][0]) {
	  ioc_status = *(volatile u_int *)(ctlspace + IOA_STATUS_REG0);
	  gba_status = *(volatile u_int *)(ctlspace + GBACTL(0) + GBA_STATUS);

	  if (gba_status & 0x000000ff) {
	    if (!ioaddr)
	      printf("ERROR, GBA0 status: 0x%x\n", gba_status);
	    *(volatile u_int *)(ctlspace + GBACTL(0) + GBA_STATUS) = 0;
	  }
	  
	  if (ioc_status & 
	      (IOC_SR_SEC_ERR | IOC_SR_ADMUX_PTY_ERR |
	       IOC_SR_DATAOUT_PTY_ERR | IOC_SR_SYS_BUS_ERR | IOC_SR_GBA_ERR)) {

	    if (!ioaddr)
	      printf("ERROR, IOA%d_STATUS_REG0: 0x%x\n", ioa, ioc_status);
	    *(volatile u_int *)(ctlspace + IOA_STATUS_REG0) = ioc_status;
	  }
	} /* if gba0 exists */

	/*
	 * Check GBA1 status register and IOA STATUS REG for errors.
	 */
	  
	if (ioa_gba_vaddr[ioa][1]) {
	  ioc_status = *(volatile u_int *)(ctlspace + IOA_STATUS_REG1);
	  gba_status = *(volatile u_int *)(ctlspace + GBACTL(1) + GBA_STATUS);

	  if (gba_status & 0x000000ff) {
	    if (!ioaddr)
	      printf("ERROR, GBA1 status: 0x%x\n", gba_status);
	    *(volatile u_int *)(ctlspace + GBACTL(1) + GBA_STATUS) = 0;
	  }
	  
	  if (ioc_status & 
	      (IOC_SR_SEC_ERR | IOC_SR_ADMUX_PTY_ERR |
	       IOC_SR_DATAOUT_PTY_ERR | IOC_SR_SYS_BUS_ERR | IOC_SR_GBA_ERR)) {

	    if (!ioaddr)
	      printf("ERROR, IOA%d_STATUS_REG1: 0x%x\n", ioa, ioc_status);
	    *(volatile u_int *)(ctlspace + IOA_STATUS_REG1) = ioc_status;
	  }
	} /* if gba1 exists */

	
      } /* if ioa exists */
    } /* for each ioa */
  } /* if an IOA error */
}
