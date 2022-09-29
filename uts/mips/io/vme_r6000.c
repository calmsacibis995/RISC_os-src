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
#ident	"$Header: vme_r6000.c,v 1.5.1.9.1.6.1.4 90/10/26 16:12:30 beacker Exp $"

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/buf.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/reg.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/edt.h"
#include "sys/bc.h"
#include "sys/ctlspace.h"
#include "sys/ioa.h"
#include "sys/vmereg.h"
#include "sys/sysmacros.h"

static int debug_ok_init_iack = 0;

#define VME_IACK_TYPE	unsigned long
#define VME_ISR_TYPE	unsigned long

#define VME_IACK_SIZE	sizeof(VME_IACK_TYPE)

#define VME_MINIPL 1		/* lowest interrupt level */
#define VME_MAXIPL 7		/* highest interrupt level */

/* RC6280 may have many devices.  The interrupt vector cache will be hashed
 * on vector number, since in an ideal situation the vector numbers will be
 * unique.  When necessary, vecotrs are chained from the hash bucket and
 * can be qualified by IOA number & GBA number.
 */
#define VME_VEC_HASH(vec) ((vec) >> 2)
#define VME_BUS_ID(ioa,gba) (((ioa) << 1) | gba)
#define VME_CACHE(vec,vbus) (vec_cache[VME_VEC_HASH(vec)])
#define VME_CACHE_HIT(ptr,vec,vbus)		\
	(((ptr)->v_vec == (vec)) && ((ptr)->v_bus == (vbus)))
#define VME_ISR_CONTENTS (*(volatile VME_ISR_TYPE *)PHYS_TO_K1(VME_ISR_6000)) 
static struct vme_intrs *vec_cache[64];

extern int ioa_ctlspace_vaddr[];
extern int ioa_gba_vaddr[SBC_SLOT_UPB][2];
extern int ioa_count;

#define VME_ISR_6000	ioa_ctlspace_vaddr[ ioa_index ] +	\
			GBACTL(gba_number) + GBA_VME_INT_PEND
#define VME_IACK_OFFSET	0x10000				/* first A24 page */
#define VME_IACK_6000	ioa_gba_vaddr[ ioa_index ][ gba_number ]	\
			 + VME_IACK_OFFSET


vme_level0_intr(ioa_index, gba_number, level0)
int  ioa_index;
int  gba_number;
int  level0;		/* 1 => real level 0 interrupt; 0 => ioa error int */
{
  u_int	 gba0_status_reg, gba1_status_reg;
  u_int gba_op_err, gba_vme_err;
  extern struct reg_desc gba_status_desc[];
  extern int vme_berr_ok;
  u_int pfn;
  
  if (gba_number == 0) {	/* GBA 0 */
    gba0_status_reg =
      *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | GBA0_MISC_BASE | GBA_STATUS);
    
    gba_op_err = *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			    | GBA0_MISC_BASE | GBA_OP_ERR_ADDR);
    gba_vme_err = *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			     | GBA0_MISC_BASE | GBA_VME_ERR_ADDR);
    if (vme_berr_ok && (gba0_status_reg & GBA_TIMEOUT)) {
      /* ignore Timeouts resulting from iobadaddr() */
      gba0_status_reg &= ~(GBA_TIMEOUT | GBA_MASTER_BUS_ERR);
    }

    /* clear errs */

    *(u_int *)(ioa_ctlspace_vaddr[ioa_index]| GBA0_MISC_BASE | GBA_STATUS) = 0;

    if (gba0_status_reg & 0xff) {
      if (level0)
	cmn_err( CE_CONT,"IOA%d Gba0 LEVEL 0 INTERRUPT\n", ioa_index+1 );
      cmn_err( CE_CONT,"IOA%d Gba0 Status:  %R \n",
	      ioa_index+1, gba0_status_reg & 0xff, gba_status_desc );
      cmn_err( CE_CONT,
	      "     Gba0 BadAddr: 0x%x StateMachine: (0x%x 0x%x 0x%x)\n",
	      *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			 | IOA_ERROR_ADDR0)   & (GBA_SPAN-1),
	      gba0_status_reg >>27,
	      (gba0_status_reg >> 22) & 0x1f,
	      (gba0_status_reg >> 18) & 0x0f );
      cmn_err( CE_CONT,
	      "     Gba0 GbaOpErrAddr: 0x%x  VmeErrAddr: 0x%x\n",
	      gba_op_err, gba_vme_err);
      if (gba0_status_reg & GBA_PARITY_ERR_MASK) {
	pfn = btoct(gba_op_err & 0x00ffffff);
	cmn_err(CE_CONT,  "     Gba0 sysmap entry (pfn 0x%x) 0x%x\n", pfn,
		  *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | SYS0_MAP | (4*pfn)));
      } else if (gba0_status_reg & GBA_MASTER_BUS_ERR) {
	  pfn = btoct(gba_vme_err & 0x00ffffff);
	  cmn_err(CE_CONT,  "     Gba0 gbamap entry (pfn 0x%x) 0x%x\n", pfn,
		  *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | GBA0_MAP | (4*pfn)));
      } else if (gba0_status_reg & GBA_TIMEOUT) {
	/* If within address range of GBA, then display map register */
	if ((gba_vme_err < 0x00800000) || (gba_vme_err >= 0xff800000)) {
	  pfn = btoct(gba_vme_err & 0x00ffffff);
	  cmn_err(CE_CONT,  "     Gba0 sysmap entry (pfn 0x%x) 0x%x\n", pfn,
		  *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | SYS0_MAP | (4*pfn)));
	} else
	  cmn_err(CE_CONT, "     VME BERR not in GBA0 address range\n");
      }	/* end GBA_TIMEOUT */
    }
  } else { 	/* GBA 1 */
    gba1_status_reg =
      *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | GBA1_MISC_BASE | GBA_STATUS);
    gba_op_err = *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			    | GBA1_MISC_BASE | GBA_OP_ERR_ADDR);
    gba_vme_err = *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			     | GBA1_MISC_BASE | GBA_VME_ERR_ADDR);
    if (vme_berr_ok && (gba1_status_reg & GBA_TIMEOUT)) {
      /* ignore Timeouts resulting from iobadaddr() */
      gba1_status_reg &= ~(GBA_TIMEOUT | GBA_MASTER_BUS_ERR);
    }

    /* clear errs */

    *(u_int *)(ioa_ctlspace_vaddr[ioa_index]| GBA1_MISC_BASE | GBA_STATUS) = 0;

    if (gba1_status_reg & 0xff) {
      if (level0)
	cmn_err( CE_CONT,"IOA%d Gba1 LEVEL 0 INTERRUPT\n", ioa_index+1 );
      cmn_err( CE_CONT,"IOA%d Gba1 Status:  %R \n",
	      ioa_index+1, gba1_status_reg & 0xff, gba_status_desc );
      cmn_err( CE_CONT,"     Gba1 BadAddr: 0x%x StateMachine: (0x%x 0x%x 0x%x)\n",
	      *(u_int *)(ioa_ctlspace_vaddr[ioa_index]
			 | IOA_ERROR_ADDR1)   & (GBA_SPAN-1),
	      gba1_status_reg >>27,
	      (gba1_status_reg >> 22) & 0x1f,
	      (gba1_status_reg >> 18) & 0x0f );
      cmn_err( CE_CONT,
	      "     Gba1 GbaOpErrAddr: 0x%x  VmeErrAddr: 0x%x\n",
	      gba_op_err, gba_vme_err );
      if (gba1_status_reg & GBA_PARITY_ERR_MASK) {
	pfn = btoct(gba_op_err & 0x00ffffff);
	cmn_err(CE_CONT,  "     Gba1 sysmap entry (pfn 0x%x) 0x%x\n", pfn,
		*(u_int *)(ioa_ctlspace_vaddr[ioa_index] | SYS1_MAP | (4*pfn)));
      } else if (gba1_status_reg & GBA_MASTER_BUS_ERR) {
	  pfn = btoct(gba_vme_err & 0x00ffffff);
	  cmn_err(CE_CONT,  "     Gba1 gbamap entry (pfn 0x%x) 0x%x\n", pfn,
		  *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | GBA1_MAP | (4*pfn)));
      } else if (gba1_status_reg & GBA_TIMEOUT) {
	/* If within address range of GBA, then display map register */
	if ((gba_vme_err < 0x00800000) || (gba_vme_err >= 0xff800000)) {
	  pfn = btoct(gba_vme_err & 0x00ffffff);
	  cmn_err(CE_CONT,  "     Gba1 sysmap entry (pfn 0x%x) 0x%x\n", pfn,
		  *(u_int *)(ioa_ctlspace_vaddr[ioa_index] | SYS1_MAP | (4*pfn)));
	} else
	  cmn_err(CE_CONT, "     VME BERR not in GBA1 address range\n");
      }	/* end GBA_TIMEOUT */
    }
  }
}


vme_intr(ep,ioa_index,gba_number)
uint *ep;
int  ioa_index;
int  gba_number;
{
	register VME_ISR_TYPE previpl;
	register int req;
	uint isr_value;
	unsigned char vme_bus;

	/* vme interrupt mask */
	previpl = 0xff;			/* mask undefined bits */

#ifdef TODO
	ffintr should not know the bit positions in the cause register 
	of the interrupt pending bits
#endif
	/* make sure we do not process an interrupt that is masked coming
	 * into this routine. so mask out non-enable bits with previpl.
	 */
	reset_ioc_retry_count();	/* in case of DBE reading ISR */
	while (req = (ffintr(( (isr_value = VME_ISR_CONTENTS) & previpl)
				 << CAUSE_IPSHIFT))) {
		VME_IACK_TYPE vec;
		register struct vme_intrs *intr;

		/* Handle GBA level 0 interrupts (generally errors ).
		 */

		if (isr_value & 0x01) {
		  vme_level0_intr( ioa_index, gba_number, 1 );
		  continue;	/* re-fetch the ISR since we had a level 0 */
		}

		/* ffintr returns a number from 8 to 1 and we want
		 *	the number 0-based so we can shift a '1' to the
		 *	correct bit location.
		 */
		req--;

		/*
		 *  Perform the InterruptAcknowledge.
		 *  Two bad things can happen:
		 *	- The LW can be rejected by the IOC with a DBE.  We
		 *	  need to retry the access.
		 *	- Another interrupt can appear in the middle of the
		 *	  LW instruction which does the IAck, after the Iack
		 *	  state transition has occurred on the VMEbus, but
		 *	  before the CPU has stored the result in the target
		 *	  register.  Unless we disable interrupts around the
		 *	  IAck, the CPU will interrupt in mid-instruction (e.g.
		 *	  to process a timer tick), and the EPC will say to
		 *	  retry the LW -- but the IAck cannot be reexecuted!
		 */
		reset_ioc_retry_count();	/* in case get DBE doing IACK */
		vec = (VME_IACK_TYPE)0xff;	/* Initial to something bad */
		if (bad_load_addr( (VME_IACK_6000 + req*VME_IACK_SIZE)
				,sizeof(VME_IACK_TYPE)
				,&vec )) {
			cmn_err(CE_WARN
				,"IOA%d GBA%d bogus ISR 0x%x says IntLevel %d is pending"
				,ioa_index+1, gba_number, isr_value, req);
			continue;
		}
		vec &= 0xff;			/* ignore upper 24 bits */

		vme_bus = VME_BUS_ID(ioa_index+1, gba_number);
		intr = VME_CACHE(vec,vme_bus);
		for (;;) {
			if (!intr) {
#ifdef DEBUG
				printf ("Stray at vec 0x%x, ipl %d\n",
					vec, req);
#endif					
				stray(ep);
				break;
			}
			if (VME_CACHE_HIT(intr,vec,vme_bus)) {
				intr->v_vintr(intr->v_unit);
				break;
			}
			intr = intr->v_link;
		}
	}
}


vme_intr_ioa1_gba0(ep)
uint *ep;
{
	vme_intr(ep, 0, 0 );
}

vme_intr_ioa1_gba1(ep)
uint *ep;
{
	vme_intr(ep, 0, 1 );
}

vme_intr_ioa2_gba0(ep)
uint *ep;
{
	vme_intr(ep, 1, 0 );
}

vme_intr_ioa2_gba1(ep)
uint *ep;
{
	vme_intr(ep, 1, 1 );
}

vme_intr_ioa3_gba0(ep)
uint *ep;
{
	vme_intr(ep, 2, 0 );
}

vme_intr_ioa3_gba1(ep)
uint *ep;
{
	vme_intr(ep, 2, 1 );
}

vme_init_interrupts( e, vme_bus )
struct	edt *e;
unsigned char vme_bus;
{
  register struct vme_intrs *vme_intr;
  register struct vme_intrs **ip;
  /*
   * Now we add the controller's interrupt vectors
   * into the appropriate hash chain.
   */
  for (vme_intr = e->e_intr_info;  vme_intr->v_vintr != NULL;  vme_intr++) {
    ip = &VME_CACHE(vme_intr->v_vec, vme_bus);
    vme_intr->v_bus = vme_bus;
    vme_intr->v_link = *ip;
    *ip = vme_intr;
  }
}

vme_init()
{
	register int i;

	/* create the VME interrupt table */
	for (i = 0; i < nedt; i++) {
		if (edt[i].e_intr_info != NULL) {
			unsigned char vme_bus;
			int ioaID, gbaID;
			unsigned int devaddr;

			/*
			 * First we determine the vme_bus number of the
			 * bus which contains this controller.  Number
			 * will be 2,3 (IOA1) 4,5 (IOA2) or 6,7 (IOA3)
			 */
			if (!IS_KSEG1(devaddr = edt[i].e_base))
			  continue;	/* Handle this in device edt init */
			ioaID=0;
			vme_bus = 0;
			while ((!vme_bus) && (ioaID < ioa_count)) {
			  gbaID = 0;
			  while ((!vme_bus) && (gbaID < 2)) {
			    if ((devaddr >= ioa_gba_vaddr[ioaID][gbaID]) &&
				(devaddr < ioa_gba_vaddr[ioaID][gbaID]
				 + GBA_SPAN))
			      {
				vme_bus = VME_BUS_ID( ioaID+1, gbaID);
			      }
			    gbaID++;
			  }
			  ioaID++;
			}
			if (!vme_bus)
			  continue;	/* Not on any known VME bus */

			/*
			 * Now we add the controller's interrupt vectors
			 * into the appropriate hash chain.
			 */
			vme_init_interrupts( &edt[i], vme_bus );
		}
	}
}

/*
 *  findmap_r6000_controller
 *
 *  This routine differs from find_r6000_controller in that the VME address
 *  desired for the controller does NOT reside in the (permanently mapped)
 *  VME A16 address space.  This routine will map the controller address
 *  range into the GBA map & then attempt to access the controller's probe
 *  address.
 *
 *  If e->e_base is already a K1 address space, then the K1 address determines
 *  which IOA/GBA the controller is on.  We must still map the address to
 *  access the controller.  Note that this limits us to addresses in the
 *  range of 0x00000000 to 0x01ffffff (high order byte determines IOA).
 *  Since we are mapping the controller, the returned address will typically
 *  NOT be the same as e->e_base.  After mapping we simply call IOBADADDR to
 *  see if the controller is present at that address.
 * 
 *  If e->e_base is NOT a K1 address, then search each Gba for a
 *  controller at the specified VME address:
 *	e->e_base + badaddr_offset
 *  beginning with IOA1-Gba1.  If we find a controller at that address on
 *  some vme bus, then complete the configuration of the controller.
 *  Returns the mapped address of that controller, or 0 if not found.
 */
int
findmap_r6000_controller( e, len, VMEam, badaddr_offset, size )
struct	edt *e;		/* e->e_base is base address of VME area to map */
int	len;		/* length of VME area to be mapped */
int	VMEam;		/* VME AM to be used on VME map area */
int	badaddr_offset;	/* offset from base of VME area to be probed */
int	size;		/* size of probe address */
{
  int vme_address;
  int	ioa;
  int	gba;
  int	system_address;
  unsigned int gmapArea;

  /*
   * Is e_base is already a K1 address, use that address to see if controller
   * is present.  Return success or failure of access.
   */
  
  if (IS_KSEG1(vme_address = e->e_base)) {
    /* Configuration address is already a K1 address.  The high order 7 bits
     * address a particular IOA & should be stripped from the address we
     * generate on the VME bus itself.  Hence addresses 0xb8000000 through
     * 0xbdffffff will generate a VME bus address from 0x00000000 through
     * 0x01ffffff.
     */
    if (eba_map( /* indicates IOA */ vme_address,
		vme_address & 0x01ffffff,	/* strip off IOA bits */
		len, VMEam,
		&gmapArea, &system_address) == 0)
      return(0);	/* Error in EBA map (Possibly EBA not present!) */
     
    if (iobadaddr( system_address + badaddr_offset, size )) {
      eba_unmap( gmapArea );
      return(0);
    } else {
      if (showconfig)
	printf("Controller address 0x%x found on IOA %d  GBA %d (0x%x)\n",
	       e->e_base, ioa+1, gba, system_address );
      return( system_address );
    }
  }

  /*
   * The address in e_base was not a K1 address.  Search all vme buses.  If
   * we find the controller, complete configuration and return address to
   * caller.
   */
  
  for (ioa = 0; ioa < 3; ioa++) {
    for (gba = 0; gba <= 1; gba++) {
      if (ioa_gba_vaddr[ioa][gba]) {
	if (eba_map( ioa_gba_vaddr[ioa][gba], vme_address, len, VMEam,
		    &gmapArea, &system_address) == 0) 
	  cmn_err(CE_PANIC, "eba_map could not map ioa%d gba%d!", ioa+1, gba);
	if (!iobadaddr( (system_address + badaddr_offset), size )) {
	  /*
	   * We have found the controller !
	   */
	  
	  if (showconfig)
	    printf("Controller address 0x%x found on IOA %d  GBA %d (0x%x)\n",
		   e->e_base, ioa+1, gba, system_address );
	  
	  e->e_base = system_address;

	  vme_init_interrupts( e, VME_BUS_ID( ioa+1, gba ));

	  return( system_address );	/* found it! */
	} else
	  eba_unmap( gmapArea );
      } /* Gba exists */
    } /* for each GBA in an IOA */
  } /* for each IOA */
  
  if (showconfig)
    printf("Controller address 0x%x NOT found\n",  e->e_base );
	  
  return( 0 );		/* no controller found */
}


/*
 *  find_r6000_controller( vme_address, badaddr_offset, size )
 *
 *  This procedure here until I/O drivers are converted to the new procedure
 *  findmap_r6000_controller.
 *
 *  If vme_address is already a K1 address, then that is the controller
 *  address.  We simply call IOBADADDR to see if the controller is present
 *  at that address.  
 *  If vme_address is NOT a K1 address, then search each Gba for a
 *  controller at the specified VME address:
 *	vme_address + badaddr_offset
 *  beginning with IOA1-Gba1.  If we find a controller at vme_address on
 *  some vme bus, then complete the configuration of the controller.
 *  Returns the K1 address of that controller, or 0 if not found.
 */

int
find_r6000_controller( e, badaddr_offset, size )
struct	edt *e;
int	badaddr_offset;
int	size;
{

  if ((e->e_base + badaddr_offset) & 0x00ff0000)
    return(
	 findmap_r6000_controller( e, 0x200, VME_A24SAMOD, badaddr_offset,
				  size ) );
  else
    return(
	 findmap_r6000_controller( e, 0x200, VME_A16SAMOD, badaddr_offset,
				  size ) );


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
    uint		pfn;
    uint		vme_addr;
    uint		ioa_base_paddr,
			ioa_mbsize;
    volatile uint	*sysmap,
    			*gbamap;
    volatile uint	*cache_addr,
			*cache_data,
			*cache_tag,
			*section_info,
			*cache_dirty;


    /*
     *  Determine the base virtual address of the Gba space.
     */
    sbc_addr32( *(volatile u_int *)(ctlspace + SBC_BRDADDR)
	       ,&ioa_base_paddr, &ioa_mbsize );

    ioa_gba_vaddr[ ioa_number-1 ][ gba_number ]
	= PHYS_TO_K1(ioa_base_paddr) + (gba_number * GBA_SPAN);

    /*
     *  Clear the GBA cache:  reset the tags to zero (not-valid and not-dirty),
     *  reset the per-line dirty bits, and initialize the per-section Control
     *  to Cacheable, Allow-Partials.
     */
    cache_addr   = (uint *)(ctlspace + GBACTL(gba_number) + GBA_DATA_ADDR);
    cache_data   = (uint *)(ctlspace + GBACTL(gba_number) + GBA_CACHE);
    cache_tag    = (uint *)(ctlspace + GBACTL(gba_number) + GBA_TAGS);
    cache_dirty  = (uint *)(ctlspace + GBACTL(gba_number) + GBA_DIRTY_BITS);
    section_info = (uint *)(ctlspace + GBACTL(gba_number) + GBA_SECT_INFO);

    for (vme_addr = 0; vme_addr < GBA_SPAN; vme_addr += GBA_SECTION_SPAN) {
	/*
	 *  Initialize both cache lines of each section.
	 */
	*cache_addr = vme_addr;
	*section_info
	   = ~(GS_UNCACHED_WRT|GS_LOCAL_WRT|GS_RANDOM_WRT|GS_NO_PARTIAL_WRT)
	     & GS_SECTIONINFO_MASK_WRT;
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
     *  Initialize all 16 LongOp Interrupt Vector locations.
     */
    for (intr_number = 0; intr_number < 16; intr_number++) {
        ioa_deposit_intr_vector( ctlspace + LOP_INT(gba_number)
					  + IOC_REG(intr_number)
				,gba_lop_int );
    }

    /*
     *  Initialize all 16 LongOp registers.
     */
    for (intr_number = 0; intr_number < 16; intr_number++) {
        *(volatile u_int *)(ctlspace + GBA_LOP(gba_number)
				     + IOC_REG(intr_number)) = 0;
    }

    /*
     *  Initialize the GBA Address Compare register.
     */
	/* XXX */
    
    *(volatile u_int *)(ctlspace + GBACTL(gba_number) + GBA_VME_ADR_CMP)
	       = 0x0000ffff;   /* Current HW can't set this */

    /*
     *  Initialize the GBA Maps and System Maps for the most trivial
     *  configuration:  a direct mapping of VME byte N to System
     *  Physical byte N, and byte offset-from-GBA-base N to VME byte N.
     *  Only map the first four pages (for A16 space) and the last page
     *  (for last page of A24).  Rest of map is dynamic (using eba_map and
     *  eba_unmap routines).
     */
    for (pfn = 0, gbamap = (uint *)(ctlspace + GMAP(gba_number));
         pfn < (GBA1-GBA0)/NBPP;
         pfn++, gbamap++) {
		if ((pfn < 4) || (pfn == ((GBA1-GBA0)/NBPP -1)))
		  *gbamap = (pfn << GMAP_SHIFT) |
			  GBA_OPCODE_RW	      |
			  ( ((pfn*NBPC) <= 0xffff ? VME_A16SAMOD : VME_A24SAMOD)
			    << GMAP_VME_AM_SHIFT );
		else
		  *gbamap = 0xef000000 | (pfn << GMAP_SHIFT);
		/* Dynamic map -- illegal but unique value until allocated */
    }
    /*
     * "Invalidate" system map by mapping to range between
     * 2 GB and 4 GB of physical address.  We can look at
     * lower bits of attempted access if get bus error or info
     * on attempted access.  This forces device handlers to properly
     * setup their maps expicitly.
     */
    for (pfn = 0, sysmap = (uint *)(ctlspace + SMAP(gba_number));
         pfn < (GBA1-GBA0)/NBPP;
         pfn++, sysmap++) {
		*sysmap |= 0x8000000;
    }
    /*
     *  Initialize one GbaMap entry for use as the VME IntAck address.
     *  (This uses the fifth GBA map register -- following A16 map)
     */
    *(uint *)(ctlspace + GMAP(gba_number)
		       + VME_IACK_OFFSET/(NBPP/sizeof(u_int)))
	 = GBA_OPCODE_IACK;

    /*
     *  The GBA reports VMEbus errors by generating a level-0 interrupt!
     *  For now, vector these interrupts into the "IOA Error" interrupt
     *  handler.
     */
    ioa_deposit_intr_vector( ctlspace + GBA_INT(gba_number) + IOC_REG(0)
			    ,CSR_IVECTSET_IOA_ERR );

    /*
     *  Clear any pending VMEbus interrupts for each VMEbus interrupt level,
     *  and initialize the Interrupt Vector locations for each level.
     *  Each of the seven levels will turn on the same target SBC 
     *  InterruptVector bit.
     */
    for (intr_number = VME_MINIPL; intr_number <= 15; intr_number++) {
	if (debug_ok_init_iack) {
	badaddr( (VME_IACK_TYPE *)(ioa_gba_vaddr[ ioa_number-1 ][ gba_number ]
					    + VME_IACK_OFFSET
					    + intr_number*VME_IACK_SIZE)
	        ,VME_IACK_SIZE);
	}
        ioa_deposit_intr_vector( ctlspace + GBA_INT(gba_number)
					  + IOC_REG(intr_number)
				,gba_int );
      }

    gba_sw_init( ctlspace, gba_number );
}

#ifdef DEBUG
/*
 * Check all VME busses for dropped interrupts. This can be
 * used by an idle loop to verify that there are no pending interrupts
 * on any of the busses.
 */

extern int ioa_count;

vme_interrupt_check(check_now)
int check_now;
{
  int ioa_index, gba_number;	/* These names used by ISR macros */
  int s, dropped=0;
  static int isr_check_count, i;

  if ((!check_now) && (isr_check_count++ < 8192))
    /*
     * If this is idle loop check, only poll ISR every 18 milliseconds
     * or so (unoptimized kernel).  Otherwise we place a heavy burden on
     * the system bus and IOA/GBA, which may be attempting to transfer data.
     * Empirical data suggests that a count of 8192 causes us to poll the ISR
     * every 17.9 milliseconds (unopt) and this inner loop take about 7
     * microseconds betweem polling GBA0 and GBA1 on the same IOA.
     */
    return(0);

  s = splall();
  isr_check_count = 0;		/* Reset the idle check counter */
  for (ioa_index=0; ioa_index < ioa_count; ioa_index++)
    for (gba_number=0; gba_number < 2; gba_number++) {
      
      reset_ioc_retry_count(); 		/* Reset DBE retry counts */
      /*
       * We must check for non-zero ioa_gba_vaddr BEFORE doing
       * VME_ISR_CONTENTS since an unused VME bus on an existent IOA will
       * cause us to hang accessing the ISR.
       */

      if ((ioa_gba_vaddr[ioa_index][gba_number]) &&
	  (VME_ISR_CONTENTS & 0xfe)) {
	/* ISR has interrupt pending */

	if (ioa_index == 0)
	  if (gba_number) {
	    while ((!(*(volatile uint *)CSR_IVECTSET & CSR_IVECTSET_IOA1_GBA1_OP))
		   && (dropped < 1024))
	      dropped++;
	    if (dropped >= 1024)
	      /* Set interrupt bit for missing interrupt */
	      *(volatile uint *)CSR_IVECTSET = CSR_IVECTSET_IOA1_GBA1_OP;
	  }
	  else {
	    while ((!(*(volatile uint *)CSR_IVECTSET & CSR_IVECTSET_IOA1_GBA0_OP))
		   && (dropped < 1024))
	      dropped++;
	    if (dropped >= 1024)
	      /* Set interrupt bit for missing interrupt */
	      *(volatile uint *)CSR_IVECTSET = CSR_IVECTSET_IOA1_GBA0_OP;
	  }
	else
	  cmn_err(CE_WARN, "vme_interrupt_check: Can only handle IOA1!");
	
	if (dropped >= 1024)
	  cmn_err(CE_WARN,
		  "vme_interrupt_check: IOA%d GBA%d ints pending ISR 0x%x",
		  ioa_index+1, gba_number, VME_ISR_CONTENTS);
	else if (dropped > 1)
	  cmn_err(CE_WARN,"vme_interrupt_check: interrupt delay: 0x%x",
		  dropped);
      }
    }
  splx(s);
  return(dropped);
}
#endif DEBUG
