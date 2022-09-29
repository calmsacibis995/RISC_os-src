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
#ident	"$Header: gba.c,v 1.5.1.13.1.4.1.4 90/10/26 16:10:49 beacker Exp $"

/*
 * GBA utilities.
 */


/*
 *  The flag R6000_SIM_IOC should normally be UNDEFINED.  When it is defined
 *  then we are attempting to simulate the operation of the M6000 IOC/GBA
 *  on a non-6000 based machine, in order to test the various allocation and
 *  deallocation routines contained in this module.  Also can be used to test
 *  proper operation of device drivers for a 6000 system.
 */
#define REALLY_UNMAP 1

#ifdef STANDALONE

#include "sys/types.h"
#include "mips/bc.h"
#include "mips/cpu.h"
#include "mips/cpu_board.h"
#include "mips/ctlspace.h"
#include "mips/ioa.h"
#include "mips/vmereg.h"
#include "mips/param.h"

#define	hwcpin	hwcopy
#define	hwcpout	hwcopy

#else

#include "sys/debug.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/sysmacros.h"
#include "sys/sbd.h"
#include "sys/immu.h"
#include "sys/ioa.h"
#include "sys/systm.h"
#include "sys/vmereg.h"
#include "sys/cmn_err.h"
#include "sys/bc.h"
#include "sys/ctlspace.h"
#endif STANDALONE

#define PANIC(x)  cmn_err( CE_PANIC, (x))
#define TRUE  1
#define FALSE 0

#define R6000_KERNEL 1		/* =1 for 6000 kernels OR 6000 IOC simulation*/

#ifdef STANDALONE
#undef R6000_KERNEL
#define R6000_KERNEL 0		/* R6000_KERNEL=0 for standalone (SAIO) */
#else

#ifndef R6000
#ifndef R6000_SIM_IOC
#undef R6000_KERNEL
#define R6000_KERNEL 0		/* R6000_KERNEL=0 for non-6000 kernels */
#endif R6000_SIM_IOC
#endif R6000
#endif STANDALONE

#ifdef R6000_SIM_IOC
ioa_dev_type ioa_dev[3];
#endif R6000_SIM_IO

#ifdef	R6000_FAKEIO
#define	CMC_VMEADDR	0x00de4000
#define CMC_SYSADDR	0xbcde4000
#define	MEM_VMEADDR	0x00800000
#define	MEM_SYSADDR	0xbc800000
unsigned int	fakeio_reported=0xff;/* Flag to print WARNING on fakio */
unsigned int	fakeio_vreported=0xff;	/* Flag to print fakeio_verify */
#ifndef	SABLE
unsigned int	fakeio = 0;
unsigned int	fakeio_nohw = 1;
unsigned int	fakeio_dumb = 0;
#if defined(PROM)
int	fakeio_copy16;		/* relies on bss being 0'ed */
int	delay_fakeio;		/* relies on bss being 0'ed */
int	fakeio_cmc;		/* relies on bss being 0'ed */
int	fakeio_verify;		/* relies on bss being 0'ed */
unsigned int	fakeio_vmeaddr;	/* relies on bss being 0'ed */
unsigned int	fakeio_sysaddr;	/* relies on bss being 0'ed */
#else
int	fakeio_copy16 = 1;
int	delay_fakeio = 1;
int	fakeio_cmc = 0;
int	fakeio_verify = 1;
unsigned int	fakeio_vmeaddr = 0x00de4000;
unsigned int	fakeio_sysaddr = 0xbcde4000;
#endif
unsigned fakeio_bypass = 0;
int      fakeio_by_addr = 0;
int      fakeio_by_len = 0;
#else
unsigned int	fakeio = 0;
unsigned int	fakeio_nohw = 0;
unsigned int	fakeio_vmeaddr = 0x00800000;
unsigned int	fakeio_sysaddr = 0xbd800000;
int	fakeio_copy16 = 0;
int	delay_fakeio = 0;
int	fakeio_cmc = 0;
int	fakeio_verify = 0;
unsigned fakeio_bypass = 0;
int      fakeio_by_addr = 0;
int      fakeio_by_len = 0;
#endif	SABLE
#endif	R6000_FAKEIO

#define NBPP_R6000	16384
/****************************************************************************/

typedef unsigned char boolean8;
typedef		 int  boolean;
typedef		 char I8;
typedef 	 short I16;
typedef 	 long I32;
typedef unsigned char U8;
typedef unsigned short U16;
typedef unsigned long U32;


#if defined(PROM)
#define	MAX_GBA		2	/* limited memory for table in proms */
#else
#define	MAX_GBA		6	/* max number of GBAs in system */
#endif
#define MAX_SMAP_REG	1024	/* number of system map regs in GBA */
#define MAX_GMAP_REG	1024	/* number of GBA map regs in GBA */
#define MAX_GBA_CSECT	32	/* number of cache sections per GBA */
#define MAX_SR_PER_CS	(MAX_SMAP_REG / MAX_GBA_CSECT)
				/* number of system map regs in CS */

#define NIL_SPR		-1	/* illegal SPR value, ends linked lists */
#define CSECT_MIN_SREG(x) (x << 5)	/* first SMAP reg in cache section */
#define CSECT_MAX_SREG(x) (CSECT_MIN_SREG(x) + MAX_SR_PER_CS -1)

typedef I8	gba_id_t;	/* type of GBA id (0..5)	*/
typedef I8	pcs_id_t;	/* type of PCS id (0..31)	*/
typedef I8	lcs_id_t;	/* type of LCS id (0..31)	*/
typedef I16	smap_reg_t;	/* type used to contain smap reg index */
				/*  (-1 used to indicate end-of-list)  */
typedef U32	p_csh_t;	/* type of Physical Cache Section Handle */
typedef U32	l_csh_t;	/* type of Logical Cache Section Handle */

#if R6000_KERNEL
/*****************************************************************************
 *                 Logical Cache Section Handle  (CSH)
 *
 * A Logical Cache Section handle is returned to a device driver as the result
 * of a call to GBAreserveCsect.  This handle is passed as a parameter to the
 * various system page map allocation routines in order to allocate/release
 * map registers from within the LCS.  An LCS is composed of one or more
 * Physical Cache Sections (PCS).
 *
 * The following defines take a CSH and return the GBAid, Logical Cache Section
 * ID (within GBA), or a pointer to the Logical Cache Section Information.
 *
 * The handle itself could easily be made a pointer to the cs_info.  You
 * would then need to store the GBAid and CSid in the cs_info and change
 * these defines to return the information from the cs_info.
 *
 *	 15 		       8 7			       0
 *	---------------------------------------------------------
 *	|     GBAid + 1		|   Logical Cache Section ID	|
 *	|  (currently 1..6)	|      (currently 0..31)	|
 *	---------------------------------------------------------
 *
 * The above definition allows us to use zero as an invalid CSH.
 *
 * WARNING: If you change this definition, check the defines for System Page
 *          Handles (SPH) for any impact.
 *
 *****************************************************************************/

#define CSHtoGBAid(CSH) ((gba_id_t)(CSH>>8)-1)
#define CSHtoCSid(CSH) ((lcs_id_t)(CSH & 0x00FF))
#define CSHtoPCSid(CSH) ((pcs_id_t)(CSH & 0x00FF))

	/* The following two defines return the MIN and MAX	*/
	/* System Map Register Indices for a Physical Cache	*/
	/* Section.						*/

#define CSHtoMinSregID(PCSH) ((smap_reg_t)CSECT_MIN_SREG( CSHtoPCSid(PCSH) ))
#define CSHtoMaxSregID(PCSH) ((smap_reg_t)CSECT_MAX_SREG( CSHtoPCSid(PCSH) ))

#define computeCSH(GBAid,CSid) ((p_csh_t)((GBAid+1) << 8) | (CSid & 0x00FF))
#define getLCSinfo(GBAid,LCSid) ((cs_info *)  \
	   (&(GET_GBA_PTR(GBAid)->smap.cacheSection[LCSid])))

	/* convert Handle to a pointer to the */
	/*   Cache Section information        */

#define CSinfoPtr(CSH) getLCSinfo( CSHtoGBAid(CSH), CSHtoCSid(CSH) )

/*****************************************************************************
 *			System Page Register Handle  (SPH)
 *
 * The following defines take a System Page Register Handle, which is
 * returned to the caller when a system page is mapped, and return the GBAid,
 * Logical Cache Section ID (within GBA), System Page Registor ID or a pointer
 * to the System Map Entry.
 *
 * The handle itself could easily be made a pointer to the systemMapEntry.  You
 * then need to store the GBAid, CSid, & System Map Registor ID in the
 * systemMapEntry and change these defines to return the information from
 * there.
 *
 *	SPHtoGBAid( SPH )  -- Given the user's map register handle,
 *                            returns id of GBA containing the page map  entry.
 *	SPHtoLCSid( SPH )  -- Given the user's map register handle, return
 *                            the Logical Cache Section ID which contains this
 *                            page map entry.
 *	SPHtoPCSid( SPH )  -- Given the user's map register handle, return
 *                            the Physical Cache Section ID which contains this
 *                            page map entry.
 *	SPHtoSPRid( SPH )  -- Given the user's map register handle, return
 *                            System Page Register ID used by these
 *                            utility routines to reference the map register
 *                            in the smapReg array.
 *	computeSPH         -- Given the logical Cache Section HANDLE, and
 *                            System Page Reg id, compute the handle used
 *                            by user to reference this map register.
 *
 *	 31 		      	      16 15			       0
 *	-----------------------------------------------------------------
 *	| Logical Cache Section Handle	|   System Page Register ID	|
 *	|	(see CSH)		|      (currently 0..1023)	|
 *	-----------------------------------------------------------------
 *
 *	Using the current definitions of the above fields (which is subject
 *	to change) we obtain the following subfields of an SPH:
 *
 *	 31 	     24  23    	      16 15  	  10 9       5 4       0
 *	-----------------------------------------------------------------
 *	|   GBAid + 1	|   LCS id	|    	    |  PCS id | reg id	|
 *	|    (1..6)	|   (0..31)	|      	    | (0..31) | (0..31) |
 *	-----------------------------------------------------------------
 *
 * The above definition allows us to use zero as an invalid SPH.
 *
 *****************************************************************************/

#define SPHtoGBAid(SPH) ((gba_id_t)((SPH>>24)-1))	   /* GBA id */
#define SPHtoLCSid(SPH) ((lcs_id_t)((SPH & 0x00FF0000) >> 16))
						/* logical cache sect id */
#define SPHtoPCSid(SPH) ((pcs_id_t)((SPH & 0x000003E0) >> 5))
						/* physical cache sect id */
#define SPHtoPCSreg(SPH) (SPH & 0x0000001F)	/* smap reg within PCS */
#define SPHtoSPRid(SPH) ((smap_reg_t)(SPH & 0x0000FFFF))
					/* system page reg ID */

#define SPRidToPCSid(SPRid) ((pcs_id_t)((SPRid & 0x000003E0) >> 5))
						/* phys cache sect id */
#define SPRidToPCSreg(SPRid) (SPRid & 0x0000001F)
					/* smap reg within PCS */

#define PCSoffset(SPRid) (NBPP * SPRidToPCSreg(SPRid))
				/* byte offset of page within the PCS      */


#define computeSPH( CSH, SPRid)  \
                  ((CSH << 16) | (SPRid & 0x0000FFFF))

/* The following defines let us obtain pointers to the smagRegInfo or	*/
/* the Physical Cache Section Info given GBAid and System Page Reg id	*/
/* (SPRid).								*/

#define getSregInfo( GBAid, SPRid )  ((smap_reg_info *) \
	   (&(GET_GBA_PTR(GBAid)->smap.smapReg[SPRid])))

#define getPCSinfo( GBAid, SPRid )  ((cs_info *) \
	   (&(GET_GBA_PTR(GBAid)->smap.cacheSection[SPRidToPCSid(SPRid)])))

/*****************************************************************************/
/*                                                                           */
/* The GBA cache is managed in cache sections.  Each cache section is 512KB  */
/* of VME address space mapped to system address as 32 pages of 16KB each.   */
/* Each section has two cache lines of 128 bytes each.  Attributes definced  */
/* in cSectControl affect the entire cache section.                          */
/*                                                                           */
/* The intention is to reserve cache sections to device drivers and then     */
/* allocate registers from within the section as we need to map pages.       */
/*                                                                           */
/*****************************************************************************/

typedef	struct smap_reg_info	{
  Bit sr_allocated	:8;	/* System Page map Register "inuse" */
  Bit sr_sah_head	:8;	/* This SPR is System Area "Handle" */
  smap_reg_t	nextSreg;	/* next smap reg in list */
  system_map_entry_type mapEntry;	/* actual contents of smap reg */
#ifdef R6000_FAKEIO
  boolean	map_flags;
  unsigned	long	mem_kaddr;
  unsigned	long	mem_len;
#endif R6000_FAKEIO  
} smap_reg_info;

typedef struct cs_info {
  Bit	cs_allocated	:8;		/* section "inuse" by device */
  Bit	cs_expandedCS	:8;		/* CS expanded, release when done */
  Bit	cs_A24ok	:8;		/* A24 address modifier is allowed */
  Bit	cs_reserved	:8;
  dev_t	           	device;		/* dev # of owner */
  U16			userGBAflags;	/* user GBAflags to GBAreserveCsect */
  p_csh_t		p_csh;		/* physical Cache Section Handle */
  struct cs_info	*nextCS;	/* next section with same owner */
  U32			gbaAddress;	/* GBA bus addr of cache section */
  smap_reg_t		freeMRlist;	/* first free map reg (list) */
  smap_reg_t		freeMRcnt;	/* # of free map regs on list */
  sect_control_reg 	cacheControl;	/* hardware cache control fields */
} cs_info;


typedef struct system_map_info {
  I16			freeCScntA32;	/* # of available A32 Cache Sections */
  I16			freeCScntA24;	/* # of available A24 cache sections */
  struct cs_info	cacheSection[MAX_GBA_CSECT];
  struct smap_reg_info 	smapReg[MAX_SMAP_REG];
} system_map_info;


/*****************************************************************************
 * The following definitions let us manage the GBA map (translation of system
 * bus addresses into VME bus addresses).
 *
 * Currently, no valid mapping has a control word of 0 (since AM can't be 0) 
 *****************************************************************************/

typedef I16	gmap_reg_t;
typedef U32	grh_id_t;	/* Gmap Register Handle */

#define computeGRH( GBAid, firstGR, lastGR)  \
  ((grh_id_t)(((GBAid+1) << 24) | (firstGR << 10) | (lastGR)))
#define GRHtoGBAid(GRH) ((gba_id_t)((GRH >> 24) - 1))
#define GRHtoFirstGR(GRH) ((GRH >> 10) & 0x03ff)
#define GRHtoLastGR(GRH)  (GRH & 0x03ff)			 

#define FREE_GMAP_REG(gbaPtr, x) (gbaPtr->gmap.gmapReg[x].mapEntry.word == 0)

typedef struct gmap_reg_info {
  gba_map_entry_type	mapEntry;
} gmap_reg_info;
  
typedef struct gba_map_info {
  struct gmap_reg_info	gmapReg[MAX_GMAP_REG];
} gba_map_info;

/*****************************************************************************/
/*                                                                           */
/* Information global to GBA.                                                */
/*                                                                           */
/*****************************************************************************/

typedef struct gba_hw_info {
  ioa_dev_type  	*ioa;		/* k1seg pointer to IOA ctl space */
  paddr_t		gba_map_low;	/* lowest address in GBA map */
  paddr_t		gba_map_high;	/* highest address in GBA map */
  unchar	      	gbaNum;		/* number (0/1) of GBA in IOA */
} gba_hw_info;
  
typedef struct gba_sw_info	{
  ioa_dev_type  	*ioa;		/* k1seg pointer to IOA ctl space */
  paddr_t		gba_map_low;	/* lowest address in GBA map */
  paddr_t		gba_map_high;	/* highest address in GBA map */
  unchar	      	gbaNum;		/* number (0/1) of GBA in IOA */
  struct system_map_info	smap;
  struct gba_map_info 	gmap;
} gba_sw_info;


/* The following structure is setup to facilitate migration to a scheme */
/* in which the gba_sw_info structures are dynamically allocated rather     */
/* than forcing all system to contain entries for MAX_GBA GBAs          */

static struct gba_hw_info gba_hw[MAX_GBA];


#define GET_GBA_PTR(GBAid)  (gba_info_ptr[GBAid])

static gba_sw_info *gba_info_ptr[MAX_GBA+1] = { 0, 0, 0, 0, 0, 0, 0 };
			/* Last entry in table is for end-of-list. */

static int gba_count = 0;
static int flush_addr = 0;
static int flush_addr_done = 0;


/****************************************************************************/

smap_reg_t gba_alloc_sprs();


/****************************************************************************
 *
 * Initialize GBA software data structures.
 *
 * This routine should be invoked once per GBA at system boot time before
 * invoking any of the other routines in this module.  Should be invoked
 * AFTER the IOA/GBA hardware has been initialized since this routine reads
 * some values from the hardware in order to complete the setup (i.e., it
 * reads the VME address compare registers).
 *
 * This routine sets up a small data structure which contains information
 * on each of the hardware GBAs installed in the system.  This information
 * is utilized by the routines: iobadaddr and get_gba_id.
 *
 * EXTERNAL ROUTINE called by system initialization routines before
 * device driver initialization. NOT CALLED BY DEVICE DRIVERS !
 ***************************************************************************/

gba_sw_init( ioa_ctlspace_addr, gba_num )
     paddr_t	ioa_ctlspace_addr;	/* address of IOA on system bus */
     int	gba_num;		/* GBA number within IOA (0 or 1) */

{
  int GBAid;
  unsigned long ioa_base_addr, ioa_mbsize;
#ifdef	R6000_FAKEIO  
  short	*cmc_csr = (short *)0xbcde1002;
  char	*cmc_reset = (char *)0xbcdfffff;
  int	i;

  if (fakeio_cmc)  {
    fakeio_vmeaddr = CMC_VMEADDR;
    fakeio_sysaddr = CMC_SYSADDR;
    fakeio_copy16 = 1;
    cmn_err(CE_CONT, "START reset of CMC\n");
    *cmc_csr = 0;
    DELAY(1000);
    *cmc_reset = 0x01;
    i = 0;
    while ((i<20) && !(*cmc_csr & 0x0004)) {
      DELAY(1000000);
      i++;
    }
    if (*cmc_csr & 0x0004) 
      cmn_err(CE_CONT, "RESET complete in %d secs!\n", i);
    else
      cmn_err(CE_CONT, "RESET failed!\n");
  }
  else {
    if (fakeio) {
      fakeio_vmeaddr = MEM_VMEADDR;
      fakeio_sysaddr = MEM_SYSADDR;
      fakeio_copy16 = 0;
      delay_fakeio = 0;
    }
    else {
      fakeio_vmeaddr = 0;
      fakeio_sysaddr =0;
      fakeio_nohw = 0;
    }
  }
#endif	R6000_FAKEIO  

  GBAid = gba_count++;		/* count of number of GBAs initialized */
  if (gba_count > MAX_GBA)
    PANIC( "Trying to initialize more than max permissible GBAs!\n" );

  gba_hw[GBAid].gbaNum = gba_num;	/* Number within IOA (0 or 1) */

  /* Setup pointers to hardware.  IOA has control space addresses on 	*/
  /* system bus (16 KB) and data space addresses on system bus (16 MB).	*/
  /* Control space is used to setup map registers and control GBA ops,	*/
  /* such as flushing GBA cache.  Data space is used to access devices	*/
  /* on the GBA bus.							*/

  gba_hw[GBAid].ioa = (ioa_dev_type *) ioa_ctlspace_addr;
#ifndef R6000_SIM_IOC  
  if (!sbc_addr32( gba_hw[GBAid].ioa->mem.sbc_board_addr,
		  &ioa_base_addr, &ioa_mbsize ))
#endif R6000_SIM_IOC   
    PANIC("IOA has invalid data space address!\n");
  
  ioa_base_addr = PHYS_TO_K1(ioa_base_addr);
  ASSERT( (ioa_base_addr == IOA1) || (ioa_base_addr == IOA2) || \
	 (ioa_base_addr == IOA3));
  
  gba_hw[GBAid].gba_map_low =  ioa_base_addr + (gba_num * GBA_SPAN);
  gba_hw[GBAid].gba_map_high = gba_hw[GBAid].gba_map_low + GBA_SPAN - 1;

}

/****************************************************************************
 *
 * Initialize gba_sw_info software data structure.
 *
 * This routine is invoked when the first attempt is made to allocate a
 * Logical Cache Section from the GBA.  It obtains virtual memory
 * for its' data structure dynamically.  This is the "real" initialization
 * of the GBA software information.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_info_init( GBAid )
  int GBAid;
{
  gba_sw_info  *gbaPtr;             /* pointer to GBA info area */
  int CSidx, sreg, greg;
  struct cs_info	*CS;
  struct smap_reg_info	*SmapReg;
  unsigned long ioa_base_addr, ioa_bsize, CSboundary;
  vme_addr_comp_reg vme_addr_reg;
  
  ASSERT( GBAid < gba_count );
  ASSERT( gba_info_ptr[GBAid] == 0);

  gba_info_ptr[ GBAid ] =
    (gba_sw_info *)kern_calloc( (unsigned) 1, (unsigned) sizeof(gba_sw_info));

  gbaPtr = GET_GBA_PTR( GBAid );	/* Reloads above pointer */
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */


  /* Move information from gba_hw structure to gba_sw_info structure.	*/
  
  gbaPtr->gbaNum = gba_hw[GBAid].gbaNum; /* Number within IOA (0 or 1) */
  gbaPtr->ioa = gba_hw[GBAid].ioa;
  gbaPtr->gba_map_low = gba_hw[GBAid].gba_map_low;
  gbaPtr->gba_map_high = gba_hw[GBAid].gba_map_high;

  /* Finally initialize new fields in gba_sw_info structure which allow	*/
  /* us to allocate/deallocate cache sections and system map registers.	*/
  
  gbaPtr->smap.freeCScntA24 = 0;
  gbaPtr->smap.freeCScntA32 = 0;
  
  gbaPtr->ioa->mem.gba_misc_ctl[gbaPtr->gbaNum].vme_addr_comp.word = 0x0000ffff;
  vme_addr_reg.word =  (volatile unsigned long)
      gbaPtr->ioa->mem.gba_misc_ctl[gbaPtr->gbaNum].vme_addr_comp.word;
  vme_addr_reg.word = 0x0000ffff;	/* In case it's programmable */
  /*
   * Can't currently read vme_addr_comp register.  HW has hard coded PAL.
   */
  
  CSboundary = 0;
  for (CSidx = 0; CSidx < MAX_GBA_CSECT;  CSidx++) {
    CS = &gbaPtr->smap.cacheSection[ CSidx ];
    CS->cs_allocated = FALSE;
    CS->p_csh = computeCSH( GBAid, CSidx );	/* compute PCS handle */
    CS->device = 0;
    CS->nextCS = NULL;

    switch (CSidx/8)
      {
      case 0:
	CS->gbaAddress = (vme_addr_reg.bits.vme_addr1 << 24) + CSboundary;
	break;
      case 1:
	CS->gbaAddress = (vme_addr_reg.bits.vme_addr2 << 24) +CSboundary;
	break;
      case 2:
	CS->gbaAddress = (vme_addr_reg.bits.vme_addr3 << 24) +CSboundary;
	break;
      case 3:
	CS->gbaAddress = (vme_addr_reg.bits.vme_addr4 << 24) +CSboundary;
	break;
      default:
	break;		/* can't get here */
      }
    
    CSboundary += MAX_SR_PER_CS * NBPP;
    if ((CS->gbaAddress >> 24) == 0)
      CS->cs_A24ok = TRUE;		/* A24 address have high nibble 0 */
    else
      CS->cs_A24ok = FALSE;		/* else it's A32 only	*/

    if (CS->cs_A24ok)
      gbaPtr->smap.freeCScntA24++;
    else
      gbaPtr->smap.freeCScntA32++;
    
    CS->freeMRlist = 0;
    CS->freeMRcnt = 0;

    CS->cacheControl.word = 0;

  }
  
  SmapReg = getSregInfo( GBAid, 0 );
  for (sreg = 0; sreg < MAX_SMAP_REG;  sreg++, SmapReg++) {
    SmapReg->sr_allocated = FALSE;
    SmapReg->mapEntry.word = 0;
  }

  for (greg = 0; greg < MAX_GMAP_REG; greg++) {
    gbaPtr->gmap.gmapReg[greg].mapEntry.word = 0;
  }
  
  /* First four GBA map registers used for mapping A16 space */

  gbaPtr->gmap.gmapReg[0].mapEntry.bits.gba_address = 0;	/* page zero */
  gbaPtr->gmap.gmapReg[0].mapEntry.bits.control =
    VME_A16SAMOD << GMAP_VME_AM_SHIFT;
  gbaPtr->gmap.gmapReg[1].mapEntry.bits.gba_address = 1;	/* page one */
  gbaPtr->gmap.gmapReg[1].mapEntry.bits.control =
    VME_A16SAMOD << GMAP_VME_AM_SHIFT;
  gbaPtr->gmap.gmapReg[2].mapEntry.bits.gba_address = 2;	/* page 3 */
  gbaPtr->gmap.gmapReg[2].mapEntry.bits.control =
    VME_A16SAMOD << GMAP_VME_AM_SHIFT;
  gbaPtr->gmap.gmapReg[3].mapEntry.bits.gba_address = 3;	/* page four */
  gbaPtr->gmap.gmapReg[3].mapEntry.bits.control =
    VME_A16SAMOD << GMAP_VME_AM_SHIFT;

  /* Next GBA map register is reserved for IACK */

  gbaPtr->gmap.gmapReg[4].mapEntry.bits.op_code = 1;

  /*
   * Reserve last GBA map register to map last page of A24 space.
   * (currently required to access serial controllers).
   */

  gbaPtr->gmap.gmapReg[MAX_GMAP_REG-1].mapEntry.bits.gba_address =  1023;
	/* Last page of A24 address space */
  gbaPtr->gmap.gmapReg[MAX_GMAP_REG-1].mapEntry.bits.control =
    VME_A24SAMOD << GMAP_VME_AM_SHIFT;
  
  return(TRUE);
}


/****************************************************************************
 * Given a control space address of an I/O controller on the system bus,
 * determine the GBAid of the GBA handling that controller.
 *
 * This routine assumes that the GBA map has been properly setup for each
 * GBA in the system.  Returns FALSE if address was not within space of
 * any known GBA.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
get_gba_id( dev_addr, gba_id )

paddr_t	dev_addr;	/* address of controller on system bus */
gba_id_t *gba_id;	/* GBA id number (0..5) */

{
  gba_sw_info *gba;
  int i;

#ifdef R6000_SIM_IOC
  if (gba_count == 0) {
    gba_count = 1;
    gba_hw[0].gbaNum = 0;
    gba_hw[0].ioa = (ioa_dev_type *) &ioa_dev[0];
    gba_hw[0].gba_map_low = IOA1 + GBA_SPAN;
    gba_hw[0].gba_map_high = gba_hw[0].gba_map_low + GBA_SPAN - 1;

    ioa_dev[0].mem.gba_misc_ctl[0].vme_addr_comp.word = 0x0000ffff;
    ioa_dev[0].mem.sbc_board_addr = IOA1_BRD;
  }
#endif R6000_SIM_IOC

  for (i=0; i < gba_count; i++ )   {

    if ((dev_addr >= gba_hw[i].gba_map_low) &&
	(dev_addr <= gba_hw[i].gba_map_high))
      {
	/* We initialize the remaining GBA software structre when first	*/
	/* attempt is made to access it.				*/
	
  	if (gba_info_ptr[i] == 0)
	  gba_info_init( i );
					   
	*gba_id = i;
	return( TRUE );
      }
  }
  
  *gba_id = MAX_GBA;		/* This will index end-of-list element */
  return( FALSE );
}


/****************************************************************************
 *
 * Load Physical Page Map entry into the System Page Map Register (SPR)
 * in the GBA hardware.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_load_spr( GBAid, sreg, PPnum )

gba_id_t GBAid;			/* GBAid of GBA containing sreg */
smap_reg_t sreg;		/* system map register ID within GBA */
U32 PPnum;			/* System bus Physical Page number */

{
  gba_sw_info *gbaPtr;		/* pointer to GBA information */
  volatile ioa_dev_type  *ioaPtr;
  
  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );
  ASSERT( (sreg >=0) && (sreg < MAX_SMAP_REG));
  ASSERT( gbaPtr->smap.smapReg[sreg].sr_allocated );
  
  gbaPtr->smap.smapReg[sreg].mapEntry.word = 0;	/* Zero entire mapEntry 1st */
  gbaPtr->smap.smapReg[sreg].mapEntry.bits.pp_num = PPnum;
  
  /**********************************************/
  /* HW: Load System Page Map Register in IOA	*/
  /**********************************************/

  ioaPtr = gbaPtr->ioa;
  ASSERT( ioaPtr );

#ifdef	R6000_FAKEIO
  if (fakeio_nohw) return(TRUE);
#endif	R6000_FAKEIO  
  ioaPtr->mem.gba_smaps[ gbaPtr->gbaNum ][sreg] =
    gbaPtr->smap.smapReg[sreg].mapEntry;
  
  return( TRUE );
}


/****************************************************************************
 *
 * Start flush of GBA Physical Cache Section.  This routine initiates a
 * flush cache on the GBA hardware.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_start_pcs_flush( GBAid, PCSid )

gba_id_t GBAid;
pcs_id_t PCSid;

{
  gba_sw_info *gbaPtr;		/* pointer to GBA information */
  volatile ioa_dev_type  *ioaPtr;

  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );
  
  /* Write GBA address to be flushed from cache into the flush_reg.	*/
  /* Any valid GBA address in the cache section will suffice since the	*/
  /* entire GBA cache section is flushed.				*/

  /* HW: Access GBA hardware to initiate flush	*/
  
  ioaPtr = gbaPtr->ioa;
  ASSERT( ioaPtr );

#ifdef	R6000_FAKEIO
  if (fakeio_nohw) return(TRUE);
#endif	R6000_FAKEIO  
  if (*(volatile uint *)CSR_IVECTSET 
      & (CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP |
	 CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
	 CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP )
	 ) {
    cmn_err(CE_WARN, "IOA LOP done when starting flush!");
      
#ifdef R6000_BUG_SBC_TIMER
    r6000_bug_ivectclr(CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP|
	 CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
	 CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP );
#else    
    *(volatile uint *)CSR_IVECTCLR =
      CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP |
      CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
      CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP;
#endif R6000_BUG_SBC_TIMER    
  };

  if (ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].wr_reg) {
    /* Attempt to reset the FLUSH_OP register so a dump may be taken. */
    ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].wr_reg = 0;
    cmn_err (CE_PANIC, "Flush complete set when initiating a flush!");
  }

  ioaPtr->mem.gba_misc_ctl[ gbaPtr->gbaNum ].flush_reg =
    gbaPtr->smap.cacheSection[ PCSid ].gbaAddress;

  if (flush_addr)
    cmn_err(CE_PANIC, "flush_addr NON_ZERO: 0x%x", flush_addr);
  else
    flush_addr = gbaPtr->smap.cacheSection[ PCSid ].gbaAddress+1;
    
#ifdef R6000_SIM_IOC
  ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].wr_reg |=
      (1<<PCSid);	/* simulate hardware setting "flush done" */
#endif R6000_SIM_IOC
  return( TRUE );
}


/****************************************************************************
 *
 * Wait for flush of GBA Physical Cache Section to complete.  This routine
 * waits for completion of flush operation by GBA hardware.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_wait_pcs_flush( GBAid, PCSid )

gba_id_t GBAid;
pcs_id_t PCSid;

{
  gba_sw_info *gbaPtr;		/* pointer to GBA information */
  volatile ioa_dev_type  *ioaPtr;
  int i;
  static int imax=128;

  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );

  /******************************************************/
  /* HW: Check GBA hardware register for flush done!	*/
  /******************************************************/
  
  ioaPtr = gbaPtr->ioa;
  ASSERT( ioaPtr );
#ifdef	R6000_FAKEIO
  if (fakeio_nohw) return(TRUE);
#endif	R6000_FAKEIO  
retry:  
  i = 0;
  while (!(*(volatile uint *)CSR_IVECTSET
	   & (CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP |
	      CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
	      CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP)
	      )) {
    if (i++ > 16384) {
      if (i=ioaPtr->mem.gba_ints[gbaPtr->gbaNum].long_op[GBA_FLUSH_OP].wr_reg){
	cmn_err(CE_WARN,
		"GBA flush completed but no LOP interrupt, adr: 0x%x op: 0x%x",
		flush_addr, i);
	ioaPtr->mem.gba_ints[gbaPtr->gbaNum].long_op[ GBA_FLUSH_OP ].and_reg =
	  ~(i);	/* Reset any set bits */
      } else
	cmn_err(CE_WARN,
		"GBA flush did NOT complete, flush_adr: 0x%x flush_op: 0x%x",
		flush_addr, i);
      flush_addr = 0;
      gba_start_pcs_flush( GBAid, PCSid );	/* Retry flush */
      i = 0;
    }
    DELAY(4);	/* don't keep System Bus flooded */
  }
  if (i > imax) {
    imax = i;
    cmn_err( CE_CONT, "!New GBA cache wait max: %d\n", imax );
  }

#ifdef R6000_BUG_SBC_TIMER  
  r6000_bug_ivectclr(CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP |
		     CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
		     CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP );
#else  
  *(volatile uint *)CSR_IVECTCLR =
    CSR_IVECTSET_IOA1_GBA0_LOP | CSR_IVECTSET_IOA1_GBA1_LOP |
    CSR_IVECTSET_IOA2_GBA0_LOP | CSR_IVECTSET_IOA2_GBA1_LOP |
    CSR_IVECTSET_IOA3_GBA0_LOP | CSR_IVECTSET_IOA3_GBA1_LOP;
#endif R6000_BUG_SBC_TIMER  

  i = ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].wr_reg;
  if (i) {
    if (!(i & (1<<PCSid))) {
      cmn_err(CE_WARN,
	      "LOP done, but WRONG flush op, flush op: 0x%x, expected: 0x%x",
	      i, 1<<PCSid);
      ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].and_reg =
	~(i);	/* Reset any set bits */
      flush_addr = 0;
      gba_start_pcs_flush( GBAid, PCSid );	/* Retry flush */
      goto retry;
    }
  }
  else
      cmn_err(CE_PANIC, "LOP done, but no flush op bits set!");
    
  ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].and_reg = 
    ~(1<<PCSid);	/* turn off the cache section flush done bit */
  
  if (!flush_addr)
    cmn_err(CE_PANIC, "flush_addr is ZERO!");
  else {
    flush_addr_done=flush_addr;
    flush_addr=0;
  }
    
#ifdef R6000_SIM_IOC
  ioaPtr->mem.gba_ints[ gbaPtr->gbaNum ].long_op[ GBA_FLUSH_OP ].wr_reg &= 
     ~(1<<PCSid);	/* simulate hardware operation of "and_reg" */
#endif

  return( TRUE );
}

/****************************************************************************
 *
 * Rebuild the System map Register (SR) list of the Physical Cache Section
 *
 * This routine performs a linear search of all SRs in the PCS and constructs
 * a free list in the PCS info area.  This is useful if we have just performed
 * an allocation which required contiguous SRs since this kind of allocation
 * does not use the free list and hence destroys it.
 *
 * It is safe to call this routine at any time to reconstruct the free list
 * however it should only be necessary after a contigous allocation OR if
 * we simply want to end up with the free list in sorted order.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_rebuild_sr_list( CS )

struct cs_info *CS;	/* Physical Cache Section to initialize */

{
  gba_id_t GBAid;
  register smap_reg_t sreg;
  register struct smap_reg_info	*SmapReg;

  GBAid = CSHtoGBAid( CS->p_csh );
  
  CS->freeMRlist = NIL_SPR;
  CS->freeMRcnt = 0;
  
  /**************************************************************/
  /* This loop searches for unallocated System Map Registers	*/
  /* and rebuilds the SR free list in the PCS.			*/	
  /**************************************************************/
  
  sreg = CSHtoMaxSregID( CS->p_csh );
  SmapReg = getSregInfo( GBAid, sreg );
  
  while ( sreg >= CSHtoMinSregID( CS->p_csh )) { 
    if (!SmapReg->sr_allocated) {
      if (CS->freeMRcnt)
	SmapReg->nextSreg = CS->freeMRlist;
      else
	SmapReg->nextSreg = NIL_SPR;
      CS->freeMRlist = sreg;
      CS->freeMRcnt++;
    }
    sreg--;
    SmapReg--;
  }

  return( TRUE );
}


/****************************************************************************
 *
 * Initialize Physical Cache Section.
 *
 * If the request is satisfied, this routine returns a result of "true"
 * and returns a valid csectHandle.  If the request is NOT satisfied, then
 * the returned value is "false".
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_setup_pcs( CS,  dev_no, GBAflags )

struct cs_info *CS;	/* Physical Cache Section to initialize */
dev_t	dev_no;			/* device which needs cache section */
U16	GBAflags;		/* cache section options */


{
  gba_sw_info  *gbaPtr;
  volatile ioa_dev_type  *ioaPtr;

  /***************************************************************/
  /* Set flags relating to use of this cache section.  These are */
  /* options supported by the software.                          */
  /***************************************************************/
  
  CS->userGBAflags = GBAflags;
  CS->cs_expandedCS = FALSE;
  
  CS->device = dev_no;
  CS->nextCS = NULL;
  
  /***************************************************************/
  /* Set cache control flags for section as specified by caller. */
  /* These settings control hardware options on GBA.             */
  /***************************************************************/
  
  CS->cacheControl.word = 0;		/* Zero all fields. */
  CS->cacheControl.write_reg.non_cacheable =
    ((GBAflags & DMA_NOCACHE) ? 1 : 0);
  CS->cacheControl.write_reg.random =
    ((GBAflags & DMA_RANDOM) ? 1 : 0);
  CS->cacheControl.write_reg.local_to_vme = FALSE;
  CS->cacheControl.write_reg.no_partial_wrt =
    ((GBAflags & DMA_NOPARTIAL) ? 1 : 0);
  
  /******************************************************/
  /* HW: Initialize actual GBA cache section registers	*/
  /******************************************************/
  
  gbaPtr = GET_GBA_PTR( CSHtoGBAid( CS->p_csh ));
  ASSERT( gbaPtr );
  ioaPtr = gbaPtr->ioa;
  ASSERT( ioaPtr );
  ioaPtr->mem.gba_misc_ctl[ gbaPtr->gbaNum ].data_address_reg =
    CS->gbaAddress;
  ioaPtr->mem.gba_misc_ctl[ gbaPtr->gbaNum ].sect_control =
    CS->cacheControl;
  
  /**********************************************************************/
  /* Initialize system map registers for this cache section. 		*/
  /* All System Map Registers should already be unallocated since this	*/
  /* routine should not be invoked to init the PCS if some registers 	*/
  /* are still inuse.  Rebuild the free list of registers so they are	*/
  /* once again in sorted order.					*/
  /**********************************************************************/
  
  gba_rebuild_sr_list( CS );

  if (CS->freeMRcnt != MAX_SR_PER_CS)
    PANIC( "Should NOT be invoked if some Smap Regs inuse!\n" );
  return( TRUE );

}

/****************************************************************************
 *
 * Check if Logical Cache Section can be "contracted" (i.e. some of its'
 * Physical Cache Sections returned).
 * 
 * If the request is satisfied, this routine returns a result of "true"
 * and returns a valid csectHandle.  If the request is NOT satisfied, then
 * the returned value is "false".
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_contract_lcs( GBAid, LCSid )

gba_id_t GBAid;
lcs_id_t LCSid;

{
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  register struct cs_info  *CS, *prevCS;
  register long	oldPri;

  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */

  prevCS = getLCSinfo( GBAid, LCSid );
  oldPri = splbio();		/* START: critical section on CS queue */
  
  /**********************************************************************/
  /* Scan all Physical Cache Sections in the Logical Cache Section.  If	*/
  /* we find a PCS which was due to expansion and is currently unused,	*/
  /* then we deallocate it. NEVER deallocate the LCS root PCS!		*/
  /**********************************************************************/

  while (CS = prevCS->nextCS) {
    ASSERT( CS->cs_allocated );
    if ((CS->cs_expandedCS) && (CS->freeMRcnt >= MAX_SR_PER_CS)) {

      /*********************************/
      /* We have found a PCS to return */
      /*********************************/
      
      prevCS->nextCS = CS->nextCS;	/* Dequeue PCS from LCS */
      CS->cs_allocated = FALSE;
      if (CS->cs_A24ok)
	gbaPtr->smap.freeCScntA24++;
      else
	gbaPtr->smap.freeCScntA32++;
    }
    else

      /******************************************/
      /* Can't return this PCS, check next PCS	*/
      /******************************************/
      
      prevCS = CS;
  }
  splx( oldPri );
  return( TRUE );
}


/****************************************************************************
 *
 * Expand Logical Cache Section by allocating an additional Physical
 * Cache Section.
 * 
 * If the request is satisfied, this routine returns a result of "true"
 * and returns a valid csectHandle.  If the request is NOT satisfied, then
 * the returned value is "false".
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_expand_lcs( GBAid, LCSid )

gba_id_t GBAid;
lcs_id_t LCSid;

{
  struct cs_info *LCS;
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  register int CSidx;
  register struct cs_info  *CS, *prevCS;
  register long	oldPri;
  boolean getA24cs;

  LCS = getLCSinfo( GBAid, LCSid );
  if (!(LCS->userGBAflags & EBA_CS_AUTOEXP))
    return(FALSE);		/* Auto-expansion NOT allowed on this LCS */
  
  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */

  oldPri = splbio();		/* START: critical section on CS queue */
  
  /**********************************************************************/
  /* If caller does not require A24 GBA address we first attempt to	*/
  /* satisfy the entire request from physical cache sections in A32	*/
  /* GBA address space.							*/
  /**********************************************************************/
  
  if (!(getA24cs = (LCS->userGBAflags & DMA_A24_AM))) {
    if ((gbaPtr->smap.freeCScntA32 -= 1) < 0) {
      gbaPtr->smap.freeCScntA32 += 1;
      getA24cs = TRUE;		/* No room in A32, go try A24 */
    }
  }
  
  /**************************************************************/
  /* If request not satisfied with A32 space, then try A24.	*/
  /* try to allocate from A24 CS space.				*/
  /**************************************************************/

  if (getA24cs) {
    if ((gbaPtr->smap.freeCScntA24 -= 1) < 0) {
      gbaPtr->smap.freeCScntA24 += 1;
      splx( oldPri );
      return( FALSE );		/* No room in either address space */
    }
  }

  /**************************************************************/
  /* Allocate the Physical Cache Section we have reserved.	*/
  /**************************************************************/

  prevCS = LCS;
  while (prevCS->nextCS)	/* Find last PCS in the LCS */
    prevCS = prevCS->nextCS;
  
  CS = &gbaPtr->smap.cacheSection[ 0 ];
  for (CSidx = 0; CSidx < MAX_GBA_CSECT;  CSidx++, CS++) {
    
    if (!CS->cs_allocated && (getA24cs == CS->cs_A24ok))
      {
	CS->cs_allocated = TRUE;
	
	if (!gba_setup_pcs( CS, LCS->device, LCS->userGBAflags ))
	  PANIC( "Can't setup Physical Cache Section!\n" );

	CS->cs_expandedCS = TRUE;	/* Expansion CS are auto-released */
	
	/*****************************************************************/
	/* Perform necessary bookkeeping to form a Logical Cache Section */
	/* out of several Physical Cache Sections.                       */
	/*****************************************************************/
	
	prevCS->nextCS = CS; /* link physical CS to logical CS */
	splx( oldPri );	/* END: critical section on CS queue */
	return(TRUE);
      }
  }
  
  PANIC("We should NEVER exit the FOR loop normally!\n");
  return( FALSE );
}

/****************************************************************************
 *
 * Allocate a group of System Page Registers which will appear as
 * consecutive addresses on the GBA address bus.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

smap_reg_t
gba_alloc_contig_sprs( LCS, pages)

struct cs_info *LCS;
int pages;		/* number of consecutive pages required */

{
  gba_id_t GBAid;
  register struct cs_info *PCS;
  register smap_reg_info *SmapReg;
  smap_reg_t sreg, firstSR, nextSR;
  register int needPages;
  long oldPri;
  
  if ((pages <= 0) || (pages > MAX_SR_PER_CS))
    return( NIL_SPR );		/* Contiguous pages must be in same PCS */
  
  ASSERT( LCS );
  GBAid = CSHtoGBAid( LCS->p_csh );
  needPages = pages;
  
  /**************************************************************/
  /* Attempt to find enough contiguous System Map Registers in	*/
  /* a single Physical Cache Section within the Logical Cache	*/
  /* Section.							*/
  /**************************************************************/

  oldPri = splbio();		/* START: critical section on SR queue */
  do {
    PCS = LCS;
    while (PCS && needPages) {
      ASSERT( PCS->cs_allocated );
      if (PCS->freeMRcnt >= pages) {
	sreg = CSHtoMinSregID( PCS->p_csh );
	SmapReg = getSregInfo( GBAid, sreg );

	/****************************************************************/
	/* Attempt to find "pages" consecutive unallocated System Map	*/
	/* Registers to satisfy caller's map request.			*/
	/****************************************************************/
	
	while (needPages && (sreg <= CSHtoMaxSregID(PCS->p_csh))) {
	  if (SmapReg->sr_allocated)
	    needPages = pages;	/* Reset page count when hit allocated page */
	  else
	    needPages--;
	  sreg++;
	  SmapReg++;
	}
      };
      
      if (needPages) {		/* If don't have enough SRs, go to next PCS */
	PCS = PCS->nextCS;
	needPages = pages;	/*  and reset count of needed pages.	*/
      }
    }

    /********************************************************************/
    /* If we have not been able to satisfy request from PCS currently	*/
    /* in the LCS, then expand the LCS and retry the search.  This	*/
    /* requires at most ONE expansion.					*/
    /********************************************************************/
    
  } while (needPages && gba_expand_lcs( GBAid, CSHtoCSid(LCS->p_csh) ));
    
  /**********************************************************************/
  /* If we don't have enough SR by now, reject caller's request.	*/
  /* Otherwise, we must mark the SRs as "allocated" and rebuild the 	*/
  /* the free list of SRs since we may be taking elements from random	*/
  /* positions in the free list.					*/
  /*								       	*/
  /*	needPages == 0  IFF allocation was successful			*/
  /*	PCS => Physical Cache Section containing allocated map regs	*/
  /*	sreg = SR index of first System map Register AFTER free group	*/
  /*	SmapReg => first System map Reg AFTER free group		*/
  /**********************************************************************/

  if (needPages) {
    splx( oldPri );
    return( NIL_SPR );		/* return invalid (EOL) SR */
  }

  firstSR = sreg - pages;	/* Compute index of 1st SR from last SR */
  nextSR = NIL_SPR;			/* SR index for End-of-list */

  for ( ; sreg > firstSR; ) {
    SmapReg--;			/* Backup to previous SR in chain */
    SmapReg->sr_allocated = TRUE;	/* Mark "inuse" */
    SmapReg->sr_sah_head = FALSE;	/* Not an SPH (SAH) */
    SmapReg->nextSreg = nextSR;	/* Link to next SR in chain */
    nextSR = --sreg;
  }

  SmapReg->sr_sah_head = TRUE;	/* First one is System Area Handle */
  gba_rebuild_sr_list( PCS );	/* Rebuild free SR list for current PCS */

  splx( oldPri );		/* END: Critical Section on SR queue */

  return( firstSR );

}


/****************************************************************************
 *
 * Allocate a group of System map Page Registers.  These do NOT need to be
 * contiguous address on the GBA address bus.  This routine will return
 * the system page map register number of the first register in the linked
 * list of SRs.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

smap_reg_t
gba_alloc_sprs( LCS, needPages)

struct cs_info *LCS;
int needPages;		/* number of consecutive pages required */

{
  gba_id_t GBAid;
  register struct cs_info *PCS;
  register smap_reg_info *SmapReg;
  smap_reg_t sreg, firstSR;
  long oldPri;
  
  if ((needPages <= 0) || (needPages > MAX_SR_PER_CS))
    return( NIL_SPR );		/* Somewhat arbitrary restriction on length */
  
  ASSERT( LCS );
  GBAid = CSHtoGBAid( LCS->p_csh );
  
  /**************************************************************/
  /* Attempt to find enough contiguous System Map Registers in	*/
  /* a single Physical Cache Section within the Logical Cache	*/
  /* Section.							*/
  /**************************************************************/

  firstSR = NIL_SPR;
  SmapReg = NULL;
  do {
    PCS = LCS;
    oldPri = splbio();		/* START: critical section on SR queue */
    
    while (PCS && needPages) {
    /********************************************************************/
    /* START: Loop on all Physical Cache Sections (PCS) within this	*/
    /*	      Logical Cache Section (LCS).				*/
    /********************************************************************/
      ASSERT( PCS->cs_allocated );
      if (PCS->freeMRcnt) {
	sreg = PCS->freeMRlist;		/* First available SR in PCS 	*/
	if (SmapReg)
	  SmapReg->nextSreg = sreg;	/* Attach to our partial list	*/
	else
	  firstSR = sreg;		/* or start the partial list.	*/

	/********************************************************/
	/* Allocate as many entries as we need from this PCS'	*/
	/* free list.  Mark entries we take as "allocated".	*/
	/********************************************************/

	while (needPages && (sreg >= 0)) {
	  SmapReg = getSregInfo( GBAid, sreg );
	  ASSERT( !SmapReg->sr_allocated );
	  SmapReg->sr_allocated = TRUE;	/* Entry is now "inuse" */
	  SmapReg->sr_sah_head = (firstSR == sreg);

	  PCS->freeMRcnt--;
	  needPages--;
	  sreg = SmapReg->nextSreg;
	}
	/********************************************************/
	/* We have either completed our allocation OR exhausted	*/
	/* the free SR supply of the current PCS.  Mark the	*/
	/* end-of-list for the SR chain.			*/
	/********************************************************/
	
	if (needPages) {
	  ASSERT( PCS->freeMRcnt == 0);
	  PCS->freeMRlist = NIL_SPR;
	}
	else {
	  SmapReg->nextSreg = NIL_SPR;	/* Terminate our completed list */
	  PCS->freeMRlist = sreg;	/* and reset SR free list head.	*/
	}
      };

      PCS = PCS->nextCS;		/* Move to next PCS within LCS	*/
    /********************************************************************/
    /* END: Loop on all PCS within LCS					*/
    /********************************************************************/
    }

    /********************************************************************/
    /* If we have not been able to satisfy request from PCS currently	*/
    /* in the LCS, then expand the LCS and retry the search.  This	*/
    /* requires at most ONE expansion.					*/
    /********************************************************************/
    
    splx( oldPri );		/* END: Critical Section on SR queue */
    
  } while (needPages && gba_expand_lcs( GBAid, CSHtoCSid(LCS->p_csh) ));
  
  /**********************************************************************/
  /* If we don't have enough SR by now, reject caller's request.	*/
  /*	needPages == 0  IFF allocation was successful			*/
  /**********************************************************************/

  if (needPages) {
    if (firstSR >= 0) 		/* If allocated something, return it */
      if (!vme_iounmap( computeSPH( LCS->p_csh, firstSR )))
	PANIC("gba_alloc_sprs: cant release SPRs from aborted allocation!\n");
    return( NIL_SPR );		/* return invalid (EOL) SR */
  }

  return( firstSR );
}


/****************************************************************************
 *
 * Release GBA Cache Section(s).
 *
 * Release GBA cache section previously reserved by call to gba_reserve_lcs.
 * Caller must have previously unmapped ALL of the system map registers
 * allocated or this routine will PANIC.
 *
 * Most device drivers will NEVER call this routine since they will tend
 * to permenently reserve their logical cache section.  It is included for
 * completeness and ease of testing.
 *
 * EXTERNAL ROUTINE calleable by device drivers.
 ***************************************************************************/

boolean
vme_release_iomap(csh)

	csh_type csh;	/* cache section descriptor */

{
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  register struct cs_info  *CS;
  register long	oldPri;
	
  gbaPtr = GET_GBA_PTR( CSHtoGBAid( csh ) );
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */
  CS = CSinfoPtr( csh );
  ASSERT( CS );			/* Make certain we have good CS ptr */

  oldPri = splbio();		/* START: critical section on CS queue */
  while (CS) {
    ASSERT( CS->cs_allocated );	/* Make sure it was reserved */
    if (CS->freeMRcnt != MAX_SR_PER_CS)
      PANIC("Can't release CS with registers in-use.\n");
    
    CS->cs_allocated = FALSE;	/* mark the PCS available */
    
    if (CS->cs_A24ok)		/* update GBA count of free CS */
      gbaPtr->smap.freeCScntA24++;
    else
      gbaPtr->smap.freeCScntA32++;

    CS = CS->nextCS;			/* go to next PCS in the LCS */
  }
  splx( oldPri );		/* END: critical section on CS queue */
}

/****************************************************************************
 *
 * Reserve GBA Cache Section(s) for use by specified device.
 * 
 * This routine will allocate sufficient GBA cache sections to enable
 * the caller to map at least "minPages" of system memory onto the I/O
 * bus.  All pages to be mapped in this section must share the same GBA
 * options, specified by GBAflags.  The returned csectHandle must be passed
 * to gba_map_spage in order to actually map a page.
 *
 * All pages mapped within a cache section share the same lines in the
 * GBA cache.
 *
 * If EBA_CS_AUTOEXP is set in the GBAflags, then gba_map_spage will
 * automatically attempt to allocate additional cache sections when all
 * system map entries within the section have been allocated.
 *
 * If the request is satisfied, this routine returns a result of "true"
 * and returns a valid csectHandle.  If the request is NOT satisfied, then
 * the returned value is "false".
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean 
vme_reserve_iomap( dev_no, dev_ctlspace, minPages, csh, userGBAflags )

dev_t		dev_no;		/* device which needs cache section */
paddr_t		dev_ctlspace;	/* system address of device control space */
int		minPages;	/* min number of pages to reserve */
csh_type	*csh;		/* section descriptor -- returned */
int		userGBAflags;	/* cache section options */


{
  gba_id_t	GBAid;		/* GBA which contains the device */
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  unsigned short  GBAflags;	/* GBA flags from user */
  int		needCS;		/* # of needed Cache Sections */
  register int CSidx;
  register struct cs_info	*CS, *prevCS;
  long		oldPri;
  boolean	getA24cs;

  if (!get_gba_id( dev_ctlspace, &GBAid ))
    PANIC( "Can't find GBAid for specified controller!\n" );

#ifdef	R6000_FAKEIO
  userGBAflags = userGBAflags |DMA_A24_AM;
#endif	R6000_FAKEIO  
  GBAflags = (unsigned short)userGBAflags;
  *csh = 0;
  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */

  /**********************************************************************/
  /* First we make certain that we have sufficient Cache Sections	*/
  /* available to satisfy the request.  The CS are effectively    	*/
  /* reserved here by decrementing the count of available CSs.    	*/
  /**********************************************************************/
	
  if (minPages <= 0)
    needCS = 1;
  else
    needCS = (minPages + MAX_SR_PER_CS - 1) / MAX_SR_PER_CS;


  oldPri = splbio();		/* START: critical section on CS queue */
  if (getA24cs = (GBAflags & DMA_A24_AM)) {

    /********************************************************************/
    /* Caller requires that all physical cache sections within the	*/
    /* requested logical cache section be in A24 GBA address space.	*/
    /********************************************************************/
    
    if ((gbaPtr->smap.freeCScntA24 -= needCS) < 0) {
      gbaPtr->smap.freeCScntA24 += needCS;
      splx( oldPri );
      return( FALSE );
    }
  }
  else {

    /********************************************************************/
    /* Caller does not require A24 GBA address so we first attempt to	*/
    /* satisfy the entire request from physical cache sections in A32	*/
    /* GBA address space.						*/
    /********************************************************************/
  
    if ((gbaPtr->smap.freeCScntA32 -= needCS) < 0) {

      /******************************************************************/
      /* We couldn't get all needed Cache Sections from A32 land so	*/
      /* try to allocate extra ones from A24 CS space.			*/
      /******************************************************************/
      
      if (gbaPtr->smap.freeCScntA24 + gbaPtr->smap.freeCScntA32 >= 0) {
	gbaPtr->smap.freeCScntA24 += gbaPtr->smap.freeCScntA32;
	gbaPtr->smap.freeCScntA32 = 0;
      }
      else {
	gbaPtr->smap.freeCScntA32 += needCS;
	splx( oldPri );
	return( FALSE );
      }
    }
  }

  /**************************************************************/
  /* Allocate the Physical Cache Sections we have reserved.	*/
  /* If this Logical Cache Section was for A32 address space	*/
  /* AND if we had to allocate some of the PCS from A24 space	*/
  /* THEN we will make two passes over the PCS array.  The 1st	*/
  /* time we will allocate the remaining A32 PCS, the 2nd time	*/
  /* we complete the allocation with A24 PCS.			*/
  /**************************************************************/

  prevCS = NULL;
  do
    for (CSidx = 0; CSidx < MAX_GBA_CSECT;  CSidx++) {
      CS = &gbaPtr->smap.cacheSection[ CSidx ];
    
      if (!CS->cs_allocated && (getA24cs == CS->cs_A24ok))
	{
	  CS->cs_allocated = TRUE;

	  if (!gba_setup_pcs( CS, dev_no, GBAflags ))
	    PANIC( "Can't setup Physical Cache Section!\n");
	  
	  /*****************************************************************/
	  /* Perform necessary bookkeeping to form a Logical Cache Section */
	  /* out of several Physical Cache Sections.                       */
	  /*****************************************************************/
      
	  if (!(*csh))  *csh = CS->p_csh;
	  if (prevCS) prevCS->nextCS = CS; /* link physical CS to logical CS */
	  prevCS = CS;
	  if (--needCS <= 0) {
	    splx( oldPri );	/* END: critical section on CS queue */
	    return(TRUE);
	  }
	}
    }

  /* We have completed a pass over the available PCS without satisfying	*/
  /* the caller's request.  The only valid reason for this is that we	*/
  /* have exhausted the supply of A32 PCS and now need to allocate one	*/
  /* of the A24 PCS to complete the A32 request.			*/

  while ( getA24cs = !getA24cs );
  
  PANIC("We should NEVER exit the FOR loop normally!\n");
  return( FALSE );
}

/****************************************************************************
 *
 * Flush and/or Invalidate VME I/O cache (i.e. GBA cache section).
 *
 * This routine guarantees that input from a device or output to a device
 * for the cache section has been flushed into system memory.  This
 * routine will flush the indicated kernel address area from this smapArea
 * in the GBA cache.  Special case of (kaddr == len == 0) is used to signify
 * a flush of the entire specified system map area.
 *
 * This section must have been allocated by a call to vme_iomap.
 *
 * If there is an error, FALSE will be returned, else TRUE.  An ERROR is
 * signalled if a flush attempt fails OR if the addres range from kaddr to
 * (kaddr + len -1) is NOT within the map area.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
vme_ioflush( smapArea, kaddr, len )

sah_type smapArea;		/* map area descriptor */
kaddr_t  kaddr;			/* system address (k0,k1,k2) */
long len;			/* length of area (bytes) */

{
  gba_id_t GBAid;
  smap_reg_t SPRid;
  pcs_id_t PCSid;
  register struct cs_info *PCS;
  register smap_reg_info *sregInfo;
  phys_page_num_t PPnum;
  boolean reqOK, foundSAH;
  long oldPri;

#ifdef	R6000_FAKEIO
  /* First time through report type of I/O to system console */
  
  if (fakeio != fakeio_reported) {
    if (fakeio)
      fakeio_vreported = fakeio_verify+1;	/* Report FAKE I/O below */
    fakeio_reported = fakeio;
  }
  
  if (fakeio) {
    r6000_fakeio_xfer(smapArea, kaddr, len, 0);   /* Copy data to VME memory */

    /*
     * If we're using FAKE I/O then report value of fakeio_verify the first
     * time through here or whenever it changes.
     */
    if (fakeio_verify != fakeio_vreported) {
      if (fakeio_verify)
	cmn_err(CE_CONT, "Using verify option in FAKE I/O\n");
      else
	cmn_err(CE_WARN, "NOT using verify option in FAKE I/O");
      fakeio_vreported = fakeio_verify;
    }
    
    if (fakeio_nohw) return(TRUE);
  }
#endif	R6000_FAKEIO  
  foundSAH = FALSE;
  ASSERT( smapArea );
  GBAid = SPHtoGBAid( smapArea );  /* For now, smapArea handle is SPH */
  SPRid = SPHtoSPRid( smapArea );	/* initial sreg in linked list */
  /* ASSERT( (GBAid >= 0) && (GBAid < gba_count) ); */
  if ((GBAid < 0)  ||  (GBAid >= gba_count)) {
      cmn_err(CE_PANIC,"smapArea 0x%x produces illegal GBAid %d"
	     ,smapArea, GBAid);
  }

  if ((kaddr == 0) && (len == 0)) {
    /**************************************************************
     * Special (common) case to flush entire map area
     **************************************************************/
      PCS = getPCSinfo( GBAid, SPRid ); /* get ptr to Phys Cache Sect info */
      ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
      ASSERT( PCS->cs_allocated );
      
      sregInfo = getSregInfo( GBAid, SPRid );	/* ptr to Sys Map Reg info */
      ASSERT( sregInfo );
      ASSERT( sregInfo->sr_allocated );
      ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */
  }
  else {
    /**************************************************************
     * First determine Physical Page Number of kernel address. 
     **************************************************************/
    
    if (IS_KSEG2( kaddr ))
      PPnum = kvtokptbl(kaddr)->pgm.pg_pfn;  /* PPnum from K2SEG address */
    else
      if (IS_KUSEG( kaddr ))
	return(FALSE);		/* KUSEG address is ILLEGAL */
      else
	PPnum = svtopfn( kaddr );	/* extract PPnum from K0seg or K1seg */
    
    /**************************************************************
     * Now look through chain of pages in smapArea and find PPnum
     **************************************************************/
    do {
      PCS = getPCSinfo( GBAid, SPRid ); /* get ptr to Phys Cache Sect info */
      ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
      ASSERT( PCS->cs_allocated );
      
      sregInfo = getSregInfo( GBAid, SPRid );	/* ptr to Sys Map Reg info */
      ASSERT( sregInfo );
      ASSERT( sregInfo->sr_allocated );
      ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */
      
      if (!(reqOK =  (PPnum == sregInfo->mapEntry.bits.pp_num)))
	SPRid = sregInfo->nextSreg;	/* move to next sreg in sarea */
    }
    while ((!reqOK) && (SPRid >= 0));
    
    if (!reqOK)
      return( FALSE );		/* start address NOT in area */
  }

  /**************************************************************/
  /* If we get here, we have FOUND the starting page within the	*/
  /* area for the flush.  Flush all necessary PCS.		*/
  /* We initiate the flush of the first Physical Cache Section.	*/
  /**************************************************************/
  
  PCSid = CSHtoPCSid( PCS->p_csh );	/* Physical Cache Section ID */

  oldPri = splbio();		/* START: critical section: flush done bits */
  reqOK = gba_start_pcs_flush( GBAid, PCSid);

  /**************************************************************/
  /* We check that any additional pages to be flushed are in 	*/
  /* the same PCS.  If NOT, we wait for outstanding flush to	*/
  /* complete and then start a flush on the next PCS.  Note 	*/
  /* the same PCS could appear multiple times in the list and	*/
  /* subsequent flushes are unnecessary -- but we don't expect	*/
  /* this to occur frequently (we could call gba_lcs_flush) so	*/
  /* we don't bother to avoid the extra flush.			*/
  /**************************************************************/
  
  len = btoc( (((long) kaddr) & (NBPC-1))  +  len);  /* len is page count */
  
  while (reqOK && (--len) && ((SPRid = sregInfo->nextSreg) >= 0)) {
      
    PCS = getPCSinfo( GBAid, SPRid );
    ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
    ASSERT( PCS->cs_allocated );
    
    sregInfo = getSregInfo( GBAid, SPRid );
    ASSERT( sregInfo );
    ASSERT( sregInfo->sr_allocated );
    ASSERT( foundSAH ^=  sregInfo->sr_sah_head); 
    
    if (PCSid != CSHtoPCSid( PCS->p_csh )) {
      
      /* Wait for previous flush */
      
      reqOK = gba_wait_pcs_flush( GBAid, PCSid);
      splx( oldPri );		/* END: critical section: flush done bits */
      PCSid = CSHtoPCSid( PCS->p_csh );
      
      /* Start another flush */
      
      if (reqOK) {
	oldPri = splbio();	/* START: critical section: flush done bits */
	reqOK = gba_start_pcs_flush( GBAid, PCSid );
      }
    }
  }

  /**************************************************************/
  /* Finally we wait for the last outstanding flush to complete.*/
  /* Return FALSE if either the wait fails OR if the sarea dir	*/
  /* not contain the total byte count requested.		*/
  /**************************************************************/

  if (reqOK)
    reqOK = gba_wait_pcs_flush( GBAid, PCSid);
  splx( oldPri );		/* END: critical section: flush done bits */

  /*
   * reqOK == TRUE  =>  all flushes successful
   * SPRid < 0      =>  we hit end of system map register chain before we
   *                    finished flushing "len" pages.  This is OK iff we
   *                    have special case of (kaddr == len == 0) which
   *                    indicates a flush of the entire map area.
   */
  
  return( reqOK & ((kaddr == 0) || (SPRid >= 0)));
}

/****************************************************************************
 *
 * Flush and/or Invalidate entire VME I/O Map.
 *
 * This routine guarantees that input from a device or output to a device
 * for the cache section has been flushed into system memory.  This
 * routine will flush all sections linked to this csectHandle.  It is
 * intended to be called for scatter/gather requests so that the caller
 * does NOT need to invoke GBApageFlush on each individual page.
 *
 * This section must have been reserved by a call to gba_reserve_lcs.
 *
 * If there is an error, FALSE will be returned, else TRUE.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
vme_iomap_flush( csh )
     csh_type csh;	/* page map descriptor */


{
  register gba_id_t GBAid;
  register struct cs_info *PCS, *LCS;
  long oldPri;
  
  GBAid = CSHtoGBAid( csh );
  LCS = PCS = CSinfoPtr( csh );
  ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */

  /**********************************************************************/
  /* Initate flush of all Physical Cache Sections which compose the	*/
  /* Logical Cache Section.			       			*/
  /**********************************************************************/

  oldPri = splbio();	/* START: critical section: flush done bits */
  while (PCS) {
    ASSERT( PCS->cs_allocated );
    if (!gba_start_pcs_flush( GBAid, CSHtoPCSid(PCS->p_csh) ))
      return( FALSE );

    if (!gba_wait_pcs_flush( GBAid, CSHtoPCSid(PCS->p_csh) ))
      return( FALSE );
    PCS = PCS->nextCS;
  }
  
  splx( oldPri );	/* END: critical section: flush done */
  return( TRUE );
}


/****************************************************************************
 *
 * Map an area of kernel memory (k0,k1,k2) onto the VME I/O bus.
 *
 * Allocate unused system page map entries from the specified cache
 * section and then map the area of system memory onto the
 * I/O bus.  All attributes of the mapping are
 * determined from the attributes of the cache section.
 *
 * The smapArea returned from this call must be passed to
 * vme_iounmap, ka_to_vmeaddr, and vme_ioflush.
 *
 * To determine the I/O bus address of an address in the area, you must invoke
 * ka_to_vmeaddr with the desired kernel address and the smapArea descriptor
 * returned from this call.
 *
 * Special Flags:
 *	EBA_CONTIG_ADDR	I/O address space must be contiguous
 *	EBA_NOPART_MAP	No partial map allowed if insufficient registers
 *
 * Returns number of pages of memory mapped.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

int
vme_iomap( csh, kaddr, len, flags, smapArea, VMEaddress )

	csh_type csh;	/* section descriptor */
	kaddr_t  kaddr;			/* system address (k0,k1,k2) */
        int	   len;			/* length of area (bytes) */
	boolean  flags;		        /* flags (see above) */
	sah_type *smapArea;		/* map area descriptor (returned)*/
        ioaddr_t *VMEaddress;		/* VME address (returned) */

{
  gba_id_t GBAid;
  register struct cs_info *LCS;
  smap_reg_t sreg, firstSR;
  int pages, map_count;
  phys_page_num_t PPnum;
  register struct cs_info *PCS;
#ifdef	R6000_FAKEIO
  int fake_kaddr = kaddr;
  int fake_len = len;
#endif	R6000_FAKEIO  
  
  LCS = CSinfoPtr( csh );
  ASSERT( LCS );
  GBAid = CSHtoGBAid( csh );
  
  *smapArea = 0;
  pages = btoc( ((long)kaddr & (NBPC-1)) + len );

  /**********************************************************************/
  /* Attempt to find enough System Page map Registers in the Logical	*/
  /* Cache Section.  We look for contiguous SPRs if requried.		*/
  /**********************************************************************/

  if ((pages > 1) && (flags & EBA_CONTIG_ADDR))
    firstSR = gba_alloc_contig_sprs( LCS, pages );
  else
    firstSR = gba_alloc_sprs( LCS, pages );
  
  if (firstSR < 0)
    /* Allocation failed.  Optionally attempt a partial allocation. */
    
    if ((flags & EBA_NOPART_MAP) ||
	((firstSR = gba_alloc_sprs( LCS, pages=1 )) < 0))
      return(0);
  
  /**************************************************************/
  /* System Map Register have been allocated.  Now load the	*/
  /* system bus Physical Page Numbers into the registers.	*/
  /**************************************************************/

  sreg = firstSR;
  *smapArea = computeSPH( csh, sreg );
  map_count = 0;
  
  while ((map_count < pages) && (sreg >= 0) ) {
    
    /************************************************************/
    /* Determine correct Physical Page Number from kaddr	*/
    /************************************************************/
    
    if (IS_KSEG2(kaddr)) 
      PPnum=kvtokptbl(kaddr)->pgm.pg_pfn;	/* K2SEG */
    else
      if (IS_KUSEG(kaddr))
	PANIC( "KUSEG is invalid address!\n");
      else 
	PPnum=svtopfn(kaddr);		/* K0SEG or K1SEG */

    /************************************************************/
    /* Then load PPnum into the System Page map Register.	*/
    /************************************************************/
    
    if (!gba_load_spr( GBAid, sreg, PPnum))
      PANIC("Could not load SPR!\n");

    if (map_count == 0) {
      /*
       * Return VME address corresponding to kaddr.
       */
      PCS = getPCSinfo( GBAid, sreg );
      ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
      ASSERT( PCS->cs_allocated );
      *VMEaddress =
	PCS->gbaAddress + PCSoffset(sreg) + ((long)kaddr & (NBPC-1));
#ifdef R6000_SIM_IOC
      *VMEaddress = ctob( PPnum ) + ((long)kaddr & (NBPC-1));
#endif R6000_SIM_IOC
#ifdef	R6000_FAKEIO
      if (fakeio)
	*VMEaddress = fakeio_vmeaddr + (sreg)*NBPP_R6000 
	  + ((long)kaddr & (NBPC-1));
#endif	R6000_FAKEIO
    }
    
#ifdef	R6000_FAKEIO
    if (fakeio) {
      if (!(flags & (EBA_MAP_READ | EBA_MAP_WRITE)))
	flags |= EBA_MAP_READ | EBA_MAP_WRITE;
      getSregInfo( GBAid, sreg)->map_flags = flags;
    }
#endif	R6000_FAKEIO
    sreg = getSregInfo( GBAid, sreg )->nextSreg;	/* go to next sreg */
    map_count ++;
    kaddr += NBPC;		/* kaddr of next page */
  }

  ASSERT( map_count == pages );
  ASSERT( sreg == NIL_SPR );

#ifdef	R6000_FAKEIO
  if (fakeio)
    r6000_fakeio_xfer( *smapArea, fake_kaddr, fake_len, 1 );
#endif	R6000_FAKEIO
  return( map_count );
}

/****************************************************************************
 *
 * Release the system map area.
 *
 * This area must have been mapped with vme_iomap.
 *
 * This routine will flush the VME iomap before releasing it.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/


boolean
vme_iounmap( smapArea )
     sah_type smapArea;	/* map area descriptor */


{
  register struct cs_info	*PCS;
  register smap_reg_info *sregInfo;
  register smap_reg_t SPRid, nextSPRid;
  register long	oldPri;
  boolean tryShrink, foundSAH;

  vme_ioflush( smapArea, 0, 0 );	/* Make sure area has been flushed */
  foundSAH = tryShrink = FALSE;
  nextSPRid = SPHtoSPRid( smapArea );	/* System Page Reg id # */

  oldPri = splbio();		/* START: critical section on SP queue */

  do {
    SPRid = nextSPRid;
    PCS = getPCSinfo( SPHtoGBAid(smapArea), SPRid  );
				/* pointer to Physical CS info */
    ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
    ASSERT( PCS->cs_allocated );
    
    sregInfo = getSregInfo( SPHtoGBAid(smapArea),  SPRid );	/* ptr to Sreg info */
    ASSERT( sregInfo );
    ASSERT( sregInfo->sr_allocated );
    ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */

#if REALLY_UNMAP
    gba_load_spr( SPHtoGBAid(smapArea), SPRid, SPRid+0x20000);
          /* point past 1st 2 GB of physical memory to cause bus error */
#endif
#ifdef	R6000_FAKEIO
    if (fakeio)
      sregInfo->mem_kaddr = sregInfo->mem_len = 0;
#endif	R6000_FAKEIO    
    /*********************************************************************/
    /* Mark system map register as available, then place it at the head	*/
    /* of the Physical Cache Section's free list.			*/
    /********************************************************************/

    sregInfo->sr_allocated = FALSE;
    nextSPRid = sregInfo->nextSreg;
    sregInfo->nextSreg = PCS->freeMRlist;
    PCS->freeMRlist = SPRid;
    PCS->freeMRcnt++;
    tryShrink |= ((PCS->freeMRcnt >= MAX_SR_PER_CS) && PCS->cs_expandedCS);

  }
  while (nextSPRid >= 0);
  
  splx( oldPri );		/* END: critical section on SP queue */
  
  /**********************************************************************/
  /* Check if ALL of the system map registers in this Physical Cache 	*/
  /* section are available.  If so, and if this PCS was an		*/
  /* auto-expansion of the Logical Cache Section then attempt to	*/
  /* release the PCS to the free pool.					*/
  /**********************************************************************/

  if (tryShrink)
    gba_contract_lcs( SPHtoGBAid(smapArea), SPHtoLCSid(smapArea));

  return( TRUE );
}

/****************************************************************************
 *
 * For a system address (k0,k1,k2) return a controller usable VME bus
 * address.
 *
 * For a system address mapped into a system map area, return its'
 * I/O bus dma address.
 *
 * The "smapArea" information returned by GMAmapSarea
 * must be supplied in this call.
 *
 * If there is an error, FALSE will be returned, else TRUE.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
ka_to_vmeaddr( smapArea, kaddr, GBAaddress )

sah_type smapArea;	/* map descriptor */
kaddr_t  kaddr;		/* system address (k0,k1,k2) */
ioaddr_t *GBAaddress;	/* address on GBA bus (returned) */

{
  gba_id_t GBAid;
  smap_reg_t SPRid;
  register struct cs_info *PCS;
  register smap_reg_info *sregInfo;
  phys_page_num_t PPnum;
  boolean foundSAH;

  foundSAH = FALSE;
  ASSERT( smapArea );
  GBAid = SPHtoGBAid( smapArea );  /* For now, smapArea handle is SPH */
  SPRid = SPHtoSPRid( smapArea );	/* initial sreg in linked list */
  ASSERT( (GBAid >= 0) && (GBAid < gba_count) );

  /**************************************************************/
  /* First determine Physical Page Number of kernel address.	*/
  /**************************************************************/
  
  if (IS_KSEG2( kaddr ))
    PPnum = kvtokptbl(kaddr)->pgm.pg_pfn;  /* get PPnum from K2SEG address */
  else
    if (IS_KUSEG( kaddr ))
      PANIC( "KUSEG address is ILLEGAL!\n" );
  else
    PPnum = svtopfn( kaddr );	/* extract PPnum from K0seg or K1seg */

  /**************************************************************/
  /* Now look through chain of pages in smapArea and find PPnum	*/
  /**************************************************************/

  do {
    
    PCS = getPCSinfo( GBAid, SPRid );	/* get ptr to Phys Cache Sect info */
    ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
    ASSERT( PCS->cs_allocated );

    sregInfo = getSregInfo( GBAid, SPRid );	/* ptr to Sys Map Reg info */
    ASSERT( sregInfo );
    ASSERT( sregInfo->sr_allocated );
    ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */

    if (PPnum == sregInfo->mapEntry.bits.pp_num) {
      *GBAaddress =
	PCS->gbaAddress + PCSoffset(SPRid) + ((long)kaddr & (NBPC-1));
#ifdef R6000_SIM_IOC
      *GBAaddress = ctob( PPnum ) + ((long)kaddr & (NBPC-1));
#endif R6000_SIM_IOC
#ifdef	R6000_FAKEIO
      if (fakeio)
	*GBAaddress = fakeio_vmeaddr + (smapArea & 0x1ff)*NBPP_R6000 
	  + ((long)kaddr & (NBPC-1));
#endif	R6000_FAKEIO
      return( TRUE );
    }

    SPRid = sregInfo->nextSreg;	/* move to next sreg in sarea */
  }
  while (SPRid >= 0);

  *GBAaddress = 0;
  return( FALSE );
  
}

/***** START ************** GBA map register routines ********************/


/****************************************************************************
 *
 * Allocate a group of System Page Registers which will appear as
 * consecutive addresses on the GBA address bus.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

gmap_reg_t
gba_alloc_contig_gprs( gbaPtr, pages)

gba_sw_info *gbaPtr;
int pages;		/* number of consecutive pages required */

{
  gmap_reg_t greg, firstGR;
  register int needPages;
  long oldPri;
  
  if ((pages <= 0) || (pages > MAX_GMAP_REG))
    return( NIL_SPR );
  
  needPages = pages;
  oldPri = splbio();		/* START: critical section on GR queue */
  greg = 5;
  
  /************************************************************/
  /* Attempt to find "pages" consecutive unallocated GBA Map	*/
  /* Registers to satisfy caller's map request.		*/
  /************************************************************/
  
  while (needPages && (greg <= MAX_GMAP_REG-1)) {
    if (!FREE_GMAP_REG(gbaPtr,greg))
      needPages = pages;	/* Reset page count when hit allocated page */
    else
      needPages--;
    greg++;
  }
    
  /**********************************************************************/
  /*	needPages == 0  IFF allocation was successful			*/
  /*	greg = GR index of first GBA map Register AFTER free group	*/
  /**********************************************************************/

  if (needPages) {
    splx( oldPri );
#ifdef DEBUG_GMAP
    cmn_err(CE_WARN, "gba_alloc_contig_gprs failed!");
#endif DEBUG_GMAP    
    return( NIL_SPR );		/* return invalid (EOL) GR */
  }

  firstGR = greg - pages;	/* Compute index of 1st GR from last GR */
  
  splx( oldPri );		/* END: Critical Section on GR queue */
#ifdef DEBUG_GMAP
  cmn_err(CE_NOTE, "gba_alloc_contig_gprs: first (0x%x) npages (0x%x)",
	  firstGR, pages);
#endif DEBUG_GMAP
  return( firstGR );

}

/****************************************************************************
 *
 * Load VME address & Address Modifier into the GBA Page Map Register
 * in the GBA hardware.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
gba_load_gpr( GBAid, greg, PPnum, vmeAM )

gba_id_t GBAid;			/* GBAid of GBA containing greg */
gmap_reg_t greg;		/* GBA map register ID within GBA */
U32 PPnum;			/* VME bus page number */
U32 vmeAM;			/* VME bus Address Modifier */

{
  gba_sw_info *gbaPtr;		/* pointer to GBA information */
  volatile ioa_dev_type  *ioaPtr;

#ifdef DEBUG_GMAP
  cmn_err(CE_NOTE, "gba_load_gpr: gba 0x%x greg 0x%x ppnum 0x%x vmeAM 0x%x",
	  GBAid, greg, PPnum, vmeAM);
#endif DEBUG_GMAP
  
  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );
  ASSERT( (greg >=0) && (greg < MAX_GMAP_REG));
  if (vmeAM != 0) {
    ASSERT( FREE_GMAP_REG(gbaPtr, greg) );
  } else {
    ASSERT( !FREE_GMAP_REG(gbaPtr, greg) );	/* this is gba_unmap */
  }
  
  gbaPtr->gmap.gmapReg[greg].mapEntry.word = 0;	/* Zero entire mapEntry 1st */
  gbaPtr->gmap.gmapReg[greg].mapEntry.bits.gba_address = PPnum;
  gbaPtr->gmap.gmapReg[greg].mapEntry.bits.control =
    vmeAM << GMAP_VME_AM_SHIFT;
  
  /**********************************************/
  /* HW: Load System Page Map Register in IOA	*/
  /**********************************************/

  ioaPtr = gbaPtr->ioa;
  ASSERT( ioaPtr );

/***** Compiler can't handle the following structure, so use alternate
  ioaPtr->mem.gba_maps[ gbaPtr->gbaNum ][greg] =
    gbaPtr->gmap.gmapReg[greg].mapEntry;
***********/
  ioaPtr->word[ (gbaPtr->gbaNum * 1024) + greg ] =
    gbaPtr->gmap.gmapReg[greg].mapEntry.word;

  return( TRUE );
}


/****************************************************************************
 *
 * Map an area of the VME I/O bus onto the system bus.
 *
 * Allocate unused EBA page map entries
 * and then map the area of VME bus space onto the
 * system bus.
 *
 * The gmapArea returned from this call must be passed to
 * gba_unmap and vmeaddr_to_ka.
 *
 * Returns number of pages of memory mapped.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

int
eba_map( dev_ctlspace, VMEaddress, len, VMEam, gmapArea, kaddr )

     paddr_t	dev_ctlspace;	/* system address of device control space */
     ioaddr_t 	VMEaddress;	/* VME address  */
     int	len;		/* length of area (bytes) */
     U32	VMEam;		/* VME Address Modifier */
     U32	*gmapArea;	/* GBA map area descriptor (returned)*/
     kaddr_t	*kaddr;		/* system address (k1) */

{
  gba_id_t GBAid; 
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  gmap_reg_t greg, firstGR;
  int pages;
  phys_page_num_t PPnum;
  long oldPri;
  
  *kaddr = *gmapArea = 0;	/* Initialize to zero for early returns */
  if (!get_gba_id( dev_ctlspace, &GBAid )) {
    cmn_err(CE_WARN,  "eba_map: Can't find EBA for address 0x%x!\n",
	    dev_ctlspace);
    return(0);
  }

  gbaPtr = GET_GBA_PTR( GBAid );
  ASSERT( gbaPtr );		/* Make sure we have valid GBA pointer */

  pages = btoc( ((long)VMEaddress & (NBPC-1)) + len );

  /* Special case for permanently mapped address range.  All of the A16
   * address space & the last page of A24 address space are permanently
   * mapped for SA access.  Use preset descriptors for attempted mappings
   * in this region.
   */
  if (((VMEam == VME_A16SAMOD) && ((VMEaddress & 0x00ffffff) < 0x0000ffff))
      ||
      ((VMEam == VME_A24SAMOD) && ((VMEaddress & 0x00ffffff) > 0x00ffc000)))
    {
      *gmapArea = computeGRH( GBAid, 0, 3 );  /*  "permanent" descriptor */
      *kaddr = gbaPtr->gba_map_low + ((long)VMEaddress & 0x00ffffff);
      return( pages );
    }

  /**********************************************************************
   * Attempt to find enough GBA Page map Registers
   **********************************************************************/

  oldPri = splbio();
  firstGR = gba_alloc_contig_gprs( gbaPtr, pages );
  
  if (firstGR < 0) {
    splx(oldPri);
    return(0);		/* Allocation failed */
  }
  
  /************************************************************/
  /* Determine correct Physical Page Number from VME addr	*/
  /************************************************************/

  *gmapArea = computeGRH( GBAid, firstGR, firstGR+pages-1 );
  *kaddr =
    gbaPtr->gba_map_low + (firstGR * NBPP) + ((long)VMEaddress & (NBPC-1));
  PPnum = VMEaddress / NBPP;

  for (greg = firstGR; greg < firstGR+pages; greg++, PPnum++)
    if (!gba_load_gpr( GBAid, greg, PPnum, VMEam))
      PANIC("Could not load GPR!\n");

  splx(oldPri);
#ifdef DEBUG_GMAP
  cmn_err(CE_NOTE,
    "gba_map: devctl 0x%x VMEadr 0x%x len 0x%x AM 0x%x GRH 0x%x kaddr 0x%x",
	  dev_ctlspace, VMEaddress, len, VMEam, *gmapArea, *kaddr );
#endif DEBUG_GMAP  
  return( pages );
}

/****************************************************************************
 *
 * Release the EBA map area.
 *
 * This area must have been mapped with gba_map.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/


boolean
eba_unmap( gmapArea )
     U32 gmapArea;	/* map area descriptor */

{
  gba_id_t GBAid;
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  register gmap_reg_t GPRid;
  register long	oldPri;

#ifdef DEBUG_GMAP
  cmn_err(CE_NOTE, "gba_unmap: GRH 0x%x", gmapArea );
#endif DEBUG_GMAP
  
  ASSERT( gmapArea );
  GBAid = GRHtoGBAid( gmapArea );  /* For now, smapArea handle is SPH */
  gbaPtr = GET_GBA_PTR( GBAid );

  /* If returning permanently mapped region, then do nothing */

  if (GRHtoFirstGR(gmapArea) == 0)
    return(TRUE);
  
  oldPri = splbio();		/* START: critical section on SP queue */

  for (GPRid= GRHtoFirstGR(gmapArea);GPRid <= GRHtoLastGR(gmapArea); GPRid++) {
    ASSERT( !FREE_GMAP_REG( gbaPtr, GPRid ));
    gba_load_gpr( GRHtoGBAid(gmapArea), GPRid, 0, 0);
  }
  
  splx( oldPri );		/* END: critical section on SP queue */
  
  return( TRUE );
}

/****************************************************************************
 *
 * For a VME bus address return a system bus address.
 *
 * For a system address mapped into a system map area, return its'
 * I/O bus dma address.
 *
 * The "smapArea" information returned by GMAmapSarea
 * must be supplied in this call.
 *
 * If there is an error, FALSE will be returned, else TRUE.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
vmeaddr_to_ka( gmapArea, VMEaddress, kaddr )

U32	 gmapArea;	/* map descriptor */
ioaddr_t VMEaddress;	/* address on GBA bus) */
kaddr_t  *kaddr;	/* system address (k1) (returned) */

{
  gba_id_t GBAid;
  gba_sw_info  *gbaPtr;		/* pointer to GBA info area */
  gmap_reg_t firstGR, lastGR;
  ioaddr_t firstVMEaddr,lastVMEaddr;

  ASSERT( gmapArea );
  GBAid = GRHtoGBAid( gmapArea );  /* For now, smapArea handle is SPH */
  gbaPtr = GET_GBA_PTR( GBAid );

  firstGR = GRHtoFirstGR(gmapArea);
  lastGR = GRHtoLastGR(gmapArea);
  ASSERT( (GBAid >= 0) && (GBAid < gba_count) );
  ASSERT( !FREE_GMAP_REG( gbaPtr, firstGR ));
  ASSERT( !FREE_GMAP_REG( gbaPtr, lastGR ));

  firstVMEaddr =
    gbaPtr->gmap.gmapReg[firstGR].mapEntry.bits.gba_address * NBPP;
  lastVMEaddr =
    gbaPtr->gmap.gmapReg[lastGR].mapEntry.bits.gba_address * NBPP + NBPP;
  if ((VMEaddress < firstVMEaddr) || (VMEaddress >= lastVMEaddr))
    return(FALSE);

  *kaddr =
    gbaPtr->gba_map_low + (firstGR * NBPP) + (VMEaddress - firstVMEaddr);
  
#ifdef DEBUG_GMAP
  cmn_err(CE_NOTE,  "vmeaddr_to_ka: GRH 0x%x VMEadr 0x%x kaddr 0x%x",
	  gmapArea, VMEaddress, *kaddr );
#endif DEBUG_GMAP  
  return( TRUE );
  
}
/***** END **************** GBA map register routines ********************/

#else

#ifndef STANDALONE 
/*
 * These routines are stubs to allow proper execution of device drivers
 * on non-6000 based machines.
 */

gba_sw_init()
{
return( TRUE );
}

vme_reserve_iomap( dev_no, dev_ctlspace, minPages, csh, userGBAflags )

dev_t		dev_no;		/* device which needs cache section */
paddr_t		dev_ctlspace;	/* system address of device control space */
int		minPages;	/* min number of pages to reserve */
csh_type	*csh;		/* section descriptor -- returned */
int		userGBAflags;	/* cache section options */
{
*csh = 1;		/* Arbitrary non-zero value to keep caller happy */  
return( TRUE );
}

vme_ioflush()
{
return( TRUE );
}

vme_iomap_flush()
{
return( TRUE );
}

vme_iomap( csh, kaddr, len, flags, smapArea, VMEaddress)

	csh_type csh;	/* section descriptor */
	kaddr_t  kaddr;			/* system address (k0,k1,k2) */
        int	   len;			/* length of area (bytes) */
	boolean  flags;		        /* flags (see above) */
	sah_type *smapArea;		/* map area descriptor (returned)*/
     	ioaddr_t *VMEaddress;		/* VME address (returned) */
{
*smapArea = 2;		/* Arbitrary non-zero value to keep caller happy */
return(ka_to_vmeaddr(smapArea, kaddr, VMEaddress));
}

vme_iounmap()
{
return( TRUE );
}

vme_release_iomap()
{
return( TRUE );
}

/****************************************************************************
 *
 * For a system address (k0,k1,k2) return a controller usable GBA address.
 *
 * For a system address mapped into a system map area, return its'
 * I/O bus dma address.
 *
 * The "smapArea" information returned by GMAmapSarea
 * must be supplied in this call.
 *
 * If there is an error, FALSE will be returned, else TRUE.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
ka_to_vmeaddr( smapArea, kaddr, GBAaddress )

sah_type smapArea;	/* map descriptor */
kaddr_t  kaddr;		/* system address (k0,k1,k2) */
ioaddr_t *GBAaddress;	/* address on GBA bus (returned) */

{
  phys_page_num_t PPnum;

  /**************************************************************/
  /* First determine Physical Page Number of kernel address.	*/
  /**************************************************************/
  
  if (IS_KSEG2( kaddr ))
    PPnum = kvtokptbl(kaddr)->pgm.pg_pfn;  /* get PPnum from K2SEG address */
  else
    if (IS_KUSEG( kaddr ))
      PANIC( "KUSEG address is ILLEGAL!\n" );
  else
    PPnum = svtopfn( kaddr );	/* extract PPnum from K0seg or K1seg */

  *GBAaddress = ctob( PPnum ) + ((long)kaddr & (NBPC-1));
#ifdef	R6000_FAKEIO
  if (fakeio)
    *GBAaddress = fakeio_vmeaddr;
#endif	R6000_FAKEIO
  return( TRUE );
  
}

#else

/*
 * These routines are stubs to allow proper execution of device drivers
 * in a standalone system.
 */

#ifdef	R6000_FAKEIO
#define	MAX_FAKEIO_MAP	256		/* 256 pages mapped = 4 MB */
#endif	R6000_FAKEIO

typedef struct gba_hw_info {
  ioa_dev_type  	*ioa;		/* k1seg pointer to IOA ctl space */
  paddr_t		gba_map_low;	/* lowest address in GBA map */
  paddr_t		gba_map_high;	/* highest address in GBA map */
  unsigned char	      	gbaNum;		/* number (0/1) of GBA in IOA */
#ifdef R6000_FAKEIO
  char			mem_alloc[MAX_FAKEIO_MAP];
  unsigned	long	mem_kaddr[MAX_FAKEIO_MAP];
  unsigned	long	mem_len[MAX_FAKEIO_MAP];
#endif R6000_FAKEIO  
} gba_hw_info;

static struct gba_hw_info gba_hw[MAX_GBA];
static int gba_count;		/* relies on BSS being 0'ed */


/****************************************************************************
 *
 * Initialize GBA software data structures.
 *
 * This routine should be invoked once per GBA at system boot time before
 * invoking any of the other routines in this module.  Should be invoked
 * AFTER the IOA/GBA hardware has been initialized since this routine reads
 * some values from the hardware in order to complete the setup (i.e., it
 * reads the VME address compare registers).
 *
 * This routine sets up a small data structure which contains information
 * on each of the hardware GBAs installed in the system.  This information
 * is utilized by the routines: iobadaddr and get_gba_id.
 *
 * EXTERNAL ROUTINE called by system initialization routines before
 * device driver initialization. NOT CALLED BY DEVICE DRIVERS !
 ***************************************************************************/

gba_sw_init( ioa_ctlspace_addr, gba_num )
     paddr_t	ioa_ctlspace_addr;	/* address of IOA on system bus */
     int	gba_num;		/* GBA number within IOA (0 or 1) */

{
  int GBAid;
  unsigned long ioa_base_addr;

#ifdef	R6000_FAKEIO  
  short	*cmc_csr = (short *)0xbcde1002;
  char	*cmc_reset = (char *)0xbcdfffff;
  int	i;
  
  if (!IS_R6300) {
    fakeio_vmeaddr = 0;
    fakeio_sysaddr = 0;
    return( TRUE );
  }

  if (fakeio_cmc)  {
    printf( "START reset of CMC\n");
    *cmc_csr = 0;
    DELAY(1000);
    *cmc_reset = 0x01;
    i = 0;
    while ((i<20) && !(*cmc_csr & 0x0004)) {
      DELAY(1000000);
      i++;
    }
    if (*cmc_csr & 0x0004) 
      printf("RESET complete in %d secs!\n", i);
    else
      printf( "RESET failed!\n");
  }
  else {
    fakeio_vmeaddr = MEM_VMEADDR;
    fakeio_sysaddr = MEM_SYSADDR;
    fakeio_copy16 = 0;
    delay_fakeio = 0;
#if	defined(PROM)
    fakeio_verify = 0;
#endif    
  }
#endif	R6000_FAKEIO
  
  if (!IS_R6300)
    return( TRUE );
  
  /* First we see if this GBA already has an entry */
  
  GBAid = 0;
  while ((GBAid < gba_count)  &&
	 ((gba_hw[GBAid].gbaNum != gba_num)  ||
	  (gba_hw[GBAid].ioa != (ioa_dev_type *) ioa_ctlspace_addr)))
    GBAid++;
  
  /* If not, allocate the next entry in the table */
  
  if (GBAid == gba_count)
    gba_count++;		/* Count of number of GBAs initialized */

  if (gba_count > MAX_GBA)  {
    printf( "ERROR: Trying to initialize more than max permissible GBAs!\n" );
    gba_count--;
    return(FALSE);
  }

  gba_hw[GBAid].gbaNum = gba_num;	/* Number within IOA (0 or 1) */

  /* Setup pointers to hardware.  IOA has control space addresses on 	*/
  /* system bus (16 KB) and data space addresses on system bus (16 MB).	*/
  /* Control space is used to setup map registers and control GBA ops,	*/
  /* such as flushing GBA cache.  Data space is used to access devices	*/
  /* on the GBA bus.							*/

  gba_hw[GBAid].ioa = (ioa_dev_type *) ioa_ctlspace_addr;
  
  /* Get ioa_base_addr as a K1 address  */
  ioa_base_addr =  PHYS_TO_K1( 0x100000 * 
    decode_brd_address( gba_hw[GBAid].ioa->mem.sbc_board_addr ));

  if( (ioa_base_addr != IOA1) && (ioa_base_addr != IOA2) &&
	 (ioa_base_addr != IOA3))
    printf("ERROR: Illegal ioa_base_addr!\n");
  
  gba_hw[GBAid].gba_map_low =  ioa_base_addr + (gba_num * GBA_SPAN);
  gba_hw[GBAid].gba_map_high = gba_hw[GBAid].gba_map_low + (GBA_SPAN - 1);
#ifdef	R6000_FAKEIO
  for (i=0; i<MAX_FAKEIO_MAP; i++) gba_hw[GBAid].mem_alloc[i] = 0;
  for (i=0; i<MAX_FAKEIO_MAP; i++) gba_hw[GBAid].mem_kaddr[i] = 0;
  for (i=0; i<MAX_FAKEIO_MAP; i++) gba_hw[GBAid].mem_len[i] = 0;
#endif	R6000_FAKEIO  

return( TRUE );
}

/****************************************************************************
 * Given a control space address of an I/O controller on the system bus,
 * determine the GBAid of the GBA handling that controller.
 *
 * This routine assumes that the GBA map has been properly setup for each
 * GBA in the system.  Returns FALSE if address was not within space of
 * any known GBA.
 *
 * INTERNAL ROUTINE ONLY !
 ***************************************************************************/

boolean
get_gba_id( dev_addr, gba_id )

paddr_t	dev_addr;	/* address of controller on system bus */
int	*gba_id;	/* GBA id number (0..5) */

{
  int i;

  for (i=0; i < gba_count; i++ )   {
    if ((dev_addr >= gba_hw[i].gba_map_low) &&
	(dev_addr <= gba_hw[i].gba_map_high))
      {
	*gba_id = i;
	return( TRUE );
      }
  }
  
  *gba_id = MAX_GBA;		/* This will index end-of-list element */
  return( FALSE );
}

/*
 * In standalone mode, we don't really need to reserve a cache section.
 * The mappings are static and already setup.  We return the "gba_id"
 * as the map descriptor so we know which GBA is accessed for this
 * controller.
 */

vme_reserve_iomap( dev_no, dev_ctlspace, minPages, csh, userGBAflags )

dev_t		dev_no;		/* device which needs cache section */
paddr_t		dev_ctlspace;	/* system address of device control space */
int		minPages;	/* min number of pages to reserve */
csh_type	*csh;		/* section descriptor -- returned */
int		userGBAflags;	/* cache section options */
{
  if (!IS_R6300) {
    *csh = 1;		/* Arbitrary non-zero value to keep caller happy */  
    return( TRUE );
  }

  return( get_gba_id( dev_ctlspace, csh ));
}


vme_release_iomap()
{
return( TRUE );
}

/****************************************************************************
 *
 * Flush and/or Invalidate GBA cache section.
 *
 * This routine guarantees that input from a device or output to a device
 * for the cache section has been flushed into system memory.  This
 * routine will flush the indicated kernel address area from this smapArea
 * in the GBA cache.  Special case of (kaddr == len == 0) is used to signify
 * a flush of the entire specified system map area.
 *
 * This section must have been allocated by a call to vme_iomap.
 *
 * If there is an error, FALSE will be returned, else TRUE.  An ERROR is
 * signalled if a flush attempt fails OR if the addres range from kaddr to
 * (kaddr + len -1) is NOT within the map area.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
vme_ioflush( smapArea, kaddr, len )

sah_type smapArea;		/* map area descriptor */
kaddr_t  kaddr;			/* system address (k0,k1,k2) */
long len;			/* length of area (bytes) */

{
  gba_id_t GBAid;
  smap_reg_t SPRid;
  pcs_id_t PCSid, lastPCSid;
  ioaddr_t GBAaddress;
  int GBAnum;
  int i,j;
  
  volatile ioa_dev_type  *ioaPtr;

  if (!IS_R6300)
    return (TRUE);
#ifdef	R6000_FAKEIO
  if (fakeio) {
    r6000_fakeio_xfer( smapArea, kaddr, len, 0 );
    return(TRUE);	/* Don't bother with GBA cache flush */
  }
#endif	R6000_FAKEIO  
  /* Check for mode which is supposed to flush the entire Logical Cache
   * Section.  Since we don't keep track of the LCS in standalone mode,
   * we will flush the entire GBA cache.
   */

  if ((kaddr == 0) && (len == 0))  {
    /* Caller defaulted flush params, so extract them from smapArea */
    kaddr = (NBPP_R6000*MAX_SR_PER_CS) * ((smapArea & 0x000003e0)>>5);
    if (smapArea & 0x1f) {
      /* More than one CS, so compute total number of pages involved */
      len = MAX_SR_PER_CS * ((smapArea & 0x0000001f) + 1);
  			/* len in R6000 pages */
    }
    else {
      /* Only one CS.  This should be 32 pages, but one page gets whole CS */
      len = 1;
    }
  }
  else
    len = (((kaddr & (NBPP_R6000-1)) + len)+NBPP_R6000-1)/NBPP_R6000;
  		/* len in R6000 pages */
  
  GBAid = smapArea/1024;	/* smapArea is same as GBAid */

  ioaPtr = gba_hw[GBAid].ioa;	/* Point to ioa HW */
  GBAnum = gba_hw[GBAid].gbaNum;
  lastPCSid = -1;

  while (len > 0) {
    SPRid = K1_TO_PHYS(kaddr) / NBPP_R6000;
    PCSid = SPRid/32;		/* Get Physical Cache Section index */

    if (PCSid == lastPCSid) {
      kaddr += NBPP_R6000;
      len--;
      continue;
    }

    lastPCSid = PCSid;

    /*
     * The following code performs a GBA cache flush and waits for it
     * to complete.  It may be necessary to retry due to bugs in the GBA
     * hardware which sometimes cause the cache flush completion to go
     * unreported OR be reported on the wrong cache section.
     */

  retry:
    
    /* Check that "flush done" is not already set */

    if (ioaPtr->mem.gba_ints[GBAnum].long_op[GBA_FLUSH_OP].wr_reg
	&  (1<<PCSid))
      printf (" ERROR: Flush complete set when initiating a flush! ");
    
    /* Start flush of GBA */
    
    ioaPtr->mem.gba_misc_ctl[GBAnum].flush_reg = kaddr;  /* uses lower bits */

    /* Wait for flush to complete */

    reset_ioc_retry_count();	/* Reset DBE count, happens during flush */
    j = 0;
    while (!(i = ioaPtr->mem.gba_ints[GBAnum].long_op[GBA_FLUSH_OP].wr_reg)
	   & (j++ < 1000))   /* Nothing -- just wait */;
    
    /* Handle timeout or wrong cache section complete */
    
    if ( (j >= 1000) || !(i & (1<<PCSid))) {
      if (j >= 1000) {
	printf(" GBA flush op did not complete, cache section: 0x%x ", PCSid);
      }
      else {
	printf(	" WRONG GBA flush op done, flush op: 0x%x, expected: 0x%x ",
		i, 1<<PCSid);
	ioaPtr->mem.gba_ints[ GBAnum ].long_op[ GBA_FLUSH_OP ].and_reg =
	  ~(i);	/* Reset any set bits */
      }
      goto retry;
    }
    
    /*
     * GBA cache section has been flushed. Reset the flush done bit.
     */
    
    ioaPtr->mem.gba_ints[ GBAnum ].long_op[ GBA_FLUSH_OP ].and_reg = 
      ~(1<<PCSid);	/* turn off the cache section flush done bit */

    kaddr += NBPP_R6000;
    len--;
  }
  return(TRUE);
}


vme_iomap_flush()
{
return( TRUE );
}

/*
 * In standalone system it is unnecessary to dynamically map
 * since a static map has already been setup.  The first half of A24 space
 * (0x0 thru 0x007fffff) is directly mapped to physical addresses
 * (0x0 thru 0x007fffff).  I/O addresses (0xFF800000 thru 0xFFFFFFFF) are
 * mapped to physical addresses (0x00800000 thru 0x00ffffff).
 *
 * We simply return the GBAid to the caller so that drivers so that a
 * subsequent call to "vme_ioflush" will know which GBA to flush.
 */

vme_iomap( csh, kaddr, len, flags, smapArea, VMEaddress )

	csh_type csh;	/* section descriptor */
	kaddr_t  kaddr;			/* system address (k0,k1,k2) */
        int	   len;			/* length of area (bytes) */
	boolean  flags;		        /* flags (see above) */
        ioaddr_t *VMEaddress;		/* VME address (returned) */
	sah_type *smapArea;		/* map area descriptor (returned)*/
{
  int	ppaddr;
#ifdef	R6000_FAKEIO
  int	GBAid, mem_page, mem_len;
#endif	R6000_FAKEIO
  
  if( K1_TO_PHYS(kaddr) < GBA_SPAN) {
    ppaddr = K1_TO_PHYS(kaddr);
    *smapArea =
      /* GBAid */ 1024*csh +  /* CS # */ 32*(ppaddr/(NBPP_R6000*MAX_SR_PER_CS))
	+   /* # of CS */    (((ppaddr & (NBPP_R6000-1)) + len)+NBPP_R6000-1)
				  /(NBPP_R6000*MAX_SR_PER_CS);
    /* Encode GBAid, Cache Section No. (0-31), & No. Cache Sections */
#ifdef	R6000_FAKEIO
    if (fakeio) {
      GBAid = csh;
      mem_len = (((kaddr & (NBPP_R6000-1)) + len)+NBPP_R6000-1)/NBPP_R6000;
  			/* len in R6000 pages */
      if (len > NBPP_R6000)
	while (TRUE) printf("Can't handle I/O map length > 1 page!\n");

      *smapArea = -1;
      for (mem_page = 0; (mem_page < MAX_FAKEIO_MAP) && (*smapArea == -1); ) {
	if (gba_hw[GBAid].mem_alloc[mem_page] == 0) {
	  gba_hw[GBAid].mem_alloc[mem_page] = 1;
	  gba_hw[GBAid].mem_kaddr[mem_page] =0;
	  gba_hw[GBAid].mem_len[mem_page] =0;
	  *smapArea = 1024*csh + mem_page;
	}
	else
	  mem_page++;
      }
      if (mem_page >=  MAX_FAKEIO_MAP)
	while (TRUE) printf("Ran out of VME memory pages for I/O!\n");
      r6000_fakeio_xfer( *smapArea, kaddr, len, 1 );
    }
#endif	R6000_FAKEIO  
    return(ka_to_vmeaddr(smapArea, kaddr, VMEaddress));
  }
  *smapArea = 1024*MAX_GBA;
  return( FALSE );
}

/****************************************************************************
 * This procedure simply needs to make certain that the VME I/O cache is
 * flushed.
 ***************************************************************************/

vme_iounmap( smapArea )

sah_type smapArea;	/* map area descriptor */
{
#ifdef	R6000_FAKEIO
  int	GBAid, mem_page;
  GBAid = smapArea / 1024;
  mem_page = smapArea - (1024 * GBAid);
  gba_hw[GBAid].mem_alloc[mem_page] = 0;
#endif	R6000_FAKEIO
  
  return(vme_ioflush( smapArea, 0, 0 ));
}

/****************************************************************************
 *
 * For a system address (k0,k1,k2) return a controller usable GBA address.
 *
 * For a system address mapped into a system map area, return its'
 * I/O bus dma address.
 *
 * The "smapArea" information returned by GMAmapSarea
 * must be supplied in this call.
 *
 * If there is an error, FALSE will be returned, else TRUE.
 *
 * EXTERNAL ROUTINE callable by device drivers.
 ***************************************************************************/

boolean
ka_to_vmeaddr( smapArea, kaddr, GBAaddress )

sah_type smapArea;	/* map descriptor */
kaddr_t  kaddr;		/* system address (k0,k1,k2) */
ioaddr_t *GBAaddress;	/* address on GBA bus (returned) */

{
  
  kaddr_t PA;

  PA = K1_TO_PHYS( kaddr );
  
  if (PA < GBA_SPAN) {
    if (IS_R6300) {
      if (PA < (GBA_SPAN/2))
	*GBAaddress = PA;		/* Lower 8 MB is in A24 space */
      else
	*GBAaddress = PA + 0xFF000000;	/* Upper 8 MB in high end of A32 */
    }
    else {
      *GBAaddress = PA;		/* Non-6000 machines map directly */
    }
  }
  else {		/* Mapping above 16 MB */
    if (IS_R6300) {
      printf("Physical Address: 0x%x   is outside 16MB map range!\n", kaddr);
      *GBAaddress = 0xF0000000;	/* invalid address */
      return( FALSE );
    }
    *GBAaddress = PA;		/* Non-6000 machines map directly */
  }
#ifdef	R6000_FAKEIO
  if (fakeio) {
    if (!IS_R6300) return(TRUE);
    *GBAaddress = fakeio_vmeaddr + (smapArea & 0x1ff)*NBPP_R6000;
  }
#endif	R6000_FAKEIO
  return(TRUE);
  
}
#endif STANDALONE
#endif R6000_KERNEL

#ifdef	R6000_FAKEIO

/****************************************************************************
 * in_out = 0 (from controller to memory) OR 1 (from memory to controller)
 ***************************************************************************/

r6000_fakeio_xfer( smapArea, kaddr, len, in_out )

sah_type smapArea;	/* map descriptor */
kaddr_t  kaddr;		/* system address (k0,k1,k2) */
int len;

{
  int badxfer, iostartaddr;
  gba_id_t GBAid;
  int	mem_page;
#ifndef STANDALONE
  smap_reg_t SPRid;
  pcs_id_t PCSid;
  register struct cs_info *PCS;
  register smap_reg_info *sregInfo, *nextSregInfo;
  phys_page_num_t PPnum;
  boolean reqOK, foundSAH;
  long oldPri;
  int prevPPnum, pageCount;
  char *buf1, *buf2;
  int i;

  if (!fakeio) return(FALSE);

  GBAid = smapArea / 1024;
  mem_page = 0x1ff & smapArea;
  iostartaddr = fakeio_sysaddr + mem_page*NBPP_R6000;
  
  foundSAH = FALSE;
  ASSERT( smapArea );
  GBAid = SPHtoGBAid( smapArea );  /* For now, smapArea handle is SPH */
  SPRid = SPHtoSPRid( smapArea );	/* initial sreg in linked list */
  if ((kaddr == 0) && (len == 0)) {
    /**************************************************************
     * Special (common) case to flush entire map area
     **************************************************************/
      PCS = getPCSinfo( GBAid, SPRid ); /* get ptr to Phys Cache Sect info */
      ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
      ASSERT( PCS->cs_allocated );
      
      sregInfo = getSregInfo( GBAid, SPRid );	/* ptr to Sys Map Reg info */
      ASSERT( sregInfo );
      ASSERT( sregInfo->sr_allocated );
      ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */
  }
  else {
    /**************************************************************
     * First determine Physical Page Number of kernel address. 
     **************************************************************/
    
    if (IS_KSEG2( kaddr ))
      PPnum = kvtokptbl(kaddr)->pgm.pg_pfn;  /* PPnum from K2SEG address */
    else
      if (IS_KUSEG( kaddr ))
	return(FALSE);		/* KUSEG address is ILLEGAL */
      else
	PPnum = svtopfn( kaddr );	/* extract PPnum from K0seg or K1seg */
    
    /**************************************************************
     * Now look through chain of pages in smapArea and find PPnum
     **************************************************************/
    do {
      PCS = getPCSinfo( GBAid, SPRid ); /* get ptr to Phys Cache Sect info */
      ASSERT( !IS_KUSEG(PCS) );		/* Catch illegal addresses early */
      ASSERT( PCS->cs_allocated );
      
      sregInfo = getSregInfo( GBAid, SPRid );	/* ptr to Sys Map Reg info */
      ASSERT( sregInfo );
      ASSERT( sregInfo->sr_allocated );
      ASSERT( foundSAH ^=  sregInfo->sr_sah_head);  /* SAH at front ONLY */
      
      if (!(reqOK =  (PPnum == sregInfo->mapEntry.bits.pp_num)))
	SPRid = sregInfo->nextSreg;	/* move to next sreg in sarea */
    }
    while ((!reqOK) && (SPRid >= 0));
    
    if (!reqOK)
      return( FALSE );		/* start address NOT in area */
  }

  /**************************************************************/
  /* If we get here, we have FOUND the starting page within the	*/
  /* area for the flush.  Flush all necessary PCS.		*/
  /**************************************************************/
  
  PCSid = CSHtoPCSid( PCS->p_csh );	/* Physical Cache Section ID */

  oldPri = splbio();		/* START: critical section: flush done bits */
  if (delay_fakeio)
    DELAY(100000);	/* DEBUG HW -- AVOID TRASHED DATA ON COPY */
  if ((kaddr==0) && (len==0))
    if (in_out)
      cmn_err(CE_WARN, "ERROR Xfer out does NOT expect kaddr==len==0!\n");
    else {
	kaddr = sregInfo->mem_kaddr; /* Just use same kaddr as last xfer out */
	len = sregInfo->mem_len;	/* and same len as last xfer out */
      }
  if (kaddr==0) cmn_err(CE_WARN, "ERROR fakeio xfer has zero kaddr!\n");
  if (len==0) cmn_err(CE_WARN, "ERROR fakeio xfer has zero len!\n");
  if ((kaddr != 0) && (len !=0)) {
    if (btoct(kaddr) != btoct(kaddr+len-1)) {
      pageCount = btoc(kaddr+len-1) - btoc(kaddr);
      if (pageCount > 1)
	cmn_err(CE_CONT,
		"FYI fakeio xfer CROSSES %d physical page boundary(s)!\n",
		btoc(kaddr+len-1) - btoc(kaddr));
      nextSregInfo  = sregInfo;
      prevPPnum = sregInfo->mapEntry.bits.pp_num;
      while (nextSregInfo->nextSreg >= 0) {
	nextSregInfo = getSregInfo( GBAid, nextSregInfo->nextSreg );
	if (nextSregInfo->mapEntry.bits.pp_num == ++prevPPnum)
	  pageCount--;		/* Keep track of CONTIGUOUS pages */
	else
	  cmn_err(CE_WARN, "ERROR fakeio found DISCONTIGUOUS physical page\n");
	
      }
      if (pageCount)
	  cmn_err(CE_WARN, "Strange page boundary in FAKEIO %d\n", pageCount);
    }
      
    if (in_out) {
      sregInfo->mem_kaddr = kaddr;
      sregInfo->mem_len = len;
      if (fakeio_copy16)
	hwcpout(PHYS_TO_K1
		(ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
	    iostartaddr,
	    len);
      else {
	if ((sregInfo->map_flags & GBA_MAP_READ) || (fakeio_dumb & 0x01)) do {
	  bcopy(PHYS_TO_K1
		(ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
		iostartaddr + (kaddr&(NBPP-1)),
		len);
	  if (fakeio_verify) 
	    badxfer = bcmp(PHYS_TO_K1
			   (ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
			   iostartaddr+(kaddr&(NBPP-1)),
			   len);
	  else
	    badxfer=0;
	  if (badxfer) {
	    cmn_err(CE_WARN, "BAD copy to VME memory, try again");
	    buf1=(char *)PHYS_TO_K1(ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1));
	    buf2 = (char *)iostartaddr+(kaddr&(NBPP-1));
	    for (i=0; i < len; i++)
	      if (buf1[i] != buf2[i])
		cmn_err(CE_CONT,"  pmem(0x%x): 0x%x   VMEmem(0x%x): 0x%x\n",
		       &buf1[i], buf1[i], &buf2[i], buf2[i]);
	  }
	}
	while (badxfer);
      }
    } else {
      if (fakeio_copy16)
	hwcpin(iostartaddr,
	       PHYS_TO_K1
	       (ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
	       len );
      else {
	if ((sregInfo->map_flags & GBA_MAP_WRITE) || (fakeio_dumb & 0x02)) do {
	  bcopy(iostartaddr+(kaddr&(NBPP-1)),
		PHYS_TO_K1
		(ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
		len );
	  if (fakeio_verify)
	    badxfer = bcmp(iostartaddr+(kaddr&(NBPP-1)),
			   PHYS_TO_K1
			   (ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1)),
			   len );
	  else
	    badxfer = 0;
	  if (badxfer) {
	    cmn_err(CE_WARN, "BAD copy from VME memory, try again");
	    buf1=(char *)PHYS_TO_K1(ctob(sregInfo->mapEntry.bits.pp_num))+(kaddr&(NBPP-1));
	    buf2 = (char *)iostartaddr+(kaddr&(NBPP-1));
	    for (i=0; i < len; i++)
	      if (buf1[i] != buf2[i])
		cmn_err(CE_CONT, "  pmem(0x%x): 0x%x   VMEmem(0x%x): 0x%x\n",
		       &buf1[i], buf1[i], &buf2[i], buf2[i]);
	  }
	}
	while (badxfer);
      }
    }
    splx( oldPri );		/* END: critical section: flush done bits */
    return(TRUE);
  }

  cmn_err(CE_PANIC, "FATAL ERROR IN FAKEIO -- Shouldn't get here!\n");
  splx( oldPri );		/* END: critical section: flush done bits */
#else

  if (!fakeio) return(FALSE);
  if (!IS_R6300) return(FALSE);

  GBAid = smapArea / 1024;
  mem_page = 0x1ff & smapArea;
  iostartaddr = fakeio_sysaddr + mem_page*NBPP_R6000;
  if ((kaddr==0) && (len==0))
    if (in_out)
      printf( "ERROR Xfer out does NOT expect kaddr==len==0!\n");
    else {
      kaddr = gba_hw[GBAid].mem_kaddr[mem_page];
		/* Just use same kaddr as last xfer out */
      len = gba_hw[GBAid].mem_len[mem_page];
		/* and same len as last xfer out */
    }
  if ((kaddr==0) || (len==0)) {
    printf("ERROR fakeio xfer has zero kaddr OR zero len!\n");
    return;
  }
  if (len > NBPP_R6000) {
    printf("ERROR fakeio xfer can't handle more than 1 page transfer!\n");
    return;
  }

  if (delay_fakeio)    
    DELAY(100000);
  if (in_out) {
    gba_hw[GBAid].mem_kaddr[mem_page] = kaddr;
    gba_hw[GBAid].mem_len[mem_page] = len;
    if (fakeio_bypass) {
      if (fakeio_by_addr != kaddr)
	printf("BAD kaddr in fakeio_bypass, retry copy\n");
      if (fakeio_by_len != len)
	printf("Short len in fakeio_bypass, may be OK\n");
      return(1);
    };
    if (fakeio_copy16)
      hwcopy( K0_TO_K1(kaddr), iostartaddr, len, 1 ); 
    else {
      do {
	bcopy( K0_TO_K1(kaddr), iostartaddr, len );
	if (fakeio_verify) {
	  badxfer = bcmp( K0_TO_K1(kaddr), iostartaddr, len );
	  if (badxfer)
	    printf("BAD copy to VME memory, try again %x to %x for %x\n",
		   kaddr, iostartaddr, len);
	}
	else
	  badxfer = 0;
      }
      while (badxfer);
    }
  }
  else {
    gba_hw[GBAid].mem_kaddr[mem_page] =  gba_hw[GBAid].mem_len[mem_page] = 0;
    if (fakeio_bypass) {
      if (fakeio_by_addr != kaddr)
	printf("BAD kaddr in fakeio_bypass, retry copy\n");
      if (fakeio_by_len != len)
	printf("Short len in fakeio_bypass, may be OK\n");
      return(1);
      };
    if (fakeio_copy16)
      hwcopy( iostartaddr, K0_TO_K1(kaddr), len, 1 ); 
    else {
      do {
	bcopy( iostartaddr, K0_TO_K1(kaddr), len );
	if (fakeio_verify) {
	  badxfer = bcmp( iostartaddr, K0_TO_K1(kaddr), len );
	  if (badxfer)
	    printf("BAD copy to VME memory, try again %x to %x for %x\n",
		   kaddr, iostartaddr, len);
	}
	else
	  badxfer = 0;
      }
      while (badxfer);
    }
  }
  return(TRUE);

#endif STANDALONE
  
}


#endif	R6000_FAKEIO
