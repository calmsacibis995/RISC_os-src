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
#ident	"$Header: dkip.c,v 1.64.1.15.1.4.1.11 91/01/04 17:51:53 beacker Exp $"
#define WSG_BUG			/* Word Scatter/Gather doesn't work	*/
/*#define NO_STATS		/* No SAR statistics			*/
#define MACSI_POLL_BROKE	/* Polling needs XOU or better to work	*/
#define BINIT_BUG		/* binit() call timing messed up	*/
/*#define ADD_RESID_TOP */
#define NOIQEA
	
/*
 * dkip.c - Interphase VME 3200/4200/4400 disk driver
 *
 *	o Recode to pre-setup on iopb for each drive so that interrupt routine
 *	  can just copy iopb in to vme i/o space and issue a go
 *	o Fix driver to not issue clear&go if another command is pending
 *	o Add code to timeout routine to reset controller and restart any
 *	  pending commands - have to fix up ui->ui_bn (etc) state?
 *
 * MACSI & 4 Unit support added by Bill Perry @ Interphase Corporation
 *
 * **WARNING** MIPSEL is untested & won't work for all SHORTS.
 *
 *
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/immu.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/map.h"
#include "sys/dir.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/region.h"
#include "sys/proc.h"
#include "sys/conf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/sysmacros.h"
#include "sys/debug.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/kmem.h"

#define		b_pblkno	b_resid
#define		b_cyl		b_dkspare
#define		b_rcyl		b_resid
#define		B_CMD		B_FORMAT	/* steal b_flag field	*/
#define IO_MAP			/* set for MIPs				*/
#ifdef IO_MAP
#define	IODONE(bp)	{ if(!(bp->b_flags & B_CMD)) iounmap(bp); iodone(bp); }
#else
#define	IODONE(bp)	{ iodone(bp); }
#endif

#define ERR_RETURN(x) \
        { \
                u.u_error = (x); \
                return; \
	}
#include "sys/dvh.h"
#include "sys/dkvj_IPtypes.h"
#ifndef NOMACSI
#define		PF_WANT	BIT(0)			/* we want to poll 	*/
#define		PF_BUSY	BIT(1)			/* we are polling	*/
#endif
#include "sys/vmereg.h"
#include "sys/elog.h"
#include "sys/ioctl.h"
#include "sys/dkio.h"
#include "sys/edt.h"
#include "sys/dump.h"
#include "sys/dkipreg.h"
/*  #include "sys/kopt.h"  comment out for MIPS */
#include "sys/cpu_board.h"
#include "sys/gen_ioctl.h"
#ifdef IP4
#	include "sys/IP4.h"
#endif
#ifdef IP5
#	include "sys/IP5.h"
#endif
#if R2300 && SGI
#	include "sys/R2300.h"
#endif
#ifdef IP6
#	include "sys/IP6.h"
#endif

#define	TRUE	(1==1)
#define	FALSE	(1==0)

#ifdef	R6000
/*
 * This turns M2000 routine calls into NOPs for the RC6280 !!!
 */
#define	wbflush()
#endif	R6000

#ifdef	R6000_FAKEIO

extern unsigned int fakeio;
int fakeio_memtype = IPMT_32BIT;
int fakeio_am = VME_A24SAMOD;
int fakeio_am_block = VME_A24NPBMOD;
#undef	IPMT_32BIT
#undef	VME_A32NPAMOD
#undef	VME_A32NPBMOD
#define	IPMT_32BIT fakeio_memtype
#define VME_A32NPAMOD fakeio_am
#define VME_A32NPBMOD fakeio_am_block
#endif	R6000_FAKEIO

#define EMSG_BOOTUP	1
#define EMSG_INRETRY	2
#define EMSG_MAXRETRIES	3
#define EMSG_RECOVER	4

int dkip_induce = 0;
extern int disable_macsi;
extern	int timeout(/*int (*func)(), caddr_t, int */);
extern	char *strncpy(/* char *, char * */);
int has_dkip();
#ifdef PROFDBG
int dkipprf;
int dkipprfintr;
#endif
#ifdef DKDEBUG
VSMD_MSHIO ipsave;
struct ipiopb iopbsave;
#endif /* DKDEBUG */

/* forward references */
int	dkipedtinit(), dkipattach(), dkipintr(), dkiptimeout(),
	dkipinitiopb(), dkipSGsetup(), dkipsetup(), dkiperror(),
	dkiplogerr(), dkippoll(), is_vh();
int	dkipslave(), dkipreset(), dkipcready(), dkipuready();
char	*dkip_name();
extern caddr_t kmemzalloc();
static int dev_busy();
IPIOPB * dkipsplq();

#define MAXTRK		3		/* tracks to search for volume header */
#define NRETRIES	3		/* time to retry reading a block */
#define FILL_PATTERN	0xdb6d		/* fill pattern for format commands */
#define WAITLOOPS	100000

/* The following MACRO allows runtime diagnostic messages	*/
/* It is recommended having an IOCTL call to toggle this flag	*/
/* You must define DEFDIAG in order to enable this feature	*/

#define DEFDIAG
#ifdef DEFDIAG
int dkipdebug = 0;
#define DIAG		if(dkipdebug) printf
#endif

/*
 * The 3200 & 3201 controllers only support two drives.  The 4201 supports
 * four drives.
 */
#define MAGIC_FS	10			/* whole-volume minor number */
#define FS(dev)		(dev & 0xf)
#define UNIT(dev)	(((dev) >> 4) & 3)
#define CTLR(dev)	((((dev) >> 8) == dkiparray[0])? ((dev) >> 6) & 3 : (((dev) >> 6) & 3)+4)
#define MKDEV(ci,ui) ((((ci)->ci_ctlr > 3) ? ((dkiparray[1] << 8 ) | (((ci)->ci_ctlr - 4) << 6)):((dkiparray[0] << 8) | ((ci)->ci_ctlr) << 6)) | ((ui)->ui_unit << 4 ))

/* 
 * XXX this should be allocated at runtime so that ##C from 
 * master.d could be used
 */
   
#define MAXCTLRS	8
char	dkipprobed[MAXCTLRS];
#define MAX_NSECS	84		/* max number of sectors per track */
struct track_id track_id[MAX_NSECS + 1];
#ifdef MEASURE_TIME
#define DKIPLIST_SIZE 512
struct {
    struct buf *bp;
    unsigned int time;
    unsigned int which;
}dkiplist[DKIPLIST_SIZE];
int dkip_lindx = 0;
int dkip_printit = 0;
int dkip_trigger = 0;
#endif


/* TABLE to obtain REAL controller name & information */

static PLIST Plist[] = {
	{IP3200, "Interphase V/SMD 3200", PL_DUAL },
	{IP3201, "Interphase Gorilla 3201", 0 },
	{IP4200, "Interphase Cheetah 4200", PL_MSPARE|PL_DUAL|PL_MACSI },
	{IP4201, "Interphase Panther 4201", PL_MSPARE|PL_SOFT|PL_MACSI },
	{IP4400, "Interphase Phoenix 4400", PL_MSPARE|PL_DUAL|PL_MACSI },
	{0x00,	"Unknown Controller", 0 } /* DO NOT REMOVE */
};

/*
 * UIB's to try while attempting to read volume header to find
 * real uib info
 */
struct ipuib inituib[] = {
/*sh0 nh0 sh1 nh1 sctk skw bytsech byscl gap1 gap2 ilv rty cylh cyll att */
/* unit initialization block for the Fuji 2333 */
    {  0, 10,  0,  0,  63,  56, 512>>8,  512,  10,  20,  1, NRETRIES, 0x3, 0x37, 0x45, 0, 0, 0 }
};
#define	NUIB	(sizeof(inituib)/sizeof(inituib[0]))

/*
 * Error messages
 */
struct	iperrtab {
	unchar	code;
	char	*msg;
} iperrtab[] = {
	{ 0x10, "disk not ready" },
	{ 0x12, "seek error" },
	{ 0x13, "ECC error in data field" },
	{ 0x14, "invalid command code" },
	{ 0x15, "illegal fetch and execute" },
	{ 0x16, "invalid sector in command" },
	{ 0x17, "illegal memory type" },
	{ 0x18, "bus timeout" },
	{ 0x19, "header checksum error" },
	{ 0x1A, "disk write protected" },
	{ 0x1B, "unit not selected" },
	{ 0x1C, "seek error timeout" },
	{ 0x1D, "fault timeout" },
	{ 0x1E, "drive faulted" },
	{ 0x1F, "ready timeout" },
	{ 0x20, "end of medium" },
	{ 0x21, "translation fault" },
	{ 0x22, "invalid header pad" },
	{ 0x23, "uncorrectable error" },
	{ 0x24, "cylinder translation error" },
	{ 0x25, "head translation error" },
	{ 0x26, "sector translation error" },
	{ 0x27, "data overrun" },
	{ 0x28, "no index pulse on format" },
	{ 0x29, "sector not found" },
	{ 0x2A, "id field error - wrong head" },
	{ 0x2B, "invalid sync in data field" },
	{ 0x2C, "no valid header found" },
	{ 0x2D, "seek timeout error" },
	{ 0x2E, "busy timeout" },
	{ 0x2F, "not on cylinder" },
	{ 0x30, "rtz timeout" },
	{ 0x31, "invalid sync in header" },
	{ 0x3E, "incorrect UIB skew factor" },
	{ 0x40, "unit not initialized" },
	{ 0x42, "gap spec error" },
	{ 0x4B, "drive seek error" },
	{ 0x50, "sectors per track spec error" },
	{ 0x51, "bytes per sector spec error" },
	{ 0x52, "interleave spec error" },
	{ 0x53, "invalid head address" },
	{ 0x54, "invalid cylinder address" },
	{ 0x56, "zero sector count" },
	{ 0x5D, "invalid dma transfer count" },
	{ 0x60, "iopb failed" },
	{ 0x61, "dma failed" },
	{ 0x62, "illegal vme address" },
	{ 0x6A, "unrecognized header field" },
	{ 0x6B, "mapped header error" },
	{ 0x6E, "mapped sector number" },
	{ 0x6F, "no spare sector enabled" },
	{ 0x77, "command aborted" },
	{ 0x78, "acfail detected" },
	{ 0xA0, "S/G list too large" },
	{ 0xA1, "Illegal element byte count" },
	{ 0xA2, "Bad element address" },
	{ 0xA6, "Illegal entry count" },
	{ 0xAB, "Illegal element size" },
	{ 0xAC, "Illegal list byte count" },
	{ 0xAD, "Illegal IOPB sector count" },
	{ 0xAD, "Illegal element count" },
	{ 0xC0, "Both bits set" },
	{ 0xC1, "MSE without Initialize Long" },
	{ 0xD0, "Write Delay" },
	{ 0xE0,	"Already in MACSI mode" },
	{ 0xE1, "Bad CIB offset" },
	{ 0xE3, "Illegal number of CQEs" },
	{ 0xE7, "MACSI mode required" },
	{ 0xE8, "MACSI IOPB size invalid" },
	{ 0xE9, "Illegal number of Work Queue entries" },
	{ 0xF0, "Mapped Header encountered" },
	{ 0xFC, "No Write List" },
	{ 0xFD, "No Write Buffers" },
	{ 0xFE, "Out of Buffers" },
	{ 0xFF, "Command not implemented" },
	{ 0, 0 },
};
		/* for some unknown reason, if this is
		 * declared as a local in dkipedtinit()
		 * the kernel pukes....
		 */

/*
 * Map the entire DKIPCTLRINFO structure into GBA I/O bus address space.
 * This facilitates the mapping of the ci_buf, ui_uib, and dkipsg_fentry
 * structures which the controller needs to access.  Save the map
 * descriptor in the ci_sph field within the dkipctlrinfo structure.
 */

dkip_ci_iomap( ci )
struct dkipctlrinfo *ci;
{
  ioaddr_t VMEaddress;
  
  ASSERT( ci->ci_csh );
  ASSERT( ci->ci_sph == 0 );
  
  if (!vme_iomap(ci->ci_csh, ci, sizeof(struct dkipctlrinfo),
		 GBA_CONTIG_ADDR+GBA_NOPART_MAP,
		 &ci->ci_sph, &VMEaddress ))
    
    cmn_err(CE_PANIC, "Can't map dkipctlrinfo!\n");

}

/*
 * Prepare specified area in the dkipctlrinfo structure for I/O.  The
 * dkipctlrinfo structure is mapped for I/O at controller initialization
 * and left permanently mapped.  This routine will cause a cache writeback
 * from the CPU cache and return the "io address" usable by a controller
 * on the GBA to access the specified region.
 */

dkip_ci_ioinit( ci, dmaaddr, dmasize, io_addr )
struct dkipctlrinfo *ci;
ulong dmaaddr;
ulong dmasize;
ioaddr_t * io_addr;
{
  ASSERT( ci->ci_sph );
  
  if (!ka_to_vmeaddr( ci->ci_sph, dmaaddr, io_addr))
    cmn_err(CE_PANIC, "Address not in dkipctlrinfo map!\n");

  /*
   * Make sure that any data in the CPU cache has been written back to
   * physical memory.
   */
  writeback_virtual_data( dmaaddr, dmasize );
#ifdef	R6000_FAKEIO
  r6000_fakeio_xfer( ci->ci_sph, dmaaddr, dmasize, 1 );
#endif	R6000_FAKEIO
}


/*
 * Complete the I/O operation on the specified area in the dkipctlrinfo
 * structure.  This routine will flush the area from the GBA cache and
 * invalidate the CPU caches so that subsequent access to the area will
 * obtain the data from the completed I/O operation.
 */

dkip_ci_iodone( ci, dmaaddr, dmasize )
struct dkipctlrinfo *ci;
int dmaaddr;
ulong dmasize;
{
  int s;

  ASSERT( ci->ci_sph );

  /*
   * Flush dma area from GBA cache.
   */
  
  if (!vme_ioflush( ci->ci_sph, dmaaddr, dmasize))
    cmn_err(CE_PANIC, "Can't flush ci_buf region!\n");
  
  /*
   * Invalidate CPU cache so next access to will obtain the
   * latest data following the I/O operation.
   */

  s = splbio();
  invalidate_virtual_data( dmaaddr, dmasize );
  splx(s);
}


/*
 * Flush the I/O buffer from the GBA cache and unmap it for the
 * current I/O request.  (NON-MACSI routine)
 */

dkipiodone( ci,bp )
struct dkipctlrinfo *ci;
struct buf *bp;
{
  /*******************************************
   * If using ci_buf, flush it from GBA cache
   *******************************************/
  
  if ((int)bp == ci->ci_bufinuse)
    dkip_ci_iodone( ci, ci->ci_buf, sizeof(ci->ci_buf) );

  /******************************************************************
   * If sph was allocated, flush associated data from GBA cache, and
   * return map registers to free list.
   *****************************************************************/

  if (ci->ci_iodata_sph)  {
    
    if (!vme_iounmap( ci->ci_iodata_sph ))
      cmn_err( CE_PANIC, "Can't unmap io buffer!\n");
    
    ci->ci_iodata_sph = 0;
  }
}

/*
 * dkipmiodone ( dkip MACSI I/O done)
 *
 * Flush all I/O buffers from the GBA cache and unmap it for the
 * current I/O request.  Also return external SG list entries.
 * Routine will free any of these I/O resources on the first call.
 * This routine may be invoked several times on the same bp, however
 * subsequent call will find no work to do.
 * (MACSI routine )
 */

dkipmiodone( ci, bp )
struct dkipctlrinfo *ci;
struct buf *bp;
{
  IOINFO_DKIP_T *ioinfo_free;
  register IPSG_DKIP_FREE *ipsg_free;

  /*******************************************
   * If using ci_buf, flush it from GBA cache
   *******************************************/
  
  if ((int)bp == ci->ci_bufinuse)
    dkip_ci_iodone( ci, ci->ci_buf, sizeof(ci->ci_buf) );

  if (!(ioinfo_free = (IOINFO_DKIP_T*)bp->av_back))
    return;		/* No IO resources held. */

  /*************************************************
   * Put scatter/gather free entry back into list.
   ************************************************/
  if( (ipsg_free = ioinfo_free->dkip_sg_list)) /* any SG entry to return? */
    {	/* link back into free list */
      dkip_ci_iodone( ci, ipsg_free, sizeof(IPSG_DKIP_FREE));
      ipsg_free->nxt = ci->dkipsg_hd;
      ci->dkipsg_hd = ipsg_free;
      ioinfo_free->dkip_sg_list = NULL;
    }

  /******************************************************************
   * If sph was allocated, flush associated data from GBA cache, and
   * return map registers to free list.
   *****************************************************************/
  
  if (ioinfo_free->sph)
    {
      vme_iounmap( ioinfo_free->sph );    /* free map registers */
      ioinfo_free->sph = 0;
    }
  /**********************************
   * Finally, free the ioinfo entry.
   *********************************/
  
  ASSERT(ioinfo_free->bp == bp );
  ioinfo_free->ioinfo_nxt = ci->dkip_ioinfo_hd;
  ioinfo_free->bp = NULL;
  ci->dkip_ioinfo_hd = ioinfo_free;
  bp->av_back = NULL;

}

/*
 * Probe the controller.  If it responds, initialize it.
 */
int
dkipedtinit(e)
struct edt *e;
{
register struct dkipctlrinfo *ci;
register struct dkipunitinfo *ui;
register struct ipdevice *ip;
register volatile struct ipiopb *iopb;
register int i, j;
register unchar ipl, vec;

	ip = (struct ipdevice *) e->e_base;
#ifdef	R6000
	if (!(ip = (struct ipdevice *)find_r6000_controller
	      ( e, (int)&ip->ipcsr - (int)ip, sizeof(ip->ipcsr))))
	  {
		if (showconfig)
			printf("ipc%d: controller not present\n",
				       e->e_intr_info->v_unit);
		return;
	  }
#endif	R6000
	if (IOBADADDR(&ip->ipcsr, sizeof(ip->ipcsr)))
	{
		if (showconfig)
			printf("ipc%d: controller not present\n",
				       e->e_intr_info->v_unit);
		return;
	}
        ASSERT(ci = (struct dkipctlrinfo *)kmemzalloc(sizeof(struct dkipctlrinfo), M_DEVBUF, M_NOWAIT|M_CONTIGUOUS));
	/* Convert ci address into a K0 address so we don't need
	 * tlbs for translation and all of our K1 bcopys will work
	 * using K0_TO_K1(ci)
	*/
	dkipctlrptr[e->e_intr_info->v_unit] = ci =
	  (struct dkipctlrinfo *)(ptosv(kvtokptbl(ci)->pgm.pg_pfn) | poff(ci));
	ci->ci_device = ip;
	ci->ci_mode = 0;		/* clear all options (sets 2 unit   */
	ci->ci_pending = 0;		/* clear number of pending commands */
	ci->ci_bufinuse = 0;		/* clear inuse/poll flags	    */
	ci->ci_cmdtime = 0;
	ipl = e->e_intr_info->v_brl;
	vec = e->e_intr_info->v_vec;
	ci->ci_ctlr = e->e_intr_info->v_unit;
	ASSERT(ci->ci_tab = (struct iobuf *)kmemzalloc(sizeof(struct iobuf), M_DEVBUF, M_NOWAIT));

        /* Reserve a Logical Cache Section in the GBA for use by controller */

	if (!vme_reserve_iomap( ci->ci_ctlr, ci->ci_device, 32, &ci->ci_csh,
			     GBA_CS_AUTOEXP ))
	  cmn_err(CE_PANIC, "Couldn't allocate cache section in GBA!\n");
#ifdef	R6000_FAKEIO
	if (fakeio) {
	  fakeio_am = 0x39;	/* VME_A24NPAMOD */
	  fakeio_am_block = 0x3b;
	}
	else {
	  fakeio_am = 0x09;	/* VME_A32NPAMOD */
	  fakeio_am_block = 0x0b;
	}
#endif	R6000_FAKEIO

	dkip_ci_iomap( ci );

	/* 
	 * reset the controller BEFORE setting up the iopb's
	 */
	if (dkipreset(ci) == 0)
	{
		if (showconfig)
			printf("ipc%d: reset failed\n", ci->ci_ctlr);
		return;
	}

  	dkipinitctlr( ci, ip, ipl, vec, 0);

	/*
	 * Initialize the unit structures
	 */
	ui = &ci->ci_unit[0];
	for (i = 0; i < DKIPUPC; i++, ui++)
	{
		ui->ui_unit = i;
		ui->ui_ci = ci;
		ASSERT(ui->ui_tab = (struct iobuf *)kmemzalloc(sizeof (struct iobuf), M_DEVBUF, M_NOWAIT));
		ui->ui_iotime = &dkipiotime[ci->ci_ctlr][i];
	}


	/* get controller firmware revision */
	iopb = &ip->ipiopb;
	ui = &ci->ci_unit[0];
	dkipinitiopb(ui, iopb, IP_HANDSHAKE, 0);
	if (dkippoll(ui) == FALSE)
	{
		dkiperror( EMSG_BOOTUP, ui, MKDEV(ci, ui));
		if (showconfig)
			printf("ipc%d: can't get controller info\n",
				       ci->ci_ctlr);
		return;
	}


	/* save handshake info in ctlr struct	*/

	dkcpy(&iopb->ip_cyl, &ci->ci_hndshk, WSIZ(HNDSHK));
#ifdef OLDWAY
	ci->ci_hndshk = *((HNDSHK*)&iopb->ip_cyl); /* struct assignment	*/
#endif

	/*
	 * determine controller's level of support for scatter/gather and
	 * data cache.
	 */
	switch ((int)iopb->ip_lbnhi)
	{
	  /*
	   * First check for specials
	   */
	  case 0x6526:				/* V/ESDI 3201 */
		if ((iopb->ip_lbnlo & 0xfff) >= 0x00D) {
#ifdef notdef
/*
** caching does not work well with the 0x00D rev.
*/
			ci->ci_cacheOK = 1;
#endif
			ci->ci_sgOK = 1;
			ci->ci_addrmod = VME_A32NPAMOD;
		}
		else
		{
			cmn_err(CE_CONT, "ipc%d: old firmware\n", ci->ci_ctlr);
		}
		break;

	  case 0x4926:				/* V/SMD 3200 */
		if ((iopb->ip_lbnlo & 0xfff) >= 0x150)
		{
			ci->ci_cacheOK = 1;
			ci->ci_sgOK = 1;
			ci->ci_addrmod = VME_A32NPAMOD;
		}
		else
		{
			cmn_err(CE_CONT, "ipc%d: old firmware\n", ci->ci_ctlr);
		}
		break;
	  case 0x7430:				/* V/ESDI 4201 */
		ci->ci_cacheOK = 1;		/* for backward compat */
		ci->ci_sgOK = 1;		/* for backward compat */
		ci->ci_addrmod = VME_A32NPAMOD;
		ci->ci_mode = CI_SG|CI_WSG|CI_CACHE|CI_4UNIT;
		break;
	  case 0x6630:				/* V/SMD 4200 */
		ci->ci_cacheOK = 1;
		ci->ci_sgOK = 1;
		ci->ci_mode = CI_SG|CI_WSG|CI_CACHE;
		if( IS_M2000_ARCH || IS_R6300 )
		    ci->ci_addrmod = VME_A32NPBMOD;
		else
		    ci->ci_addrmod = VME_A32NPAMOD;
		break;
	  default:
		switch ( iopb->ip_lbnhi & 0xFF00 )
		{
		   case 0x6500:				/* V/ESDI 3201	*/
			ci->ci_cacheOK = 1;
			ci->ci_sgOK = 1;
			ci->ci_addrmod = VME_A32NPAMOD;
			ci->ci_mode = CI_SG|CI_CACHE;
			break;
		   case 0x4900:				/* V/SMD 3200	*/
			ci->ci_cacheOK = 1;
			ci->ci_sgOK = 1;
			ci->ci_addrmod = VME_A32NPAMOD;
			ci->ci_mode = CI_SG|CI_CACHE;
			break;
		   case 0x7400:				/* V/ESDI 4201 */
			ci->ci_cacheOK = 1;
			ci->ci_sgOK = 1;
			ci->ci_addrmod = VME_A32NPAMOD;
			ci->ci_mode = CI_SG|CI_WSG|CI_CACHE|CI_4UNIT;
			break;
		   case 0x6600:				/* V/SMD 4200 */
		   case 0x8200:				/* V/SMD 4400 */
			ci->ci_cacheOK = 1;
			ci->ci_sgOK = 1;
			ci->ci_mode = CI_SG|CI_WSG|CI_CACHE;
			if( IS_M2000_ARCH || IS_R6300 )
		    	    ci->ci_addrmod = VME_A32NPBMOD;
			else
		    	    ci->ci_addrmod = VME_A32NPAMOD;
			break;

		   default:
			printf("ipc%d: unknown controller type\n", ci->ci_ctlr);
			printf("firmware level high-0x%x, low-0x%x\n", iopb->ip_lbnhi,
					iopb->ip_lbnlo);
		}
	}

        dkipreinitiopbs(ci, ip);

#ifndef NOMACSI

	/*
	 * Test to determine which controllers have even a chance to support
	 * MACSI.  This test checks for 4200 controllers of at least rev 50
	 * proms and 4400 controllers of at least 4C proms.
	 */

	if((ci->ci_hndshk.p_code == IP4200 && (ci->ci_hndshk.p_idh >= '5')) ||
           (ci->ci_hndshk.p_code == IP4400 && 
		((ci->ci_hndshk.p_idh > '4' ) || 
		((ci->ci_hndshk.p_idh == '4') && (ci->ci_hndshk.p_idl >= 'C')))))
	{
		/* 
		 * Don't try macsi mode if kopt disable flag is set.
		 * MACSI is onlys supported on 4200 controllers with 
		 * revision levelsof 050 or greater, and 4400 with
		 * revisions of 04C and greater.  This is because this
		 * is the only configurations we have verified in house.
		 * We need to fall into this section of the code regardless 
		 * of our MACSIness since the scatter gather entries are 
		 * initialized below.
		 */
		if( (!disable_macsi)){
		        if(dkip_gmacsi(ci, ipl, vec, vec) >= 0)
			    ci->ci_mode |= CI_MACSI | CI_4UNIT;
			/* We are now in MACSI mode...	*/
		}else if( ci->ci_hndshk.p_code == IP4400 )
			ci->ci_mode |= CI_4UNIT;

		/* init Scatter/Gather Free list	*/
		for(i = 0; i < (NUM_DKIP_SG - 1); i++)
		{
			ci->dkipsg_fentry[i].nxt = &ci->dkipsg_fentry[i+1];
			for(j = 0; j < MACSI_DKIP_SG; j++)
			{
				ci->dkipsg_fentry[i].ipsg[j].sg_meminfo = 
				 IPMT_32BIT;
			        ci->dkipsg_fentry[i].ipsg[j].sg_addrmod = 
				    ci->ci_addrmod;
			}
		}

		ci->dkipsg_fentry[i].nxt = (IPSG_DKIP_FREE*) 0;/* end list */
		ci->dkipsg_hd = &ci->dkipsg_fentry[0]; /* init head	*/
		for(j = 0; j < MACSI_DKIP_SG; j++)
		{
			ci->dkipsg_fentry[i].ipsg[j].sg_meminfo = IPMT_32BIT;
			ci->dkipsg_fentry[i].ipsg[j].sg_addrmod = 
				ci->ci_addrmod;
		}
		
		/* init ioinfo list	*/
		
		for(i = 0; i < (NUM_DKIP_IOINFO - 1); i++)
		{
			ci->dkip_ioinfo[i].ioinfo_nxt = &ci->dkip_ioinfo[i+1];
			ci->dkip_ioinfo[i].dkip_sg_list = NULL;
			ci->dkip_ioinfo[i].sph = 0;
		}

		ci->dkip_ioinfo[i].ioinfo_nxt =
		  (IOINFO_DKIP_T *) NULL;/* end list */
		ci->dkip_ioinfo_hd = &ci->dkip_ioinfo[0]; /* init head	*/

	}
#endif /* NOMACSI */

	/* print something, if we are supposed to */

	if (showconfig)
	{
		dkipsplprctlr(ci);
	}
#ifdef BINIT_BUG
	binit();
#endif
	dkipprobed[ci->ci_ctlr] = 1;

	ui = &ci->ci_unit[0];
	if(ci->ci_mode & CI_4UNIT)	/* if in 4unit mode */
	{
		
		for (i = 0; i < DKIPUPC; i++, ui++)
		{
#ifdef MIPSEL
			ui->ui_dstatus = &ip->ipds4[(3-i) ^ 3];
#else MIPSEL
			ui->ui_dstatus = &ip->ipds4[3-i];
#endif MIPSEL
			ui->ui_ilv_drv = ipl | (i << 4);
			ui->ui_ecc = IPO_ECCEN;
			dkipattach(ui);
		}
	}
	else
	{
		for (i = 0; i < 2; i++, ui++)
		{
			if(i)
				ui->ui_dstatus = &ip->ipds1;
			else
				ui->ui_dstatus = &ip->ipds0;
				
			ui->ui_ilv_drv = ipl;
			ui->ui_ecc = IPO_ECCEN;
			dkipattach(ui);
		}
	}
	/*
	 * Now that everything is initialized, set a flag.  When the
	 * kernel panics, sometimes when it comes back up, it gets
	 * an interrupt while in this routine.  We want to throw 
	 * those spurious interrupts away, so we need to be able to
	 * check if the board has been initialized yet.
	 */
	ci->ci_isinit = 1;
}

/*
 * attach slave device
 */
int
dkipattach(ui)
struct dkipunitinfo *ui;
{
register volatile struct ipiopb *iopb;
register struct ipuib *ipu;
register struct dkipctlrinfo *ci;
register head;
ioaddr_t io_address;
sah_type temp_sph;

	ci = ui->ui_ci;

	if(((ui->ui_unit > 1) && (ci->ci_hndshk.p_code != IP4400)) || ui->ui_unit > 3)
	{
		return;
	}

	/* reset drive fault if necessary */
	if(!(ci->ci_mode & CI_4UNIT))	/* 4201 clears fault on first read */
	{				/* NEW 3201 firmware does also	   */
		if (dkipuready(ui, 1) == FALSE)
		{
			return;
		}
	}
#ifdef NOMACSI
	iopb = &ci->ci_device->ipiopb;
#endif

	/*
	 * Try reading sector 0 of some number of tracks on cylinder 0 with a
	 * variety of uib setups until the volume header can be read
	 * IOPBS move when MACSI is running so you just cant assign a ptr
	 * once..
	 */
	for (head = 0; head < MAXTRK; head++)
	{
		for (ipu = inituib; ipu < &inituib[NUIB]; ipu++)
		{
			/*
			 * Do INITIALIZE command to try a uib setup
			 * Controller can only access UIB via D16 references
			 */
			if(ci->ci_mode & CI_4UNIT) /* 4unit mode?	*/
				ipu->ipu_mbz |= RA_4UNIT; /* set 4 unit mode */
			else
				ipu->ipu_mbz &= ~RA_4UNIT; /* 2 unit mode */
#ifndef NOMACSI
			if(ci->ci_mode & CI_MACSI)	/* MACSI mode?	*/
				iopb = (volatile IPIOPB *)(MACSI_IOPB_ADDR(ci));
			else
				iopb = &ci->ci_device->ipiopb;
#endif

			/*
			 * get UIB out to phys mem and flush the GbaCache so
			 * the controller can read the config information
			 */
			writeback_virtual_data( (uint *)ipu, sizeof(struct ipuib) );

			/*
			 * Map ipu structure into GBA bus address space and
			 * obtain the GBA bus address usable by the
			 * controller. The mapping must be contiguous.
			 */
			
			if (!vme_iomap(ci->ci_csh,ipu,sizeof(struct ipuib),
					GBA_CONTIG_ADDR+GBA_NOPART_MAP,
					&temp_sph, &io_address ))
			  cmn_err(CE_PANIC, "Can't map ipu structure!\n");
					   
			dkipinitiopb(ui, iopb, IP_INITIALIZE, 0);
			iopb->ip_memtype = IPMT_16BIT;		wbflush();
			iopb->ip_addrmod = VME_A32NPAMOD;	wbflush();
			iopb->ip_bahi = HI16(io_address);	wbflush();
			iopb->ip_balo = LO16(io_address);	wbflush();
			if (dkippoll(ui) == FALSE)
			{
				if (showconfig)
				{
					dkiperror( EMSG_BOOTUP, ui, MKDEV(ci, ui));
				}
				return;
			}

			/*
			 * By convention, we just flush GBA cache at
			 * completion of each I/O operation.  This implies
			 * GBA cache is flushed at I/O initiation.
			 */
			if (!vme_iounmap( temp_sph))
			  cmn_err(CE_PANIC, "Can't flush/unmap ipu!\n");

			/*
			 * re-seek the drive
			 */

			/* Flush ci_buf from CPU cache & map into I/O space */
			dkip_ci_ioinit( ci, ci->ci_buf,
					    sizeof(ci->ci_buf), &io_address );
					   
#ifndef NOMACSI
			if(ci->ci_mode & CI_MACSI)	/* MACSI mode?	*/
				iopb = (volatile IPIOPB *)(MACSI_IOPB_ADDR(ci));
			else
				iopb = &ci->ci_device->ipiopb;
#endif
			dkipinitiopb(ui, iopb, IP_RECAL, IPO_ECCEN);
			iopb->ip_memtype = 0;		wbflush();
			iopb->ip_addrmod = ci->ci_addrmod;	wbflush();
			if (dkippoll(ui) == FALSE)
			{
			  if (showconfig)
			    {
			      dkiperror( EMSG_BOOTUP, ui, MKDEV(ci, ui));
			    }
			    return;
			}
    			if(!dkip_ci_iodone(ci, ci->ci_buf, sizeof(ci->ci_buf) ))
			  cmn_err(CE_PANIC, "Can't flush/unmap ipu!\n");

			/* Flush ci_buf from CPU cache & map into I/O space */
			dkip_ci_ioinit( ci, ci->ci_buf,
					    sizeof(ci->ci_buf), &io_address );
#ifndef NOMACSI
			if(ci->ci_mode & CI_MACSI)	/* MACSI mode?	*/
				iopb = (volatile IPIOPB *)(MACSI_IOPB_ADDR(ci));
			else
				iopb = &ci->ci_device->ipiopb;
#endif
			dkipinitiopb(ui, iopb, IP_SEEK, IPO_ECCEN);
			iopb->ip_memtype = IPMT_32BIT;		wbflush();
			iopb->ip_addrmod = ci->ci_addrmod;	wbflush();
			iopb->ip_cyl = 0;			wbflush();
			iopb->ip_head = head;			wbflush();
			iopb->ip_sec = 0;			wbflush();
			if (dkippoll(ui) == FALSE)
			{
			  if (showconfig)
			    {
			      dkiperror( EMSG_BOOTUP, ui, MKDEV(ci, ui));
			    }
			    return;
			}
			
    			if(!dkip_ci_iodone(ci, ci->ci_buf, sizeof(ci->ci_buf) ))
			  cmn_err(CE_PANIC, "Can't flush/unmap ipu!\n");

			/* Flush ci_buf from CPU cache & map into I/O space */
			dkip_ci_ioinit( ci, ci->ci_buf,
					    sizeof(ci->ci_buf), &io_address );

		DELAY(500000);
			/*
			 * Read in the first sector on this head, seeing if
			 * we find a volume header.
			 */
#ifndef NOMACSI
			if(ci->ci_mode & CI_MACSI)	/* MACSI mode?	*/
				iopb = (volatile IPIOPB *)(MACSI_IOPB_ADDR(ci));
			else
				iopb = &ci->ci_device->ipiopb;
#endif

			dkipinitiopb(ui, iopb, IP_READ, IPO_ECCEN);
			iopb->ip_scnt = 1;			wbflush();
			iopb->ip_memtype = IPMT_32BIT;		wbflush();
			iopb->ip_addrmod = ci->ci_addrmod;	wbflush();
			iopb->ip_bahi = HI16(io_address); wbflush();
			iopb->ip_balo = LO16(io_address); wbflush();
			iopb->ip_cyl = 0;			wbflush();
			iopb->ip_head = head;			wbflush();
			iopb->ip_sec = 0;			wbflush();
			if (dkippoll(ui) == FALSE)
			{
			  /* Flush from GBA & unmap ci_buf */
			  dkip_ci_iodone( ci, ci->ci_buf,
					      sizeof(ci->ci_buf) );
			  
			  if (showconfig)
			    {
			      dkiperror( EMSG_BOOTUP, ui, MKDEV(ci, ui));
			    }
			}
			else
			{
			  /* Flush from GBA & unmap ci_buf */
			  dkip_ci_iodone( ci, ci->ci_buf,
					      sizeof(ci->ci_buf) );
			  
			  if (is_vh((struct volume_header *)
				    K0_TO_K1(ci->ci_buf)) == TRUE)
			    {
			      goto gotvh;
			    }
			}
			/* reset drive fault if necessary */
			/* Four unit init MUST occur before any drive stats */
			/* can be looked at. So this cannot be moved up */
			if (dkipuready(ui, 1) == FALSE)
			{
				return;
			}
		}
	}
	return;

gotvh:

	/* config as per device params */
	bcopy((caddr_t) K0_TO_K1(ci->ci_buf), (caddr_t) &ui->ui_vh,
	      sizeof(ui->ui_vh));
	ui->ui_vhvalid = 1;
	ui->ui_spc = ui->ui_vh.vh_dp.dp_secs * ui->ui_vh.vh_dp.dp_trks0;
	ui->ui_maxretries = NRETRIES;
#ifndef  NOMACSI
	if(ci->ci_mode & CI_MACSI)		/* macsi mode?	*/
		iopb = (volatile IPIOPB*) MACSI_IOPB_ADDR(ci);
	else
#endif
		iopb = &ci->ci_device->ipiopb;

#ifndef SABLE
	dkipconfig(ui, iopb);

	if (dkippoll(ui) == FALSE)
	{
		dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );

		dkiperror( EMSG_BOOTUP, ui, MKDEV(ui->ui_ci, ui));
		/* drive didn't configure using info in volume header. oops */
		if (showconfig)
		    printf("ipc%dd%d: volume header contains bad parameters\n",
				      ci->ci_ctlr, ui->ui_unit);
		return;
	}
	dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );

#endif !SABLE
	if (showconfig)
	{
	    printf("ipc%dd%d: %d/%d/%d, %d:1 interleave\n",
			      ci->ci_ctlr, ui->ui_unit,
			      ui->ui_vh.vh_dp.dp_cyls,
			      ui->ui_vh.vh_dp.dp_trks0,
			      ui->ui_vh.vh_dp.dp_secs,
			      ui->ui_vh.vh_dp.dp_interleave);
	}
	ui->ui_attached = 1;
}

/*
 * check for slave's existence
 */
int
dkipslave(ui)
register struct dkipunitinfo *ui;
{
register volatile unchar *ipds;
register unchar status;

	if(((ui->ui_unit > 1) && (ui->ui_ci->ci_hndshk.p_code != IP4400)) 
					|| ui->ui_unit > 3)
		return (0);

#ifdef MACSI_POLL_BROKE		/* firmware bug: MACSI doesn't always poll */
	if(ui->ui_ci->ci_mode & CI_MACSI)	/* so just say its there */
		return(1);
#endif
	ipds = ui->ui_dstatus;

	/*
	 * Read drive status.  For some reason, the controller can return
	 * all zeros here.  If it does, try again after waiting a long time.
	 */
	status = *ipds;
	if (status & IPDS_UALIVE) 
	{
		return (1);
	}
#ifndef NOMACSI

	/* make sure that done is clear,
	 * controller doesn't poll if done is set
	 * This isn't true for MACSI mode
	 */
	if(!(ui->ui_ci->ci_mode & CI_MACSI))
#endif
	{
		if(ui->ui_ci->ci_device->ipcsr & IPCS_OPDONE)
	  andh_rmw(&ui->ui_ci->ci_device->ipcsr, ~(IPCS_OPDONE|IPCS_ERLST));
	}

	if (status == 0)
	{
		DELAY(500000);
		status = *ipds;
		if (status & IPDS_UALIVE) 
		{
			return (1);
		}

	}
	return (0);
}

/*
 * Retry the command presently scheduled for ui.  If necessary, reset a
 * drive fault.  Return FALSE if the command is successfully
 * restarted.
 */
int
dkipretry(ui)
register struct dkipunitinfo *ui;
{
register struct dkipctlrinfo *ci;
register unchar status;


	/*
	 * Look at drive status.  If the drive faulted, then issue a
	 * drive fault clearing command.
	 */
	ci = ui->ui_ci;
	status = *ui->ui_dstatus;

	if (((status & IPDS_UALIVE) == 0) ||
	    (((status & IPDS_URDY) == 0) && (dkipuready(ui, 0) == FALSE)))
	{
		/*
		 * If drive needed reseting, and dkipuready() couldn't
		 * reset it, give up.
		 */
		return (FALSE);
	}
	/*
	 * Restart command from the top by calling dkipcstart.
	 * Since drive may have been reset, the iopb is potentially
	 * garbage.  Use dkipcstart() to restart the current command
	 * (which is at the head of the controller queue) from ground
	 * zero.  Note that dkipcstart() does not clear ui_nretries so
	 * the command will eventually terminate.
	 */
	dkipcstart(ci);
	return (TRUE);
}

/*
 * interrupt handler
 */
int
dkipintr(ctlr)
int ctlr;
{
register volatile struct ipdevice *ip;
register volatile struct ipiopb *iopb;
register struct dkipctlrinfo *ci;
register struct dkipunitinfo *ui;
register struct buf *bp;
register struct iobuf *utab;
register struct iobuf *ctab;
register int s;


#ifdef PROFDBG
	if( dkipprfintr )
	    printf("i");
#endif
	/*
	 * Check to see if we get interrupts even before the controller
	 * has been completely initialized.
	 */
	if( (!dkipctlrptr[ctlr]) || !((int)dkipctlrptr[ctlr]->ci_isinit)){
		if( showconfig )
		    cmn_err(CE_CONT, "ipc%d: spurious interrupt\n", ctlr);
		return;
	}
	ASSERT((ctlr >= 0) && (ctlr < dkipctlrs));

	/*
	 * clear safety timeout and initialize some variables
	 */
	ci = dkipctlrptr[ctlr];

#ifndef NOMACSI
	if(ci->ci_mode & CI_MACSI){	/* MACSI MODE? */
		dkipmintr(ci);		/* call macsi int handlr */
		return;
	}
#endif
	ip = ci->ci_device;
	
	iopb = &ip->ipiopb;
	if(ci->ci_mode & CI_4UNIT)		/* 4unit mode?	*/
	{
		ui = &ci->ci_unit[(iopb->ip_ipl >> 4)];
	}
	else
	{
		if (iopb->ip_cmdopt & IPO_UNIT)
			ui = &ci->ci_unit[1];
		else
			ui = &ci->ci_unit[0];
	}
        if (!(ip->ipcsr & (IPCS_ERLST | IPCS_SC | IPCS_BERR | IPCS_OPDONE))) {
                cmn_err(CE_CONT, "ipc%dd%d: spurious interrupt, csr=0x%x\n",
                                 ci->ci_ctlr, ui->ui_unit, ip->ipcsr);
                ci->ci_sintr++;
                return;
        }

	s = splclock();
	if (ci->ci_timeid) {
		untimeout(ci->ci_timeid);
		ci->ci_timeid = 0;
	}
	splx(s);

 	ASSERT( ci->ci_pending >= 1);	/* Catch nested interrupt problems */
	ci->ci_pending--;
	ctab = ci->ci_tab;
	utab = (struct iobuf *) ctab->b_actf;
	bp = utab->b_actf;

	/*
	 * Insure that bp's unit matches the iopb
	 */
	ASSERT(UNIT(bp->b_dev) == ui->ui_unit);
	ASSERT(CTLR(bp->b_dev) == ci->ci_ctlr);
	ASSERT(ui->ui_bp == bp);

	/* 
	 * either error on last command, status change on drive,
	 * or bus error occurred
	 */
	if (ip->ipcsr & (IPCS_ERLST | IPCS_SC | IPCS_BERR))
	{
		dkipiodone( ci,bp );	/* Flush buffer & unmap */
		andh_rmw(&ip->ipcsr,
			 ~(IPCS_ERLST|IPCS_SC|IPCS_BERR|IPCS_OPDONE));

	/*
	 * REMEMBER: special commands get NO retries & print no ERRORS
	 */
		if(!(bp->b_flags & B_CMD))
		{
			if (ui->ui_nretries++ <= ui->ui_maxretries)
			{
				if (dkipretry(ui) == TRUE){
					return;
				}
			}
			dkiperror( EMSG_BOOTUP, ui, (int) bp->b_dev);
		}
		ui->ui_nretries = 0;
		bp->b_flags |= B_ERROR;
		bp->b_resid = bp->b_bcount;		/* XXX wrong */
		ci->ci_bufinuse = 0;		/* clear for possible retry */
		/*
		 * Since were clearing it, we better wakeup anybody
		 * that's waiting on it.
		 */
		wakeup(&ci->ci_bufinuse);
	}
	else if (ip->ipcsr & IPCS_OPDONE)
	{		/* normal command completion */
		dkipiodone( ci,bp );	/* Flush GBA cache & unmap */
		andh_rmw(&ip->ipcsr, ~IPCS_OPDONE);
		if ((ip->ipiopb.ip_statcode == IPS_EXCEPT) &&
		    ((iopb->ip_errcode & IPEX_ICKY) != IPEX_ECC))
		{
			/*
			 * Print out an error message for recovered non-ecc
			 * errors.
			 */
			dkiperror( EMSG_BOOTUP, ui, (int) bp->b_dev);
		}
		/* 
		 * Temporary buffer may hold data from transfer.  This buffer
		 * is only used when a transfer must cross a page boundary.
		 */
		if ( (int)bp == ci->ci_bufinuse )
		{
			ASSERT(ci->ci_bufva >= bp->b_dmaaddr);
			ASSERT(ci->ci_bufva < bp->b_dmaaddr + bp->b_bcount);
			ASSERT(ci->ci_bufdata <= NBPSCTR);
			if (bp->b_flags & B_READ)
			{
				/*
				 * Copy data from private buffer into mapped
				 * region.
				 */
				bcopy((caddr_t) K0_TO_K1(ci->ci_buf),
				      ci->ci_bufva, (int) ci->ci_bufdata);
			}
			if( bp->b_resid == 0 ){
			    ci->ci_bufinuse = 0;
			    wakeup(&ci->ci_bufinuse);
			}
			ci->ci_bufva = 0;
		}

		/* check for more data to transfer */
		if (bp->b_resid > 0)
		{
			dkipcommand(ui);
			return;
		}
		ui->ui_nretries = 0;
	}
	else
	{				/* spurious interrupts */
	  /*
	   * Should not be able to get here since we now catch spurious
	   * interrupts early in the interrupt handler.  Can only get here
	   * if the ip->csr value changes since we first read it.
	   */
 		cmn_err(CE_CONT, "ipc%dd%d: ipcsr value changed, csr=0x%x\n",
				 ci->ci_ctlr, ui->ui_unit, ip->ipcsr);
 		ci->ci_pending++;	/* Restore pending count */
		ci->ci_sintr++;
		return;
	}

	/*
	 * Advance queues.  Pull the current units queue off of the controller
	 * queue, giving other units a chance.  Advance this units queue to
	 * the next buffer that needs attention.
	 */
	ctab->b_active = 0;
	ctab->b_actf = utab->b_forw;
	utab->b_forw = NULL;
	utab->b_active = 0;
	utab->b_actf = bp->av_forw;
	utab->qcnt--;
#ifdef MEASURE_TIME
dkipinsert(bp, 0x2d2d2d2d);
#endif

#ifndef NO_STATS
	/* update accounting */
	ui->ui_iotime->io_resp += lbolt - bp->b_start;
	ui->ui_iotime->io_act += lbolt - utab->io_start;
#endif

	/*
	 * If this unit has more commands pending, put it at the end of
	 * the controller queue.
	 */
	if (utab->b_actf)
		dkipustart(ui);
	/*
	 * If the controller has more commands pending, start the next one
	 * going.
	 */
	if (ctab->b_actf && !ctab->b_active)
	{
#ifndef NO_STATS
		ci->ci_istart++;
#endif
		dkipcstart(ci);
	}

	/* lastly, mark the completed buffer done */
	IODONE(bp);
}

dkipscintr()
{
}

/*
 * no interrupt received from controller, within timeout period,
 * for last command issued to it.
 * XXX add in code to reset controller if lost interrupt and restart
 * XXX pending command
 */
int
dkiptimeout(ui)
register struct dkipunitinfo *ui;
{
register volatile struct ipdevice *ip;
register int s;


	ip = ui->ui_ci->ci_device;
	s = splbio();
	ui->ui_ci->ci_timeouts++;
	cmn_err(CE_CONT, "ipc%dd%d: timeout. bn=%d cmd=0x%x csr=0x%x",
			 ui->ui_ci->ci_ctlr, ui->ui_unit, ui->ui_bn,
			 ui->ui_ci->ci_device->ipiopb.ip_cmdcode,
			 ip->ipcsr);
	if (ip->ipcsr & IPCS_GOBUSY)
	{
		cmn_err(CE_CONT, " (command still in progress)\n");
		/*
		 * Start up timer again, in hopes of getting command to
		 * complete.
		 */
		ui->ui_ci->ci_timeid = timeout(dkiptimeout, ui, HZ*2);
	}
	else if (ip->ipcsr & IPCS_OPDONE)
	{
		cmn_err(CE_CONT, "(command finished)\n");
		dkipintr(ui->ui_ci->ci_ctlr);
	}
	splx(s);
}

/*ARGSUSED*/
dkipopen(dev, flag)
dev_t dev;
int flag;
{
register struct dkipunitinfo *ui;
register int ctlr;
#ifndef NOMACSI
register int s;
register DKIPCTLRINFO *ci;
#endif


	/* check that controller # is legit, and that it probed */
	ctlr = CTLR(dev);
	
	if ((ctlr >= dkipctlrs) || !dkipprobed[ctlr])
	{
		u.u_error = ENXIO;
		return;
	}

	/*
	 * Attach the given drive, if (a) it hasn't been attached before,
	 * and (b) it exists
	 */
	ui = &dkipctlrptr[ctlr]->ci_unit[UNIT(dev)];

	if (!ui->ui_attached)
	{
/*
 * SLAVE & ATTATCH run in polled mode and cannot be used in a command
 * Q'ing environment. Much work is needed to correct this. This is poor
 * design anyway. It can bring a system to a halt for several seconds.
 */

#ifndef NOMACSI
		ci = ui->ui_ci;
		if(ci->ci_mode & CI_MACSI)	/* MACSI MODE?	*/
		{
			s = splbio();
			while(ci->ci_pending || (ci->ci_pflgs & PF_BUSY))
			{
				ci->ci_pflgs |= PF_WANT; /* we want to poll */
				sleep((caddr_t)ci, PRIBIO);
			}
			ci->ci_pflgs = PF_BUSY;	/* only 1 poller at a time */
			splx(s);
		}
#endif
		if (dkipslave(ui))
			dkipattach(ui);
/*
 * MAGIC FS allows an open to succeed if the drive is there
 *
 */
		if(!ui->ui_attached)
		{
			if((FS(dev) != MAGIC_FS))
				u.u_error = ENXIO;
#ifndef MACSI_POLL_BROKE
			else if( !(*ui->ui_dstatus & IPDS_UALIVE))
				u.u_error = ENXIO;
#endif
		}
#ifndef NOMACSI
		if(ci->ci_mode & CI_MACSI)
		{
			ci->ci_pflgs &= ~PF_BUSY;
			if(ci->ci_pflgs & PF_WANT)	/* anybody else? */
			{
				wakeup((caddr_t)ci);
			}
			else
			{
				/*
	 			 * start up controller again
				 */

				if (ci->ci_tab->b_actf && !ci->ci_tab->b_active)
				{
#ifndef NO_STATS
					ci->ci_istart += macsi_cstart(ci);
#else
					macsi_cstart(ci);
#endif
				}
			}
		}
#endif
	}
	if( u.u_error == 0 )
	    ui->ui_open[FS(dev)]++;
}

/*ARGSUSED*/
dkipclose(dev, flag, otyp)
{
    struct dkipunitinfo *ui;
    register int ctlr;

    ctlr = CTLR(dev);
    ui = &dkipctlrptr[ctlr]->ci_unit[UNIT(dev)];

    /* XXX should wait for all pending i/o to finish and then return */
    ui->ui_open[FS(dev)] = 0;

    /* 
     * Make sure ECC and maxretries is re-enabled.
     */
    ui->ui_ecc = IPO_ECCEN;
    ui->ui_maxretries = NRETRIES;
}

/*
 * queue device request 
 */
dkipstrategy(bp)
register struct buf *bp;
{
register struct partition_table *pt;
register struct dkipctlrinfo *ci;
register struct dkipunitinfo *ui;
int sc;
int s;
unsigned int tempresid;


	/*
	 * Check for valid unit number and unit's existence
	 */
	ci = dkipctlrptr[CTLR(bp->b_dev)];
	ui = &ci->ci_unit[UNIT(bp->b_dev)];

	if (!dkipprobed[CTLR(bp->b_dev)] )
		goto bad;

	/* 
	 * Compute number of blocks in transfer and make sure the request 
	 * is contained in the partition.
	 */
	pt = &ui->ui_vh.vh_pt[FS(bp->b_dev)];
	sc = BTOBB(bp->b_bcount);

#ifndef NOMACSI
	/*
	 * In MACSI mode, b_error is used to track the retry count
	 * zeroing this in non-macsi mode won't hurt anything
	 * av_back is used to hold the residual when a request is put
	 * back in the unit Q. This assumes that sdisksort uses ONLY
	 * av_forw.
	 */
	bp->b_error = 0;	/* zero out MACSI retry count	*/
	bp->av_back = NULL;
#endif

	if(!(bp->b_flags & B_CMD))	/* check for special commands */
    	{
	        if (!ui->ui_attached)
		    goto bad;
	        /*
	         * make sure unit has a valid volume header
	         */
	        if (!ui->ui_vhvalid) {
		    cmn_err(CE_CONT, "ipc%dd%d dkipstrategy: bad volume header\n", CTLR(bp->b_dev), UNIT(bp->b_dev));
		    goto bad;
	        }
	        if ((bp->b_blkno < 0) || (bp->b_blkno + sc > pt->pt_nblks))
	        {
		        /* XXX this seams somewhat useless */
		        if ((bp->b_blkno == pt->pt_nblks) && 
		           (bp->b_flags & B_READ))
		        {
			        bp->b_resid = bp->b_bcount;
			        IODONE(bp);
			        return;
		        }
		        goto bad;
	        }

	        /* set block number for sdisksort() */
		bp->b_pblkno = (bp->b_blkno + pt->pt_firstlbn);
#ifdef IO_MAP
		iomap(bp);
#endif
	}
	else
	{
	/*
	 * special commands are NOT retried and have no residual
	 *
	 */
		bp->b_pblkno = -1;	/* make big to put at end of list */
	}

	bp->av_forw = NULL;		/* for BUG in sdisksort */
	ASSERT(ui->ui_spc);
	bp->b_cyl = (bp->b_blkno + pt->pt_firstlbn) / ui->ui_spc;

	/* update accounting for this buffer */
	s = splbio();
#ifndef NO_STATS
	bp->b_start = lbolt;
	ui->ui_iotime->io_cnt++;
	ui->ui_iotime->io_bcnt += sc;
#endif

#ifdef WSG_BUG
	if(bp->b_flags & B_PHYS && !(bp->b_flags & B_SWAP)){
	    /*
	     * There's an interlock problem for raw i/o.  If the
	     * buffer is not sector aligned in memory, or if the 
	     * transfer length is not a sector multiple, then it
	     * will need to use a local intermediate buffer, and
	     * copy between the buffer and user space.  However,
	     * there is only one buffer, so it must sleep here
	     * if it already in use.  Note that the sleep must
	     * be done before the bp has been put in the unit
	     * queue.
	     */
	    if( !(((long)bp->b_un.b_addr & (NBPSCTR - 1)) == 0) ||
	        !((bp->b_bcount & (NBPSCTR - 1)) == 0)){
	        while(ci->ci_bufinuse != 0) {
#ifdef PROFDBG
if(dkipprf)
    printf("Buf sleep - bp %x dev %x blkno %x bufinuse %x\n", bp, bp->b_dev, bp->b_blkno,ci->ci_bufinuse);
#endif /* PROFDBG */
			sleep((caddr_t)&ci->ci_bufinuse, PRIBIO);
#ifdef PROFDBG
if(dkipprf)
    printf("Buf awake - bp %x dev %x blkno %x\n", bp, bp->b_dev, bp->b_blkno);
#endif /* PROFDBG */

		}
	    /*
	     * The buffer must be reserved here.
	     */
	    ci->ci_bufinuse = (int)bp;
	    }
	}
#endif /* WSG_BUG */
	/* queue request */
	ui->ui_tab->qcnt++;
#ifdef MEASURE_TIME
dkipinsert(bp, 0x97979797);
#endif
	tempresid = bp->b_rcyl;
	bp->b_rcyl = bp->b_cyl;		/* disksort things b_cyl is in resid */
#ifdef IP_DISKSORT
	if( ci->ci_mode & CI_MACSI )
	    sdisksort(ui->ui_tab, bp);
	else
#endif
	    disksort(ui->ui_tab, bp, ui->ui_lastcyl);
	bp->b_rcyl = tempresid;		/* disksort things b_cyl is in resid */

	if (ui->ui_tab->b_active == 0)	/* put on ctlr Q if not already there */
	{
		dkipustart(ui);		/* put unit Q on Ctlr Q		*/
		if (ci->ci_tab->b_actf && !ci->ci_tab->b_active)
			dkipcstart(ci);
	}
	splx(s);
	return;
bad:
	bp->b_flags |= B_ERROR;
	IODONE(bp);
	return;
}

/*
 * Unit start routine.  If unit is already active, or if unit has nothing
 * to do, just return.  Otherwise, put this unit queue on the controllers
 * activity queue.  The controllers b_actf and b_actl pointers are used
 * to point to the first unit header which needs processing.  The b_forw and
 * b_back linkage is used within each unit header to link multiple units to
 * a controller.
 */
dkipustart(ui)
register struct dkipunitinfo *ui;
{
register struct dkipctlrinfo *ci;
register struct iobuf *utab;


	ci = ui->ui_ci;
	utab = ui->ui_tab;
	if (utab->b_active || (utab->b_actf == NULL))
		return;

	/* put unit on controller queue */
	if (ci->ci_tab->b_actf == NULL)
		ci->ci_tab->b_actf = (struct buf *) utab;
	else
		ci->ci_tab->b_actl->b_forw = (struct buf *) utab;
	ci->ci_tab->b_actl = (struct buf *) utab;
	utab->b_forw = NULL;

	utab->b_active = 1;
}

/*
 * setup a device operation
 */
dkipcstart(ci)
register struct dkipctlrinfo *ci;
{
register struct dkipunitinfo *ui;
register struct buf *bp;
register struct iobuf *utab;

#ifndef NOMACSI
	if(ci->ci_mode & CI_MACSI){		/* macsi mode?	*/
		macsi_cstart(ci);
		return;
	}
#endif

loop:
	/*
	 * If controller has nothing left to do, return
	 */
	if ((utab = (struct iobuf *) ci->ci_tab->b_actf) == NULL)
		return;

	/*
	 * Get first command off unit queue.  If this unit's queue is now
	 * empty, try the next unit queue.
	 */
	if ((bp = utab->b_actf) == NULL)
	{
		/*
		 * unit on controller queue is idle
		 */
		utab->b_active = 0;
		ci->ci_tab->b_actf = utab->b_forw;
		utab->b_forw = NULL;
		goto loop;
	}
	ci->ci_tab->b_active = 1;
	ui = &ci->ci_unit[UNIT(bp->b_dev)];

	if (dkipuready(ui, 0) == FALSE)
		goto bad;

	/*
	 * Set up the current "cylinder" for the non-macsi disk sort
	 * algorithm
	 */
	ui->ui_lastcyl = bp->b_cyl;
	bp->b_resid = bp->b_bcount;
	ui->ui_bn = bp->b_blkno + 
		ui->ui_vh.vh_pt[FS(bp->b_dev)].pt_firstlbn;
	ui->ui_bp = bp;
#ifndef NO_STATS
	utab->io_start = lbolt;
#endif
	dkipcommand(ui);
	return;

bad:
	ci->ci_tab->b_active = 0;
	utab->b_actf = bp->av_forw;
	bp->b_flags |= B_ERROR;
	IODONE(bp);
	goto loop;
}

/*
 * read from device
 */
dkipread(dev)
register dev_t dev;
{
	/* 
	 * Make sure that the transfer is on a sector boundary
	 * And that it's either a multiple of sector size or less
	 * than the sector size
	 */
	if( (u.u_offset & (NBPSCTR - 1)) || 
		((u.u_count > NBPSCTR) && (u.u_count & (NBPSCTR - 1)))){
		u.u_error = EIO;
		return;
	}
	if (physck(dkipctlrptr[CTLR(dev)]->
				    ci_unit[UNIT(dev)].
				    ui_vh.vh_pt[FS(dev)].pt_nblks, B_READ))
	{
		physio(dkipstrategy, 0, dev, B_READ);
	}
}

/*
 * write to device
 */
dkipwrite(dev)
register dev_t dev;
{
register volatile unchar *ipds;
register struct dkipctlrinfo *ci;
int unit;

	ci = dkipctlrptr[CTLR(dev)];
	unit = UNIT(dev);

	/* 
	 * Make sure that the transfer is on a sector boundary
	 * And that it's either a multiple of sector size or less
	 * than the sector size
	 */
	if( ( u.u_offset & (NBPSCTR - 1) ) ||
		((u.u_count > NBPSCTR) && (u.u_count & (NBPSCTR - 1)))){
		u.u_error = EIO;
		return;
	}
	ipds = ci->ci_unit[unit].ui_dstatus;
	if (*ipds & IPDS_WPROT)
	{
		cmn_err(CE_CONT, "ipc%dd%d: drive write protected\n", CTLR(dev), UNIT(dev));
		u.u_error = EIO;
		return;
	}
	if (physck(ci->ci_unit[unit].ui_vh.vh_pt[FS(dev)].pt_nblks, B_WRITE))
		physio(dkipstrategy, 0, dev, B_WRITE);
}

/*
 * Dump data to disk.
 */
int
dkipdump(dev, flag, bn, physaddr, count)
dev_t dev;
int flag;
daddr_t bn;
caddr_t physaddr;
int count;
{
register struct dkipctlrinfo *ci;
register struct dkipunitinfo *ui;
register volatile struct ipiopb *iopb;
struct partition_table *pt;
ioaddr_t io_address;
sah_type temp_sph;
int saveconfig;

#ifdef NODUMPING
	return(0);
#else
	ci = dkipctlrptr[CTLR(dev)];
	ui = &ci->ci_unit[UNIT(dev)];

	/*
	 * If controller didn't probe, or if the drive doesn't exist,
	 * or if it doesn't have a valid label, return an error.
	 */
	if (!dkipprobed[CTLR(dev)])
		return (ENXIO);
	if (!ui->ui_attached)
		return (ENXIO);
	if (!ui->ui_vhvalid)
		return (EINVAL);

	if (flag == DUMP_OPEN)
	{
	        /* 
	         * MACSI can't be used, since the state of the work queues
	         * is unknown after a PANIC.  Reset the controller and turn
	         * off MACSI if applicable.
	         */
	        if (dkipreset(ci) == 0)
	        {
		    if (showconfig)
			printf("ipc%d: reset failed\n", ci->ci_ctlr);
		    return(EIO);
	        }
#ifndef  NOMACSI
		ci->ci_mode &= ~CI_MACSI;
#endif
		iopb = &ci->ci_device->ipiopb;
		dkipinitctlr( ci, ci->ci_device, 0,0, 1);

		/*
		 * Turn off showconfig so that the volume header info
		 * isn't printed out
		 */
		saveconfig = showconfig;
		showconfig = 0;
		dkipattach(ui);
		showconfig = saveconfig;
		/* initialize device */
		if (dkippoll(ui) == FALSE) {
		  dkiperror( EMSG_BOOTUP, ui, (int)dev);
		  dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );
		  return (EIO);
		}
		dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );
		return (0);
	}
	if (flag == DUMP_CLOSE)
	{
		/* nop */
		return (0);
	}

	/* insure that request is within partition boundaries */
	pt = &ui->ui_vh.vh_pt[FS(dev)];
	if ((bn < 0) || (bn + count > pt->pt_nblks))
	{
		return (EINVAL);
	}
	bn += pt->pt_firstlbn;

	iopb = &ci->ci_device->ipiopb;
	if (dkipuready(ui, 0) == 0)
	{
		/*
		 * If drive went away on us, return an i/o error
		 */
		return (EIO);
	}

	/* write count sectors worth of data */
	dkipinitiopb(ui, iopb, IP_WRITE, IPO_LOGICAL);
	iopb->ip_lbnlo = LO16(bn);
	wbflush();
	iopb->ip_lbnhi = HI16(bn);
	wbflush();
	iopb->ip_scnt = count;
	wbflush();

	/* Following kludge only works for physical memory less than
	 * 512 MB.  Panic if physical memory exceeds this amount.
	 */

	if (physaddr > (caddr_t)K1SIZE)
	  cmn_err(CE_PANIC, "Can't dump more than 512 MB physical memory!\n");
	physaddr = (caddr_t)PHYS_TO_K0(physaddr);

	/*
	 * Map dump area into GBA bus address space and
	 * obtain the GBA bus address usable by the
	 * controller. The mapping must be contiguous.
	 */

	if (!vme_iomap(ci->ci_csh, physaddr, count*NBPSCTR,
			GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			&temp_sph, &io_address))
	  cmn_err(CE_PANIC, "Can't map area to be dumped!\n");
					   
	iopb->ip_balo = LO16(io_address);
	wbflush();
	iopb->ip_bahi = HI16(io_address);
	wbflush();

	writeback_virtual_data( (uint *)physaddr, count*NBPSCTR );

	/*
	 * Caller has already done a CPU cache writeback (i.e. called
	 * flush_cache).  The GBA (I/O) cache is always maintained in a
	 * "flushed" state by flushing on each I/O completion.
	 */

	if (dkippoll(ui) == FALSE)
	{
	  	if (!vme_iounmap( temp_sph ))
		  cmn_err(CE_PANIC, "Can't flush/unmap dump area!\n");
		dkiperror( EMSG_BOOTUP, ui, (int)dev);
		return (EIO);
	}
	if (!vme_iounmap( temp_sph ))
	  cmn_err(CE_PANIC, "Can't flush/unmap dump area!\n");
	return (0);
#endif
}

/*
 * return partition size, in blocks 
 */
long
dkipsize(dev)
dev_t dev;
{
register struct dkipunitinfo *ui;

	if (!dkipprobed[CTLR(dev)])
		return (-1);
	ui = &dkipctlrptr[CTLR(dev)]->ci_unit[UNIT(dev)];
	if (!ui->ui_attached || !ui->ui_vhvalid)
		return (-1);
	return (ui->ui_vh.vh_pt[FS(dev)].pt_nblks);
}

/*
 * ioctl routine
 *	See the dkipsplxxxxx routines for support in ioctl handling
 */
dkipioctl(dev, cmd, arg, flag)
dev_t dev;
unsigned int cmd;
caddr_t arg;
int flag;
{
register struct dkipctlrinfo *ci;
register struct dkipunitinfo *ui;
struct device_parameters *dp;
struct media_defect *md;
struct ctlr_info ct;
struct io_arg io_arg;
unsigned int status;
volatile IPIOPB *iopb;
ioaddr_t io_addr;
sah_type temp_sph;
struct volume_header dkipvh;

	ci = dkipctlrptr[CTLR(dev)];
	ui = &ci->ci_unit[UNIT(dev)];
	dp = &ui->ui_vh.vh_dp;

	if( (CTLR(dev) > dkipctlrs) || (UNIT(dev) > DKIPUPC) ||
	   !((int)ci->ci_isinit))
	    ERR_RETURN(ENXIO)

	status = 0;
	switch (cmd) {
	    case GIOCPRSTR:
	        ERR_RETURN(ENOTSUP);
	    case DIOCGETCTLR:
	        if (copyin(arg, (caddr_t) &io_arg, sizeof( struct io_arg)) < 0)
		    ERR_RETURN(EIO)
	        if (io_arg.datasz != sizeof(struct ctlr_info)) {
		    status = DIOC_BADSIZE;
		}else{
		    bzero(&ct,sizeof(struct ctlr_info));
		    ct.ci_flags = DP_SMD;
		    dkipgetctlr( ci, &ct );
		    if (copyout((caddr_t) &ct,
			(caddr_t) io_arg.memaddr, (int) io_arg.datasz) < 0)
		        status = DIOC_EFAULT;
		}
	        break;
#ifdef DIOCGETCONFIG
	    case DIOCGETCONFIG:
	        if (dkipsplgetconfig(ui, arg) == FALSE)
		    ERR_RETURN(EIO)
		break;
#endif

	    case DIOCDIAG:
	    case DIOCTEST:
		/*
		 * for now just look at Board-OK
		 *
		 */
#ifndef NOMACSI
 	        if(ci->ci_mode & CI_MACSI){
		    if(!(MACSI_MSR(ci) & MSR_BOK))
			ERR_RETURN(EIO)
		} else
#endif
		{
		    if(!(ci->ci_device->ipcsr & IPCS_BOK))
			ERR_RETURN(EIO)
		}
		break;
	    case DIOCGETVH:
		/* get volume header */

		if (!ui->ui_vhvalid)
		    ERR_RETURN(EIO)
		if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
		if( (int)io_arg.datasz != sizeof(struct volume_header))
		    ERR_RETURN(EIO)
		if (copyout((caddr_t) &ui->ui_vh,
			    (caddr_t) io_arg.memaddr, (int) io_arg.datasz) < 0)
		    ERR_RETURN(EFAULT)
		break;

	    case DIOCSETVH:
	    case DIOCINITVH:
	    case DIOCRECONFIG:
	        /* set volume header */

                if (!suser())
		    ERR_RETURN(EPERM)
		if (copyin(arg, (caddr_t)&io_arg, sizeof(io_arg))) 
		    ERR_RETURN(EFAULT)
		if( (int)io_arg.datasz != sizeof(struct volume_header)){
		    status = DIOC_BADSIZE;
		}else{
		    if (copyin((caddr_t) io_arg.memaddr, (caddr_t)&dkipvh,
				    (int) io_arg.datasz) < 0){
		        status = DIOC_EFAULT;
		    }else if (!is_vh(&dkipvh)){
			status = DIOC_NOTVOLHDR;
		    }else{
		     
		        bcopy((caddr_t)&dkipvh, (caddr_t)&ui->ui_vh, sizeof(dkipvh));
			if( cmd == DIOCRECONFIG ){
		            ui->ui_spc = (ui->ui_vh.vh_dp.dp_secs *
					     ui->ui_vh.vh_dp.dp_trks0);
		            if (dkipsplconfig(ui) == FALSE)
		                ERR_RETURN(EIO)
		            ui->ui_vhvalid = 1;
		            ui->ui_attached = 1;
			}
		    }
		}
		break;

	    case DIOCFMTTRK: /* format track */
                /*
		 * perform format operation.
                 * must be superuser and partition cannot be currently mounted.
                 */
                if (!suser())
		    ERR_RETURN(EPERM)
		if (copyin(arg, (caddr_t)&io_arg, sizeof(io_arg))) 
		    ERR_RETURN(EFAULT)
                if (io_arg.datasz != sizeof(struct fmt_map_info)) {
                    status = DIOC_BADSIZE;
                } else if (dkip_dev_busy(dev)) {
                    status = DIOC_DISKBUSY;
                } else if (copyin((caddr_t)io_arg.memaddr, 
		    (caddr_t) &ui->ui_fmi, sizeof(struct fmt_map_info)) < 0) {
                    status = DIOC_EFAULT;
                } else {
			if( dkipsplfmt(ui, &ui->ui_fmi) == FALSE )
			    ERR_RETURN(EIO);
		}
		break;
	    case DIOCVFYSEC:
                /*
                 * verify sectors on a track contain no ECC errors
                 */
                if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
                    ERR_RETURN(EFAULT)
                if ( !io_arg.datasz ){
                    status = DIOC_BADSIZE;
		}else {
		    iopb = dkipsplq(ui);
		    iopb->ip_cmdcode = IP_VERIFY;	wbflush();
		    iopb->ip_cmdopt = 0;		wbflush();
		    iopb->ip_cyl = io_arg.sectst / (dp->dp_trks0 * dp->dp_secs);
		    wbflush();
	 	    iopb->ip_head = (io_arg.sectst/dp->dp_secs) % dp->dp_trks0;
		    wbflush();
	 	    iopb->ip_sec = io_arg.sectst % dp->dp_secs;	wbflush();
		    iopb->ip_scnt = io_arg.datasz;	wbflush();
		    if( dkipsplexec(ui) == FALSE ){
			ERR_RETURN(EIO);
		    }
		}
		break;
	    case DIOCNOECC:
		/*
        	 * Bit 0	1 = Turn off drive ECC and retries
                 *       	0 = Turn on drive ECC and retries
        	 * Bit 1        1 = Turn off driver retries
	         * 		0 = Turn on driver retries
		 *
		 * Unlike SCSI, ECC is enabled/disabled with every
		 * command.  An array will be kept and checked when
		 * a command is issued.
		 */
                if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
                    ERR_RETURN(EFAULT)
		/* 
		 * Make it be that ui_ecc is 1 if ecc is enabled, 0 if
		 * not. This will save lots of calculations later.
		 */
		ui->ui_ecc = ((io_arg.sectst & 1) == 0) ? IPO_ECCEN : 0;
		if( ((io_arg.sectst & 1) == 0 ) || (io_arg.sectst & 2)){
		    ui->ui_maxretries = 0;
		}else{
		    ui->ui_maxretries = NRETRIES;
		}
		break;

	    case DIOCSOFTCNT:
		/*
		 * sectst - contains flag
		 * datasz - new value
		 * 
		 * sets retval to the soft error count.  If 'flag' is
		 * non-zero, the soft error count is reset to datasz.
		 */
                if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
                    ERR_RETURN(EFAULT)
		io_arg.retval = ui->ui_softcnt;
		if(io_arg.sectst)
		    ui->ui_softcnt = io_arg.datasz;
		if(copyout((caddr_t)&io_arg,(caddr_t)arg, sizeof(io_arg)) < 0 )
		    status = DIOC_EFAULT;
		break;

	    case DIOCRDEFECTS:

		if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
		/*
		 * The real size of struct media defect is 30.  For
		 * I/O purposes it's padded
		 */
                if (io_arg.datasz != sizeof(struct media_defect)) {
                    status = DIOC_BADSIZE;
                } else if (copyin((caddr_t)io_arg.memaddr, 
		    (caddr_t)ui->ui_md, sizeof( struct media_defect)) < 0) {
                    status = DIOC_EFAULT;
                } else {

		    if (!vme_iomap(ci->ci_csh, ui->ui_md, 
			    DEV_BSIZE, GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			    &temp_sph, &io_addr))
	  	        cmn_err(CE_PANIC, "Can't map DIOCRDEFECTS!\n");
		    md = (struct media_defect *)ui->ui_md;
		    iopb = dkipsplq(ui);
		    iopb->ip_cmdcode = IP_READFLAWS;
		    iopb->ip_cmdopt = 0;
		    iopb->ip_cyl = md->md_cyl;
	 	    iopb->ip_head = md->md_trk;
		    iopb->ip_sec = 0;
		    iopb->ip_balo = LO16(io_addr);
		    iopb->ip_bahi = HI16(io_addr);
		    iopb->ip_memtype = IPMT_32BIT;
		    iopb->ip_addrmod = ci->ci_addrmod;
		    if( dkipsplexec(ui) == FALSE ){
			if (!vme_iounmap( temp_sph ))
	  		    cmn_err(CE_PANIC, "Can't flush/unmap DIOCRDEFECTS!\n");
			ERR_RETURN(EIO);
		    }
		    if (!vme_iounmap( temp_sph ))
	  	        cmn_err(CE_PANIC, "Can't flush/unmap DIOCRDEFECTS!\n");
		    if(copyout((caddr_t)ui->ui_md,(caddr_t)io_arg.memaddr,
			 (int)io_arg.datasz) < 0 )
		        status = DIOC_EFAULT;

		}
		break;

	    case DIOCTRKID:
		/*
		 * track_id is an array large enough to fill in all the
		 * header information for each sector on a track.  
		 * The first entry contains the cylinder and head info.
		 */
		if (copyin(arg, (caddr_t) &io_arg, sizeof(io_arg)) < 0)
		    ERR_RETURN(EFAULT)
                if (!io_arg.datasz || (io_arg.datasz > 
			(sizeof(struct track_id) * MAX_NSECS))){
                    status = DIOC_BADSIZE;
                } else if (copyin((caddr_t)io_arg.memaddr, (caddr_t) track_id,
                        sizeof(struct track_id)) < 0) {
                    status = DIOC_EFAULT;
                } else {
		    dkip_ci_ioinit( ci, track_id, io_arg.datasz, &io_addr);
		    if (!vme_iomap(ci->ci_csh, track_id, io_arg.datasz,
			    GBA_CONTIG_ADDR+GBA_NOPART_MAP,
			    &temp_sph, &io_addr))
	  	        cmn_err(CE_PANIC, "Can't map DIOCTRKID!\n");
		    iopb = dkipsplq(ui);
		    iopb->ip_cmdcode = IP_TRACKID;
		    iopb->ip_cmdopt = 0;
		    iopb->ip_cyl = track_id[0].cylno;
	 	    iopb->ip_head = track_id[0].head;
		    iopb->ip_sec = 0;
		    iopb->ip_balo = LO16(io_addr);
		    iopb->ip_bahi = HI16(io_addr);
		    iopb->ip_memtype = IPMT_32BIT;
		    iopb->ip_addrmod = ci->ci_addrmod;
		    if( dkipsplexec(ui) == FALSE ){
		        if (!vme_iounmap( temp_sph ))
	  	            cmn_err(CE_PANIC, "Can't flush/unmap DIOCTRKID!\n");
		        ERR_RETURN(EIO);
		    }
		    if (!vme_iounmap( temp_sph ))
	  	        cmn_err(CE_PANIC, "Can't flush/unmap DIOCTRKID!\n");
		    if(copyout((caddr_t)track_id,(caddr_t)io_arg.memaddr,
			     (int)io_arg.datasz) < 0 )
		        status = DIOC_EFAULT;
		}
		break;

	    case DIOCRDCAP:
	    case DIOCSEEK:
	    case DIOCWRTVFY:
	    case DIOCREMOVE:
	    case DIOCDISKGEOM:
	    case DIOCDISKCACHE:
	    case DIOCGETDKTAB:
	    case DIOCADDDKTAB:
	    case DIOCDELDKTAB:
	    case DIOCSETATTR:
	    case DIOCSETDP:
		ERR_RETURN(ENOTSUP)
	    default:
		u.u_error = EINVAL;
		break;
	}
    io_arg.retval = status;
    if (copyout((caddr_t)&io_arg,arg,sizeof(io_arg)) < 0)
            ERR_RETURN(EFAULT)
    if (status)
        ERR_RETURN(EIO)
    u.u_error = 0;
    return;
}

/*
 * reset controller
 */
int
dkipreset(ci)
register struct dkipctlrinfo *ci;
{
register volatile struct ipdevice *ip;
register int timo;

	/* 
	 * force board into reset 
	 */
	ip = ci->ci_device;
	ip->ipcsr = IPCS_BDCLR;
	wbflush();
	DELAY(10);			/* hold bit high for a while */
	ip->ipcsr = 0;
	wbflush();

	/* 
	 * wait until power-up diagnostics are complete 
	 */
	timo = 0;
	do
	{
		DELAY(5000);		/* delay for at least 100us */
		if (timo++ > WAITLOOPS)
		{
			cmn_err(CE_CONT, "ipc%d: power up diag never completed\n",
				       ci->ci_ctlr);
			return (0);
		}
	} while (ip->ipcsr & IPCS_GOBUSY);


	DELAY(10);			/* Delay enought for BOK to go high */
	/* 
	 * check for successful diagnostics 
	 */
	if ((ip->ipcsr & (IPCS_BOK|IPCS_GOBUSY)) == 0)
	{
		return (0);
	}
	return (1);
}

/*
 * issue a data transfer command
 */
dkipcommand(ui)
register struct dkipunitinfo *ui;
{
register struct dkipctlrinfo *ci;
register struct buf *bp;
int s;

	ci = ui->ui_ci;
	bp = ui->ui_bp;
	if(bp->b_flags & B_CMD)		/* ioctl special command?	*/
	{
		dkipsplsetup(ui, bp);
	}
	else
	{
		ASSERT(bp->b_resid > 0);

	/*
	 * See if transfer can be done with scatter gather.
	 * Scatter gather can only be used if:
	 *	(a) the board can do it (new enough proms)
	 *	(b) the address and count are sector aligned
	 */
		if ((ci->ci_sgOK) &&
		    (((long)bp->b_dmaaddr & (NBPSCTR - 1)) == 0) &&
		    ((bp->b_resid & (NBPSCTR - 1)) == 0))
		{
			dkipSGsetup(ui, bp);
		}
		else
		{
			/*
			 * Can't use scatter gather.  Try to chain together
			 * as many iopbs as possible
			 */
			dkipsetup(ui, bp);
		}
	}

	/* start controller on command */
	ci->ci_pending++;
	s = splbio();
  
	orh_rmw(&ci->ci_device->ipcsr, IPCS_GOBUSY);

	/* start timeout */
	ASSERT(ci->ci_timeid == 0);
	ci->ci_timeid = timeout(dkiptimeout, ui, HZ);
	splx(s);
}

/*
 * Setup a read/write command, using the scatter gather registers.
 */
int
dkipSGsetup(ui, bp)
register struct dkipunitinfo *ui;
register struct buf *bp;
{
register volatile struct ipsg *sg;
register volatile struct ipiopb *iopb;
register long amount;
register int links;
register long dmaaddr;
register long physaddr;
register long total;
register long offset;
volatile struct ipsg *firstsg;
ioaddr_t io_address;


	sg = &ui->ui_ci->ci_device->ipsg[0];
	iopb = &ui->ui_ci->ci_device->ipiopb;
	firstsg = sg;

	/* fill in the iopb */
	if(ui->ui_ci->ci_mode & CI_4UNIT)		/* 4 unit mode?	*/
	{
		iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
		iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL;
	}
	else
	{
	    if (ui->ui_unit)
		iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL | IPO_UNIT;
	    else
		iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL;
	}
	wbflush();
	iopb->ip_lbnhi = HI16(ui->ui_bn);		wbflush();
	iopb->ip_lbnlo = LO16(ui->ui_bn);		wbflush();

	iopb->ip_errcode = 0;					wbflush();
	iopb->ip_statcode = 0;					wbflush();

	/* fill in scatter gather structs */
	dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
	links = 0;
	total = 0;
	/*
	 * Setup the GBA system map for as much of this request as
	 * we can handle.
	 */
	ASSERT( ui->ui_ci->ci_iodata_sph == 0);
	if (vme_iomap( ui->ui_ci->ci_csh, dmaaddr, bp->b_resid, 0,
			  &ui->ui_ci->ci_iodata_sph, &io_address ) == 0)
	  cmn_err(CE_PANIC, "Can't map dmaaddr!\n");

	while (bp->b_resid)
	{
		/* limit this sg to this page */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;
		ASSERT((amount & (NBPSCTR - 1)) == 0);

		/* fill in sg struct */
		
		if (!ka_to_vmeaddr( ui->ui_ci->ci_iodata_sph, dmaaddr,
				  &io_address))
		  break;   /* Ran out of map regs.  Start transfer. */
		  
		physaddr = io_address;
		ASSERT(physaddr != 0);
		sg->sg_count = amount;				wbflush();
		sg->sg_addrlo = LO16(physaddr | offset);	wbflush();
		sg->sg_addrhi = HI16(physaddr);			wbflush();

		bp->b_resid -= amount;
		dmaaddr += amount;
		total += amount;
		if (++links >= IP_MAXSG)
		{
			/*
			 * Ran out of links.  Stop now and start transfer.
			 */
			break;
		}
		sg++;
	}

	total >>= SCTRSHFT;			/* convert to sectors */
	ui->ui_bn += total;
	iopb->ip_scnt = total;					wbflush();
	if (links == 1)
	{
		/*
		 * With only one link, don't bother scatter/gathering
		 */
		if (bp->b_flags & B_READ)
			iopb->ip_cmdcode = IP_READ;
		else
			iopb->ip_cmdcode = IP_WRITE;
		wbflush();
		iopb->ip_memtype = IPMT_32BIT;			wbflush();
		iopb->ip_addrmod = ui->ui_ci->ci_addrmod;	wbflush();
		iopb->ip_sgentries = 0;				wbflush();
		iopb->ip_balo = firstsg->sg_addrlo;		wbflush();
		iopb->ip_bahi = firstsg->sg_addrhi;
	}
	else
	{
		if (bp->b_flags & B_READ)
			iopb->ip_cmdcode = IP_SCATTER;
		else
			iopb->ip_cmdcode = IP_GATHER;
		wbflush();
		iopb->ip_memtype = IPMT_16BITI;			wbflush();
		iopb->ip_addrmod = 0;				wbflush();
		iopb->ip_sgentries = links;			wbflush();
		iopb->ip_balo = LO16(firstsg);			wbflush();
		iopb->ip_bahi = 0;
	}
	wbflush();
}

/*
 * Setup chained iopbs for buffers that can't use scatter gather
 */
int
dkipsetup(ui, bp)
register struct dkipunitinfo *ui;
register struct buf *bp;
{
register volatile struct ipiopb *iopb;
register int amount;
register int iopbs;
register long dmaaddr;
register long physaddr;
register long offset;
register volatile struct ipiopb *lastiopb;
register struct dkipctlrinfo *ci;
ioaddr_t io_address;


	ci = ui->ui_ci;
	iopb = &ci->ci_device->ipiopb;
	lastiopb = 0;

	/* fill in iopbs */
	dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
	iopbs = 0;
	iopb->ip_memtype = IPMT_32BIT;				wbflush();
	iopb->ip_addrmod = ci->ci_addrmod;			wbflush();
	/*
	 * Setup the GBA system map for as much of this request as
	 * we can handle.
	 */
	ASSERT( ui->ui_ci->ci_iodata_sph == 0);
	if (vme_iomap( ui->ui_ci->ci_csh, dmaaddr, bp->b_resid, 0,
			  &ui->ui_ci->ci_iodata_sph, &io_address ) == 0)
	  cmn_err(CE_PANIC, "Can't map dmaaddr!\n");

	while (bp->b_resid)
	{
		/*
		 * fill in the generic part of the iopb
		 */
		if (bp->b_flags & B_READ)
			iopb->ip_cmdcode = IP_READ;
		else
			iopb->ip_cmdcode = IP_WRITE;
		wbflush();
	    if(ci->ci_mode & CI_4UNIT)		/* 4 unit mode?	*/
	    {
		iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
		iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL;
	    }
	    else
	    {
		if (ui->ui_unit)
		    iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL| IPO_UNIT;
		else
		    iopb->ip_cmdopt = ui->ui_ecc|IPO_INTEN|IPO_LOGICAL;
	    }
		wbflush();
		iopb->ip_lbnhi = HI16(ui->ui_bn);		wbflush();
		iopb->ip_lbnlo = LO16(ui->ui_bn);		wbflush();
		iopb->ip_errcode = 0;
		iopb->ip_statcode = 0;				wbflush();

		iopb->ip_sgentries = 0;				wbflush();

		/*
		 * limit this iopb to this page
		 */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;

		ASSERT(amount != 0);
		if (bp->b_resid < NBPSCTR)
			amount = bp->b_resid;
		else
			amount = NBPSCTR;
		if(ci->ci_bufinuse == (int)bp){

		    ci->ci_bufva = (caddr_t) dmaaddr;
		    ci->ci_bufdata = amount;
		    if (bp->b_flags & B_READ)
		    {
			/*
			 * reads get data from disk. copy is done in
			 * interrupt routine
			 */
		    } else {
			/*
			 * Copy data from users buffer into
			 * static buffer where disk can get to it.
			 */
			bcopy((caddr_t) dmaaddr,
				      (caddr_t) K0_TO_K1(ci->ci_buf), amount);
		    }
		    dkip_ci_ioinit( ci, ci->ci_buf,
					sizeof(ci->ci_buf), &io_address );
					   
	 	    physaddr = io_address;
		    iopb->ip_scnt = 1;
		}
		else
		{
			/* fill in iopb struct */
		  
			if (!ka_to_vmeaddr( ui->ui_ci->ci_iodata_sph, dmaaddr,
					  &io_address))
			  break;   /* Ran out of map regs.  Start transfer. */
		  
			physaddr = io_address;
			ASSERT(physaddr != offset);
			iopb->ip_scnt = amount >> SCTRSHFT;
		}
		wbflush();
		iopb->ip_balo = LO16(physaddr);			wbflush();
		iopb->ip_bahi = HI16(physaddr);			wbflush();
		ui->ui_bn += iopb->ip_scnt;
		if (lastiopb)
		{
			/*
			 * If this iopb is to be linked to a previous one,
			 * link it now.
			 */
			lastiopb->ip_cmdopt |= IPO_LINKPG;	wbflush();
		}

		bp->b_resid -= amount;
		dmaaddr += amount;
		if(ci->ci_bufinuse == (int)bp){
			/*
			 * Can only do one transfer if using temp buffer
			 */
			break;
		}
		if (++iopbs >= (IP_MAXIOPBS + 1))
		{
			/*
			 * Ran out of iopbs.  Stop now and start transfer.
			 */
			break;
		}

		/* advance to next iopb */
		lastiopb = iopb;
		if (iopb == &ci->ci_device->ipiopb) {
		  	/*
			 * Make sure all previous I/O completions have
			 * been flushed from the GBA cache if we're going
			 * to use the external IOPBs.
			 */
			if (bp->b_resid)
			  dkip_ci_iodone( ci, &ci->ci_iopb[0],
					 sizeof(ci->ci_iopb));
			iopb = (volatile struct ipiopb *)
				K0_TO_K1(&ci->ci_iopb[0]);
		      }
		else
			iopb++;
	}
}

/*
 * initialize controller with device parameters
 */
int
dkipconfig(ui, iopb)
register struct dkipunitinfo *ui;
register struct ipiopb *iopb;
{
register struct device_parameters *dp;
register DKIPCTLRINFO *ci = ui->ui_ci;
ioaddr_t io_addr;

	dp = &ui->ui_vh.vh_dp;
	ui->ui_uib.ipu_v0sh = dp->dp_shd0;
	ui->ui_uib.ipu_v0nh = dp->dp_trks0;
	ui->ui_uib.ipu_v1sh = dp->dp_shd1;
	ui->ui_uib.ipu_v1nh = dp->dp_trks1;
	ui->ui_uib.ipu_sectrk = dp->dp_secs;

/* We assume that if we get a skew that is less than half a track,
 * we were given a normal skew.
 * REMEMBER:
 *	The Interphase skew is backwards. ie.. for a skew of 5 on a 32
 *	sector/track drive. Use a skew of 27.
 */
	if(dp->dp_skew < (dp->dp_secs/2))
		ui->ui_uib.ipu_skew = dp->dp_secs - dp->dp_skew;
	else
		ui->ui_uib.ipu_skew = dp->dp_skew;

	ui->ui_uib.ipu_bytsechi = HI8(dp->dp_secbytes);
	ui->ui_uib.ipu_bytseclo = LO8(dp->dp_secbytes);
	ui->ui_uib.ipu_gap1 = dp->dp_gap1;
	ui->ui_uib.ipu_gap2 = dp->dp_gap2;
#ifndef INTERLEAVE
	dp->dp_interleave = 1;
#endif
	ui->ui_uib.ipu_intrlv = dp->dp_interleave;
	ui->ui_uib.ipu_retries = dp->dp_nretries;
	ui->ui_uib.ipu_cylhi = HI8(dp->dp_cyls);
	ui->ui_uib.ipu_cyllo = LO8(dp->dp_cyls);
	if (ci->ci_cacheOK)
		ui->ui_uib.ipu_attrib = IPAT_INCBYHEAD | IPAT_CACHEEN;
	else
		ui->ui_uib.ipu_attrib = IPAT_INCBYHEAD;
	if (dp->dp_flags & DP_RESEEK)
		ui->ui_uib.ipu_attrib |= IPAT_RESEEK;
	if (dp->dp_flags & DP_SECTSLIP)
	{
		ui->ui_uib.ipu_attrib |= IPAT_SPSECEN;
	}
	if (dp->dp_spare0)
	{
		ui->ui_uib.ipu_attrib |= IPAT_RUNTSECEN;
	}
/*
	if (dp->dp_flags & DP_IGNOREERRORS)
		ui->ui_uib.ipu_attrib |= IPAT_MVBADDATA;
*/
	if(ci->ci_mode & CI_4UNIT)			/* 4unit mode?	*/
		ui->ui_uib.ipu_mbz |= RA_4UNIT;	/* set 4 unit mode 	*/
	else
		ui->ui_uib.ipu_mbz &= ~RA_4UNIT; /* 2 unit mode 	*/
	if( dp->dp_cyls >= 1024 )
		ui->ui_uib.ipu_mbz |= RA_EXTADDR; /* Set extended addressing */
	else
		ui->ui_uib.ipu_mbz &= ~RA_EXTADDR; /* Set extended addressing */
	ui->ui_uib.ipu_statipl = 0;
	ui->ui_uib.ipu_statvec = 0;

	if (dkipuready(ui, 0) == FALSE)
	{
		/* oh well, drive isn't working */
		return;
	}


	dkipinitiopb(ui, iopb, IP_INITIALIZE, 0);
	/*
	 * Flush UIB from CPU cache out to physical memory and obtain
	 * I/O space address used by controller to access UIB.  GBA
	 * cache already flushed since we flush it at completion of each
	 * I/O operation.
	 */
	dkip_ci_ioinit( ci, &ui->ui_uib, sizeof(struct ipuib), &io_addr);

	/* have to use D16 for uib transfers */
	iopb->ip_memtype = IPMT_16BIT;				wbflush();
	iopb->ip_addrmod = VME_A32NPAMOD;			wbflush();
	iopb->ip_bahi = HI16(io_addr);				wbflush();
	iopb->ip_balo = LO16(io_addr);				wbflush();
	return;
}

/*
 * command independent I/O parameter block setup
 */
int
dkipinitiopb(ui, iopb, cmd, cmdopt)
register struct dkipunitinfo *ui;
register volatile IPIOPB *iopb;
int cmd, cmdopt;
{

	iopb->ip_cmdcode = cmd;					wbflush();
	iopb->ip_cmdopt = cmdopt;				wbflush();
	if(ui->ui_ci->ci_mode & CI_4UNIT)
	{
		iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
	}
	else
	{
		if (ui->ui_unit)
			iopb->ip_cmdopt |= IPO_UNIT;
	}
	iopb->ip_sgentries = 0;					wbflush();
	iopb->ip_errcode = 0;					wbflush();
	iopb->ip_statcode = 0;					wbflush();
}

/*
 * Find which error message corresponds to the given error code.
 */
char *
dkiperrmsg(errcode)
register unchar errcode;
{
register struct iperrtab *ipe;

	ipe = &iperrtab[0];
	while (ipe->msg)
	{
		if (ipe->code == errcode)
			return (ipe->msg);
		ipe++;
	}
	return ("Unknown error");
}

/*
 * error handler 
 */
int
dkiperror( msgtype, ui, dev)
int msgtype;
register struct dkipunitinfo *ui;
int dev;
{
register volatile struct ipdevice *ip;
register volatile struct ipiopb *iopb;
register struct dkipctlrinfo *ci;
unchar dstatus;

	ci = ui->ui_ci;
	ip = ci->ci_device;

#ifndef NOMACSI
	if(ci->ci_mode & CI_MACSI)	/* MACSI mode?	*/
	{
	    iopb = (volatile IPIOPB*)MACSI_RIOPB_ADDR(ci);
	    cmn_err(CE_CONT, "ipc%dd%ds%d : ", 
		 CTLR(dev), UNIT(dev), FS(dev));
	    if(MACSI_CRSW(ci) & CRSW_EX)	/* Exception?	*/
	    {

		if (iopb->ip_errcode & IPEX_ECC){
		    cmn_err(CE_CONT,
		    "Command completed successully after ECC correction was applied. chs=%d/%d/%d\n",
		 iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
		}
		if (iopb->ip_errcode & IPEX_RECAL){
		    cmn_err(CE_CONT, 
		    "Command completed successfully after drive restore & recal. chs=%d/%d/%d\n",
		    iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
		}
		if( !(iopb->ip_errcode & IPEX_ECC) && 
		    !(iopb->ip_errcode & IPEX_RECAL)){
		    if( (iopb->ip_errcode & 0xf) == 1 ){
		        cmn_err(CE_CONT, "1 controller retry attempted before command completed successfully. chs=%d/%d/%d\n",
			iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
		    }else{
			cmn_err(CE_CONT, "%d controller retries attempted before command completed successfully. chs=%d/%d/%d\n", 
			iopb->ip_errcode & 0xf, 
			iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
		    }
		}
	    }
	    else
	    {
		switch (msgtype ){
		    case EMSG_BOOTUP:
			cmn_err(CE_CONT,"Error during system initialization - %s chs=%d/%d/%d",
		 	dkiperrmsg(iopb->ip_errcode),
		 	iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
			break;
		    case EMSG_INRETRY:
			cmn_err(CE_CONT,"Retrying... %s chs=%d/%d/%d.",
		 	dkiperrmsg(iopb->ip_errcode),
		 	iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
			break;
		    case EMSG_MAXRETRIES:
			cmn_err(CE_CONT,"Hard error after %d retries - %s chs=%d/%d/%d.",
			ui->ui_maxretries, dkiperrmsg(iopb->ip_errcode),
		 	iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
			break;
		    case EMSG_RECOVER:
			cmn_err(CE_CONT,"Recovered error - %s chs=%d/%d/%d.",
			dkiperrmsg(iopb->ip_errcode),
		 	iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
			break;
		    default:
			cmn_err(CE_CONT,"Error - %s chs=%d/%d/%d.",
			dkiperrmsg(iopb->ip_errcode),
		 	iopb->ip_cyl, iopb->ip_head, iopb->ip_sec);
			break;
		}
		if( iopb->ip_errcode == 0x61  || iopb->ip_errcode == 0x62){
		    /* 
		     * If it was a DMA error, also print the address
		     */
		    cmn_err(CE_CONT, " addr : 0x%x\n", 
				iopb->ip_bahi << 16 | iopb->ip_balo);
		}else
		    cmn_err(CE_CONT, "\n");

	    }
	}
	else
#endif
	{
		iopb = &ip->ipiopb;

	cmn_err(CE_CONT,
		"ipc%dd%ds%d: error csr=0x%x bn=%d statcode=%x ",
		CTLR(dev), UNIT(dev), FS(dev),
		ip->ipcsr, ui->ui_bn, iopb->ip_statcode);
	}

	/*
	 * If this is called before the ui has been set up ui_dstatus will
	 * still be zero
	 */
	if( ui->ui_dstatus != 0 )
	    dstatus = *ui->ui_dstatus;
	else
	    dstatus = 0;


#ifndef NOMACSI
	if(ci->ci_mode & CI_MACSI)
		return;
#endif
	/*
	 * flush linked iopb information from cache
	 */
	if (iopb->ip_statcode == IPS_OK)
	{
		if (iopb->ip_cmdopt & IPO_LINKPG)
		{
			iopb = &ci->ci_iopb[0];
			/*
			 * Flush all external IOPBs from GBA cache and
			 * invalidate CPU cache before attempting to read
			 * status of completed I/O operation.
			 */
			dkip_ci_iodone( ci, iopb, sizeof(ci->ci_iopb));
			for (; ; iopb++)
			{
				if (iopb->ip_statcode != IPS_OK)
					goto badiopb;
				if ((iopb->ip_cmdopt & IPO_LINKPG) == 0)
					break;
			}
		}
		if (dstatus & (IPDS_FAULT|IPDS_SKERR))
		{
			/* drive fault occured */
			cmn_err(CE_CONT, "drive fault\n");
			dkiplogerr(ui, ERR_HOTHER, iopb);
		}
		else
		{
			/* command completed without error */
			cmn_err(CE_CONT, "software error\n");
		}
		goto done;
	}

badiopb:
	switch ((int)iopb->ip_statcode)
	{
	  case IPS_INPROG:		/* command is in progress */
		cmn_err(CE_CONT, "command in progress\n");
		break;

	  case IPS_EXCEPT:		/* command completed but with error */
		cmn_err(CE_CONT, "recovered");
		if (iopb->ip_errcode & IPEX_ECC)
			cmn_err(CE_CONT, ", data ecc correction done");
		if (iopb->ip_errcode & IPEX_RECAL)
			cmn_err(CE_CONT, ", drive restore & recal done");
		cmn_err(CE_CONT, ", errcode=0x%x\n", iopb->ip_errcode);
		if ((iopb->ip_cmdcode == IP_READ) || 
		    (iopb->ip_cmdcode == IP_WRITE))
		{
			if (IPEX_ECC & iopb->ip_errcode)
				dkiplogerr(ui, ERR_SECC, iopb);
			else
				dkiplogerr(ui, ERR_SOTHER, iopb);
		}
		break;
	
	  case IPS_ERROR:		/* command encountered an error */
		cmn_err(CE_CONT, "unrecovered '%s' (0x%x)\n",
				 dkiperrmsg(iopb->ip_errcode),
				 iopb->ip_errcode);

		if ((iopb->ip_cmdcode == IP_READ) || 
		    (iopb->ip_cmdcode == IP_WRITE))
		{
			switch ((int)iopb->ip_errcode)
			{
			case IPER_DFAULT:
				dkiplogerr(ui, ERR_HOTHER, iopb);
				break;

			case IPER_ECCERR:
			case IPER_HARDECC:
				dkiplogerr(ui, ERR_HECC, iopb);
				break;

			case IPER_HDRCSUM:
				dkiplogerr(ui, ERR_HCSUM, iopb);
				break;

			default:
				dkiplogerr(ui, ERR_HOTHER, iopb);
			}
		}
		break;

	  default:			/* unknown error type */
		cmn_err(CE_CONT, "unknown statcode=0x%x, errcode=0x%x\n",
				 iopb->ip_statcode, iopb->ip_errcode);
		break;
	}

done:
/*
	if(ci->ci_mode & CI_4UNIT)
	{

		cmn_err(CE_CONT, "Drive status: unit 0: 0x%x unit 1: 0x%x\n",
			 ip->ipds4[3], ip->ipds4[2]);
		if(ui->ui_ci->ci_hndshk.p_code == IP4400)
		    cmn_err(CE_CONT,"Drive status: unit 2: 0x%x unit 3: 0x%x\n",
			 ip->ipds4[1], ip->ipds4[0]);
	}
	else
	{
		cmn_err(CE_CONT, "Drive status: unit 0: 0x%x unit 1: 0x%x\n",
			 ip->ipds0, ip->ipds1);
	}
*/

	/* clear drive fault */
	if (dstatus & (IPDS_FAULT|IPDS_SKERR))
		(void) dkipuready(ui, 0);

#ifndef NOMACSI
	if((ci->ci_mode & CI_4UNIT) && !(ci->ci_mode & CI_MACSI))
#else
	if(ci->ci_mode & CI_4UNIT)
#endif
	{
		andh_rmw(&ip->ipcsr, ~(IPCS_SC|IPCS_BERR));
		andh_rmw(&ip->ipdscr, ~(IPCS_SC|IPCS_BERR));
	}
	else
	{
		andh_rmw(&ip->ipcsr, ~(IPCS_SC|IPCS_BERR));
	}
}

/*
 * log error in error table
 */
int
dkiplogerr(ui, errtype, iopb)
struct dkipunitinfo *ui;
int errtype;
register volatile struct ipiopb *iopb;
{
register volatile struct ipdevice *ip;
register struct error_table *ertp;
register int lbn;
register int i;

	ip = ui->ui_ci->ci_device;
	if (errtype >= NERRTYPES)
		return;

	if ((ip->ipiopb.ip_cmdcode != IP_READ) && 
	    (ip->ipiopb.ip_cmdcode != IP_WRITE))
	{
		lbn = -1;
	}
	else
	{
#ifndef NOMACSI
		if(ui->ui_ci->ci_mode & CI_MACSI)
		{
			lbn = (iopb->ip_cyl * ui->ui_spc) +
			 (iopb->ip_head * ui->ui_vh.vh_dp.dp_secs) +
			  iopb->ip_sec;
		}
		else
#endif
		{
			lbn = (iopb->ip_lbnhi << 16) | iopb->ip_lbnlo;
		}
	}
	ertp = ui->ui_errtab;
	if (!ertp)
		return;
	for (i=0; i < MAX_ERRBLOCKS; i++, ertp++)
	{
		if (ertp->et_lbn == lbn)
		{
			ertp->et_errcount[errtype]++;
			if (ertp->et_errcount[errtype] == 0)
			{
			    cmn_err(CE_CONT,
				"ipc%dd%d: error table overflowed for lbn %d\n",
				lbn);
			}
			return;
		}
		if (ertp->et_lbn == 0)
		{
			ertp->et_lbn = lbn;
			ertp->et_errcount[errtype]++;
			if (ertp->et_errcount[errtype] == 0)
			{
			    cmn_err(CE_CONT,
				"ipc%dd%d: error table overflowed for lbn %d\n",
				lbn);
			}
			return;
		}
	}
	cmn_err(CE_CONT, "ipc%dd%d: error table full\n",
			 ui->ui_ci->ci_ctlr, ui->ui_unit);
}

/*
 * Cause the controller to execute a command and wait for completion.
 * Return FALSE if it fails, TRUE if it succeeds.
 * 	(MACSI mode supported)
 */
int
dkippoll(ui)
struct dkipunitinfo *ui;
{
volatile struct ipdevice *ip;
#ifndef NOMACSI
register volatile VSMD_MSHIO *mshio;
#endif
int err;


	wbflush();				/* just in case */

#ifndef NOMACSI
    if(ui->ui_ci->ci_mode & CI_MACSI)		/* MACSI mode?	*/
    {
	mshio = (VSMD_MSHIO*)ui->ui_ci->ci_device;
	mshio->vsmd_cqe[MACSI_CQE_INDX(ui->ui_ci)].control = 
	 ((DISK_IOPB_TYPE << 8)|CQE_DKIP_GO);

	if(++MACSI_CQE_INDX(ui->ui_ci) >= NUM_CQES) /* advance to next CQE */
		MACSI_CQE_INDX(ui->ui_ci) = 0;

	/* wait for command to complete */
	err = macsi_wait(mshio);

	mshio->vsmd_crb.crsw = 0;

	/* return error status */
	if (err || (mshio->vsmd_crb.copyiopb.ip_statcode == IPS_ERROR))
	{
#ifdef DKDEBUG
dkipsave(mshio);
dkipiopbsave(&mshio->vsmd_crb.copyiopb);
#endif
		return (FALSE);
	}
    }
    else
#endif
    {
	/* start command */
	ip = ui->ui_ci->ci_device;
	orh_rmw(&ip->ipcsr, IPCS_GOBUSY);

	/* wait for command to complete */
	err = dkipcready(ui);
	andh_rmw(&ip->ipcsr, ~IPCS_OPDONE);

	/* return error status */
	if ((err == FALSE) || (ip->ipiopb.ip_statcode == IPS_ERROR))
		return (FALSE);
    }
	return (TRUE);
}

/*
 * Wait for controller to become ready.
 * Return TRUE if its working, FALSE if its busted.
 */
int
dkipcready(ui)
register struct dkipunitinfo *ui;
{
register volatile struct ipdevice *ip;
long timo;
register int x;
register struct dkipctlrinfo *ci;

#ifndef NOMACSI
	if(ui->ui_ci->ci_mode & CI_MACSI)
		return(TRUE);			/* MACSI is always ready */
#endif
	ip = ui->ui_ci->ci_device;
	/*
	 * Wait a reasonable amount of time for controller to become ready
	 */
	timo = 0;
	while (ip->ipcsr & IPCS_GOBUSY)
	{
		if (++timo > WAITLOOPS)
		{
#ifdef OLD_WAY
			if (dkipreset(ui->ui_ci) == FALSE)
				return (FALSE);
		        dkipinitctlr( ci, ip, 0,0,1);
			break;
#else
/* REMEMBER: after a RESET all units must be RE-inited */
			ci = ui->ui_ci;
			for(x = 0; x < DKIPUPC; x++)
				ci->ci_unit[x].ui_attached = 0;

/* once in 4 unit mode or MACSI don't send a reset or mode will change	*/
/* also 4200 & 4201 use dynamic rams get corrupted on reset		*/

			if(!ci->ci_mode & (CI_4UNIT|CI_MACSI)){
				dkipreset(ui->ui_ci);
		                dkipinitctlr( ci, ip, 0,0,1);
			}

			return (FALSE);
#endif
		}
#ifdef	R6000		
		/* We need to reset the DBE count since dkipdump() was
		 * not able to complete due to DBEs.
		 */
		reset_ioc_retry_count();
#endif	R6000		
		DELAY(1000);
	}
	return (TRUE);
}

/*
 * See if the given unit is ready.
 * Return TRUE if it is, FALSE if it isn't.
 */
int
dkipuready(ui, isprobe)
register struct dkipunitinfo *ui;
int isprobe;
{
register volatile struct ipdevice *ip;
register volatile unchar *ipds;
int i;
long timo;

#ifndef NOMACSI
	if(ui->ui_ci->ci_mode & CI_MACSI)	/* MACSI is always ready */
	{
		return(TRUE);
	}
#endif
	if (dkipcready(ui) == FALSE)
		return (FALSE);

	ip = ui->ui_ci->ci_device;
	ipds = ui->ui_dstatus;
	if (*ipds & IPDS_URDY)
	{
		return (TRUE);
	}

	/* clear fault on drive (try twice) */
	for (i = 0; i < 5; i++)
	{
		dkipinitiopb(ui, &ip->ipiopb, IP_RECAL, 0);
		wbflush();
		orh_rmw(&ip->ipcsr, IPCS_GOBUSY);
		for (timo = 0; timo < WAITLOOPS; timo++)
		{
			DELAY(1000);
			if ((ip->ipcsr & IPCS_GOBUSY) == 0)
				break;
		}
		andh_rmw(&ip->ipcsr, ~IPCS_OPDONE);
		DELAY(1000);
		if (*ipds & IPDS_URDY)
		{
			return (TRUE);
		}
	}

	/* oh well, give up */
	if (!isprobe)
	{
		/*
		 * For 3201 controllers, this happens when a drive is
		 * missing.  Thus, don't print anything if we time out.
		 * Note that this is only necessary on the 3201 because
		 * of a firmware bug where a drive will not always show its
		 * presence in the ipds register.
		 */
		cmn_err(CE_CONT, "ipc%dd%d: failed to clear drive fault\n",
				 ui->ui_ci->ci_ctlr, ui->ui_unit);
	}
	return (FALSE);
}

dkipprint(dev,str)
dev_t dev;
char * str;
{
	cmn_err(CE_CONT, "ipc%dd%ds%d: %s (dev 0x%x)\n",
			 CTLR(dev), UNIT(dev), FS(dev), str, dev);
}

/*
 * check for valid volume header
 */
is_vh(vhp)   /*  This used to be commented out  DAN */
register struct volume_header *vhp;
{
	register csum;
	register int *ip;

	if (vhp->vh_magic != VHMAGIC)
		return (FALSE);

	csum = 0;
	for (ip = (int *)vhp; ip < (int *)(vhp + 1); ip++)
		csum += *ip;
	if (csum == 0) {
		return (TRUE);
	} else {
#ifdef SABLE
		cmn_err(CE_WARN, "dkip is_vh() returns FALSE!\n");
#endif
		return (FALSE);
	}
} /*  This is the end of what was commented out ... DAN */

/*
 * Given a product code, This function will return a pointer to
 * the product name
 */

char *
dkip_name(p)
unchar p;
{
register PLIST *pl = &Plist[0];

	while(pl->pl_pcode)
	{
		if(pl->pl_pcode == p)
			break;
		pl++;
	}
	return(pl->pl_pname);
}

#ifndef NOMACSI
/*----------------------------------------------------------------------*
 *	Copyright (c) 1987, 1988 Interphase Corporation			*
 *									*
 *	THIS IS PROPRIETARY SOURCE CODE OF Interphase Corp		*
 *		MACSI support routines					*
 *----------------------------------------------------------------------*/

/*
 * setup mutiple device operations
 *
 * This is the key to making Command Queuing work.
 * The major difference between this version and the normal cstart routine
 * is that the Queues MUST be advanced as commands are sent to the board
 * and not when the command completes.
 *
 *	We will link the unit structure to the end of controller Q to insure
 *	that no unit gets starved off.
 *
 *	RETURNS: The number of commands started.
 */
macsi_cstart(ci)
register DKIPCTLRINFO *ci;
{
register DKIPUNITINFO *ui;
register struct iobuf *utab;
register struct iobuf *utabn;
register struct iobuf *ctab;
register struct buf *bp;
#ifndef NO_STATS
register int scnt = 0;
#endif


	ctab = ci->ci_tab;
	utab = (struct iobuf *) ctab->b_actf;	/* get the Top unit	*/

	/*
	 * If Loop while there is something to do
	 */

	while(utab)
	{

		utabn = (struct iobuf *)utab->b_forw;

	/*
	 * Get first command off unit queue.  If this unit's queue is now
	 * empty, unlink from ctlr Q and try next;
	 */
		if((bp = utab->b_actf) == NULL)
		{
			/*
			 * unit on controller queue is idle
			 */

			utab->b_active = 0;
			utab->b_forw = NULL;
			ctab->b_actf = (struct buf*)utabn;
			utab = utabn;
			continue;	/* start at top again */
		}
		ui = &ci->ci_unit[UNIT(bp->b_dev)];
		ui->ui_lastcyl = bp->b_cyl;

		if(macsi_ego(ui))	/* Controller Full?	*/
		{
			break;		/* YES, get out of here	*/
		}
#ifndef NO_STATS
		scnt++;
#endif
	/*
	 * If there is i/o for another unit put the next unit at
	 * the top of the ctlr's Q and put the unit just queued at the end
	 * of the ctlr's i/o Q.
	 * 
	 * Otherwise we just sit on the same unit for as long as we can.
	 *
	 */
		if(utabn )
		{
			ctab->b_actf = (struct buf*) utabn;
			ctab->b_actl->b_forw = (struct buf *) utab;
			ctab->b_actl = (struct buf*) utab;	
			utab->b_forw = NULL;
			utab = utabn;
		}
	}
#ifndef NO_STATS
	return(scnt);
#else
	return;
#endif
		
}
/*
 * Setup a special command (for ioctl functions)
 *	This function simply copies the iopb from the unit structure
 *	to the board. It force fills some fields to insure that the 
 *	driver resumes control.
 *
 *	These fields are selectively filled in so not to disturb vector &
 *	interrupt level fields.
 *
 */
int
dkipsplsetup(ui, bp)
register struct dkipunitinfo *ui;
register struct buf *bp;
{
register volatile IPIOPB *fiopb;		/* from iopb		*/
register volatile IPIOPB *iopb;			/* iopb on board	*/
register DKIPCTLRINFO *ci;


	ci = ui->ui_ci;

	fiopb = (IPIOPB*)(bp->b_dmaaddr);

	if(ci->ci_mode & CI_MACSI)
		iopb = (volatile IPIOPB*)MACSI_IOPB_ADDR(ci);
	else
		iopb = &ci->ci_device->ipiopb;

	/* fill in the forced values */

	fiopb->ip_cmdopt |= IPO_INTEN;			/* force ints	    */
	
	if(!(ui->ui_ci->ci_mode & CI_4UNIT))		/* 2 unit mode?	    */
	{
	    if (ui->ui_unit)
		fiopb->ip_cmdopt |= IPO_UNIT;		/* set for unit 1   */
	}

	iopb->ip_cmdcode = fiopb->ip_cmdcode;		wbflush();
	iopb->ip_cmdopt = fiopb->ip_cmdopt;		wbflush();
	iopb->ip_statcode = 0;				wbflush();
	iopb->ip_errcode = 0;				wbflush();
	iopb->ip_lbnhi = fiopb->ip_lbnhi;		wbflush();
	iopb->ip_lbnlo = fiopb->ip_lbnlo;		wbflush();
	iopb->ip_scnt = fiopb->ip_scnt;			wbflush();
	iopb->ip_bahi = fiopb->ip_bahi;			wbflush();
	iopb->ip_balo = fiopb->ip_balo;			wbflush();
	iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
	if(!(ci->ci_mode & CI_4UNIT)){
		if (ui->ui_unit){
			iopb->ip_cmdopt |= IPO_UNIT;
			wbflush();
		}
	}

	/* assume normvec, burst, errvec are same as last time */
	/* don't assume memory_type and address moddifier are OK too */

        iopb->ip_memtype = fiopb->ip_memtype;           wbflush();
        iopb->ip_addrmod = fiopb->ip_addrmod;           wbflush();

	iopb->ip_skew = fiopb->ip_skew;			wbflush();


	bp->b_resid = 0;		/* MUST be clean */
#ifndef NOMACSI
	bp->av_forw = 0;		/* no link	*/
	bp->av_back = 0;		/* no scatter/gather entry to free */
#endif
	return;
}

/*
 * Setup a read/write command, using the scatter gather.
 *		(MACSI Version)
 *	Word-wide scatter/gather will be used if the request is non-page
 *	aligned or if the request is not a sector multiple.
 * NOTE: Word-wide scatter/gather requests still have to multiples of sectors
 *	 if the request is not, the remaining portion of the request will
 *	 dumped into a bit bucket.
 */
int
dkipmSGsetup(ui, bp)
register struct dkipunitinfo *ui;
register struct buf *bp;
{
register DKIP_IPSG *sg;
register volatile struct ipiopb *iopb;
register long amount;
register int links;
register long dmaaddr;
register long total;
register long offset;
register DKIP_IPSG *firstsg;
register DKIPCTLRINFO *ci;
IPSG_DKIP_FREE *ipsg_free;
register uint temp;
register uint cbn;	/* block number of current bp	*/
register uint sbn;	/* starting block number	*/
int wsg;		/* set if word-wide scatter/gather */
register struct buf *nbp;
register struct iobuf *utab;
#ifdef PART_GATS
int byttrk;		/* number of bytes to read/write before head switch */
int bytereqs;		/* bytes to r/w for current bp */
#else
register int lbpcnt = 0;	/* count the number of linked bps */
#endif
ioaddr_t io_address;
IOINFO_DKIP_T *ioinfo_free;
int read_or_write;
ushort cmd_options;


	ci = ui->ui_ci;

	iopb = (volatile IPIOPB*)MACSI_IOPB_ADDR(ci);

	sbn = bp->b_blkno + ((bp->b_bcount - bp->b_resid) >> SCTRSHFT) +
		 ui->ui_vh.vh_pt[FS(bp->b_dev)].pt_firstlbn;

	cbn = sbn;	/* make it the current blk no */

	/*
	 * if request is sector aligned & sector mutiple, use
	 * sector-wide scatter/gather
	 */

	if( (((long)bp->b_dmaaddr & (NBPSCTR - 1)) == 0) &&
	    ((bp->b_resid & (NBPSCTR - 1)) == 0))
	{
		wsg = 0;	/* use sector-wide scatter/gather */
	}
	else
	{
#ifdef WSG_BUG
		wsg = 0;
		bp->av_forw = NULL;			/* end lbp list	*/
		bp->av_back = NULL;	/* no entry to free	*/
		dmaaddr = (long)bp->b_dmaaddr + (bp->b_bcount - bp->b_resid);
		/* 
		 * Since we can't sleep here, make sure that the interlock
		 * flag has already been assigned.
		 */
		ASSERT(ci->ci_bufinuse == (int)bp);
		if (bp->b_resid < NBPSCTR)
			amount = bp->b_resid;
		else
			amount = NBPSCTR;
		ci->ci_bufdata = amount;
		bp->b_resid -= amount;
		ci->ci_bufva = (caddr_t)dmaaddr;
		dkip_ci_ioinit( ci, ci->ci_buf,
				    sizeof(ci->ci_buf), &io_address );
					   
		IP_MEMINFO_AM( iopb, IPMT_32BIT, ci->ci_addrmod ); wbflush();
		iopb->ip_balo = LO16(io_address); wbflush();
		iopb->ip_bahi = HI16(io_address); wbflush();
		iopb->ip_scnt = 1;		wbflush();
		iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
		cmd_options = ui->ui_ecc|IPO_INTEN;
		if(!(ci->ci_mode & CI_4UNIT)){
		    if (ui->ui_unit){
			cmd_options |= IPO_UNIT;
		    }
	        }
		if (bp->b_flags & B_READ)
		  IP_CMD_CODEOPT( iopb, IP_READ, cmd_options )
		else
		  IP_CMD_CODEOPT( iopb, IP_WRITE, cmd_options )
		wbflush();
		iopb->ip_cyl = sbn / ui->ui_spc;		wbflush();
		temp = sbn % ui->ui_spc;
		IP_HEADSEC( iopb, 
		   temp / ui->ui_vh.vh_dp.dp_secs,
		   temp % ui->ui_vh.vh_dp.dp_secs);	wbflush();

		if (!(bp->b_flags & B_READ))
			bcopy((caddr_t) dmaaddr,
			      (caddr_t) K0_TO_K1(ci->ci_buf), amount);
		return;
#else WSG_BUG
		wsg = 1;	/* use word wide scatter/gather	*/
#endif WSG_BUG
	}
#ifdef PART_GATS

	temp = sbn % ui->ui_spc;
	IP_HEADSEC( iopb, 
		   temp / ui->ui_vh.vh_dp.dp_secs,
		   temp % ui->ui_vh.vh_dp.dp_secs);	wbflush();
	byttrk = (ui->ui_vh.vh_dp.dp_secs - iopb->ip_sec) * 512;
#endif

	/* Allocate ioinfo struct and point to SG struct */

	ioinfo_free = ci->dkip_ioinfo_hd;	/* get free entry */
	bp->av_back = (struct buf *)ioinfo_free; /* save for int rout to free*/
	ioinfo_free->bp = bp;			/* link ioinfo to bp */
	ci->dkip_ioinfo_hd = ioinfo_free->ioinfo_nxt;	/* unlink */
	ASSERT( ioinfo_free->sph == 0 );
	ASSERT( ioinfo_free->dkip_sg_list == NULL);

	/* fill in scatter gather structs */

	ipsg_free = ci->dkipsg_hd;		/* get top free entry	*/
	ioinfo_free->dkip_sg_list = ipsg_free; /* save for int rout to free */
	ci->dkipsg_hd = ipsg_free->nxt; 	/* unlink 		*/
	sg = &ipsg_free->ipsg[0];		/* get top SG  entry	*/
	firstsg = (DKIP_IPSG*)sg;

	utab = ui->ui_tab;
	links = 0;
	total = 0;
	temp = 0;
	/* Keep track of current operation for combining */
	read_or_write = bp->b_flags & B_READ;

lbpl:
	dmaaddr = (long)bp->b_dmaaddr + bp->b_bcount - bp->b_resid;
	/*
	 * Setup the GBA system map for as much of this request as
	 * we can handle.
	 */
	if (vme_iomap( ui->ui_ci->ci_csh, dmaaddr, bp->b_resid,
#ifdef R6000_FAKEIO		      
		      (bp->b_flags & B_READ) ?  GBA_MAP_WRITE : GBA_MAP_READ,
#else
		      0,
#endif R6000_FAKEIO		      
		      &ioinfo_free->sph, &io_address ) == 0)
	  cmn_err(CE_PANIC, "Can't map dmaaddr!\n");
#ifdef PART_GATS
	bytereqs = 0;
	while (bp->b_resid && !temp)
#else
	while (bp->b_resid)
#endif
	{
		/* limit this sg to this page */
		amount = bp->b_resid;
		offset = dmaaddr & (NBPP - 1);
		if (offset + amount > NBPP)
			amount = NBPP - offset;
#ifdef PART_GATS
	 	if( !(bp->b_flags & B_READ) && !wsg)
		{
			if((total + amount) > byttrk)
			{
				amount = byttrk - total;
				temp++;	
			}
			if((total + amount) == byttrk)
				temp++;
		}
#endif

		/* fill in sg struct */

		sg->sg_count = amount;
		sg->sg_addrlo = LO16(io_address | offset);
		sg->sg_addrhi = HI16(io_address);
		bp->b_resid -= amount;
		dmaaddr += amount;
		total += amount;
#ifdef PART_GATS
		bytereqs += amount;
#endif
		sg++;
		if (++links >= (MACSI_DKIP_SG - 1))	/* reserve 1 for alignment */
		{
			/*
			 * Too Many links, start up request
			 */
			temp++;
			break;
		}
		if (bp->b_resid)
		  /* If we're going through loop again, need to compute the
		   * io-address of the next page in the SG list.
		   */
		  if (!ka_to_vmeaddr( ioinfo_free->sph, dmaaddr, &io_address))
		    break;   /* Ran out of map regs.  Start transfer. */
	}

	if(!temp)
	{

		/*
		 * If there is root on the scatter/gather list, check
		 * and see if there are a number of contiguous reads/writes 
		 * in the unit queue.  If there are, combine them into
		 * one iopb transfer, using the s/g list for all of the
		 * bp's.
		 */
	 	if( ((bp->b_flags & B_READ) == read_or_write) &&
		  (!bp->b_error) &&
		  (!wsg) &&
	  	  ((nbp = bp->av_forw) != NULL) &&
		  ((nbp->b_flags & B_READ) == read_or_write) &&
		  (!nbp->av_back) &&
		  (!nbp->b_error) &&
#ifdef PART_GATS
                  ((cbn + (bytereqs >> SCTRSHFT)) == nbp->b_pblkno) &&
#else
                  ((cbn + (bp->b_bcount >> SCTRSHFT)) == nbp->b_pblkno) &&
#endif
		  (!((long)nbp->b_dmaaddr & (NBPSCTR - 1))) &&
		  (!(nbp->b_bcount & (NBPSCTR - 1)))
		  && (ci->dkip_ioinfo_hd)
#ifndef PART_GATS
		  && ( ++lbpcnt < LBPMAX)
#endif
		 )
		{

			bp = nbp;

			utab->qcnt--;
#ifdef MEASURE_TIME
dkipinsert(bp, 0x26262626);
#endif
			utab->b_actf = bp->av_forw;
			cbn = bp->b_pblkno;		/* get current blk # */
			bp->b_resid = bp->b_bcount;

			/*
			 * Allocate an ioinfo structure for the next bp.
			 */
			ioinfo_free = ci->dkip_ioinfo_hd;
			bp->av_back = (struct buf *)ioinfo_free;
			ioinfo_free->bp = bp;		/* link ioinfo to bp */
			ci->dkip_ioinfo_hd = ioinfo_free->ioinfo_nxt;
			ASSERT( ioinfo_free->sph == 0 );
			ASSERT( ioinfo_free->dkip_sg_list == NULL);

			goto lbpl;
		}
	}
#ifdef PART_GATS
        else
        {
                if(bp->b_resid)
                {
                        bp->b_flags |= B_PARTIAL;
			ui->ui_partial++;
                /* remove unit structure from ctlr Q.                   */
                /* leave the unit flagged as busy                       */
                /* this keeps future commands from being                */
                /* sent to the board.                                   */

                        if( !( ci->ci_tab->b_actf = utab->b_forw))
			    ci->ci_tab->b_actl = NULL;
                        utab->b_forw = NULL;

                }
        }
#endif

	bp->av_forw = NULL;			/* end lbp list	*/

	/* fill in the iopb */
	/* MACSI runs in physical */

	iopb->ip_ipl = ui->ui_ilv_drv;			wbflush();
	cmd_options = ui->ui_ecc|IPO_INTEN;
	if(!(ci->ci_mode & CI_4UNIT)){
		if (ui->ui_unit){
			cmd_options |= IPO_UNIT;
			wbflush();
		}
	}
	iopb->ip_cyl = sbn / ui->ui_spc;		wbflush();

#ifndef PART_GATS
	temp = sbn % ui->ui_spc;
	IP_HEADSEC( iopb, 
		   temp / ui->ui_vh.vh_dp.dp_secs,
		   temp % ui->ui_vh.vh_dp.dp_secs);	wbflush();
#endif

	if( amount = (total & (NBPSCTR -1)))	/* non-sector multiple? */
	{					/* YES, then fix it	*/
		sg->sg_count = NBPSCTR - amount;
		total += sg->sg_count;
		dkip_ci_ioinit( ci, ci->ci_buf,
				    sizeof(ci->ci_buf), &io_address );

		sg->sg_addrlo = LO16(io_address);
		sg->sg_addrhi = HI16(io_address);
		++links;
	}

	total >>= SCTRSHFT;			/* convert to sectors */
	iopb->ip_scnt = total;					wbflush();
	if (links == 1)
	{

		/*
		 * With only one link, don't bother scatter/gathering
		 */
		if (bp->b_flags & B_READ)
		  IP_CMD_CODEOPT( iopb, IP_READ, cmd_options )
		else
		  IP_CMD_CODEOPT( iopb, IP_WRITE, cmd_options )
		wbflush();
		IP_MEMINFO_AM( iopb, IPMT_32BIT, ci->ci_addrmod ); wbflush();
		iopb->ip_balo = firstsg->sg_addrlo;		wbflush();
		iopb->ip_bahi = firstsg->sg_addrhi;

		/*
		 * put scatter/gather entry back into free list
		 */

		ipsg_free->nxt = ci->dkipsg_hd;
		ci->dkipsg_hd = ipsg_free;
		ioinfo_free->dkip_sg_list = NULL;    /* no SG entry to free */
	}
	else
	{
		if(wsg)		/* word wide scatter/gather?	*/
		{
			if (bp->b_flags & B_READ)
			  IP_CMD_CODEOPT( iopb, IP_WSCATTER, cmd_options )
			else
			  IP_CMD_CODEOPT( iopb, IP_WGATHER, cmd_options )
		}
		else
		{
			if (bp->b_flags & B_READ)
			  IP_CMD_CODEOPT( iopb, IP_SCATTER, cmd_options )
			else
			  IP_CMD_CODEOPT( iopb, IP_GATHER, cmd_options )
		}
		wbflush();

		dkip_ci_ioinit( ci, firstsg, sizeof(IPSG_DKIP_FREE),
			       &io_address );

		iopb->ip_sgentries = links;			wbflush();
		IP_MEMINFO_AM( iopb, IPMT_32BIT, ci->ci_addrmod ); wbflush();
		iopb->ip_bahi = HI16(io_address);		wbflush();
		iopb->ip_balo = LO16(io_address);		wbflush();
	}
	wbflush();
}

/*
 *      routine to wait on a command by polling the CRBV (MACSI mode)
 */
int
macsi_wait(mshio)
volatile VSMD_MSHIO *mshio;
{
register volatile IPIOPB *iopb;
register int cnt;

	iopb = &mshio->vsmd_crb.copyiopb;
	for (cnt = 1000; !(mshio->vsmd_crb.crsw & CRSW_CRBV) && cnt; cnt--)
	{
		DELAY(3000);
	}
	if(!cnt)
	{
	    cmn_err(CE_CONT,"ipc%d macsi_wait: timeout on command crsw CRBV (%x)\n",
		whichctlr(mshio), mshio->vsmd_crb.crsw);
	    return(-1);
	}
	for (cnt = 1000; !(mshio->vsmd_crb.crsw & CRSW_CC) && cnt; cnt--)
	{
		DELAY(3000);
	}
	if(!cnt)
	{
	    cmn_err(CE_CONT,"ipc%d macsi_wait: timeout on command crsw CC (%x)\n",
		whichctlr(mshio), mshio->vsmd_crb.crsw);
	    return(-1);
	}
	if (iopb->ip_statcode == IPS_ERROR)
	{
		return (iopb->ip_errcode);
	}
	return (0);
}

/*
 * MACSI interrupt handler
 */
int
dkipmintr(ci)
register DKIPCTLRINFO *ci;
{
register volatile VSMD_MSHIO *mshio;
register volatile CRB *crb;
register volatile struct ipiopb *iopb;
register struct dkipunitinfo *ui;
register struct buf *bp;
register struct partition_table *pt;
register struct buf *nbp;
int i;
ushort crsw_copy;		/* For efficiency (esp on 6280), read once */
unsigned int tempresid;


	mshio = (VSMD_MSHIO*) ci->ci_device;
	crb = &mshio->vsmd_crb;
	iopb = &crb->copyiopb;

	crsw_copy = crb->crsw;
#ifndef NOIQEA
	if(crsw_copy & CRSW_CQA)	/* CQE Available int?	*/
	{
		ci->ci_tab->b_active = 0;	/* ctlr no longer busy	*/
#ifndef NO_STATS
		ci->ci_istart += macsi_cstart(ci);
#else
		macsi_cstart(ci);
#endif
		crb->crsw = 0;			/* clear CRBV		*/
		wbflush();
		return;
	}
#endif
	if(!(crsw_copy & CRSW_CC))
	{				/* spurious interrupts */
		cmn_err(CE_CONT, "ipc%d: spurious interrupt, crsw=0x%x\n",
				 ci->ci_ctlr, crb->crsw);
		ci->ci_sintr++;
		return;
	}

	ci->ci_pending--;		/* one less command out there */

	/*
	 * Normally the cmdtag is an index into the dkip_ioinfo array.
	 * Sometimes it is the bp pointer itself.
	 */
	if ((i = crb->cmdtag_h) > NUM_DKIP_IOINFO)
	  bp = (struct buf *) (crb->cmdtag_l + (i << 16));
	else
	  bp = ci->dkip_ioinfo[i].bp;
	ASSERT( bp );

        if(bp->b_flags & B_CMD)
        {
                unsigned short *p, *p2;
                int cnt = sizeof(*iopb) / sizeof(unsigned short);
                p2 = (unsigned short *)bp->b_dmaaddr;
                p = (unsigned short *) iopb;
                while( cnt--)
                        *p2++ = *p++;

        }
	ui = &ci->ci_unit[UNIT(bp->b_dev)];
#ifdef PROFDBG
if(dkipprf)
    printf("dkipmintr - bp %x start %x\n", bp,bp->b_start);
#endif
#ifdef MEASURE_TIME
dkipinsert(bp, 0x0f0f0f0f);
#endif


	/*
	 * Insure that bp's unit matches the iopb
	 */
	ASSERT(UNIT(bp->b_dev) == ui->ui_unit);
	ASSERT(CTLR(bp->b_dev) == ci->ci_ctlr);

	if(crsw_copy & CRSW_ER || (dkip_induce && bp->b_dev == dkip_induce)) 	/* ERROR?		*/
#ifdef DKDEBUG
dkipiopbsave(iopb);
#endif
	{
lbpl:
#ifdef PART_GATS
	  	if(bp->b_flags & B_PARTIAL)
		  {
		    bp->b_flags &= ~B_PARTIAL;
		    ui->ui_partial--;
		    
		    /* mark unit structure as no longer busy                 */
		    /* this allows dkipustart() to re-queue the unit struct. */
		    /* onto the ctlr queue                                   */
		    
		    ui->ui_tab->b_active = 0;
		  }
#endif
		dkipmiodone( ci, bp );	/* Flush & unmap GBA, return SG list */
		ASSERT(bp->av_back == NULL);
		nbp = bp->av_forw;

			/* time to give up?	*/
		/* SPECIAL commands get NO retries & print no ERRORS */
		if(bp->b_flags & B_CMD)
		{
		  	ASSERT( (int)bp != ci->ci_bufinuse);
			bp->b_flags |= B_ERROR;
			IODONE(bp);
		}
		else if(++bp->b_error > ui->ui_maxretries || dkip_induce)
		{
				/* update accounting */
#ifndef NO_STATS
			ui->ui_iotime->io_resp += lbolt - bp->b_start;
#endif
			ui->ui_hardcnt++;

			dkiperror( EMSG_MAXRETRIES, ui, (int) bp->b_dev);
			bp->b_flags |= B_ERROR;
			bp->b_resid = bp->b_bcount;
			bp->b_error = EIO;
			if(iopb->ip_errcode == IPER_NOUNIT) /* drive hosed */
			{
				ui->ui_attached = 0;
			}
			if ((int)bp == ci->ci_bufinuse) {
			  ci->ci_bufinuse = 0;
#ifdef PROFDBG
if(dkipprf)
    printf("Buf wakeup - bp %x dev %x blkno %x inuse %x\n", bp, bp->b_dev, bp->b_blkno, ci->ci_bufinuse);
#endif
			  wakeup(&ci->ci_bufinuse);
			}
			IODONE(bp);
		}
		else
		{
			ui->ui_softcnt++;
			/*
			printf("ipc%dd%d : %x retry: %d blk: %d ", 
			 ci->ci_ctlr, ui->ui_unit, iopb->ip_cmdcode,
			 bp->b_error, bp->b_blkno);
			*/
			dkiperror( EMSG_INRETRY, ui, (int) bp->b_dev);

			bp->av_back = NULL; /* retry whole request */

			/* set block number for sdisksort() */

			pt = &ui->ui_vh.vh_pt[FS(bp->b_dev)];

			bp->b_pblkno = (bp->b_blkno +
			 ((bp->b_bcount - bp->b_resid) >> SCTRSHFT) +
			 pt->pt_firstlbn);

		/*
		 * Put request back in unit Q
		 */
			ui->ui_tab->qcnt++;
#ifdef MEASURE_TIME
dkipinsert(bp, 0x9d9d9d9d);
#endif
			bp->av_forw = NULL;
			tempresid = bp->b_rcyl;
			bp->b_rcyl = bp->b_cyl;
#ifdef IP_DISKSORT
			if( ci->ci_mode & CI_MACSI )
	    		    sdisksort(ui->ui_tab, bp);
			else
#endif
	    		    disksort(ui->ui_tab, bp, ui->ui_lastcyl);
			bp->b_rcyl = tempresid;

		/*
		 * Put Unit Q back on controller Q
		 */
			dkipustart(ui);

		}
		if( nbp)
		{
			bp = nbp;
			goto lbpl;
		}
		/*
		 * We've gotten all the info we need out of the CRB.
		 * Re-enable the controller now.
		 */
		crb->crsw = 0;			/* clear CRBV	*/
		goto mdone;
	}
	else if((crsw_copy & CRSW_EX) &&
		    ((iopb->ip_errcode & IPEX_ICKY) != IPEX_ECC) ) 
	{
			/*
			 * Print out an error message for recovered non-ecc
			 * errors.
			 */
			dkiperror( EMSG_RECOVER, ui, (int) bp->b_dev);
	}

	/* We've gotten all the info we need out of the CRB.  Let's	*/
	/* re-enable the controller now, while we finish processing.	*/
	crb->crsw = 0;				/* clear CRBV		*/

	do
	{
		dkipmiodone( ci, bp );	/* Flush & unmap GBA, return SG list */
		ASSERT(bp->av_back == NULL);
#ifdef WSG_BUG
		if ( (int)bp == ci->ci_bufinuse ){
		  if (bp->b_flags & B_READ){
		    bcopy((caddr_t)K0_TO_K1(ci->ci_buf),
			  (caddr_t)ci->ci_bufva,ci->ci_bufdata);
		  }
		  if( bp->b_resid == 0 ){
		    ci->ci_bufinuse = 0;
#ifdef PROFDBG
if(dkipprf)
    printf("Buf wakeup - bp %x dev %x blkno %x inuse %x\n", bp, bp->b_dev, bp->b_blkno, ci->ci_bufinuse);
#endif
		    wakeup(&ci->ci_bufinuse);
		  }
		}
#endif WSG_BUG

#ifdef PART_GATS
	  	if(bp->b_flags & B_PARTIAL)
		  {
		    bp->b_flags &= ~B_PARTIAL;
		    ui->ui_partial--;
		    
		    /* mark unit structure as no longer busy                 */
		    /* this allows dkipustart() to re-queue the unit struct. */
		    /* onto the ctlr queue                                   */
		    
		    ui->ui_tab->b_active = 0;
		  }
#endif

		/*
		 * If there were a number of writes combined together
		 * in one iopb, walk through the list of bp's and call
		 * IODONE for each.
		 */
		if(bp->b_resid)
		{
			bp->av_back = (struct buf*) bp->b_resid;

			/* set block number for sdisksort() */

			pt = &ui->ui_vh.vh_pt[FS(bp->b_dev)];

			bp->b_pblkno = (bp->b_blkno +
			 ((bp->b_bcount - bp->b_resid) >> SCTRSHFT) +
			 pt->pt_firstlbn);

		/*
		 * Put request back in unit Q
		 */
			ui->ui_tab->qcnt++;
#ifdef MEASURE_TIME
dkipinsert(bp, 0x90909090);
#endif
			nbp = bp->av_forw;
#ifdef PART_GATS
                        if( !(bp->av_forw = ui->ui_tab->b_actf))
                        {
                                ui->ui_tab->b_actl = bp;
                        }
                        ui->ui_tab->b_actf = bp;
#else

			bp->av_forw = NULL;
			tempresid = bp->b_rcyl;
			bp->b_rcyl = bp->b_cyl;
#ifdef IP_DISKSORT
			if( ci->ci_mode & CI_MACSI )
	    		    sdisksort(ui->ui_tab, bp);
			else
#endif
	    		    disksort(ui->ui_tab, bp, ui->ui_lastcyl);
			bp->b_rcyl = tempresid;
#endif

		/*
		 * Put Unit Q back on controller Q
		 */

			dkipustart(ui);

			bp = NULL;		/* exit while loop	*/

		}
		else
		{
#ifndef NO_STATS
				/* update accounting */
			ui->ui_iotime->io_resp += lbolt - bp->b_start;
#endif

			nbp = bp->av_forw;
			IODONE(bp);	/* IODONE stomps on av_forw	*/
			bp = nbp;
		}
	}while(bp);

mdone:
#ifndef NO_STATS
	if( ci->ci_cmdtime > ui->ui_tab->io_start){
	    /* this command was issued to controller while another was active */
	    ui->ui_iotime->io_act += lbolt - ci->ci_cmdtime;
	}else{
	    /* No commmands were pending on controller */
	    ui->ui_iotime->io_act += lbolt - ui->ui_tab->io_start;
	}
	ci->ci_cmdtime = lbolt;
#endif

	/*
	 * If the controller has more commands pending, start them going
	 * if the controller can take them & no-one wants to poll.
	 */

	if (ci->ci_tab->b_actf && !ci->ci_tab->b_active)
	{
		if(ci->ci_pflgs & PF_WANT)
		{
			if(!ci->ci_pending)
				wakeup((caddr_t)ci);
		}
		else
		{
#ifndef NO_STATS
			ci->ci_istart += macsi_cstart(ci);
#else
			macsi_cstart(ci);
#endif
		}
	}
}

/*
 * MACSI iopb enqueue & go routine
 * Called with interrupts dissabled
 * Returns non-zero if Board can't take anymore commands.
 */

macsi_ego(ui)
register DKIPUNITINFO *ui;
{
register DKIPCTLRINFO *ci = ui->ui_ci;
register volatile VSMD_MSHIO *mshio = (VSMD_MSHIO*)(ci->ci_device);
volatile CQE *cqe = &mshio->vsmd_cqe[MACSI_CQE_INDX(ci)];
register struct iobuf *utab;
register struct buf *bp;
int temp;

	utab = ui->ui_tab;

	if(!ci->dkipsg_hd)	/* Make sure free scat/gath */
	{
		return(-1);
	}
	if (!ci->dkip_ioinfo_hd)	/* Make sure free ioinfo entry */
	  	return(-1);
	if(!utab->qcnt)				/* sanity check	*/
	{
		DIAG("macsi_ego called with utab->qcnt = 0\n");
		return(0);
	}

	if(cqe->control & CQE_DKIP_GO)		/* CQE Q full?	*/
	{
#ifndef NOIQEA
		mshio->vsmd_mcsb.iqar |= IQAR_IQEA;	/* int when avail */
		wbflush();
		ci->ci_tab->b_active = 1;		/* ctlr busy now  */
#endif
		return(-1);
	}

	bp = utab->b_actf;		/* get next bp		*/
	utab->b_actf = bp->av_forw;	/* unlink from list	*/
	utab->qcnt--;			/* one less entry	*/
#ifdef MEASURE_TIME
dkipinsert(bp, 0x21212121);
#endif
	if(bp->b_flags & B_CMD)
	{
		dkipsplsetup(ui, bp);	/* special command setup	*/
		/* Use full 32 bit cmdtag to point directly to bp */
		cqe->cmdtag_h = temp = (USHORT)((UINT)bp >> 16);wbflush();
		cqe->cmdtag_l = (USHORT)(bp);			wbflush();
		ASSERT( temp > NUM_DKIP_IOINFO );
	}
	else
	{
		if(bp->av_back)
			bp->b_resid = (uint) bp->av_back;
		else
			bp->b_resid = bp->b_bcount;
		ASSERT(bp->b_resid > 0);
		dkipmSGsetup(ui, bp);	/* MACSI scat/gath setup	*/
		/*
		 * Set high 16 bits of cmdtag to dkip_ioinfo index,
		 * if there was one allocated by dkipSGsetup.  Otherwise,
		 * we must use the bp pointer here too!
		 */
		if (bp->av_back) {
		  cqe->cmdtag_h = temp = (USHORT)
		    ((IOINFO_DKIP_T *)bp->av_back -  ci->dkip_ioinfo);
		  ASSERT( temp <= NUM_DKIP_IOINFO );
		} else {
		  /* Use full 32 bit cmdtag to point directly to bp */
		  cqe->cmdtag_h = temp = (USHORT)((UINT)bp >> 16);wbflush();
		  cqe->cmdtag_l = (USHORT)(bp);			wbflush();
		  ASSERT( temp > NUM_DKIP_IOINFO );
		}
	}
#ifndef NO_STATS
	ui->ui_tab->io_start = lbolt;
#endif

#ifdef PROFDBG
if(dkipprf)
    printf("GO - bp %x\n", bp);
if(dkipprfintr)
    printf("I");
#endif
#ifdef DKDEBUG
dkipsave(mshio);
#endif
#ifdef MEASURE_TIME
dkipinsert(bp, 0x5a5a5a5a);
#endif

	if(ci->ci_mode & CI_SORT)
	{
		cqe->control = (DISK_IOPB_TYPE << 8)|CQE_DKIP_GO|CQE_SORT;
	}
	else
	{
		cqe->control = (DISK_IOPB_TYPE << 8)|CQE_DKIP_GO;
	}
	if(++MACSI_CQE_INDX(ci) >= NUM_CQES)
		MACSI_CQE_INDX(ci) = 0;

	wbflush();
	ci->ci_pending++;	/* one more command out there */
#ifdef PART_GATS
        /* if a partial was done, don't allow any more commands to be queued */

        if(ui->ui_partial)
                return(-1);             /* no more commands     */
#endif
	return(0);					/* all is well	*/
}

/*
 * dkip_gmacsi:	routine to take controller to macsi mode
 *	
 *	svec is used for CQA interrupts
 */

dkip_gmacsi(ci,ipl,nvec,evec)
DKIPCTLRINFO *ci;
unchar ipl, nvec, evec;
{
int gtimeout;
int errflg;
register int x;
register volatile IPIOPB *iopb = (IPIOPB*) &ci->ci_device->ipiopb;
volatile VSMD_MSHIO *mshio = (VSMD_MSHIO*)(ci->ci_device);
volatile VSMD_CIB *cibptr = &mshio->init_info;


	iopb->ip_cmdcode = IP_GMACSI;
	iopb->ip_cmdopt = 0; 					wbflush();
	iopb->ip_memtype = IPMT_16BITI;		/* on board short i/o	*/
	iopb->ip_balo = (ushort)((UINT)cibptr - (UINT)mshio);	wbflush();
	iopb->ip_bahi = 0;
	cibptr->cib_num_cqes = NUM_CQES;			wbflush();
	cibptr->cib_crb_off = CRB_OFFSET;			wbflush();
	cibptr->cib_num_free = NUM_MACSI;			wbflush();
#ifndef NO_OVERLAPPED
	cibptr->cib_m_flags = M_RESTORE|M_OVL_SEEK;		wbflush();
#else
	cibptr->cib_m_flags = M_RESTORE;			wbflush();
#endif
	cibptr->cib_ce_vectr = (ipl << 8) | evec;		wbflush();
	cibptr->cib_cn_vectr = (ipl << 8) | nvec;		wbflush();
	cibptr->cib_num_back = 0;				wbflush();
	cibptr->cib_r_robin = NUM_R_ROBIN;			wbflush();
	cibptr->cib_res2 = 0;					wbflush();
	cibptr->cib_res3 = 0;					wbflush();
	cibptr->cib_res4 = 0;					wbflush();
	cibptr->cib_res5 = 0;					wbflush();
	cibptr->cib_res6 = 0;					wbflush();
	cibptr->cib_res7 = 0;					wbflush();
	cibptr->cib_res8 = 0;					wbflush();
	cibptr->cib_res9 = 0;					wbflush();
	MACSI_CRSW(ci) = 0;					wbflush();
	VSMD_CSR(ci) |= IPCS_GOBUSY;				wbflush();
	DELAY(200);
	for(errflg = 0, gtimeout = 2000; gtimeout--;)
	{
		if(VSMD_CSR(ci) & (IPCS_OPDONE|IPCS_ERLST))
		{
			errflg++;
			break;
		}
		if(MACSI_CRSW(ci) & CRSW_CRBV)
		{
			break;
		}
		DELAY(1000);
	}
	if(gtimeout <= 0 || (MACSI_CRSW(ci) & (CRSW_ER|CRSW_EX)) || errflg)
	{
		/*
		 * reset the board, in case it's in some unknown state.
		 * this has been seen on some of the 4200's
		 */
		dkipreset(ci);
		dkipinitctlr( ci, ci->ci_device, 0,0,1);
		return(-1);
	}

			/* now fill in iopb address in the CQES		*/
			/* also fill in the iopb SIZE (long words)	*/
			/* NOTE: SIZE cannot be filled in if SCSI is used */

	iopb = (volatile IPIOPB*) &mshio->vsmd_iopb[0];
	for(x = 0; x < NUM_CQES; x++)
	{
		mshio->vsmd_cqe[x].iopbaddr = 
		(USHORT)(((UINT)&mshio->vsmd_iopb[x] - (UINT)(mshio)) & 0xffff);
		mshio->vsmd_cqe[x].length = sizeof(IPIOPB)/4;
		iopb->ip_normvec = nvec;
		iopb->ip_errvec = evec;
		iopb++;
	}
#ifdef NOIQEA
	MACSI_IQAR(ci) = 0;
#else
	MACSI_IQAR(ci) = ( (((UINT)(ipl) << 8) | svec) & ~IQAR_IQEA);
#endif
	wbflush();
	MACSI_CQE_INDX(ci) = 0;	/* index into cqe array	*/
	mshio->vsmd_mcsb.mcr = 0;
	wbflush();
	MACSI_CRSW(ci) = 0;		/* clear CRBV		*/
	wbflush();
					/* update ctlr flags		*/
#ifdef FIRMWARE_SORT
#ifndef NO_OVERLAPPED
	ci->ci_mode |= CI_SORT|CI_OVLSEEK|CI_AUTOREST;
#else
	ci->ci_mode |= CI_SORT|CI_AUTOREST;
#endif
#else
#ifndef NO_OVERLAPPED
	ci->ci_mode |= CI_OVLSEEK|CI_AUTOREST;
#else
	ci->ci_mode |= CI_AUTOREST;
#endif
#endif
	return(0);				/* all is well		*/
}
#endif
/*----------------------------------------------------------------------*\
 *			IOCTL assist functions				*
\*----------------------------------------------------------------------*/

/*
 * resource manager for special commands
 *	A call MUST be made to this routine before a special
 *	command can be sent to the board. 
 *
 * This routine will block until the Unit IOPB is free for use.
 * When this routine returns the unit IOPB is free for use.
 * Fill in the iopb and call dkipsplexec
 *
 *	RETURN VALUE:
 *			ptr to iopb to use for special command
 *
 */
IPIOPB *
dkipsplq(ui)
register DKIPUNITINFO *ui;
{
register struct buf *bp;
register int s;

DIAG("dkipsplq....\n");
	bp = &ui->ui_ctab;
	s = splbio();
	while (bp->b_flags & B_BUSY)
	{
		bp->b_flags |= B_WANTED;
		sleep((caddr_t)bp, PRIBIO);
	}
	bp->b_flags = B_BUSY|B_CMD;
	splx(s);
	bp->b_dmaaddr = (caddr_t) &ui->ui_iopb;
	bp->b_dev = MKDEV(ui->ui_ci, ui);	/* Fake up a DEV */
	return((IPIOPB*)bp->b_dmaaddr);
}

/*
 * IOPB resource free (called only on wierd errors)
 *
 */
int
dkipspldq(ui)
register DKIPUNITINFO *ui;
{

	iodone(&ui->ui_ctab); /* simply a done for now	*/
}

/*
 * execute a special command
 *	
 */
int
dkipsplexec(ui)
register DKIPUNITINFO *ui;
{
register struct buf *bp = &ui->ui_ctab;

DIAG("dkipsplexec....\n");
	dkipstrategy(bp);
	iowait(bp);
	bp->b_flags &= ~B_BUSY;
	if (bp->b_flags & B_WANTED)
		wakeup((caddr_t)bp);
	if(bp->b_flags & B_ERROR)
		return(FALSE);
	return(TRUE);
}

/*
 * handle format related commands
 */
int
dkipsplfmt(ui, fmi)
register DKIPUNITINFO *ui;
register struct fmt_map_info *fmi;
{
register volatile IPIOPB *iopb;

	iopb = dkipsplq(ui);		/* get address of iopb	*/

	switch ((int)fmi->fmi_action)
	{
	  case FMI_FORMAT_TRACK:		/* format a track */
		iopb->ip_cmdcode = IP_FORMATTRK;
		iopb->ip_cyl = fmi->fmi_cyl;
		iopb->ip_head = fmi->fmi_trk;
		iopb->ip_balo = FILL_PATTERN;
		break;

	  case FMI_MAP_TRACK:			/* map a track */
		iopb->ip_cmdcode = IP_MAPTRK;
		iopb->ip_cyl = fmi->fmi_cyl;
		iopb->ip_head = fmi->fmi_trk;
		iopb->ip_scnt = fmi->fmi_rplcyl;
		iopb->ip_bahi = (fmi->fmi_rpltrk << 8);
		iopb->ip_balo = FILL_PATTERN;
		break;

	  case FMI_SLIP_SECTOR:			/* slip a sector */
		iopb->ip_cmdcode = IP_MAPSEC;
		iopb->ip_cyl = fmi->fmi_cyl;
		iopb->ip_head = fmi->fmi_trk;
		iopb->ip_sec = fmi->fmi_sec;
		iopb->ip_balo = FILL_PATTERN;
		break;
	  default:
		dkipspldq(ui);		/* remember to release IOPB	*/
		return (FALSE);
	}
	iopb->ip_cmdopt = 0;			/* NO ECC		*/

	return(dkipsplexec(ui));
}
/*
 * handle reiniting a unit
 *
 */
dkipsplconfig(ui)
register DKIPUNITINFO *ui;
{
register volatile IPIOPB *iopb;
register int return_value;

	iopb = dkipsplq(ui);		/* get address of iopb	*/

	dkipconfig(ui, iopb);
	return_value = dkipsplexec(ui);
	dkip_ci_iodone( ui->ui_ci, &ui->ui_uib, sizeof(struct ipuib) );
	return( return_value );
}
/*
 * print controller information
 *
 */
dkipsplprctlr(ci)
register DKIPCTLRINFO *ci;
{
char tbuf[5];

	printf("CONTROLLER: %s\n", dkip_name(ci->ci_hndshk.p_code));

  	printf("ipc%d:prom id ", ci->ci_ctlr);
	if(ci->ci_hndshk.p_var != '0')		/* print if special */
	{
		tbuf[0] = ci->ci_hndshk.p_var;
		tbuf[1] = '.';
		tbuf[2] = NULL;
		printf("%s", tbuf);
	}

	if(ci->ci_hndshk.p_revh == 0x0f)	/* pre-release? */
	{					/* in ASCII	*/
		tbuf[0] = ci->ci_hndshk.p_revl;
		tbuf[1] = ci->ci_hndshk.p_idh;
		tbuf[2] = ci->ci_hndshk.p_idl;
		tbuf[3] = NULL;
		printf("%s", tbuf);
	}
	else
	{
  		printf("%x%x%x", ci->ci_hndshk.p_revl,
			ci->ci_hndshk.p_idh,
			ci->ci_hndshk.p_idl);
	}
  	printf(" (%x/%x/%x)",
	 ci->ci_hndshk.p_month, ci->ci_hndshk.p_day,
	 (ci->ci_hndshk.p_year & 0x0ff));

	if (!ci->ci_sgOK)
		printf(", no scatter gather");
	if (!ci->ci_cacheOK)
		printf(", no cache");
	if(ci->ci_mode & CI_MACSI)
	{
		printf(", MACSI");
		if(ci->ci_mode & CI_OVLSEEK)
			printf(", overlapped seeks");
		if(ci->ci_mode & CI_SORT)
			printf(", sorting");
		if(ci->ci_mode & CI_AUTOREST)
			printf(", auto restore");
	}
	if(ci->ci_mode & CI_4UNIT)
	{
		printf(", Four-Unit mode");
	} else {
		printf(", Two-Unit mode");
	}
	printf("\n");
}
#ifdef DIOCGETCONFIG
/*
 * get current UIB setting
 *
 */
dkipsplgetconfig(ui, arg)
register DKIPUNITINFO *ui;
caddr_t arg;
{
register volatile IPIOPB *iopb;
ioaddr_t io_addr;

	
	dkip_ci_ioinit( ci, &ui->ui_uib, sizeof(struct ipuib), &io_addr);
	iopb = dkipsplq(ui);	/* get iopb */

	iopb->ip_cmdcode = IP_GETCONFIG;
	iopb->ip_bahi = HI16(io_addr);
	iopb->ip_balo = LO16(io_addr);
	iopb->ip_memtype = IPMT_16BIT;
	iopb->ip_addrmod = VME_A32NPAMOD;
	if(dkipsplexec(ui) == FALSE) {
		dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );
		return(FALSE);
	      }
	dkip_ci_iodone( ci, &ui->ui_uib, sizeof(struct ipuib) );
	

	if(!arg)		/* if no pointer, then print info	*/
	{

		printf("v0sh 0x%x, v0nh 0x%x, v1sh 0x%x, v1nh 0x%x\n",
		 ui->ui_uib.ipu_v0sh, ui->ui_uib.ipu_v0nh,
		 ui->ui_uib.ipu_v1nh, ui->ui_uib.ipu_v1nh);

		printf("sectrk %d, skew 0x%x, byte/sech 0x%x, byte/secl 0x%x\n",
		ui->ui_uib.ipu_sectrk, ui->ui_uib.ipu_skew,
		ui->ui_uib.ipu_bytsechi, ui->ui_uib.ipu_bytseclo);

		printf("gap1 0x%x, gap2 0x%x, intrlv 0x%x, nretries %d\n",
		ui->ui_uib.ipu_gap1, ui->ui_uib.ipu_gap2, ui->ui_uib.ipu_intrlv,
		ui->ui_uib.ipu_retries);

printf("cylhi 0x%x, cyllo 0x%x, attrib 0x%x, alt_attrib 0x%x ipl %d, vec 0x%x\n",
		ui->ui_uib.ipu_cylhi, ui->ui_uib.ipu_cyllo,
		ui->ui_uib.ipu_attrib, ui->ui_uib.ipu_mbz,
		ui->ui_uib.ipu_statipl, ui->ui_uib.ipu_statvec);
	}
	return(TRUE);
}
#endif
dkcpy(from, to, cnt)
volatile ushort *from, *to;
int cnt;
{
	while(cnt--)
		*to++ = *from++;
}
/* Copyright 1986, Silicon Graphics Inc., Mountain View, CA. */
/*
 *  Misc subroutines used by disk drivers
 */
#ifdef XXXXXVVV
#include "sys/param.h"
#include "sys/types.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/dvh.h"

#endif

#ifdef IP_DISKSORT
/*
 * Seek sort for disks.  The driver has already decomposed the disk address
 * into b_cyl.
 *
 * The argument dp structure holds a av_forw activity chain pointer
 * on which we keep two queues, sorted in ascending cylinder order.
 * The first queue holds those requests which are positioned after
 * the current cylinder (in the first request); the second holds
 * requests which came in after their cylinder number was passed.
 * Thus we implement a one way scan, retracting after reaching the
 * end of the drive to the first request on the second queue,
 * at which time it becomes the first queue.
 *
 * A one-way scan is natural because of the way UNIX read-ahead
 * blocks are allocated.
 */
sdisksort(dp, bp)
	register struct iobuf *dp;
	register struct buf *bp;
{
	register struct buf *ap;

	/*
	 * If nothing on the activity queue, then
	 * we become the only thing.
	 */
	ap = dp->b_actf;
	if(ap == NULL) {
		dp->b_actf = bp;
		dp->b_actl = bp;
		bp->av_forw = NULL;
		return;
	}
	/*
	 * If we lie after the first (currently active)
	 * request, then we must locate the second request list
	 * and add ourselves to it.
	 */
	if (bp->b_cyl < ap->b_cyl) {
		while (ap->av_forw) {
			/*
			 * Check for an ``inversion'' in the
			 * normally ascending cylinder numbers,
			 * indicating the start of the second request list.
			 */
			if (ap->av_forw->b_cyl < ap->b_cyl) {
				/*
				 * Search the second request list
				 * for the first request at a larger
				 * cylinder number.  We go before that;
				 * if there is no such request, we go at end.
				 */
				do {
					if (bp->b_cyl < ap->av_forw->b_cyl)
						goto insert;
					ap = ap->av_forw;
				} while (ap->av_forw);
				goto insert;		/* after last */
			}
			ap = ap->av_forw;
		}
		/*
		 * No inversions... we will go after the last, and
		 * be the first request in the second request list.
		 */
		goto insert;
	}
	/*
	 * Request is at/after the current request...
	 * sort in the first request list.
	 */
	while (ap->av_forw) {
		/*
		 * We want to go after the current request
		 * if there is an inversion after it (i.e. it is
		 * the end of the first request list), or if
		 * the next request is a larger cylinder than our request.
		 */
		if (ap->av_forw->b_cyl < ap->b_cyl ||
		    bp->b_cyl < ap->av_forw->b_cyl)
			goto insert;
		ap = ap->av_forw;
	}
	/*
	 * Neither a second list nor a larger
	 * request... we go at the end of the first list,
	 * which is the same as the end of the whole schebang.
	 */
insert:
	bp->av_forw = ap->av_forw;
	ap->av_forw = bp;
	if (ap == dp->b_actl)
		dp->b_actl = bp;
}
#endif

whichctlr( ip )
register volatile struct ipdevice *ip;
{
	register int i;
	for( i = 0; i < dkipctlrs; i++){
		if( dkipctlrptr[i] && ( ip == dkipctlrptr[i]->ci_device))
			break;
	}
	return(i);
}
#ifdef DKDEBUG
/*
 * Call for dbx, since it doesn't do short accesses from the controller
 */
dkipsave(ip)
	register volatile VSMD_MSHIO *ip;
{
	
	dkcpy(ip,&ipsave,WSIZ(VSMD_MSHIO));
}
dkipiopbsave(ip)
	register volatile struct ipiopb *ip;
{
	
	dkcpy(ip,&iopbsave,WSIZ(struct ipiopb));
}
#endif
#ifdef MEASURE_TIME
dkipinsert( bp,what ) 
struct buf *bp;
unsigned int what;
{ 
    if( dkip_printit != 0 && CTLR(bp->b_dev) == dkip_printit){
	dkip_trigger = 1;
	cmn_err(CE_CONT, "(%x: %x|%x|%x)\n", bp->b_dev, bp, bp->b_blkno, what);
        (dkip_lindx >= 1024) ? dkip_lindx = 0 : dkip_lindx++;
    }else{
        dkiplist[dkip_lindx].bp = bp; 
        dkiplist[dkip_lindx].time = lbolt; 
        dkiplist[dkip_lindx].which = what;
        (dkip_lindx >= DKIPLIST_SIZE) ? dkip_lindx = 0 : dkip_lindx++;
    }
 }
#endif
#ifndef STANDALONE
extern struct devtable *Devboot;
extern struct devtable Dev_dkip[];
int has_dkip()
{
	Devboot = Dev_dkip;
	return(1);
}
#endif

/*
 * Returns a string of the form :
 *
 *	ipc0 : Interphase Cheetah 4200 Rev 05A
 */
dkipgetctlr(ci, ct)
register DKIPCTLRINFO *ci;
struct ctlr_info *ct;
{
char tbuf[6];


	strcpy( ct->ci_type, "ipc");
	dkip_btoa( ci->ci_ctlr, tbuf, 16);
	strcat( ct->ci_type, tbuf);
	strcat( ct->ci_type, " : ");
	strcat( ct->ci_type, dkip_name(ci->ci_hndshk.p_code));
	strcat( ct->ci_type, " Revision (");
	if(ci->ci_hndshk.p_var != '0')		/* print if special */
	{
		tbuf[0] = ci->ci_hndshk.p_var;
		tbuf[1] = '.';
		tbuf[2] = NULL;
	}
	strcat( ct->ci_type, tbuf);

	if(ci->ci_hndshk.p_revh == 0x0f)	/* pre-release? */
	{					/* in ASCII	*/
		tbuf[0] = ci->ci_hndshk.p_revl;
		tbuf[1] = ci->ci_hndshk.p_idh;
		tbuf[2] = ci->ci_hndshk.p_idl;
		tbuf[3] = NULL;
		strcat( ct->ci_type, tbuf);
	}
	else
	{
		dkip_btoa( ci->ci_hndshk.p_revl, tbuf, 16);
		strcat( ct->ci_type, tbuf);
		dkip_btoa( ci->ci_hndshk.p_idh, tbuf, 16);
		strcat( ct->ci_type, tbuf);
		dkip_btoa( ci->ci_hndshk.p_idl, tbuf, 16);
		strcat( ct->ci_type, tbuf);
	}
	strcat( ct->ci_type, ") ");
	dkip_btoa( ci->ci_hndshk.p_month, tbuf, 16);
	strcat( ct->ci_type, tbuf);
	strcat ( ct->ci_type, "/");
	dkip_btoa( ci->ci_hndshk.p_day, tbuf, 16);
	strcat( ct->ci_type, tbuf);
	strcat ( ct->ci_type, "/");
	dkip_btoa( ci->ci_hndshk.p_year, tbuf, 16);
	strcat( ct->ci_type, tbuf);

	if(ci->ci_mode & CI_MACSI)
	{
		strcat( ct->ci_type, ", MACSI enabled");
	}
}
dkip_btoa(n, str, base)
        u_char n;
        char *str;
{
        char prbuf[11];
        register char *cp;

        cp = prbuf;
	if( base == 16 ){
            do {
                *cp++ = "0123456789ABCDEF"[n%16];
                n >>= 4;
            } while (n);
	
	} else{
            do {
                *cp++ = "0123456789"[n%base];
                n /= base;
            } while (n);
	}

        do {
                *str++ = *--cp;
        } while (cp > prbuf);
	*str = NULL;
        return;
}

dkip_resetbootctlr()
{

    struct dkipctlrinfo *ci;
    int ctlr = CTLR(rootdev);
    int i;

    /*
     * Make sure rootdev is a dkip ctlr
     */
    for( i = 0; i < dkipmajors; i++)
	if( major(rootdev) == dkiparray[i] )
	    break;
    if( i == dkipmajors )
	return;
     
    ci = dkipctlrptr[ctlr];
    dkipreset(ci);
    dkipinitctlr( ci, ci->ci_device, 0,0,1);
}

dkipinitctlr( ci, ip, ipl, vec, reinit )
struct dkipctlrinfo *ci;
struct ipdevice *ip;
int ipl, vec;
int reinit;
{
        volatile struct ipsg *sg;
        volatile struct ipiopb *iopb;
        volatile struct ipiopb *nextiopb;
	int i, j;
	ioaddr_t io_address;

	/*
	 * Initialize the iopbs and scatter gather structs on the
	 * board (in the vme i/o space).  We have to explicitly zero
	 * things here, because the i/o space is not bss.
	 */
	dkip_ci_ioinit(ci, &ci->ci_iopb[0], sizeof(ci->ci_iopb),&io_address);

	/* 
	 * If reinit is set, the controller is being reset.  That
	 * means we already know whether or not the board can support
	 * block mode, and if so, we can set the iopbs up correctly
	 * the first time instead of having to go back and reinitialize
	 * them.
	 */
	iopb = &ip->ipiopb;
	if( !reinit ){
	    iopb->ip_ipl = ipl;					wbflush();
	    iopb->ip_normvec = vec;				wbflush();
	    iopb->ip_errvec = vec;				wbflush();
	}
	iopb->ip_dmaburst = 0;					wbflush();
	iopb->ip_pbmemtype = IPMT_32BIT;			wbflush();
	if( reinit ){
	    iopb->ip_pbaddrmod = ci->ci_addrmod;		wbflush();
	}else{
	    iopb->ip_pbaddrmod = VME_A32NPAMOD;			wbflush();
	}

	iopb->ip_iopblo = LO16(io_address);			wbflush();
	iopb->ip_iopbhi = HI16(io_address);			wbflush();
	iopb->ip_skew = 0;					wbflush();
	iopb->ip_sgentries = 0;					wbflush();
	sg = &ip->ipsg[0];
	for (i = IP_MAXSG; --i >= 0; sg++)
	{
		sg->sg_count = 0;				wbflush();
		sg->sg_addrlo = 0;				wbflush();
		sg->sg_addrhi = 0;				wbflush();
		sg->sg_meminfo = IPMT_32BIT;			wbflush();
		sg->sg_addrmod = VME_A32NPAMOD;			wbflush();
	}

	/* init iopb structs */
	iopb = (volatile struct ipiopb *)K0_TO_K1(&ci->ci_iopb[0]);
	for (j = IP_MAXIOPBS; --j >= 0; )
	{
		if( !reinit ){
		    iopb->ip_ipl = ipl;
		    iopb->ip_normvec = vec;
		    iopb->ip_errvec = vec;
		}
		iopb->ip_memtype = IPMT_32BIT;
		iopb->ip_addrmod = VME_A32NPAMOD;
		if (j)
		{
			/* link this iopb to the next one */
			nextiopb = iopb + 1;
			/*
			 * Get io_address usable by controller on I/O bus.
			 * Note that nextiopb is a K1 address rather than
			 * the usual K0 address.
			 */
			dkip_ci_ioinit(ci, nextiopb, sizeof(struct ipiopb),
				       &io_address);
			iopb->ip_pbmemtype = IPMT_32BIT;
			if( reinit ){
	    	 	    iopb->ip_pbaddrmod = ci->ci_addrmod; wbflush();
			}else{
	    		    iopb->ip_pbaddrmod = VME_A32NPAMOD;	wbflush();
			}
			iopb->ip_iopblo = LO16(io_address);
			iopb->ip_iopbhi = HI16(io_address);
			iopb = nextiopb;
		}
	}
	wbflush();
}

dkipreinitiopbs( ci, ip)
struct dkipctlrinfo *ci;
struct ipdevice *ip;
{
        volatile struct ipsg *sg;
        volatile struct ipiopb *iopb;
        volatile struct ipiopb *nextiopb;
	int i,j;

	/* If the controller and memory system can do block mode,
	 * go back and change all of the address modifiers in iopb's
	 * and scatter/gather structures.
	 * This should really be done right the first time, but it is
	 * cheaper in people cycles to make the mod this way and get
	 * a working driver.
	 */
	if (ci->ci_addrmod == VME_A32NPBMOD) {
		iopb = &ip->ipiopb;
		iopb->ip_pbaddrmod = ci->ci_addrmod;		wbflush();
		sg = &ip->ipsg[0];
		for (i = IP_MAXSG; --i >= 0; sg++) {
			sg->sg_addrmod = ci->ci_addrmod;	wbflush();
		}
		/* init iopb structs */
		iopb = (volatile struct ipiopb *)K0_TO_K1(&ci->ci_iopb[0]);
		for (j = IP_MAXIOPBS; --j >= 0; ) {
			iopb->ip_addrmod = ci->ci_addrmod;
			if (j) {
				/* link this iopb to the next one */
				nextiopb = iopb + 1;
				iopb->ip_pbaddrmod = ci->ci_addrmod;
				iopb = nextiopb;
			}
		}
		wbflush();
	}
}
static int 
dkip_dev_busy(dev)
dev_t dev;
{
    int partition = FS(dev);
    struct dkipunitinfo *ui;
    register int ctlr;
    int i;

    ctlr = CTLR(dev);
    ui = &dkipctlrptr[ctlr]->ci_unit[UNIT(dev)];

    if( ui->ui_open[partition] > 1 )
	/* This partition should only be opened once */
	return(1);
    if( dev == swapdev)
	return(1);
    /*
     * If this is the entire volume, make sure that none of the
     * file systems are mounted.
     */
    if( ui->ui_vh.vh_pt[partition].pt_type == PTYPE_VOLUME ){
	for( i = 0;i < 16; i++ ){
	    if( ui->ui_open[i] == 0 )
		continue;
	    if( i == partition && ui->ui_open[i] <= 1 )
		continue;
	    return(1);
	}
    }
    return(0);
}
