/* ------------------------------------------------------------------ */
/* | Copyright Unpublisheed, MIPS Computer Systems, Inc.  All Rights | */
/* | Reserved.  This software contains proprietary and confidential | */
/* | information of MIPS and its suppliers.  Use, disclosure or     | */
/* | reproduction is prohibited without the prior express written   | */
/* | consent of MIPS.                                               | */
/* ------------------------------------------------------------------ */
/* $Header: if_egl.c,v 1.21 90/11/14 18:58:09 chungc Exp $ */
/*
 * Interphase Eagle Link Level Ethernet Interface Driver
 *
12/09/87	Manlio D. Marquez
01/04/88	Start putting in queue mode instead of MCE only
02/01/88	Make one generic (combined) driver for berkeley style
			systems.  Currently includes SUN and ISI.
03/29/88	Added RCS definitions. jhd
04/07/88	Port to MIPS M/SERIES.  Assumes that Short/IO allows for
		a16/d32 transfers, board based at 0x4000 in a16 space.
08/15/88	Start Move to RISC/os (UMIPS system V).
11/16/88	Start incorporating MIPS standalone code.

 ***			Copyright (C) 1987, Interphase Corporation
 ***            Any use, copy or alteration is strictly prohibited
 ***            and gosh darn awfully -- morally inexcusable
 ***            unless authorized by Interphase.
 ***/

#define	SYSVR2		/* system V release 2 */
#ifdef STANDALONE
#define ECHO		/* Stops Driver recognising packets destined */
			/* for itself so an echo server can be used */
#else
#undef ECHO
#endif STANDALONE

#define NEGL	4	/* Maximum number of eagles supported */

#ifdef STANDALONE

#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/vmereg.h"
#include "sys/errno.h"
#include "sys/param.h"
#include "netinet/in.h"
#include "saio/saio.h"
#include "saio/saioctl.h"
#include "saio/socket.h"
#include "saio/debug.h"
#include "saio/arp.h"
#include "saio/ei.h"
#include "saio/mbuf.h"
#include "netinet/in_systm.h"
#include "netinet/ip.h"
#include "machine/if_IPtypes.h"						
#include "machine/if_egl.h"  					

#define m_freem		_m_freem
#define m_free		_m_freem
#define m_get		_m_get
#define cei(x)	((struct ether_info *)(x->i_ino_dir))
#define panic(a)	{ printf(a); exit(-1); }
#define log(pri,fmt,args) printf(fmt,args)

#define Mvtop(addr)	(addr)	/* No VM in STANDALONE */
#define VtoK1(addr)	(addr)
#define MGET(m, flag1, flag2)	m = m_get()

extern _arpinput(), _arpresolve();
extern int Debug;

static EGL_SHIO *eglstd[] = { (EGL_SHIO *)0x4000, (EGL_SHIO *)0x4800,
			      (EGL_SHIO *)0x5000, (EGL_SHIO *)0x5800 };

#define	NCTLRS	(sizeof(eglstd)/sizeof(eglstd[0]))
#define CQEBUSYWAIT    2000000
#define CE_PANIC	3
#define IOBADADDR	badaddr     

#else /*STANDALONE*/

#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/mbuf.h"
#include "sys/buf.h"						
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/uio.h"
#include "sys/edt.h"
#include "sys/sysmacros.h"
#include "sys/immu.h"
#include "sys/debug.h"
#include "sys/cpu_board.h"
#include "sys/vmereg.h"     
#include "bsd43/sys/syslog.h"

#include "../net/soioctl.h"

#include "../net/if.h"
#include "../net/netisr.h"
#include "../net/route.h"

#ifdef INET
#include "../netinet/in.h"
#include "../netinet/in_systm.h"
#include "../netinet/in_var.h"
#include "../netinet/ip.h"
#include "../netinet/if_ether.h"
#endif INET

#ifdef NS
#include "../netns/ns.h"
#include "../netns/ns_if.h"
#endif NS

#include "sys/if_IPtypes.h"						
#include "sys/if_egl.h"  					


#include "../sys/time.h"

extern struct timeval boottime;

#define	if_addr	if_addrlist->ifa_addr
#define Mvtop(addr)	(ctob(kvtokptbl(addr)->pgm.pg_pfn) | \
			 ((int)addr & POFFMASK))
#endif /* STANDALONE */

#define QSTARTTIMEOUT  200000	/* allow q-mode time to start */
#define EGLWATCHINTVL	20	/* call eglwatch every x secs */
#define EGLQWATCH	5*HZ	/* start queue mode timer */
#define EGLTWATCH	5*HZ	/* transmit interrupt timer */
#define NOINT		0	/* don't want interrupt */
#define INT		1	/* want interrupt */

#define EGL_TOPT	((TT_NORMAL<< 10) | 		/* Normal */\
			 (MEMT_32BIT << 8) | 		/* 32 bits */\
			  ADRM_EXT_N_D)			/* AM == 09 */

#define EGL_TOPT_BLK	((TT_BLOCK<< 10) | 		/* Block Mode */\
			 (MEMT_32BIT << 8) | 		/* 32 bits */\
			  ADRM_EXT_BLK)			/* AM == 0B */
#ifdef	R6000_FAKEIO
#undef	EGL_TOPT
#undef	EGL_TOPT_BLK
#define EGL_TOPT	((TT_NORMAL<< 10) | 		/* Normal */\
			 (MEMT_32BIT << 8) | 		/* 32 bits */\
			  ADRM_STD_N_D)			/* AM == 39 */

#define EGL_TOPT_BLK	((TT_BLOCK<< 10) | 		/* Block Mode */\
			 (MEMT_32BIT << 8) | 		/* 32 bits */\
			  ADRM_STD_BLK)			/* AM == 3B */
#endif

#ifdef STANDALONE
int	eglattach(), eglinit();
int	egloutput(), eglreset();
int	_eglopen(), _eglstrategy(), _eglinit();
struct mbuf *dtom();
#else
int	egledtinit(),
	eglnest(),
	eglwatch(),
	eglqdog(), 
	egltdog(), 
	egltint(),			/* transmit interrupt */
	eglteint(),			/* transmit error interrupt */
	eglrint(),			/* receive interrupt */
	eglreint(),			/* receive error interrupt */
       	eglqint(),			/* queue entry avail interrupt */
	egl_init(),
	egloutput(),
	eglioctl(),
	eglreset();
/*	eglbtint();	*/
#endif  STANDALONE
/*
Parameters for controlling the eagle
*/
#define EGL_MTU		ETHERMTU+18	/* Max Packet size, 1500+14+4 */
#define NUM_CQE		MAX_CQE		/* Num Command Queue Entries */

/* Next two MUST be power of 2 */
#define EGL_RX_NRINGS	16		/* Num LANCE Rx Rings */
#define EGL_TX_NRINGS	16		/* Num LANCE Tx Rings */
#define EGL_RX_BUFSZ	2000		/* Max Rx packet */
#define EGL_TX_BUFSZ	2000		/* Max Tx packet */
#define EGL_RX_NBUFF	20		/* Num receive buffers (max possible) */
#define EGL_RX_SLOTS	20		/* Num receive workq entries */
#define EGL_TX_NBUFF	16		/* Num transmit buffers */
#define EGL_TX_SLOTS	16		/* Num transmit workq entries */
#define EGL_RX_IOPBF	0		/* Receive IOPB starting offset */
#define EGL_TX_IOPBF	EGL_RX_IOPBF+EGL_RX_NBUFF
#define EGL_MS_SLOTS	1		/* Num misc workq entries */
#define EGL_DM_SLOTS	1		/* Num dma workq entries */
#define EGL_RX_PRIORITY	1		/* Receive workq priority */
#define EGL_TX_PRIORITY	1		/* Transmit workq priority */
#define EGL_MS_PRIORITY	1		/* misc workq priority */
#define EGL_DM_PRIORITY	1		/* dma workq priority */
#define EGL_RX_WDIV	0		/* Num receive workq processed */
#define EGL_TX_WDIV	0		/* Num transmit workq processed */
#define EGL_MS_WDIV	0		/* Num misc workq processed */
#define EGL_DM_WDIV	0		/* Num dma workq processed */
#define EGL_NHBUF	1		/* Num Host Managed Buffers */
#define EGL_NIBUF	16		/* Num Internal Tx Buffers */
#define EGL_BURST	0		/* Send Whole Message */

#define TXINC(x) ((x) = (ULONG)(++x&(EGL_TX_NBUFF-1)))
#define TXDEC(x) ((x) = (ULONG)(--x&(EGL_TX_NBUFF-1)))
#define TXFULL(x) ((x) == EGL_TX_NBUFF)

UWORD eglwait();
struct mbuf *m_get();
/*
 * Since we need to remember the mapping from virtual to physical and back,
 * we keep these structures on each buffer we point the eagle at.
 * If we point it at an mbuf chain, we only need to get back to the first
 * one.
 */
struct vp {
	struct mbuf *virtual;
	ULONG physical;
	ULONG iopb;
	sah_type io_sah;	/* I/O system map area descriptor */
	ioaddr_t ioaddr;	/* I/O bus address */
};
/*
 * Ethernet software status per interface.
 *
 * Each interface is referenced by a network interface structure, egl_if,
 * which the routing code uses to locate the interface.  This structure 
 * contains the output queue for the interface, its address, ... 
 */
struct  egl_softc {
        struct arpcom	egl_ac;		/* Ethernet Common Part	*/
	BYTE		 enaddr_set;	/* Has the ethernet address been set by an ioctl */
	EGL_SHIO	*egl_shio;	/* Ptr to I/O Space	*/
	EGL_IMOD	egl_IMOD;	/* Current Physical Interface Mode*/
	BYTE		egl_PHY[6];	/* Current Physical Address	*/
	BYTE		egl_FILT[8];	/* Current Logical Address Filter*/
	BYTE		laf_set;	/* Has the LAF been set by an ioctl */
	EGL_VECT	egl_t_vect;	/* Transmit Interrupt Vector	*/
	EGL_VECT	egl_te_vect;	/* Transmit Error Interrupt Vector*/
	EGL_VECT	egl_r_vect;	/* Receive Interrupt Vector	*/
	EGL_VECT	egl_re_vect;	/* Receive Error Interrupt Vector*/
	EGL_VECT	egl_qav_vect;	/* Queue Available Interrupt Vector */
	struct vp	egl_tbinfo[EGL_TX_NBUFF]; /* phys addrs of txbufs */
	struct vp	egl_rbinfo[EGL_RX_NBUFF]; /* phys addrs of rxbufs */
	ULONG		egl_txadd;	/* add index to xmit packet ring */
	ULONG		egl_txrem;	/* remove index to xmit packet ring */
	EGL_CQE		*egl_cqe;	/* index for feed queue	*/
	ULONG		egl_qmode;	/* queue mode is started */
	ULONG		egl_qfull;	/* feed queue is full */
	ULONG		egl_configured;	/* We're ready to go */
	int		egl_tx_timerid;
	int		egl_qa_timerid;
	ULONG 		egl_txcnt; 	/* # of mbufs queued in tbuf array */
	ULONG		egl_quecnt;	/* # of xmit cmds queued on eagle */
	int 		egl_isopen;	/* used for STAND-ALONE */
	ULONG		egl_csr0miss;	/* driver count of rx misses */
	ULONG		egl_csr0mem;	/* driver count of lance memory errs */
	csh_type	egl_csh;	/* GBA cache section descriptor */
	
} egl_softc[NEGL];

#define egl_if           egl_ac.ac_if     /* network-visible interface	*/
#define egl_enaddr       egl_ac.ac_enaddr /* hardware Ethernet address	*/

/* 
 * Macros to set various bits in Queue Entry Control Register (QECR)
 * of the Command Queue Entry (CQE).
 */
/* tell egl to initiate action on this cqe */
#define CQE_GO( qecr )		{ wbflush(); (W( qecr ) = M_QECR_GO); }
/* check if cqe is available */
#define CQE_BUSY( qecr )	(W( qecr ) & M_QECR_GO)
/* clear cqe GO bit; when should host be doing this??? */
#define CQE_CLR_BUSY( qecr )	{ wbflush(); (W( qecr ) &= ~M_QECR_GO); }
/* indicate that any commands that are queued up should be aborted on error */
#define CQE_AA_GO( qecr ) { wbflush(); (W( qecr ) |= (M_QECR_GO + M_QECR_AA)); }

/* 
 * Get the next command queue entry in the command queue 
 */
#define INC_CQE( cqe ) (((x)==&shio->sh_CQE[NUM_CQE-1]?&shio->sh_CQE[0]?cqe+1))

/*
 * Macros to check/set bits in the Command Response Status (CRSW) field of the
 * Command Response Block (CRB).
 */
/* clear the crsw */
#define CRB_CLR_DONE( crsw )	 { (W( crsw ) = 0); wbflush(); }
/*#define CRB_CLR_DONE( crsw )	 { (W( crsw ) &= ~M_CRSW_CRBV); wbflush(); }*/
/* Check if command is done */
#define CRB_DONE( crsw )	 (W( crsw ) & M_CRSW_CRBV)
/* Check is the command represented by this CRB is done. */
#define CRB_COMPLETE( crsw )	 (W( crsw ) & (M_CRSW_CRBV|M_CRSW_CC))
/* Check if the command to start queue mode has completed. */
#define CRB_QSTARTED( crsw )	(W( crsw ) & (M_CRSW_CRBV|M_CRSW_QMS))
/* Check if this command queue entry is available. */
#define CRB_QAVAIL( crsw )	(W( crsw ) & (M_CRSW_CRBV|M_CRSW_QEA))

#ifndef STANDALONE
void egl_delay1(), egl_delay2();
#else
static int egl_rxblock;	/* relies on BSS being 0'ed */
static int egl_txblock;	/* relies on BSS being 0'ed */
static int egl_txdone;		/* relies on BSS being 0'ed */
#endif  STANDALONE


/*
 * Setup an mbuf cluster for I/O.  This includes flushing the CPU cache
 * as well as setting up an I/O mapping.
 */

ioaddr_t
egl_iosetup( egl, iomap, mbuffer, read_op )
struct egl_softc *egl;
struct vp *iomap;
struct mbuf *mbuffer;
int read_op;
{
  ULONG flush_addr, flush_len, bcount;
  
  if (iomap->io_sah) {		/* Buffer is already setup */
    ASSERT( mbuffer == iomap->virtual );
#ifndef STANDALONE
    ASSERT( iomap->physical == Mvtop(mtod(mbuffer, char *)));
#else
    ASSERT( iomap->physical == K1_TO_PHYS(mtod(mbuffer, char *)));
#endif STANDALONE
  }
  else {			/* Need to perform the setup */
    iomap->virtual = mbuffer;
    
    /*	Translate an mbuf pointer into the physical address of its buffer
     *	area.  Assumes page size cluster mbufs.		*/
    
#ifndef STANDALONE
    iomap->physical = Mvtop(mtod(mbuffer, char *));
#ifdef	R6000    
    flush_len = mbuffer->m_len;
    flush_addr = mtod(mbuffer, ULONG);	/* virtual addr of data */
    while (flush_len > 0) {	/* Must flush a page at a time */
      bcount = min(NBPC-(flush_addr & (NBPC-1)), flush_len);
      writeback_cache( PHYS_TO_K0(Mvtop(flush_addr)), bcount);
      flush_len -= bcount;
      flush_addr += bcount;
    }
#else    
    clean_dcache(PHYS_TO_K0(iomap->physical), mbuffer->m_len );
#endif	R6000    
#else
    iomap->physical = K1_TO_PHYS(mtod(mbuffer, char *));
    clear_cache( PHYS_TO_K0(iomap->physical), mbuffer->m_len );
#endif STANDALONE
    

    if (!vme_iomap( egl->egl_csh, PHYS_TO_K0(iomap->physical),
		      mbuffer->m_len,
#ifdef	R6000_FAKEIO
		      ((read_op) ? GBA_MAP_WRITE : GBA_MAP_READ) |
#endif	R6000_FAKEIO		   
		      GBA_CONTIG_ADDR | GBA_NOPART_MAP,
		      &iomap->io_sah,
		      &iomap->ioaddr))
      
      cmn_err( CE_PANIC, "Can't map mbuf into GBA\n");
  }
  
  return( iomap->ioaddr );
}

/*
 * Cleanup a mapped I/O area when I/O operation is complete.  In particular,
 * flush any data in the I/O cache.  If read_op, then invalidate CPU cache
 * so caller can access the new data from the controller.
 */

egl_iodone(iomap, read_op)
struct vp *iomap;
int read_op;
{
  ULONG flush_addr, flush_len, bcount;
  
  ASSERT(iomap->io_sah);
  if (!vme_iounmap( iomap->io_sah ))
    cmn_err(CE_PANIC, "Can't flush/unmap buffer from GBA\n");

  /*
   * We need to invalidate the CPU cache since the start and end of an mbuf
   * may occupy the same secondary cache line as other data.  Even if we flush
   * and invalidate before performing I/O, other data in the line may have been
   * read, causing the old data to be placed back in the cache.  We then will
   * see stale data after the I/O completes (this occurred in an SA system
   * where the mbuf header is in the same secondary cache line as the data --
   * and we end up reading mbuf->m_len after invalidating the CPU cache).
   */
   
#ifndef STANDALONE
  if (read_op)
#ifdef	R6000
    {
    flush_len = iomap->virtual->m_len;
    flush_addr = mtod(iomap->virtual, ULONG);	/* virtual addr of data */
    while (flush_len > 0) {	/* Must flush a page at a time */
      bcount = min(NBPC-(flush_addr & (NBPC-1)), flush_len);
      invalidate_virt_dcache( flush_addr, bcount );
      invalidate_scache( PHYS_TO_K0(Mvtop(flush_addr)),
			bcount);	/* write back & invalidate */
      flush_len -= bcount;
      flush_addr += bcount;
    }
  }
#else  
    clean_dcache(PHYS_TO_K0(iomap->physical), iomap->virtual->m_len );
#endif	R6000  
#else
  clear_cache( PHYS_TO_K0(iomap->physical), iomap->virtual->m_len );
#endif STANDALONE
  
  iomap->physical = NULL;
  iomap->virtual = NULL;
  iomap->io_sah = 0;
  iomap->ioaddr = 0;
}

/* probe and attach a single board
 */
#ifdef STANDALONE
eglattach(io)
struct iob *io;
#else
if_egledtinit(edtp)
register struct edt *edtp;
#endif STANDALONE
{
	register u_int unit;
	register struct egl_softc *egl;
	register struct vme_intrs *intp;
	register EGL_SHIO *addr;
	int vector;

#ifndef STANDALONE
	if (0 == (intp = edtp->e_intr_info)
	    || NEGL <= (unit = intp->v_unit) ) {
		log(BSD43_LOG_ERR,"egl%d: bad egl EDT entry\n", unit);
		return(-1);
	}
	addr = (EGL_SHIO*)PHYS_TO_K1(edtp->e_base);
	if (IOBADADDR(&addr->sh_MCSB, sizeof(short))) {
		if (showconfig)
#else
	unit = io->i_ctlr;	/* controller number */
	addr = (EGL_SHIO*)(IS_R6300 ?
		   find_r6000_controller( eglstd[unit], 0, sizeof(short))
		   : PHYS_TO_K1(VMESA16_TO_PHYS(eglstd[unit])));
	io->i_devaddr = (unsigned)addr;
	if (badaddr(&addr->sh_MCSB, sizeof(short))) {
#endif
			printf("egl%d: controller not available\n", unit);
		return(-1);
	}
	egl = &egl_softc[unit];
#ifndef STANDALONE
	vector = intp->v_vec;
	B_VECT_IVCT(egl->egl_t_vect) = vector++; /* remember int vector */
	B_VECT_ILVL(egl->egl_t_vect) = intp->v_brl;
	B_VECT_IVCT(egl->egl_te_vect) = vector++;
	B_VECT_ILVL(egl->egl_te_vect) = intp->v_brl;
	B_VECT_IVCT(egl->egl_r_vect) = vector++;
	B_VECT_ILVL(egl->egl_r_vect) = intp->v_brl;
	B_VECT_IVCT(egl->egl_re_vect) = vector++;
	B_VECT_ILVL(egl->egl_re_vect) = intp->v_brl;
	B_VECT_IVCT(egl->egl_qav_vect) = vector;
	B_VECT_ILVL(egl->egl_qav_vect) = intp->v_brl;
#else   STANDALONE
	B_VECT_IVCT(egl->egl_t_vect)  = 0;
	B_VECT_ILVL(egl->egl_t_vect)  = 0;
	B_VECT_IVCT(egl->egl_te_vect) = 0;
	B_VECT_ILVL(egl->egl_te_vect) = 0;
	B_VECT_IVCT(egl->egl_r_vect)  = 0;
	B_VECT_ILVL(egl->egl_r_vect)  = 0;
	B_VECT_IVCT(egl->egl_re_vect) = 0;
	B_VECT_ILVL(egl->egl_re_vect) = 0;
	B_VECT_IVCT(egl->egl_qav_vect)= 0;
	B_VECT_ILVL(egl->egl_qav_vect)= 0;
#endif
	egl->egl_shio = (EGL_SHIO *)addr;	/* remember address */
	if (!vme_reserve_iomap( unit, addr, 32, &egl->egl_csh, GBA_CS_AUTOEXP))
	  cmn_err( CE_PANIC, "Couldn't allocate cache section in GBA!\n");
	eglsetup(unit);
#ifndef STANDALONE
	mbinit();	/* start the mbufs */
	if (showconfig)
		printf("egl%d: hardware address %s\n",
			   unit, ether_sprintf(egl->egl_enaddr));
#endif
#ifdef STANDALONE
	XPR3(XPR_ENET,"egl%d: hardware address %s\n",
			   unit, ether_sprintf(egl->egl_enaddr));
#endif
}
/*
 * Interface exists: make available by filling in network interface
 * record.  System will initialize the interface when it is ready
 * to accept packets. 
 */
eglsetup(unit)
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	register struct ifnet *ifp = &egl->egl_if;
	struct sockaddr_in *sin;

	egl->egl_configured = 0;	/* not configured */
	if (!eglpreset(shio,unit)) {	/* reset the board */
		log(BSD43_LOG_ERR,"egl%d: reset failed\n",unit);
#ifdef STANDALONE
		exit(-1);	/* no need to go any further */
#else
		return;		/* didn't configure */
#endif STANDALONE
	}
	W(egl->egl_IMOD) = (ushort)M_IMOD_PE;		/* Ethernet */
	bcopy(shio->sh_CSTB.cstb_PHY, egl->egl_PHY, 
		sizeof(egl->egl_PHY));	/* Default Physical Address */
	if (!egl->laf_set) {
		egl->egl_FILT[0] = 0x00;
		egl->egl_FILT[1] = 0x00;
		egl->egl_FILT[2] = 0x00;
		egl->egl_FILT[3] = 0x00;
		egl->egl_FILT[4] = 0x00;
		egl->egl_FILT[5] = 0x00;
		egl->egl_FILT[6] = 0x00;
		egl->egl_FILT[7] = 0x00;
	}
	eglconfig(unit, NOINT);		/* configure the settings */
	if (!egl->egl_configured) {
#ifdef STANDALONE
		exit(-1);	/* no need to go any further */
#else
		return;		/* didn't configure */
#endif STANDALONE
	}
#ifndef STANDALONE
	if (showconfig) {
		printf("egl%d hardware revision %x.%x.%x\n", unit,
			shio->sh_CSTB.cstb_FREV[0],
			shio->sh_CSTB.cstb_FREV[1],
			shio->sh_CSTB.cstb_FREV[2]);
	}
#endif  STANDALONE
	if (!egl->enaddr_set)
		bcopy(egl->egl_PHY, egl->egl_enaddr, sizeof(egl->egl_PHY));
#ifndef	STANDALONE	
	egl->egl_if.if_physaddrlen = 6;
	bcopy ((caddr_t)egl->egl_enaddr, (caddr_t)egl->egl_if.if_physaddr, 6);
#endif	STANDALONE	
#if defined(SYSVR2) && !defined(STANDALONE)
	ifp->if_ioctl = eglioctl;
#endif
	ifp->if_mtu = ETHERMTU - 4;	/* MTU = 1496 for block bug */
	ifp->if_unit = unit;
	ifp->if_name = "egl";
	ifp->if_output = egloutput;
	ifp->if_reset = eglreset;

#ifndef STANDALONE
	ifp->if_description = "Interphase 4207 Eagle";
	ifp->if_type = 6;		/* ethernet-csmacd */
	ifp->if_speed = 10000000;	/* 10 megabits */

	ifp->if_init = egl_init;	/* So lboot won't grab it */
	ifp->if_flags = IFF_BROADCAST;
	ifp->if_watchdog = eglwatch;
/*	ifp->if_timer = EGLWATCHINTVL;	/* don't use till it does something */
	ifp->if_timer = 0;	
	if_attach(ifp);
#else   STANDALONE
	ifp->if_init = eglinit;
#endif  STANDALONE
}
/*
 * Reset of interface
 */
eglreset(unit)
	int unit;
{
	if (unit < 0 || unit >= NEGL) return;
#ifndef STANDALONE
	egl_init(unit);
#else
	eglinit(unit);
#endif  STANDALONE
}
/*
 * Physical RESET of the Eagle
 */
eglpreset(shio,unit)
	volatile  EGL_SHIO *shio;
{
	volatile  EGL_MSR *msr = &shio->sh_MCSB.mcsb_MSR;
	volatile  EGL_MCR *mcr = &shio->sh_MCSB.mcsb_MCR;
	volatile int i;

	i = 1024;
	while (((WORDP(msr) & M_MSR_CNA) || !(WORDP(msr) & M_MSR_BOK))
	       && --i) /* controller available? */
		DELAY(8192);	/* give the board a chance, so wait */
	if (i == 0) {
		log(BSD43_LOG_ERR,"egl%d: POWER-ON DIAGS FAILED!\n",unit);
	}
	WORDP(msr) |= M_MSR_CNA;/* set controller not available */
	wbflush();
	WORDP(mcr) = M_MCR_RES; /* reset the eagle */
	wbflush();
	DELAY(512); 		/* at least 50usec required */
	WORDP(mcr) &= ~M_MCR_RES;
	wbflush();
	i = 1024;
	while ((WORDP(msr) & M_MSR_CNA) && --i) /* controller available? */
		DELAY(8192);	/* give the board a chance, so wait */
	if (i == 0) {
		return(0);	/* Eagle has crashed */
	} else {
		/* additional delay is necessary because the reset
		   is really not done yet! (iphase bug) */
		i = 1024;
		while ((!(WORDP(msr) & M_MSR_BOK)) && i--)
		  DELAY(8192);
		if (i == 0) {
		  log(BSD43_LOG_ERR,"egl%d: RESET DIAGS FAILED!\n",unit);
		  WORDP(msr) |= M_MSR_BOK; /* re-set board OK (iphase bug) */
		  return(0);
		}
		return(1);	/* Eagle has landed */
	}
}
/*
 * Initialization of interface; clear pending
 * operations.
 */
#ifndef STANDALONE
egl_init(unit)				/* So Lboot does not grab it */
#else
eglinit(unit)
#endif  STANDALONE
	int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_CRB *crb = &shio->sh_CRB;
	register struct ifnet *ifp = &egl->egl_if;
	register struct	sockaddr_in	*sin;
	int s, i = EGL_TX_NBUFF, qstarttimeout;

	ifp->if_flags &= ~(IFF_UP|IFF_RUNNING);
#ifndef STANDALONE
	s = splimp();
	while (i--)
		untimeout(egltdog, unit); /* clean off xmits */
	if (egl->egl_tx_timerid)
		untimeout(egl->egl_tx_timerid); /* clean off xmits */
	sin = (struct sockaddr_in *)&ifp->if_addr;
	if (sin->sin_addr.s_addr == 0) { /* address still unknown */
		splx(s);
		log(BSD43_LOG_ERR,"eagle: internet address unknown\n");
		return(-1);
	}
	egl->egl_configured = 0;	/* not configured */
	if (!eglpreset(shio,unit)) {	/* reset the board */
		log(BSD43_LOG_ERR,"egl%d: reset failed\n",unit);
		return(-1);
	}
	/* lance is turned OFF now */
	eglconfig(unit, NOINT);		/* configure board */
	if (!egl->egl_configured) {
		return(-1);		/* didn't configure */
	}
	/* lance is turned ON now */
#endif  STANDALONE
	eglsetiopbs(unit);		/* setup iopbs */
	egl->egl_qmode = 0;		/* not queue mode */
	egliworkq(unit, NOINT);		/* start up queue mode */
        qstarttimeout = QSTARTTIMEOUT;
        while( (!(CRB_QSTARTED(crb->crb_CRSW))) && qstarttimeout--) ;
        if( CRB_QSTARTED(crb->crb_CRSW) ) {
        	egl->egl_qmode++;	
        } else {
        	log(BSD43_LOG_ERR,"eagle %d Failed to start Q-Mode\n",unit);
#ifndef STANDALONE
		splx(s);
#endif  STANDALONE
        	return(-1);
        }
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	eglhangemhigh(unit);		/* initial hang receive requests */
	egl->egl_if.if_flags |= (IFF_UP | IFF_RUNNING);
#ifndef STANDALONE
	eglstart(unit);			/* start transmits */
	splx(s);
#endif  STANDALONE
	return(1);
}
eglsetiopbs(unit)
	int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_IOPB *iopb;
	register int i;
 	struct mbuf *tbuf, *rbuf;

	iopb = &shio->sh_IOPB[EGL_RX_IOPBF];
	for(i = 0;i < EGL_RX_NBUFF;i++) {
#ifndef STANDALONE
		W(iopb->iopb_OPTION) = M_OPT_DMA | M_OPT_IE;
#else   STANDALONE
		W(iopb->iopb_OPTION) = M_OPT_DMA;
#endif  STANDALONE
		iopb->iopb_CMD = CNTR_RECEIVE;
		iopb->iopb_NVCT = egl->egl_r_vect;
		iopb->iopb_EVCT = egl->egl_re_vect;
		if (IS_R3200 || IS_RB3125 || IS_R6300) /* use block mode transfers */
			W(iopb->iopb_TOPT) = EGL_TOPT_BLK | DIR_READ;
		else
			W(iopb->iopb_TOPT) = EGL_TOPT | DIR_READ;
		/* For each rbuf pointer, get an mbuf, get a cluster */
		if(!(rbuf = egl->egl_rbinfo[i].virtual)) {
			MGET(rbuf, M_DONTWAIT, MT_DATA);
			if (!m_goodm(rbuf))
				panic("eglsetup: bad mbuf");
#ifdef STANDALONE
			rbuf->m_off = 0;
			rbuf->m_len = MLEN;
#else
			if (!mclget(rbuf)) {
				panic("eglsetup: mclget failed");
			}
#endif  STANDALONE
		}
		/* egl_rbinfo[i] always uses iopb at index (EGL_RX_IOPBF+i) */
		egl->egl_rbinfo[i].iopb = (ULONG)iopb;
		iopb->iopb_BUFF = 
		  egl_iosetup( egl, &egl->egl_rbinfo[i], rbuf, 1 );
		iopb->iopb_LENGTH = EGL_MTU;
		iopb->iopb_HBUF = 0;
		iopb->iopb_PTLF = 0;
		bzero(iopb->iopb_NODE,
			sizeof(iopb->iopb_NODE));
		iopb->iopb_SGEC = 0;
		iopb->iopb_LAN1 = 0;
		iopb->iopb_LAN3 = 0;
		iopb++;
	}
	iopb = &shio->sh_IOPB[EGL_TX_IOPBF];
	for(i = 0;i < EGL_TX_NBUFF;i++) {
		iopb->iopb_CMD = CNTR_TRANSMIT;
#ifndef STANDALONE
		W(iopb->iopb_OPTION) = M_OPT_DMA | M_OPT_IE;
		iopb->iopb_NVCT = egl->egl_t_vect;
		iopb->iopb_EVCT = egl->egl_te_vect;
#else   STANDALONE
		W(iopb->iopb_OPTION) = M_OPT_DMA;
#endif  STANDALONE
 		/* We need to recover any hung transmit buffers if
 		 * initialization is due to a HW reset after an error
 		 * such as a transmit timeout.
 		 */
 		if (tbuf = egl->egl_tbinfo[i].virtual) {
 		  if (m_goodm(tbuf)) {
 		    egl_iodone( &egl->egl_tbinfo[i], 0 );
 		    m_freem(tbuf);
 		  }
 		  else log(BSD43_LOG_ERR,
 		   "egl%d: eglinit, bad mbuf %x, couldn't free\n",unit,tbuf);
		}

		if (IS_R3200 || IS_RB3125 || IS_R6300) /* use block mode transfers */
			W(iopb->iopb_TOPT) = EGL_TOPT_BLK | DIR_WRITE;
		else
			W(iopb->iopb_TOPT) = EGL_TOPT | DIR_WRITE;
		iopb->iopb_HBUF = 0;
		iopb->iopb_SGEC = 0;
		iopb->iopb_LAN1 = 0;
		iopb->iopb_LAN3 = 0;
		iopb++;
	}
	egl->egl_txadd = egl->egl_txrem = EGL_TX_NBUFF-1;
	egl->egl_quecnt = egl->egl_txcnt = 0;
	egl->egl_cqe = (EGL_CQE *)&shio->sh_CQE[0];
}
/*
 * Start or re-start output on interface. Get another datagram to send off of 
 * the interface queue, and add it to the transmit packet queue for the
 * interface.
 */
eglstart(unit)
	int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_IOPB *iopb;
	register unsigned buff_addr;
	register struct mbuf *m, *n;
	register struct ether_header *eh;
	int len, index;

#ifdef STANDALONE
	if(egl_txblock) return 0; /* don't send it yet */
#endif  STANDALONE
	/* if PACKET QUEUE is FULL, can't send anything */
	if (TXFULL(egl->egl_txcnt + egl->egl_quecnt))
		return 0;
	IF_DEQUEUE(&egl->egl_if.if_snd, m);
	if (m == 0)
		return 0;
#ifdef STANDALONE
	egl_txblock = 1;
#endif  STANDALONE
	TXINC(egl->egl_txadd);
	egl->egl_txcnt++;
	index = egl->egl_txadd;
	iopb = (volatile EGL_IOPB *)&shio->sh_IOPB[EGL_TX_IOPBF + index];
	/* get it all pulled into one mbuf, since we can't S/G */
	n = m;
	len = 0;
#ifndef STANDALONE
	if (n->m_next) { /* a chain... */
		while (n) {
			len += n->m_len;
			n = n->m_next;
		}
		if (len > MLEN) { /* won't fit in one */
			MGET(n, M_DONTWAIT, MT_DATA);
			if (n == NULL) {
				TXDEC(egl->egl_txadd);	/* back down */
				egl->egl_txcnt--;
				IF_PREPEND(&egl->egl_if.if_snd, m);
				timeout(egl_delay1,unit,1);
				return 0;
			}
			if (!mclget(n)) {
				m_free(n);
				TXDEC(egl->egl_txadd);	/* back down */
				egl->egl_txcnt--;
				IF_PREPEND(&egl->egl_if.if_snd, m);
				timeout(egl_delay1,unit,1);
				return 0;
			}
			m_movtoc(m, mtod(n, caddr_t), len);
		} else {
			n = m_pullup(m, len);
		}
	} else {
#endif  STANDALONE
		len = n->m_len;
#ifndef STANDALONE
	}
#endif  STANDALONE
	eh = mtod(n, struct ether_header *);

	if (len - sizeof(struct ether_header) < ETHERMIN)
		len = ETHERMIN + sizeof(struct ether_header);
	/* we MUST be modulo 4 in order to use block mode transfers */
	iopb->iopb_LENGTH = (len + 3) & ~0x3;
	iopb->iopb_BUFF =  buff_addr =
	  egl_iosetup( egl, &egl->egl_tbinfo[index], n, 0 );
	if (IS_R3200 || IS_RB3125 || IS_R6300) { /* we can (try and) use block mode transfer */
		/* we must be aligned on a long-word boundary */
		if (!(buff_addr & 0x2)) { 
			W(iopb->iopb_TOPT) = EGL_TOPT_BLK | DIR_WRITE;
		} else { /* put it back to non-block */
			W(iopb->iopb_TOPT) = EGL_TOPT | DIR_WRITE;
		}
	}
	iopb->iopb_PTLF = eh->ether_type;
	/*
	 * Following code performs a more efficient copy of the ethernet
	 * destination address when we know src & dst are word aligned.
	 * This is important for RC6280 to avoid a sequence of 6 one byte
	 * writes on the VME bus.
	 */
	if (((unsigned)eh->ether_dhost & 0x3) == 0) {
	    *(int *)iopb->iopb_NODE =  *(int *)eh->ether_dhost;
	    *(short *)(iopb->iopb_NODE+4) =  *(short *)(eh->ether_dhost+4);
	}
	else 
	  bcopy(eh->ether_dhost, iopb->iopb_NODE,
		sizeof(iopb->iopb_NODE));
	bcopy(egl->egl_enaddr, eh->ether_shost, 
		sizeof(eh->ether_shost));
	eglsend(unit);		/* kick off send */
#ifdef STANDALONE
	egl_iodone(&egl->egl_tbinfo[index], 0);
	m_freem(n);		/* free regardless */
	egl_txblock = 0;	/* unblock transmits */
	egl_rxblock = 0;	/* unblock receives */
#endif STANDALONE
	return 1;
}

void
egl_mbuf_shortage(unit)
	register int unit;
{
	static unsigned int last_mbuf_msg=0;
	static int last_unit=0;
	
	/* Issue mbuf complaint at most once per minute */

#ifndef STANDALONE	
	if ((lbolt - last_mbuf_msg) > 60*HZ) {
	  log(BSD43_LOG_ERR,"egl%d out of mbufs\n", unit);
	  last_mbuf_msg = lbolt;
	  last_unit = unit;
	} else if (last_mbuf_msg && (unit != last_unit) && (last_unit != -1)) {
	  log(BSD43_LOG_ERR,"Multiple egl(s) out of mbufs\n");
	  last_unit = -1;
	}
#else
	  log(BSD43_LOG_ERR,"egl%d out of mbufs\n", unit);
#endif STANDALONE
}

#ifndef STANDALONE
void
egl_delay1(unit)
	register int unit;
{
	int s;

	s = splimp();
	while (eglstart(unit));
	splx(s);
}
void
egl_delay2(index_unit)
	register int index_unit;
{
	int s, index, unit;

	s = splimp();
	/* decode our values */
	index = index_unit >> 16;
	unit = index_unit & 0xffff;
	eglhangrcv(unit, index);
	splx(s);
}
#endif  STANDALONE

eglsend(unit)
	int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CQE *cqe;
	volatile EGL_CRB *crb = &shio->sh_CRB;
        register int i, index;

	if(egl->egl_txcnt == 0) { /* this should never happen */
		log(BSD43_LOG_ERR,"egl%d: egl_txcnt null in eglsend\n",unit);
		return(0);
	}
	if(egl->egl_qfull) 	/* the bird is full */
		return(0);
	cqe = egl->egl_cqe;
	if(CQE_BUSY(cqe->cqe_QECR)) { /* feed queue is full */
		egl->egl_qfull++;
#ifdef  STANDALONE
                i = CQEBUSYWAIT;
                while(i--);
                if(CQE_BUSY(cqe->cqe_QECR)) {
			printf("eglsend: feed queue full\n");
			return;
                }
#else   STANDALONE
		return(0);
#endif  STANDALONE
        }
	TXINC(egl->egl_txrem);
        egl->egl_txcnt--;
        index = egl->egl_txrem;
	cqe->cqe_CTAG = index;
	cqe->cqe_IOPB_ADDR = (caddr_t)(&shio->sh_IOPB[EGL_TX_IOPBF] + index)
		- (caddr_t)shio;
	cqe->cqe_WORK_QUEUE = EGL_XMTQN(0); /* TX work queue 0 */
#ifdef STANDALONE
	if(egl_rxblock == 2) { /* already in eglscan? */
		CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle to allow TX */
	} else egl_rxblock = 1;
	if (egl_txdone) {
	  printf("eglsend: Found egl_txdone already set before transmit!\n");
	  egl_txdone = 0;
	}
#endif STANDALONE
	CQE_GO(cqe->cqe_QECR);		/* fire one! */
	egl->egl_quecnt++;
	if((++egl->egl_cqe) == &shio->sh_CQE[MAX_CQE])
		egl->egl_cqe = (EGL_CQE *)&shio->sh_CQE[0];
#ifdef  STANDALONE
	i = eglwait(&shio->sh_CRB,1,unit);
	if (i & (M_CRSW_ER | M_CRSW_EX)) {
		printf("eglsend: transmit error\n");
        } else if (i == 0x1000) { /* unused value */
		printf("eglsend: transmit timeout\n");
	}	
	egl->egl_quecnt--;
	egl->egl_qfull = 0;		/* at least one slot open now */
#else   STANDALONE
	if (!egl->egl_tx_timerid)
		egl->egl_tx_timerid = timeout(egltdog, unit, EGLTWATCH);
#endif  STANDALONE
}
eglconfig(unit, itype)
	int unit;
	int itype;
{
	register struct egl_softc *egl= &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	/* scratch area */
	volatile  EGL_CIB *cib = (EGL_CIB *)&shio->sh_SCRTCH[0];
	volatile  EGL_IC_IOPB *iopb = (EGL_IC_IOPB *)&shio->sh_MCE_IOPB;
	register int i;
	/*
	 Make the Controller Initialization Block
	 */
	cib->cib_RES0 = 0;
	cib->cib_NCQE = NUM_CQE;
	W(cib->cib_IMOD) = (ushort)M_IMOD_PE;	/* Ethernet */
	cib->cib_NTXR = EGL_TX_NRINGS;		/* LANCE Tx Rings */
	cib->cib_NRXR = EGL_RX_NRINGS;		/* LANCE Rx Rings */
	bcopy(egl->egl_PHY, cib->cib_PHY, 
		sizeof(egl->egl_PHY));	/* Default Physical Address */
	bcopy(egl->egl_FILT, cib->cib_FILT,
		 sizeof(egl->egl_FILT)); /* Default Logical Address Filter */
	cib->cib_RXSIZ = EGL_RX_BUFSZ;		/* Max Rx Packet */
	cib->cib_NRBUF = EGL_RX_NRINGS;		/* Num Rx Buffers */
	cib->cib_TXSIZ = EGL_TX_BUFSZ;		/* Max Tx Packet */
	cib->cib_NIBUF = EGL_TX_NRINGS;		/* Num Tx Buffers */
	cib->cib_NHBUF = EGL_NHBUF;		/* Num Host Buffers */
	cib->cib_NVECT = egl->egl_qav_vect;	/* needed for error handling! */
	cib->cib_EVECT = egl->egl_qav_vect;	/* needed for error handling! */
	cib->cib_BURST = EGL_BURST;		/* DMA Burst Count */
	bzero(cib->cib_RES1, sizeof(cib->cib_RES1));
	/*
	 Make the MCE IOPB
	 */
	iopb->ic_iopb_CMD = CNTR_INIT;
	B_OPT_IE(iopb->ic_iopb_OPTION) = itype;     /* 0 no int; 1 int */
	B_OPT_SG(iopb->ic_iopb_OPTION) = CIB_ALIGN; /* enable Mips alignment */
	iopb->ic_iopb_NVCT = egl->egl_qav_vect;
	iopb->ic_iopb_EVCT = egl->egl_qav_vect;
	iopb->ic_iopb_RES0 = 0;
	iopb->ic_iopb_BUFF = (ULONG)cib - (ULONG)shio;/* CIB offset from shio */
	bzero(iopb->ic_iopb_RES1, sizeof(iopb->ic_iopb_RES1));
	/*
	 Make the MCE
	 */
	shio->sh_MCE.cqe_CTAG = 0;	/* for now */
	shio->sh_MCE.cqe_IOPB_ADDR = O_MCE_IOPB;
	shio->sh_MCE.cqe_WORK_QUEUE = 0;
	CQE_GO(shio->sh_MCE.cqe_QECR);		/* fire one! */
	if(!itype) {
		i = eglwait(&shio->sh_CRB,2,unit);
		if (i & (M_CRSW_ER | M_CRSW_EX)) {
			log(BSD43_LOG_ERR,
			"egl%d: error ** Unable to configure Eagle **\n",unit);
			if(iopb->ic_iopb_STATUS)
				log(BSD43_LOG_ERR,
				"iopb error status: %x\n",iopb->ic_iopb_STATUS);
			egl->egl_configured = 0;
		} else if (i == 0x1000) { /* unused value */
			log(BSD43_LOG_ERR,
			"egl%d: timeout * Unable to configure Eagle *\n",unit);
			egl->egl_configured = 0;
		} else egl->egl_configured = 1;
	}
}
egliworkq(unit, itype)
	int unit;
	int itype;
{
	register struct egl_softc *egl= &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_WQCF *iopb = (EGL_WQCF *)&shio->sh_MCE_IOPB;
	volatile  EGL_MCR *mcr = &shio->sh_MCSB.mcsb_MCR;
	register int i;
	/*
	 Make the misc MCE IOPB
	 */
	iopb->wqcf_CMD = CNTR_INIT_WORKQ;
	B_OPT_IE(iopb->wqcf_OPTION) = itype;	/* 0 no int; 1 int */
	iopb->wqcf_NVCT = egl->egl_qav_vect;
	iopb->wqcf_EVCT = egl->egl_qav_vect;
	bzero(iopb->wqcf_RES0, sizeof(iopb->wqcf_RES0));
	iopb->wqcf_WORKQ = EGL_MISCQ;		/* misc queue */
	W(iopb->wqcf_WOPT) = 0;
	iopb->wqcf_SLOTS = EGL_MS_SLOTS;	/* misc queue size */
	iopb->wqcf_PRIORITY = EGL_MS_PRIORITY;	/* misc queue priority */
	iopb->wqcf_WDIV = EGL_MS_WDIV;		/* misc queue work division */
	bzero(iopb->wqcf_RES1, sizeof(iopb->wqcf_RES1));
	/*
	 Make the MCE
	 */
	shio->sh_MCE.cqe_CTAG = 0;		/* for now */
	shio->sh_MCE.cqe_IOPB_ADDR = O_MCE_IOPB;
	shio->sh_MCE.cqe_WORK_QUEUE = 0;
	CQE_GO(shio->sh_MCE.cqe_QECR);		/* fire one! */
	if(!itype) {
		i = eglwait(&shio->sh_CRB,0,unit);
		if (i & (M_CRSW_ER | M_CRSW_EX)) {
			log(BSD43_LOG_ERR,
			"egl%d: error ** Unable to init misc workq **\n",unit);
			egl->egl_configured = 0;
			return;
		} else if (i == 0x1000) { /* unused value */
			log(BSD43_LOG_ERR,
			"egl%d: timeout * Unable to init misc workq **\n",unit);
			egl->egl_configured = 0;
			return;
		}
	}
	/*
	 Make the DMA MCE IOPB
	 */
	iopb->wqcf_CMD = CNTR_INIT_WORKQ;
	B_OPT_IE(iopb->wqcf_OPTION) = itype;	/* 0 no int; 1 int */
	iopb->wqcf_NVCT = egl->egl_qav_vect;
	iopb->wqcf_EVCT = egl->egl_qav_vect;
	bzero(iopb->wqcf_RES0, sizeof(iopb->wqcf_RES0));
	iopb->wqcf_WORKQ = EGL_DMAQ;		/* misc queue */
	W(iopb->wqcf_WOPT) = 0;
	iopb->wqcf_SLOTS = EGL_DM_SLOTS;	/* misc queue size */
	iopb->wqcf_PRIORITY = EGL_DM_PRIORITY;	/* misc queue priority */
	iopb->wqcf_WDIV = EGL_DM_WDIV;		/* misc queue work division */
	bzero(iopb->wqcf_RES1, sizeof(iopb->wqcf_RES1));
	/*
	 Make the MCE
	 */
	shio->sh_MCE.cqe_CTAG = 0;		/* for now */
	shio->sh_MCE.cqe_IOPB_ADDR = O_MCE_IOPB;
	shio->sh_MCE.cqe_WORK_QUEUE = 0;
	CQE_GO(shio->sh_MCE.cqe_QECR);		/* fire one! */
	if(!itype) {
		i = eglwait(&shio->sh_CRB,0,unit);
		if (i & (M_CRSW_ER | M_CRSW_EX)) {
			log(BSD43_LOG_ERR,
			"egl%d: error ** Unable to init dma workq **\n",unit);
			egl->egl_configured = 0;
			return;
		} else if (i == 0x1000) { /* unused value */
			log(BSD43_LOG_ERR,
			"egl%d: timeout * Unable to init dma workq **\n",unit);
			egl->egl_configured = 0;
			return;
		}
	}
	/*
	 Make the receive MCE IOPB
	 */
	iopb->wqcf_CMD = CNTR_INIT_WORKQ;
	B_OPT_IE(iopb->wqcf_OPTION) = itype;	/* 0 no int; 1 int */
	iopb->wqcf_NVCT = egl->egl_qav_vect;
	iopb->wqcf_EVCT = egl->egl_qav_vect;
	bzero(iopb->wqcf_RES0, sizeof(iopb->wqcf_RES0));
	iopb->wqcf_WORKQ = EGL_RECVQ;		/* receive queue */
	W(iopb->wqcf_WOPT) = 0;
	iopb->wqcf_SLOTS = EGL_RX_SLOTS;	/* receive queue size */
	iopb->wqcf_PRIORITY = EGL_RX_PRIORITY;	/* receive queue priority */
	iopb->wqcf_WDIV = EGL_RX_WDIV;		/* receive q work division */
	bzero(iopb->wqcf_RES1, sizeof(iopb->wqcf_RES1));
	/*
	 Make the MCE
	 */
	shio->sh_MCE.cqe_CTAG = 0;		/* for now */
	shio->sh_MCE.cqe_IOPB_ADDR = O_MCE_IOPB;
	shio->sh_MCE.cqe_WORK_QUEUE = 0;
	CQE_GO(shio->sh_MCE.cqe_QECR);		/* fire one! */
	if(!itype) {
		i = eglwait(&shio->sh_CRB,0,unit);
		if (i & (M_CRSW_ER | M_CRSW_EX)) {
			log(BSD43_LOG_ERR,
			"egl%d: error ** Unable to init rx workq **\n",unit);
			egl->egl_configured = 0;
			return;
		} else if (i == 0x1000) { /* unused value */
			log(BSD43_LOG_ERR,
			"egl%d: timeout * Unable to init rx workq **\n",unit);
			egl->egl_configured = 0;
			return;
		}
	}
	/*
	 Make the transmit MCE IOPB
	 */
	iopb->wqcf_CMD = CNTR_INIT_WORKQ;
	B_OPT_IE(iopb->wqcf_OPTION) = itype;	/* 0 no int; 1 int */
	iopb->wqcf_NVCT = egl->egl_qav_vect;
	iopb->wqcf_EVCT = egl->egl_qav_vect;
	bzero(iopb->wqcf_RES0, sizeof(iopb->wqcf_RES0));
	iopb->wqcf_WORKQ = EGL_XMTQN(0);	/* transmit queue 0 */
	W(iopb->wqcf_WOPT) = 0;
	iopb->wqcf_SLOTS = EGL_TX_SLOTS;	/* transmit queue size */
	iopb->wqcf_PRIORITY = EGL_TX_PRIORITY;	/* transmit queue priority */
	iopb->wqcf_WDIV = EGL_TX_WDIV;		/* transmit q work division */
	bzero(iopb->wqcf_RES1, sizeof(iopb->wqcf_RES1));
	/*
	 Make the MCE
	 */
	shio->sh_MCE.cqe_CTAG = 0;		/* for now */
	shio->sh_MCE.cqe_IOPB_ADDR = O_MCE_IOPB;
	shio->sh_MCE.cqe_WORK_QUEUE = 0;
	CQE_GO(shio->sh_MCE.cqe_QECR);		/* fire one! */
	if(!itype) {
		i = eglwait(&shio->sh_CRB,0,unit);
		if (i & (M_CRSW_ER | M_CRSW_EX)) {
			log(BSD43_LOG_ERR,
			"egl%d: error ** Unable to init tx workq **\n",unit);
			egl->egl_configured = 0;
			return;
		} else if (i == 0x1000) { /* unused value */
			log(BSD43_LOG_ERR,
			"egl%d: timeout * Unable to init tx workq **\n",unit);
			egl->egl_configured = 0;
			return;
		}
	}
#ifndef STANDALONE
	if (!egl->egl_qa_timerid)
		egl->egl_qa_timerid = timeout(eglqdog, unit, EGLQWATCH);
#endif  STANDALONE
	WORDP(mcr) |= M_MCR_SQM;	/* start queue mode */
	wbflush();
}
UWORD
eglwait(crb, wait_flag, unit)
	volatile EGL_CRB *crb;
	int wait_flag;
	int unit;
{
	volatile UWORD lcrsw;
	volatile int i;

onceagain:
	i = 8000;
	while ((!(W(crb->crb_CRSW) & M_CRSW_CRBV)) && --i) {
#ifdef STANDALONE
		if (egl_txdone) { /* tx fielded in eglscan() */
			egl_txdone--;
			return ((UWORD)M_CRSW_CC); /* show success */
		}
		if (wait_flag == 2) /* needed for CNTLR INIT only */
			DELAY(1024);
#else   STANDALONE
		DELAY(1024);
#endif  STANDALONE
	}
	lcrsw = W(crb->crb_CRSW);
	/*
	 * We must check for the Command Response Block Valid bit.  Sometimes
	 * the Eagle turns on "Queue Mode Started" just before completing
	 * a command (I don't know why, but I've seen this occur!).
	 */
	if (lcrsw & M_CRSW_CRBV) {	/* Has Eagle completed an op ? */
#ifdef STANDALONE
	  	/*
		 * If we were waiting for xmit complete (wait_flag == 1) and
		 * instead we received a packet, process the received pkt
		 * and go back to waiting for xmit complete.
		 */
		if (wait_flag == 1) {
			if(crb->crb_WORK_QUEUE == EGL_RECVQ) {
				egl_rxblock = 0;
				if (!eglscan(unit))
				  panic("eglwait: eglscan failed on RX\n");
				egl_rxblock = 1; /* restore RX blocking */
				goto onceagain;
			}
		}
#endif STANDALONE
		CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	}
	if (i == 0) {
		return ((UWORD)0x1000); /* indicate a timeout occurred */
	} else return(lcrsw);
}
#ifndef STANDALONE
/*
 * Ethernet interface transmit interrupt.
 */
egltint(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_CRB *crb = &shio->sh_CRB;
	register int index;
	struct mbuf *tbuf;

	if(shio->sh_RET_IOPB.iopb_LAN1 & (LANCE_TONE | LANCE_TMORE))
		egl->egl_if.if_collisions++;
	index = crb->crb_CTAG;
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	tbuf = egl->egl_tbinfo[index].virtual;
	if (m_goodm(tbuf)) {
	  egl_iodone( &egl->egl_tbinfo[index], 0 );
	  m_freem(tbuf);
	}
	else log(BSD43_LOG_ERR,
		"egl%d: egltint, bad mbuf %x, couldn't free\n",unit,tbuf);
	egl->egl_if.if_opackets++;
	egl->egl_quecnt--;
	egl->egl_qfull = 0;		/* at least one slot open now */
	if (egl->egl_tx_timerid)
		untimeout(egl->egl_tx_timerid);
	else log(BSD43_LOG_ERR,"egltint: egl%d no timer id\n",unit);
	while(eglstart(unit));		/* queue up next until can't */
}
/*
 * Ethernet interface transmit error interrupt.
 */
eglteint(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile  EGL_SHIO *shio = egl->egl_shio;
	volatile  EGL_CRB *crb = &shio->sh_CRB;
	register int index;
	volatile EGL_IOPB tiopb;
	struct mbuf *tbuf;
	
	tiopb = shio->sh_RET_IOPB;
	if(tiopb.iopb_LAN1 & LANCE_TERR) {
		log(BSD43_LOG_ERR,"egl%d: tx error, LAN1 = %x LAN3 = %x\n",
			unit,tiopb.iopb_LAN1,tiopb.iopb_LAN3);
	} else if(tiopb.iopb_STATUS) {
		log(BSD43_LOG_ERR,"egl%d: tx error status %x\n",
			unit,tiopb.iopb_STATUS);
	}
	egl->egl_if.if_oerrors++;
	index = crb->crb_CTAG;
	tbuf = egl->egl_tbinfo[index].virtual;
	if (m_goodm(tbuf)) {
	  egl_iodone( &egl->egl_tbinfo[index], 0 );
	  m_freem(tbuf);
	}
	else log(BSD43_LOG_ERR,
		"egl%d: eglteint, bad mbuf %x, couldn't free\n",unit,tbuf);
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	egl->egl_quecnt--;
	egl->egl_qfull = 0;		/* at least one slot open now */
	if (egl->egl_tx_timerid)
		untimeout(egl->egl_tx_timerid);
	else log(BSD43_LOG_ERR,"eglteint: egl%d no timer id\n",unit);
	while(eglstart(unit));		/* queue up next until can't */
}
/*
 * Ethernet interface receive interrupt.
 */
eglrint(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CRB *crb = &shio->sh_CRB;
	int len;
	struct mbuf *rbuf;
	int index;

	if (shio == NULL) {
		if (showconfig)
		      cmn_err(CE_CONT,"egl%d: spurious receive interrupt",unit);
		return;
	}
	len = shio->sh_RET_IOPB.iopb_LENGTH;   /* length of recv packet */
	index = crb->crb_CTAG;
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	rbuf = egl->egl_rbinfo[index].virtual;
	if (!m_goodm(rbuf))
		log(BSD43_LOG_ERR,
		"egl%d: eglrint, bad mbuf %x\n",unit,rbuf);
	egl->egl_if.if_ipackets++;	
	egl_iodone( &egl->egl_rbinfo[index], 1 );
	rbuf->m_act = (struct mbuf*)0; /* allow trailers to work */
	eglrecv(unit, len, rbuf);
	eglhangrcv(unit, index);
 	egl->egl_qfull = 0;		/* at least one slot open now */
}
/*
 * Ethernet interface receive error interrupt.
 */
eglreint(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CRB *crb = &shio->sh_CRB;
	volatile EGL_CSB *csb = &shio->sh_CSB;
	volatile EGL_IOPB tiopb;
	struct mbuf *rbuf;
	
	tiopb = shio->sh_RET_IOPB;
	if (tiopb.iopb_LAN1 & LANCE_RERR) {
		log(BSD43_LOG_ERR,"egl%d: rx error, LAN1 = %x LAN3 = 0x%x\n",
			unit,tiopb.iopb_LAN1,tiopb.iopb_LAN3);
	} else if(tiopb.iopb_STATUS) { /* only interesting if non RERR */
		log(BSD43_LOG_ERR,"egl%d: rx error status %x\n",
			unit,tiopb.iopb_STATUS);
		if (!tiopb.iopb_LENGTH) /* valid only if no RERR */
			log(BSD43_LOG_ERR,"egl%d: 0 length rx\n",unit);
	}
	if(csb->csb_CSR0MISS > egl->egl_csr0miss) { /* any new ones? */
		egl->egl_csr0miss = csb->csb_CSR0MISS;
		log(BSD43_LOG_ERR,
		"egl%d: csr0 MISSED RX PACKET; count is %d\n",
		unit,egl->egl_csr0miss);
	}
	egl->egl_if.if_ierrors++;
	rbuf = egl->egl_rbinfo[crb->crb_CTAG].virtual;
	/* No call to "egl_iodone" needed since we simply reuse same mbuf */
	eglhangrcv(unit, crb->crb_CTAG);
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
	egl->egl_qfull = 0;		/* at least one slot open now */
}
/*
 * Ethernet interface queue entry available interrupt.
 * This is shared with the configure stuff.
 */
eglqint(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CRB *crb = &shio->sh_CRB;
	volatile EGL_CSB *csb = &shio->sh_CSB;
	volatile EGL_MSR *msr = &shio->sh_MCSB.mcsb_MSR;
	volatile EGL_IOPB tiopb;
	register int i;
	
	tiopb = shio->sh_RET_IOPB;
	if(WORDP(msr) & M_MSR_CNA) {	/* controller not available */
		log(BSD43_LOG_ERR,
		"egl%d: died and restarted; error status was %x\n",
			unit,tiopb.iopb_STATUS);
		/* these errors (among others) will turn the lance transmitter
		   off, which will send us here */
		if(csb->csb_CSR0MEM > egl->egl_csr0mem) { /* any new ones? */
			egl->egl_csr0mem = csb->csb_CSR0MEM;
			log(BSD43_LOG_ERR,
			"egl%d: %d csr0 MEMORY ERRORS caught\n",
			unit,egl->egl_csr0mem);
		}
		log(BSD43_LOG_ERR,	/* save the returned IOPB */
		"returned IOPB: cmd %x options %x status %x nvect %x\n",
		tiopb.iopb_CMD,tiopb.iopb_OPTION.U.w,tiopb.iopb_STATUS,
		tiopb.iopb_NVCT.U.w);
		log(BSD43_LOG_ERR,
		"evect %x topt %x buff_addr %x length %x\n",
		tiopb.iopb_EVCT.U.w,tiopb.iopb_TOPT.U.w, tiopb.iopb_BUFF,
		tiopb.iopb_LENGTH);
		log(BSD43_LOG_ERR,
		"host_buf %x ptlf %x sge_cnt %x lan1 %x lan3 %x\n",
		tiopb.iopb_HBUF,tiopb.iopb_PTLF,tiopb.iopb_SGEC,
		tiopb.iopb_LAN1,tiopb.iopb_LAN3);
		log(BSD43_LOG_ERR,
		"src/dest node %x:%x:%x:%x:%x:%x\n",
		tiopb.iopb_NODE[0],tiopb.iopb_NODE[1],tiopb.iopb_NODE[2],
		tiopb.iopb_NODE[3],tiopb.iopb_NODE[4],tiopb.iopb_NODE[5]);
		egl_init(unit);
		return;
	}
	if(CRB_QAVAIL(crb->crb_CRSW)) { /* feed queue is no longer full */
		egl->egl_qfull = 0;	/* at least one slot open now */
	}
	if(CRB_QSTARTED(crb->crb_CRSW)) { /* how about q mode started */
		if (egl->egl_qa_timerid)
			untimeout(egl->egl_qa_timerid);
		else log(BSD43_LOG_ERR,"eglqint: egl%d no timer id\n",unit);
		egl->egl_qmode++;	/* remember for later */
	}
	CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
}
#endif STANDALONE
/*
 * Process Ethernet receive completion:  If input error just drop packet, 
 * otherwise examine packet to determine type.  If can't determine length from 
 * type, then have to drop packet, otherwise decapsulate packet based on type 
 * and pass to type-specific higher-level input routine.
 */
eglrecv(unit, len, rbuf)
	int unit;
	int len;
	struct mbuf *rbuf;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	register struct ether_header *eh;
	register struct mbuf *m, *n;
	register struct ifqueue *inq;
	extern   struct ifqueue	ipintrq;
	struct ifnet *ifp = &egl->egl_if;
	unsigned int i;
	int off, resid;

	if (!m_goodm(rbuf))	/* this is logged in eglrint */
		return;
	/* total length - ether header - CRC */
	len =  len - sizeof(struct ether_header) - 4;
	if ((len < 46)||(len > EGL_MTU)) { /* 64-14-4 mindata-etherheader-CRC */
#ifndef STANDALONE
		log(BSD43_LOG_ERR,"egl%d: bad rx packet length %d\n",unit,len);
#else
		printf("egl%d: bad rx packet length %d\n",unit,len);
#endif  STANDALONE
		m_freem(rbuf);
		return;
	}
	rbuf->m_len = len;
	eh = mtod(rbuf, struct ether_header *);
	/* this has to do with a speed fix (in egl firmware) for 14 byte ether
	 * header */
	i = (unsigned int) eh + 2;
	eh = (struct ether_header*) i;
/*
* Deal with trailer protocol: if type is PUP trailer get true type from
* first 16-bit word past data.  Remember that type was trailer by 
* setting off.
*/
	eh->ether_type = ntohs((u_short)eh->ether_type);
	rbuf->m_off += sizeof(struct ether_header);
	rbuf->m_off += 2;  /* allow for firmware hack (force 16 byte header) */
#define egldataaddr(eh, off, type) ((type)(((caddr_t)((eh)+1)+(off))))
#ifndef STANDALONE
	if (eh->ether_type >= ETHERTYPE_TRAIL &&
	eh->ether_type < ETHERTYPE_TRAIL+ETHERTYPE_NTRAILER) {
		off = (eh->ether_type - ETHERTYPE_TRAIL) * 512;
#else
	if (eh->ether_type >= ETHERPUP_TRAIL &&
	eh->ether_type < ETHERPUP_TRAIL+ETHERPUP_NTRAILER) {
		off = (eh->ether_type - ETHERPUP_TRAIL) * 512;
#endif  STANDALONE
		if (off >= ETHERMTU) {
			log(BSD43_LOG_ERR,
			"egl%d: trailer offset >= ETHERMTU\n",unit);
			if (m_goodm(rbuf))
				m_freem(rbuf);
			else
				log(BSD43_LOG_ERR,
				"egl%d: not a m_goodm mbuf",unit);
			return;
		}
		eh->ether_type = ntohs(*egldataaddr(eh, off, u_short *));
		resid = ntohs(*(egldataaddr(eh, off+2, u_short *)));
		if (off + resid > len) {
#ifndef STANDALONE
			log(BSD43_LOG_ERR,
			"egl%d: trailer offset(%d)+resid(%d) > len(%d)\n",
				unit, off, resid, len);
#else
			printf("egl%d: trailer offset(%d)+resid(%d) >len(%d)\n",
				unit, off, resid, len);
#endif  STANDALONE
			if (m_goodm(rbuf))
				m_freem(rbuf);
			else
				log(BSD43_LOG_ERR,
				"egl%d: not a m_goodm mbuf",unit);
			return;
		}
		len = off + resid;
	} else
		off = 0;
/*
* Pull packet off interface.  Off is nonzero if packet has trailing 
* header; if_rqbget will then force this header information to be at 
* the front, but we still have to drop the type and length which are 
* at the front of any trailer data.
*/
	if (off) {
		MGET(m, M_DONTWAIT, MT_HEADER);
		if (m == NULL) {
#ifndef STANDALONE
			egl_mbuf_shortage(unit);
			if (m_goodm(rbuf))
				m_freem(rbuf);
			else
				log(BSD43_LOG_ERR,
				"egl%d: not a m_goodm mbuf",unit);
			return;
#else
			panic("eglrecv: out of mbufs\n");
#endif  STANDALONE
		}
		if (resid > MLEN)
			panic("Can't copy trailing header into one mbuf");
		/* don't want the 4 byte trailer header */
		bcopy(mtod(rbuf, caddr_t)+off+4, mtod(m, caddr_t), resid-4);
		m->m_next = rbuf;
		m->m_len = resid-4;
		rbuf->m_len -= resid;
	} else
		m = rbuf;
#ifndef STANDALONE
	MGET(n, M_DONTWAIT, MT_HEADER);
	if (n == NULL) {
	  	egl_mbuf_shortage(unit);
		if (m_goodm(m))
			m_freem(m);
		else
			log(BSD43_LOG_ERR,"egl%d: not a m_goodm mbuf",unit);
		return;
	}
	n->m_next = m;
	n->m_off = MMINOFF;
	n->m_len = sizeof (struct ifnet *);
	m = n;
	bcopy(ifp, mtod(m, struct ifnet **), sizeof(ifp));
#endif  STANDALONE
	switch (eh->ether_type) {
/* #ifdef INET	*/
#ifndef STANDALONE
		case ETHERTYPE_IP:
#else
		case ETHERPUP_IPTYPE:
#endif  STANDALONE
#ifdef STANDALONE
			_ip_input(ifp, m);
#else
#ifdef SYSVR2
			schednetisr(NETISR_IP);
#else
			schednetisr(AF_INET);
#endif SYSVR2
#endif STANDALONE
#ifndef STANDALONE
			inq = &ipintrq;
#endif
			break;

#ifndef STANDALONE
		case ETHERTYPE_ARP:
		case ETHERTYPE_RARP:
#else
		case ETHERPUP_ARPTYPE:
#endif  STANDALONE
			{
#ifndef STANDALONE
			m->m_act = 0;
			arpinput(&egl->egl_ac, m);
#else
			_arpinput(&egl->egl_ac, m);
#endif  STANDALONE
			return;
			}
/* #endif INET	*/

#ifndef STANDALONE			
		case ETHERTYPE_DECRESV0:
		case ETHERTYPE_MOPDL:
		case ETHERTYPE_MOPRC:
		case ETHERTYPE_DECIV:
		case ETHERTYPE_LAT:
		case ETHERTYPE_DECDIAG:
		case ETHERTYPE_DECCUSTOMER:
		case ETHERTYPE_LAVC:
		case ETHERTYPE_DECRESV1:
		case ETHERTYPE_DECRESV2:
		/* If eth_io_main is a dummy it will return a non-zero */
		/* value and we will have to free the mbuf ourselves. */
		if (eth_io_main(m)) {
			egl->egl_if.if_iunknownprotos++;
			m_freem(m);
		}
			return;
#endif STANDALONE			

		default:
#ifndef	STANDALONE			
			egl->egl_if.if_iunknownprotos++;
#endif	STANDALONE			
			if (m_goodm(m))
				m_freem(m);
			else
				log(BSD43_LOG_ERR,
				"egl%d: not a m_goodm mbuf",unit);
			return;
	}
#ifndef STANDALONE
	if(IF_QFULL(inq)) {
		IF_DROP(inq);
		m_freem(m);
		egl->egl_if.if_idiscards++;
		egl->egl_if.if_ierrors++;
	} else {
		m->m_act = 0;
		IF_ENQUEUE(inq, m);
	}
#endif  STANDALONE
}

/*
 * Call eglhangrcv to start up a receive request for all of the
 * receive IOPB's we have.
 */
eglhangemhigh(unit)
{
	register struct egl_softc *egl = &egl_softc[unit];
	register LONG i;
	volatile EGL_SHIO *shio = egl->egl_shio;

	for(i = 0;i < EGL_RX_NBUFF;i++) {
		if (!eglhangrcv(unit, i)) {
			break;
		}
	}
}
/*
 * Place a recevie request for the specified iobp.
 */
eglhangrcv(unit, index)
	int unit;
	int index;
{
	volatile EGL_IOPB *iopb;
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CQE *cqe = egl->egl_cqe;
	struct mbuf *rbuf;
        register int i;

	iopb = (EGL_IOPB *)egl->egl_rbinfo[index].iopb;
	if(iopb == NULL) {
		log(BSD43_LOG_ERR,
		"egl%d: eglhangrcv; null iopb so couldn't post rx\n",unit);
		return(0);
	}
	if(egl->egl_qfull) /* the bird is still full */
		return(0);
	/*
	 * Place a receive request.
	 */
	if(CQE_BUSY(cqe->cqe_QECR)) { /* feed queue full? */
#ifdef  STANDALONE
                i = CQEBUSYWAIT;
                while(i--);	/* Delay a bit */
                if(CQE_BUSY(cqe->cqe_QECR)) {
			printf("feed que full?\n");
                	return;
                }
         }
#else   STANDALONE
		egl->egl_qfull++;
		return(0);
	}
#endif  STANDALONE
	if ((rbuf = egl->egl_rbinfo[index].virtual) == NULL) {
		MGET(rbuf, M_DONTWAIT, MT_DATA);
		if (rbuf == NULL) {
#ifndef STANDALONE
			timeout(egl_delay2,(index<<16)|unit,1*HZ);
			return(0);
#else   STANDALONE
			panic("eglhangrecv: out of mbufs\n");
#endif  STANDALONE
		}
#ifdef STANDALONE
		rbuf->m_off = 0;
		rbuf->m_len = MLEN;
#else
		if (!mclget(rbuf)) {
			m_free(rbuf);
			timeout(egl_delay2,(index<<16)|unit,1*HZ);
			return(0);
		}
#endif  STANDALONE
	}
	iopb->iopb_BUFF =
	  egl_iosetup( egl, &egl->egl_rbinfo[index], rbuf, 1 );
	cqe->cqe_CTAG = index;
	cqe->cqe_IOPB_ADDR = (caddr_t)iopb - (caddr_t)shio;
	cqe->cqe_WORK_QUEUE = EGL_RECVQ;
	CQE_GO(cqe->cqe_QECR);		/* fire one! */
	if((++egl->egl_cqe) == &shio->sh_CQE[MAX_CQE])
		egl->egl_cqe = (EGL_CQE *)&shio->sh_CQE[0];
	return(1);			/* success */
}
egloutput(ifp, m0, dst)
	struct ifnet *ifp;
	struct mbuf *m0;
	struct sockaddr *dst;
{
	register struct egl_softc *egl = &egl_softc[ifp->if_unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CRB *crb = &shio->sh_CRB;
	register struct mbuf *m = m0;
	register struct ether_header *eh;
	register int off;
	int type, s, error;
	struct in_addr idst;
	int can_trail;
	u_char	edst[6];

	if ((egl->egl_if.if_flags & IFF_UP) == 0)
		return(0);
	switch( dst->sa_family ) {
		case AF_INET:
			idst = ((struct sockaddr_in *)dst)->sin_addr;
#ifdef STANDALONE
			if (!_arpresolve(&egl->egl_ac, &idst, edst))
#else
			if (!arpresolve(&egl->egl_ac,m,&idst,edst,&can_trail))
#endif
				return(NULL);	/* if not yet resolved */
#ifndef STANDALONE
			off = ntohs((u_short)
				mtod(m, struct ip *)->ip_len) - m->m_len;
			if ((ifp->if_flags & IFF_NOTRAILERS) == 0  &&
			       ( can_trail ) && off > 0 && (off & 0x1ff) == 0 &&
			       m->m_off >= MMINOFF + 2 * sizeof (u_short)) 
			{
				type = ETHERTYPE_TRAIL + (off >> 9);
				m->m_off -= 2 * sizeof (u_short);
				m->m_len += 2 * sizeof (u_short);
				*mtod(m, ushort *) = ETHERTYPE_IP;
				*(mtod(m, u_short *) + 1) = m->m_len;
				goto gottrailertype;
			}
#endif  STANDALONE
#ifndef STANDALONE
			type = ETHERTYPE_IP;
#else
			type = ETHERPUP_IPTYPE;
#endif  STANDALONE
			goto gottype;
		case AF_IMPLINK:
			/* should do some ARP here? */
			eh = mtod(m, struct ether_header *);
			goto gotheader;
		case AF_UNSPEC:
			eh = (struct ether_header *)dst->sa_data;
			bcopy((caddr_t)eh->ether_dhost,(caddr_t)edst,sizeof(edst));
			type = eh->ether_type;
			goto gottype;
		default:
			error = EAFNOSUPPORT;
		goto bad;
	}
gottrailertype:
/*
* Packet to be sent as trailer: move first packet
* (control information) to end of chain.
*/
	while (m->m_next)
		m = m->m_next;
	m->m_next = m0;
	m = m0->m_next;
	m0->m_next = 0;
	m0 = m;
gottype:
/*
* Add local net header.  If no space in first mbuf, allocate another.
*/
#ifdef STANDALONE
	if (m->m_off < sizeof(struct ether_header)
	    || m->m_off + m->m_len > MLEN)
#else
	if (1) /* always alloc a new buffer, to ensure alignment for eagle */
#endif
	{
		MGET(m, M_DONTWAIT, MT_DATA);
		if (m == 0) {
		  	egl_mbuf_shortage(ifp->if_unit);
			error = ENOBUFS;
			goto bad;
		} else if (!m_goodm(m)) {
			log(BSD43_LOG_ERR,
#ifdef STANDALONE
			"egl%d: bad mbuf\n",ifp->if_unit);
#else
			"egl%d: bad mbuf %x\n",ifp->if_unit,m);
#endif
			error = ENOBUFS;
			goto bad;
		}
		m->m_next = m0;
		m->m_off = MMINOFF;
		m->m_len = sizeof (struct ether_header);
	} else {
		m->m_off -= sizeof (struct ether_header);
		m->m_len += sizeof (struct ether_header);
	}

#ifndef	STANDALONE
	m0 = m;
	while (m0 != NULL) {
		egl->egl_if.if_ooctets += m0->m_len;
		m0 = m0->m_next;
	}
#endif STANDALONE
	  
	eh = mtod(m, struct ether_header *);
	eh->ether_type = htons((u_short)type);
#ifndef	STANDALONE
	if (edst[0] & 0x1)
		egl->egl_if.if_onucastpackets++;
#endif	STANDALONE

	bcopy((caddr_t)edst, (caddr_t)eh->ether_dhost, sizeof(edst));
gotheader:
	bcopy(egl->egl_enaddr, eh->ether_shost, sizeof(eh->ether_shost));
/*
* Queue message on interface if possible 
*/
#ifndef STANDALONE
	s = splimp();
#endif  STANDALONE
	if(IF_QFULL(&ifp->if_snd)) {
		IF_DROP(&ifp->if_snd);
		egl->egl_if.if_oerrors++;
#ifndef STANDALONE
		egl->egl_if.if_odiscards++;
		splx(s);
#endif  STANDALONE
		m_freem(m);
		return(ENOBUFS);
	}
	IF_ENQUEUE(&ifp->if_snd, m);
	eglstart(ifp->if_unit);
#ifndef STANDALONE
	splx( s );
#endif  STANDALONE
	return(0);
bad:
	if (m_goodm(m0))
		m_freem(m0);
	else log(BSD43_LOG_ERR,
#ifdef STANDALONE
	     "egl%d: bad mbuf",ifp->if_unit);
#else
	     "egl%d: bad mbuf %x",ifp->if_unit,m0);
#endif  STANDALONE
	egl->egl_if.if_oerrors++;
	return(error);
}

#ifndef STANDALONE
egltdog(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	register int s;

	s = splimp();
	if (egl->egl_tx_timerid)
		egl->egl_tx_timerid = 0;
	else log(BSD43_LOG_ERR,"egltdog: timeout with no timerid\n");
	log(BSD43_LOG_ERR,
	"egl%d: transmit timeout occurred, restarting\n",unit);
	egl_init(unit);	/* restart everything */
	splx(s);
}
eglqdog(unit)
	register int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;

	if(egl->egl_qmode)return; /* everything is ok */
	log(BSD43_LOG_ERR,"egl%d: queue mode timed out\n", unit);
	egl->egl_configured = 0; /* shut down interface */
}
/*
 * Process an ioctl request.
 */
eglioctl(ifp, cmd, data)
	register struct ifnet *ifp;
	int cmd;
	caddr_t data;
{
	register int unit = ifp->if_unit;
	register struct egl_softc *egl= &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	register struct ifreq *ifr = (struct ifreq *)data;
	register struct ifaddr *ifa = (struct ifaddr *)data;
	int s = splimp(), error = 0;
	int old_flags = ifp->if_flags;
	int old_change = ifp->if_lastchange;
	BYTE buff[16];
	struct sockaddr_in *sin;
	struct sockaddr *sa;

	switch (cmd) {
	case SIOCSIFADDR:
		if (!(ifp->if_flags & IFF_UP)) {
			ifp->if_lastchange =
				(time.tv_sec - boottime.tv_sec) * 100 + 
				(time.tv_usec - boottime.tv_usec) / 10000;
		}
		ifp->if_flags |= IFF_UP;
		switch (ifa->ifa_addr.sa_family) {
#ifdef INET
		case AF_INET:
			if (egl_init(ifp->if_unit)==-1) { /* before arpwhohas */
				error = EINVAL;
				break;
			}
			DELAY(5000);
			((struct arpcom *)ifp)->ac_ipaddr = 
				IA_SIN(ifa)->sin_addr;
			arpwhohas((struct arpcom *)ifp, &IA_SIN(ifa)->sin_addr);
			break;
#endif INET
#ifdef not
#ifdef NS
		case AF_NS:
		    {
			register struct ns_addr *ina = &(IA_SNS(ifa)->sns_addr);

			if (ns_nullhost(*ina)) {
				ina->x_host = *(union ns_host *)(es->es_enaddr);
			} else {
			/* TODO */
			}
			egl_init(ifp->if_unit);
			break;
		    }
#endif NS
#endif not
		case AF_UNSPEC:
			ifp->if_flags = old_flags; /* Don't turn anything on here for now. */
			ifp->if_lastchange = old_change;
			egl->enaddr_set = 1;
			bcopy((caddr_t)ifa->ifa_addr.sa_data, (caddr_t)egl->egl_enaddr, sizeof(egl->egl_enaddr));
			bcopy((caddr_t)egl->egl_enaddr, (caddr_t)egl->egl_PHY, sizeof (egl->egl_PHY));
			egl_init(ifp->if_unit);
			break;
		default:
			egl_init(ifp->if_unit);
			break;
		}
		break;
	case SIOCSIFFLAGS:
		if ( ((ifp->if_flags & IFF_UP) == 0) &&
		     (ifp->if_flags & IFF_RUNNING)) {
			/* if the eagle must be taken down, log the statistics
			 * just in case they could be useful for debug.
			 */
			log(BSD43_LOG_ERR,
			"egl%d: TXATT %x TXDMACMP %x LTXINTS %x\n",
			   ifp->if_unit,shio->sh_CSB.csb_TXATT,
			   shio->sh_CSB.csb_TXDMACMP,shio->sh_CSB.csb_LTXINTS);
			log(BSD43_LOG_ERR,
			"LGDTXS %x TXERRS %x TXDONES %x RXATMPS %x\n",
			   shio->sh_CSB.csb_LGDTXS,shio->sh_CSB.csb_TXERRS,
			   shio->sh_CSB.csb_TXDONES,shio->sh_CSB.csb_RXATMPS);
			log(BSD43_LOG_ERR,
			"RXSTARVE %x RXLINTS %x RXGOOD %x RXERROR %x\n",
			   shio->sh_CSB.csb_RXSTARVE,shio->sh_CSB.csb_RXLINTS,
			   shio->sh_CSB.csb_RXGOOD,shio->sh_CSB.csb_RXERROR);
			log(BSD43_LOG_ERR,
			"RXDMACMP %x RXDONE %x MISCDMA %x TOTLINTS %x\n",
			   shio->sh_CSB.csb_RXDMACMP,shio->sh_CSB.csb_RXDONE,
			   shio->sh_CSB.csb_MISCDMA,shio->sh_CSB.csb_TOTLINTS);
			log(BSD43_LOG_ERR,
			"CSR0INIT %x CSR0BABB %x CSR0CERR %x\n",
			   shio->sh_CSB.csb_CSR0INIT,shio->sh_CSB.csb_CSR0BABB,
			   shio->sh_CSB.csb_CSR0CERR);
			log(BSD43_LOG_ERR,
			"CSR0MISS %x CSR0MEM %x\n",
			   shio->sh_CSB.csb_CSR0MISS,shio->sh_CSB.csb_CSR0MEM);
			eglreset(ifp->if_unit);
			ifp->if_flags &= ~IFF_RUNNING;
		} else if ((ifp->if_flags & IFF_UP) &&
		    ((ifp->if_flags & IFF_RUNNING) == 0)) {
			egl_init(ifp->if_unit);
		}
		break;
        case SIOCSLAF:
		egl->laf_set = 1;
		bcopy (ifr->ifr_data, (caddr_t)egl->egl_FILT, 8);
		egl_init(ifp->if_unit);
		break;

        case SIOCGLAF:
		bcopy ((caddr_t)egl->egl_FILT, ifr->ifr_data, 8);
		break;
#ifdef notdef
	case SIOCGIFSTATS:
		{
		EGL_CSB stat;
		volatile EGL_CQE *cqe = egl->egl_cqe;
		volatile EGL_IOPB *iopb = &shio->sh_MCE_IOPB;
		int s = splimp();

		if((++egl->egl_cqe) == &shio->sh_CQE[MAX_CQE])
			egl->egl_cqe = (EGL_CQE *)&shio->sh_CQE[0];
		/* First, we must get him to update the data */
		iopb->iopb_CMD = 0x44; /* get stats */
		/* do not interrupt nor zero counters */
		B_OPT_IE(iopb->iopb_OPTION) = 0;	/* 0 no int; 1 int */
		bzero(&iopb->iopb_STATUS, sizeof(EGL_IOPB) - 4); /* hack */
		cqe->cqe_IOPB_ADDR = O_MCE_IOPB;
		cqe->cqe_WORK_QUEUE = EGL_MISCQ;
		CQE_GO(cqe->cqe_QECR);		/* fire one! */
		if(eglwait(&shio->sh_CRB,0,unit) & (M_CRSW_ER | M_CRSW_EX)) {
			printf("failed in get stats...\n");
		}
		splx(s);
		/* Now, we can copy it out */
		bcopy(&shio->sh_CSB, &stat, sizeof(EGL_CSB));
		error = copyout(&stat, ((struct ifreq *)data)->ifr_data,
			sizeof(EGL_CSB));
		break;
		}
#endif notdef
		default:
			error = EINVAL;
	}
	splx(s);
	return(error);
}
/*	This routine should be used for statistics gather and post ala enp10
 */
eglwatch(unit)
	int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	register int s;

	s = splimp();
	egl->egl_if.if_timer = EGLWATCHINTVL;
	splx(s);
}
#endif STANDALONE

#ifdef STANDALONE
/*
 * _eglinit -- cleanup globals, reset boards, also cleanup arp tables
 */
_eglinit()
{
	volatile  EGL_SHIO *shio;
	volatile  EGL_MSR *msr;
	volatile  EGL_MCR *mcr;
	volatile int i;
	register unit;

	if (sizeof (struct ether_info) > IOB_INODE)
		_io_abort ("bad size in iob for egl");
	bzero(egl_softc, sizeof(egl_softc));
	_eglshutdown();
	_init_arp();
}
/*
 * _eglopen -- setup EAGLE board and initialize network driver data structs
 */
_eglopen(io)
struct iob *io;
{
	register struct egl_softc *egl= &egl_softc[io->i_ctlr];
	register struct ifnet *ifp = &egl->egl_if;
	struct sockaddr_in *sin;
	volatile EGL_SHIO *addr;
	char *cp;
	extern struct in_addr inet_addr();
	extern char *getenv();

	if (io->i_ctlr >= (int)NEGL) {
		printf("egl: bad controller number\n");
		return (-1);
	}
	cei(io)->ei_registry = -1;	/* mark not bound yet */
	if (!egl->egl_isopen) {
		if (eglattach(io)==-1)
			return(-1);
		if (eglinit(io->i_ctlr) < 0)
			return(-1);
		cei(io)->ei_acp = &egl->egl_ac;	/* iob ptr to arpcom */
		sin = (struct sockaddr_in *)&egl->egl_if.if_addr;
		bzero(sin, sizeof(struct sockaddr_in));
		sin->sin_family = AF_INET;
#if !defined(SABLE)
		cp = getenv("netaddr");
#else
		cp = "0.0.0.0";			/* So SABLE sash will work */
#endif
		if (!cp || !*cp)
			goto bad;
		sin->sin_addr = inet_addr(cp);
		if (*(int *)&sin->sin_addr == -1) {
bad:
			printf("$netaddr not set or incorrect\n");
			io->i_errno = EADDRNOTAVAIL;
			return(-1);
		}
		egl->egl_isopen = 1;
		ifp->if_snd.ifq_maxlen = 1;
	} else {
		egl_rxblock = egl_txblock = 0; /* unblock tx's and rx's */
	}
	io->i_flgs |= F_SCAN;
	XPR3(XPR_ENET,"egl%d: netaddr %s\n",io->i_ctlr,cp);
	return(0);
}
/*
 * _eglioctl -- network control operations
 */
_eglioctl(io, cmd, arg)
struct iob *io;
int cmd;
int arg;
{
	struct so_table *st;
	struct sockaddr_in sin;

	switch (cmd) {
	case NIOCBIND:
		/*
		 * scan registry table, add new entry if entry for port
		 * doesn't already exist
		 */
		if (cei(io)->ei_registry >= 0) {
			printf("egl: already bound\n");
			return(-1);
		}
		cei(io)->ei_udpport = arg;
		if (st = _find_socket(arg)) {
			cei(io)->ei_registry = st - _so_table;
			return(0);
		}

		/* find an empty slot */
		st = _get_socket();
		if (st == NULL) {
			printf("egl: out of socket buffers\n");
			return(-1);
		}
#ifdef DEBUG
		printf("binding to %d 0x%x %d\n", sin.sin_family,
		    sin.sin_addr, ntohs((u_short)sin.sin_port));
#endif

		st->st_udpport = arg;
		cei(io)->ei_registry = st - _so_table;
		break;

	case FIOCSCAN:
		eglscan(io->i_ctlr);
		break;
	}
	return(0);
}
/*
 * eglscan -- look for recv'd packets
static void
 */
eglscan(unit)
int unit;
{
	register struct egl_softc *egl = &egl_softc[unit];
	volatile EGL_SHIO *shio = egl->egl_shio;
	volatile EGL_CRB *crb = &shio->sh_CRB;
	volatile EGL_IOPB tiopb;
	struct mbuf *rbuf;
	int index = 0;

	if (egl_rxblock) return(0);
	while (CRB_DONE( crb->crb_CRSW)) { /* CRB valid? */
		egl_rxblock = 2;
		if( crb->crb_WORK_QUEUE != EGL_RECVQ) {
			CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
			egl_txdone++;
			return(1);
		}
		tiopb = shio->sh_RET_IOPB; /* returned iopb */
		index = crb->crb_CTAG;
		rbuf = egl->egl_rbinfo[index].virtual;
		if (!m_goodm(rbuf))
			panic("eglscan: bad rbuf");
		egl->egl_if.if_ipackets++;	
		egl_iodone( &egl->egl_rbinfo[index], 1 );
		if (tiopb.iopb_STATUS)
			printf("eglscan: RX failed with status %x\n",
					tiopb.iopb_STATUS);
		eglrecv(unit, tiopb.iopb_LENGTH, rbuf);
		eglhangrcv(unit, index);
		CRB_CLR_DONE(crb->crb_CRSW);	/* release Eagle */
		egl_rxblock = 0; /* allow scandevs to work */
		index = 1; /* remember that index could be 0 */
	}
	eglstart(unit);	/* see if any transmits are queed */
	return(index);
}
/*
 * _eglstrategy -- performs io
 */
_eglstrategy(io, func)
register struct iob *io;
{
	register struct mbuf *m;
	struct so_table *st;
	int ocnt;

	if (cei(io)->ei_registry < 0) {
		printf("egl: socket not bound\n");
		io->i_errno = EINVAL;
		return (-1);
	}
	st = &_so_table[cei(io)->ei_registry];
	if (st->st_count <= 0) {
		printf("_eglstrategy: socket screw-up\n");
		io->i_errno = EINVAL;
		return (-1);
	}
	if (func == READ) {
		while ((io->i_flgs & F_NBLOCK) == 0 && st->st_mbuf == NULL)
			_scandevs();
		/*
		 * It's all or nothing when reading a packet
		 */
		if (m = _so_remove(st)) {
			ocnt = _min(m->m_len, io->i_cc);
			bcopy(mtod(m, char *), io->i_ma, ocnt);
			bcopy((char *)&m->m_srcaddr,
			    (char *)&cei(io)->ei_srcaddr,
			    sizeof(cei(io)->ei_srcaddr));
			_m_freem(m);
			return(ocnt);
		} else
			return(0);
	} else if (func == WRITE) {
		m = _m_get();
		if (m == 0) {
			panic("_eglstrategy: out of mbufs\n");
		}
		if (io->i_cc > MLEN - MMAXOFF) {
			_m_freem(m);
			printf("_eglstrategy: datagram too large\n");
			io->i_errno = EIO;
			return(-1);
		}
		m->m_off = MMAXOFF;
		bcopy(io->i_ma, mtod(m, char *), io->i_cc);
		m->m_len = io->i_cc;
		ocnt = _udp_output(io, &egl_softc[io->i_ctlr].egl_if, m);
		return(ocnt < 0 ? -1 : io->i_cc);
	} else
		_io_abort("egl bad function");
}

/*
 * _eglclose -- release any socket that's being held
 */
_eglclose(io)
struct iob *io;
{
	if (cei(io)->ei_registry >= 0)
		_free_socket(cei(io)->ei_registry);
	_eglshutdown();
}

/*
 * _eglshutdown -- turn off all egl boards
 */
_eglshutdown()
{
volatile  EGL_SHIO *shio;
volatile  EGL_MSR *msr;
volatile  EGL_MCR *mcr;
volatile int i,j;
register unit;

    for (unit=0; unit < NEGL; unit++) {
	    shio = (EGL_SHIO*)(IS_R6300 ?
		   find_r6000_controller( eglstd[unit], 0, sizeof(short))
		   : PHYS_TO_K1(VMESA16_TO_PHYS(eglstd[unit])));
	    msr = &shio->sh_MCSB.mcsb_MSR;
	    mcr = &shio->sh_MCSB.mcsb_MCR;
	    if (badaddr(&shio->sh_MCSB, sizeof(short)))
		    continue;
	    if (WORDP(msr) & M_MSR_CNA) /* running diags? */
		    continue; /* no need to reset */
	    i = 1024;
	    while ((!(WORDP(msr) & M_MSR_BOK)) && i--)
	      DELAY(8192);
	    if (i == 0) {
		    printf("_eglshutdown: egl%d POWER-ON DIAGS FAILED\n",unit);
	    }
	    WORDP(msr) |= M_MSR_CNA;/* set controller not available */
	    wbflush();
	    WORDP(mcr) = M_MCR_RES; /* reset the eagle */
	    wbflush();
	    DELAY(128); 		/* at least 50usec required */
	    WORDP(mcr) &= ~M_MCR_RES;
	    wbflush();
	    i = 1000;
	    while ((WORDP(msr) & M_MSR_CNA) && --i) /* controller ready? */
		    DELAY(8192);	/* give the board a chance, so wait */
	    if (i == 0) {
		    printf("_eglshutdown: egl%d failed to reset!\n",unit);
		    continue;
	    } else {
		    i = 1024;
		    while ((!(WORDP(msr) & M_MSR_BOK)) && i--)
		      DELAY(8192);
		    if (i == 0) {
		      log(BSD43_LOG_ERR,"egl%d: RESET DIAGS FAILED!\n",unit);
		      WORDP(msr) |= M_MSR_BOK; /* reset board OK (iphase bug)*/
		    }
	    }
	    /*
	     * Mark Eagle as closed. Also unmap and flush any I/O regions
	     * still used by the Eagle.
	     */
	    egl_softc[unit].egl_isopen = 0;
	    for ( j=0; j < EGL_TX_NBUFF; j++)
	      if (egl_softc[unit].egl_tbinfo[j].io_sah)
		egl_iodone( &egl_softc[unit].egl_tbinfo[j], 0 );
	    for ( j=0; j < EGL_RX_NBUFF; j++)
	      if (egl_softc[unit].egl_rbinfo[j].io_sah)
		egl_iodone( &egl_softc[unit].egl_rbinfo[j], 1 );
    }
}
#endif STANDALONE
