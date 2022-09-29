#ident "$Header: vmereg.h,v 1.5 90/01/23 13:30:25 huang Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * VMEbus related defines
 */

#define	VME_MINIPL	1
#define	VME_MAXIPL	7

/*
 * VME interrupt control registers
 */
#define	VME_IMR		0x1e020003	/* interrupt mask */
#define	VME_ISR		0x1e020007	/* interrupt status */
#define	VME_IACK	0x1df00002	/* interrupt acknowledge */

#define	NVMEVEC		1000	/* size of vmevec_tbl, WAG for now */

#ifdef LANGUAGE_C
struct vmevec {
	int (*vme_intr)();
	int vme_unit;
};
#endif LANGUAGE_C

/*
 * VME I/O space defines
 */
#define	VME_A16NPBASE	0x1d100000	/* a16 non-privileged address sp */
#define	VME_A16NPAMOD	0x29		/* a16 non-privileged addr modifier */

#define	VME_A16SBASE	0x1d000000	/* a16 supervisor address sp */
#define	VME_A16SAMOD	0x2D		/* a16 supervisor addr modifier */

/*
 * TODO: A24 NP stuff should go away since it doesn't exist
 */
#define	VME_A24NPBASE	0x1c000000	/* a24 non-privileged address sp */
#define	VME_A24NPAMOD	0x39		/* a24 non-privileged addr modifier */

#define	VME_A24SBASE	0x1c000000	/* a24 supervisor address sp */
#define	VME_A24SAMOD	0x3D		/* a24 supervisor addr modifier */

#define	VME_A32NPAMOD	0x09		/* a32 non-privileged addr modifier */
#define VME_A32NPBMOD   0x0B            /* a32 n-p block mode addr modifier */

#ifdef LANGUAGE_C
/*
 * VME address conversion macros
 */
#define	VMESA16_TO_PHYS(x)	((unsigned)(x)+VME_A16SBASE)
#define	VMENPA16_TO_PHYS(x)	((unsigned)(x)+VME_A16NPBASE)

/*
 * TODO: should delete
 */
#define	VMENPA24_TO_PHYS(x)	((unsigned)(x)+VME_A24NPBASE)
#define	VMESA24_TO_PHYS(x)	((unsigned)(x)+VME_A24SBASE)

#ifdef KERNEL
extern struct vmevec vmevec_tbl[];	/* interrupt dispatch table */
#endif KERNEL

/*****************************************************************************
 *
 * Definitions used by callers of the GBA utility routines.
 *
 *****************************************************************************/
 
typedef unsigned long csh_type;		/* Cache Section Handle type */
typedef unsigned long sph_type;		/* System Page Handle type */
typedef unsigned long sah_type;		/* System Area Handle type */
typedef unsigned long phys_page_num_t;	/* System Bus Physical Page Number */
typedef unsigned long kaddr_t;		/* kernel address (K0,K1, or K2) */
typedef unsigned long ioaddr_t;		/* GBA bus address for controllers */
 
/***************************************************************************
 * GBA flags (used in gba_reserve_lcs)
 ***************************************************************************/

#define	DMA_A24_AM	0x0001	/* dma region must be in A24 addr space	*/
#define	DMA_NOCACHE	0x0002	/* dma region should not cache in GBA */
#define	DMA_RANDOM	0x0004	/* auto prefetch and auto flush not useful */
#define DMA_NOPARTIAL	0x0008	/* write word not just the dirty bytes */
 
#define GBA_CS_AUTOEXP	0x0100	/* auto "expansion" of cache section *
 				   
/***************************************************************************
 * GBA flags (used in gba_kseg_map)
 ***************************************************************************/
 
#define GBA_CONTIG_ADDR	0x0001	/* Mapping must be contiguous on GBA bus */
#define GBA_NOPART_MAP  0x0002	/* Must map entire request (no partial map) */
#endif LANGUAGE_C
