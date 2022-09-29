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
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: vmereg.h,v 1.11.1.5.1.3 90/07/12 10:46:29 hawkes Exp $ */

#ifndef	_SYS_VMEREG_
#define	_SYS_VMEREG_	1



/*
 * VMEbus related defines
 */

/*
 * VME interrupt control registers
 */
#define	VME_IMR		0x1e020003	/* interrupt mask */
#define	VME_IMR_RB3125	0x1e020002	/* interrupt mask */
#define	VME_ISR		0x1e020007	/* interrupt status */
#define	VME_ISR_RB3125	0x1e020006	/* interrupt status */
#define	VME_IACK	0x1df00002	/* interrupt acknowledge */

/*
 * VME I/O space defines
 */
#define	VME_A16NPBASE	0x1d100000	/* a16 non-privileged address sp */
#define VME_A16NPSIZE	0x10000		/* size */
#define	VME_A16NPAMOD	0x29		/* a16 non-privileged addr modifier */

#define	VME_A16SBASE	0x1d000000	/* a16 supervisor address sp */
#define VME_A16SSIZE	0x10000		/* size */
#define	VME_A16SAMOD	0x2D		/* a16 supervisor addr modifier */

#define	VME_A24SBASE	0x1c000000	/* a24 supervisor address sp */
#define VME_A24SSIZE	0x100000	/* size */
#define	VME_A24SAMOD	0x3D		/* a24 supervisor addr modifier */

					/* R2300 cannot reach a24 non-prov */
#define VME_A24NPSIZE	0x100000	/* size */
#define VME_A24NPAMOD	0x39		/* a24 non-priv addr modifier */
#define VME_A24NPBMOD	0x3B		/* a24 non-priv block mode addr mod */

#define	VME_A32NPAMOD	0x09		/* a32 non-privileged addr modifier */
#define	VME_A32NPBMOD	0x0B		/* a32 non-priv block mode addr mod */

/*
 * VME address conversion macros
 */
#define	VMESA16_TO_PHYS(x)	((unsigned)(x)+VME_A16SBASE)
#define	VMENPA16_TO_PHYS(x)	((unsigned)(x)+VME_A16NPBASE)
#define VMESA24_TO_PHYS(x)	((unsigned)(x)+VME_A24SBASE)

#define IS_VME_A16NP(x)	((unsigned)(x) >= VME_A16NPBASE \
			 && (unsigned)(x) < VME_A16NPBASE+VME_A16NPSIZE)
#define IS_VME_A16S(x)	((unsigned)(x) >= VME_A16SBASE \
			 && (unsigned)(x) < VME_A16SBASE+VME_A16SSIZE)
#define IS_VME_A24S(x)	((unsigned)(x) >= VME_A24SBASE \
			 && (unsigned)(x) < VME_A24SBASE+VME_A24SSIZE)


/*****************************************************************************
 *
 * Definitions used by callers of the EBA utility routines.
 *
 *****************************************************************************/

typedef unsigned long csh_type;		/* Cache Section Handle type */
typedef unsigned long sph_type;		/* System Page Handle type */
typedef unsigned long sah_type;		/* System Area Handle type */
typedef unsigned long phys_page_num_t;	/* System Bus Physical Page Number */
typedef unsigned long kaddr_t;		/* kernel address (K0,K1, or K2) */
typedef unsigned long ioaddr_t;		/* EBA bus address for controllers */

/***************************************************************************
 * EBA flags (used in vme_reserve_iomap)
 ***************************************************************************/

#define	DMA_A24_AM	0x0001	/* dma region must be in A24 addr space	*/
#define	DMA_NOCACHE	0x0002	/* dma region should not cache in EBA */
#define	DMA_RANDOM	0x0004	/* auto prefetch and auto flush not useful */
#define DMA_NOPARTIAL	0x0008	/* write word not just the dirty bytes */

#define EBA_CS_AUTOEXP	0x0100	/* auto "expansion" of cache section */
				   
/***************************************************************************
 * EBA flags (used in vme_iomap)
 ***************************************************************************/

#define EBA_CONTIG_ADDR	0x0001	/* Mapping must be contiguous on EBA bus */
#define EBA_NOPART_MAP  0x0002	/* Must map entire request (no partial map) */

#define EBA_MAP_READ	0x0100	/* Request mapped for read by EBA (I/O write)*/
#define EBA_MAP_WRITE	0x0200	/* REquest mapped for write by EBA (I/O read)*/

/**** OBSOLETE VALUES *****/

#define GBA_CS_AUTOEXP	0x0100	/* auto "expansion" of cache section */
#define GBA_CONTIG_ADDR	0x0001	/* Mapping must be contiguous on GBA bus */
#define GBA_NOPART_MAP  0x0002	/* Must map entire request (no partial map) */
#define GBA_MAP_READ	0x0100	/* Request mapped for read by GBA (I/O write)*/
#define GBA_MAP_WRITE	0x0200	/* REquest mapped for write by GBA (I/O read)*/

/***************************************************************************
 * I/O related routines -- conditionally compiled
 ***************************************************************************/

#ifdef R6000

#define IOBADADDR(x,y) iobadaddr(x,y)

#else

#define IOBADADDR(x,y) badaddr(x,y)

#endif R6000


#endif	_SYS_VMEREG_
