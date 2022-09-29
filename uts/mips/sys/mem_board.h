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
/* $Header: mem_board.h,v 1.5.4.2 90/05/10 06:28:09 wje Exp $ */

#ifndef	_SYS_MEM_BOARD_
#define	_SYS_MEM_BOARD_	1


/* Copyright 1986 by MIPS Computer Systems, Inc.
 */

/* MIPS Memory board I/O MAP Register defines
 * XXX this file should be linked/combined/coordinated with the 
 *	corresponding PROM file.
 */

#ifdef LANGUAGE_C
struct memdevice {
        u_short mem_iv; 	/* base +  0 interrupt vector register */
        u_short mem_ioaddr; 	/* base +  2 I/O address register */
        u_short mem_cntrl; 	/* base +  4 control register */
        u_short mem_addr; 	/* base +  6 memory address register */
        u_short mem_stat; 	/* base +  8 status register */
        u_short mem_evsyn; 	/* base +  a even syndrome register */
        u_short mem_odsyn; 	/* base +  c odd syndrome register */
        u_short mem_evchk; 	/* base +  e even check register */
        u_short mem_odchk; 	/* base + 10 odd check register */
        u_short mem_ledreg;  	/* base + 12 led register */
        u_short mem_promid;  	/* base + 14 ID prom register */
};
#endif LANGUAGE_C

#define	IOADDR_SHIFT	10		/* bits to shift io address */

/*
 * MEMORY register bit defines
 *
 * CONTROL register bit defines
 */
#define PRIV_ENAB	0x0001		/* enable local mem bus port */
#define PRIV_DISABLE	0x0000
#define SYS_ENAB	0x0002		/* enable vme mem bus port */
#define SYS_DISABLE	0x0000
#define EN_PRIVERR	0x0004		/* enable local bus errors */
#define EN_SYNERR	0x0008		/* enable syndrome latches */
#define ENINT_TERR	0x0010		/* interrupt on local transfer error */
#define ENINT_SGLERR	0x0020		/* interrupt on single bit ecc error */
#define ENINT_DBLERR	0x0040		/* interrupt on double bit ecc error */
#define INHIB_ECC	0x0080		/* inhibit ecc correction */
#define INHIB_WRTDATA	0x0100		/* inhibit data writes */
#define INHIB_CHKDATA	0x0200		/* inhibit chk bit writes */
#define REF_SLOT0	0x0400		/* refresh slot bit 0 */
#define REF_SLOT1	0x0800		/* refresh slot bit 1 */
#define REF_SLOT2	0x1000		/* refresh slot bit 2 */
#define INT_LEVELA	0x2000		/* interrupt level select bit */
#define INT_LEVELB	0x4000		/* interrupt level select bit */
#define AM29_ENABLE	0x8000		/* enable AM29 references */

#define	REFRESH_MASK	+(REF_SLOT0|REF_SLOT1|REF_SLOT2)
#define	REFRESH_SHIFT	10

/*
 * MEMADR register defines
 */
#define	MEMBASE_MASK	0x03ff		/* a31 - a22 address bits */
#define INTLV_BRD0	0x0000
#define INTLV_BRD1	0x0400		/* enable odd quad-word interleave */
#define ENAB_22		0x0800		/* enable a22 address compare */
#define ENAB_23		0x1000		/* enable a23 address compare */
#define ENAB_24		0x2000		/* enable a24 address compare */
#define INTLV		0x4000		/* enable interleaving */
#define	MEGDRAM		0x8000		/* enable 1 Meg DRAMS */

/*
 * useful MEMADR defines
 */
#define REFRESH0	0x0000		/* refresh on refresh cntr 0 */
#define REFRESH1	0x0400		/* refresh on refresh cntr 1 */
#define REFRESH2	0x0800		/* refresh on refresh cntr 2 */
#define REFRESH3	0x0c00		/* refresh on refresh cntr 3 */
#define REFRESH4	0x1000		/* refresh on refresh cntr 4 */
#define REFRESH5	0x1400		/* refresh on refresh cntr 5 */
#define REFRESH6	0x1800		/* refresh on refresh cntr 6 */
#define REFRESH7	0x1c00		/* refresh on refresh cntr 7 */
#define INT_LEVEL0	0x0000		/* no interrupt level defined */
#define INT_LEVEL1	0x4000		/* interrupt at level 1 */
#define INT_LEVEL3	0x2000		/* interrupt at level 3 */
#define INT_LEVEL7	0x6000		/* interrupt at level 7 */

/*
 * STATUS register defines
 * (names starting with N* are active low)
 */
#define	SYS_DBLERR	0x0001		/* double bit ecc error on vme port */
#define	NSYS_ALIGN	0x0002		/* alignment error on vme port */
#define	PRIV_DBLERR	0x0004		/* double bit ecc error on local port */
#define	PRIV_ALIGN	0x0008		/* alignment error on local port */
#define	NEVEN_DBLERR	0x0010		/* dbl err syn latched on even bank */
#define	NEVEN_ERR	0x0020		/* sgl err syn latched on even bank */
#define	NODD_DBLERR	0x0040		/* dbl err syn latched on odd bank */
#define	NODD_ERR	0x0080		/* sgl err syn latched on odd bank */

#define CNTRL_DFLT	(PRIV_ENAB|SYS_ENAB|INT_LEVEL3)
#define CNTRL_ERR    (EN_PRIVERR|EN_SYNERR|ENINT_TERR|ENINT_SGLERR|ENINT_DBLERR)

#ifdef LMEMHACK
#define	LMEM_CNTRL	0xbd000004
#define	LMEM_ADDR	0xbd000006
#endif LMEMHACK

/*
 * Definitions for prom local memory configuration code
 */
#define	LMEM_VEC_BASE	0x1		/* vme vectors for local memory */
#define	LMEM_ADR_BASE	0x1000		/* vme SA16 io address */
#define	LMEM_IO_SIZE	0x400		/* size of lmem io region */
#define LMEM_MAX	32		/* maximum number of memory boards */

/*
 * io space offsets
 */
#define	MEM_IV		0x0		/* interrupt vector */
#define	MEM_IOADDR	0x2		/* base address of io space */
#define	MEM_CNTRL	0x4		/* control register */
#define	MEM_ADDR	0x6		/* base address of memory */
#define	MEM_STAT	0x8		/* status register */
#define	MEM_EVSYN	0xa		/* even syndrome */
#define	MEM_ODSYN	0xc		/* odd syndrome */
#define	MEM_EVCHK	0xe		/* even check bits */
#define	MEM_ODCHK	0x10		/* odd check bits */
#define	MEM_LEDREG	0x12		/* board edge leds */
#define	MEM_PROMID	0x14		/* base of id prom */

/*
 * Offsets within id prom
 */
#define	ID_DRAMTYPE	0xd6		/* M->1MB, m->1MB half-pop, K->256K */

#endif	_SYS_MEM_BOARD_
