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
/* $Header: vmereg.h,v 1.6.3.2 90/05/10 04:45:01 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * VMEbus related defines
 */

#define	BSD43_VME_MINIPL	1
#define	BSD43_VME_MAXIPL	7

/*
 * VME interrupt control registers
 */
#define	BSD43_VME_IMR		0x1e020003	/* interrupt mask */
#define	BSD43_VME_ISR		0x1e020007	/* interrupt status */
#define	BSD43_VME_IACK	0x1df00002	/* interrupt acknowledge */

#define	BSD43_NVMEVEC		0x100		/* size of vmevec_tbl */

#define	BSD43_VME_VEC_POOL	0x10		/* first dynamically assigned vec */

#ifdef LANGUAGE_C
struct bsd43_(vmevec) {
	int (*vme_intr)();
	int vme_unit;
};
#endif LANGUAGE_C

/*
 * VME I/O space defines
 */
#define	BSD43_VME_A16NPBASE	0x1d100000	/* a16 non-privileged address sp */
#define	BSD43_VME_A16NPAMOD	0x29		/* a16 non-privileged addr modifier */

#define	BSD43_VME_A16SBASE	0x1d000000	/* a16 supervisor address sp */
#define	BSD43_VME_A16SAMOD	0x2D		/* a16 supervisor addr modifier */

/*
 * TODO: A24 NP stuff should go away since it doesn't exist
 */
#define	BSD43_VME_A24NPBASE	0x1c000000	/* a24 non-privileged address sp */
#define	BSD43_VME_A24NPAMOD	0x39		/* a24 non-privileged addr modifier */

#define	BSD43_VME_A24SBASE	0x1c000000	/* a24 supervisor address sp */
#define	BSD43_VME_A24SAMOD	0x3D		/* a24 supervisor addr modifier */

#define	BSD43_VME_A32NPAMOD	0x09		/* a32 non-privileged addr modifier */

#ifdef LANGUAGE_C
/*
 * VME address conversion macros
 */
#define	BSD43_VMESA16_TO_PHYS(x)	((unsigned)(x)+BSD43_VME_A16SBASE)
#define	BSD43_VMENPA16_TO_PHYS(x)	((unsigned)(x)+BSD43_VME_A16NPBASE)

/*
 * TODO: should delete
 */
#define	BSD43_VMENPA24_TO_PHYS(x)	((unsigned)(x)+BSD43_VME_A24NPBASE)
#define	BSD43_VMESA24_TO_PHYS(x)	((unsigned)(x)+BSD43_VME_A24SBASE)

#ifdef KERNEL
extern struct bsd43_(vmevec) bsd43_(vmevec_tbl)[];	/* interrupt dispatch table */
#endif KERNEL
#endif LANGUAGE_C

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define NVMEVEC BSD43_NVMEVEC
#   define VMENPA16_TO_PHYS BSD43_VMENPA16_TO_PHYS
#   define VMENPA24_TO_PHYS BSD43_VMENPA24_TO_PHYS
#   define VMESA16_TO_PHYS BSD43_VMESA16_TO_PHYS
#   define VMESA24_TO_PHYS BSD43_VMESA24_TO_PHYS
#   define VME_A16NPAMOD BSD43_VME_A16NPAMOD
#   define VME_A16NPBASE BSD43_VME_A16NPBASE
#   define VME_A16SAMOD BSD43_VME_A16SAMOD
#   define VME_A16SBASE BSD43_VME_A16SBASE
#   define VME_A24NPAMOD BSD43_VME_A24NPAMOD
#   define VME_A24NPBASE BSD43_VME_A24NPBASE
#   define VME_A24SAMOD BSD43_VME_A24SAMOD
#   define VME_A24SBASE BSD43_VME_A24SBASE
#   define VME_A32NPAMOD BSD43_VME_A32NPAMOD
#   define VME_IACK BSD43_VME_IACK
#   define VME_IMR BSD43_VME_IMR
#   define VME_ISR BSD43_VME_ISR
#   define VME_MAXIPL BSD43_VME_MAXIPL
#   define VME_MINIPL BSD43_VME_MINIPL
#   define VME_VEC_POOL BSD43_VME_VEC_POOL
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


