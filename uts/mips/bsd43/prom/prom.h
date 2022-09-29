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
/* $Header: prom.h,v 1.6.3.2 90/05/10 04:45:35 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * prom.h -- prom monitor definitions
 */

/*
 * catch bogus compiles
 */
#if defined(MIPSEB) && defined(MIPSEL)
# include "error -- both MIPSEB and MIPSEL defined"
#endif

#if !defined(MIPSEB) && !defined(MIPSEL)
# include "error -- neither MIPSEB or MIPSEL defined"
#endif

/*
 * PROM_STACK is the address of the first word above the prom stack
 * the prom stack grows downward from the first word less than PROM_STACK
 */
#define	BSD43_PROM_STACK	0xa0010000

/*
 * PROM_SR is the status register value used for normal prom operation
 * currently set to let bus error interrupts occur
 */
#define	BSD43_PROM_SR		(BSD43_SR_IMASK7|BSD43_SR_IEC)

/*
 * width defines for memory operations
 */
#define	BSD43_SW_BYTE		1
#define	BSD43_SW_HALFWORD	2
#define	BSD43_SW_WORD		4

/*
 * non-volatile ram addresses
 * NOTE: everything must fit within 50 bytes for the 146818 TOD chip
 */
#define	BSD43_NVLEN_MAX	50
#define	BSD43_NVADDR_BASE	0

/*
 * netaddr is used by network software to determine the internet
 * address, it should be a string containing the appropriate
 * network address in "." format
 */
#define	BSD43_NVADDR_NETADDR	(BSD43_NVADDR_BASE)
#define	BSD43_NVLEN_NETADDR	16

/*
 * lbaud/rbaud are the initial baud rates for the duart
 * (e.g. "9600")
 */
#define	BSD43_NVADDR_LBAUD	(BSD43_NVADDR_NETADDR+BSD43_NVLEN_NETADDR)
#define	BSD43_NVLEN_LBAUD	5

#define	BSD43_NVADDR_RBAUD	(BSD43_NVADDR_LBAUD+BSD43_NVLEN_LBAUD)
#define	BSD43_NVLEN_RBAUD	5

/*
 * bootfile is the initial program loaded on an autoboot
 * (e.g. "bfs(0)mipsboot_le")
 */
#define	BSD43_NVADDR_BOOTFILE	(BSD43_NVADDR_RBAUD+BSD43_NVLEN_RBAUD)
#define	BSD43_NVLEN_BOOTFILE	20

/*
 * bootmode controls autoboots/warm starts/command mode on reset
 * "a" => autoboot on reset
 * "w" => warm start if restart block correct, else autoboot
 * anything else cause entry to command mode
 */
#define	BSD43_NVADDR_BOOTMODE	(BSD43_NVADDR_BOOTFILE+BSD43_NVLEN_BOOTFILE)
#define	BSD43_NVLEN_BOOTMODE	1

/*
 * console controls what consoles are enabled at power-up
 * 'a' indicates "all" consoles
 * 'r' indicates both local and remote uarts
 * anything else indicates only local uart
 */
#define	BSD43_NVADDR_CONSOLE	(BSD43_NVADDR_BOOTMODE+BSD43_NVLEN_BOOTMODE)
#define	BSD43_NVLEN_CONSOLE	1

/*
 * state maintains the current validity of the tod clock and
 * non-volatile ram
 * see NVSTATE_* definitions below
 */
#define	BSD43_NVADDR_STATE	(BSD43_NVADDR_CONSOLE+BSD43_NVLEN_CONSOLE)
#define	BSD43_NVLEN_STATE	1

/*
 * failcode is used by the power-on diagnostics to save a failure
 * code for use by service techs
 */
#define	BSD43_NVADDR_FAILCODE	(BSD43_NVADDR_STATE+BSD43_NVLEN_STATE)
#define	BSD43_NVLEN_FAILCODE	1

#define	BSD43_NVLEN_TOTAL	(NVADDR_VALID+NVLEN_VALID)
#if BSD43_NVLEN_TOTAL > BSD43_NVLEN_MAX
# include "error -- non-volatile ram overflow"
#endif

#define	BSD43_NVSTATE_TODVALID	0x1	/* tod can be trusted */
#define	BSD43_NVSTATE_RAMVALID	0x2	/* nv ram can be trusted */

/*
 * Misc constants
 */
#define	BSD43_AUTOBOOT_DELAY	20		/* seconds to abort autoboots */
#define	BSD43_DEFAULT_BOOTFILE "dkip(0,0,8)sash"	/* boot standalone shell */

#define	bsd43_streq(a,b)	(bsd43_(strcmp)(a,b) == 0)

/*
 * RMW_TOGGLE -- cpu board address which when read
 * cause a read/modify/write cycle to occur with next read and
 * write cycles
 */
#define	BSD43_RMW_TOGGLE	0xbe400003

/*
 * startup led sequence values
 */
#define	BSD43_MEMCFG_PATTERN		0x21	/* memory configured */
#define	BSD43_WARMST_PATTERN		0x22	/* warm start attempted */
#define	BSD43_ZEROBSS_PATTERN		0x23	/* prom bss zero'ed */
#define	BSD43_CACHE_PATTERN		0x24	/* cache initialized */
#define	BSD43_SAIO_PATTERN		0x25	/* saio initialized */
#define	BSD43_CACHE2_PATTERN		0x26	/* cache initialized */
#define	BSD43_ENV_PATTERN		0x27	/* environment initialized */
#define	BSD43_SAIO2_PATTERN		0x28	/* saio initialized */
#define	BSD43_ZEROMEM_PATTERN		0x29	/* memory cleared */
#define	BSD43_LMEM_PATTERN		0x2a	/* in local memory config */
#define	BSD43_LMEM_WAIT_PATTERN	0x2b	/* lmem waiting for exception */
#define	BSD43_LMEM_ACK_PATTERN	0x2c	/* lmem acking vme interrupt */
#define	BSD43_LMEM_NOMEM_PATTERN	0x2d	/* no local memory found */
#define	BSD43_LMEM_ERROR_PATTERN	0x2e	/* error in config code */
#define	BSD43_LMEM_FAULT_PATTERN	0x31	/* unexpected exception in lmem */
#define	BSD43_LMEM_RESET_PATTERN	0x32	/* resetting local memory */
#define	BSD43_LMEM_RSTDONE_PATTERN	0x33	/* local memory reset complete */
#define	BSD43_LMEM_CLRVME_PATTERN	0x34	/* clearing pending vme intrs */
#define	BSD43_BEV_UTLBMISS_PATTERN	0x35	/* unexpected bev utlbmiss */
#define	BSD43_BEV_GENERAL_PATTERN	0x36	/* unexpected bev exception */

/*
 * HACK
 * define eprintf to be printf for now, maybe implement as printf
 * to console() in future
 */
#define	bsd43_eprintf	printf

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define AUTOBOOT_DELAY BSD43_AUTOBOOT_DELAY
#   define BEV_GENERAL_PATTERN BSD43_BEV_GENERAL_PATTERN
#   define BEV_UTLBMISS_PATTERN BSD43_BEV_UTLBMISS_PATTERN
#   define CACHE2_PATTERN BSD43_CACHE2_PATTERN
#   define CACHE_PATTERN BSD43_CACHE_PATTERN
#   define DEFAULT_BOOTFILE BSD43_DEFAULT_BOOTFILE
#   define ENV_PATTERN BSD43_ENV_PATTERN
#   define LMEM_ACK_PATTERN BSD43_LMEM_ACK_PATTERN
#   define LMEM_CLRVME_PATTERN BSD43_LMEM_CLRVME_PATTERN
#   define LMEM_ERROR_PATTERN BSD43_LMEM_ERROR_PATTERN
#   define LMEM_FAULT_PATTERN BSD43_LMEM_FAULT_PATTERN
#   define LMEM_NOMEM_PATTERN BSD43_LMEM_NOMEM_PATTERN
#   define LMEM_PATTERN BSD43_LMEM_PATTERN
#   define LMEM_RESET_PATTERN BSD43_LMEM_RESET_PATTERN
#   define LMEM_RSTDONE_PATTERN BSD43_LMEM_RSTDONE_PATTERN
#   define LMEM_WAIT_PATTERN BSD43_LMEM_WAIT_PATTERN
#   define MEMCFG_PATTERN BSD43_MEMCFG_PATTERN
#   define NVADDR_BASE BSD43_NVADDR_BASE
#   define NVADDR_BOOTFILE BSD43_NVADDR_BOOTFILE
#   define NVADDR_BOOTMODE BSD43_NVADDR_BOOTMODE
#   define NVADDR_CONSOLE BSD43_NVADDR_CONSOLE
#   define NVADDR_FAILCODE BSD43_NVADDR_FAILCODE
#   define NVADDR_LBAUD BSD43_NVADDR_LBAUD
#   define NVADDR_NETADDR BSD43_NVADDR_NETADDR
#   define NVADDR_RBAUD BSD43_NVADDR_RBAUD
#   define NVADDR_STATE BSD43_NVADDR_STATE
#   define NVLEN_BOOTFILE BSD43_NVLEN_BOOTFILE
#   define NVLEN_BOOTMODE BSD43_NVLEN_BOOTMODE
#   define NVLEN_CONSOLE BSD43_NVLEN_CONSOLE
#   define NVLEN_FAILCODE BSD43_NVLEN_FAILCODE
#   define NVLEN_LBAUD BSD43_NVLEN_LBAUD
#   define NVLEN_MAX BSD43_NVLEN_MAX
#   define NVLEN_NETADDR BSD43_NVLEN_NETADDR
#   define NVLEN_RBAUD BSD43_NVLEN_RBAUD
#   define NVLEN_STATE BSD43_NVLEN_STATE
#   define NVLEN_TOTAL BSD43_NVLEN_TOTAL
#   define NVSTATE_RAMVALID BSD43_NVSTATE_RAMVALID
#   define NVSTATE_TODVALID BSD43_NVSTATE_TODVALID
#   define PROM_SR BSD43_PROM_SR
#   define PROM_STACK BSD43_PROM_STACK
#   define RMW_TOGGLE BSD43_RMW_TOGGLE
#   define SAIO2_PATTERN BSD43_SAIO2_PATTERN
#   define SAIO_PATTERN BSD43_SAIO_PATTERN
#   define SW_BYTE BSD43_SW_BYTE
#   define SW_HALFWORD BSD43_SW_HALFWORD
#   define SW_WORD BSD43_SW_WORD
#   define WARMST_PATTERN BSD43_WARMST_PATTERN
#   define ZEROBSS_PATTERN BSD43_ZEROBSS_PATTERN
#   define ZEROMEM_PATTERN BSD43_ZEROMEM_PATTERN
#   define eprintf bsd43_eprintf
#   define streq bsd43_streq
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


