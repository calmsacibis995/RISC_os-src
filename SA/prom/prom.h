#ident "$Header: prom.h,v 1.24.1.3 91/01/02 18:56:28 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

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
#define	PROM_STACK		0xa0010000
/* NOTE: the following size may have some interaction with how cache's
 *	are sized (especially in a multi environment).
 *	PROM_STACK_SIZE should be >= MAXCACHE and no good values should
 *	be stored at top_of_stack-cache_page_size.
 */
#define PROM_STACK_SIZE		0x00010000
#define	PROM_STACK_BASE		0xa0000000
#define PROM_STACK_SIZE_SHIFT	16
#define MAX_CPUS		4

#define CHAR_0	48

#ifdef MULTI
/* load cpuid */
#define L_CPUID(result) \
	NVRAM(NVADDR_CPUID); \
	lw	result,v0; \
	subu	result,CHAR_0

/* calculate prom sp */
#define CALCULATE_SP(sp,slop) \
	L_CPUID(sp); \
	sll	sp,PROM_STACK_SIZE_SHIFT; \
	addu	sp,PROM_STACK_BASE+PROM_STACK_SIZE - (slop)

#else MULTI

#define L_CPUID(result) \
	move	result,zero

#define CALCULATE_SP(sp,slop) \
	li	sp,PROM_STACK-(slop)

#endif MULTI

/*
 * PROM_SR is the status register value used for normal prom operation
 * currently set to let bus error interrupts occur
 */
#define	PROM_SR		(SR_IMASK7|SR_IEC)

/*
 * width defines for memory operations
 */
#define	SW_BYTE		1
#define	SW_HALFWORD	2
#define	SW_WORD		4

/*
 * non-volatile ram addresses
 * NOTE: everything must fit within 50 bytes for the 146818 TOD chip
 *
 * NB:
 *	The 50 byte limitation is for backward compatiblilty only.
 *	Most machines use the MK48T02 clock calendar/ nvram chip.
 *	This chip actually has 2K of nvram (minus 8 bytes for the clock
 *	registers)
 *
 * NVRAM is now divided into MIPS space and OEM space.  No MIPS nvram
 * variables are to be stored in OEM space.  This must be adhered to,
 * as we have contractual obligations to provide nvram space for OEM's.
 *
 * OEM nvram space is 512 bytes, located in the address range:
 *		NVADDR_BASE + 1024
 *		    :
 *		NVADDR_BASE + 1536
 */

#define NVSTART_OEM	1024
#define NVLEN_OEM	512	

#define	NVLEN_MAX	50
#ifdef R3030
#define	NVADDR_BASE	1536
#else
#define	NVADDR_BASE	0
#endif

/*
 * netaddr is used by network software to determine the internet
 * address, to save space this is stored as four bytes and converted
 * coming into/outof nvram.
 */
#define	NVADDR_NETADDR	(NVADDR_BASE)
#define	NVLEN_NETADDR	4

/*
 * lbaud/rbaud are the initial baud rates for the duart
 * (e.g. "9600")
 */
#define	NVADDR_LBAUD	(NVADDR_NETADDR+NVLEN_NETADDR)
#define	NVLEN_LBAUD	1

#define	NVADDR_RBAUD	(NVADDR_LBAUD+NVLEN_LBAUD)
#define	NVLEN_RBAUD	1

/*
 * bootfile is the initial program loaded on an autoboot
 * (e.g. "bfs(0)mipsboot_le")
 */
#ifdef R3030
#define	NVADDR_DUMMY	(NVADDR_RBAUD+NVLEN_RBAUD)
#define	NVLEN_DUMMY	20
#else
#define NVADDR_BOOTFILE (NVADDR_RBAUD+NVLEN_RBAUD)
#define NVLEN_BOOTFILE  20
#endif R3030
/*
 * bootmode controls autoboots/warm starts/command mode on reset
 * "a" => autoboot on reset
 * "w" => warm start if restart block correct, else autoboot
 * anything else cause entry to command mode
 */
#ifdef R3030
#define	NVADDR_BOOTMODE	(NVADDR_DUMMY+NVLEN_DUMMY)
#define	NVLEN_BOOTMODE	1
#else
#define NVADDR_BOOTMODE (NVADDR_BOOTFILE+NVLEN_BOOTFILE)
#define NVLEN_BOOTMODE  1
#endif R3030
/*
 * console controls what consoles/uarts are enabled at power-up
 */
#define	NVADDR_CONSOLE	(NVADDR_BOOTMODE+NVLEN_BOOTMODE)
#define	NVLEN_CONSOLE	1

/*
 * state maintains the current validity of the tod clock and
 * non-volatile ram
 * see NVSTATE_* definitions below
 */
#define	NVADDR_STATE	(NVADDR_CONSOLE+NVLEN_CONSOLE)
#define	NVLEN_STATE	1

/*
 * failcode is used by the power-on diagnostics to save a failure
 * code for use by service techs
 */
#define	NVADDR_FAILCODE	(NVADDR_STATE+NVLEN_STATE)
#define	NVLEN_FAILCODE	1

/*
 * slot is the logical slot number for multi-cpu's to tell which slot
 *	they are in.
 */
#define	NVADDR_CPUID	(NVADDR_FAILCODE+NVLEN_FAILCODE)
#define	NVLEN_CPUID	1

/*
 * Flag is a bit field that controls various things
 * a 1 in that bit position indicates "yes" a 0 indicates "no"
 *	bit 0	--	Mask the printing of "MIPS" from the prom/sash startup?
 *	bit7..4 --	Count incomplete hw resets for diags monitor interface.
 * see NVFLAG_* definitions below
 */
#define	NVADDR_FLAG	(NVADDR_CPUID+NVLEN_CPUID)
#define	NVLEN_FLAG	1

#define	NVADDR_PONMASK	(NVADDR_FLAG+NVLEN_FLAG)
#define	NVLEN_PONMASK	4

/*
 *	save epc and ra in nvram
 */
#define	NVADDR_RESETEPC	(NVADDR_PONMASK+NVLEN_PONMASK)
#define	NVLEN_RESETEPC	4

#define	NVADDR_RESETRA	(NVADDR_RESETEPC+NVLEN_RESETEPC)
#define	NVLEN_RESETRA	4
/*
 *	env variable to say if we want parity turned on in memory, may
 *	go away.
 */
#define	NVADDR_MEMPARITY	(NVADDR_RESETRA+NVLEN_RESETRA)
#define	NVLEN_MEMPARITY	4

#define NVADDR_CHKSUM	(NVADDR_MEMPARITY+NVLEN_MEMPARITY)
#define	NVLEN_CHKSUM	1

#define	NVLEN_TOTAL	(NVADDR_CHKSUM+NVLEN_CHKSUM-NVADDR_BASE)

/*
** New nvram area following the first 50 bytes(old area)
** New area + old area = first 512 bytes (0-511)
** Note: This new area is not for BRDTYPE_R2300 plateform.
*/
/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#define	NVADDR_MAGIC	(NVADDR_BASE+50)
#define	NVLEN_MAGIC	8

#define	NVADDR_VENDOR	(NVADDR_MAGIC+NVLEN_MAGIC)
#define	NVLEN_VENDOR	16

#define	NVADDR_MODEL	(NVADDR_VENDOR+NVLEN_VENDOR)
#define	NVLEN_MODEL	16

#define NVADDR_ROOT     (NVADDR_MODEL+NVLEN_MODEL)
#define NVLEN_ROOT      8

/*
 * The following variable is only needed for the 62xx machines.  It is
 * unconditionally defined so that "sash" may call get_nvram to obtain
 * the proper value.  Other machines may overlay another parameter here.
 */

#define NVADDR_IOAPARM	(NVADDR_ROOT+NVLEN_ROOT)
#define NVLEN_IOAPARM	1

/*
 * use_bootparams, should we use bottparams to find root and swap
 * 0 => no, 1 => yes.
 */

#define NVADDR_UBOOTPARAMS	(NVADDR_ROOT+NVLEN_ROOT)
#ifdef R3030
#define NVLEN_UBOOTPARAMS	1

#define NVADDR_BOOTFILE		(NVADDR_UBOOTPARAMS+NVLEN_UBOOTPARAMS)
#define NVLEN_BOOTFILE		64

/*
 * cntrl & caps lock keys switched? 1 == yes, 0 == no
 */
#define	NVADDR_KEYSWTCH	(NVADDR_BOOTFILE+NVLEN_BOOTFILE)
#define	NVLEN_KEYSWTCH	1

/*
 * type of keyboard used: MIPS or AT 
 */
#define	NVADDR_KEYBOARD	(NVADDR_KEYSWTCH+NVLEN_KEYSWTCH)
#define	NVLEN_KEYBOARD	4

/*
 * scsi_id (0-7) - default to 7
 */
#define	NVADDR_SCSI_ID	(NVADDR_KEYBOARD+NVLEN_KEYBOARD)
#define	NVLEN_SCSI_ID	1

/*
 * should we reset the scsi bus on reset (0 or 1) - default to 1
 */
#define	NVADDR_SCSI_RESET	(NVADDR_SCSI_ID+NVLEN_SCSI_ID)
#define	NVLEN_SCSI_RESET 1

/*
 * probe RAT bus for color card or digi-board (1 or 0) - default to 1
 */
#define	NVADDR_BUS_TEST	(NVADDR_SCSI_RESET+NVLEN_SCSI_RESET)
#define	NVLEN_BUS_TEST	1

/*
 * language - a string of up to 8 chars - default to "american" 
 */
#define NVADDR_LANGUAGE (NVADDR_BUS_TEST+NVLEN_BUS_TEST)
#define NVLEN_LANGUAGE 	8

#else
#define	NVLEN_UBOOTPARAMS	0
/* define the following symbols just to make the compiles work for non-3030 */
#define NVADDR_SCSI_ID		(NVADDR_UBOOTPARAMS+NVLEN_UBOOTPARAMS)
#define NVLEN_SCSI_ID		0
#define NVADDR_SCSI_RESET	(NVADDR_UBOOTPARAMS+NVLEN_UBOOTPARAMS)
#define NVLEN_SCSI_RESET	0
#define NVADDR_LANGUAGE		(NVADDR_UBOOTPARAMS+NVLEN_UBOOTPARAMS)
#define NVLEN_LANGUAGE		0
#endif

#define	NVADDR_NEW_AREA_START	NVADDR_MAGIC
#define	NVLEN_NEW_AREA_TOTAL	(512 - 50)

/*- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -*/

#if NVLEN_TOTAL > NVSTART_OEM
# include "error -- non-volatile ram overflow"
#endif

#define	NVSTATE_TODVALID	0x1	/* tod can be trusted */
#define	NVSTATE_RAMVALID	0x2	/* nv ram can be trusted */

#define NVFLAG_RSTMASK		0xF0	/* hardware reset counter */
#define NVFLAG_RSTSHIFT		4

/*
 * NVRAM index constants for nvram_addrs structure in timer.c.  Kernel
 * will also use these so that it does not have to know the layout of
 * NVRAM.  New entries, regardless of where they appear in NVRAM, should
 * be placed at the end.
 */

#define NVINDEX_NETADDR		0x0
#define NVINDEX_LBAUD		0x1
#define NVINDEX_RBAUD		0x2
#define NVINDEX_DUMMY		0x3
#define NVINDEX_BOOTMODE	0x4
#define NVINDEX_CONSOLE		0x5
#define NVINDEX_STATE		0x6
#define NVINDEX_FAILCODE	0x7
#define NVINDEX_CPUID		0x8
#define NVINDEX_FLAG		0x9
#define NVINDEX_PONMASK		0xa
#define NVINDEX_RESETEPC	0xb
#define NVINDEX_RESETRA		0xc
#define NVINDEX_MEMPARITY	0xd
#define	NVINDEX_UBOOTPARAMS	0xe
#define NVINDEX_CHKSUM		0xf

/*
 * Misc constants
 */
#ifdef R3030
#define	AUTOBOOT_DELAY	1		/* seconds to abort autoboots */
#define	DEFAULT_BOOTFILE "dksd(0,0,8)sash"	/* boot standalone shell */
#else
#define	AUTOBOOT_DELAY	20		/* seconds to abort autoboots */
#define	DEFAULT_BOOTFILE "dkip(0,0,8)sash"	/* boot standalone shell */
#endif R3030

#define	streq(a,b)	(strcmp(a,b) == 0)

/*
 * RMW_TOGGLE -- cpu board address which when read
 * cause a read/modify/write cycle to occur with next read and
 * write cycles
 */
#define	RMW_TOGGLE	0xbe400003

/*
 * startup led sequence values
 */
#define	MEMCFG_PATTERN		0x21	/* memory configured */
#define	WARMST_PATTERN		0x22	/* warm start attempted */
#define	ZEROBSS_PATTERN		0x23	/* prom bss zero'ed */
#define	CACHE_PATTERN		0x24	/* cache initialized */
#define	SAIO_PATTERN		0x25	/* saio initialized */
#define	CACHE2_PATTERN		0x26	/* cache initialized */
#define	ENV_PATTERN		0x27	/* environment initialized */
#define	SAIO2_PATTERN		0x28	/* saio initialized */
#define	ZEROMEM_PATTERN		0x29	/* memory cleared */
#define	LMEM_PATTERN		0x2a	/* in local memory config */
#define	LMEM_WAIT_PATTERN	0x2b	/* lmem waiting for exception */
#define	LMEM_ACK_PATTERN	0x2c	/* lmem acking vme interrupt */
#define	LMEM_NOMEM_PATTERN	0x2d	/* no local memory found */
#define	LMEM_ERROR_PATTERN	0x2e	/* error in config code */
#define CPUID_RESET_PATTERN	0x2f	/* cpuid had to be reset to 0 */
#define	LMEM_FAULT_PATTERN	0x31	/* unexpected exception in lmem */
#define	LMEM_RESET_PATTERN	0x32	/* resetting local memory */
#define	LMEM_RSTDONE_PATTERN	0x33	/* local memory reset complete */
#define	LMEM_CLRVME_PATTERN	0x34	/* clearing pending vme intrs */
#define	BEV_UTLBMISS_PATTERN	0x35	/* unexpected bev utlbmiss */
#define	BEV_GENERAL_PATTERN	0x36	/* unexpected bev exception */

#define LED_TYPE_A		0x1	/* Init LED settings */
#define LED_TYPE_B		0x2	/* Init LED settings */
#define LED_TYPE_C		0x3	/* Init LED settings */
#define LED_TYPE_D		0x4	/* Init LED settings */
#define LED_TYPE_E		0x5	/* Init LED settings */
#define LED_TYPE_F		0x6	/* Init LED settings */
#define LED_TYPE_G		0x7	/* Init LED settings */
#define LED_TYPE_H		0x8	/* Init LED settings */

/*
 * HACK
 * define eprintf to be printf for now, maybe implement as printf
 * to console() in future
 */
#define	eprintf	printf
