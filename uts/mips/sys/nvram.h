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
/* $Header: nvram.h,v 1.11.3.2 90/05/10 06:30:58 wje Exp $ */

#ifndef	_SYS_NVRAM_
#define	_SYS_NVRAM_	1

/*
 * non-volatile ram addresses
 * NOTE: everything must fit within 50 bytes for the 146818 TOD chip
 */
#define	NVLEN_MAX	50
#define	NVADDR_BASE	0

#ifdef LANGUAGE_C
struct nvram_cmblk {
	short command;
	short offset;
	short len;
	char block[NVLEN_MAX];
};
#endif

/*
 * Commands.
 */
#define NVRAM_READ	0x1
#define NVRAM_WRITE	0x2

/*
 * Status.
 */
#define NVRAM_CMD_OK	0L	/* Something was copied into memory */
#define NVRAM_CMD_FAIL	1L	/* Guess what? */
  
/*
 * netaddr is used by network software to determine the internet
 * address, it should be a string containing the appropriate
 * network address in "." format
 */
#define	NVADDR_NETADDR	(NVADDR_BASE)
#define	NVLEN_NETADDR	16

/*
 * lbaud/rbaud are the initial baud rates for the duart
 * (e.g. "9600")
 */
#define	NVADDR_LBAUD	(NVADDR_NETADDR+NVLEN_NETADDR)
#define	NVLEN_LBAUD	5

#define	NVADDR_RBAUD	(NVADDR_LBAUD+NVLEN_LBAUD)
#define	NVLEN_RBAUD	5

/*
 * bootfile is the initial program loaded on an autoboot
 * (e.g. "bfs(0)mipsboot_le")
 */
#define	NVADDR_BOOTFILE	(NVADDR_RBAUD+NVLEN_RBAUD)
#define	NVLEN_BOOTFILE	20

/*
 * bootmode controls autoboots/warm starts/command mode on reset
 * "a" => autoboot on reset
 * "w" => warm start if restart block correct, else autoboot
 * anything else cause entry to command mode
 */
#define	NVADDR_BOOTMODE	(NVADDR_BOOTFILE+NVLEN_BOOTFILE)
#define	NVLEN_BOOTMODE	1

/*
 * console controls what consoles are enabled at power-up
 * 'a' indicates "all" consoles
 * 'r' indicates both local and remote uarts
 * anything else indicates only local uart
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
 * Should tty1 be enabled upon bringup. 0 => no, 1 => yes
 */
#define	NVADDR_TTY1	(NVADDR_CPUID+NVLEN_CPUID)
#define	NVLEN_TTY1	1

#define	NVLEN_TOTAL	(NVADDR_VALID+NVLEN_VALID)
#if NVLEN_TOTAL > NVLEN_MAX
# include "error -- non-volatile ram overflow"
#endif

#define	NVSTATE_TODVALID	0x1	/* tod can be trusted */
#define	NVSTATE_RAMVALID	0x2	/* nv ram can be trusted */

/*
 * NVRAM index constants for nvram_addrs structure in timer.c.  Kernel
 * will also use these so that it does not have to know the layout of
 * NVRAM.  New entries, regardless of where they appear in NVRAM, should
 * be placed at the end.
 */

#define NVINDEX_NETADDR		0x0
#define NVINDEX_LBAUD		0x1
#define NVINDEX_RBAUD		0x2
#define NVINDEX_BOOTFILE	0x3
#define NVINDEX_BOOTMODE	0x4
#define NVINDEX_CONSOLE		0x5
#define NVINDEX_STATE		0x6
#define NVINDEX_FAILCODE	0x7
#define NVINDEX_CPUID		0x8
#define NVINDEX_TTY1		0x9

#endif	_SYS_NVRAM_
