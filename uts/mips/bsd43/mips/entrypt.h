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
/* $Header: entrypt.h,v 1.6.3.2 90/05/10 04:41:38 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * entrypt.h -- misc. defines of interest to standalones and kernels
 */

/*
 * memory map assumed by prom and standalone system
 *
 * physical	kseg1			use
 *
 * 0x1fc20000	0xbfc20000
 * to					prom text and read-only data
 * 0x1fc00000	0xbfc00000		(in cpu board "prom space")
 *
 * (Top of RAM - 8K) downward		sash and standalone program stack
 *		|			( - 8K to preserve kernel message bufs)
 *		V			(standalone programs grow their stack
 *					 immediately below sash's stack)
 *
 *		^
 *		|
 * 0x00100000	0xa0100000 upward	sash program text, data, and bss
 *
 *		^
 *		|
 * 0x00020000	0xa0020000 upward	standalone program text, data, and bss
 *					(kernel is loaded here, also)
 *
 * 0x0001ffff	0xa001ffff downward	dbgmon stack
 *		|
 *		V
 *
 *		^
 *		|
 * 0x00010000	0xa0010000 upward	dbgmon text, data, and bss
 *
 * 0x0000ffff	0xa000ffff downward	prom monitor stack
 *		|
 *		V
 *
 *		^
 *		|
 * 0x00000500	0xa0000500 upward	prom monitor bss
 *
 * 0x000004ff	0xa00004ff
 * to					restart block
 * 0x00000400	0xa0000400
 *
 * 0x000003ff	0xa00003ff
 * to					general exception code
 * 0x00000080	0xa0000080		(note cpu addresses as 0x80000080!)
 *
 * 0x0000007f	0xa000007f
 * to					utlbmiss exception code
 * 0x00000000	0xa0000000		(note cpu addresses as 0x80000000!)
 */

#define	BSD43_PROM_STACK	0x80010000

/*
 * Prom entry points
 */

/*
 * Return control to prom entry points
 *
 * RESET	transferred to on hardware reset, configures MIPS boards,
 *		runs diags, check for appropriate auto boot action in
 *		"bootmode" environment variable and performs that action.
 *
 * EXEC		called to utilize prom to boot new image.  After the booted
 *		program returns control can either be returned to the
 *		original caller of the exec routine or to the prom monitor.
 *		(to return to the original caller, the new program must
 *		not destroy any text, data, or stack of the parent.  the
 *		new programs stack continues on the parents stack.
 *
 * RESTART	re-enter the prom command parser, do not reset prom state
 *
 * REINIT	reinitialize prom state and re-enter the prom command parser
 *
 * REBOOT	check for appropriate bootmode and perform, no configuration
 *		or diags run
 *
 * AUTOBOOT	perform an autoboot sequence, no configuration or diags run
 *
 */
#define	BSD43_PROM_ENTRY(x)	(BSD43_R_VEC+((x)*8))

#define	BSD43_PROM_RESET	BSD43_PROM_ENTRY(0)	/* run diags, check bootmode, reinit */
#define	BSD43_PROM_EXEC	BSD43_PROM_ENTRY(1)	/* load new program image */
#define	BSD43_PROM_RESTART	BSD43_PROM_ENTRY(2)	/* re-enter monitor command loop */
#define	BSD43_PROM_REINIT	BSD43_PROM_ENTRY(3)	/* re-init monitor, then cmd loop */
#define	BSD43_PROM_REBOOT	BSD43_PROM_ENTRY(4)	/* check bootmode, no config */
#define	BSD43_PROM_AUTOBOOT	BSD43_PROM_ENTRY(5)	/* autoboot the system */
/*
 * these routines access prom "saio" routines, and may be used
 * by standalone programs that would like to use prom io
 */
#define	BSD43_PROM_OPEN	BSD43_PROM_ENTRY(6)
#define	BSD43_PROM_READ	BSD43_PROM_ENTRY(7)
#define	BSD43_PROM_WRITE	BSD43_PROM_ENTRY(8)
#define	BSD43_PROM_IOCTL	BSD43_PROM_ENTRY(9)
#define	BSD43_PROM_CLOSE	BSD43_PROM_ENTRY(10)
#define	BSD43_PROM_GETCHAR	BSD43_PROM_ENTRY(11)	/* getchar from console */
#define	BSD43_PROM_PUTCHAR	BSD43_PROM_ENTRY(12)	/* putchar to console */
#define	BSD43_PROM_SHOWCHAR	BSD43_PROM_ENTRY(13)	/* show a char visibly */
#define	BSD43_PROM_GETS	BSD43_PROM_ENTRY(14)	/* gets with editing */
#define	BSD43_PROM_PUTS	BSD43_PROM_ENTRY(15)	/* puts to console */
#define	BSD43_PROM_PRINTF	BSD43_PROM_ENTRY(16)	/* kernel style printf to console */
/*
 * prom protocol entry points
 */
#define	BSD43_PROM_INITPROTO	BSD43_PROM_ENTRY(17)	/* initialize protocol */
#define	BSD43_PROM_PROTOENABLE BSD43_PROM_ENTRY(18)	/* enable protocol mode */
#define	BSD43_PROM_PROTODISABLE BSD43_PROM_ENTRY(19)/* disable protocol mode */
#define	BSD43_PROM_GETPKT	BSD43_PROM_ENTRY(20)	/* get protocol packet */
#define	BSD43_PROM_PUTPKT	BSD43_PROM_ENTRY(21)	/* put protocol packet */
/*
 * read-modify-write routine use special cpu board circuitry to accomplish
 * vme bus r-m-w cycles.  all routines are similar to:
 *	unsigned char
 *	orb_rmw(addr, mask)
 *	unsigned char *addr;
 *	unsigned mask;
 *	{
 *		register unsigned rval;
 *
 *		lockbus();
 *		rval = *addr;
 *		*addr = rval & mask;
 *		unlockbus();
 *		return(rval);
 *	}
 */
#define	BSD43_PROM_ORW_RMW	BSD43_PROM_ENTRY(22)	/* r-m-w version of or word */
#define	BSD43_PROM_ORH_RMW	BSD43_PROM_ENTRY(23)	/* r-m-w version of or halfword */
#define	BSD43_PROM_ORB_RMW	BSD43_PROM_ENTRY(24)	/* r-m-w version of or byte */
#define	BSD43_PROM_ANDW_RMW	BSD43_PROM_ENTRY(25)	/* r-m-w version of and word */
#define	BSD43_PROM_ANDH_RMW	BSD43_PROM_ENTRY(26)	/* r-m-w version of and halfword */
#define	BSD43_PROM_ANDB_RMW	BSD43_PROM_ENTRY(27)	/* r-m-w version of and byte */
/*
 * cache control entry points
 * flushcache is called without arguments and invalidates entire contents
 *	of both i and d caches
 * clearcache is called with a base address and length (where address is
 * 	either K0, K1, or physical) and clears both i and d cache for entries
 * 	that alias to specified address range.
 */
#define	BSD43_PROM_FLUSHCACHE	BSD43_PROM_ENTRY(28)	/* flush entire cache */
#define	BSD43_PROM_CLEARCACHE	BSD43_PROM_ENTRY(29)	/* clear_cache(addr, len) */
/*
 * The following entry points are sole to reduce the size of the debug
 * monitor and could be removed by including the appropriate code in the
 * debugger
 *
 * Libc-ish entry points
 */
#define	BSD43_PROM_SETJMP	BSD43_PROM_ENTRY(30)	/* save stack state */
#define	BSD43_PROM_LONGJMP	BSD43_PROM_ENTRY(31)	/* restore stack state */
#define	BSD43_PROM_BEVUTLB	BSD43_PROM_ENTRY(32)	/* utlbmiss boot exception vector */
#define	BSD43_PROM_GETENV	BSD43_PROM_ENTRY(33)	/* get environment variable */
#define	BSD43_PROM_SETENV	BSD43_PROM_ENTRY(34)	/* set environment variable */
#define	BSD43_PROM_ATOB	BSD43_PROM_ENTRY(35)	/* convert ascii to binary */
#define	BSD43_PROM_STRCMP	BSD43_PROM_ENTRY(36)	/* string compare */
#define	BSD43_PROM_STRLEN	BSD43_PROM_ENTRY(37)	/* string length */
#define	BSD43_PROM_STRCPY	BSD43_PROM_ENTRY(38)	/* string copy */
#define	BSD43_PROM_STRCAT	BSD43_PROM_ENTRY(39)	/* string concat */
/*
 * command parser entry points
 */
#define	BSD43_PROM_PARSER	BSD43_PROM_ENTRY(40)	/* command parser */
#define	BSD43_PROM_RANGE	BSD43_PROM_ENTRY(41)	/* range parser */
#define	BSD43_PROM_ARGVIZE	BSD43_PROM_ENTRY(42)	/* tokenizer */
#define	BSD43_PROM_HELP	BSD43_PROM_ENTRY(43)	/* prints help from command table */
/*
 * prom commands
 */
#define	BSD43_PROM_DUMPCMD	BSD43_PROM_ENTRY(44)	/* dump memory command */
#define	BSD43_PROM_SETENVCMD	BSD43_PROM_ENTRY(45)	/* setenv command */
#define	BSD43_PROM_UNSETENVCMD BSD43_PROM_ENTRY(46)	/* unsetenv command */
#define	BSD43_PROM_PRINTENVCMD BSD43_PROM_ENTRY(47)	/* printenv command */
#define	BSD43_PROM_BEVEXCEPT	BSD43_PROM_ENTRY(48)	/* general boot exception vector */
#define	BSD43_PROM_ENABLECMD	BSD43_PROM_ENTRY(49)	/* enable console command */
#define	BSD43_PROM_DISABLECMD	BSD43_PROM_ENTRY(50)	/* disable console command */

/*
 * Restart block -- monitor support for "warm" starts
 *
 * prom will perform "warm start" if restart_blk is properly set-up:
 *	rb_magic == RESTART_MAGIC
 *	rb_occurred == 0
 *	rb_checksum == 2's complement, 32-bit sum of first 32, 32-bit words 
 */
#define	BSD43_RESTART_MAGIC	0xfeedface
#define	BSD43_RESTART_CSUMCNT	32		/* chksum 32 words of restart routine */
#define	BSD43_RESTART_ADDR	0xa0000400	/* prom looks for restart block here */
#define	BSD43_RB_BPADDR	(BSD43_RESTART_ADDR+24)/* address of rb_bpaddr */

#ifdef LANGUAGE_C
struct bsd43_(restart_blk) {
	int	rb_magic;		/* magic pattern */
	int	(*rb_restart)();	/* restart routine */
	int	rb_occurred;		/* to avoid loops on restart failure */
	int	rb_checksum;		/* checksum of 1st 32 wrds of restrt */
	char	*rb_fbss;		/* start of prom bss and stack area */
	char	*rb_ebss;		/* end of prom bss and stack area */
	/*
	 * These entries are for communication between the debug monitor
	 * and the client process being debugged
	 * NOTE: a return value of -1 from (*rb_vtop)() is distinguished
	 * to indicate that a translation could not be made.
	 */
	int	(*rb_bpaddr)();		/* breakpoint handler */
	int	(*rb_vtop)();		/* virtual to physical conversion rtn */
	/*
	 * config table goes here
	 */
};

/*
 * args to promexec -- monitor support for loading new programs
 *
 * bootfiles should be specified as DEV(UNIT)FILE
 * (e.g. bfs(0)bootmips_le)
 */
struct bsd43_(promexec_args) {
	char	*pa_bootfile;		/* file to boot (only some devices) */
	int	pa_argc;		/* arg count */
	char	**pa_argv;		/* arg vector */
	char	**pa_environ;		/* environment vector */
	int	pa_flags;		/* flags, (see below) */
};
#endif LANGUAGE_C

/*
 * promexec flags
 */
#define	BSD43_EXEC_NOGO	1	/* just load, don't transfer control */

/*
 * prom non-volatile ram conventions
 */
#define	BSD43_NVSTATE_ADDR	48	/* byte in nv ram that indicates if nv valid */
#define	BSD43_NVTOD_VALID	1	/* flag bit that indicates clock ok if set */

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define EXEC_NOGO BSD43_EXEC_NOGO
#   define NVSTATE_ADDR BSD43_NVSTATE_ADDR
#   define NVTOD_VALID BSD43_NVTOD_VALID
#   define PROM_ANDB_RMW BSD43_PROM_ANDB_RMW
#   define PROM_ANDH_RMW BSD43_PROM_ANDH_RMW
#   define PROM_ANDW_RMW BSD43_PROM_ANDW_RMW
#   define PROM_ARGVIZE BSD43_PROM_ARGVIZE
#   define PROM_ATOB BSD43_PROM_ATOB
#   define PROM_AUTOBOOT BSD43_PROM_AUTOBOOT
#   define PROM_BEVEXCEPT BSD43_PROM_BEVEXCEPT
#   define PROM_BEVUTLB BSD43_PROM_BEVUTLB
#   define PROM_CLEARCACHE BSD43_PROM_CLEARCACHE
#   define PROM_CLOSE BSD43_PROM_CLOSE
#   define PROM_DISABLECMD BSD43_PROM_DISABLECMD
#   define PROM_DUMPCMD BSD43_PROM_DUMPCMD
#   define PROM_ENABLECMD BSD43_PROM_ENABLECMD
#   define PROM_ENTRY BSD43_PROM_ENTRY
#   define PROM_EXEC BSD43_PROM_EXEC
#   define PROM_FLUSHCACHE BSD43_PROM_FLUSHCACHE
#   define PROM_GETCHAR BSD43_PROM_GETCHAR
#   define PROM_GETENV BSD43_PROM_GETENV
#   define PROM_GETPKT BSD43_PROM_GETPKT
#   define PROM_GETS BSD43_PROM_GETS
#   define PROM_HELP BSD43_PROM_HELP
#   define PROM_INITPROTO BSD43_PROM_INITPROTO
#   define PROM_IOCTL BSD43_PROM_IOCTL
#   define PROM_LONGJMP BSD43_PROM_LONGJMP
#   define PROM_OPEN BSD43_PROM_OPEN
#   define PROM_ORB_RMW BSD43_PROM_ORB_RMW
#   define PROM_ORH_RMW BSD43_PROM_ORH_RMW
#   define PROM_ORW_RMW BSD43_PROM_ORW_RMW
#   define PROM_PARSER BSD43_PROM_PARSER
#   define PROM_PRINTENVCMD BSD43_PROM_PRINTENVCMD
#   define PROM_PRINTF BSD43_PROM_PRINTF
#   define PROM_PROTODISABLE BSD43_PROM_PROTODISABLE
#   define PROM_PROTOENABLE BSD43_PROM_PROTOENABLE
#   define PROM_PUTCHAR BSD43_PROM_PUTCHAR
#   define PROM_PUTPKT BSD43_PROM_PUTPKT
#   define PROM_PUTS BSD43_PROM_PUTS
#   define PROM_RANGE BSD43_PROM_RANGE
#   define PROM_READ BSD43_PROM_READ
#   define PROM_REBOOT BSD43_PROM_REBOOT
#   define PROM_REINIT BSD43_PROM_REINIT
#   define PROM_RESET BSD43_PROM_RESET
#   define PROM_RESTART BSD43_PROM_RESTART
#   define PROM_SETENV BSD43_PROM_SETENV
#   define PROM_SETENVCMD BSD43_PROM_SETENVCMD
#   define PROM_SETJMP BSD43_PROM_SETJMP
#   define PROM_SHOWCHAR BSD43_PROM_SHOWCHAR
#   define PROM_STACK BSD43_PROM_STACK
#   define PROM_STRCAT BSD43_PROM_STRCAT
#   define PROM_STRCMP BSD43_PROM_STRCMP
#   define PROM_STRCPY BSD43_PROM_STRCPY
#   define PROM_STRLEN BSD43_PROM_STRLEN
#   define PROM_UNSETENVCMD BSD43_PROM_UNSETENVCMD
#   define PROM_WRITE BSD43_PROM_WRITE
#   define RB_BPADDR BSD43_RB_BPADDR
#   define RESTART_ADDR BSD43_RESTART_ADDR
#   define RESTART_CSUMCNT BSD43_RESTART_CSUMCNT
#   define RESTART_MAGIC BSD43_RESTART_MAGIC
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


