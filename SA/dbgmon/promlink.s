#ident "$Header: promlink.s,v 1.2 90/01/11 14:43:08 huang Exp $"
/*	%Q%	%I%	%M%
 * promlink.s -- routines for linking dbgmon (loaded version) to prom routines
 */
/* $Copyright$ */

#include "prom/entrypt.h"
#include "machine/regdef.h"
#include "machine/asm.h"
#include "machine/cpu.h"

	.text

/*
 * PROM_LINK -- unconditionally link to prom entry point
 */
#define	PROM_LINK(a,b)			\
	.globl	a;			\
	.ent	a,0;			\
a:	la	v0,+b;			\
	j	v0;			\
	.end	a

/*
 * PROM_CLINK -- conditionally link to prom entry point if
 * implemented, if not implemented return -1
 */
#define	PROM_CLINK(a,b)			\
	.globl	a;			\
	.ent	a,0;			\
a:	lw	v0,+b;			\
	lw	v1,+PROM_NOTIMPLEMENT;	\
	beq	v0,v1,1f;		\
	la	v0,+b;			\
	j	v0;			\
1:	li	v0,-1;			\
	j	ra;			\
	.end	a

PROM_LINK(open, PROM_OPEN)
PROM_LINK(close, PROM_CLOSE)
PROM_LINK(ioctl, PROM_IOCTL)
PROM_LINK(printf, PROM_PRINTF)
PROM_LINK(putchar, PROM_PUTCHAR)
PROM_LINK(getchar, PROM_GETCHAR)
PROM_LINK(showchar, PROM_SHOWCHAR)
PROM_LINK(puts, PROM_PUTS)

PROM_LINK(init_proto, PROM_INITPROTO)
PROM_LINK(proto_enable, PROM_PROTOENABLE)
PROM_LINK(proto_disable, PROM_PROTODISABLE)
PROM_LINK(getpkt, PROM_GETPKT)
PROM_LINK(putpkt, PROM_PUTPKT)

PROM_LINK(setjmp, PROM_SETJMP)
PROM_LINK(longjmp, PROM_LONGJMP)
PROM_LINK(atob, PROM_ATOB)
PROM_LINK(strcmp, PROM_STRCMP)
PROM_LINK(strlen, PROM_STRLEN)
PROM_LINK(strcpy, PROM_STRCPY)
PROM_LINK(strcat, PROM_STRCAT)

PROM_LINK(command_parser, PROM_PARSER)
PROM_LINK(parse_range, PROM_RANGE)
PROM_LINK(_argvize, PROM_ARGVIZE)
PROM_LINK(help, PROM_HELP)

PROM_LINK(clear_cache, PROM_CLEARCACHE)
PROM_LINK(flush_cache, PROM_FLUSHCACHE)

PROM_LINK(_dump, PROM_DUMPCMD)
PROM_LINK(disable, PROM_DISABLECMD)
PROM_LINK(enable, PROM_ENABLECMD)

PROM_CLINK(clear_nofault, PROM_CLEARNOFAULT)
