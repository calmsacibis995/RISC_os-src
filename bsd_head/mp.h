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
/* $Header: mp.h,v 1.4.1.2 90/05/07 20:08:20 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define BSD43_MINT struct bsd43_(mint)
BSD43_MINT
{	int len;
	short *val;
};
#define BSD43_FREE(x) {if(x.len!=0) {bsd43_(free)((char *)x.val); x.len=0;}}
#ifndef DBG
#define bsd43_shfree(bsd43_u) bsd43_(free)((char *)bsd43_u)
#else
#include <bsd43/stdio.h>
#define bsd43_shfree(bsd43_u) { if(bsd43_(dbg)) bsd43_(fprintf)(bsd43_stderr, "free %o\n", bsd43_u); bsd43_(free)((char *)bsd43_u);}
extern int bsd43_(dbg);
#endif
#ifndef vax
struct bsd43_(half)
{	short high;
	short low;
};
#else
struct bsd43_(half)
{	short low;
	short high;
};
#endif
extern BSD43_MINT *bsd43_(itom)();
extern BSD43_MINT *bsd43_(xtom)();
extern char *bsd43_(mtox)();
extern short *bsd43_(xalloc)();
extern void bsd43_(mfree)();

#ifdef lint
extern bsd43_(xv_oid);
#define BSD43_VOID bsd43_(xv_oid) =
#else
#define BSD43_VOID
#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define FREE BSD43_FREE
#   define MINT BSD43_MINT
#   define VOID BSD43_VOID
#   define shfree bsd43_shfree
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


