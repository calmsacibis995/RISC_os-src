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
/* $Header: stringlist.h,v 1.6.3.2 90/05/10 04:48:14 wje Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * stringlist.h -- definitions for stringlist routines
 */

#define	BSD43_MAXSTRINGS	16		/* max number of strings */
#define	BSD43_STRINGBYTES	256		/* max total length of strings */

/*
 * string lists are used to maintain argv and environment string lists
 */
struct bsd43_(string_list) {
	char *strptrs[BSD43_MAXSTRINGS];	/* vector of string pointers */
	char strbuf[BSD43_STRINGBYTES];	/* strings themselves */
	char *strp;			/* free ptr in strbuf */
	int strcnt;			/* number of strings in strptrs */
};

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define MAXSTRINGS BSD43_MAXSTRINGS
#   define STRINGBYTES BSD43_STRINGBYTES
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


