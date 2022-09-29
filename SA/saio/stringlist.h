#ident "$Header: stringlist.h,v 1.5 90/05/25 17:06:20 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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
 * stringlist.h -- definitions for stringlist routines
 */

#define	MAXSTRINGS	32		/* max number of strings */
#define	STRINGBYTES	768		/* max total length of strings */

/*
 * string lists are used to maintain argv and environment string lists
 */
struct string_list {
	char *strptrs[MAXSTRINGS];	/* vector of string pointers */
	char strbuf[STRINGBYTES];	/* strings themselves */
	char *strp;			/* free ptr in strbuf */
	int strcnt;			/* number of strings in strptrs */
};
