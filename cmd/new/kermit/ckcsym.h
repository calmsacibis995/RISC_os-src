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
/* $Header: ckcsym.h,v 1.1.1.2 90/05/09 17:47:03 wje Exp $ */
/* This file is for use with compilers that don't have the capability to
 * #define symbols on the C compiler command line.  This file must
 * be #include'd before all other ck*.h files so that the symbols #define'd
 * here can be used for any subsequent conditional code.  Symbols should be
 * #define'd as 1 if TRUE and 0 if FALSE.  This implies, of course, that they
 * be tested with #if's, not #ifdef's and #ifndef's.
 */ 

/*
 * For example, this file was required to compile Macintosh Kermit under
 * Megamax C (which we don't do any more), because Megamax did not have
 * a mechanism for defining symbols on the command line, so this file was
 * used to define the symbol MAC.
 */
