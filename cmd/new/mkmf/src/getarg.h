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
/* $Header: getarg.h,v 1.1.1.2 90/05/09 18:00:43 wje Exp $ */

/*
 * Get command line argument
 *
 * Author: Peter J. Nicklin
 */

/*
 * Argument syntax: `-xargument' or `-x argument'
 */
#define GETARG(p) ((p[1] != '\0') ? ++p : (--argc, *++argv))

/*
 * Argument syntax: `-xargument'
 *
 * #define GETARG(p) (++p)
 */
