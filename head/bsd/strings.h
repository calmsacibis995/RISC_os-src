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
/* $Header: strings.h,v 1.5.3.2 90/05/09 19:49:10 wje Exp $ */

#ifndef	_BSD_STRINGS_
#define	_BSD_STRINGS_	1


/* BSD compatiblity hack
 */


/* just get the S5 version of the stuff */
#include "../string.h"

/* plus BSD functions not in S5 */
extern char
	*index(),
	*rindex();

#endif	_BSD_STRINGS_
