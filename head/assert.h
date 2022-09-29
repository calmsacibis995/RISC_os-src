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
/* $Header: assert.h,v 1.8.1.2 90/05/09 19:46:52 wje Exp $ */

#ifndef	_ASSERT_
#define	_ASSERT_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifdef NDEBUG
#define _assert(EX)
#define assert(EX)
#else
#ifdef SYSTYPE_BSD43
# define _assert(ex)	if (ex) ; else {fprintf(stderr,"Assertion failed: file \"%s\", line %d\n", __FILE__, __LINE__);exit(1);}
#define assert(ex) _assert(ex)
#else SYSTYPE_BSD43
extern void _assert();
#define assert(EX) if (EX) ; else _assert("EX", __FILE__, __LINE__)
#endif SYSTYPE_BSD43
#endif

#endif	_ASSERT_
