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
/* $Header: execargs.h,v 1.8.3.2 90/05/10 01:00:16 wje Exp $ */

#ifndef	_EXECARGS_
#define	_EXECARGS_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#if vax
char **execargs = (char**)(0x7ffffffc);
#endif

#if pdp11
char **execargs = (char**)(-2);
#endif

#if u3b || M32 || u3b15 || u3b5 || u3b2
/* empty till we can figure out what to do for the shell */
#endif

#if  mips
/*
 * We grow our stack downwords but allocate args upwards so the pointer
 * to where execargs started would be different for each process. It
 * would depend upon how many args were passed to the process. Could
 * wind the stack back to crt0 to get where exec args started.
 */
#endif

#endif	_EXECARGS_
