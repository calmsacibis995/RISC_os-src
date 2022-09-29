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
/* $Header: debug.h,v 1.1.1.2 90/05/09 17:26:59 wje Exp $ */
#ifndef		_DEBUG_H_
#define		_DEBUG_H_

/*
 *	$Header: debug.h,v 1.1.1.2 90/05/09 17:26:59 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#ifdef		DEBUG

#include	<stdio.h>
#include	<asx.h>

#define		DEBUG0(a)		\
			(void) printf ((a));	\
			(void) fflush (stdout)

#define		DEBUG1(a, b)		\
			(void) printf ((a), (b));	\
			(void) fflush (stdout)

#define		DEBUG2(a, b, c)		\
			(void) printf ((a), (b), (c));	\
			(void) fflush (stdout)

#define		DEBUG3(a, b, c, d)	\
			(void) printf ((a), (b), (c), (d)); \
			(void) fflush (stdout)

#define		DEBUGBYTES(b, n)		\
			{	\
			CIntfType	i;	\
			CBytePtrType	cp;	\
			cp = (CBytePtrType) (b);	\
			for (i = (CIntfType) (n); i > 0; i--) {	\
				printf ("%02.02X ", *cp++);	\
			}}

#define		DEBUGASN(a)	\
			(void) asxPrint ((a), (CUnsfType) 0); \
			(void) fflush (stdout)

#else		/*	DEBUG	*/

#define		DEBUG0(a)
#define		DEBUG1(a, b)
#define		DEBUG2(a, b, c)
#define		DEBUG3(a, b, c, d)
#define		DEBUGBYTES(b, n)
#define		DEBUGASN(a)

#endif		/*	DEBUG	*/

#endif		/*	_DEBUG_H_	*/
