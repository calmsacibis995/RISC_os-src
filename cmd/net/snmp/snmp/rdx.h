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
/* $Header: rdx.h,v 1.1.1.2 90/05/09 17:32:09 wje Exp $ */
#ifndef		_RDX_H_
#define		_RDX_H_

/*
 *	$Header: rdx.h,v 1.1.1.2 90/05/09 17:32:09 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>

CIntfType		rdxDecode10 ();
CIntfType		rdxDecode08 ();
CIntfType		rdxDecode16 ();
CIntfType		rdxDecodeAny ();

CIntfType		rdxEncode10 ();
CIntfType		rdxEncode08 ();

#endif		/*	_RDX_H_		*/
