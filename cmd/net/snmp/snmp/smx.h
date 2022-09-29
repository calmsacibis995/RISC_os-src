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
/* $Header: smx.h,v 1.2.1.2 90/05/09 17:32:35 wje Exp $ */
#ifndef		_SMX_H_
#define		_SMX_H_

/*
 *	$Header: smx.h,v 1.2.1.2 90/05/09 17:32:35 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<smp.h>

CCharPtrType		smxErrorToText ();
SmpErrorType		smxTextToError ();

CCharPtrType		smxKindToText ();
SmpKindType		smxTextToKind ();

CIntfType		smxValueToText ();
CIntfType		smxTextToValue ();

#define			smxNameToText(text, n, name, m, enump)	\
				(smxObjectIdToText ((text), (n), (name), (m), (enump)))
#define			smxTextToName(name, m, text)	\
				(smxObjectIdToText ((name), (m), (text)))

CIntfType		smxIPAddrToText ();
CIntfType		smxTextToIPAddr ();

CIntfType		smxOctetStringToText ();
CIntfType		smxTextToOctetString ();

CIntfType		smxObjectIdToText ();
CIntfType		smxTextToObjectId ();

CIntfType		smxIntegerToText ();
CIntfType		smxTextToInteger ();

CIntfType		smxCounterToText ();
CIntfType		smxTextToCounter ();

CIntfType		smxGuageToText ();
CIntfType		smxTextToGuage ();

#endif		/*	_SMX_H_		*/
