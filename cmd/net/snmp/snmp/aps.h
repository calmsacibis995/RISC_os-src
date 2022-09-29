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
/* $Header: aps.h,v 1.1.1.2 90/05/09 17:25:10 wje Exp $ */
#ifndef		_APS_H_
#define		_APS_H_

/*
 *	$Header: aps.h,v 1.1.1.2 90/05/09 17:25:10 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>
#include	<asn.h>

typedef		ErrStatusType		ApsStatusType;

typedef		CUnswType		ApsIdType;

typedef		CBytePtrType		ApsNameType;

typedef		CUnswType		ApsGoodiesType;

typedef		CBoolType		(*ApsVerifyFnType) ();

typedef		AsnIdType		(*ApsEncodeFnType) ();

typedef		AsnIdType		(*ApsDecodeFnType) ();

ApsStatusType	apsScheme ();
ApsIdType	apsNew ();
ApsIdType	apsFree ();
ApsIdType	apsVerify ();
AsnIdType	apsEncode ();
AsnIdType	apsDecode ();
CVoidType	apsInit ();

#endif		/*	_APS_H_	*/
