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
/* $Header: mix.h,v 1.2.1.2 90/05/09 17:31:12 wje Exp $ */
#ifndef		_MIX_H_
#define		_MIX_H_

/*
 *	$Header: mix.h,v 1.2.1.2 90/05/09 17:31:12 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>
#include	<asn.h>
#include	<smp.h>

typedef		CUnswType		MixIdType;

typedef		CUnswType		MixCookieType;

typedef		CByteType		MixNameType;

typedef		MixNameType		*MixNamePtrType;

typedef		CUnsfType		MixLengthType;

typedef		MixLengthType		*MixLengthPtrType;

typedef		SmpErrorType		MixStatusType;

typedef		MixStatusType		(*MixReleaseOpType) ();

typedef		AsnIdType		(*MixNextOpType) ();

typedef		AsnIdType		(*MixGetOpType) ();

typedef		MixStatusType		(*MixSetOpType) ();

typedef		MixStatusType		(*MixCreateOpType) ();

typedef		MixStatusType		(*MixDestroyOpType) ();

typedef		struct			MixOpsTag {

		MixReleaseOpType	mixOpsReleaseOp;
		MixCreateOpType		mixOpsCreateOp;
		MixDestroyOpType	mixOpsDestroyOp;
		MixNextOpType		mixOpsNextOp;
		MixGetOpType		mixOpsGetOp;
		MixSetOpType		mixOpsSetOp;

		}			MixOpsType;

typedef		MixOpsType		*MixOpsPtrType;

#define         mixValueAsnTag          ((AsnTagType) 0x99)
#define         mixValueAsnClass        (asnClassApplication)

#define         mixMaxPathLen        	(32)

CVoidType	mixInit ();
MixIdType	mixNew ();
MixIdType	mixFree ();
AsnIdType	mixValue ();

MixStatusType	mixCreate ();
MixStatusType	mixDestroy ();
MixStatusType	mixSet ();
AsnIdType	mixNext ();
AsnIdType	mixGet ();

#endif		/*	_MIX_H_	*/
