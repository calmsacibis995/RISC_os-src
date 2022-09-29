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
/* $Header: asn.h,v 1.3.1.2 90/05/09 17:25:40 wje Exp $ */
#ifndef		_ASN_H_
#define		_ASN_H_

/*
 *	$Header: asn.h,v 1.3.1.2 90/05/09 17:25:40 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>

typedef		CUnswType		AsnIdType;

typedef		CUnswType		AsnLanguageType;

typedef		CUnslType		AsnTagType;

typedef		CIntsType		AsnLengthType;

#define		asnLengthIndef		((AsnLengthType) -1)

typedef		CUnssType		AsnIndexType;

typedef		CIntlType		AsnNumberType;

typedef		enum			AsnStatusTag {

		asnStatusOk,
		asnStatusAccept,
		asnStatusReject,
		asnStatusBad

		}			AsnStatusType;

typedef		enum			AsnClassTag {

		asnClassUniversal,
		asnClassApplication,
		asnClassContext,
		asnClassPrivate

		}			AsnClassType;

typedef		enum			AsnTypeTag {

		asnTypeNone,
		asnTypeInteger,
		asnTypeOctetString,
		asnTypeObjectId,
		asnTypeSequence,
		asnTypeSequenceOf,
		asnTypeNull,
		asnTypeAny

		}			AsnTypeType;

CVoidType	asnInit ();

AsnIdType	asnNew ();
AsnIdType	asnUnsl ();
AsnIdType	asnIntl ();
AsnIdType	asnOctetString ();
AsnIdType	asnObjectId ();
AsnIdType	asnSequence ();

AsnStatusType	asnDecode ();
AsnStatusType	asnAppend ();
AsnLengthType	asnEncode ();

AsnNumberType	asnNumber ();
AsnLengthType	asnContents ();

#ifdef		INLINE

#include	<asndefs.h>

#define		asnTag(asn)		(asnTagDef (asn))
#define		asnType(asn)		(asnTypeDef (asn))
#define		asnClass(asn)		(asnClassDef (asn))
#define		asnLength(asn)		(asnLengthDef (asn))
#define		asnConstructor(asn)	(asnConstructorDef (asn))
#define		asnNegative(cp, n)	(asnNegativeDef(cp, n))
#define		asnNonZero(cp, n)	(asnNonZeroDef(cp, n))
#define		asnSons(asn)		(asnSonsDef (asn))
#define		asnComponent(asn, i)	(asnComponentDef (asn, i))
#define		asnFree(asn)		(asnFreeDef (asn))
#define		asnValue(asn)		(asnValueDef (asn))

#else		/*	INLINE	*/

AsnTypeType	asnType ();
AsnTagType	asnTag ();
AsnClassType	asnClass ();
AsnLengthType	asnLength ();
CBoolType	asnConstructor ();
CBoolType	asnNegative ();
CBoolType	asnNonZero ();
AsnIndexType	asnSons ();
AsnIdType	asnComponent ();
AsnIdType	asnFree ();
CBytePtrType	asnValue ();

#endif		/*	INLINE	*/

#define		asnMakeInteger(value)	(asnIntl (asnClassUniversal, \
					(AsnTagType) 2, (CIntlType) value))

#define		asnMakeOctetString(str, len) \
					(asnOctetString (asnClassUniversal, \
					(AsnTagType) 4, (CBytePtrType) str, \
					(AsnLengthType) len))

#define		asnMakeIpAddress(str)	(asnOctetString (asnClassApplication, \
					(AsnTagType) 0, (CBytePtrType) str, \
					(AsnLengthType) 4))

#define		asnMakeOpaque(str, len) \
					(asnOctetString (asnClassApplication, \
					(AsnTagType) 4, (CBytePtrType) str, \
					(AsnLengthType) len))

#define		asnMakeCounter(size)	(asnUnsl (asnClassApplication, \
					(AsnTagType) 1, (CIntlType) size))

#define		asnMakeGauge(value)	(asnUnsl (asnClassApplication, \
					(AsnTagType) 2, (CIntlType) value))

#define		asnMakeTimeTicks(value) (asnUnsl (asnClassApplication, \
					(AsnTagType) 3, (CIntlType) value))

#define		asnMakeObjectId(str, len) \
					(asnObjectId (asnClassUniversal, \
					(AsnTagType) 6, (CBytePtrType) str, \
					(AsnLengthType) len))

#endif		/*	_ASN_H_	*/
