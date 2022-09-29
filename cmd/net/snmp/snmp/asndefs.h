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
/* $Header: asndefs.h,v 1.1.1.2 90/05/09 17:25:45 wje Exp $ */
#ifndef		_ASNDEFS_H_
#define		_ASNDEFS_H_

/*
 *	$Header: asndefs.h,v 1.1.1.2 90/05/09 17:25:45 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<asn.h>
#include	<asl.h>

typedef		struct			AsnDatumTag {

		AsnIndexType		asnDatumParent;
		AsnIndexType		asnDatumMyself;
		AsnIndexType		asnDatumSons;
		AsnLengthType		asnDatumTotalLen;
		AsnLengthType		asnDatumMaxLen;
		AsnLengthType		asnDatumActualLen;
		AsnLengthType		asnDatumUserLen;
		AsnLengthType		asnDatumAlarm;
		AsnTypeType		asnDatumCmd;
		AsnClassType		asnDatumClass;
		AsnTagType		asnDatumTag;
		AsnLengthType		asnDatumValue;
		AslIdType		asnDatumNode;
		CUnssType		asnDatumFlags;

		}			AsnDatumType;

typedef		AsnDatumType		*AsnDatumPtrType;

#define		asnDatumConstructorGet(x)	\
			(((AsnDatumPtrType)(x))->asnDatumFlags & 0x04)
#define		asnDatumConstructorSet(x,c)	\
			((c) ? \
			(((AsnDatumPtrType)(x))->asnDatumFlags |= 0x04) : \
			(((AsnDatumPtrType)(x))->asnDatumFlags &= ~(0x04)))

#define		asnDatumMustMatchGet(x)	\
			(((AsnDatumPtrType)(x))->asnDatumFlags & 0x08)
#define		asnDatumMustMatchSet(x,c)	\
			((c) ? \
			(((AsnDatumPtrType)(x))->asnDatumFlags |= 0x08) : \
			(((AsnDatumPtrType)(x))->asnDatumFlags &= ~(0x08)))

#define		asnDatumIndefLengthGet(x)	\
			(((AsnDatumPtrType)(x))->asnDatumFlags & 0x10)
#define		asnDatumIndefLengthSet(x,c)	\
			((c) ? \
			(((AsnDatumPtrType)(x))->asnDatumFlags |= 0x10) : \
			(((AsnDatumPtrType)(x))->asnDatumFlags &= ~(0x10)))

typedef		AsnStatusType		(*AsnParseFnType) ();

typedef		struct			AsnTag {

		AslIdType		asnLanguage;
		AsnStatusType		asnStatus;
		CIntfType		asnParseLevel;
		AsnLengthType		asnBytesLeft;
		AsnIndexType		asnDatumFree;
		AsnIndexType		asnNewId;
		AsnIndexType		asnSize;
		AsnLengthType		asnSoFar;
		CUnsfType		asnLenCnt;
		CUnsfType		asnRefCnt;
		AsnParseFnType		asnFn;
		AslIdType		asnWomb;
		AsnIndexType		asnDatum;
		AsnDatumPtrType		asnArea;

		}			AsnType;

typedef		AsnType			*AsnPtrType;

#define		asnPtrToId(x)		((AsnIdType) ((AsnDatumPtrType) (x)))
#define		asnIdToPtr(x)		((AsnDatumPtrType) ((AsnIdType) (x)))
#define		asnPtrToRoot(x)		((AsnPtrType) \
						((x) + (x)->asnDatumMyself))
#define		asnRootToPtr(x)		(((AsnPtrType)(x))->asnArea + \
						((AsnPtrType)(x))->asnDatum)

#define		asnTagDef(asn)	\
			((asnIdToPtr (asn))->asnDatumTag)

#define		asnTypeDef(asn)	\
			((asnIdToPtr (asn))->asnDatumCmd)

#define		asnLengthDef(asn)	\
			((asnIdToPtr (asn))->asnDatumUserLen)

#define		asnSonsDef(asn)	\
			((asnIdToPtr (asn))->asnDatumSons)

#define		asnConstructorDef(asn)	\
			(asnDatumConstructorGet (asnIdToPtr (asn)))

#define		asnClassDef(asn)	\
			((asnIdToPtr (asn))->asnDatumClass)

#define		asnNegativeDef(cp, n)	\
			((CBoolType) ((n),	\
			(*((CIntbPtrType) (cp)) < (CIntbType) 0)))

#define		asnNonZeroDef(cp, n)	\
			((CBoolType) (((n) > 1) || (*(cp) != 0)))

#ifdef		SAFE

#define		asnComponentDef(asn, i)		\
			(((asnPtrToRoot (asnIdToPtr (asn)))->asnRefCnt++), \
			(asnPtrToId (asnIdToPtr (asn) - (i))))

#else		/*	SAFE		*/

#define		asnComponentDef(asn, i)		\
			(((asn) == (AsnIdType) 0) ? (asn) : \
			(((i) > (asnIdToPtr (asn))->asnDatumSons) ? \
			((AsnIdType) 0) : \
			(((asnPtrToRoot (asnIdToPtr (asn)))->asnRefCnt++), \
			(asnPtrToId (asnIdToPtr (asn) - (i))))))

#endif		/*	SAFE		*/

#define		asnFreeDef(asn)		\
			(((asn) == (AsnIdType) 0) ? (asn) : \
			((--((asnPtrToRoot (asnIdToPtr (asn)))->asnRefCnt)) ? \
			((AsnIdType) 0) : \
			((free ((char *) \
			((asnPtrToRoot (asnIdToPtr (asn)))->asnArea))), \
			((AsnIdType) 0))))

#ifdef		SAFE

#define		asnValueDef(asn)	\
       			(((CBytePtrType)	\
			(asnPtrToRoot (asnIdToPtr (asn))->asnArea)) +	\
       			(asnIdToPtr (asn))->asnDatumValue)

#else		/*	SAFE		*/

#define		asnValueDef(asn)	\
        		((asn == (AsnIdType) 0) ?	\
       			((CBytePtrType) 0) :		\
			((asnDatumConstructorGet (asnIdToPtr (asn))) ?	\
       			((CBytePtrType) 0) : (((CBytePtrType)	\
			(asnPtrToRoot (asnIdToPtr (asn))->asnArea)) +	\
       			(asnIdToPtr (asn))->asnDatumValue)))

#endif		/*	SAFE		*/

#endif		/*	_ASNDEFS_H_	*/
