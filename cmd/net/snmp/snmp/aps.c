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
#ident	"$Header: aps.c,v 1.1.1.2 90/05/09 17:25:04 wje Exp $"

/*
 *	$Header: aps.c,v 1.1.1.2 90/05/09 17:25:04 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<aps.h>
#include	<asn.h>
#include	<local.h>

#define		apsMaxNameSize		(16)
#define		apsVersionCode		(0)

typedef		struct			ApsSchemeTag {

		ApsNameType		apsSchemeName;
		ApsVerifyFnType		apsSchemeVerifyFn;
		ApsEncodeFnType		apsSchemeEncodeFn;
		ApsDecodeFnType		apsSchemeDecodeFn;
		struct	ApsSchemeTag	*apsSchemeNext;

		}			ApsSchemeType;

typedef		ApsSchemeType		*ApsSchemePtrType;

typedef		struct			ApsCommTag {

		ApsNameType		apsCommName;
		AsnIdType		apsCommAsn;
		ApsSchemePtrType	apsCommScheme;
		ApsGoodiesType		apsCommGoodies;
		CIntfType		apsCommRefCnt;
		struct	ApsCommTag	*apsCommNext;

		}			ApsCommType;

typedef		ApsCommType		*ApsCommPtrType;

#define		apsIdToPtr(x)		((ApsCommPtrType)((ApsIdType) (x)))
#define		apsPtrToId(x)		((ApsIdType) ((ApsCommPtrType) (x)))

static		AsnIdType		apsVersion;
static		ApsSchemePtrType	apsSchemes;
static		ApsCommPtrType		apsComms;

ApsStatusType		apsScheme (name, verifyFn, encodeFn, decodeFn)

ApsNameType		name;
ApsVerifyFnType		verifyFn;
ApsEncodeFnType		encodeFn;
ApsDecodeFnType		decodeFn;

{
	ApsSchemePtrType		sp;

	for (sp = apsSchemes; (sp != (ApsSchemePtrType) 0) &&
		strcmp ((char *) name, (char *) sp->apsSchemeName) != 0;
		sp = sp->apsSchemeNext);

	if (sp != (ApsSchemePtrType) 0) {
		return (errBad);
	}

	if ((sp = (ApsSchemePtrType) malloc ((unsigned) sizeof (*sp))) !=
		(ApsSchemePtrType) 0) {
		sp->apsSchemeName = name;
		sp->apsSchemeVerifyFn = verifyFn;
		sp->apsSchemeEncodeFn = encodeFn;
		sp->apsSchemeDecodeFn = decodeFn;
		sp->apsSchemeNext = apsSchemes;
		apsSchemes = sp;
		return (errOk);
	}
	else {
		return (errBad);
	}
}

ApsIdType		apsNew (name, scheme, goodies)

ApsNameType		name;
ApsNameType		scheme;
ApsGoodiesType		goodies;

{
	ApsSchemePtrType		sp;
	AsnIdType			asn;
	ApsCommPtrType			ap;

	if (strlen ((char *) name) > apsMaxNameSize) {
		return ((ApsIdType) 0);
	}

	for (sp = apsSchemes; (sp != (ApsSchemePtrType) 0) &&
		strcmp ((char *) scheme, (char *) sp->apsSchemeName) != 0;
		sp = sp->apsSchemeNext);

	if (sp == (ApsSchemePtrType) 0) {
		return ((ApsIdType) 0);
	}

	for (ap = apsComms; (ap != (ApsCommPtrType) 0) &&
		strcmp ((char *) name, (char *) ap->apsCommName) != 0;
		ap = ap->apsCommNext);

	if (ap != (ApsCommPtrType) 0) {
		return ((ApsIdType) 0);
	}

	asn = asnOctetString (asnClassUniversal, (AsnTagType) 4,
		(CBytePtrType) name,
		(AsnLengthType) strlen ((char *) name));
	if (asn == (AsnIdType) 0) {
		return ((ApsIdType) 0);
	}

	if ((ap = (ApsCommPtrType) malloc ((unsigned) sizeof (*ap))) !=
		(ApsCommPtrType) 0) {
		ap->apsCommName = name;
		ap->apsCommScheme = sp;
		ap->apsCommGoodies = goodies;
		ap->apsCommAsn = asn;
		ap->apsCommRefCnt = (CIntfType) 1;
		ap->apsCommNext = apsComms;
		apsComms = ap;
	}
	else {
		asn = asnFree (asn);
	}
	return (apsPtrToId (ap));
}

ApsIdType		apsFree (s)

ApsIdType		s;

{
	ApsCommPtrType			ap;
	ApsCommPtrType			sp;
	ApsCommPtrType			pp;

	if (s == (ApsIdType) 0) {
		return ((ApsIdType) 0);
	}

	sp = apsIdToPtr (s);
	if (--sp->apsCommRefCnt != 0) {
		return ((ApsIdType) 0);
	}

	for (ap = apsComms; (ap != (ApsCommPtrType) 0) && (sp != ap);
		ap = ap->apsCommNext) {
		pp = ap;
	}

	if (ap != (ApsCommPtrType) 0) {
		if (ap == apsComms) {
			apsComms = ap->apsCommNext;
		}
		else {
			pp->apsCommNext = ap->apsCommNext;
		}
		ap->apsCommAsn = asnFree (ap->apsCommAsn);
		(void) free ((char *) ap);
	}

	return ((ApsIdType) 0);
}

ApsIdType		apsVerify (asn)

AsnIdType		asn;

{
	ApsCommPtrType			ap;
	CByteType			name [ (apsMaxNameSize + 1) ];
	AsnLengthType			n;
	AsnIdType			asnVer;
	AsnIdType			asnName;

	asnVer  = asnComponent (asn, (AsnIndexType) 1);
	if (asnVer == (AsnIdType) 0) {
		return ((ApsIdType) 0);
	}

	n = asnLength (asnVer);
	if ((n != (AsnLengthType) 1) ||
		(asnNumber (asnValue (asnVer), n) !=
		(AsnNumberType) apsVersionCode)) {
		return ((ApsIdType) 0);
	}
	asnVer = asnFree (asnVer);

	asnName  = asnComponent (asn, (AsnIndexType) 2);
	if (asnName == (AsnIdType) 0) {
		return ((ApsIdType) 0);
	}

	n = asnContents (asnName, name, (AsnLengthType) apsMaxNameSize);
	asnName = asnFree (asnName);
	if (n < (AsnLengthType) 0) {
		return ((ApsIdType) 0);
	}
	name [ (int) n ] = (CByteType) 0;

	for (ap = apsComms; (ap != (ApsCommPtrType) 0) &&
		strcmp ((char *) name, (char *) ap->apsCommName) != 0;
		ap = ap->apsCommNext);

	if (ap != (ApsCommPtrType) 0) {
		if ((*((ap->apsCommScheme)->apsSchemeVerifyFn))
			(ap->apsCommGoodies, asn)) {
			ap->apsCommRefCnt++;
			return (apsPtrToId (ap));
		}
		else {
			return ((ApsIdType) 0);
		}
	}
	else {
		return ((ApsIdType) 0);
	}
}

AsnIdType		apsDecode (aps, asn)

ApsIdType		aps;
AsnIdType		asn;

{
	ApsCommPtrType		ap;

	if (aps == (ApsIdType) 0) {
		return ((AsnIdType) 0);
	}
	ap = apsIdToPtr (aps);

	return ((*((ap->apsCommScheme)->apsSchemeDecodeFn))
		(ap->apsCommGoodies, asn));
}

AsnIdType		apsEncode (aps, asn)

ApsIdType		aps;
AsnIdType		asn;

{
	AsnIdType		result;
	ApsCommPtrType		ap;
	AsnIdType		code;

	if (aps == (ApsIdType) 0) {
		return ((AsnIdType) 0);
	}
	ap = apsIdToPtr (aps);

	if (apsVersion == (AsnIdType) 0) {
		apsVersion = asnIntl (asnClassUniversal,
			(AsnTagType) 2, (CIntlType) apsVersionCode);
		if (apsVersion == (AsnIdType) 0) {
			return ((AsnIdType) 0);
		}
	}

	code = (*((ap->apsCommScheme)->apsSchemeEncodeFn))
		(ap->apsCommGoodies, asn);
	if (code == (AsnIdType) 0) {
		return ((AsnIdType) 0);
	}

        if ((result = asnSequence (asnClassUniversal, (AsnTagType) 0x10,
                asnTypeSequence)) == (AsnIdType) 0) {
        }
	else if (asnAppend (result, apsVersion) != asnStatusOk) {
		result = asnFree (result);
	}
	else if (asnAppend (result, ap->apsCommAsn) != asnStatusOk) {
		result = asnFree (result);
	}
	else if (asnAppend (result, code) != asnStatusOk) {
		result = asnFree (result);
	}

	code = asnFree (code);
	return (result);
}

CVoidType		apsInit ()

{
	apsVersion = (AsnIdType) 0;
	apsSchemes = (ApsSchemePtrType) 0;
	apsComms = (ApsCommPtrType) 0;
}

