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
#ident	"$Header: mis.c,v 1.1.1.2 90/05/09 17:29:32 wje Exp $"

/*
 *	$Header: mis.c,v 1.1.1.2 90/05/09 17:29:32 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>
#include	<local.h>
#include	<mis.h>
#include	<mix.h>
#include	<avl.h>
#include	<aps.h>
#include	<asn.h>

static	MixIdType	misTree;
static	MisAccessType	misAccess;

MisStatusType		misExport (name, namelen, ops, cookie)

MixNamePtrType		name;
MixLengthType		namelen;
MixOpsPtrType		ops;
MixCookieType		cookie;

{
	AsnIdType		value;

	if ((value = mixValue (ops, cookie)) == (AsnIdType) 0) {
		return (errBad);
	}
	else if (mixCreate (misTree, name, namelen, value) != smpErrorNone) {
		value = asnFree (value);
		return (errBad);
	}
	else {
		value = asnFree (value);
		return (errOk);
	}
}

MixIdType		misCommunityToMib (s)

ApsIdType		s;

{
	s = s;
	return (misTree);
}

MisAccessType		misCommunityToAccess (s)

ApsIdType		s;

{
	s = s;
	return (misAccess);
}

ApsIdType		misCommunityByName (name)

CBytePtrType		name;

{
	name = name;
	return ((ApsIdType) 0);
}

#ifdef		NOTDEF

static	MixStatusType	misRelease (cookie)

MixCookieType		cookie;

{
	cookie = cookie;
	return (smpErrorGeneric);
}

static	MixStatusType	misCreate (cookie, name, namelen, asn)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;
AsnIdType		asn;

{
	cookie = cookie;
	name = name;
	namelen = namelen;
	asn = asn;
	return (smpErrorGeneric);
}

static	MixStatusType	misDestroy (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	cookie = cookie;
	name = name;
	namelen = namelen;
	return (smpErrorGeneric);
}

static	AsnIdType	misGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	cookie = cookie;
	name = name;
	namelen = namelen;
	return ((AsnIdType) 0);
}

static	MixStatusType	misSet (cookie, name, namelen, asn)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;
AsnIdType		asn;

{
	cookie = cookie;
	name = name;
	namelen = namelen;
	asn = asn;
	return (smpErrorGeneric);
}

static	AsnIdType	misNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	cookie = cookie;
	name = name;
	namelenp = namelenp;
	return ((AsnIdType) 0);
}

static	MixOpsType		misOps	= {

				misRelease,
				misCreate,
				misDestroy,
				misNext,
				misGet,
				misSet

				};

#endif		/*	NOTDEF	*/

CVoidType		misInit ()

{
	misAccess = (MisAccessType) TRUE;
	misTree = mixNew ();
}
