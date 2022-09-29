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
#ident	"$Header: stubs.c,v 1.1.1.2 90/05/09 17:32:41 wje Exp $"

/*
 *	$Header: stubs.c,v 1.1.1.2 90/05/09 17:32:41 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<error.h>
#include	<debug.h>
#include	<local.h>
#include	<mix.h>
#include	<mis.h>
#include	<avl.h>
#include	<asn.h>

MixStatusType	stubsRelease (cookie)

MixCookieType		cookie;

{
	cookie = cookie;
	return (smpErrorGeneric);
}

MixStatusType	stubsCreate (cookie, name, namelen, asn)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;
AsnIdType		asn;

{
	return (smpErrorGeneric);
}

MixStatusType	stubsDestroy (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (smpErrorGeneric);
}

AsnIdType	stubsGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (smpErrorGeneric);
}

MixStatusType	stubsSet (cookie, name, namelen, asn)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;
AsnIdType		asn;

{
	return (smpErrorGeneric);
}

AsnIdType	stubsNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	return (smpErrorGeneric);
}
