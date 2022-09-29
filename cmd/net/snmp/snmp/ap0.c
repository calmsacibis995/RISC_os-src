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
#ident	"$Header: ap0.c,v 1.1.1.2 90/05/09 17:24:50 wje Exp $"

/*
 *	$Header: ap0.c,v 1.1.1.2 90/05/09 17:24:50 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<aps.h>
#include	<asn.h>


static	AsnIdType	ap0OpDecode (goodies, asn)

ApsGoodiesType		goodies;
AsnIdType		asn;

{
	goodies = goodies;
	return (asnComponent (asn, (AsnIndexType) 3));
}

static	AsnIdType	ap0OpEncode (goodies, asn)

ApsGoodiesType		goodies;
AsnIdType		asn;

{
	AsnIndexType		i;

	goodies = goodies;
	i = (AsnIndexType) 0;
	return (asnComponent (asn, i));
}

static	CBoolType	ap0OpVerify (asn)

AsnIdType		asn;

{
	asn = asn;
	return (TRUE);
}

CVoidType		ap0Init ()

{
	(void) apsScheme ((ApsNameType) "trivial",
		ap0OpVerify, ap0OpEncode, ap0OpDecode);
}

