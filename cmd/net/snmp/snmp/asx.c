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
#ident	"$Header: asx.c,v 1.2.1.2 90/05/09 17:26:17 wje Exp $"

/*
 *	$Header: asx.c,v 1.2.1.2 90/05/09 17:26:17 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctypes.h>
#include	<debug.h>
#include	<asn.h>
#include	<asx.h>

#define			asxBufSize		(512)

CVoidType		asxInit ()

{
}

static	CUnsfType	asxTab (level)

CUnsfType		level;

{
	while (level-- != 0) {
		printf ("\t");
	}
	return (level);
}

CBytePtrType		asxTypeToLabel (type)

AsnTypeType		type;

{
	char		*result;

	switch (type) {
	
	case asnTypeInteger:
		result = "INTEGER";
		break;

	case asnTypeOctetString:
		result = "OCTETSTRING";
		break;

	case asnTypeObjectId:
		result = "OBJECTIDENTIFIER";
		break;

	case asnTypeNull:
		result = "NULL";
		break;

	case asnTypeSequence:
		result = "SEQUENCE";
		break;

	case asnTypeSequenceOf:
		result = "SEQUENCE OF";
		break;

	default:
		result = "??";
		break;
	}

	return ((CBytePtrType) result);
}

AsxStatusType		asxPrint (asn, level)

AsnIdType		asn;
CUnsfType		level;

{
	if (asn == (AsnIdType) 0) {
		return (errOk);
	}
	(void) asxTab (level);
	printf ("[ ");
	switch (asnClass (asn)) {
	
	case asnClassUniversal:
		printf ("U");
		break;

	case asnClassContext:
		printf ("C");
		break;

	case asnClassApplication:
		printf ("A");
		break;

	case asnClassPrivate:
		printf ("P");
		break;

	default:
		printf ("?");
		break;
	}
	printf (" %d ] %s {\n", asnTag (asn),
		asxTypeToLabel (asnType (asn)));
	switch (asnType (asn)) {

	case asnTypeInteger:
	case asnTypeOctetString:
	case asnTypeObjectId:
		{
			CByteType		buf [ asxBufSize ];
			CBytePtrType		cp;
			AsnLengthType		n;

			cp = buf;
			(void) asxTab (level + 1);
			for (n = asnContents (asn, buf, asxBufSize); n > 0;
				n--) {
				printf (" %02.02X", *cp++);
			}
			printf ("\n");
		}
		break;

	case asnTypeSequence:
	case asnTypeSequenceOf:
		{
			AsnIndexType		i;
			AsnIdType		item;

			for (i = 1;  i <= asnSons (asn);
				item = asnFree (item)) {
				item = asnComponent (asn, i);
				(void) asxPrint (item, level + 1);
				i += (asnSons (item) + 1);
			}
		}
		break;

	default:
		break;
	}

	(void) asxTab (level);
	printf ("}\n");
	return (errOk);
}

