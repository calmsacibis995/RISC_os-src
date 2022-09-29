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
#ident	"$Header: smx.c,v 1.6.1.3 90/05/09 17:32:28 wje Exp $"

/*
 *	$Header: smx.c,v 1.6.1.3 90/05/09 17:32:28 wje Exp $
 *	Author: J. Davin
 *	Copyright 1988, 1989, Massachusetts Institute of Technology
 *	See permission and disclaimer notice in file "notice.h"
 */

#include	<notice.h>

#include	<ctype.h>
#include	<netdb.h>
#include	<sys/types.h>
#include	<sys/socket.h>
#include	<netinet/in.h>

#include	<ctypes.h>
#include	<local.h>
#include	<debug.h>
#include	<smp.h>
#include	<smx.h>
#include	<oid.h>
#include	<asn.h>

extern		CBoolType	DisplayInternetNumbers;

static	CCharPtrType		smxKindVector []	= {

	"None",
	"Integer",
	"OctetString",
	"IPAddr",
	"Opaque",
	"Counter",
	"Guage",
	"TimeTicks",
	"ObjectId",
	"Null",

};

static	CCharPtrType		smxErrorVector []	= {

	"noError",
	"tooBig",
	"noSuchName",
	"badValue",
	"readOnly",
	"genErr"
};

SmpErrorType		smxTextToError (s)

CCharPtrType		s;

{
	CIntfType		i;

	for (i = (CIntfType) smpErrorNone;
		((i != (CIntfType) smpErrorGeneric) &&
		(strcmp (smxErrorVector [ i ], s) != 0)); i++);
	return ((strcmp (smxErrorVector [ i ], s) != 0) ? smpErrorNone :
		(SmpErrorType) i);
}

CCharPtrType		smxErrorToText (error)

SmpErrorType		error;

{
	return (smxErrorVector [ (CIntfType) error ]);
}

SmpKindType		smxTextToKind (s)

CCharPtrType		s;

{
	CIntfType		i;

	for (i = (CIntfType) smpKindNone; ((i != (CIntfType) smpKindNull) &&
		(lc_cmp (smxKindVector [ i ], s) != 0)); i++);
	return ((lc_cmp (smxKindVector [ i ], s) != 0) ? smpKindNone :
		(SmpKindType) i);
}

CCharPtrType		smxKindToText (kind)

SmpKindType		kind;

{
	return (smxKindVector [ (CIntfType) kind ]);
}

CIntfType		smxObjectIdToText (text, n, value, m, enums)

CCharPtrType		text;
CIntfType		n;
CBytePtrType		value;
CIntfType		m;
asn_enum **		enums;

{
	return (oidDecode (text, n, value, m, enums));
}

CIntfType		smxTextToObjectId (value, m, text)

CBytePtrType		value;
CIntfType		m;
CCharPtrType		text;

{
	return (oidEncode (value, m, text));
}

CIntfType		smxOctetStringToText (text, n, value, m)

CCharPtrType		text;
CIntfType		n;
CBytePtrType		value;
CIntfType		m;

{
	CIntfType		s;
	CIntfType		k;
	CByteType		c;
	CUnslType		hex;
	CBytePtrType		bp;

	if (m > n) {
		return ((CIntfType) -1);
	}

	hex = 0;

	for (bp = value, k = 0; k < m; bp++, k++) {
		if (!(isprint (*bp) || (*bp == '\12')))
			hex = 1;
	}

	if (hex) {
		s = n - 1;
		if (s > 6) {
			s -= 6;
			bcopy ("(hex) ", text, 6);
			text += 6;
		}
		while ((s > 0) && (m > 0)) {
			c = (CByteType) *value++;
			if ((k = rdxEncode16 (text, s, (CUnslType) c)) <
				(CIntfType) 0) {
				s = (CIntfType) -1;
			} else {
				text += k;
				s -= k;
			}
			m--;
			if (s > 0) {
				*text = 040;
				text++;
				s--;
			}
		}
	} else {
		if (n > 0) {
			*text++ = (CCharType) '"';
		}

		for (s = n - 1; ((s > 0) && (m > 0)); m--) {
			c = (CByteType) *value++;
			*text++ = (CCharType) c;
			s--;
		}
	}

	if (m > 0) {
		s = (CIntfType) -1;
	} else if (hex) {
		*text = (CCharType) 0;
		s = n - s;
	} else if (s > (CIntfType) 0) {
		*text++ = (CCharType) '"';
		s--;
		*text = (CCharType) 0;
		s = n - s;
	} else {
		s = (CIntfType) -1;
	}
	return (s);
}

CIntfType		smxTextToOctetString (value, m, text)

CCharPtrType		text;
CBytePtrType		value;
CIntfType		m;

{
	CIntfType		k;

	if ((k = (CIntfType) strlen (text)) > m) {
		k = (CIntfType) -1;
	}
	else {
		(void) strcpy ((char *) value, (char *) text);
	}
	return (k);
}

CIntfType		smxTextToIPAddr (value, m, text)

CBytePtrType		value;
CIntfType		m;
CCharPtrType		text;

{
	CCharType		c;
	CCharType		num [ 32 ];
	CCharPtrType		np;
	CUnslType		octet;
	CIntfType		nn;

	struct hostent *	host;
	struct netent *		net;

	if (m < (CIntfType) 4) {
		return ((CIntfType) -1);
	}

/* First see if we can encode the hostname into an address. */

	if (host = gethostbyname (text)) {
		bcopy (host->h_addr_list[0], value, 4);
		return (4);
	}
			

	if (net = getnetbyname (text)) {
		bcopy (net->n_net, value, 4);
		return (4);
	}
			

/* If not, convert the address into a host number. */

	np = num;
	nn = (CIntfType) 32;

	for (m = (CIntfType) 3; ((c = *text) != (CCharType) 0) &&
		(m > (CIntfType) 0); text++) {
		if (c != (CCharType) '.') {
			if (nn > (CIntfType) 0) {
				nn--;
				*np++ = c;
			}
			else {
				m = (CIntfType) -1;
			}
		}
		else {
			*np = (CCharType) 0;
			np = num;
			nn = (CIntfType) 32;
			if (rdxDecodeAny (& octet, num) < (CIntfType) 0) {
				m = (CIntfType) -1;
			}
			else if (octet > (CUnslType) 0xFF) {
				m = (CIntfType) -1;
			}
			else {
				*value++ = (CByteType) octet;
				m--;
			}
		}
	}

	if (m != (CIntfType) 0) {
		m = (CIntfType) -1;
	}
	else if (c == (CCharType) 0) {
		m = (CIntfType) -1;
	}
	else if (rdxDecodeAny (& octet, text) < (CIntfType) 0) {
		m = (CIntfType) -1;
	}
	else if (octet > (CUnslType) 0xFF) {
		m = (CIntfType) -1;
	}
	else {
		*value++ = (CByteType) octet;
		m = (CIntfType) 4;
	}

	return (m);
}

CIntfType		smxIPAddrToText (text, n, value, m)

CCharPtrType		text;
CIntfType		n;
CBytePtrType		value;
CIntfType		m;

{
	CIntfType		k;
	CIntfType		s;
	struct hostent *	host;
	struct netent *		net;
	u_long			netaddr;
	u_long			mask, subnetshift;

	if (m != (CIntfType) 4) {
		return ((CIntfType) -1);
	}

/* First see if we can decode the address into a hostname. */

	if (!DisplayInternetNumbers) {
		if (host = gethostbyaddr (value, m, AF_INET)) {
			s = strlen (host->h_name);
			if ((s > 0) && (s < n)) {
				strcpy (text, host->h_name);
				return (n - s);
			}
		}
			
		bcopy (value, (char *)&netaddr, sizeof (long));
		if (IN_CLASSA(netaddr)) {
			mask = IN_CLASSA_NET;
			subnetshift = 8;
		} else if (IN_CLASSB(netaddr)) {
			mask = IN_CLASSB_NET;
			subnetshift = 8;
		} else {
			mask = IN_CLASSC_NET;
			subnetshift = 4;
		}
		/*
		 * If there are more bits than the standard mask
		 * would suggest, subnets must be in use.
		 * Guess at the subnet mask, assuming reasonable
		 * width subnet fields.
		 */
		while (netaddr &~ mask)
			mask = (long)mask >> subnetshift;

		netaddr = netaddr & mask;
		while ((mask & 1) == 0)
			mask >>= 1, netaddr >>= 1;
		if (net = getnetbyaddr (netaddr, AF_INET)) {
			s = strlen (net->n_name);
			if ((s > 0) && (s < n)) {
				strcpy (text, net->n_name);
				return (n - s);
			}
		}
	}

/* If not, convert the address into a host number. */

	for (s = n; ((m != (CIntfType) 0) && (s > (CIntfType) 0)); m--) {
		k = rdxEncode10 (text, n, (CUnslType) *value++);
		if (k < (CIntfType) 0) {
			s = (CIntfType) -1;
		}
		else {
			s -= k;
			text += k;
			*text++ = (CCharType) '.';
			s--;
		}
	}

	if (s >= (CIntfType) 0) {
		if (n > (CIntfType) 0) {
			s++;
			*(--text) = (CCharType) 0;
		}
		s = n - s;
	}
	else {
		s = (CIntfType) -1;
	}
	return (s);
}

CIntfType		smxTextToCounter (value, text)

CUnslPtrType		value;
CCharPtrType		text;

{
	return (rdxDecodeAny (value, text));
}

CIntfType		smxCounterToText (text, n, value)

CCharPtrType		text;
CIntfType		n;
CUnslType		value;

{
	return (rdxEncode10 (text, n, value));
}

CIntfType		smxTimeTicksToText (text, n, value)

CCharPtrType		text;
CIntfType		n;
CUnslType		value;

{
	int seconds, minutes, hours, days;
	char buf[128];
	int k;


	sprintf (buf, "(%d) ", value);
	k = strlen (buf);
	if (k > n)
		return (-1);
	else {
		strcpy (text, buf);
		text += k;
	}

	value /= 100;
	days = value / (60 * 60 * 24);
	value %= (60 * 60 * 24);

	hours = value / (60 * 60);
	value %= (60 * 60);

	minutes = value / 60;
	seconds = value % 60;

	if (days == 0){
		sprintf(buf, "%d:%02d:%02d", hours, minutes, seconds);
	} else if (days == 1) {
		sprintf(buf, "%d day, %d:%02d:%02d", days, hours, minutes, seconds);
	} else {
		sprintf(buf, "%d days, %d:%02d:%02d", days, hours, minutes, seconds);
	}

	k += strlen (buf);

	if (k > n) {
		k = -1;
	} else {
		strcpy (text, buf);
	}

	return (k);
}

CIntfType		smxGuageToText (text, n, value)

CCharPtrType		text;
CIntfType		n;
CUnslType		value;

{
	return (rdxEncode10 (text, n, value));
}

CIntfType		smxTextToGuage (value, text)

CUnslPtrType		value;
CCharPtrType		text;

{
	return (rdxDecodeAny (value, text));
}

CIntfType		smxTextToInteger (value, text)

CIntlPtrType		value;
CCharPtrType		text;

{
	CIntfType		status;
	CBoolType		hassign;

	if (text == (CCharPtrType) 0) {
		return ((CIntfType) -1);
	}

	hassign = FALSE;
	if (*text == (CCharType) '-') {
		text++;
		hassign = TRUE;
	}
	status = rdxDecodeAny ((CUnslPtrType) value, text);
	if (hassign) {
		*value = (- *value);
	}
	return (status);
}

CIntfType		smxIntegerToText (text, n, value)

CCharPtrType		text;
CIntfType		n;
CIntlType		value;

{
	CIntfType		s;
	CIntfType		k;

	s = (CIntfType) 0;
	if (value < 0) {
		if (n <= 0) {
			k = (CIntfType) -1;
		}
		else {
			*text++ = (CCharType) '-';
			n--;
			s++;
			value = (- value);
		}
	}

	if (n <= 0) {
		k = (CIntfType) -1;
	}
	else if ((k = rdxEncode10 (text, n, (CUnslType) value)) <
		(CIntfType) 0) {
		k = (CIntfType) -1;
	}
	else {
		k += s;
	}

	return (k);
}

CIntfType		smxValueToText (text, n, bind, enums)

CCharPtrType		text;
CIntfType		n;
SmpBindPtrType		bind;
asn_enum *		enums;

{
	CIntfType		j, k;
	CIntfType		found;
	CCharType		buf[64];

	DEBUG2 ("smxValueToText: Kind %d Len %d\n",
		bind->smpBindKind, bind->smpBindValueLen);
	switch (bind->smpBindKind) {

	case smpKindInteger:
		DEBUG0 ("smxValueToText 0\n");
		if (bind->smpBindValueLen >
			(SmpLengthType) sizeof (CIntlType)) {
			DEBUG0 ("smxValueToText 1\n");
			if (n > (CIntfType) strlen ("OVERFLOW")) {
				(void) strcpy ((char *) text, "OVERFLOW");
			}
			else {
				k = (CIntfType) -1;
			}
		}
		else {
			if (enums != (asn_enum *) 0) {
				found = 0;
				for (; enums->enum_text; enums++) {
					if (enums->enum_number == bind->smpBindNumber){
						if ((j = (CIntfType) strlen (enums->enum_text)) <= n) {
							(void) strcpy ((char *) text, enums->enum_text);
						}
						DEBUG0 ("smxValueToText 2\n");
						k = smxIntegerToText (buf,
							n - j, (CIntlType)
							bind->smpBindNumber);
						if (k + j + 2 < n) {
							text += j;
							*text = '(';
							text++;
							bcopy (buf, text, k);
							text += k;
							*text = ')';
							text++;
							*text = '\0';
							k = k + j + 2;
						} else k = j;
						found = 1;
						break;
					}
				}
				if (found)
					break;
			}

			DEBUG0 ("smxValueToText 2\n");
			k = smxIntegerToText (text, n, (CIntlType)
				bind->smpBindNumber);
		}
		break;
	
	case smpKindCounter:
	case smpKindGuage:
	case smpKindTimeTicks:
		DEBUG0 ("smxValueToText 3\n");
		if (asnNegative ((CBytePtrType) bind->smpBindValue,
			(AsnLengthType) bind->smpBindValueLen)) {
			DEBUG0 ("smxValueToText 4\n");
			if (n > (CIntfType) strlen ("NEGATIVE")) {
				(void) strcpy ((char *) text, "NEGATIVE");
			}
			else {
				k = (CIntfType) -1;
			}
		}
		else if ((bind->smpBindValueLen >
			(SmpLengthType) sizeof (CUnslType)) &&
			(*((CBytePtrType) bind->smpBindValue) !=
			(CByteType) 0)) {
			DEBUG0 ("smxValueToText 5\n");
			if (n > (CIntfType) strlen ("OVERFLOW")) {
				(void) strcpy ((char *) text,
					"OVERFLOW");
			}
			else {
				k = (CIntfType) -1;
			}
		}
		else {
			DEBUG0 ("smxValueToText 6\n");
			if (bind->smpBindKind == smpKindTimeTicks) {
				k = smxTimeTicksToText (text, n, (CUnslType)
							bind->smpBindNumber);
				if (k < 0)
					k = smxCounterToText (text, n,
						(CUnslType)
						bind->smpBindNumber);
			} else {
				k = smxCounterToText (text, n, (CUnslType)
					bind->smpBindNumber);
			}
		}
		break;
	
	case smpKindObjectId:
		k = smxObjectIdToText (text, n,
			(CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen, 0);
		break;

	case smpKindOctetString:
		k = smxOctetStringToText (text, n,
			(CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen);
		break;

	case smpKindIPAddr:
		k = smxIPAddrToText (text, n,
			(CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen);
		break;

	case smpKindNone:
	case smpKindOpaque:
	case smpKindNull:
	default:
		k = (CIntfType) -1;
		break;

	}
	return (k);
}

CIntfType		smxTextToValue (bind, text)

SmpBindPtrType		bind;
CCharPtrType		text;

{
	CIntfType		k;

	switch (bind->smpBindKind) {

	case smpKindInteger:
		k = smxTextToInteger ((CIntlPtrType) & bind->smpBindNumber,
			text);
		bind->smpBindValueLen = (SmpLengthType) 0;
		bind->smpBindValue = (CBytePtrType) 0;
		break;
	
	case smpKindCounter:
		k = smxTextToCounter ((CUnslPtrType) & bind->smpBindNumber,
			text);
		bind->smpBindValueLen = (SmpLengthType) 0;
		bind->smpBindValue = (CBytePtrType) 0;
		break;
	
	case smpKindGuage:
		k = smxTextToCounter ((CUnslPtrType) & bind->smpBindNumber,
			text);
		bind->smpBindValueLen = (SmpLengthType) 0;
		bind->smpBindValue = (CBytePtrType) 0;
		break;
	
	case smpKindObjectId:
		k = smxTextToObjectId ((CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen, text);
		bind->smpBindValueLen = (SmpLengthType) k;
		bind->smpBindNumber = (SmpNumberType) 0;
		break;

	case smpKindOctetString:
		k = smxTextToOctetString ((CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen, text);
		bind->smpBindValueLen = (SmpLengthType) k;
		bind->smpBindNumber = (SmpNumberType) 0;
		break;

	case smpKindIPAddr:
		k = smxTextToIPAddr ((CBytePtrType) bind->smpBindValue,
			(CIntfType) bind->smpBindValueLen, text);
		bind->smpBindValueLen = (SmpLengthType) k;
		bind->smpBindNumber = (SmpNumberType) 0;
		break;

	case smpKindNone:
	case smpKindNull:
	case smpKindOpaque:
	default:
		k = (CIntfType) -1;
		break;

	}
	return (k);
}

