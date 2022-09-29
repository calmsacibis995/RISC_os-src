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
#ident	"$Header: udp.c,v 1.5.1.2 90/05/09 17:33:42 wje Exp $"

#include	<sysv/sys/sysmips.h>
#include	<sysv/sys/limits.h>
#include	<sysv/sys/netman.h>
#include	<sysv/sys.s>

#include	<ctypes.h>
#include	<error.h>
#include	<debug.h>
#include	<local.h>
#include	<mix.h>
#include	<mis.h>
#include	<avl.h>
#include	<asn.h>

#include	<udp.h>
#include	<stubs.h>


static	AsnIdType	udpRetrieve (item)

CIntfType	item;

{
	struct udpinfo		udpent;

	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_UDP, &udpent, 0) <= 0)
	return ((AsnIdType) 0);

	switch (item) {

	case 1:
	/* udpInDatagrams */
		return (asnMakeCounter (udpent.udp_rpkt));

	case 2:
	/* udpNoPorts */
		return (asnMakeCounter (udpent.udp_noport));

	case 3:
	/* udpInErrors */
		return (asnMakeCounter (udpent.udp_rerr));

	case 4:
	/* udpOutDatagrams */
		return (asnMakeCounter (udpent.udp_xpkt));

	}
}

static	AsnIdType	udpGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{

	if ((namelen != (MixLengthType) 1) ||
		(*name != (MixNameType) 0)) {
		return ((AsnIdType) 0);
	} else {
		return (udpRetrieve (cookie));
	}
}

static	AsnIdType	udpNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{

	if (*namelenp == (MixLengthType) 0) {
		*namelenp = (MixLengthType) 1;
		*name = (MixNameType) 0;
		return (udpRetrieve ((CIntfType) cookie));
	} else {
		return ((AsnIdType) 0);
	}
}

static	MixOpsType	udpOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			udpNext,
			udpGet,
			stubsSet

			};

CVoidType		udpInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\7\1",
		(MixLengthType) 7, & udpOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\7\2",
		(MixLengthType) 7, & udpOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\7\3",
		(MixLengthType) 7, & udpOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\7\4",
		(MixLengthType) 7, & udpOps, (MixCookieType) 4);
}

