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
#ident	"$Header: interfaces.c,v 1.4.1.2 90/05/09 17:28:27 wje Exp $"

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

#include	<interfaces.h>
#include	<stubs.h>

static	AsnIdType	interfacesRetrieve (mix, info)

MixCookieType	mix;
struct ifentry *info;

{
	switch (mix) {
	case 1:
	/* ifIndex */
		return (asnMakeInteger (info->if_index));

	case 2:
	/* ifDescr */
		return (asnMakeOctetString (info->if_descr, info->if_descrlen));

	case 3:
	/* ifType */
		return (asnMakeInteger (info->if_type));

	case 4:
	/* ifMtu */
		return (asnMakeInteger (info->if_mtu));

	case 5:
	/* ifSpeed */
		return (asnMakeGauge (info->if_speed));

	case 6:
	/* ifPhysAddress */
		return (asnMakeOctetString (info->if_physaddr, info->if_physaddrlen));

	case 7:
	/* ifAdminStatus */
		return (asnMakeInteger (info->if_adminstat));

	case 8:
	/* ifOperStatus */
		return (asnMakeInteger (info->if_operstat));

	case 9:
	/* ifLastChange */
		return (asnMakeTimeTicks (info->if_uptime));

	case 10:
	/* ifInOctets */
		return (asnMakeCounter (info->if_rbyte));

	case 11:
	/* ifInUcastPkts */
		return (asnMakeCounter (info->if_rdirpkt));

	case 12:
	/* ifInNUcastPkts */
		return (asnMakeCounter (info->if_rcastpkt));

	case 13:
	/* ifInDiscards */
		return (asnMakeCounter (info->if_rdiscard));

	case 14:
	/* ifInErrors */
		return (asnMakeCounter (info->if_rerr));

	case 15:
	/* ifInUnknownProtos */
		return (asnMakeCounter (info->if_rbadproto));

	case 16:
	/* ifOutOctets */
		return (asnMakeCounter (info->if_xbyte));

	case 17:
	/* ifOutUcastPkts */
		return (asnMakeCounter (info->if_xdirpkt));

	case 18:
	/* ifOutNUcastPkts */
		return (asnMakeCounter (info->if_xcastpkt));

	case 19:
	/* ifOutDiscards */
		return (asnMakeCounter (info->if_xdiscard));

	case 20:
	/* ifOutErrors */
		return (asnMakeCounter (info->if_xerr));

	case 21:
	/* ifOutQLen */
		return (asnMakeGauge (info->if_xqlen));

	default:
		return ((AsnIdType) 0);
	}
}

AsnIdType	interfacesTotal ()

{
	int i, cnt;


	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFNUMBER, &cnt, 0);
	if (i >= 0) 
		return (asnMakeInteger ((CUnslType) cnt));
	else return ((AsnIdType) 0);
}

static	AsnIdType	ifGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	if ((namelen == 1) && (*name == 0)) {
		return (interfacesTotal ());
	} else {
		return ((AsnIdType) 0);
	}
}

static	AsnIdType	ifNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	if (*namelenp == (MixLengthType) 0) {
		*namelenp = 1;
		name[0] = 0;
		return (interfacesTotal ());
	} else {
		return ((AsnIdType) 0);
	}
}

static	AsnIdType	ifTableGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	struct ifentry ifent;
	int	i;

	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFTABLE_IFENTRY, &ifent, *name);

	if (i > 0) {
		return (interfacesRetrieve (cookie, &ifent));
	} else return ((AsnIdType) 0);
}

static	AsnIdType	ifTableNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	struct ifentry ifent;
	int	i;
	CUnslType number;

	if (*namelenp == (MixLengthType) 0) {
		number = 1;
		*namelenp = 1;
		name[0] = 1;
	} else {
		number = NametoLong (name, *namelenp);
		number++;
		*namelenp = 0;
		LongtoName (number, name, namelenp);
	}

	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_INTERFACES_IFTABLE_IFENTRY, &ifent, number);

	if (i > 0) {
		return (interfacesRetrieve (cookie, &ifent));
	} else return ((AsnIdType) 0);
}

static	MixOpsType	ifOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			ifNext,
			ifGet,
			stubsSet

			};

static	MixOpsType	ifTableOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			ifTableNext,
			ifTableGet,
			stubsSet

			};

CVoidType		interfacesInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\1",
		(MixLengthType) 7, & ifOps, (MixCookieType) 0);

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\1",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\2",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\3",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\4",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\5",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 5);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\6",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 6);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\7",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 7);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\10",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 8);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\11",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 9);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\12",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 10);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\13",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 11);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\14",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 12);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\15",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 13);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\16",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 14);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\17",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 15);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\20",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 16);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\21",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 17);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\22",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 18);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\23",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 19);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\24",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 20);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\2\2\1\25",
		(MixLengthType) 9, & ifTableOps, (MixCookieType) 21);

}
