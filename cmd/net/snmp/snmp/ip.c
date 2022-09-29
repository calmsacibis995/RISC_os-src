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
#ident	"$Header: ip.c,v 1.4.1.2 90/05/09 17:28:39 wje Exp $"

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

#include	<ip.h>
#include	<stubs.h>

#include	<gated.h>
#include	<snmp.h>

static	struct ipinfo		ipTable;
static	struct ipaddrtab	ipAddr;
static	struct iproutab		ipRouting;

static	AsnIdType	ipRetrieve (mix, info)

MixCookieType		mix;
struct ipinfo *		info;

{
	if (info != (struct ipinfo *) 0) {
		switch (mix) {
		case 1:
		/* ipForwarding */
			return (asnMakeInteger (info->ip_forwarding));

		case 2:
		/* ipDefaultTTL */
			return (asnMakeInteger (info->ip_defttl));

		case 3:
		/*ipInReceives */
			return (asnMakeCounter (info->ip_rpkts));

		case 4:
		/* ipInHdrErrors */
			return (asnMakeCounter (info->ip_rhdrerr));

		case 5:
		/* ipInAddrErrors */
			return (asnMakeCounter (info->ip_raddrerr));

		case 6:
		/* ipForwDatagrams */
			return (asnMakeCounter (info->ip_forwarded));

		case 7:
		/* ipInUnknownProtos */
			return (asnMakeCounter (info->ip_badproto));

		case 8:
		/* ipInDiscards */
			return (asnMakeCounter (info->ip_rdiscard));

		case 9:
		/* ipInDelivers */
			return (asnMakeCounter (info->ip_delivers));

		case 10:
		/* ipOutRequests */
			return (asnMakeCounter (info->ip_xpkts));

		case 11:
		/* ipOutDiscards */
			return (asnMakeCounter (info->ip_xdiscard));

		case 12:
		/* ipOutNoRoutes */
			return (asnMakeCounter (info->ip_noroute));

		case 13:
		/* ipReasmTimeout */
			return (asnMakeInteger (info->ip_reasm_timeout));

		case 14:
		/* ipReasmReqds */
			return (asnMakeCounter (info->ip_reasm_req));

		case 15:
		/* ipReasmOKs */
			return (asnMakeCounter (info->ip_reasm_ok));

		case 16:
		/* ipReasmFails */
			return (asnMakeCounter (info->ip_reasm_fail));

		case 17:
		/* ipFragOKs */
			return (asnMakeCounter (info->ip_frag_ok));

		case 18:
		/* ipFragFails */
			return (asnMakeCounter (info->ip_frag_fail));

		case 19:
		/* ipFragCreates */
			return (asnMakeCounter (info->ip_frag));

		default:
			return ((AsnIdType) 0);
		}
	} else
		return ((AsnIdType) 0);
}

static	AsnIdType	ipAddrRetrieve (mix, info)

MixCookieType		mix;
struct ipaddrtab *	info;

{
	if (info != (struct ipaddrtab *) 0) {
		switch (mix) {
		case 1:
		/* ipAdEntAddr */
			return (asnMakeIpAddress (&info->ipa_addr));

		case 2:
		/* ipAdEntIfIndex */
			return (asnMakeInteger (info->ipa_ifindex));

		case 3:
		/* ipAdEntNetMask */
			return (asnMakeIpAddress (&info->ipa_mask));

		case 4:
		/* ipAdEntBcastAddr */
			return (asnMakeInteger (info->ipa_bcast));

		default:
			return ((AsnIdType) 0);

		}
	} else
		return ((AsnIdType) 0);
}

static	AsnIdType	ipRoutingRetrieve (mix, info)

MixCookieType		mix;
struct iproutab *	info;

{
	AsnIdType		result;
	struct	rtentry		*rp;
	struct	sockaddr_in	*sin;
	CIntlType		value;
	struct snmpmsg		msg;
	struct snmpmsg *	msgp;

	switch (mix) {

	case 1:
	/* ipRouteDest */
		return (asnMakeIpAddress (&info->ipr_dest));

	case 2:
	/* ipRouteIfIndex */
		return (asnMakeInteger (info->ipr_ifindex));
		break;

	case 3:
	/* ipRouteMetric1 */
		msg.type = SNMP_IPROUTEMETRIC1;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEMETRIC1) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 4:
	/* ipRouteMetric2 */
		msg.type = SNMP_IPROUTEMETRIC2;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEMETRIC2) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 5:
	/* ipRouteMetric3 */
		msg.type = SNMP_IPROUTEMETRIC3;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEMETRIC3) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 6:
	/* ipRouteMetric4 */
		msg.type = SNMP_IPROUTEMETRIC4;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEMETRIC4) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
		break;

	case 7:
	/* ipRouteNextHop */
		msg.type = SNMP_IPROUTENEXTHOP;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTENEXTHOP) {
			return (asnMakeIpAddress (&msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 8:
	/* ipRouteType */
		msg.type = SNMP_IPROUTETYPE;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTETYPE) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 9:
	/* ipRouteProto */
		msg.type = SNMP_IPROUTEPROTO;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEPROTO) {
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 10:
	/* ipRouteAge */
		msg.type = SNMP_IPROUTEAGE;
		msg.index = info->ipr_dest;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_IPROUTEAGE) {
			return (asnMakeInteger (msgp->value));
		} else {
			return  ((AsnIdType) 0);
		}

	default:
		return (AsnIdType) 0;
	}

	return (result);
}

static	struct ipinfo *	ipGetEntry (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{
	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_IP, &ipTable, 0) > 0)
		return (&ipTable);
	else return ((struct ipinfo *) 0);
}

static	AsnIdType	ipGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (ipRetrieve (cookie, ipGetEntry (name, namelen)));
}

static	AsnIdType	ipAddrGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	AsnIdType		result;
	CUnsfType		addr;
	int			i;

	addr = NametoIpAddr (name, namelen);

	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPADDRTABLE_IPADDRENTRY, &ipAddr, addr);
	
	if ((i == 0) || (i == -1)) {
		result = (AsnIdType) 0;
	}
	else {
		result = ipAddrRetrieve (cookie, &ipAddr);
	}

	return (result);
}

static	AsnIdType	ipRoutingGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	AsnIdType		result;
	CUnsfType		addr;
	int			i;

	addr = NametoIpAddr (name, namelen);

	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPROUTINGTABLE_IPROUTEENTRY, &ipRouting, addr);
	
	if ((i == 0) || (i == -1)) {
		result = (AsnIdType) 0;
	}
	else {
		result = ipRoutingRetrieve (cookie, &ipRouting);
	}

	return (result);
}

static	AsnIdType	ipNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	if (*namelenp == (MixLengthType) 0) {
		*namelenp = 1;
		name[0] = 0;
		return (ipRetrieve (cookie, ipGetEntry (name, *namelenp)));
	} else {
		return ((AsnIdType) 0);
	}
}

static	AsnIdType	ipRoutingNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	AsnIdType		result;
	CUnsfType		addr;
	int			i;

	addr = NametoIpAddr (name, *namelenp);

	i = syscall (SYS_sysmips, MIPS_GET_NEXT_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPROUTINGTABLE_IPROUTEENTRY, &ipRouting, addr);
	
	if ((i == 0) || (i == -1)) {
		result = (AsnIdType) 0;
	}
	else {
		result = ipRoutingRetrieve (cookie, &ipRouting);
	}

	*namelenp = 0;
	IpAddrtoName (ipRouting.ipr_dest, name, namelenp);

	return (result);
}

static	AsnIdType	ipAddrNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	AsnIdType		result;
	CUnsfType		addr;
	int			i;

	addr = NametoIpAddr (name, *namelenp);

	i = syscall (SYS_sysmips, MIPS_GET_NEXT_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_IP_IPADDRTABLE_IPADDRENTRY, &ipAddr, addr);
	
	if ((i == 0) || (i == -1)) {
		result = (AsnIdType) 0;
	}
	else {
		result = ipAddrRetrieve (cookie, &ipAddr);
	}

	*namelenp = 0;
	IpAddrtoName (ipAddr.ipa_addr, name, namelenp);

	return (result);
}

static	MixOpsType	ipOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			ipNext,
			ipGet,
			stubsSet

			};

static	MixOpsType	ipAddrOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			ipAddrNext,
			ipAddrGet,
			stubsSet

			};

static	MixOpsType	ipRoutingOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			ipRoutingNext,
			ipRoutingGet,
			stubsSet

			};

CVoidType		ipInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\1",
		(MixLengthType) 7, & ipOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\2",
		(MixLengthType) 7, & ipOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\3",
		(MixLengthType) 7, & ipOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\4",
		(MixLengthType) 7, & ipOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\5",
		(MixLengthType) 7, & ipOps, (MixCookieType) 5);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\6",
		(MixLengthType) 7, & ipOps, (MixCookieType) 6);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\7",
		(MixLengthType) 7, & ipOps, (MixCookieType) 7);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\10",
		(MixLengthType) 7, & ipOps, (MixCookieType) 8);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\11",
		(MixLengthType) 7, & ipOps, (MixCookieType) 9);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\12",
		(MixLengthType) 7, & ipOps, (MixCookieType) 10);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\13",
		(MixLengthType) 7, & ipOps, (MixCookieType) 11);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\14",
		(MixLengthType) 7, & ipOps, (MixCookieType) 12);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\15",
		(MixLengthType) 7, & ipOps, (MixCookieType) 13);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\16",
		(MixLengthType) 7, & ipOps, (MixCookieType) 14);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\17",
		(MixLengthType) 7, & ipOps, (MixCookieType) 15);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\20",
		(MixLengthType) 7, & ipOps, (MixCookieType) 16);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\21",
		(MixLengthType) 7, & ipOps, (MixCookieType) 17);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\22",
		(MixLengthType) 7, & ipOps, (MixCookieType) 18);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\23",
		(MixLengthType) 7, & ipOps, (MixCookieType) 19);

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\24\1\1",
		(MixLengthType) 9, & ipAddrOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\24\1\2",
		(MixLengthType) 9, & ipAddrOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\24\1\3",
		(MixLengthType) 9, & ipAddrOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\24\1\4",
		(MixLengthType) 9, & ipAddrOps, (MixCookieType) 4);

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\1",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\2",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\3",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\4",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\5",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 5);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\6",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 6);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\7",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 7);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\10",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 8);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\11",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 9);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\4\25\1\12",
		(MixLengthType) 9, & ipRoutingOps, (MixCookieType) 10);

}
