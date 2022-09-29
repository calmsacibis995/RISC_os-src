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
#ident	"$Header: egp.c,v 1.5.1.2 90/05/09 17:27:05 wje Exp $"

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

#include	<stubs.h>
#include	<gated.h>
#include	<snmp.h>
#include	<utils.h>


static	AsnIdType	egpRetrieve (mix)

MixCookieType		mix;

{
	struct snmpmsg msg;
	struct snmpmsg * msgp;

	switch (mix) {
	case 0:
		msg.type = SNMP_EGPINMSGS;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPINMSGS) {
			return (asnMakeCounter (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
		break;

	case 1:
		msg.type = SNMP_EGPINERRORS;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPINERRORS) {
			return (asnMakeCounter (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
		break;

	case 2:
		msg.type = SNMP_EGPOUTMSGS;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPOUTMSGS) {
			return (asnMakeCounter (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
		break;

	case 3:
		msg.type = SNMP_EGPOUTERRORS;
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPOUTERRORS) {
			return (asnMakeCounter (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
		break;

	default:
		return ((AsnIdType) 0);
	}
}


static	AsnIdType	egpNeighRetrieve (mix, name, namelenp, next)

MixCookieType		mix;
MixNamePtrType		name;
MixLengthPtrType	namelenp;
CUnslType		next;

{
	struct snmpmsg msg;
	struct snmpmsg * msgp;

	switch (mix) {
	case 0:
		msg.type = SNMP_EGPNEIGHSTATE;
		msg.next = next;
		msg.index = NametoIpAddr (name, *namelenp);
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPNEIGHSTATE) {
			IpAddrtoName (msgp->index, name, namelenp);
			return (asnMakeInteger (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}

	case 1:
		msg.type = SNMP_EGPNEIGHADDR;
		msg.next = next;
		if (next)
			msg.index = NametoIpAddr (name, *namelenp);
		msgp = get_gated_info (&msg);
		if (msgp->type == SNMP_EGPNEIGHADDR) {
			if (next)
				IpAddrtoName (msgp->index, name, namelenp);
			return (asnMakeIpAddress (msgp->value));
		} else {
			return ((AsnIdType) 0);
		}
	default:
		return ((AsnIdType) 0);
	}
}

static	AsnIdType	egpGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (egpRetrieve (cookie));
}

static	AsnIdType	egpNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	if (*namelenp == (MixLengthType) 0) {
		*namelenp = 1;
		name[0] = 0;
		return (egpRetrieve (cookie));
	} else {
		return ((AsnIdType) 0);
	}

}

static	AsnIdType	egpNeighGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (egpNeighRetrieve (cookie, name, &namelen, 0));
}

static	AsnIdType	egpNeighNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	return (egpNeighRetrieve (cookie, name, namelenp, 1));
}

static	MixOpsType	egpOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			egpNext,
			egpGet,
			stubsSet

			};

static	MixOpsType	egpNeighOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			egpNeighNext,
			egpNeighGet,
			stubsSet

			};

CVoidType		egpInit ()

{

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\1",
		(MixLengthType) 7, & egpOps, (MixCookieType) 0);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\2",
		(MixLengthType) 7, & egpOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\3",
		(MixLengthType) 7, & egpOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\4",
		(MixLengthType) 7, & egpOps, (MixCookieType) 3);

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\5\1\1",
		(MixLengthType) 9, & egpNeighOps, (MixCookieType) 0);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\10\5\1\2",
		(MixLengthType) 9, & egpNeighOps, (MixCookieType) 1);

}
