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
#ident	"$Header: tcp.c,v 1.5.1.2 90/05/09 17:33:04 wje Exp $"

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

#include	<tcp.h>
#include	<stubs.h>


static	CUnslType		tcpConnAddr;

static	CUnslType		tcpAddr;
static	CUnslType		tcpIndex;
static	struct tcpinfo		tcpTable;
static	struct tcpcontab	tcpConnTable;

static	AsnIdType	tcpRetrieve (mix, info)

MixCookieType		mix;
struct tcpinfo *	info;
{
	CIntlType	result;

	if (info != (struct tcpinfo *) 0) {
		switch (mix) {
		case 1:
		/* tcpRtoAlgorithm */
			return (asnMakeInteger (info->tcp_rtalg));

		case 2:
		/* tcpRtoMin */
			return (asnMakeInteger (info->tcp_rtmin));

		case 3:
		/* tcpRtoMax */
			return (asnMakeInteger (info->tcp_rtmax));

		case 4:
		/* tcpMaxConn */
			return (asnMakeInteger (info->tcp_maxcon));

		case 5:
		/* tcpActiveOpens */
			return (asnMakeCounter (info->tcp_actopen));

		case 6:
		/* tcpPassiveOpens */
			return (asnMakeCounter (info->tcp_pasopen));

		case 7:
		/* tcpAttemptFails */
			return (asnMakeCounter (info->tcp_fails));

		case 8:
		/* tcpEstabResets */
			return (asnMakeCounter (info->tcp_est_reset));

		case 9:
		/* tcpCurrEstab */
			return (asnMakeGauge (info->tcp_estab));

		case 10:
		/* tcpInSegs */
			return (asnMakeCounter (info->tcp_rpkt));

		case 11:
		/* tcpOutSegs */
			return (asnMakeCounter (info->tcp_xpkt));

		case 12:
		/* tcpRetransSegs */
			return (asnMakeCounter (info->tcp_retrans));

		default:
			return ((AsnIdType) 0);
		}
	} else
		return ((AsnIdType) 0);
}

static struct tcpinfo *	tcpGetEntry (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{

	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP, &tcpTable, 0) > 0)
		return (&tcpTable);
	else return ((struct tcpinfo *) 0);
}

static	AsnIdType	tcpConnRetrieve (mix, info)

MixCookieType		mix;
struct tcpcontab *	info;

{
	if (info != (struct tcpcontab *) 0) {
		switch (mix) {
		case 1:
		/* tcpConnState */
			return (asnMakeInteger ((CUnslType) info->tcpc_state));

		case 2:
		/* tcpConnLocalAddress */
			return (asnMakeIpAddress ((CBytePtrType)&info->tcpc_laddr));

		case 3:
		/* tcpConnLocalPort */
			return (asnMakeInteger ((CUnslType)info->tcpc_lport));

		case 4:
		/* tcpConnRemAddress */
			return (asnMakeIpAddress ((CBytePtrType)&info->tcpc_raddr));

		case 5:
		/* tcpConnRemPort */
			return (asnMakeInteger ((CUnslType)info->tcpc_rport));

		default:
			return ((AsnIdType) 0);
		}
	} else
		return ((AsnIdType) 0);
}

static	struct tcpcontab *	tcpConnGetEntry (number)

CUnslType		number;

{
	CUnslType	idx;
	CUnslType	tmpAddr;


	
	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_TCP_TCPCONNTABLE_TCPCONNENTRY, &tcpConnTable, number) > 0)
		return (&tcpConnTable);
	else return ((struct tcpcontab *) 0);
}

static	AsnIdType	tcpGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (tcpRetrieve (cookie, tcpGetEntry (name, namelen)));
}

static	AsnIdType	tcpNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	if (*namelenp == (MixLengthType) 0) {
		*namelenp = 1;
		name[0] = 0;
		return (tcpRetrieve (cookie, tcpGetEntry (name, *namelenp)));
	} else {
		return ((AsnIdType) 0);
	}
}

static	AsnIdType	tcpConnGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	CUnslType	number;

	number = NametoLong (name, namelen);
	return (tcpConnRetrieve (cookie, tcpConnGetEntry (number)));
}

static	AsnIdType	tcpConnNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	CUnslType number;

	if (*namelenp == (CUnslType) 0) {
		number = 1;
		name[0] = 1;
		*namelenp = 1;
	} else {
		number = NametoLong (name, *namelenp);
		number++;
		*namelenp = 0;
		LongtoName (number, name, namelenp);
	}

	return (tcpConnRetrieve (cookie, tcpConnGetEntry (number)));
}

static	MixOpsType	tcpOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			tcpNext,
			tcpGet,
			stubsSet

			};

static	MixOpsType	tcpConnOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			tcpConnNext,
			tcpConnGet,
			stubsSet

			};

CVoidType		tcpInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\1",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\2",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\3",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\4",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\5",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 5);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\6",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 6);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\7",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 7);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\10",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 8);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\11",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 9);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\12",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 10);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\13",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 11);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\14",
		(MixLengthType) 7, & tcpOps, (MixCookieType) 12);


	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\15\1\1",
		(MixLengthType) 9, & tcpConnOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\15\1\2",
		(MixLengthType) 9, & tcpConnOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\15\1\3",
		(MixLengthType) 9, & tcpConnOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\15\1\4",
		(MixLengthType) 9, & tcpConnOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\6\15\1\5",
		(MixLengthType) 9, & tcpConnOps, (MixCookieType) 5);

}
