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
#ident	"$Header: icmp.c,v 1.4.1.2 90/05/09 17:28:15 wje Exp $"

#include	<sysv/sys/sysmips.h>
#include	<sysv/sys/limits.h>
#include	<sysv/sys/netman.h>
#include	<sysv/sys.s>

#include	<ctypes.h>
#include	<error.h>
#include	<local.h>
#include	<icmp.h>
#include	<mix.h>
#include	<mis.h>
#include	<asn.h>
#include	<stubs.h>

static	CUnslType		icmpAddr;
static	struct icmpinfo		icmpTable;

static struct icmpinfo *	icmpGetEntry (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{
	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_ICMP, &icmpTable, 0) > 0)
		return (&icmpTable);
	else return ((struct icmpinfo *) 0);
}

static	AsnIdType	icmpRetrieve (mix, info)

MixCookieType		mix;
struct icmpinfo *	info;

{
	if (info != (struct icmpinfo *) 0) {
		switch (mix) {
		case 1:
		/* icmpInMsgs */
			return (asnMakeCounter (info->icmp_rpkt));

		case 2:
		/* icmpInErrors */
			return (asnMakeCounter (info->icmp_rerr));

		case 3:
		/* icmpInDestUnreachs */
			return (asnMakeCounter (info->icmp_rdest_unreach));

		case 4:
		/* icmpInTimeExcds */
			return (asnMakeCounter (info->icmp_rtime_exceed));

		case 5:
		/* icmpInParmProbs */
			return (asnMakeCounter (info->icmp_rparm_prob));

		case 6:
		/* icmpInSrcQuenchs */
			return (asnMakeCounter (info->icmp_rsrc_quench));

		case 7:
		/* icmpInRedirects */
			return (asnMakeCounter (info->icmp_rredirect));

		case 8:
		/* icmpInEchos */
			return (asnMakeCounter (info->icmp_recho));

		case 9:
		/* icmpInEchoReps */
			return (asnMakeCounter (info->icmp_recho_rep));

		case 10:
		/* icmpInTimestamps */
			return (asnMakeCounter (info->icmp_rtimes));

		case 11:
		/* icmpInTimestampReps */
			return (asnMakeCounter (info->icmp_rtimes_rep));

		case 12:
		/* icmpInAddrMasks */
			return (asnMakeCounter (info->icmp_raddrmask));

		case 13:
		/* icmpInaddrMaskReps */
			return (asnMakeCounter (info->icmp_raddrmask_rep));

		case 14:
		/* icmpOutMsgs */
			return (asnMakeCounter (info->icmp_xpkt));

		case 15:
		/* icmpOutErrors */
			return (asnMakeCounter (info->icmp_xerr));

		case 16:
		/* icmpOutDestUnreachs */
			return (asnMakeCounter (info->icmp_xdest_unreach));

		case 17:
		/* icmpOutTimeExcds */
			return (asnMakeCounter (info->icmp_xtime_exceed));

		case 18:
		/* icmpOutParmProbs */
			return (asnMakeCounter (info->icmp_xparm_prob));

		case 19:
		/* icmpOutSrcQuenchs */
			return (asnMakeCounter (info->icmp_xsrc_quench));

		case 20:
		/* icmpOutRedirects */
			return (asnMakeCounter (info->icmp_xredirect));

		case 21:
		/* icmpOutEchos */
			return (asnMakeCounter (info->icmp_xecho));

		case 22:
		/* icmpOutEchoReps */
			return (asnMakeCounter (info->icmp_xecho_rep));

		case 23:
		/* icmpOutTimestamps */
			return (asnMakeCounter (info->icmp_xtimes));

		case 24:
		/* icmpOutTimestampReps */
			return (asnMakeCounter (info->icmp_xtimes_rep));

		case 25:
		/* icmpOutAddrMasks */
			return (asnMakeCounter (info->icmp_xaddrmask));

		case 26:
		/* icmpOutAddrMaskReps */
			return (asnMakeCounter (info->icmp_xaddrmask_rep));

		default:
			return ((AsnIdType) 0);
		}
	} else
		return ((AsnIdType) 0);
}

static	AsnIdType	icmpGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	CIntfType		item;

	return (icmpRetrieve (cookie, icmpGetEntry (name, namelen)));
}

static	AsnIdType	icmpNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	if (*namelenp == (MixLengthType) 0) {
		*namelenp = (MixLengthType) 1;
		*name = (MixNameType) 0;
		return (icmpRetrieve (cookie, icmpGetEntry (name, *namelenp)));
	} else {
		return ((AsnIdType) 0);
	}
}

static	MixOpsType	icmpOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			icmpNext,
			icmpGet,
			stubsSet

			};

CVoidType		icmpInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\1",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\2",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\3",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 3);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\4",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 4);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\5",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 5);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\6",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 6);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\7",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 7);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\10",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 8);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\11",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 9);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\12",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 10);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\13",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 11);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\14",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 12);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\15",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 13);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\16",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 14);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\17",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 15);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\20",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 16);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\21",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 17);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\22",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 18);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\23",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 19);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\24",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 20);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\25",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 21);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\26",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 22);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\27",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 23);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\30",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 24);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\31",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 25);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\5\32",
		(MixLengthType) 7, & icmpOps, (MixCookieType) 26);
}

