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
#ident	"$Header: systm.c,v 1.4.1.2 90/05/09 17:32:52 wje Exp $"

#include	<sysv/sys/sysmips.h>
#include	<sysv/sys/limits.h>
#include	<sysv/sys/netman.h>
#include	<sysv/sys.s>

#include	<ctypes.h>
#include	<local.h>
#include	<debug.h>
#include	<miv.h>
#include	<asn.h>
#include	<mix.h>

#include	<systm.h>
#include	<stubs.h>


static struct sysinfo	systmTable;

static	AsnIdType	systmRetrieve (mix, info)

MixCookieType		mix;
struct sysinfo *	info;

{
	if (info != (struct sysinfo *) 0) {
		switch (mix) {
		case 1:
		/* SysDescr */
			return (asnMakeOctetString (info->sys_id, strlen (info->sys_id)));

		case 2:
		/* SysObjectID */
			return (asnMakeObjectId (info->sys_objid, info->sys_objidlen));

		case 3:
		/* SysUpTime */
			return (asnMakeTimeTicks (info->sys_lastinit));

		default:
			return ((AsnIdType) 0);
		}
		
	} else
		return ((AsnIdType) 0);
}

static struct sysinfo *	systmGetEntry (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{
	if (syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_SYSTEM, &systmTable, 0) > 0)
		return (&systmTable);
	else return ((struct sysinfo *) 0);
}


static	AsnIdType	systmGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	if ((namelen != (MixLengthType) 1) ||
		(*name != (MixNameType) 0)) {
		return ((AsnIdType) 0);
	}

	return (systmRetrieve (cookie, systmGetEntry (name, namelen)));
}

static	AsnIdType	systmNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{

	if (*namelenp == (MixLengthType) 0) {
		*namelenp = (MixLengthType) 1;
		*name = (MixNameType) 0;
		return (systmRetrieve (cookie, systmGetEntry (name, *namelenp)));
	} else {
		return ((AsnIdType) 0);
	}
}

static	MixOpsType	systmOps	= {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			systmNext,
			systmGet,
			stubsSet

			};

CVoidType		systmInit ()

{
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\1\1",
		(MixLengthType) 7, & systmOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\1\2",
		(MixLengthType) 7, & systmOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\1\3",
		(MixLengthType) 7, & systmOps, (MixCookieType) 3);
}

