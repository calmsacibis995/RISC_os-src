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
#ident	"$Header: at.c,v 1.5.1.2 90/05/09 17:26:29 wje Exp $"

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

#include	<at.h>
#include	<stubs.h>


static struct atentry	atEntry;

static	AsnIdType	atRetrieve (mix, info)

MixCookieType		mix;
struct atentry *	info;

{
	if (info != (struct atentry *) 0) {
		switch (mix) {

		case 1:
		/* atIfIndex */
			return (asnMakeInteger (info->at_index));

		case 2:
		/* atPhysAddress */
			return (asnMakeOctetString (info->at_physaddr, info->at_physaddr_size));
		case 3:
		/* atNetAddress */
			return (asnMakeIpAddress (&info->at_netaddr));
		}
	} else
		return ((AsnIdType) 0);
}

static	CUnsfType	atnametoaddr (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{

	int	idx;
	int	tmpaddr, tmpbyte;

	if (*name == (MixNameType) 1) {	/* We only know about internet addresses. */
		name++;
		namelen--;
	} else return ((CUnsfType) 0);


	return (NametoIpAddr (name, namelen));
}

static	struct atentry *atGetEntry (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{
	CUnsfType		entaddr;
	int			i;


	entaddr = atnametoaddr(name, namelen);

	i = syscall (SYS_sysmips, MIPS_GET_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_AT_ATTABLE_ATENTRY, &atEntry, entaddr);

	if ((i != 0) && (i != -1))
		return (&atEntry);
	else return ((struct atentry *) 0);
}

static	struct atentry *	atGetNextEntry (name, namelenp)

MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	CUnsfType	entaddr;
	int		i;

	if (*namelenp != 0)
		entaddr = atnametoaddr(name, *namelenp);
	else entaddr = 0;

	i = syscall (SYS_sysmips, MIPS_GET_NEXT_MIB, ISO_ORG_DOD_INTERNET_MGMT_MIB_AT_ATTABLE_ATENTRY, &atEntry, entaddr);

	if (i == 0) {
		return ((struct atentry *) 0);
	}

	*namelenp = 1;
	*name = 1;
	name++;
	IpAddrtoName (atEntry.at_netaddr, name, namelenp);

	return (&atEntry);

}

static	AsnIdType	atGet (cookie, name, namelen)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthType		namelen;

{
	return (atRetrieve (cookie, atGetEntry (name, namelen)));
}

static	AsnIdType	atNext (cookie, name, namelenp)

MixCookieType		cookie;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	return (atRetrieve (cookie, atGetNextEntry (name, namelenp)));
}

static	MixOpsType	atOps = {

			stubsRelease,
			stubsCreate,
			stubsDestroy,
			atNext,
			atGet,
			stubsSet

			};

CVoidType		atInit ()

{

	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\3\1\1\1",
		(MixLengthType) 9, & atOps, (MixCookieType) 1);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\3\1\1\2",
		(MixLengthType) 9, & atOps, (MixCookieType) 2);
	(void) misExport ((MixNamePtrType) "\53\6\1\2\1\3\1\1\3",
		(MixLengthType) 9, & atOps, (MixCookieType) 3);

}
