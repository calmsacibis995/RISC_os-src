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
#ident	"$Header: utils.c,v 1.1.1.2 90/05/09 17:34:06 wje Exp $"

#include	<ctypes.h>
#include	<error.h>
#include	<debug.h>
#include	<local.h>
#include	<mix.h>
#include	<mis.h>
#include	<avl.h>
#include	<asn.h>

CUnsfType	NametoIpAddr (name, namelen)

MixNamePtrType	name;
MixLengthType	namelen;

{

	int	idx;
	int	tmpaddr, tmpbyte;

	tmpaddr = 0;
	for (idx = 0; idx < 4; idx++) {
		if (*name == (MixNameType) 129) {
			tmpbyte = 128;
			name++;
			namelen--;
		} else tmpbyte = 0;
		tmpbyte += (*name & 0x7F);
		name++;
		namelen--;
		tmpaddr = (tmpaddr << 8) + tmpbyte;
	}

	if (namelen != 0)
		return ((CUnsfType) 0);

	else return ((CUnsfType) tmpaddr);
}

void IpAddrtoName (addr, name, namelenp)

CUnsfType	addr;
MixNamePtrType	name;
MixLengthType	*namelenp;

{
char *tmpaddr;
int idx;

	tmpaddr = (char *)&addr;
		
	for (idx = 0; idx < 4; idx++) {
		if ((*tmpaddr & 0x80) != 0) {
			*name = 129;
			name++;
			*namelenp = *namelenp + 1;
		}
		*name = *tmpaddr & 0x7F;
		tmpaddr++;
		name++;
		*namelenp = *namelenp + 1;
	}
	
}

CUnslType		NametoLong (name, namelen)

MixNamePtrType		name;
MixLengthType		namelen;

{
	CUnslType		count;

	count = 0;

	while (namelen > 0) {
		if (*name & 128) {
			count = (count << 7) + (*name & 127);
			name++;
			namelen  =  namelen - 1;
		} else {
			return ((count << 7) + (*name & 127));
		}
	}

	return (count);
}

void			LongtoName (number, name, namelenp)

CUnslType		number;
MixNamePtrType		name;
MixLengthPtrType	namelenp;

{
	CUnslType	total, tmp, idx;

	*namelenp = 0;
	tmp = ((number >> 28) & 127);
	if (tmp) {
		*name = 128 | tmp;
		name++;
		*namelenp = *namelenp + 1;
	}
	
	tmp = ((number >> 21) & 127);
	if (tmp) {
		*name = 128 | tmp;
		name++;
		*namelenp = *namelenp + 1;
	}
	
	tmp = ((number >> 14) & 127);
	if (tmp) {
		*name = 128 | tmp;
		name++;
		*namelenp = *namelenp + 1;
	}
	
	tmp = ((number >> 7) & 127);
	if (tmp) {
		*name = 128 | tmp;
		name++;
		*namelenp = *namelenp + 1;
	}
	
	tmp = (number & 127);
	*name = tmp;
	*namelenp = *namelenp + 1;
	
	return;
}
