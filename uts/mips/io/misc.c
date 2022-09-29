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
#ident	"$Header: misc.c,v 1.2.3.2 90/05/10 05:22:57 wje Exp $"
#include "sys/types.h"
#include "sys/dvh.h"

#define	TRUE	(1==1)
#define	FALSE	(1==0)
/*
 * check for valid volume header
 */
is_vh(vhp)
register struct volume_header *vhp;
{
	register csum;
	register int *ip;

	if (vhp->vh_magic != VHMAGIC)
		return (FALSE);

	csum = 0;
	for (ip = (int *)vhp; ip < (int *)(vhp + 1); ip++)
		csum += *ip;
	if (csum == 0)
		return (TRUE);
	else
		return (FALSE);
}
