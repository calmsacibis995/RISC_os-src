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
#ident	"$Header: dns_sethent.c,v 1.2.1.2 90/05/07 20:50:14 wje Exp $"

#ifdef SYSTYPE_BSD43
#include <sys/types.h>
#include <arpa/nameser.h>
#include <netinet/in.h>
#include <resolv.h>
#else
#ifdef SYSTYPE_SYSV
#include <bsd/sys/types.h>
#include <bsd/arpa/nameser.h>
#include <bsd/netinet/in.h>
#include <bsd/resolv.h>
#endif
#endif

dns_sethostent(stayopen)
{
	if (stayopen)
		_res.options |= RES_STAYOPEN | RES_USEVC;
}

dns_endhostent(a)
	int a;
{
	_res.options &= ~(RES_STAYOPEN | RES_USEVC);
	_res_close();
}
