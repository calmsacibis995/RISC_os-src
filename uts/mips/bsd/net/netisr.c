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
#ident	"$Header: netisr.c,v 1.6.4.2 90/05/10 10:49:41 wje Exp $"

/*
 * Netisr simulator for non vaxen
 */
#ifdef mips
#include "../tcp-param.h"
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/mbuf.h"
#else
#include "../h/param.h"
#include "../h/mbuf.h"
#endif
#include "netisr.h"

#ifndef mips
char	netisrflag;

setsoftnet()
{
	int s;

	s = splmax();
	if (netisrflag == 0)
		netisrflag = 1;
	splx(s);
}
#endif

#ifdef mips
netintr()
#else
service_net()
#endif
{
	int s;

#ifdef mips
	acksoftnet();
#endif
	if (netisr) {
		s = splnet();
#ifdef	IMP
		if (netisr & SCHEDBIT(NETISR_IMP)) {
			netisr &= ~NETISR_IMP;
			impintr();
		}
#endif
#ifdef	INET
		if (netisr & SCHEDBIT(NETISR_IP)) {
			netisr &= ~NETISR_IP;
			ip_intr();
		}
#endif
#ifdef	NS
		if (netisr & SCHEDBIT(NETISR_NS)) {
			netisr &= ~NETISR_NS;
			nsintr();
		}
#endif
		if (netisr & SCHEDBIT(NETISR_RAW)) {
			netisr &= ~NETISR_RAW;
			rawintr();
		}
		splx(s);
	}
}
