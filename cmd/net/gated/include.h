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
/* $Header: include.h,v 1.2.1.2 90/05/09 17:00:26 wje Exp $ */
/*
 *   CENTER FOR THEORY AND SIMULATION IN SCIENCE AND ENGINEERING
 *			CORNELL UNIVERSITY
 *
 *      Portions of this software may fall under the following
 *      copyrights: 
 *
 *	Copyright (c) 1983 Regents of the University of California.
 *	All rights reserved.  The Berkeley software License Agreement
 *	specifies the terms and conditions for redistribution.
 *
 *  GATED - based on Kirton's EGP, UC Berkeley's routing daemon (routed),
 *	    and DCN's HELLO routing Protocol.
 *
 * $Header: include.h,v 1.2.1.2 90/05/09 17:00:26 wje Exp $
 *
 */

/* include.h
 *
 * System and EGP header files to be included.
 */

#ifdef	vax11c
#include "config.h"
#endif	vax11c
#include <sys/types.h>
#include <sys/mbuf.h>
#include <sys/time.h>
#ifdef	vax11c
#include <sys/ttychars.h>
#include <sys/ttydev.h>
#endif	vax11c
#include <sys/ioctl.h>
#ifndef vax11c
#include <sys/uio.h>
#endif	vax11c

#include <sys/socket.h>
#ifndef vax11c
#include <sys/file.h>
#endif	vax11c

#include <netinet/in.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/ip_icmp.h>

#include <stdio.h>
#include <ctype.h>
#include <netdb.h>
#include <errno.h>
#include <signal.h>
#include <strings.h>

#ifdef vax11c
#define DONT_INCLUDE_IF_ARP
#endif vax11c
#include <net/if.h>
#include <net/route.h>
#include <syslog.h>

#include "routed.h"
#include "defs.h"
#include "egp.h"
#include "egp_param.h"
#include "if.h"
#include "rt_table.h"
#include "rt_control.h"
#include "af.h"
#include "trace.h"
#ifndef	NSS
#include "rip.h"
#include "hello.h"
#endif	NSS
