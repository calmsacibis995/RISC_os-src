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
/* $Header: ioctl.h,v 1.8.4.2 90/05/10 04:36:40 wje Exp $ */

#ifndef	_BSD_SYS_IOCTL_
#define	_BSD_SYS_IOCTL_

/* 4.3 compatibility hack
 */

#ifdef KERNEL
#ifdef mips
#include "../bsd/sys/ttychars.h"
#include "../net/soioctl.h"
#else /* mips */
#include "sys/ttychars.h"
#include "net/soioctl.h"
#endif /* mips */

#else /* kernel */
#include <bsd/sys/ttychars.h>
#include <bsd/net/soioctl.h>
#endif

/* 4.2-like IOCTLs defined in termio.h */
#ifndef TIOCFLUSH
#define TIOCFLUSH TCFLSH
#endif

#endif	_BSD_SYS_IOCTL_
