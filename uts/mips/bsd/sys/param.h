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
/* $Header: param.h,v 1.7.1.2 90/05/10 04:36:59 wje Exp $ */

#ifndef	_BSD_SYS_PARAM_
#define	_BSD_SYS_PARAM_	1


/*
 * BSD compatibility header
 */

#ifdef KERNEL
#include "sys/types.h"
#else
#include <bsd/sys/types.h>
#endif
#include "../../sys/param.h"

#ifndef MIN
#define MIN(a,b) (((a)<(b))?(a):(b))
#endif MIN
#ifndef MAX
#define MAX(a,b) (((a)>(b))?(a):(b))
#endif MAX

#endif	_BSD_SYS_PARAM_
