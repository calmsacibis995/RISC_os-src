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
/* $Header: signal.h,v 1.5.3.2 90/05/10 04:37:25 wje Exp $ */

#ifndef	_BSD_SYS_SIGNAL_
#define	_BSD_SYS_SIGNAL_	1


/* BSD compatibility header
 */


/* get the 'real' signal.h */
#include "../../sys/signal.h"

#ifndef SIGCHLD
#define SIGCHLD SIGCLD
#endif 

#endif	_BSD_SYS_SIGNAL_
