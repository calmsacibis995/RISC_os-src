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
#ident	"$Header: uipc_pipe.c,v 1.2.1.2 90/05/10 04:34:50 wje Exp $"
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 * @(#)uipc_pipe.c 2.1 88/05/18 NFSSRC4.0 SMI
 * @(#)uipc_pipe.c 4.18 82/10/31 UCB
 */

/* originally included  param.h mbuf.h protosw.h socket.h socketvar.h unpcb.h
 */

#include "sys/types.h"
#include "sys/param.h"
#include "sys/mbuf.h"
#include "sys/protosw.h"
#include "sys/socket.h"
#include "sys/socketvar.h"
#include "unpcb.h"

#define	PIPSIZ	4096

/*
 * Sneakily connect a pipe from wso to rso.
 * This will get cleaned up when socketpair is added.
 */
piconnect(wso, rso)
	struct socket *wso, *rso;
{

	/* when we reserve memory this routine may fail */
	sotounpcb(wso)->unp_conn = sotounpcb(rso);
	sotounpcb(rso)->unp_conn = sotounpcb(wso);
	wso->so_snd.sb_hiwat = PIPSIZ;
	wso->so_snd.sb_mbmax = 2*PIPSIZ;
	wso->so_state |= SS_ISCONNECTED|SS_CANTRCVMORE;
	rso->so_rcv.sb_hiwat = 0;
	rso->so_rcv.sb_mbmax = 0;
	rso->so_state |= SS_ISCONNECTED|SS_CANTSENDMORE;
	return (1);
}
