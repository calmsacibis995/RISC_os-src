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
#ident	"$Header: du_init_i2000.c,v 1.2.3.2 90/05/10 05:18:00 wje Exp $"
/*
 * $Header
 */
/*
 * du_init for i2000 so we don't need stuff in startup.c
 * Project: Jupiter Workstation
 * Date: 09-Feb-1988
 */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"

du_init()
{
	showconfig = 1;	/* My preference during debug */
	iop_initialize();
	stopclocks();
	early_uartinit();
	grafprobe();
} /* du_init */
