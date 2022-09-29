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
/* $Header: spray.h,v 1.2.1.2 90/05/09 14:48:35 wje Exp $ */

/*	@(#)spray.h 1.1 86/09/25 SMI */

/*
 * Copyright (c) 1985 by Sun Microsystems, Inc.
 */

#define SPRAYPROG 100012
#define SPRAYPROC_SPRAY 1
#define SPRAYPROC_GET 2
#define SPRAYPROC_CLEAR 3
#define SPRAYVERS_ORIG 1
#define SPRAYVERS 1

#define SPRAYOVERHEAD 86	/* size of rpc packet when size=0 */
#define SPRAYMAX 8845		/* related to max udp packet of 9000 */

int xdr_sprayarr();
int xdr_spraycumul();

struct spraycumul {
	unsigned counter;
	struct timeval clock;
};

struct sprayarr {
	int *data;
	int lnth;
};
