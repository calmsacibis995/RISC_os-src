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
#ident	"$Header: bsdinit.c,v 1.4.4.2 90/05/10 04:20:28 wje Exp $"

/* Initialize the TCP stuff
 *
 * $Header: bsdinit.c,v 1.4.4.2 90/05/10 04:20:28 wje Exp $
 */

#include "../tcp-param.h"

/* initialize the network stuff.
 *	This function must be called after the drivers have 'attached' 
 *	themselves.  Therefore, it is not simply a module-initialization
 *	function that lboot knows about.
 *
 *	Notice that the MBUFs must be initialized before the drivers are
 *	attached, and so each driver should call the mbuf initialization
 *	function.
 */
bsd_init()
{
	register int s = splimp();

	ifinit();			/* generally initialize interfaces */
	domaininit();			/* initialize the domain tables */
	loattach();			/* start the 'loop back driver' */

	splx(s);
}
