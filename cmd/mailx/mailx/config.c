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
#ident	"$Header: config.c,v 1.4.1.2 90/05/09 16:38:56 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#

/*
 * mailx -- a modified version of a University of California at Berkeley
 *	mail program
 */


/*
 * This file contains definitions of network data used by mailx
 * when replying.  See also:  configdefs.h and optim.c
 */

/*
 * The subterfuge with CONFIGFILE is to keep cc from seeing the
 * external defintions in configdefs.h.
 */
#define	CONFIGFILE
#include "configdefs.h"

/*
 * Set of network separator characters.
 */
char	*metanet = "!^:%@.";

/*
 * Host table of "known" hosts.  See the comment in configdefs.h;
 * not all accessible hosts need be here (fortunately).
 */
struct netmach netmach[] = {
	EMPTY,		EMPTYID,	BN|AN,	/* Filled in dynamically */
	EMPTY,		EMPTYID,	BN|AN,	/* Filled in dynamically */
	0,		0,		0
};

/*
 * Table of ordered of preferred networks.  You probably won't need
 * to fuss with this unless you add a new network character (foolishly).
 */
struct netorder netorder[] = {
	AN,	'@',
	AN,	'%',
	SN,	':',
	BN,	'!',
	-1,	0
};

/*
 * Table to convert from network separator code in address to network
 * bit map kind.  With this transformation, we can deal with more than
 * one character having the same meaning easily.
 */
struct ntypetab ntypetab[] = {
	'%',	AN,
	'@',	AN,
	':',	SN,
	'!',	BN,
	'^',	BN,
	0,	0
};

struct nkindtab nkindtab[] = {
	AN,	IMPLICIT,
	BN,	EXPLICIT,
	SN,	IMPLICIT,
	0,	0
};
