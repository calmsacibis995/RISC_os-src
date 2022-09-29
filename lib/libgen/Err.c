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
#ident	"$Header: Err.c,v 1.5.2.2 90/05/10 02:33:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	Set default values in Err global structure.
*/

#include	"errmsg.h"

static	char	deftofix[] = "Refer to help error database or manual.";
static	char	*defsevmsg[] = {	/* default severity messages */
		"INFORM: ",
		"WARNING: ",
		"ERROR: ",
		"HALT: ",
		0
	};

struct Err	Err = {
					/* verbosity flags */
		/* vbell */	ENO,
		/* vprefix */	EYES,
		/* vsource */	EYES,
		/* vsevmsg */	EYES,
		/* vsyserr */	EDEF,
		/* vfix */	EYES,
		/* vtag */	EYES,
		/* vtext */	EYES,
					/* message content */
		/* prefix */	0,
		/* envsource */	0,
		/* source */	0,
		/* severity */	0,
		/* sevmsg */	defsevmsg,
		/* tofix */	deftofix,
		/* tagnum */	0,
		/* tagstr */	0,
		/* exit */	1,
};

