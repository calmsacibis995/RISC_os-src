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
#ident	"$Header: tic_captab.c,v 1.2.1.2 90/05/09 19:40:50 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	comp_captab.c -- The names of the capabilities in a form ready for
 *		         the making of a hash table for the compiler.
 *
 */


#include "curses_inc.h"
#include "compiler.h"


struct name_table_entry	cap_table[512];

struct name_table_entry *cap_hash_table[360];

int	Hashtabsize = 360;
int	Captabsize = 0;
int	BoolCount = 0;
int	NumCount = 0;
int	StrCount = 0;
