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
#ident	"$Header: gf.c,v 1.6.2.2 90/05/09 18:39:33 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/* EMACS_MODES: c !fill tabstop=4 */

/*
 *	gf -- Get a .FRED file name for a particular application and subsystem.
 *
 *	The resulting pathname is placed in a static area that is overwritten
 *	by each call to gf ().
 *
 */

#include <stdio.h>
#include "filehand.h"

/* Debugging options */

#ifdef TRACE
#define TR(W,X,Y,Z) fprintf (stdout, W, X, Y, Z)
#else
#define TR(W,X,Y,Z) /* W X Y Z */
#endif

#define SIZE 132

char *gf (appl)
char	*appl;
{
	static char	filename[SIZE];
	char		inline[SIZE], *ptrs[3], *fmat[2], *tmp;
	extern char	*strrchr ();
	extern int	sweep ();

	TR("Gf: entry appl=(%s)\n", appl, EMPTY, EMPTY);
	cat (filename, "/usr/lib/M2/", appl, EMPTY);
	fmat[0] = "DBBD";
	fmat[1] = EMPTY;
	TR("Gf: fmat[0]=(%s) fmat[1]=(%s)\n", fmat[0], fmat[1], EMPTY);
	if (sweep (VERIFY, filename, EMPTY, '\n', ':', SIZE, fmat, inline, ptrs,
	  (int (*)()) NULL, (int (*)()) NULL) != FOUND) {
		TR("Gf: not found\n", EMPTY, EMPTY, EMPTY);
		return (EMPTY);
		}
	tmp = strrchr (ptrs[1], (char) 01);
	*tmp = NULL;						/* Find and clobber control A. */
	cat (filename, ptrs[1], "/.fred/.FRED", EMPTY);
	TR("Gf: returns (%s)\n", filename, EMPTY, EMPTY);
	return (filename);
}

