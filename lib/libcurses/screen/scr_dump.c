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
#ident	"$Header: scr_dump.c,v 1.2.1.2 90/05/10 02:20:32 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#include	"curses_inc.h"

/*
 * Dump a screen image to a file. This routine and scr_reset
 * can be used to communicate the screen image across processes.
 */

scr_dump(file)
char	*file;
{
    int		rv;
    FILE	*filep;

    if ((filep = fopen(file,"w")) == NULL)
    {
#ifdef	DEBUG
	if (outf)
	    (void) fprintf (outf, "scr_dump: cannot open \"%s\".\n", file);
#endif	/* DEBUG */
	return (ERR);
    }
    rv = scr_ll_dump(filep);
    fclose(filep);
    return (rv);
}
