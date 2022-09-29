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
#ident	"$Header: ring.c,v 1.2.1.2 90/05/10 02:19:36 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#include	"curses_inc.h"

void
_ring(bf)
bool	bf;
{
    extern	int	_outch();
    static	char	offsets[2] = {45 /* flash_screen */, 1 /* bell */ };
    char	**str_array = (char **) cur_strs;
#ifdef	DEBUG
    if (outf)
	fprintf(outf, "_ring().\n");
#endif	/* DEBUG */
    _PUTS(str_array[offsets[bf]] ? str_array[offsets[bf]] : str_array[offsets[1 - bf]], 0);
    (void) fflush(SP->term_file);
    if (_INPUTPENDING)
	(void) doupdate();
}
