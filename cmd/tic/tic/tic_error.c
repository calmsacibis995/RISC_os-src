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
#ident	"$Header: tic_error.c,v 1.2.1.2 90/05/09 19:40:56 wje Exp $"
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*********************************************************************
*                         COPYRIGHT NOTICE                           *
**********************************************************************
*        This software is copyright (C) 1982 by Pavel Curtis         *
*                                                                    *
*        Permission is granted to reproduce and distribute           *
*        this file by any means so long as no fee is charged         *
*        above a nominal handling fee and so long as this            *
*        notice is always included in the copies.                    *
*                                                                    *
*        Other rights are reserved except as explicitly granted      *
*        by written permission of the author.                        *
*                Pavel Curtis                                        *
*                Computer Science Dept.                              *
*                405 Upson Hall                                      *
*                Cornell University                                  *
*                Ithaca, NY 14853                                    *
*                                                                    *
*                Ph- (607) 256-4934                                  *
*                                                                    *
*                Pavel.Cornell@Udel-Relay   (ARPAnet)                *
*                decvax!cornell!pavel       (UUCPnet)                *
*********************************************************************/

/*
 *	tic_error.c -- Error message routines
 *
 *  $Log:	tic_error.c,v $
 * Revision 1.2.1.2  90/05/09  19:40:56  wje
 * add restricted rights legend
 * 
 * Revision 1.2.1.1  89/11/27  04:41:47  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.2  89/09/08  09:52:21  vicki
 * Removed ident string.
 * 
 * Revision 1.1  89/09/08  09:48:26  vicki
 * Initial revision
 * 
 * Revision 1.1  89/09/07  13:01:15  vicki
 * Initial revision
 * 
 * Revision 2.1  82/10/25  14:45:31  pavel
 * Added Copyright Notice
 * 
 * Revision 2.0  82/10/24  15:16:32  pavel
 * Beta-one Test Release
 * 
 * Revision 1.3  82/08/23  22:29:31  pavel
 * The REAL Alpha-one Release Version
 * 
 * Revision 1.2  82/08/19  19:09:44  pavel
 * Alpha Test Release One
 * 
 * Revision 1.1  82/08/12  18:36:02  pavel
 * Initial revision
 * 
 *
 */

#include "compiler.h"
#include <varargs.h>

extern char *string_table;
extern short term_names;
extern char *progname;

/* VARARGS1 */
warning(va_alist)
va_dcl
{
    va_list args;
    char *fmt;

    va_start(args);
    fmt = va_arg(args, char *);
    fprintf (stderr, "%s: Warning: near line %d: ", progname, curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
    va_end(args);
}


/* VARARGS1 */
err_abort(va_alist)
va_dcl
{
    va_list args;
    char *fmt;
    
    va_start(args);
    fprintf (stderr, "%s: Line %d: ", progname, curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
    va_end(args);
    exit(1);
}


/* VARARGS1 */
syserr_abort(va_alist)
va_dcl
{
    va_list args;
    char *fmt;
    
    va_start(args);
    fprintf (stderr, "PROGRAM ERROR: Line %d: ", curr_line);
    fprintf (stderr, "terminal '%s', ", string_table+term_names);
    vfprintf (stderr, fmt, args);
    fprintf (stderr, "\n");
	fprintf(stderr,"*** Possibly corrupted terminfo file ***\n");
    va_end(args);
    exit(1);
}
