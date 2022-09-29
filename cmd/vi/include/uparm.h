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
/* $Header: uparm.h,v 1.6.2.2 90/05/10 00:54:31 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Local configuration of various files.  Used if you can't put these
 * things in the standard places or aren't the super user, so you
 * don't have to modify the source files.  Thus, you can install updates
 * without having to re-localize your sources.
 */

/* Path to library files */
#define libpath(file) "/usr/lib/file"

/* Path to local library files */
#define loclibpath(file) "/usr/lib/file"

/* Path to binaries */
#define binpath(file) "/usr/bin/file"

/* Path to things under /usr (e.g. /usr/preserve) */
#define usrpath(file) "/usr/file"

/* Location of terminfo binary directory tree */
#define termpath(file)	"/usr/lib/terminfo/file"

#define TMPDIR	"/tmp"
