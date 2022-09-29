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
/* $Header: less.h,v 1.4.2.2 90/05/09 17:57:09 wje Exp $ */

/*
 * Standard include file for "less".
 */

/*
 * Language details.
 */
#define	public		/* PUBLIC FUNCTION */

/*
 * Special types and constants.
 */
typedef long		POSITION;
/*
 * {{ Warning: if POSITION is changed to other than "long",
 *    you may have to change some of the printfs which use "%ld"
 *    to print a variable of type POSITION. }}
 */

#define	NULL_POSITION	((POSITION)(-1))

#define	FILENAME	128	/* Max size of a filename */

#define	EOF		(0)

/* How quiet should we be? */
#define	NOT_QUIET	0	/* Ring bell at eof and for errors */
#define	LITTLE_QUIET	1	/* Ring bell only for errors */
#define	VERY_QUIET	2	/* Never ring bell */

/* How should we prompt? */
#define	PR_SHORT	0	/* Prompt with colon */
#define	PR_MEDIUM	1	/* Prompt with message */
#define	PR_LONG		2	/* Prompt with longer message */

/* How should we handle backspaces? */
#define	BS_SPECIAL	0	/* Do special things for underlining and bold */
#define	BS_NORMAL	1	/* \b treated as normal char; actually output */
#define	BS_CONTROL	2	/* \b treated as control char; prints as ^H */

/* Special chars used to tell put_line() to do something special */
#define	UL_CHAR		'\201'	/* Enter underline mode */
#define	UE_CHAR		'\202'	/* Exit underline mode */
#define	BO_CHAR		'\203'	/* Enter boldface mode */
#define	BE_CHAR		'\204'	/* Exit boldface mode */

#define	CONTROL(c)		((c)&037)
#define	SIGNAL(sig,func)	signal(sig,func)

#include <sys/types.h>
#include <sys/param.h>

#ifdef NULL
#undef NULL
#define	NULL		(0)
#endif

off_t lseek();

#define REGCMP		1
#define RECOMP		0

#define SHELL_ESCAPE	1

#define EDITOR 		1
#ifndef EDIT_PGM
#define EDIT_PGM	"/usr/ucb/vi"
#endif

#define ONLY_RETURN	0

#ifndef HELPFILE
#define HELPFILE	"/usr/new/lib/less.help"
#endif

#define GLOB		1
#define STAT		1
#define PERROR		1

#define LOGFILE		1

#define TERMIO		1

#include "funcs.h"
