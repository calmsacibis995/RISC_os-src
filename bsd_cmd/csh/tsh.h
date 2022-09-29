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
/* $Header: tsh.h,v 1.1.2.2 90/05/07 18:18:29 wje Exp $ */


/*
 * tsh.h:
 * Include file for tsh extensions to csh
 */



#define		CNTL(c)		(c&037)

#define		NOCHAR		-1

#define		TRUE		1
#define		FALSE		0

#define		EOF_CHAR	CNTL('d')	/* char for EOF if bol */
#define		QUOTE_CHAR	CNTL('q')	/* char for quoting things */
#define		ABORT_CHAR	CNTL('g')	/* char for aborting command */
#define		COLON_CHAR	':'		/* modify hist expansion */
#define		PREFIX_CHAR	CNTL('[')	/* previx char for commands */
#define		PREFIX_PRINT	'$'		/* printing form of above */

#define		BACKSLASH	'\\'		/* prevents shell expansion */
#define		CONTROL		'^'		/* prefix for control chars */
#define		CONTROL_ADD	('A'-1)		/* add to control to print */
#define		DEL_CHAR	'?'		/* what to print for delete */
#define		INDIRECT	'>'		/* I/O redirection for >! */

#define		MAX_LINE_LEN	80		/* screen width */



/*
 * stuff for error bells, crt cursor manipulation
 */

extern char *crt_chars[];
#define DOWN (crt_chars[0])
#define BACKWARD (crt_chars[1])
#define LEFT_MARGIN (crt_chars[2])
#define RING (crt_chars[3])
#define BLANK (crt_chars[4])

#define		BELL()		write(tsh_out, RING, strlen(RING));


/*
 * char handling stuff ripped off from ctype.h and changed slightly
 */

#define	_U	01
#define	_L	02
#define	_N	04
#define	_S	010
#define _P	020
#define _C	040
#define _X	0100
#define _V	0200

extern short *_cshctype_;

#define	isalpha(c)	((_cshctype_)[c]&(_U|_L))
#define	isupper(c)	((_cshctype_)[c]&_U)
#define	islower(c)	((_cshctype_)[c]&_L)
#define	isdigit(c)	((_cshctype_)[c]&_N)
#define	isxdigit(c)	((_cshctype_)[c]&(_N|_X))
#define	isaspace(c)	((_cshctype_)[c]&_S)
#define ispunct(c)	((_cshctype_)[c]&_P)
#define isalnum(c)	((_cshctype_)[c]&(_U|_L|_N|_V))
#define isprint(c)	((_cshctype_)[c]&(_P|_U|_L|_N))
#define iscntrl(c)	((_cshctype_)[c]&_C)
#define isascii(c)	((unsigned)(c)<=0177)
#define toupper(c)	((c)-'a'+'A')
#define tolower(c)	((c)-'A'+'a')
#define toascii(c)	((c)&0177)
