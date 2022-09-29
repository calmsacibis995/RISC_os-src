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
/* $Header: ctype.h,v 1.3.1.3 90/05/10 04:06:53 wje Exp $ */

#ifndef	_POSIX_CTYPE_
#define	_POSIX_CTYPE_	1

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#define	_U	01	/* Upper case */
#define	_L	02	/* Lower case */
#define	_N	04	/* Numeral (digit) */
#define	_S	010	/* Spacing character */
#define	_P	020	/* Punctuation */
#define	_C	040	/* Control character */
#define	_B	0100	/* Blank */
#define	_X	0200	/* heXadecimal digit */

extern char	_ctype[];
extern int	isalpha();
extern int	isupper();
extern int	islower();
extern int	isdigit();
extern int	isxdigit();
extern int	isalnum();
extern int	isspace();
extern int	ispunct();
extern int	isprint();
extern int	isgraph();
extern int	iscntrl();
extern int	isascii();
extern int	_toupper();
extern int	_tolower();
extern int	toascii();

#define	_toupper(c)     ((_ctype + 258)[c])
#define	_tolower(c)	((_ctype + 258)[c])

#endif	_POSIX_CTYPE_
