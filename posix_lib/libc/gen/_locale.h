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
/* $Header: _locale.h,v 1.1.1.2 90/05/10 04:12:12 wje Exp $ */

#define LOCALE_DIR	"/posix/lib/locale/"
#define LEN_LC_ROOT	18
#define DFL_LANG	"C"

#define	LC_CATEGORIES	6	/* number of categories for setlocale */
#define	LC_NAMELEN	16	/* max. length of category and locale names */
#define SZ_CTYPE	514 	/* (257 + 257) upper and lower */
#define SZ_NUMERIC	2
#define DECIMAL_POINT	0	/* index into _numeric[] */
#define THOUSANDS_SEP	1	/* index into _numeric[] */

struct _catinfo {
	char	name[LC_NAMELEN];
	char	locale[LC_NAMELEN];
	int	(*setfunc)();
};
 
extern struct _catinfo	_catinfo[LC_CATEGORIES];
extern char _ctype[SZ_CTYPE];
extern unsigned char _numeric[SZ_NUMERIC];

