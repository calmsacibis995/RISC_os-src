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
/* $Header: strfile.h,v 1.1.2.2 90/05/10 03:07:32 wje Exp $ */

/* @(#)strfile.h	1.2 (Berkeley) 5/14/81 */

# define	MAXDELIMS	3

/*
 * bits for flag field
 */

# define	STR_RANDOM	0x1
# define	STR_ORDERED	0x2

struct	strfile {		/* information table */
	unsigned int	str_numstr;		/* # of strings in the file */
	unsigned int	str_longlen;		/* length of longest string */
	unsigned int	str_shortlen;		/* length of shortest string */
	long		str_delims[MAXDELIMS];	/* delimiter markings */
	int		str_flags;		/* bit field for flags */
};

typedef struct strfile	STRFILE;
