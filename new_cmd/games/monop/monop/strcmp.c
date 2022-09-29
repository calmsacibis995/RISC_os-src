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
#ident	"$Header: strcmp.c,v 1.1.2.2 90/05/10 03:18:11 wje Exp $"

# include	<stdio.h>
# include	<ctype.h>

# define	reg	register

# define	makelower(c)	(isupper(c) ? tolower(c) : c)

/*
 * Compare strings:  s1>s2: >0  s1==s2: 0  s1<s2: <0
 */

strcmp(s1, s2)
reg char	*s1, *s2; {

	while (makelower(*s1) == makelower(*s2)) {
		if (*s1 == '\0')
			return 0;
		s1++, s2++;
	}
	return *s1 - *s2;
}
