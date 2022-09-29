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
#ident	"$Header: strpcpy.c,v 1.1.1.2 90/05/09 18:04:21 wje Exp $"

/*
 * Author: Peter J. Nicklin
 */

/*
 * strpcpy() copies string s2 to s1 and returns a pointer to the
 * next character after s1.
 */
char *
strpcpy(s1, s2)
	register char *s1;
	register char *s2;
{
	while (*s1++ = *s2++)
		continue;
	return(--s1);
}
