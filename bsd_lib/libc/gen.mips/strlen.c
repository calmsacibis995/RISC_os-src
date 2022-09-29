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
#ident	"$Header: strlen.c,v 1.1.2.2 90/05/07 20:33:24 wje Exp $"

/*
 * Returns the number of non-NULL bytes in string argument.
 * Original code:
 *	strlen(s)
 *	register char *s;
 *	{
 *		register n;
 *		n = 0;
 *		while (*s++)
 *			n++;
 *		return(n);
 *	}
 */

/* Code optimized for mips.  4 cycles/byte. */
strlen(s)
register char *s;
{
	register char *p = s + 1;
	register unsigned c;	/* c exists only because UOPT wastes a
				   register (but no cycles) if the
				   while is written as *s++ != '\0' */
	do {
		c = *s++;
	} while (c != '\0');
	return s - p;
}
