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
#ident	"$Header: bcopy.c,v 1.2.2.2 90/05/09 18:07:39 wje Exp $"
#ifdef vax
bcopy(from, to, len)
	char *from, *to;
	int len;
{
	asm("	movc3	12(ap),*4(ap),*8(ap)");
}
#else 
#ifdef	SYSV
/*
 * Added by moderator, bcopy.c for Sys V.
 *
 */

extern char *memcpy();

char *
bcopy(from, to, count)
	char *from;
	char *to;
	int count;
{
	return (memcpy(to, from, count));
}
#else
bcopy(from, to, len)
	register char	*to, *from;
	register int	len;
{
	if (from > to) {
		while(len--)
			*to++ = *from++;
	} else {	/* not handled in standard version of bcopy() */
		to	+= len;
		from	+= len;
		while(len--)
			*--to = *--from;
	}
}
#endif
#endif
