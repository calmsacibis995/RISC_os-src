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
#ident	"$Header: index.c,v 1.5.2.2 90/05/10 01:08:51 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
	If `s2' is a substring of `s1' return the offset of the first
	occurrence of `s2' in `s1',
	else return -1.
*/

index(as1,as2)
char *as1,*as2;
{
	register char *s1,*s2,c;
	int offset;

	s1 = as1;
	s2 = as2;
	c = *s2;

	while (*s1)
		if (*s1++ == c) {
			offset = s1 - as1 - 1;
			s2++;
			while ((c = *s2++) == *s1++ && c) ;
			if (c == 0)
				return(offset);
			s1 = offset + as1 + 1;
			s2 = as2;
			c = *s2;
		}
	 return(-1);
}
