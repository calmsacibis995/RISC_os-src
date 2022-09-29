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
#ident	"$Header: strstr.c,v 1.1.1.2 90/05/10 01:39:41 wje Exp $"

/*LINTLIBRARY*/
/*
 *  strstr() locates the first occurrence in the string pointed to by s1
 *  of the sequence of characters (excluding the null terminating byte)
 *  in the string pointed to by s2.
 * 
 *  if no matching string is located, or s1 is null, strstr() returns NULL
 *  if s2 is NULL, then s1 is returned.
 */

#ifndef	NULL
#define	NULL	(char *)0
#endif

char *
strstr(s1, s2)
register char *s1, *s2;
{
	char *str1, *str2;
	char c;
	char *tstr;

	str1 = s1;
	str2 = s2;

	if (str1 == NULL || str2 == NULL || *str2 == '\0')
		return(str1);

	c = *str2;
	do {
		while (*str1 && *str1 != c)
			str1++;
		if (*str1) {
			tstr = str1;
			while(*tstr && *str2 && *tstr++ == *str2++);
			if (*str2) {
				str1++;
				str2 = s2;
			} else
				return(str1);
		} 
	} while (*str1);
	return(NULL);
}
