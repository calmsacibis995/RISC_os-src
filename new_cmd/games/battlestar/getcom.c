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
#ident	"$Header: getcom.c,v 1.1.2.2 90/05/10 03:00:47 wje Exp $"

/*
 * Copyright (c) 1983 Regents of the University of California,
 * All rights reserved.  Redistribution permitted subject to
 * the terms of the Berkeley Software License Agreement.
 */

#include <stdio.h>
#include <ctype.h>

char *
getcom(buf, size, prompt, error)
	char *buf;
	int size;
	char *prompt, *error;
{
	for (;;) {
		fputs(prompt, stdout); 
		if (fgets(buf, size, stdin) == 0) {
			clearerr(stdin);
			continue;
		}
		while (isspace(*buf))
			buf++;
		if (*buf)
			break;
		if (error)
			puts(error);
	}
	return (buf);
}


/*
 * shifts to UPPERCASE if flag > 0, lowercase if flag < 0,
 * and leaves it unchanged if flag = 0
 */
char *
getword(buf1, buf2, flag)
	register char *buf1, *buf2;
	register flag;
{
	while (isspace(*buf1))
		buf1++;
	if (*buf1 != ',') {
		if (!*buf1) {
			*buf2 = 0;
			return (0);
		}
		while (*buf1 && !isspace(*buf1) && *buf1 != ',')
			if (flag < 0)
				if (isupper(*buf1))
					*buf2++ = tolower(*buf1++);
				else
					*buf2++ = *buf1++;
			else if (flag > 0)
				if (islower(*buf1))
					*buf2++ = toupper(*buf1++);
				else
					*buf2++ = *buf1++;
			else
				*buf2++ = *buf1++;
	} else
		*buf2++ = *buf1++;
	*buf2 = 0;
	while (isspace(*buf1))
		buf1++;
	return (*buf1 ? buf1 : 0);
}
