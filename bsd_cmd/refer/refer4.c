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
#ident	"$Header: refer4.c,v 1.1.2.2 90/05/07 19:12:48 wje Exp $"

#include "refer..c"
#define punctuat(c) (c=='.' || c=='?' || c=='!' || c==',' || c==';' || c==':')

static gate = 0;
static char buff[BUFSIZ];

output(s)
char *s;
{
	if (gate)
		fputs(buff,ftemp);
	else
		gate = 1;
	strcpy(buff, s);
	if (strlen(buff) > BUFSIZ)
		err("one buff too big (%d)!", BUFSIZ);
}

append(s)
char *s;
{
	char *p;
	int lch;

	trimnl(buff);
	for (p = buff; *p; p++)
		;
	lch = *--p;
	if (postpunct && punctuat(lch))
		*p = NULL;
	else /* pre-punctuation */
		switch (lch) {
		case '.': 
		case '?':
		case '!':
		case ',':
		case ';':
		case ':':
			*p++ = lch;
			*p = NULL;
		}
	strcat(buff, s);
	if (postpunct)
		switch(lch) {
		case '.': 
		case '?':
		case '!':
		case ',':
		case ';':
		case ':':
			for(p = buff; *p; p++)
				;
			if (*--p == '\n')
				*p = NULL;
			*p++ = lch;
			*p++ = '\n';
			*p = NULL;
		}
	if (strlen(buff) > BUFSIZ)
		err("output buff too long (%d)", BUFSIZ);
}

flout()
{
	if (gate)
		fputs(buff,ftemp);
	gate = 0;
}

char *
trimnl(ln)
char *ln;
{
	register char *p = ln;

	while (*p)
		p++;
	p--;
	if (*p == '\n')
		*p = 0;
	return(ln);
}
