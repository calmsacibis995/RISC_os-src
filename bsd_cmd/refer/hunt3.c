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
#ident	"$Header: hunt3.c,v 1.1.2.2 90/05/07 19:10:33 wje Exp $"

#include "refer..c"
#define BSIZ 250

getq(v)
char *v[];
{
	static char buff[BSIZ];
	static int eof = 0;
	extern char *sinput;
	char *p;
	int c, n = 0, las = 0;
	if (eof) return(-1);
	p = buff;
	while ( (c = (sinput ? *sinput++ : getchar()) ) > 0)
	{
		if (c== '\n')
			break;
		if (isalpha(c) || isdigit(c))
		{
			if (las==0)
			{
				v[n++] = p;
				las=1;
			}
			if (las++ <= 6)
				*p++ = c;
		}
		else
		{
			if (las>0)
				*p++ = 0;
			las=0;
		}
	}
	*p=0;
	if (p > buff + BSIZ)
		fprintf(stderr, "query long than %d characters\n", BSIZ);
	assert(p < buff + BSIZ);
	if (sinput==0 && c<= 0) eof=1;
# if D1
	fprintf(stderr, "no. keys %d\n",n);
	for(c=0; c<n; c++)
		fprintf(stderr, "keys X%sX\n", v[c]);
# endif
	return(n);
}
