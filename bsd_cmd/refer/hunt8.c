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
#ident	"$Header: hunt8.c,v 1.1.2.3 90/05/07 19:10:56 wje Exp $"

#include <stdio.h>
#include <assert.h>
#define unopen(fil) {if (fil!=NULL) {fclose(fil); fil=NULL;}}

extern long indexdate, gdate();
extern FILE *iopen();
runbib (s)
char *s;
{
	/* make a file suitable for fgrep */
	char tmp[200];
#ifdef RISCOS
	sprintf(tmp, "/bsd43/usr/lib/refer/mkey '%s' >'%s.ig'", s,s);
#else
	sprintf(tmp, "/usr/lib/refer/mkey '%s' >'%s.ig'", s,s);
#endif
	system(tmp);
}

makefgrep(indexname)
char *indexname;
{
	FILE *fa, *fb;
	if (ckexist(indexname, ".ig"))
	{
		/* existing gfrep -type index */
# if D1
		fprintf(stderr, "found fgrep\n");
# endif
		fa = iopen(indexname, ".ig");
		fb = iopen(indexname, "");
		if (gdate(fb)>gdate(fa))
		{
			if (fa!=NULL)
				fclose(fa);
			runbib(indexname);
			fa= iopen(indexname, ".ig");
		}
		indexdate = gdate(fa);
		unopen(fa); 
		unopen(fb);
	}
	else
		if (ckexist(indexname, ""))
		{
			/* make fgrep */
# if D1
			fprintf(stderr, "make fgrep\n");
# endif
			runbib(indexname);
			time(&indexdate);
		}
		else /* failure */
		return(0);
	return(1); /* success */
}

ckexist(s, t)
char *s, *t;
{
	char fnam[100];
	strcpy (fnam, s);
	strcat (fnam, t);
	return (access(fnam, 04) != -1);
}

FILE *
iopen(s, t)
char *s, *t;
{
	char fnam[100];
	FILE *f;
	strcpy (fnam, s);
	strcat (fnam, t);
	f = fopen (fnam, "r");
	if (f == NULL)
	{
		err("Missing expected file %s", fnam);
		exit(1);
	}
	return(f);
}
