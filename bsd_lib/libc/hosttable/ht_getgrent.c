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
#ident	"$Header: ht_getgrent.c,v 1.2.1.5.1.3 90/08/13 17:13:41 hawkes Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <stdio.h>
#include <grp.h>

#define	CL	':'
#define	CM	','
#define	NL	'\n'
#define TRUE	1
#define FALSE	0

extern int atoi(), fclose();
extern char *fgets();
extern FILE *fopen();
extern void rewind();

static char *GROUP = "/etc/group";
static FILE *grf = NULL;
static char *line, *gr_mem;
static struct group grp;
static int size, gr_size;

ht_setgrent()
{
	if (grf == NULL)
		grf = fopen(GROUP, "r");
	else
		rewind(grf);
	return(0);
}

ht_endgrent()
{
	if (grf != NULL)
		{
		(void) fclose(grf);
		grf = NULL;
		}
	return(0);
}

static void
cleanup()
{
	if (line != NULL)
		{
		free(line);
		line = NULL;
		}
	if (gr_mem != NULL)
		{
		free (gr_mem);
		gr_mem = NULL;
		}
	(void) ht_endgrent();
}

static char *
grskip(p, c)
char *p;
int c;
{
	while (*p != '\0' && *p != c)
		++p;
	if (*p != '\0')
	 	*p++ = '\0';
	return(p);
}

struct group *
ht_getgrent()
{
	extern struct group *ht_fgetgrent();

	if (grf == NULL && (grf = fopen(GROUP, "r")) == NULL)
		return(NULL);
	return (ht_fgetgrent(grf));
}

struct group *
ht_fgetgrent(f)
FILE *f;
{
	char *p, **q;
	int len, count;
	long offset, ftell(), lseek();
	char done, *calloc(), *realloc();

	if (f == (FILE *)NULL)
		return(NULL);
	count = 1;
	if (line == NULL)
		{
		size = BUFSIZ+1;
		if ((line = calloc((unsigned)size, sizeof(char))) == NULL)
			{
			(void) cleanup();
			return(NULL);
			}
		}
	done = FALSE;
	while (!done)
		{
		offset = ftell(f);
		if ((p = fgets(line, size, f)) == NULL)
		{
			(void) cleanup();
			return(NULL);
		}
		len = strlen(p);
		if ((len <= size) && (p[len-1] == NL))
			done = TRUE;
		else
			{
			size *= 32;
			if ((line = realloc(line, (unsigned)size * sizeof(char))) == NULL)
				{
				(void) cleanup();
				return(NULL);
				}
			fseek(f, offset, 0);
			}
		}
	grp.gr_name = p;
	/* names starting with + or - are special yp names */
	if ((*p == '+') || (*p == '-'))
	{
	    (void) cleanup();
	    return(NULL);
	}
	grp.gr_passwd = p = grskip(p, CL);
	grp.gr_gid = atoi(p = grskip(p, CL));
	p = grskip(p, CL);
	(void) grskip(p, NL);
	if (gr_mem == NULL)
		{
		gr_size = 2;
		if ((gr_mem = calloc((unsigned)(gr_size), sizeof(char *))) == NULL)
			{
			(void) cleanup();
			return(NULL);
			}
		}
		grp.gr_mem = (char **)gr_mem;
		q = grp.gr_mem;
		while (*p != '\0')
			{
			if (count >= gr_size - 1)
				{
				*q = NULL;
				gr_size *= 2;
				if ((gr_mem = realloc(gr_mem, (unsigned)(gr_size) * sizeof(char *))) == NULL)
					{
					(void) cleanup();
					return(NULL);
					}
				grp.gr_mem = (char **)gr_mem;
				q = grp.gr_mem;
				while (*q != NULL)
					q++;
				}
			count++;
			*q++ = p;
			p = grskip(p, CM);
			}
		*q = NULL;
		return(&grp);
}


setgrfile(file)
	char *file;
{
	GROUP = file;
}
