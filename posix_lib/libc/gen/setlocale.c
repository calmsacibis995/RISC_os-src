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
#ident	"$Header: setlocale.c,v 1.3.1.4 90/05/10 04:13:54 wje Exp $"

#include <limits.h>
#include <locale.h>
#include <ctype.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include "_locale.h"

extern	char 		*getenv(), *strcpy(), *memcpy(), *strncpy(), *strcat();
extern	int 		_set_numeric(), _set_ctype(), _set_null();
extern  int 		open(), read(), close();

struct _catinfo _catinfo[] = { 
	{ "LC_ALL",  	"C", 0 },
	{ "LC_CTYPE",  	"C", _set_ctype  },
	{ "LC_COLLATE", "C", _set_null },
	{ "LC_TIME",  	"C", _set_null },
	{ "LC_NUMERIC",	"C", _set_numeric },
	{ "LC_MONETARY","C", _set_null },
};

unsigned char _numeric[SZ_NUMERIC] = { '.', '\0', }; 

char * 
setlocale(category, locale)
int category; 
char *locale;
{
	char	*loc;
	int 	i;

	if (category < 0 || category >= LC_CATEGORIES)
		return(NULL);

	/* query locale */
	if (locale == NULL) {
		if (category == LC_ALL) {
			loc = _catinfo[1].locale;
			for (i = 2; i < LC_CATEGORIES; i++) {
				if (strcmp(_catinfo[i].locale, loc) != 0) {
					loc = NULL;
					goto done;
				}
			}
		} else
			loc = _catinfo[category].locale;
		goto done;
	}

	/* set locale to implementation-defined default */
	if (category == LC_ALL) {
		int	cnt = 0;  /* # of categories differing from the first */

		if (*locale == '\0' ) {
			char	*lang, *name;

			/* The results of 
			 *	setlocale(LC_ALL, "");
			 * when LANG is not defined, or is defined to the
			 * null string, is implementation-defined...
			 * In this case, we set LANG to the "C" locale.
			 */
			
			if ((lang=getenv("LANG")) == NULL || *lang == '\0')
				lang = "C";

			for (i = 1; i < LC_CATEGORIES; i++) {
				name = _catinfo[i].name;
				if ((loc=getenv(name)) == NULL || *loc == '\0')
					loc = lang;
				/* if loc is the same, or loc is different,
				 * and the set succeeds, then copy loc
				 * into _catinfo
				 */
				if (strcmp(_catinfo[i].locale, loc) != 0 &&
				    (*_catinfo[i].setfunc)(loc) == 0) {
					strncpy(_catinfo[i].locale, loc, 
						LC_NAMELEN);
					_catinfo[i].locale[LC_NAMELEN-1] = '\0';
				}
				if (i > 1)
					cnt += strcmp(loc, _catinfo[i-1].locale);
			}
		} else {
			loc = locale;
			for (i = 1; i < LC_CATEGORIES; i++) {
				/* if loc is not the same, and the setfunc 
				 * succeeds, then copy loc into _catinfo
				 */
				if (strcmp(_catinfo[i].locale, loc) != 0 &&
				    (*_catinfo[i].setfunc)(loc) == 0) {
					strncpy(_catinfo[i].locale, loc, 
						LC_NAMELEN);
					_catinfo[i].locale[LC_NAMELEN-1] = '\0';
				}
				if (i > 1)
					cnt += strcmp(loc, _catinfo[i-1].locale);
			}
		}
		/* if all of the categories are not set to the
		 * same locale, we return NULL...the standard
		 * does not specify how to handle this....
		 */
		if (cnt) 
			loc = NULL;
	} else {
		if (*locale == '\0') {
			loc = getenv(_catinfo[category].name);
			if (loc == NULL || *loc == '\0') {
				loc = getenv("LANG");
				if (loc == NULL || *loc == '\0')
					loc = "C";
			}
		} else {
			loc = locale;
		}
		if ((*_catinfo[category].setfunc)(loc) != 0)
			loc = NULL;
	}

	if (loc != NULL) {
		strncpy(_catinfo[category].locale, loc, LC_NAMELEN);
		_catinfo[category].locale[LC_NAMELEN-1] = '\0';
	}
done:
	return(loc);
}


static int
_set_numeric(loc)
char 	*loc;
{
	char pathname[128];
	unsigned char my_numeric[SZ_NUMERIC];	/* local copy */
	register int fd, ret = -1;

	(void)strcpy(pathname, LOCALE_DIR);
	(void)strcat(pathname, loc);
	(void)strcat(pathname, "/LC_NUMERIC");
	if ((fd = open(pathname, O_RDONLY)) >= 0)
	{
		if (read(fd, my_numeric, SZ_NUMERIC) == SZ_NUMERIC) 
		{
			/*
			* Update _numeric[] only after everything works.
			*/
			(void)memcpy(_numeric, my_numeric, SZ_NUMERIC);
			ret = 0;
		}
		(void)close(fd);
	}
	return(ret);
}

static int
_set_ctype(loc)
char 	*loc;
{
	char pathname[128];
	char my_ctype[SZ_CTYPE];		/* local copy */
	register int fd, ret = -1;

	(void)strcpy(pathname, LOCALE_DIR);
	(void)strcat(pathname, loc);
	(void)strcat(pathname, "/LC_CTYPE");
	if ((fd = open(pathname, O_RDONLY)) >= 0)
	{
		if (read(fd, my_ctype, SZ_CTYPE) == SZ_CTYPE)
		{
			/*
			* Update _ctype[] only after everything works.
			*/
			(void)memcpy(_ctype, my_ctype, SZ_CTYPE);
			ret = 0;
		}
		(void)close(fd);
	}
	return (ret);
}

static int
_set_null(loc)
char	*loc;
{
	return(0);
}
