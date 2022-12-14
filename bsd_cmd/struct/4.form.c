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
#ident	"$Header: 4.form.c,v 1.1.1.2 90/05/07 19:24:44 wje Exp $"

#ifndef lint
static char sccsid[] = "@(#)4.form.c	4.1	(Berkeley)	2/11/83";
#endif not lint

#include <stdio.h>
#
#include "def.h"
#include "4.def.h"
extern int linechars;
extern int rdfree(), comfree(), labfree(), contfree();
extern int rdstand(), comstand(), labstand(), contstand();
extern int (*rline[])();
extern int (*comment[])();
extern int (*getlabel[])();
extern int (*chkcont[])();
null(c)
char c;
	{return;}



comprint()
	{
	int c, blank, first,count;
	blank = 1;
	first = 1;
	count = 0;
	while ((c = (*comment[inputform])(0) ) || blankline() )
		{
		++count;
		if (c)
			{
			(*comment[inputform])(1);		/* move head past comment signifier */
			blank = blankline();
			/* if (first && !blank)
				OUTSTR("#\n");*/
			prline("#");
			first = 0;
			}
		else
			(*rline[inputform])(null);
		}
	/* if (!blank) 
		OUTSTR("#\n"); */
	return(count);
	}



prcode(linecount,tab)
int linecount, tab;
	{
	int someout;
	someout = FALSE;
	while (linecount)
		{
		if ( (*comment[inputform])(0) )
			{
			linecount -= comprint();
			someout = TRUE;
			continue;
			}
		else if (blankline() )
			(*rline[inputform])(null);
		else if ((*chkcont[inputform])() )
			{
			TABOVER(tab);
			prline("&");
			someout  = TRUE;
			}
		else 
			{if (someout) TABOVER(tab);
			(*getlabel[inputform])(null);
			prline("");
			someout=TRUE;
			}
		--linecount;
		}
	}


charout(c)
char c;
	{
	putc(c,outfd);
	}



prline(str)
char *str;
	{
	fprintf(outfd,"%s",str);
	(*rline[inputform]) (charout);
	putc('\n',outfd);
	}


input2()
	{
	static int c;
	c = inchar();
	if (c == '\n')
		linechars = 0;
	else
		++linechars;
	return(c);
	}


unput2(c)
int c;
	{
	unchar(c);
	--linechars;
	return(c);
	}
