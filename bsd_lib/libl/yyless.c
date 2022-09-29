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
#ident	"$Header: yyless.c,v 1.1.2.2 90/05/07 21:42:03 wje Exp $"

/*	@(#)yyless.c	4.1	12/25/82	*/

yyless(x)
{
extern char yytext[];
register char *lastch, *ptr;
extern int yyleng;
extern int yyprevious;
lastch = yytext+yyleng;
if (x>=0 && x <= yyleng)
	ptr = x + yytext;
else
	ptr = (char *) x;
while (lastch > ptr)
	yyunput(*--lastch);
*lastch = 0;
if (ptr >yytext)
	yyprevious = *--lastch;
yyleng = ptr-yytext;
}
