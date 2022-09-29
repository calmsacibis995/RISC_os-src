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
#ident	"$Header: yyless.c,v 1.5.2.2 90/05/10 02:49:30 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

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
	/*
	 * The cast on the next line papers over an unconscionable nonportable
	 * glitch to allow the caller to hand the function a pointer instead of
	 * an integer and hope that it gets figured out properly.  But it's
	 * that way on all systems .   
	 */
	ptr = (char *) x;
while (lastch > ptr)
	yyunput(*--lastch);
*lastch = 0;
if (ptr >yytext)
	yyprevious = *--lastch;
yyleng = ptr-yytext;
}
