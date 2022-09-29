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
#ident	"$Header: allprint.c,v 1.1.2.2 90/05/07 21:41:20 wje Exp $"

/*	@(#)allprint.c	4.1	12/25/82	*/

# include <stdio.h>
allprint(c)
  char c; {
	extern FILE *yyout;
	switch(c){
		case '\n':
			fprintf(yyout,"\\n");
			break;
		case '\t':
			fprintf(yyout,"\\t");
			break;
		case '\b':
			fprintf(yyout,"\\b");
			break;
		case ' ':
			fprintf(yyout,"\\\bb");
			break;
		default:
			if(!printable(c))
				fprintf(yyout,"\\%-3o",c);
			else 
				putc(c,yyout);
			break;
		}
	return;
	}
sprint(s)
  char *s; {
	while(*s)
		allprint(*s++);
	return;
	}
printable(c)
  int c;
	{
	return(040 < c && c < 0177);
	}
