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
/* $Header: m4y.y,v 1.5.2.2 90/05/09 16:35:13 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

%{
extern long	evalval;
#define	YYSTYPE	long
%}

%term DIGITS
%left OROR
%left ANDAND
%left '|' '^'
%left '&'
%right '!' '~'
%nonassoc GT GE LT LE NE EQ
%left '+' '-'
%left '*' '/' '%'
%right POWER
%right UMINUS
%%

s	: e	={ evalval = $1; }
	|	={ evalval = 0; }
	;

e	: e OROR e	={ $$ = ($1!=0 || $3!=0) ? 1 : 0; }
	| e ANDAND e	={ $$ = ($1!=0 && $3!=0) ? 1 : 0; }
	| '!' e		={ $$ = $2 == 0; }
	| '~' e		={ $$ = ~$2; }
	| e EQ e	={ $$ = $1 == $3; }
	| e NE e	={ $$ = $1 != $3; }
	| e GT e	={ $$ = $1 > $3; }
	| e GE e	={ $$ = $1 >= $3; }
	| e LT e	={ $$ = $1 < $3; }
	| e LE e	={ $$ = $1 <= $3; }
	| e '|' e	={ $$ = ($1|$3); }
	| e '&' e	={ $$ = ($1&$3); }
	| e '^' e	={ $$ = ($1^$3); }
	| e '+' e	={ $$ = ($1+$3); }
	| e '-' e	={ $$ = ($1-$3); }
	| e '*' e	={ $$ = ($1*$3); }
	| e '/' e	={ $$ = ($1/$3); }
	| e '%' e	={ $$ = ($1%$3); }
	| '(' e ')'	={ $$ = ($2); }
	| e POWER e	={ for ($$=1; $3-->0; $$ *= $1); }	
	| '-' e %prec UMINUS	={ $$ = $2-1; $$ = -$2; }
	| '+' e %prec UMINUS	={ $$ = $2-1; $$ = $2; }
	| DIGITS	={ $$ = evalval; }
	;

%%

extern char *pe;

yylex() {

	while (*pe==' ' || *pe=='\t' || *pe=='\n')
		pe++;
	switch(*pe) {
	case '\0':
	case '+':
	case '-':
	case '/':
	case '%':
	case '^':
	case '~':
	case '(':
	case ')':
		return(*pe++);
	case '*':
		return(peek('*', POWER, '*'));
	case '>':
		return(peek('=', GE, GT));
	case '<':
		return(peek('=', LE, LT));
	case '=':
		return(peek('=', EQ, EQ));
	case '|':
		return(peek('|', OROR, '|'));
	case '&':
		return(peek('&', ANDAND, '&'));
	case '!':
		return(peek('=', NE, '!'));
	default: {
		register	base;

		evalval = 0;

		if (*pe == '0') {
			if (*++pe=='x' || *pe=='X') {
				base = 16;
				++pe;
			} else
				base = 8;
		} else
			base = 10;

		for (;;) {
			register	c, dig;

			c = *pe;

			if (c>='0' && c<='9')
				dig = c - '0';
			else if (c>='a' && c<='f')
				dig = c - 'a' + 10;
			else if (c>='A' && c<='F')
				dig = c - 'A' + 10;
			else
				break;

			evalval = evalval*base + dig;
			++pe;
		}

		return(DIGITS);
	}
	}
}

peek(c, r1, r2)
{
	if (*++pe != c)
		return(r2);
	++pe;
	return(r1);
}

/* VARARGS */
yyerror() {;}
