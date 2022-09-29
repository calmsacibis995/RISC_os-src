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
#ident	"$Header: scan.c,v 1.3.2.2 90/05/09 16:50:05 wje Exp $"

/*
	scanner for a header file describing C function declarations 
*/
#include <stdio.h>
/* lex.h contains the definitions for each character class */
#include "lex.h"
/* tokens.h contains the definitions for each token */
#include "tokens.h"

/* the character map is initialized in charmap.c */
int cmap[128];

/* the token array is initialized in token.c */
extern token_t token[];

/* on EOF, agetchar has the value 0x7f */
#define agetchar() (getchar() & 0x7f)


char tokenbuf[80],lasttoken[80],lastname[80];
char linebuf[512];
char *tokenptr,*linebufptr;
#define LINEBUFEND &linebuf[511]
int yylval,_par_lineno=1;
int retval;

#ifdef YYDEBUG
#define COUNT_LINES	\
			fprintf(stderr,"%d..",_par_lineno++)
#else
#define COUNT_LINES	_par_lineno++
#endif

#define ISDIGIT(a) ((a >= '0') && (a <= '9'))


int yylex() {

	int curclass,class,c;
	/* scanner */
	yylval = 0;
#ifdef DEBUG
	fflush(stdout);
	fflush(stderr);
#endif

	/* skip null characters */

	while (1) {

		while ((curclass = cmap[c=agetchar()]) & IGNORE) {
	    
			if (curclass == CPP) {
				while (((curclass = cmap[c=agetchar()]) != 
					ENDFILE) &&
				       (curclass != NL))
					;
			}
			if (curclass == NL) COUNT_LINES;
		}

		if ((curclass == BC) && (gobble_comment()))
	    		continue;
		else break;
	}



	/* the class better be SPECIAL, ALPH, or ENDFILE */

	if (curclass == ENDFILE) {
#ifdef NOTDEF
#ifdef DEBUG
	fprintf(stderr,"ENDFILE hit. eofhit = %d\n",eofhit);
#endif
#endif
		yylval = ENDFILE;
		return(ENDFILE);
	}

	tokenptr = tokenbuf;
	class = curclass;

	if (class == ALPH) {
		while (curclass == class) {

			/* gobble the character */
			*tokenptr++ = c;
			curclass = cmap[c = agetchar()];
		}
		/* just broke the class, put the character back */
		if (curclass != ENDFILE) ungetc(c,stdin);
	} else *tokenptr++ = c;

	*tokenptr++ = '\0';

	if (class == ALPH)  {
		if (ISDIGIT(tokenbuf[0])) {
			yylval = atoi(tokenbuf);
			strcpy(lasttoken,tokenbuf);
			return(NUMBER);
		} else {
			retval = lookup(tokenbuf);
			yylval = YYLVAL(retval);
		}
#ifdef DEBUG
fprintf(stderr,"returning yylval = 0x%x, 0x%x token is %s\n",yylval,
			  PARSEVAL(retval),tokenbuf);
#endif
		retval = PARSEVAL(retval);
	}

	else {
#ifdef DEBUG
fprintf(stderr,"returning yylval = 0x%x, 0x%x token is %s\n",yylval,
			  PARSEVAL(class),tokenbuf);
#endif
	retval = PARSEVAL(class);
	}
	return(retval);
}


gobble_comment()
{
	/* the '/' of a possible comment has been seen.
	   If it is a comment, gobble it up, and return TRUE, else
	   return false */
	    
	int class,c;
	if (cmap[c = agetchar()] == BC2) {
		while (1) {
			while (((class = cmap[c = agetchar()]) != BC2) 
			       && (class != ENDFILE)) 
			{
				if (class == NL) COUNT_LINES;
			}
			if (class == ENDFILE) break;
			if (cmap[c = agetchar()] == BC) break;
			else ungetc(c,stdin);
		}
		/* end of comment */
		return(1);
	}
	ungetc(c,stdin);
	return(0);
}


lookup(cptr) char *cptr;
{
	/* cptr is a string with the last token read.  run
	   through the list of our tokens looking for it.  Return
	   the token number (if found) or NAME */

	
	token_t *tptr;

	strcpy(lastname,cptr);
	for (tptr = token; (tptr->tokenstr); tptr++) 
		if (!strcmp(tptr->tokenstr,cptr)) return(tptr->tokenid);
	return(NAME);
}

skiptobrace() {

	/* this routine skips text until either a left or right brace
	   is encountered which is NOT in a comment */

	int cval,c;

#ifdef DEBUG
fprintf(stderr,"skiptobrace");
#endif
	while (1) {
		cval = cmap[c = agetchar()];
#ifdef DEBUG
fprintf(stderr,"%c",c);
#endif
		switch (cval /*& ~SP */) {

		    default: 	break;

		    case NL:
				COUNT_LINES;
				break;		
		    case (ENDFILE):
		    case (SP|LBRACE):  
#ifdef DEBUG
fprintf(stderr,"skiptobrace found lbrace at line %d\n",_par_lineno);
				ungetc(c,stdin);
				return(cval);
#endif
		    case (SP|RBRACE):  	
#ifdef DEBUG
fprintf(stderr,"skiptobrace found rbrace at line %d\n",_par_lineno);
#endif
				ungetc(c,stdin);
				return(cval);

		    case BC:	     	
				gobble_comment();
				break;
	    	}

	}
}

doparse()
{

	yylval=0;
	while (yylval != ENDFILE) yyparse();
}


