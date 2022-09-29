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
#ident	"$Header: common.c,v 1.1.1.2 90/05/09 15:37:04 wje Exp $"
/* $Log:	common.c,v $
 * Revision 1.1.1.2  90/05/09  15:37:04  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:17:22  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:41:27  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:03  bettina
 * 2.0G
 * 
 * Revision 1040.2  88/09/29  15:52:50  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:41:24  gb
 * added $Log keyword
 *  */
#include "mfile1"
#include "tables.h"

# ifndef EXIT
# define EXIT exit
# endif
/*	ref		date		description			*/
/*	!01		17may85		introduced volatile		*/

int nerrors = 0;  /* number of errors */
int Wflag   = 0;  /* if nonzero silent warning messages */
unsigned int offsz;
char *myname;
extern int gdebug;

unsigned int caloff(){
	register i;
	unsigned int temp;
	unsigned int off;
	temp = 1;
	i = 0;
	do {
		temp <<= 1;
		++i;
		} while( temp > 0 );
	off = ( (unsigned) 1 ) << (i-1);
	return (off);
	}

NODE *lastfree;  /* pointer to last free node; (for allocator) */

	/* VARARGS1 */
uerror( s, a ) char *s; { /* nonfatal error message */
	/* the routine where is different for pass 1 and pass 2;
	/*  it tells where the error took place */

	++nerrors;
#ifndef LINT
	fprintf( stderr, "%s: Error: ", myname);
#endif
	where('u');
	fprintf( stderr, s, a );
	fprintf( stderr, "\n" );
#ifndef LINT
	printline();
#endif
	if( nerrors > 30 ) cerror( "too many errors");
#ifndef LINT
	gdebug = 0;
#endif
	}

	/* VARARGS1 */
cerror( s, a, b, c ) char *s; { /* compiler error: die */
	fprintf( stderr, "(ccom): ");
	where('c');
#ifdef LINT
	fprintf( stderr, "\n\t**** cannot recover from this error ****\n\n" );
#else
	if( nerrors && nerrors <= 30 ){ 
	    /* give the compiler the benefit of the doubt */
		fprintf( stderr, "cannot recover from earlier errors: goodbye!\n" );
	} else {
		fprintf( stderr, "%s: Internal: ", myname);
		fprintf( stderr, s, a, b, c );
		fprintf( stderr, "\n" );
	}
	printline();
#endif
#ifdef FORT
	{
	extern short debugflag;
	if (debugflag)
		abort();
	}
#endif /*FORT*/
	EXIT(2);
	}

	/* VARARGS1 */
werror( s, a, b ) char *s; {  /* warning */
	if( Wflag ) return;
#ifndef LINT
	fprintf( stderr, "%s: ", myname);
#endif
	fprintf( stderr, "Warning: ");
	where('w');
	fprintf( stderr, s, a, b );
	fprintf( stderr, "\n" );
#ifndef LINT
	printline();
#endif
	}

tinit(){ /* initialize expression tree search */

	register NODE *p;

	for( p=node; p<= &node[nodemax-1]; ++p ) p->in.op = FREE;
	lastfree = node;

	}

# define TNEXT(p) (p== &node[nodemax-1]?node:p+1)

NODE *
talloc(){
	register NODE *p, *q;

	q = lastfree;
	for( p = TNEXT(q); p!=q; p= TNEXT(p))
		if( p->in.op ==FREE ) return(lastfree=p);

	cerror( "out of tree space; increase default option: -Wf,-XNh%d", nodemax);
	/* NOTREACHED */
	}

tcheck(){ /* ensure that all nodes have been freed */

	register NODE *p;
	extern NODE *first;
	extern int eprint();
	if( !nerrors ) {
		for( p=first; p!= lastfree; p= TNEXT(p) ) {
			if( p->in.op != FREE ) {
				printf( "Tree not freed:\n");
				fwalk( p, eprint, 0);
				cerror( "wasted space: %o", p );
				}
			}
		first = lastfree;
		}

		/* only call tinit() if there are errors */
	else tinit();
#ifdef FLEXNAMES
	freestr();
#endif
	}
tfree( p )  NODE *p; {
	/* free the tree p */
	extern tfree1();

	if( p->in.op != FREE ) walkf( p, tfree1 );

	}

tfree1(p)  NODE *p; {
	if( p == 0 ) cerror( "freeing blank tree!");
	else p->in.op = FREE;
	}

fwalk( t, f, down ) register NODE *t; int (*f)(); {

	int down1, down2;

	more:
	down1 = down2 = 0;

	(*f)( t, down, &down1, &down2 );

	switch( optype( t->in.op ) ){

	case BITYPE:
		fwalk( t->in.left, f, down1 );
		t = t->in.right;
		down = down2;
		goto more;

	case UTYPE:
		t = t->in.left;
		down = down1;
		goto more;

		}
	}

walkf( t, f ) register NODE *t;  int (*f)(); {
	register opty;

	opty = optype(t->in.op);

	if( opty != LTYPE ) walkf( t->in.left, f );
	if( opty == BITYPE ) walkf( t->in.right, f );
	(*f)( t );
	}



int dope[ DSIZE ];
char *opst[DSIZE];

struct dopest { int dopeop; char opst[8]; int dopeval; } indope[] = {

	NAME, "NAME", LTYPE,
	STRING, "STRING", LTYPE,
	REG, "REG", LTYPE,
	OREG, "OREG", LTYPE,
	ICON, "ICON", LTYPE,
	FCON, "FCON", LTYPE,
	CCODES, "CCODES", LTYPE,
	UNARY MINUS, "U-", UTYPE,
	UNARY MUL, "U*", UTYPE,
	UNARY AND, "U&", UTYPE,
	UNARY CALL, "UCALL", UTYPE|CALLFLG,
	UNARY FORTCALL, "UFCALL", UTYPE|CALLFLG,
	NOT, "!", UTYPE|LOGFLG,
	COMPL, "~", UTYPE,
	FORCE, "FORCE", UTYPE,
	INIT, "INIT", UTYPE,
	SCONV, "SCONV", UTYPE,
	PCONV, "PCONV", UTYPE,
	PLUS, "+", BITYPE|FLOFLG|SIMPFLG|COMMFLG,
	ASG PLUS, "+=", BITYPE|ASGFLG|ASGOPFLG|FLOFLG|SIMPFLG|COMMFLG,
	MINUS, "-", BITYPE|FLOFLG|SIMPFLG,
	ASG MINUS, "-=", BITYPE|FLOFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	MUL, "*", BITYPE|FLOFLG|MULFLG,
	ASG MUL, "*=", BITYPE|FLOFLG|MULFLG|ASGFLG|ASGOPFLG,
	AND, "&", BITYPE|SIMPFLG|COMMFLG,
	ASG AND, "&=", BITYPE|SIMPFLG|COMMFLG|ASGFLG|ASGOPFLG,
	QUEST, "?", BITYPE,
	COLON, ":", BITYPE,
	ANDAND, "&&", BITYPE|LOGFLG,
	OROR, "||", BITYPE|LOGFLG,
	CM, ",", BITYPE,
	COMOP, ",OP", BITYPE,
	ASSIGN, "=", BITYPE|ASGFLG,
	DIV, "/", BITYPE|FLOFLG|MULFLG|DIVFLG,
	ASG DIV, "/=", BITYPE|FLOFLG|MULFLG|DIVFLG|ASGFLG|ASGOPFLG,
	MOD, "%", BITYPE|DIVFLG,
	ASG MOD, "%=", BITYPE|DIVFLG|ASGFLG|ASGOPFLG,
	LS, "<<", BITYPE|SHFFLG,
	ASG LS, "<<=", BITYPE|SHFFLG|ASGFLG|ASGOPFLG,
	RS, ">>", BITYPE|SHFFLG,
	ASG RS, ">>=", BITYPE|SHFFLG|ASGFLG|ASGOPFLG,
	OR, "|", BITYPE|COMMFLG|SIMPFLG,
	ASG OR, "|=", BITYPE|COMMFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	ER, "^", BITYPE|COMMFLG|SIMPFLG,
	ASG ER, "^=", BITYPE|COMMFLG|SIMPFLG|ASGFLG|ASGOPFLG,
	INCR, "++", BITYPE|ASGFLG,
	DECR, "--", BITYPE|ASGFLG,
	STREF, "->", BITYPE,
	CALL, "CALL", BITYPE|CALLFLG,
	FORTCALL, "FCALL", BITYPE|CALLFLG,
	EQ, "==", BITYPE|LOGFLG,
	NE, "!=", BITYPE|LOGFLG,
	LE, "<=", BITYPE|LOGFLG,
	LT, "<", BITYPE|LOGFLG,
	GE, ">=", BITYPE|LOGFLG,
	GT, ">", BITYPE|LOGFLG,
	UGT, "UGT", BITYPE|LOGFLG,
	UGE, "UGE", BITYPE|LOGFLG,
	ULT, "ULT", BITYPE|LOGFLG,
	ULE, "ULE", BITYPE|LOGFLG,
	ARS, "A>>", BITYPE,
	TYPE, "TYPE", LTYPE,
	LB, "[", BITYPE,
	CBRANCH, "CBRANCH", BITYPE,
	FLD, "FLD", UTYPE,
	PMCONV, "PMCONV", BITYPE,
	PVCONV, "PVCONV", BITYPE,
	RETURN, "RETURN", BITYPE|ASGFLG|ASGOPFLG,
	CAST, "CAST", BITYPE|ASGFLG|ASGOPFLG,
	GOTO, "GOTO", UTYPE,
	STASG, "STASG", BITYPE|ASGFLG,
	STARG, "STARG", UTYPE,
	STCALL, "STCALL", BITYPE|CALLFLG,
	UNARY STCALL, "USTCALL", UTYPE|CALLFLG,
	UNARY_VOLATILE_MUL, "U_VOL*", UTYPE,
	PASSIGN, "PASSIGN", BITYPE|ASGFLG,

-1,	0
};

mkdope(){
	register struct dopest *q;

	for( q = indope; q->dopeop >= 0; ++q ){
		dope[q->dopeop] = q->dopeval;
		opst[q->dopeop] = q->opst;
		}
	}
# ifndef BUG4
tprint( t )  TWORD t; { /* output a nice description of the type of t */

	static char * tnames[] = {
		"undef",
		"farg",
		"char",
		"short",
		"int",
		"long",
		"float",
		"double",
		"strty",
		"unionty",
		"enumty",
		"moety",
		"uchar",
		"ushort",
		"unsigned",
		"ulong",
		"?", "?"
		};

	for(;; t = DECREF(t) ){

		if( ISVP(t) ) printf( "VOL PTR " );
		else if( ISPTR(t) ) printf( "PTR " );
		else if( ISFTN(t) ) printf( "FTN " );
		else if( ISARY(t) ) printf( "ARY " );
		else if( ISVOL(t) ) printf( "VOL " );			/*!01*/
		else {
			printf( "%s", tnames[t] );	
			return;
			}
		}
	}
# endif

extern char	**curtstr;
int	tstrused;

char *
tstr( cp )			/* place copy of string into temp storage */
	register char *cp;	/* strings longer than TSTRSZ will break tstr */
{
	register int i = strlen( cp );
	register char *dp;

	if ( tstrused + i >= itstrbufmax )
	{
		if ( ++curtstr >= &tstrbuf[tstrbufmax] )
			cerror( "out of temporary string space; increase default option: -Wf,-XNc%d", tstrbufmax );
		tstrused = 0;
		if ( *curtstr == 0 )
		{
			if ( ( dp = (char *) malloc( itstrbufmax ) ) == 0 )
				cerror( "out of memory [tstr()]; increase default option: -Wf,-XNb%d", itstrbufmax );
			*curtstr = dp;
		}
	}
	strcpy( dp = *curtstr + tstrused, cp );
	tstrused += i + 1;
	return ( dp );
}


printline()
{
	extern char	*bufr;
	extern char	*bufptr;
	char		*trav;
	if (bufr == NULL ) return;
	if (bufr[2] == EOF)
		fprintf(stderr,"%*c  END OF FILE\n", strlen(myname), ' ');
	else {
		int i;
		fprintf (stderr, "%*c  %s", strlen(myname), ' ', &bufr[2]);
		fprintf (stderr, "%*c  ", strlen(myname), ' ');
		for (trav= &bufr[2],i= strlen(myname)+3; trav < bufptr; i++, trav++) {
			if (*trav == '	')	/* tab */
				while (i%8 != 0) 
					(fprintf (stderr, "-"), i++);
			fprintf (stderr, "-");
		};
	    fprintf (stderr, "^\n");
	};
} /* printline */

assertion_error(line, file)
int line;
char *file;
{
	char msg[120];

	cerror("assertion error at file %s, line %d", file, line);
	/*NOTREACHED*/
}
