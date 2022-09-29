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
#ident	"$Header: scan.c,v 1.5.2.3 90/05/09 15:38:46 wje Exp $"


/* $Log:	scan.c,v $
 * Revision 1.5.2.3  90/05/09  15:38:46  wje
 * add restricted rights legend
 * 
 * Revision 1.5.2.2  89/12/10  22:27:05  wje
 * add RID 1.6 to branch RISCOS_4_50_FIXES
 * 
 * Revision 1.6  89/12/10  18:43:06  jay
 * replaced with files from 2.0 C compiler's lint, modified to support cxref.
 * closes bugs 1373
 * 
 * Revision 2000.7  89/03/26  14:48:25  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:12  bettina
 * bootstraped 1.40
 * 
 * Revision 1.3  88/01/12  11:05:03  gb
 * fix #2126, restoring -float option after unprototyped function calls.
 * 
 * Revision 1.2  87/12/09  11:42:17  gb
 * added $Log keyword
 *  */

/* ref.		date		description */
/* !02		26mar85		remove surrounding quotes		*/
/* !03		02apr85		installing ucode writer			*/
/* !04		10apr85		initialization				*/
/* !05		10apr85		added debug option			*/
/* !06		23apr85		added dimension for enumerations	*/
/* !07		16may85		introduced volatile			*/
/* !08		04jun85		symbol table 				*/
/* !09		05sep85		hard-wired maximum size of a unsigned int */
/* 				into maxint, because MIPS C doesn't support */
/*				shifting by 32.				*/
/* !10          16sep85         removed profiling option. If engage, watch*/
/*                              for VAX dependent code.                 */
/* !11		10oct85		added -g option				*/

# include <stdio.h>
# include <signal.h>
# include "mfile1"
# include "messages.h"
# include <ctype.h>
# include "cmplrs/usys.h"						/*!03*/
# include "cmplrs/ucode.h"						/*!03*/
# include "limits.h"
# include "tables.h"

# ifndef ASMBUF
# define ASMBUF 50
# endif
char asmbuf[ASMBUF];
char *asmp;
int asm_esc = 0; /* asm escaped used in file */
	/* lexical actions */

# define A_ERR 0		/* illegal character */
# define A_LET 1		/* saw a letter */
# define A_DIG 2		/* saw a digit */
# define A_1C 3			/* return a single character */
# define A_STR 4		/* string */
# define A_CC 5			/* character constant */
# define A_BCD 6		/* GCOS BCD constant */
# define A_SL 7			/* saw a / */
# define A_DOT 8		/* saw a . */
# define A_PL 9		/* + */
# define A_MI 10		/* - */
# define A_EQ 11		/* = */
# define A_NOT 12		/* ! */
# define A_LT 13		/* < */
# define A_GT 14		/* > */
# define A_AND 16		/* & */
# define A_OR 17		/* | */
# define A_WS 18		/* whitespace (not \n) */
# define A_NL 19		/* \n */

	/* character classes */

# define LEXLET 01
# define LEXDIG 02
# define LEXOCT 04
# define LEXHEX 010
# define LEXWS 020
# define LEXDOT 040

	/* reserved word actions */

# define AR_TY 0		/* type word */
# define AR_RW 1		/* simple reserved word */
# define AR_CL 2		/* storage class word */
# define AR_S 3		/* struct */
# define AR_U 4		/* union */
# define AR_E 5		/* enum */
# define AR_A 6		/* asm */

/* the following macro removes the need for the subroutine call to check
	if a sourcelisting is asked for */
# define XGETCHAR (*(++bufptr)? *bufptr : xgetchar()) 
# define XUNGETC(c,filedes)  (bufptr--)
char	*bufr;		 /* holds the lcurrent line */
char	*bufptr;
int sourcelist = 0;	/* flag to intersperse source with object */

extern int st_firstfile;	/* set if before first file */		/*!08*/
extern int st_procfile;
extern int st_fileidn;
extern int st_lineno;
extern	char *malloc();

	/* text buffer */
#ifdef FLEXNAMES
#	define LXTSZ BUFSIZ
#else
#	define LXTSZ 100
#endif
char yytext[LXTSZ];
char * lxgcp;
#ifdef FLEXNAMES
	int truncate_flag = 0;		/* force all names <= 8 chars. */
#endif
#ifdef CXREF
extern FILE *outfp;
#endif

char *st_filename=0;						/*!07*/
int  varargs = 0;
char	*myname;			/* how ccom was invoked for err msgs */

unsigned caloff();
	/* ARGSUSED */
# ifdef LINT
mainp1( argc, argv ) int argc; char *argv[]; {  /* control multiple files */
# else
main( argc, argv ) int argc; char *argv[]; {  /* control multiple files */
# endif
	register i, k;
	register int *ip;
	register d;							/*!11*/
	register char *cp;
	extern int idebug, bdebug, tdebug, edebug, ddebug, xdebug,
	           floatflag, singflag, /* floating-point precision flags */
		   udebug, gdebug, proflg, sign, volt, verboseflag, cflag,
		   dollar;
	extern unsigned int offsz;
	extern int wloop_level, floop_level;
	char *mksuf();							/*!03*/
	char *savefn();							/*!03*/
	int dexit();
	int fdef = 0;

	myname = argv[0];
	st_cuinit();							/*!07*/
	offsz = caloff();
	for( i=1; i<argc; ++i ){
		if( *(cp=argv[i]) == '-' && *++cp == 'X' ){
			while( *++cp ){
				switch( *cp ){


				case 'v':	/* make chars signed */
				      if( strcmp(cp, "volatile") ) 
					if( strcmp(cp, "varargs") ) 
					  if( strcmp(cp, "v" ) )
 					     cerror("bad option: %s", cp);
                                          else verboseflag = 1;
				        else {
					  cp +=6;
					  varargs++;
					}
				      else {
				        cp += 7;
				        volt++;
				      }
				      break;
				  
				 case 's':	/* make chars signed */
				      if( strcmp(cp, "signed") ) 
					  cerror("bad option: %s", cp);
				      cp += 5;
				      sign++;
				      break;

				 case 'f':/* use single precision */
				      if( strcmp(cp, "float") )
					  cerror("bad option: %s", cp);
				      cp += 4;
				      floatflag = singflag = 1;
				      break;

				case 'W':	/* test at top for 'while' */
					wloop_level = LL_TOP;
					break;
				case 'F':	/* test at top for 'for' */
					floop_level = LL_TOP;
					break;

			       case 'N': /* increase table limits */
				      switch(*++cp)
				      {
				      case 'a':
					ip = &strtabmax;
					goto getnum;
				      case 'b':
					ip = &itstrbufmax;
					goto getnum;
				      case 'c':
					ip = &tstrbufmax;
					goto getnum;
				      case 'd':
					ip = &stabmax;
					goto getnum;
				      case 'e':
					ip = &bnestmax;
					goto getnum;
				      case 'f':
					ip = &paramstkmax;
					goto getnum;
				      case 'g':
					ip = &swtabmax;
					goto getnum;
				      case 'h':
					ip = &nodemax;
					goto getnum;
				      case 'i':
					ip = &udeltreesmax;
					goto getnum;
				      case 'j':
					ip = &htabmax;
					goto getnum;
				      case 'k':
					ip = &file_name_length_max;
					goto getnum;
				      case 'l':
					ip = &strbufmax;
					goto getnum;
				      case 'm':
					ip = &instackmax;
					goto getnum;
				      case 'n':
					ip = &bufrmax;
					goto getnum;
				      case 'o':
					ip = &filestackmax;
					goto getnum;
				      case 'p':
					ip = &dimtabmax;
					goto getnum;
				      case 'q':
					ip = &asavbcmax;
					goto getnum;
				      default:
					fprintf(stderr,"ccom: options are:\n");
					fprintf(stderr,"\t-Na<number> - temporary string space[%d]\n",
					 BUFSIZ);
					fprintf(stderr,"\t-Nb<number> - temporary string space[%d]\n",
					 TSTRSZ);
					fprintf(stderr,"\t-Nc<number> - temporary string buffers[%d]\n",
					NTSTRBUF);
					fprintf(stderr,"\t-Nd<number> - symbol table space[%d]\n", SYMTSZ);
					fprintf(stderr,"\t-Ne<number> - nesting level[%d]\n",
						SYMTSZ/30);
					fprintf(stderr,"\t-Nf<number> - parameter stack space[%d]\n",
						PARAMSZ);
					fprintf(stderr,"\t-Ng<number> - switch table space[%d]\n",
						SWITSZ);
					fprintf(stderr,"\t-Nh<number> - tree space[%d]\n",
						TREESZ);
					fprintf(stderr,"\t-Ni<number> - delayed tree space[%d]\n",
						 DELAYS);
					fprintf(stderr,"\t-Nj<number> - hash table space[%d]\n",
						MAXHASH);
					fprintf(stderr,"\t-Nk<number> - file name space[%d]\n",
						FILE_NAME_LENGTH);
					fprintf(stderr,"\t-Nl<number> - string literal space[%d]\n",
						STRBUFLEN);
					fprintf(stderr,"\t-Nm<number> - initialization stack space[%d]\n",
						 INSTACKLEN);
					fprintf(stderr,"\t-Nn<number> - line length[%d]\n",
						 LINE_LENGTH);
					fprintf(stderr,"\t-No<number> - files tack size[%d]\n",
						 filestackmax);
					fprintf(stderr,"\t-Np<number> - dimension table size[%d]\n",
						 DIMTABSZ);
					fprintf(stderr,"\t-Nq<number> - block nesting size[%d]\n",
						 BCSZ);
					exit(1);
				}
				
				getnum:
				  k = 0;
				  while( isdigit(*++cp) )
				    k = 10*k + (*cp - '0');
				  if(k <= 0)
				   cerror("Table size too small");
				  *ip = k;
				  goto nextarg;

				case 'S': /* symbol table filename */
					st_filename = cp+1;
					goto nextarg;			/*!07*/

				case 'c':
				        ++cflag;
				        break;
				
				 case 'd':
				      if( strcmp(cp, "dollar") ) 
					if( strcmp(cp, "d" ) )	
					  cerror("bad option: %s", cp);
					else ddebug++;
				      else{
					cp += 5;
					dollar++;
				      }
				      break;

				case 'i':
					++idebug;
					break;
				case 'b':
					++bdebug;
					break;
				case 't':
					++tdebug;
					break;
				case 'e':
					++edebug;
					break;
				case 'x':
					++xdebug;
					break;
				case 'l':
					++sourcelist;
					break;
#ifdef FLEXNAMES
				case 'T':
					++truncate_flag;
					break;
#endif
				case 'u':				/*!05*/
					++udebug;
					break;

				case 'P':	/* profiling */
/*					++proflg;                         !10*/
					break;

				case 'g':				/*!11*/
				     ++cp;
				     gdebug = *cp - '0';
				     if( ( gdebug < 0) || ( gdebug > 3) )
				     cerror("bad debugging level: -g%d",gdebug);
				     break;
				}
			}
		}
# ifndef LINT
		if( *(argv[i]) != '-' ) { /* open input and output files */
			if( fdef < 2 ){
				if( freopen(argv[i], !fdef ?"r":"w", !fdef ?stdin:stdout) == NULL)
				{
					fprintf(stderr, "ccom:can't open %s\n", argv[i]);
					exit(1);
				}
				if( !fdef ) {
					register unsigned l = strlen(argv[i]);

					if( l > file_name_length_max) {
					  fprintf(stderr,
					  "filename %s too long; increase default option: -Wf,-XNk%d\n", argv[i], file_name_length_max);
					} else {

					  ftitle = malloc(sizeof(*ftitle) *
						    file_name_length_max);
					  strcpy(ftitle, argv[i]);

					}
				}
				++fdef;
				}
			}
# endif
nextarg:;							/*!07*/
		} /* end of for loop */

	if(signal( SIGHUP, SIG_IGN) != SIG_IGN) signal(SIGHUP, dexit);
	if(signal( SIGINT, SIG_IGN) != SIG_IGN) signal(SIGINT, dexit);
	if(signal( SIGTERM, SIG_IGN) != SIG_IGN) signal(SIGTERM, dexit);

	p2init( argc, argv );
	build_tables();
	for( i=0; i<stabmax; ++i ) stab[i].stype = TNULL;
	lxinit();
	tinit();
	mkdope();

	st_lineno = lineno;

 	if( verboseflag ) 
	  fprintf(stderr, "ccom:");
	init_ucode_writer( udebug, stdout );
	beg_file();							/*!07*/

	/* dimension table initialization */

	dimtab[NULL] = 0;
	dimtab[CHAR] = SZCHAR;
	dimtab[INT] = SZINT;
	dimtab[FLOAT] = SZFLOAT;
	dimtab[DOUBLE] = SZDOUBLE;
	dimtab[LONG] = SZLONG;
	dimtab[SHORT] = SZSHORT;
	dimtab[UCHAR] = SZCHAR;
	dimtab[USHORT] = SZSHORT;
	dimtab[UNSIGNED] = SZINT;
	dimtab[SIGNED] = SZINT;
	dimtab[ULONG] = SZLONG;
	dimtab[ENUMTY] = SZINT;						/*!06*/

	/* starts past any of the above */
	curdim = 16;
	reached = 1;
	reachflg = 0;

	yyparse();
	yyaccpt();

	ejobcode( nerrors ? 1 : 0 );
	exit(nerrors?1:0);

	}


/*
 * Mksuf() returns a pointer a created string that is made with name and the
 * suffix suf.  The name it returns is always the last componet of the filename.
 * It is a bug in the program if name has no suffix to start with.
 */
char *
mksuf(name, suf)
char *name;
char suf;
{
	int i;
	char *s, *p;
	char c;

	s = savefn(name);
	p = s;
	while(c = *p++)
		if(c == '/')
			i = 0;
		else
			i++;
	if(i > 2 && p[-3] == '.'){
		p[-2] = suf;
	}
	else{
		fprintf(stderr,
		 "%s: BUG! mksuf() passed a name with no suffix: %s\n",
		 ftitle, name);
		exit(1);
	}
	p = s;
	while(*s)
		if(*s++ == '/')
			p = s;
	return(p);
}



/*
 * Savefn() returns a pointer to a copy of the source file name s.
 */
char *
savefn(s)
char *s;
{
	char *p;

	if((p = malloc(strlen(s)+1)) == (char *) 0){
		fprintf(stderr, "%s: Out of memory (savefn)\n", ftitle);
		exit(1);
	}
	strcpy(p, s);
	return(p);
}


# ifdef ibm

# define CSMASK 0377
# define CSSZ 256

# else

# define CSMASK 0177
# define CSSZ 128

# endif

short lxmask[CSSZ+1];

lxenter( s, m ) register char *s; register short m; {
	/* enter a mask into lxmask */
	register c;

	while( c= *s++ ) lxmask[c+1] |= m;

	}


# define lxget(c,m) (lxgcp=yytext,lxmore(c,m))

lxmore( c, m )  register c, m; {
	register char *cp;

	*(cp = lxgcp) = c;
	while( c=XGETCHAR, lxmask[c+1]&m ){
		if( cp < &yytext[LXTSZ-1] ){
			*++cp = c;
			}
		}
	XUNGETC(c,stdin);
	*(lxgcp = cp+1) = '\0';
	}

struct lxdope {
	short lxch;	/* the character */
	short lxact;	/* the action to be performed */
	short lxtok;	/* the token number to be returned */
	short lxval;	/* the value to be returned */
	} lxdope[] = {

	'$',	A_ERR,	0,	0,	/* illegal characters go here... */
	'_',	A_LET,	0,	0,	/* letters point here */
	'0',	A_DIG,	0,	0,	/* digits point here */
	' ',	A_WS,	0,	0,	/* whitespace goes here */
	'\n',	A_NL,	0,	0,
	'"',	A_STR,	0,	0,	/* character string */
	'\'',	A_CC,	0,	0,	/* character constant */
	'`',	A_BCD,	0,	0,	/* GCOS BCD constant */
	'(',	A_1C,	LP,	0,
	')',	A_1C,	RP,	0,
	'{',	A_1C,	LC,	0,
	'}',	A_1C,	RC,	0,
	'[',	A_1C,	LB,	0,
	']',	A_1C,	RB,	0,
	'*',	A_1C,	MUL,	MUL,
	'?',	A_1C,	QUEST,	0,
	':',	A_1C,	COLON,	0,
	'+',	A_PL,	PLUS,	PLUS,
	'-',	A_MI,	MINUS,	MINUS,
	'/',	A_SL,	DIVOP,	DIV,
	'%',	A_1C,	DIVOP,	MOD,
	'&',	A_AND,	AND,	AND,
	'|',	A_OR,	OR,	OR,
	'^',	A_1C,	ER,	ER,
	'!',	A_NOT,	UNOP,	NOT,
	'~',	A_1C,	UNOP,	COMPL,
	',',	A_1C,	CM,	CM,
	';',	A_1C,	SM,	0,
	'.',	A_DOT,	STROP,	DOT,
	'<',	A_LT,	RELOP,	LT,
	'>',	A_GT,	RELOP,	GT,
	'=',	A_EQ,	ASSIGN,	ASSIGN,
	-1,	A_1C,	0,	0,
	};

struct lxdope *lxcp[CSSZ+1];

lxinit(){
	register struct lxdope *p;
	register i;
	register char *cp;
	/* set up character classes */

	if( dollar ) 
        lxenter( "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_$", LEXLET );
        else
        lxenter( "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ_", LEXLET );
	lxenter( "0123456789", LEXDIG );
	lxenter( "0123456789abcdefABCDEF", LEXHEX );
		/* \013 should become \v someday; \013 is OK for ASCII and EBCDIC */
	lxenter( " \t\r\b\f\013", LEXWS );
	lxenter( "01234567", LEXOCT );
	lxmask['.'+1] |= LEXDOT;

	/* make lxcp point to appropriate lxdope entry for each character */

	/* initialize error entries */

	for( i= 0; i<=CSSZ; ++i ) lxcp[i] = lxdope;

	/* make unique entries */

	for( p=lxdope; ; ++p ) {
		lxcp[p->lxch+1] = p;
		if( p->lxch < 0 ) break;
		}

	/* handle letters, digits, and whitespace */
	/* by convention, first, second, and third places */

	cp = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[1];
	cp = "123456789";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[2];
	cp = "\t\b\r\f\013";
	while( *cp ) lxcp[*cp++ + 1] = &lxdope[3];

	/* first line might have title */
	lxtitle();

	}

int lxmatch;  /* character to be matched in char or string constant */

lxstr(ct){
	/* match a string or character constant, up to lxmatch */

	register c;
	register val;
	register i;

	i=0;
	while( (c=XGETCHAR) != lxmatch ){
		switch( c ) {

		case EOF:
			/* "unexpected EOF" */
			UERROR( MESSAGE( 113 ) );
			break;

		case '\n':
			/* "newline in string or char constant" */
			UERROR( MESSAGE( 78 ) );
			++lineno;
#ifndef LINT
			if (!gdebug || st_procfile == st_currentifd())
			    st_lineno = lineno;
#endif
			break;

		case '\\':
			switch( c = XGETCHAR ){

			case '\n':
			++lineno;

#ifndef LINT
				if (!gdebug || st_procfile == st_currentifd())
				    st_lineno = lineno;
#endif
				continue;

			default:
				val = c;
				goto mkcc;

			case 'n':
				val = '\n';
				goto mkcc;

			case 'r':
				val = '\r';
				goto mkcc;

			case 'b':
				val = '\b';
				goto mkcc;

			case 't':
				val = '\t';
				goto mkcc;

			case 'f':
				val = '\f';
				goto mkcc;

			case 'v':
				val = '\013';
				goto mkcc;

			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				val = c-'0';
				c=XGETCHAR;  /* try for 2 */
				if( lxmask[c+1] & LEXOCT ){
					val = (val<<3) | (c-'0');
					c = XGETCHAR;  /* try for 3 */
					if( lxmask[c+1] & LEXOCT ){
						val = (val<<3) | (c-'0');
						}
					else XUNGETC( c ,stdin);
					}
				else XUNGETC( c ,stdin);

				goto mkcc1;

				}
		default:
			val =c;
		mkcc:
			val = CCTRANS(val);
		mkcc1:
			if( lxmatch == '\'' ){
			  /* it is, after all, a "character" constant */
			  val = CHARCAST(val);
			  if (sign) makecc(val,i);
			  else makeucc(val,i);
			}
			
			else { /* stash the byte into the string */

			  if( strflg ) {
					if( ct==0 || i<ct ) putbyte( val );
					/* "non-null byte ignored in string initializer" */
					else if( i == ct ) WERROR( MESSAGE( 81 ) );
					}
				else bycode( val, i );
				}
			++i;
			continue;
			}
		break;
		}
	/* end of string or  char constant */

	if( lxmatch == '"' ){
		if( strflg ){ /* end the string */
			if( ct==0 || i<ct ) putbyte( 0 );  /* the null at the end */
			}
		else {  /* the initializer gets a null byte */
			bycode( 0, i++ );				 
			bycode( -1, i );		
			dimtab[curdim] = i;  /* in case of later sizeof ... */
			}
		}
	else { /* end the character constant */
		/* "empty character constant" */
		if( i == 0 ) UERROR( MESSAGE( 36 ) );
		if( i>(SZINT/SZCHAR) || ( (pflag||hflag)&&i>1) )
			/* "too many characters in character constant" */
			UERROR( MESSAGE( 107 ) );
		}
	}

lxcom(){
	register c;
	/* saw a /*: process a comment */

	for(;;){

		switch( c = XGETCHAR ){

		case EOF:
			/* "unexpected EOF"  */
			UERROR( MESSAGE( 113 ) );
			return;

		case '\n':
			++lineno;
#ifndef LINT
			if (!gdebug || st_procfile == st_currentifd())
			    st_lineno = lineno;
#endif

		default:
			continue;

		case '*':
			if( (c = XGETCHAR) == '/' ) return;
			else XUNGETC( c ,stdin);
			continue;

# ifdef LINT
		case 'V':
			lxget( c, LEXLET|LEXDIG );
			{
				extern int vaflag;
				int i;
				i = yytext[7]?yytext[7]-'0':0;
				yytext[7] = '\0';
				if( strcmp( yytext, "VARARGS" ) ) continue;
				vaflag = i;
				continue;
				}
		case 'L':
			lxget( c, LEXLET );
			if( strcmp( yytext, "LINTLIBRARY" ) ) continue;
			{
				extern int libflag, vflag;
				libflag = 1;
				vflag = 0;
				}
			continue;

		case 'A':
			lxget( c, LEXLET );
			if( strcmp( yytext, "ARGSUSED" ) ) continue;
			{
				extern int argflag, vflag;
				argflag = 1;
				vflag = 0;
				}
			continue;

		case 'N':
			lxget( c, LEXLET );
			if( strcmp( yytext, "NOTREACHED" ) ) continue;
			reached = 0;
			{
			 /* 6/18/80 */
				extern int reachflg;
				reachflg = 1;
			}
			continue;
# endif
			}
		}
	}

yylex(){
	for(;;){

		register int lxchar;
		register struct lxdope *p;
		register struct symtab *sp;
		int id;

		lxchar = (SIGN char) XGETCHAR;
		if( lxchar < EOF) {
			/* "illegal character: %03o (octal)" */
			UERROR( MESSAGE( 51 ), 0377 & lxchar );
			continue;
		}
		p = lxcp[ lxchar + 1];
		switch( p->lxact ){

		onechar:
			XUNGETC( lxchar ,stdin);

		case A_1C:
			/* eat up a single character, and return an opcode */

			yylval.intval = p->lxval;
			return( p->lxtok );

		case A_ERR:
			if( lxchar == '$' && !dollar ) {
			  /* "illegal character: %03o (octal)" */
			  UERROR( MESSAGE( 51 ), 0377 & lxchar );
			  break;
			} /* A_LET must follow */
		case A_LET:
			/* collect an identifier, check for reserved word, and return */
			lxget( lxchar, LEXLET|LEXDIG );
			if( (lxchar=lxres()) > 0 ) return( lxchar ); /* reserved word */
			if( lxchar== 0 ) continue;
			if ( truncate_flag )
				yytext[8] = '\0';	/* truncate it */
			if( !is_prototype_id() ) {
			  id = lookup( hash( yytext ),
				/* tag name for struct/union/enum */
				(stwart&TAGNAME)? STAG:
				/* member name for struct/union */
				(stwart&(INSTRUCT|INUNION|FUNNYNAME))?SMOS:0 );
			sp = &stab[id];
			if( sp->sclass == TYPEDEF && !stwart ){
#ifdef CXREF
				ref(id, lineno);
#endif
				stwart = instruct;
				yylval.nodep = mkty( sp->stype, sp->dimoff, sp->sizoff );
				return( TYPE );
				}
			 stwart = (stwart&SEENAME) ? instruct : 0;
			} else id = 0;
			yylval.intval = id;
			return( NAME );

		case A_DIG:
			/* collect a digit string, then look at last one... */
			{
			CONSZ maxint;

			lastcon = 0;
			lxget( lxchar, LEXDIG );
			switch( lxchar=XGETCHAR ){

			case 'x':
			case 'X':
				/* "illegal hex constant"  */
				if( yytext[0] != '0' || yytext[1] ) UERROR( MESSAGE( 59 ) );
				lxmore( lxchar, LEXHEX );
				/* convert the value */
				{
					register char *cp;
					for( cp = yytext+2; *cp; ++cp ){
						/* this code won't work for all wild character sets,
						   but seems ok for ascii and ebcdic */
						lastcon <<= 4;
						if( isdigit( *cp ) ) lastcon += *cp-'0';
						else if( isupper( *cp ) ) lastcon += *cp - 'A'+ 10;
						else lastcon += *cp - 'a'+ 10;
						}
					}

			hexlong:
				/* criterion for longness for hex and octal
					constants is that it
				   does not fit within an int */
				if( (SZINT != SZLONG) &&
                                    (lastcon > ULONG_MAX || lastcon < 0) )
					yylval.intval = 1;
				else
					yylval.intval = 0;

				goto islong;

			case '.':
 			  lxmore( lxchar, LEXDIG );
	                  if( !( (lxchar=XGETCHAR) == 'e' || lxchar == 'E') ){
			    XUNGETC( lxchar ,stdin);
    			    return( isitfloat( yytext ) );
                          }  /* else get  exponent */


			case 'e':
			case 'E':
 			  if( (lxchar=XGETCHAR) == '+' || lxchar == '-' ){
			    *lxgcp++ = 'e';
			  }
			  else {
			    XUNGETC(lxchar,stdin);
			    lxchar = 'e';
			  }
	 		  lxmore( lxchar, LEXDIG );
			  /* now have the whole thing... */
    			  return( isitfloat( yytext ) );

			default:
				XUNGETC( lxchar ,stdin);
				if( yytext[0] == '0' ){
					/* convert in octal */
					register char *cp;
					for( cp = yytext+1; *cp; ++cp ){
						if( *cp > '7' )
							/* bad octal digit */
							WERROR( MESSAGE( 124 ), *cp );
						lastcon <<= 3;
						lastcon += *cp - '0';
						}
					goto hexlong;
					}
				else {
					/* convert in decimal */
					register char *cp;
					for( cp = yytext; *cp; ++cp ){ /*don't let lastcon overflow needlessly */
						lastcon *= (CONSZ)10;
						lastcon += (CONSZ)(*cp - '0');
						}
					}

				/* decide if it is long or not (decimal case) */

				/* if it fits in SZINT-1 bits it is int; otherwise long */
				/* if there is an l or L following, all bets are off... */
				maxint = ~( ~0L << (SZINT-1) );
				if( (SZINT != SZLONG) && (lastcon > maxint || lastcon < 0) )
					yylval.intval = 1;
				else
					yylval.intval = 0;


			islong:
				/* finally, look for trailing L or l */
				if( (lxchar = XGETCHAR) != 'L' && lxchar != 'l' )
					XUNGETC( lxchar ,stdin);
				else
					yylval.intval = 1;

				if( (SZINT != SZLONG) && !yylval.intval )
					lastcon = (int)lastcon;
				return( ICON );
				}
			} /* end of case A_DIG */

		case A_DOT:
			/* look for a dot: if followed by a digit, floating point */
			lxchar = XGETCHAR;
			if( lxmask[lxchar+1] & LEXDIG ){
			  XUNGETC(lxchar,stdin);
			  lxget( '.', LEXDIG );
			  if( (lxchar=XGETCHAR) == 'e' || lxchar == 'E' ){
			    /* exponent */
 			  if( (lxchar=XGETCHAR) == '+' || lxchar == '-' ){
			   *lxgcp++ = 'e';
		          }
			  else {
				XUNGETC(lxchar,stdin);
				lxchar = 'e';
			  }
				lxmore( lxchar, LEXDIG );
				/* now have the whole thing... */
			  }
			  else {  /* no exponent */
				XUNGETC( lxchar ,stdin);
			  }
				return( isitfloat( yytext ) );
			}
		        if( lxchar == '.' ) {
		          if(  (lxchar=XGETCHAR) == '.')  
			    return (ELLIPSIS);
			  UERROR( MESSAGE( 135 ), lxchar );
			  XUNGETC( lxchar ,stdin);
			  return (ELLIPSIS);
			}
			stwart = FUNNYNAME;
			goto onechar;

		case A_STR:
			/* string constant */
			lxmatch = '"';
			return( STRING );

		case A_CC:
			/* character constant */
			lxmatch = '\'';
			lastcon = 0;
			lxstr(0);
			yylval.intval = 0;
			return( ICON );

		case A_BCD:
			{
				register i;
				int j;
				for( i=0; i<LXTSZ; ++i ){
					if( ( j = XGETCHAR ) == '`' ) break;
					if( j == '\n' ){
						/* "newline in BCD constant" */
						UERROR( MESSAGE( 77 ) );
						break;
						}
					yytext[i] = j;
					}
				yytext[i] = '\0';
				/* "BCD constant exceeds 6 characters" */
				if( i>6 ) UERROR( MESSAGE( 10 ) );
# ifdef gcos
				else strtob( yytext, ( unsigned * )&lastcon,( unsigned) i );
				lastcon = ( unsigned ) lastcon >> 6*(6-i);
# else
				/* "gcos BCD constant illegal" */
				UERROR( MESSAGE( 48 ) );
# endif
				yylval.intval = 0;  /* not long */
				return( ICON );
				}

		case A_SL:
			/* / */
			if( (lxchar=XGETCHAR) != '*' ) goto onechar;
			lxcom();
		case A_WS:
			continue;

		case A_NL:
			++lineno;
#ifndef LINT
			if (!gdebug || st_procfile == st_currentifd())
			    st_lineno = lineno;
#endif
			lxtitle();
			continue;

		case A_NOT:
			/* ! */
			if( (lxchar=XGETCHAR) != '=' ) goto onechar;
			yylval.intval = NE;
			return( EQUOP );

		case A_MI:
			/* - */
			if( (lxchar=XGETCHAR) == '-' ){
				yylval.intval = DECR;
				return( INCOP );
				}
			if( lxchar != '>' ) goto onechar;
			stwart = FUNNYNAME;
			yylval.intval=STREF;
			return( STROP );

		case A_PL:
			/* + */
			if( (lxchar=XGETCHAR) != '+' ) goto onechar;
			yylval.intval = INCR;
			return( INCOP );

		case A_AND:
			/* & */
			if( (lxchar=XGETCHAR) != '&' ) goto onechar;
			return( yylval.intval = ANDAND );

		case A_OR:
			/* | */
			if( (lxchar=XGETCHAR) != '|' ) goto onechar;
			return( yylval.intval = OROR );

		case A_LT:
			/* < */
			if( (lxchar=XGETCHAR) == '<' ){
				yylval.intval = LS;
				return( SHIFTOP );
				}
			if( lxchar != '=' ) goto onechar;
			yylval.intval = LE;
			return( RELOP );

		case A_GT:
			/* > */
			if( (lxchar=XGETCHAR) == '>' ){
				yylval.intval = RS;
				return(SHIFTOP );
				}
			if( lxchar != '=' ) goto onechar;
			yylval.intval = GE;
			return( RELOP );

		case A_EQ:
			/* = */
			switch( lxchar = XGETCHAR ){

			case '=':
				yylval.intval = EQ;
				return( EQUOP );

			case '-':
			case '*':
			case '&':
				/* "ambiguous assignment: simple assign, unary op assumed" */
				WERROR( MESSAGE( 12 ) );
				goto onechar;

			case '+':
			case '/':
			case '%':
			case '|':
			case '^':
			case '<':
			case '>':
				/* "old style assign-op causes syntax error" */
				WERROR( MESSAGE( 126 ) );

			default:
				goto onechar;

				}

		default:
			cerror( "yylex error, character %03o (octal)", lxchar );

			}

		/* ordinarily, repeat here... */
		cerror( "out of switch in yylex" );

		}

	}

struct lxrdope {
	/* dope for reserved, in alphabetical order */

	char *lxrch;	/* name of reserved word */
	short lxract;	/* reserved word action */
	short lxrval;	/* value to be returned */
	} lxrdope[] = {

	"auto",		AR_CL,	AUTO,
	"break",	AR_RW,	BREAK,
	"char",		AR_TY,	UCHAR,
	"case",		AR_RW,	CASE,
	"const",	AR_TY,	CONST,
	"continue",	AR_RW,	CONTINUE,
	"double",	AR_TY,	DOUBLE,
	"default",	AR_RW,	DEFAULT,
	"do",		AR_RW,	DO,
	"extern",	AR_CL,	EXTERN,
	"else",		AR_RW,	ELSE,
	"enum",		AR_E,	ENUM,
	"for",		AR_RW,	FOR,
	"float",	AR_TY,	FLOAT,
	"goto",		AR_RW,	GOTO,
	"if",		AR_RW,	IF,
	"int",		AR_TY,	INT,
	"long",		AR_TY,	LONG,
	"return",	AR_RW,	RETURN,
	"register",	AR_CL,	REGISTER,		
	"switch",	AR_RW,	SWITCH,
	"struct",	AR_S,	0,
	"signed",	AR_TY,	SIGNED,
	"sizeof",	AR_RW,	SIZEOF,
	"short",	AR_TY,	SHORT,
	"static",	AR_CL,	STATIC,
	"typedef",	AR_CL,	TYPEDEF,
	"unsigned",	AR_TY,	UNSIGNED,
	"union",	AR_U,	0,
	"void",		AR_TY,	UNDEF, /* tymerge adds FTN */
	"volatile",	AR_TY,  VOL,					/*!07*/
	"while",	AR_RW,	WHILE,
	"",		0,	0,	/* to stop the search */
	};

lxres() {
	/* check to see of yytext is reserved; if so,
	/* do the appropriate action and return */
	/* otherwise, return -1 */

	register c, ch;
	register struct lxrdope *p;

	ch = yytext[0];

	if( !islower(ch) ) return( -1 );

	switch( ch ){

	case 'a':
		c=0; break;
	case 'b':
		c=1; break;
	case 'c':
		c=2; break;
	case 'd':
		c=6; break;
	case 'e':
		c=9; break;
	case 'f':
		c=12; break;
	case 'g':
		c=14; break;
	case 'i':
		c=15; break;
	case 'l':
		c=17; break;
	case 'r':
		c=18; break;
	case 's':
		c=20; break;
	case 't':
		c=26; break;
	case 'u':
		c=27; break;
	case 'v':
		c=29; break;
	case 'w':
		c=31; break;						/*!07*/

	default:
		return( -1 );
		}

	for( p= lxrdope+c; p->lxrch[0] == ch; ++p ){
		if( !strcmp( yytext, p->lxrch ) ){ /* match */
			switch( p->lxract ){

			case AR_TY:
				/* type word */
				stwart = instruct;
				if( sign ) {
				  if( p->lxrval == UCHAR )
				    p->lxrval = CHAR;
				}
				yylval.nodep = mkty( (TWORD)p->lxrval, 0, p->lxrval );
				return( TYPE );

			case AR_RW:
				/* ordinary reserved word */
				return( yylval.intval = p->lxrval );

			case AR_CL:
				/* class word */
				yylval.intval = p->lxrval;
				return( CLASS );

			case AR_S:
				/* struct */
				stwart = INSTRUCT|SEENAME|TAGNAME;
				yylval.intval = INSTRUCT;
				return( STRUCT );

			case AR_U:
				/* union */
				stwart = INUNION|SEENAME|TAGNAME;
				yylval.intval = INUNION;
				return( STRUCT );

			case AR_E:
				/* enums */
				stwart = SEENAME|TAGNAME;
				return( yylval.intval = ENUM );


			default:
				cerror( "bad AR_?? action" );
				}
			}
		}
	return( -1 );
	}

lxtitle(){
	/* called after a newline; set linenumber and file name */

	register c, val;
	register char *cp;

	for(;;){  /* might be several such lines in a row */
		if( (c=XGETCHAR) != '#' ){
			if( c != EOF ) XUNGETC(c,stdin);
			return;
			}

		lxget( ' ', LEXWS );
		val = 0;
		for( c=XGETCHAR; isdigit(c); c=XGETCHAR ){
			val = val*10+ c - '0';
			}
		XUNGETC( c, stdin );
		lxget( ' ', LEXWS );
		if( (c=XGETCHAR) != '\n' ){
			if( c == '$' ) {
#ifdef 0
			        process_pragma();
#endif
				for( c = XGETCHAR; c!='\n'; c=XGETCHAR );
	
			} else {
				for( cp=ftitle; c!='\n'; c=XGETCHAR ){
					if (c != '"') *cp++ = c;		/*!02*/
			}
			*cp = '\0';
		}
		if (!st_firstfile)
			beg_file ();					/*!07*/
#ifdef CXREF
			fprintf(outfp, "\"%s\"\n", ftitle);
#endif
			}
		lineno = val ;						/*!07*/
#ifndef LINT
		if (!gdebug || st_procfile == st_currentifd())
		    st_lineno = lineno;
#endif
		}
	}

# ifndef MYASMOUT

asmout()
	/* write out asm string
	 * this is a null function for lint
	 */
{
# ifndef LINT
# ifndef ONEPASS
	putchar(')');
# endif
	printf( "%s\n", asmbuf );
# endif
}

# endif

#ifdef FLEXNAMES


char *
savestr( cp )			

register char *cp;

{
    /* place string into permanent string storage. 
     *	If there's no room left in the
     *	current one, allocate another. Return a new string ptr.
     */

    register int	len;
    static int		istrtab = -1;
    static int		saveleft = 0;
    static char		**strtab;
    static char		*savetab;
    extern int 		strtabmax;

    if (!strtab) {
	strtab = (char **)malloc (strtabmax * sizeof(char **));
	if (!strtab)
		cerror ("cannot allocate strings\n");
    } /* if */

    len = strlen (cp) + 1;


    if (len > saveleft) {

	/* no room, allocate some more space */
	if (len > BUFSIZ)
		saveleft = len;
	else 
	    saveleft = BUFSIZ;

	if (++(istrtab) >= strtabmax)
		cerror ("too many symbol table strings to handle; increase default option: -Wf,-XNa%d\n", strtabmax);

	savetab = strtab[istrtab] = (char *) malloc(saveleft);

	if (savetab == 0)
		cerror("out of memory [savestr()]");

	strtab[istrtab] = savetab;
    }

    /* copy the string into the string table and return a ptr to it.
     */
    strncpy( savetab, cp, len );
    cp = savetab;
    savetab += len;
    saveleft -= len;
    return (cp);
} /* savestr */



#ifdef LINT
#	define LNCHNAM	8	/* length of symbols to check for pflag */
#endif

/*
* The segmented hash tables.
*/

#define HASHINC		1013
HT *htab;


char *
hash( s )	/* look for s in seg. hash tables.  Not found, make new entry */
	char *s;
{
	register char **h;
	register int i;
	register char *cp;
	HT *htp;
	int sh;
#ifdef LINT
	char *found = 0;	/* set once LNCHNAM chars. matched for name */
#endif

	/*
	* Hash on the correct number of characters.  Lint needs to be able
	* to limit this so that it can note length of names for portablility
	* concerns.
	*/
	cp = s;
	i = 0;
#ifdef LINT
	while ( *cp && ( ( cp - s ) < LNCHNAM ) )
#else
	while ( *cp )
#endif
	{
		i = ( i << 1 ) + *cp++;
	}
	sh = ( i & 077777 ) % HASHINC;
	cp = s;
	/*
	* Look through each table for name.  If not found in the current
	* table, skip to the next one.
	*/
	for ( htp = htab; htp < &htab[htabmax]; htp++ )
	{
		if ( htp->ht_low == 0 )
		{
			register char **hp = (char **) calloc(
				sizeof (char **), HASHINC );

			if ( hp == 0 )
				cerror( "out of memory [hash()]" );
			htp->ht_low = hp;
			htp->ht_high = hp + HASHINC;
		}
		h = htp->ht_low + sh;
		/*
		* Use quadratic re-hash
		*/
		i = 1;
		do
		{
			if ( *h == 0 )
			{
				if ( htp->ht_used > ( HASHINC * 3 ) / 4 )
					break;
				htp->ht_used++;
				*h = savestr( cp );
#ifdef LINT
				if ( pflag && found )
				{
					/*
					* If pflag set, then warn of greater
					* than LNCHNAM character names which
					* differ past the LNCHNAM'th character.
					*/
					/*
					* "`%s' may be indistinguishable from
					*  `%s' due to internal name truncation
					*/
					WERROR( MESSAGE( 128 ), *h, found );
				}
#endif
				return ( *h );
			}
#ifdef LINT
			if ( pflag )
			{
				if ( **h == *cp &&
					strncmp( *h, cp, LNCHNAM ) == 0 )
				{
					/*
					* We have matched on LNCHNAM chars.
					* Now, look for the ``total'' name.
					*/
					found = *h;
					if ( strcmp( *h, cp ) == 0 )
					{
						/*
						* This entry really is
						* the name we want.
						*/
						return ( *h );
					}
				}
			}
			else	/* No pflag - use entire name length */
			{
				if ( **h == *cp && strcmp( *h, cp ) == 0 )
					return ( *h );
			}
#else
			if ( **h == *cp && strcmp( *h, cp ) == 0 )
				return ( *h );
#endif
			h += i;
			i += 2;
			if ( h >= htp->ht_high )
				h -= HASHINC;
		} while ( i < HASHINC );
	}
	cerror( "out of hash tables" );
}
#endif


xgetchar()
{

	if (sourcelist && udebug &&
		(bufptr >= &bufr[2] && bufptr <= &bufr[514]))
			printf ("| C:	%s\n", &bufr[2]);

	bufr[1]= *bufptr;	/* for ungetc */
	if (bufptr > &bufr[0] && bufptr < &bufr [514]) {
		bufptr= &bufr[2];
		if (fgets (&bufr[2], 512, stdin) == NULL) {
		    bufptr[1]= 0;
		    *bufptr = EOF;
		    return EOF;
		} else {
		    return *bufptr;
		}
	};
	cerror ("bad input state");
}
