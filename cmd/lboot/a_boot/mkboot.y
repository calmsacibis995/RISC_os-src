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
/* $Header: mkboot.y,v 1.5.2.2 90/05/09 16:19:11 wje Exp $ */
/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

%{

# include	<stdio.h>
# include	"mkboot.h"

struct	flag {
	short	bits;
	short	vec;
};

static struct flag *cvtflag();
%}


%union	{
	char			word[DIRSIZ+1];
	char			*string;
	struct tnode		*tptr;
	struct format		*fptr;
	int			number;
	struct flag		flag;
	}

%token	<word>		HWORD
%token	<number>	NUMBER
%token	<string>	STRING DWORD ROUTINE

%type	<word>		header prefix
%type	<string>	routine
%type	<flag>		flag
%type	<number>	master
%type	<number>	software ndevice dependencies data type

%left	'+' '-'
%left	'*' '/'

%%

%{
static struct format *nextformat;	/* -> next format[] during initializer list processing */
%}

master		:		header data
				{
				/* master.magic = MMAGIC; */
				}
		;

header		:	flag prefix software ndevice dependencies
				{
				master.flag |= $1.bits;
				strncat( strcpy(master.prefix,""), $2, sizeof(master.prefix)-1 );
				master.ndep = $5;
				}
		|	flag prefix software ndevice
				{
				master.flag |= $1.bits;
				strncat( strcpy(master.prefix,""), $2, sizeof(master.prefix)-1 );
				master.ndep = 0;
				}
		|	flag prefix software
				{
				master.flag |= $1.bits;
				strncat( strcpy(master.prefix,""), $2, sizeof(master.prefix)-1 );
				master.ndev = 0;
				master.ndep = 0;
				}
		|	flag prefix
				{
				master.flag |= $1.bits;
				strncat( strcpy(master.prefix,""), $2, sizeof(master.prefix)-1 );
				master.soft = 0;
				master.ndev = 0;
				master.ndep = 0;
				}
		|	flag
				{
				master.flag |= $1.bits;
				master.prefix[0] = '\0';
				master.soft = 0;
				master.ndev = 0;
				master.ndep = 0;
				}
		;

flag		:	HWORD
				{
				struct flag *f;

				if ( (f=cvtflag($1)) == NULL )
					YYERROR;

				$$ = *f;
				}
		|	'-'
				{
				$$ = *cvtflag("none");
				}
		;

prefix		:	'-'
				{
				strcpy( $$, "" );
				}
		|	HWORD
				{
				if ( strlen($1) >= sizeof(master.prefix) )
					yyerror( "prefix \"%s\" truncated to %d characters", $1, sizeof(master.prefix)-1 );
				}
		;

software        :	software ',' NUMBER
                        {
			int xxx;

			for (xxx=0; xxx < MAX_MAJOR; xxx++)
			  if (master.msoft[xxx] == DONTCARE) break;
			master.msoft[xxx] = $3;
			}
                |	NUMBER
                        {
			master.soft = $1;
			master.msoft[0] = $1;
			}
                |	'-'
                        {
			master.soft = DONTCARE;
			$$ = DONTCARE;
		        }
                ;

ndevice		:	NUMBER ',' NUMBER
                                {
				master.ndev = $1;
				master.ncontmaj = $3;
				$$ = DONTCARE;
			        }
                |	'-' ',' NUMBER
                                {
				master.ndev = 0;
				master.ncontmaj = $3;
				$$ = DONTCARE;
			        }
                |	NUMBER ',' '-'
                                {
				master.ndev = $1;
				master.ncontmaj = DONTCARE;
				$$ = DONTCARE;
			        }
                |	NUMBER
                                {
				master.ndev = $1;
				master.ncontmaj = DONTCARE;
				$$ = DONTCARE;
			        }
                |	'-'
                                {
				master.ndev = 0;
				master.ncontmaj = DONTCARE;
				$$ = DONTCARE;
			        }
                ;


/*
 * <dependencies> will maintain the total number of struct depend elements
 */
dependencies	:	dependencies ',' HWORD
				{
				if ( ndepend-depend >= MAXDEP )
					{
					yyerror( "too many dependencies" );
					YYERROR;
					}

				if ( nstring-string >= MAXSTRING-strlen($3)-1 )
					{
					yyerror( "string table overflow" );
					YYERROR;
					}

				ndepend->name = Offset( strcpy(nstring,($3)), string );
				nstring += strlen(nstring) + 1;
				++ndepend;
				$$ = $1 + 1;
				}
		|	HWORD
				{
				if ( nstring-string >= MAXSTRING-strlen($1)-1 )
					{
					yyerror( "string table overflow" );
					YYERROR;
					}

				ndepend->name = Offset( strcpy(nstring,($1)), string );
				nstring += strlen(nstring) + 1;

				++ndepend;
				$$ = 1;
				}
		;

data		:	/* empty */
				{
				master.nrtn = 0;
				}
		|	data routine
				{
				++master.nrtn;
				++nroutine;
				}

routine		:	ROUTINE '{' type '}'
				{
				if ( nroutine-routine >= MAXRTN )
					{
					yyerror( "too many routine definitions" );
					YYERROR;
					}

				nroutine->id = $3;
				nroutine->name = Offset( $1, string );
				}
		;

/*
 * <type> is the routine type: RNULL, RNOSYS, RNODEV, etc.
 */
type		:	/* empty */
				{
				$$ = RNULL;
				}
		|	DWORD
				{
				static struct
					{
					char	*name;
					int	flag;
					}
					table[] ={
						{ "nulldev", RNULL },
						{ "nosys", RNOSYS },
						{ "nodev", RNODEV },
						{ "true", RTRUE },
						{ "false", RFALSE },
						{ "fsnull", RFSNULL },
						{ "fsstray", RFSSTRAY },
						{ "nopkg", RNOPKG},
						{ "noreach", RNOREACH},
						0
						};
				int i;

				lcase( $1 );

				for( i=0; table[i].name; ++i )
					if ( 0 == strcmp(table[i].name,$1) )
						break;

				if ( table[i].name )
					$$ = table[i].flag;
				else
					{
					yyerror( "unknown routine type \"%s\"", $1 );
					YYERROR;
					}
				}
		;

%% /*start of programs */

/*
 * yyfatal( message, a1, a2, a3, a4, a5 )
 *
 * Printf for fatal error messages; error message is:
 *
 *	mkboot: /etc/master: line 0, <message>
 */
 /*VARARGS1*/
 void
yyfatal( message, a1, a2, a3, a4, a5 )
	char *message;
	int	a1, a2, a3, a4, a5;
	{
	yyerror( message, a1, a2, a3, a4, a5 );

	if ( jmpbuf != NULL )
		longjmp( jmpbuf );
	else
	exit( 1 );
	}

/*
 * yyerror( message, a1, a2, a3, a4, a5 )
 *
 * Printf for warning error messages; error message is:
 *
 *	mkboot: /etc/master: line 0, <message>
 */
 /*VARARGS1*/
 void
yyerror( message, a1, a2, a3, a4, a5 )
	char *message;
	int	a1, a2, a3, a4, a5;
	{
	extern char master_file[];
	extern char *Argv;
	extern int yylineno;
	char buffer[256];

	any_error = TRUE;

	sprintf( buffer, message, a1, a2, a3, a4, a5 );

	fprintf( stderr, "%s: %s: line %d, %s\n",
			Argv, master_file, yylineno, buffer );
	}

/*
 * fatal( message, a1, a2, a3, a4, a5 )
 *
 * Printf for fatal error messages; error message is:
 *
 *	mkboot: <message>
 */
 /*VARARGS1*/
 void
fatal( message, a1, a2, a3, a4, a5 )
	char	*message;
	int	a1, a2, a3, a4, a5;
	{
	warn( message, a1, a2, a3, a4, a5 );

	if ( jmpbuf != NULL )
		longjmp( jmpbuf );
	else
		exit( 1 );
	}

/*
 * warn( message, a1, a2, a3, a4, a5 )
 *
 * Printf for warning error messages; error message is:
 *
 *	mkboot: <message>
 */
 /*VARARGS1*/
 void
warn( message, a1, a2, a3, a4, a5 )
	char	*message;
	int	a1, a2, a3, a4, a5;
	{
	extern	char	*Argv;
	char buffer[256];

	any_error = TRUE;

	sprintf( buffer, message, a1, a2, a3, a4, a5 );

	fprintf( stderr, "%s: %s\n", Argv, buffer );
	}

/*
 * This routine converts the alphanumeric string of flags
 * (from /etc/master) into the correct bit flags and vector number.
 */
 static
 struct flag*
cvtflag(string)
register  char  *string;
{
	register  char  	c;
	char			*f = string;
	char			once = FALSE;
	static struct flag	flag;

	flag.bits = 0;
	flag.vec = 0;

	if (strcmp(f,"none") == 0)
		return( &flag );

	while(c = *f++)
		switch (c) {
		case 'k':
			flag.bits |= KOBJECT;
			break;
		case 'o':
			flag.bits |= ONCE;
			break;
		case 'r':
			flag.bits |= REQ;
			break;
		case 'b':
			flag.bits |= BLOCK;
			break;
		case 'c':
			flag.bits |= CHAR;
			break;
		case 'a':
			flag.bits |= REQADDR;
			break;
		case 's':
			flag.bits |= SOFT;
			break;
		case 't':
			flag.bits |= TTYS;
			break;
		case 'x':
			flag.bits |= NOTADRV;
			break;
		case 'f':
			flag.bits |= FUNDRV;
			break;
		case 'm':
			flag.bits |= FUNMOD;
			break;
		case 'j':
			flag.bits |= FSTYP;
			break;
		case '0':
		case '1':
		case '2':
		case '3':
		case '4':
		case '5':
		case '6':
		case '7':
		case '8':
		case '9':
			if ( once )
				{
				yyerror( "multiple vector numbers in \"%s\"", string );
				return( NULL );
				}
			once = TRUE;
			flag.vec = strtol( f-1, &f, 0 );
			break;
		default:
			yyerror( "illegal device flags \"%s\"", string );
			return( NULL );
		}
	return( &flag );
}


# include "lex.yy.c"


/*
 * reset lex to its initial state prior to each file
 */
lexinit()
	{
	BEGIN INITIAL;
	NLSTATE;
	yymorfg = 0;
	yysptr = yysbuf;
	}
