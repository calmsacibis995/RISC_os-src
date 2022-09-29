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
#ident	"$Header: master.c,v 1.7.2.2 90/05/09 16:19:05 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

# include	<stdio.h>
# include	"mkboot.h"
# include	<ctype.h>
# include	<a.out.h>
# include	<fcntl.h>
# include	<errno.h>

extern	FILE		*yyin;		/* LEX's input stream */

jmp_buf			*jmpbuf;
char			any_error = FALSE;

struct master		master, *Opthdr, *copy_master();
struct depend		depend[MAXDEP], *ndepend;
struct routine		routine[MAXRTN], *nroutine;
char			string[MAXSTRING], *nstring;

char master_file[1024];	/* current master file entity */
extern char Debug;
extern char *master_dot_d;	/* master database directory */
char *Argv;

/*
 * Process a driver master file.
 */
 struct master *
mkboot(module)
char *module;
{
	extern int yylineno;
	int size;
	jmp_buf	env[1];
	char *p;
#ifdef MULTIPLE_MAJOR
	int mcnt;
#endif

	Argv = module;

	master.flag = 0;

#ifdef MULTIPLE_MAJOR
	for (mcnt=0; mcnt < MAX_MAJOR; mcnt++)
	  master.msoft[mcnt] = DONTCARE;	/* Initialize to known state */
#endif

	/* allocate memory to build optional header for driver */
	Opthdr = (struct master *) malloc(	sizeof(master) +
						sizeof(depend) +
						sizeof(routine) +
						sizeof(string) );
	
	if (Opthdr == NULL) {
		warn("cannot malloc memory to build master for %s", master_file);
		exit(2);
	}

	ndepend = depend;
	nroutine = routine;
	nstring = &string[1];	/* so Offset can never be NULL */

	/*
	 * scan /etc/master.d to find the configurable declarations
	 */
	strcpy( master_file, master_dot_d );
	strcat( master_file, "/" );
	strcat( master_file, module );


	if ((yyin = fopen(master_file,"r")) == NULL)
		{
		warn( "cannot open master file %s", master_file );
		free(Opthdr);
		return( (struct master *)0 );
		}

	if ( setjmp(jmpbuf=env) ) {
		jmpbuf = NULL;
		fclose( yyin );
		warn( "%s: not processed", module );
		return( (struct master *)0 );
	}
	
	/*
	 *	Start parsing master file from the beginning
	 */
/*
	rewind( yyin );
*/

	lexinit();

	yylineno = 1;

	if ( yyparse() == 0 && check_master(module,&master) == TRUE ) {
		/*
		 * build the new optional header
		 */
		size = build_header();

		if ( Debug )
			print_master( module, Opthdr );

		fclose(yyin);
		return( copy_master( Opthdr, size ) );
	} else {
		warn( "%s: not processed", module );
		fclose(yyin);
		return( (struct master *)0 );
	}
}


/*
 * copy_driver( opthdr, size )
 *
 * Copy the parsed master file to a smaller area.
 * Free opthdr.
 */
 struct master *
copy_master( opthdr, size )
struct master *opthdr;
int	size;
{
	struct master *Hdr;
	Hdr = (struct master *) malloc( size );
	if ( Hdr == NULL ) {
		warn( "out of memory" );
		exit( 2 );
	}
	memcpy( Hdr, opthdr, size );
	free( opthdr );
	return( Hdr );
}

/*
 * build_header()
 *
 * Build the new optional header; return the total size
 */

struct	sort {
	char	*string;	/* -> the string or expression */
	offset	*referenced;	/* -> the offset that references the string or expression */
};

 static
 int
cmpstrlen( s1, s2 )
struct sort *s1, *s2;
{
	return( strlen(s2->string) - strlen(s1->string) );
}


 int
build_header()
{
	struct	depend	*dp;
	struct	routine	*rp;
	char		*xp, *p, *q, *s;
	int		size = 0;
	int		i, j, k;
	struct sort	*sorted;

	/*
	 * copy master structure information
	 */
	*Opthdr = master;

	/*
	 * copy dependencies
	 *
	 * dp points to the word boundary one past the end
	 * of the master struct pointed to by Opthdr
	 */
	dp = (struct depend *) ROUNDUP(Opthdr+1);

	if (Opthdr->ndep > 0) {
		Opthdr->o_depend = Offset(dp,Opthdr);

		for (i=0; i<Opthdr->ndep; i++) {
			dp[i] = depend[i];
		}
	}
	else
		Opthdr->o_depend = NULL;

	
	/*
	 * copy routine information
	 *
	 * rp points to the word boundary one past the end
	 * of the last dependency structure
	 */
	rp = (struct routine *) ROUNDUP(dp+(Opthdr->ndep));

	if (Opthdr->nrtn > 0) {
		Opthdr->o_routine = Offset(rp,Opthdr);

		/*                             routine name */
		for ( i=0; i<Opthdr->nrtn; i++ )
			rp[i] = routine[i];
	}
	else
		Opthdr->o_routine = NULL;

	/*
	 * xp points to memory beyond the last routine structure
	 */

	xp =(char *) ROUNDUP(rp+(Opthdr->nrtn));

	/*
	 * copy the dependency strings and build a string table;
	 * this is done in a way to minimize the size of the string table
	 *
	 *	1. extract dependency strings
	 *	2. sort into decreasing order by length
	 *	3. for each string, search the string table for an exact
	 *		match, and if found use that one, otherwise, copy
	 *		the unique string into the new string table. 
	 *
	 */
	
	if (Opthdr->ndep) {
	
		if ( (sorted=(struct sort*)malloc(Opthdr->ndep*sizeof(*sorted))) == NULL )
			fatal( "no memory for building string table" );

		i = 0;

		for( j=0; j<Opthdr->ndep; ++j ) {
			sorted[i].string = (char*)POINTER(dp[j].name,string);
			sorted[i++].referenced = &dp[j].name;
		}

		qsort( sorted, Opthdr->ndep, sizeof(*sorted), cmpstrlen );

		for( i=0; i<Opthdr->ndep; ++i ) {
			k = strlen( s=sorted[i].string );

			/* search the string table for a duplicate */
			for( p=xp, q=xp+size-k; p < q; ++p ) {
				if ( 0 == strcmp(p,s) )
					break;
			}

			if ( p < q )
				/* its already in the new string table */
				*sorted[i].referenced = Offset(p,Opthdr);
			else {
				/* copy the unique string into the new string table */
				*sorted[i].referenced = Offset(strcpy(xp+size,s),Opthdr);
				size += k+1;
			}
		}

		free( sorted );
	}

	/*
	 * copy the routine strings and build a string table;
	 * cannot make the same minimizations as dependency string table,
	 * because dependency strings may be stubbed out "in situ".
	 */
	
	for( j=0; j<Opthdr->nrtn; ++j ) {
		k = strlen(s=POINTER(rp[j].name,string));
		rp[j].name = Offset(strcpy(xp+size,s),Opthdr);
		size += k+1;
	}

	return( ROUNDUP(Offset(xp+size, Opthdr)) );
}
