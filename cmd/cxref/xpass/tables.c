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
#ident	"$Header: tables.c,v 1.1.1.2 90/05/09 15:39:00 wje Exp $"
/* $Log:	tables.c,v $
 * Revision 1.1.1.2  90/05/09  15:39:00  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:20:25  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:43:18  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:51  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:52  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:42:22  gb
 * added $Log keyword
 *  */
# include <stdio.h>
# include "tables.h"
# include "mfile1"

/*
** temporary string buffers
*/
char	*itstrbuf;
char	**tstrbuf;
char	**curtstr;
NODE    *first;

extern struct symtab **schain; 	/* sym chains for clearst */
extern long *msizoff_idn;	/* map sizoff indexes into iaux */
extern  int *paramstk;		/* used in the definition of function
				   parameters */
extern struct sw *swtab;	/* table for cases within a switch */
extern struct sw *heapsw;	/* heap for switches */
extern NODE *node;
extern int *asavbc;
extern int *psavbc;
extern int *bb_flags;
extern NODE **udeltrees;
extern HT *htab;
extern char *ftitle;
extern char *strbuf;
extern INSTK *instack;
extern char *bufr;
extern char *bufptr;
extern long *filestack;		/* follows include files */

/* 
** default table sizes
*/
int	strtabmax   = BUFSIZ;   /* maximum size of string table */
int     itstrbufmax = TSTRSZ;   /* maximum size of temporary strings */
int     tstrbufmax  = NTSTRBUF; /* number of string buffers */
int	stabmax     = SYMTSZ; 
int     bnestmax    = SYMTSZ/30;
int     dimtabmax   = DIMTABSZ;
int	paramstkmax = PARAMSZ; 
int	swtabmax    = SWITSZ;
int     nodemax     = TREESZ;
int	asavbcmax   = BCSZ;
int	udeltreesmax= DELAYS;
int	htabmax	    = MAXHASH;
int     file_name_length_max = FILE_NAME_LENGTH;
int     strbufmax   = STRBUFLEN;
int     instackmax    = INSTACKLEN;
int	bufrmax      = LINE_LENGTH;
int	filestackmax = 1024;


/*
** build tables with either default sizes or sizes specified by users
*/
static char *uc_alloc(n,s)
     int n,s;
{
  extern char *calloc();	/* Use calloc because some routines */
				/* expect arrays initialized to 0 */
  register char *mem;
  mem = calloc(n,s);
  if (!mem)
    cerror("build_tables out of memory");
  return mem;
}

void
build_tables()
{
	itstrbuf   = uc_alloc(itstrbufmax, sizeof( *itstrbuf )) ;
	tstrbuf    = (char **)uc_alloc(tstrbufmax, sizeof( *tstrbuf ) );
	tstrbuf[0] =  itstrbuf;
	curtstr    = tstrbuf;
	stab       = (struct symtab *) uc_alloc(stabmax, sizeof(*stab));
	if( bnestmax == SYMTSZ/30 ) {
		bnestmax   = stabmax/30;
	} /* else user enlarged it */
	dimrec     = (struct dnode *) uc_alloc(bnestmax, sizeof(*dimrec));
	dimptr     =  &dimrec[-1];
	schain     = (struct symtab **)uc_alloc(bnestmax, sizeof( *schain ));
	dimtab     = (int *)uc_alloc( dimtabmax,sizeof(*dimtab));
	msizoff_idn= (long *)uc_alloc(dimtabmax , sizeof(*msizoff_idn));
	paramstk   = (int*)uc_alloc(paramstkmax,sizeof(*paramstk));
	swtab	   = (struct sw *)uc_alloc(swtabmax,sizeof(*swtab));
	heapsw	   = (struct sw *)uc_alloc(swtabmax,sizeof(*heapsw));
	node       = (NODE *)uc_alloc( nodemax,sizeof(*node));
	first	   = node;
	asavbc     = (int *)uc_alloc(asavbcmax,sizeof(*asavbc));
	psavbc	   = asavbc;
	bb_flags   = (int *)uc_alloc( asavbcmax,sizeof(*asavbc));
	udeltrees  = (NODE **)uc_alloc( udeltreesmax,sizeof(*udeltrees));
	htab       = (HT *)uc_alloc( htabmax,sizeof(*htab));
	strbuf     = uc_alloc(  strbufmax,sizeof( strbuf));
	instack    = (INSTK *)uc_alloc(instackmax,sizeof(*instack));
        if( ftitle == NULL )
	  ftitle = uc_alloc(  file_name_length_max,sizeof(*ftitle));
	bufr	   = uc_alloc( bufrmax,sizeof(*bufr) );
	bufptr	   = bufr;
	filestack = (long *)uc_alloc(filestackmax,sizeof(*filestack));
}
