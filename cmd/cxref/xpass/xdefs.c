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
#ident	"$Header: xdefs.c,v 1.5.2.3 90/05/09 15:39:28 wje Exp $"

/* $Log:	xdefs.c,v $
 * Revision 1.5.2.3  90/05/09  15:39:28  wje
 * add restricted rights legend
 * 
 * Revision 1.5.2.2  89/12/10  22:27:53  wje
 * add RID 1.6 to branch RISCOS_4_50_FIXES
 * 
 * Revision 1.6  89/12/10  18:43:45  jay
 * replaced with files from 2.0 C compiler's lint, modified to support cxref.
 * closes bugs 1373
 * 
 * Revision 2000.7  89/03/26  14:48:31  bettina
 * 2.0G
 * 
 * Revision 1040.6  88/09/29  15:53:28  bettina
 * bootstraped 1.40
 * 
 * Revision 1.2  87/12/09  11:42:36  gb
 * added $Log keyword
 *  */


/* ref.		date		description				*/
/* !01		19mar85		don't need surrounding double quotes    */
/* !02		09apr85		implementing initialization		*/
/* !03		31may85		added flag that indicates byte-sex of target */

# include "mfile1"

/*	communication between lexical routines	*/

char	*ftitle;		/* title of the file */
int	lineno = 1;		/* line number of the input file */

CONSZ lastcon;  /* the last constant read by the lexical analyzer */
double dcon;   /* the last double read by the lexical analyzer */



/*	symbol table maintainence */

struct symtab *stab;

int	curftn;  /* "current" function */
int	ftnno;  /* "current" function number */
int	curclass,	  /* current storage class */
	instruct,	/* "in structure" flag */
	stwart,		/* for accessing names which are structure members or names */
	blevel,		/* block level: 0 for extern, 1 for ftn args, >=2 inside function */
	curdim;		/* current offset into the dimension table */
	
int	*dimtab;


struct dnode *dimrec;

struct dnode *dimptr;

int	*paramstk;  /* used in the definition of function parameters */
int	paramno;	  /* the number of parameters */
int	autooff,	/* the next unused automatic offset */
	argoff,	/* the next unused argument offset */
	strucoff;	/*  the next structure offset position */
int	regvar;		/* the next free register for register variables */
int	minrvar;	/* the smallest that regvar gets witing a function */
OFFSZ	inoff;		/* offset of external element being initialized */
int	indblkno;	/* data block number of element being initialized !02*/
int	brkflag = 0;	/* complain about break statements not reached */
int     endian;	/* indicates byte_sex of target, runtime set in reader.c  !03*/



struct sw *swtab;	  /* table for cases within a switch */
struct sw *swp;  /* pointer to next free entry in swtab */
int swx;  /* index of beginning of cases for current switch */

/* debugging flag */
int xdebug = 0;

int strflg;  /* if on, strings are to be treated as lists */

int reached;	/* true if statement can be reached... */
int reachflg;	/* true if NOTREACHED encountered in function */

int idname;	/* tunnel to buildtree for name id's */

/* Formal type for current argument of a function call whose prototype   */
/* is in scope. If the function doesn't have a prototype in scope the    */
/* value is NULL. 						         */
function_prototype_list  curprototype;


NODE *node;

int cflag = 0;  /* do we check for funny casts */
int hflag = 0;  /* do we check for various heuristics which may indicate errors */
int pflag = 0;  /* do we check for portable constructions */

int brklab;
int contlab;
int flostat;
int retlab = NOLAB;
int retstat;

/* save array for break, continue labels, and flostat */

int *asavbc;
int *psavbc;


# ifndef BUG1
static char *
ccnames[] = { /* names of storage classes */
	"SNULL",
	"AUTO",
	"EXTERN",
	"STATIC",
	"REGISTER",
	"EXTDEF",
	"LABEL",
	"ULABEL",
	"MOS",
	"PARAM",
	"STNAME",
	"MOU",
	"UNAME",
	"TYPEDEF",
	"FORTRAN",
	"ENAME",
	"MOE",
	"UFORTRAN",
	"USTATIC",
	};

char * scnames( c ) register c; {
	/* return the name for storage class c */
	static char buf[12];
	if( c&FIELD ){
		sprintf( buf, "FIELD[%d]", c&FLDSIZ );
		return( buf );
		}
	return( ccnames[c] );
	}
# endif

