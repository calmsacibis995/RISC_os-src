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
#ident	"$Header: pftn.c,v 1.5.2.3 90/05/09 15:37:50 wje Exp $"
/* $Log:	pftn.c,v $
 * Revision 1.5.2.3  90/05/09  15:37:50  wje
 * add restricted rights legend
 * 
 * Revision 1.5.2.2  89/12/10  22:26:37  wje
 * add RID 1.6 to branch RISCOS_4_50_FIXES
 * 
 * Revision 1.6  89/12/10  18:42:33  jay
 * replaced with files from 2.0 C compiler's lint, modified to support cxref.
 * closes bugs 1373
 * 
 * Revision 2000.7  89/03/26  14:48:20  bettina
 * 2.0G
 * 
 * Revision 1.3  87/12/16  14:30:27  gb
 * fix bug #1936, a core dump caused by a dimensionless array initialization.
 * 
 * 
 * Revision 1.2  87/12/09  11:41:59  gb
 * added $Log keyword
 *  */
# include "mfile1"
# include "messages.h"
# include "cmplrs/usys.h"						/*!01*/
# include "cmplrs/ucode.h"						/*!01*/
# include "symconst.h"							/*!07*/
# include "sym.h"							/*!07*/
# include "cmplrs/stsupport.h"						/*!07*/
# include "sex.h"							/*!15*/
# include "tables.h"

/* ref.		date		description		*/
/* !01		20mar85		implement import and export UCODES */
/* !02		09apr85		initialization				    */
/* !03		15apr85		ignoring register variables; optimizer will
				allocate.				    */
/* !04		19apr85		adjust parameter offset for implicit 
				destination address.			    */
/* !05		30apr85		mark register varibles and temporaries      */
/* !06		20may85		introduced volatile			    */
/* !07		04jun85		symbol table				    */
/* !08		12jun85		prevented marking structs or unions
				as VREG					    */
/* !09		11jul85		put actual size of object in length field   */
/* !10		30jul85		added data type field to Uvreg		    */
/* !11		19aug85		added to !08, prevent arrays from being     */
/*				allocated to registers.			    */
/* !12		29aug85		fixed wrong array declaration, wasn't enough*/
/*				elements declared, e.g. int t[3], t[3] = 4  */
/* !13		30aug85		only unsigned int's bit field supported	    */
/* !14		20sep85		added field to VREG marking it as a compiler*/
/*				temporary if it is true.		    */
/* !15		27jan85		adjustment of address for big endian machines*/
/*				for parameters with size less than an int.  */

extern unsigned int offsz;
struct symtab **schain;    	/* sym chains for clearst */
int chaintop;				/* highest active entry */
extern int cblkno;							/*!05*/
extern int gdebug;							/*!07*/
extern int endian;							/*!15*/

INSTK *instack, *pstk;

	/* defines used for getting things off of the initialization stack */


struct symtab *relook();

int ddebug = 0;

struct symtab * mknonuniq();

defid( q, class )  NODE *q; {
	register struct symtab *p;
	int idp;
	TWORD type;
	TWORD stp;
	int scl;
	int dsym, ddef;
	int slev, temp;
	int freenter = 0;					/*!07*/
        function_prototype_list fp;

	if( q == NIL ) return;  /* an error was detected */
	idp = q->tn.rval;
	fp  = q->tn.prototype;

	if( idp < 0 ) cerror( "tyreduce" );
	p = &stab[idp];

# ifndef BUG1
	if( ddebug ){
#ifdef FLEXNAMES
		printf( "defid( %s (%d), ", p->sname, idp );
#else
		printf( "defid( %.8s (%d), ", p->sname, idp );
#endif
		tprint( q->in.type );
		printf( ", %s, (%d,%d) ), level %d\n", scnames(class), q->fn.cdim, q->fn.csiz, blevel );
		}
# endif

	fixtype( q, class );

	type = q->in.type;
	class = fixclass( class, type );

	stp = p->stype;
	slev = p->slevel;

# ifndef BUG1
	if( ddebug ){
		printf( "\tmodified to " );
		tprint( type );
		printf( ", %s\n", scnames(class) );
		printf( "\tprevious def'n: " );
		tprint( stp );
		printf( ", %s, (%d,%d) ), level %d\n", scnames(p->sclass), p->dimoff, p->sizoff, slev );
		}
# endif

	if( ISEQUAL(stp, FTN )&& p->sclass == SNULL )goto enter;	/*!07*/
		/* name encountered as function, not yet defined */
	if( stp == UNDEF|| ISEQUAL(stp, FARG) ){
		if( blevel==1 && ! ISEQUAL(stp,FARG) ) switch( class ){

		default:
			/* "declared argument %.8s is missing" */
			/* "declared argument %s is missing" */
			if(!(class&FIELD)) UERROR( MESSAGE( 28 ), p->sname );
		case MOS:
		case STNAME:
		case MOU:
		case UNAME:
		case MOE:
		case ENAME:
		case TYPEDEF:
			;
			}
		goto enter;
		}

	if( type != stp)  goto mismatch;
	if( fp && p->sprototype ) { 
	  /* see if both protototypes are identical */
	  if( !prototype_lists_equal(fp, p->sprototype)) {
 	  	UERROR( MESSAGE( 136 ), p->sname);	   
	  } else {
		if( has_prototype_ids( fp ) ) {
		/* keep ids until we know p is not a function defintion */
	  		free_prototype_list( p->sprototype );
			p->sprototype = fp;
	        } else {
		 	free_prototype_list( fp );
		}
	  }
	} else
	if( !fp && p->sprototype && (class != EXTDEF) && (class != STATIC)) {
	    /* backward compatibility */
 	    WERROR( MESSAGE( 136 ), p->sname);
        }

    	/* test (and possibly adjust) dimensions */
	dsym = p->dimoff;
	ddef = q->fn.cdim;
	for( temp=type; temp&TMASK; temp = DECREF(temp) ){
		if( ISARY(temp) ){
			if( dimtab[dsym] == 0 ) dimtab[dsym] = dimtab[ddef];
			else if( dimtab[ddef]!=0 && dimtab[dsym] != dimtab[ddef] ){
				goto mismatch;
				}
			++dsym;
			++ddef;
			}
		}

	/* check that redeclarations are to the same structure */
	if( (ISEQUAL(temp,STRTY)||ISEQUAL(temp,UNIONTY)||
	     ISEQUAL(temp,ENUMTY)) &&
     	    p->sizoff != q->fn.csiz && class!=STNAME && class!=UNAME &&
	    class!=ENAME ){
		goto mismatch;
		}

	scl = ( p->sclass );

# ifndef BUG1
	if( ddebug ){
		printf( "\tprevious class: %s\n", scnames(scl) );
		}
# endif

	if( class&FIELD ){
		/* redefinition */
		if( !falloc( p, class&FLDSIZ, 1, NIL ) ) {
			/* successful allocation */
			psave( idp );
			if (gdebug)
				st_def (p, freenter);
			return;
			}
		/* blew it: resume at end of switch... */
		}

	else switch( class ){

	case EXTERN:
		switch( scl ){
		case STATIC:
		case USTATIC
:
			if( slev==0 )
			    return;
			break;
		case EXTDEF:
		case EXTERN:
		case FORTRAN:
		case UFORTRAN:
			return;
			}
		break;

	case STATIC:
		if( scl==USTATIC || (scl==EXTERN && blevel==0) ){
			p->sclass = STATIC;
			if( ISFTN(type) ) {
			  curftn = idp;
			}
			p->offset = st_def(p, p->offset);		/*!07*/
			return;
			}
		break;

	case USTATIC:
		if( scl==STATIC || scl==USTATIC ) {
			    return;
		};							/*!07*/
		break;

	case LABEL:
		if( scl == ULABEL ){
			p->sclass = LABEL;
			if (gdebug)
			    defnamedlab( p->offset, st_def(p, 0) );	/*!07*/
			else
			    deflab ( p->offset );
			return;
			}
		break;

	case TYPEDEF:
		if( scl == class ) {
			    return;
			}						/*!07*/
		break;

	case UFORTRAN:
		if( scl == UFORTRAN || scl == FORTRAN ) {
			    return;
			}						/*!07*/
		break;

	case FORTRAN:
		if( scl == UFORTRAN ){
			p->sclass = FORTRAN;
			if( ISFTN(type) ) curftn = idp;
			p->offset = st_def(p, p->offset);				/*!07*/
			return;
			}
		break;

	case MOU:
	case MOS:
		if( scl == class ) {
			if( oalloc( p, &strucoff ) ) break;
			if( class == MOU ) strucoff = 0;
			psave( idp );
			if (gdebug)
			    st_def (p, freenter);
			return;
			}
		break;

	case MOE:
		if( scl == class ){
			if( p->offset!= strucoff++ ) break;
			psave( idp );
			if (gdebug)
			    st_def (p, freenter);
			}
		break;

	case EXTDEF:
		if( scl == EXTERN ) {
			p->sclass = EXTDEF;
			if( ISFTN(type) ) curftn = idp;
			p->offset = st_def(p, p->offset);				/*!07*/
			return;
			}
		break;

	case STNAME:
	case UNAME:
	case ENAME:
		if( scl != class ) break;
		if( dimtab[p->sizoff] == 0 ) {
			/* previous entry just a mention */
			    if (gdebug)
				st_def(p, freenter);
			    return;
			}						/*!07*/
		break;

	case ULABEL:
		if( scl == LABEL || scl == ULABEL ) return;

	case PARAM:
	case AUTO:
	case REGISTER:
		;  /* mismatch.. */

		}

	mismatch:
	/* allow nonunique structure/union member names */

	if( class==MOU || class==MOS || class & FIELD ){/* make a new entry */
		int * memp;
		p->sflags |= SNONUNIQ;  /* old entry is nonunique */
		/* determine if name has occurred in this structure/union */
        	if (paramno == 0) 
			cerror("paramstk error");
		for( memp = &paramstk[paramno-1];
			/* while */ *memp>=0 && stab[*memp].sclass != STNAME
				&& stab[*memp].sclass != UNAME;
			/* iterate */ --memp){ char * cname, * oname;
			if( stab[*memp].sflags & SNONUNIQ ){int k;
				if ( p->sname != stab[*memp].sname )
					continue;
				/* "redeclaration of %s" */
				UERROR(MESSAGE( 96 ),p->sname);
				break;
				}
			}
		p = mknonuniq( &idp ); /* update p and idp to new entry */
		goto enter;
		}
	if( blevel > slev && class != EXTERN && class != FORTRAN &&
		class != UFORTRAN && !( class == LABEL && slev >= 2 ) ){
		q->tn.rval = idp = hide( p );
		p = &stab[idp];
		goto reenter;						/*!mh*/
		}
	/* "redeclaration of %.8s" */
	/* "redeclaration of %s" */
	UERROR( MESSAGE( 96 ), p->sname );
	if( class==EXTDEF && ISFTN(type) ) curftn = idp;
	return;

	reenter:	/* enter an existing symbol as something else */						/*!mh*/
	if (p->sclass == EXTERN || p->sclass == EXTDEF || p->sclass == USTATIC
	    || p->sclass == STATIC)
	    freenter = p->offset;

	enter:  /* make a new entry */

# ifndef BUG1
	if( ddebug ) printf( "\tnew entry made\n" );
# endif
	/* "void type for %.8s" */
	/* "void type for %s" */
	if( ISEQUAL(type, UNDEF) && (class != TYPEDEF))
		UERROR(MESSAGE( 117 ),p->sname);
	p->offset = NOOFFSET;						/*!mh*/


	p->sprototype = fp;
	p->stype = type;
	p->sclass = class;
	p->slevel = blevel;
	p->suse = lineno;
	if( (class == EXTDEF) && fp)
          p->sflags |= SFUNCTION_DEF_WITH_PROTOTYPE;
	if( class == STNAME || class == UNAME || class == ENAME ) {
		p->sizoff = curdim;
		dstash( 0 );  /* size */
		dstash( -1 ); /* index to members of str or union */
		dstash( ALSTRUCT );  /* alignment */
		dstash( idp );
		}
	else {
		switch( BTYPE(type) ){
		case STRTY:
		case UNIONTY:
		case ENUMTY:
			p->sizoff = q->fn.csiz;
			break;
		default:
			p->sizoff = BTYPE(type);
			}
		}

	/* copy dimensions */

	p->dimoff = q->fn.cdim;

	/* allocate offsets */
	if( class&FIELD ){
		falloc( p, class&FLDSIZ, 0, NIL );  /* new entry */
		psave( idp );
		if (gdebug)
			st_def (p, freenter);					/*!07*/
		}
	else switch( class ){

	case AUTO:
		oalloc( p, &autooff );
		if (gdebug) {
			int idn;
			setfile(scText, stBlock);
			if (idn = st_textblock())
 				ucw_write (Ubgnb, idn);
			unsetfile();
			st_def (p, freenter);
		}							/*!07*/
		break;
	case STATIC:
	case EXTDEF:
		p->offset = st_def(p, freenter);					/*!*07*/
		if( ISFTN(type) ) curftn = idp;
		break;
	case ULABEL:
	case LABEL:
		p->offset = getlab();
		p->slevel = 2;
		if( class == LABEL ){
			locctr( PROG );
			if (gdebug)
			    defnamedlab( p->offset, st_def(p, freenter) );/*!07*/
			else
			    deflab ( p->offset );
			}
		break;

	case EXTERN:
	case UFORTRAN:
	case FORTRAN:
		p->slevel = 0;
		p->offset = st_def(p, freenter);					/*!07*/
		if (dimptr >= dimrec)
			dimptr->cextern = 1;
		break;
	case MOU:
	case MOS:
		oalloc( p, &strucoff );
		if( class == MOU ) strucoff = 0;
		if (gdebug)
			st_def (p, freenter);					/*!07*/
		psave( idp );
		break;

	case MOE:
		p->offset = strucoff++;
		if (gdebug)
			st_def (p, freenter);					/*!07*/
		psave( idp );
		break;

	case REGISTER:
# ifdef LINT
		if( blevel == 1 ) p->sflags |= SSET;
# endif
		if (blevel == 1) break; /* it's a param we'll do it later */
		else {
		    oalloc( p, &autooff );			/*!05!07*/
		    if (gdebug) {
			    int idn;
			    setfile(scText, stBlock);
			    if (idn = st_textblock())
				    ucw_write (Ubgnb, idn);
			    unsetfile();
			    st_def (p, freenter);
		    }							/*!07*/
		    break;
		}

	default:
		if (gdebug)
		    p->offset = st_def (p, freenter);
		break;						          

	case USTATIC:
		if (ISFTN(type)) {
		    p->slevel = 0;	/* need to keep it around! */
		}
	        p->offset = st_def (p, freenter);
		break;

	case PARAM:
		/* can't st_def this puppy until we're past the procdef */
		break;							/*!07*/
		}

	{
		register int l = p->slevel;

		if( l >= bnestmax )
			cerror( "scopes nested too deep; increase default option: -Wf,-XNd%d", stabmax );

		p->snext = schain[l];
		schain[l] = p;
		if( l >= chaintop )
			chaintop = l + 1;
		}

	/* user-supplied routine to fix up new definitions */

	FIXDEF(p);

# ifndef BUG1
	if( ddebug ) printf( "\tdimoff, sizoff, offset: %d, %d, %d\n", p->dimoff, p->sizoff, p->offset );
# endif

	}

psave( i ){
	if( paramno >= paramstkmax ){
		cerror( "parameter stack overflow; increase default option: -Wf,-XNf%d", paramstkmax);
		}
	paramstk[ paramno++ ] = i;
	}

ftnend(){ /* end of function */
	if( retlab != NOLAB ){ /* inside a real function */
		efcode();
		}
	checkst(0);
	retstat = 0;
	tcheck();
	curclass = SNULL;
	brklab = contlab = retlab = NOLAB;
	flostat = 0;
	if( nerrors == 0 ){
		if( psavbc != & asavbc[0] ) cerror("bcsave error");
		if( paramno != 0 ) cerror("parameter reset error");
		if( swx != 0 ) cerror( "switch error");
		}
	psavbc = &asavbc[0];
	paramno = 0;
	autooff = AUTOINIT;
	minrvar = regvar = MAXRVAR;
	reached = 1;
	swx = 0;
	swp = swtab;
	locctr(DATA);
	}

dclargs(){
	register i, j;
	register struct symtab *p;
	register NODE *q;
	register int temp, formalno;					/*!04*/

	argoff = ARGINIT;
	temp = stab[curftn].stype;					/*!04*/
	temp = DECREF(temp);						/*!04*/
	if( ISEQUAL(temp,STRTY) || ISEQUAL(temp, UNIONTY))		/*!04*/
	  argoff += SZPOINT; /* space for destination pointer */	
# ifndef BUG1
	if( ddebug > 2) printf("dclargs()\n");
# endif
	formalno = num_of_formals(stab[curftn].sprototype);
	if( (stab[curftn].sprototype != NULL) && (formalno != paramno) )
	  UERROR( MESSAGE(139) );
	for( i=0; i<paramno; ++i ){
		register function_prototype_list fp = NULL;
		

		if( (j = paramstk[i]) < 0 ) continue;
		/* if there's a prototype in scope, get the declared */

		/* type for the parameter.			     */
		if( formalno )
		  fp = formal_type(stab[curftn].sprototype, i+1);
		p = &stab[j];
# ifndef BUG1
		if( ddebug > 2 ){
			printf( "\t%s (%d) ", p->sname, j );
			tprint(p->stype);
			printf("\n");
			}
# endif
		if( ISEQUAL(p->stype, FARG) ) {
 			if( VOLATILIZE )
                          q = block(FREE,NIL,NIL,VOL|INT,0,INT);
			else 
        		  q = block(FREE,NIL,NIL,INT,0,INT);
			/*if we have a type for parameter, use it to*/
			/* declare the parameter. This is the case  */
			/* where the prototype description is part  */
			/* of the function definition (slevel ==1). */
			if( fp && (p->slevel == 1))
			  *q = fp->prototype;
		        q->tn.rval = j;
			defid( q, PARAM );
			}
		if( formalno ) {
		  register function_prototype_list f_type;
                  /*register*/ NODE a_type;
		  /* check consistency between prototype and argument */
                  /* declarations. */
		  a_type.tn.type = p->stype;
                  a_type.fn.cdim = p->dimoff;
		  a_type.fn.csiz = p->sizoff;
		  a_type.fn.prototype = p->sprototype;
		  f_type = formal_type(stab[curftn].sprototype, i+1); 
		  valid_argument_declaration( f_type->prototype, a_type );
                }
		FIXARG(p); /* local arg hook, eg. for sym. debugger */
		oalloc( p, &argoff );  /* always set aside space, even for register arguments */
		}
	cendarg();
	locctr(PROG);
	++ftnno;
	bfcode( paramstk, paramno );
	paramno = 0;
	}

NODE *
rstruct( idn, soru ){ /* reference to a structure or union, with no definition */
	register struct symtab *p;
	register NODE *q;
	p = &stab[idn];
	switch( NORM(p->stype) ){

	case UNDEF:
	def:
		q = block( FREE, NIL, NIL, 0, 0, 0 );
		q->tn.rval = idn;
		q->in.type = (soru&INSTRUCT) ? STRTY : ( (soru&INUNION) ? UNIONTY : ENUMTY );
		defid( q, (soru&INSTRUCT) ? STNAME : ( (soru&INUNION) ? UNAME : ENAME ) );
		if (gdebug)
		    st_forwstr(p->offset);
		break;

	case STRTY:
		if( soru & INSTRUCT ) break;
		goto def;

	case UNIONTY:
		if( soru & INUNION ) break;
		goto def;

	case ENUMTY:
		if( !(soru&(INUNION|INSTRUCT)) ) break;
		goto def;

		}
	stwart = instruct;
	return( mkty( p->stype, 0, p->sizoff ) );
	}

moedef( idn ){
	register NODE *q;

	q = block( FREE, NIL, NIL, MOETY, 0, 0 );
	q->tn.rval = idn;
	if( idn>=0 ) defid( q, MOE );
	}

bstruct( idn, soru ){ /* begining of structure or union declaration */
	register NODE *q;

	psave( instruct );
	psave( curclass );
	psave( strucoff );
	strucoff = 0;
	instruct = soru;
	q = block( FREE, NIL, NIL, 0, 0, 0 );
	if (gdebug && idn < 0) {
		/* no name struct/union/enum -- so give it one */
		char	tempname [256];
		sprintf (tempname, ".F%d", getlab());
		idn = lookup (hash (tempname), STAG);
	} /* if */							/*!07*/
	q->tn.rval = idn;
	if( instruct==INSTRUCT ){
		curclass = MOS;
		q->in.type = STRTY;
		if( idn >= 0 ) defid( q, STNAME );
		}
	else if( instruct == INUNION ) {
		curclass = MOU;
		q->in.type = UNIONTY;
		if( idn >= 0 ) defid( q, UNAME );
		}
	else { /* enum */
		curclass = MOE;
		q->in.type = ENUMTY;
		if( idn >= 0 ) defid( q, ENAME );
		}
	psave( idn = q->tn.rval );
	/* the "real" definition is where the members are seen */
	if ( idn >= 0 ) stab[idn].suse = lineno;
	if (gdebug)
	    setfile(scInfo, stBlock); /* set's file for all members */
	return( paramno-4 );
	}

NODE *
dclstruct( oparam, vorc )
	register int vorc;{		/* volatile ? */
	register struct symtab *p;
	register i, al, sa, j, sz, szindex;
	register TWORD temp;
	register high, low;

	/* paramstack contains:
		paramstack[ oparam ] = previous instruct
		paramstack[ oparam+1 ] = previous class
		paramstk[ oparam+2 ] = previous strucoff
		paramstk[ oparam+3 ] = structure name

		paramstk[ oparam+4, ... ]  = member stab indices

		*/


	if( (i=paramstk[oparam+3]) < 0 ){
		szindex = curdim;
		dstash( 0 );  /* size */
		dstash( -1 );  /* index to member names */
		dstash( ALSTRUCT );  /* alignment */
		dstash( -lineno );	/* name of structure */
		}
	else {
		szindex = stab[i].sizoff;
		}

# ifndef BUG1
	if( ddebug ){
#ifdef FLEXNAMES
		printf( "dclstruct( %s ), szindex = %d\n",
			(i>=0)? stab[i].sname : "??", szindex );
#else
		printf( "dclstruct( %.8s ), szindex = %d\n",
			(i>=0)? stab[i].sname : "??", szindex );
#endif
		}
# endif
	temp = (instruct&INSTRUCT)?STRTY:((instruct&INUNION)?UNIONTY:ENUMTY);
	stwart = instruct = paramstk[ oparam ];
	curclass = paramstk[ oparam+1 ];
	dimtab[ szindex+1 ] = curdim;
	al = ALSTRUCT;

	high = low = 0;

	for( i = oparam+4;  i< paramno; ++i ){
		dstash( j=paramstk[i] );
		if( j<0 || j>= stabmax ) cerror( "gummy structure member" );
		p = &stab[j];
		if( temp == ENUMTY ){
			if( p->offset < low ) low = p->offset;
			if( p->offset > high ) high = p->offset;
			p->sizoff = szindex;
			continue;
			}
		sa = talign( p->stype, p->sizoff );
		if( p->sclass & FIELD ){
			sz = p->sclass&FLDSIZ;
			}
		else {
			sz = tsize( p->stype, p->dimoff, p->sizoff );
			}
		if( sz == 0 ){
			/* "illegal zero sized structure member: %.8s" */
			/* "illegal zero sized structure member: %s" */
			WERROR( MESSAGE( 73 ), p->sname );
			}
		if(  sz > ( unsigned ) strucoff ) strucoff = sz;  /* for use with unions */
		SETOFF( al, sa );
		/* set al, the alignment, to the lcm of the alignments of the members */
		}
	dstash( -1 );  /* endmarker */
	SETOFF( strucoff, al );

	if( temp == ENUMTY ){
		register TWORD ty;

# ifdef ENUMSIZE
		ty = ENUMSIZE(high,low);
# else
		if( (char)high == high && (char)low == low ) ty = ctype( CHAR );
		else if( (short)high == high && (short)low == low ) ty = ctype( SHORT );
		else ty = ctype(INT);
#endif
		strucoff = tsize( ty, 0, NORM((int)ty) );		/*!06*/
		dimtab[ szindex+2 ] = al = talign( ty, (int)ty );
		}

	/* "zero sized structure" */
	if( strucoff == 0 ) UERROR( MESSAGE( 121 ) );
	dimtab[ szindex ] = strucoff;
	dimtab[ szindex+2 ] = al;
	dimtab[ szindex+3 ] = paramstk[ oparam+3 ];  /* name index */

	if (gdebug) {
		st_blockend (strucoff/SZCHAR);				/*!07*/
		unsetfile(); /* started in bstruct */
	} /* if */
	/*FIXSTRUCT( szindex, oparam ); /* local hook, eg. for sym debugger *//*!07*/
# ifndef BUG1
	if( ddebug>1 ){
		printf( "\tdimtab[%d,%d,%d] = %d,%d,%d\n", szindex,szindex+1,szindex+2,
				dimtab[szindex],dimtab[szindex+1],dimtab[szindex+2] );
		for( i = dimtab[szindex+1]; dimtab[i] >= 0; ++i ){
#ifdef FLEXNAMES
			printf( "\tmember %s(%d)\n",
				stab[dimtab[i]].sname, dimtab[i] );
#else
			printf( "\tmember %.8s(%d)\n",
				stab[dimtab[i]].sname, dimtab[i] );
#endif
			}
		}
# endif

	strucoff = paramstk[ oparam+2 ];
	paramno = oparam;

	return( mkty( temp | vorc, 0, szindex ) );
	}

	/* VARARGS */
yyerror( s ) char *s; { /* error printing routine in parser */

	uerror( s );

	}

yyaccpt(){
	ftnend();
	}

ftnarg( idn ) {
	switch( NORM(stab[idn].stype) ){

	case UNDEF:
		/* this parameter, entered at scan */
		break;
	case FARG:
		/* "redeclaration of formal parameter, %.8s" */
		/* "redeclaration of formal parameter, %s" */
		  UERROR(MESSAGE( 97 ), stab[idn].sname);
		/* fall thru */
	case FTN:
		/* the name of this function matches parm */
		/* fall thru */
	default:
	                idn = hide( &stab[idn]);
		break;
	case TNULL:
		/* unused entry, fill it */
		;
		}
	stab[idn].stype =  FARG;
	stab[idn].sclass = PARAM;
	psave( idn );
	}

talign( ty, s) register unsigned ty; register s; {
	/* compute the alignment of an object with type ty, sizeoff index s */

	register i;
	ty = NORM(ty);							/*!06*/
	if( s<0 && ty!=INT && ty!=CHAR && ty!=SHORT && ty!=UNSIGNED && ty!=UCHAR && ty!=USHORT 
#ifdef LONGFIELDS
		&& ty!=LONG && ty!=ULONG
#endif
					){
		return( fldal( ty ) );
		}

	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			cerror( "compiler takes alignment of function");
		case PTR:
			return( ALPOINT );
		case ARY:
			continue;
		case 0:
			break;
			}
		}

	switch( BTYPE(ty) ){

	case UNIONTY:
	case ENUMTY:
	case STRTY:
		return( (unsigned int) dimtab[ s+2 ] );
	case CHAR:
	case UCHAR:
		return( ALCHAR );
	case FLOAT:
		return( ALFLOAT );
	case DOUBLE:
		return( ALDOUBLE );
	case LONG:
	case ULONG:
		return( ALLONG );
	case SHORT:
	case USHORT:
		return( ALSHORT );
	default:
		return( ALINT );
		}
	}
/*
* Size calculations need to be checked for overflow (especially
* on 16 bit machines).  However, ``unsigned long'' isn't supported
* on all of our C compilers (Ritchie's PDP11 C compiler).  So this
* typedef is for the internal calculations.
*/
#if pdp11
	typedef long		ulong;
#else
	typedef unsigned long	ulong;
#endif


OFFSZ
tsize( ty, d, s )  TWORD ty; {
	/* compute the size associated with type ty,
	    dimoff d, and sizoff s */
	/* BETTER NOT BE CALLED WHEN t, d, and s REFER TO A BIT FIELD... */

	int i;
	ulong mult, tmp;

	mult = 1L;
	ty= NORM(ty);							/*!06*/
	for( i=0; i<=(SZINT-BTSHIFT-1); i+=TSHIFT ){
		switch( (ty>>i)&TMASK ){

		case FTN:
			cerror( "compiler takes size of function");
		case PTR:
			if ( ( tmp = ( (ulong) SZPOINT ) * mult )
				>= (ulong) offsz )
			{
				cerror("array too large");
			}
			else
				return (tmp);
		case ARY:
			mult *= (ulong) dimtab[ d++ ];
			continue;
		case 0:
			break;

			}
		}

	if( dimtab[s]==0 ) {					
		/* "unknown size" */
		UERROR( MESSAGE( 114 ));
		return( SZINT );
		}
	if ( ( tmp = (ulong) dimtab[s] * mult ) >= (ulong) offsz )
		cerror("array too large");
	return ((unsigned int) tmp);
	}

inforce( n ) OFFSZ n; {  /* force inoff to have the value n */
	/* inoff is updated to have the value n */
	OFFSZ wb;
	register rest;
	/* rest is used to do a lot of conversion to ints... */

	if( inoff == n ) return;
	if( inoff > n ) {
		cerror( "initialization alignment error");
		}

	wb = inoff;
	SETOFF( wb, SZINT );

	/* wb now has the next higher word boundary */

	if( wb >= n ){ /* in the same word */
		rest = n - inoff;
		vfdzero( rest );
		return;
		}

	/* otherwise, extend inoff to be word aligned */

	rest = wb - inoff;
	vfdzero( rest );

	/* now, skip full words until near to n */

	rest = (n-inoff)/SZINT;
	zecode( rest );

	/* now, the remainder of the last word */

	rest = n-inoff;
	vfdzero( rest );
	if( inoff != n ) cerror( "inoff error");

	}

vfdalign( n ){ /* make inoff have the offset the next alignment of n */
	OFFSZ m;

	m = inoff;
	SETOFF( m, n );
	inforce( m );
	}


int idebug = 0;

int ibseen = 0;  /* the number of } constructions which have been filled */

int iclass;  /* storage class of thing being initialized */

int ilocctr = 0;  /* location counter for current initialization */

beginit(curid){
	/* beginning of initilization; set location ctr and set type */
	register struct symtab *p;

# ifndef BUG1
	if( idebug >= 3 ) printf( "beginit(), curid = %d\n", curid );
# endif

	p = &stab[curid];

	iclass = p->sclass;
	if( curclass == EXTERN || curclass == FORTRAN ) iclass = EXTERN;
	switch( iclass ){

	case UNAME:
	case EXTERN:
		return;						  
	case AUTO:
	case REGISTER:
		break;
	case EXTDEF:
	case STATIC:
		ilocctr = ISARY(p->stype)?ADATA:DATA;
		locctr( ilocctr );		
		defnam( p );

		}

	inoff = 0;
	indblkno = p->offset;						/*!02*/ 
	ibseen = 0;

	pstk = 0;

	instk( curid, p->stype, p->dimoff, p->sizoff, inoff );

	}

instk( id, t, d, s, off ) OFFSZ off; TWORD t; {
	/* make a new entry on the parameter stack to initialize id */
	register struct symtab *p;

	for(;;){
# ifndef BUG1
		if( idebug ) printf( "instk((%d, %o,%d,%d, %d)\n", id, t, d, s, off );
# endif

		/* save information on the stack */

		if( !pstk ) pstk = instack;
		else ++pstk;

		pstk->in_fl = 0;	/* { flag */
		pstk->in_id =  id ;
		pstk->in_t =  t ;
		pstk->in_d =  d ;
		pstk->in_s =  s ;
		pstk->in_n = 0;  /* number seen */
		pstk->in_x =  ISEQUAL(t,STRTY) ?dimtab[s+1] : 0 ;
		pstk->in_off =  off;   /* offset at the beginning of this element */
		/* if t is an array, DECREF(t) can't be a field */
		/* INS_sz has size of array elements, and -size for fields */
		if( ISARY(t) ){
			pstk->in_sz = tsize( DECREF(t), d+1, s );
			}
		else if( stab[id].sclass & FIELD ){
			pstk->in_sz = - ( stab[id].sclass & FLDSIZ );
			}
		else {
			pstk->in_sz = 0;
			}

		if( (iclass==AUTO || iclass == REGISTER ) &&
			/* "no automatic aggregate initialization" */
			(ISARY(t) || ISEQUAL(t,STRTY)) ) UERROR( MESSAGE( 79 ) );

		/* now, if this is not a scalar, put on another element */

		if( ISARY(t) ){
			t = DECREF(t);
			++d;
			continue;
			}
		else if( ISEQUAL(t,STRTY) ){
		  if (pstk->in_x < 0 || pstk->in_x >= dimtabmax) {
		    /* Unknown size */
		    UERROR( MESSAGE ( 114 ) );
		    return;
		  }
			id = dimtab[pstk->in_x];
			p = &stab[id];
			if( p->sclass != MOS && !(p->sclass&FIELD) ) cerror( "insane structure member list" );
			t = p->stype;
			d = p->dimoff;
			s = p->sizoff;
			off += p->offset;
			continue;
			}
		else return;
		}
	}

NODE *
getstr(){ /* decide if the string is external or an initializer, and get the contents accordingly */

	register l, temp;
	register NODE *p;
	if( (iclass==EXTDEF||iclass==STATIC) &&
	    (ISEQUAL(pstk->in_t, CHAR) || ISEQUAL(pstk->in_t,UCHAR)) &&
			pstk!=instack && ISARY( pstk[-1].in_t ) ){
		/* treat "abc" as { 'a', 'b', 'c', 0 } */

		strflg = 1;
		ilbrace();  /* simulate { */
		inforce( pstk->in_off );
		/* if the array is inflexible (not top level), pass in the size and
			be prepared to throw away unwanted initializers */
		lxstr((pstk-1)!=instack?dimtab[(pstk-1)->in_d]:0);  /* get the contents */
		irbrace();  /* simulate } */
		return( NIL );
		}
	else { /* make a label, and get the contents and stash them away */
		register dblkno = indblkno; /*!02 current data block number */
		char     lname[10];        /*!02 label for initializing string*/
		
		if( iclass != SNULL ){ /* initializing */
			/* fill out previous word, to permit pointer */
			vfdalign( ALPOINT );
			}
		/*set up location counter */
		/*temp = locctr( blevel==0?ISTRNG:STRNG );		  !02*/

		/*deflab( l = getlab() );				  !02*/
		/* dummy symbols point at the file -- ?????? */
		indblkno = l = st_idn_index_fext (ST_ANONINDEX,0);    /*!02,07*/
		/*sprintf(lname, "L%d", indblkno);		      /*!02,07*/
		ucw_write(Ulsym, indblkno, 0, 0);		      /*!02,07*/
		strflg = 0;
		lxstr(0); /* get the contents */
		indblkno = dblkno; /* restore data block number 	  !02*/
		/*locctr( blevel==0?ilocctr:temp );			  !02*/
		p = buildtree( STRING, NIL, NIL );
		p->tn.rval = -l;
		return(p);
		}
	}

putbyte( v ){ /* simulate byte v appearing in a list of integer values */
	register NODE *p;
	p = bcon(v);
	incode( p, SZCHAR );
	tfree( p );
	gotscal();
	}

endinit(){
	register TWORD t;
	register d, s, n, d1;
# ifndef BUG1
	if( idebug ) printf( "endinit(), inoff = %d\n", inoff );
# endif

	switch( iclass ){

	case EXTERN:
	case AUTO:
	case REGISTER:
		return;
		}

	pstk = instack;

	t = pstk->in_t;
	d = pstk->in_d;
	s = pstk->in_s;
	n = pstk->in_n;

	if( ISARY(t) && pstk->in_sz ){
		d1 = dimtab[d];

		vfdalign( pstk->in_sz );  /* fill out part of the last element, if needed */
		n = inoff/pstk->in_sz;  /* real number of initializers */
		if( d1 >= n ){
			/* once again, t is an array, so no fields */
			inforce( tsize( t, d, s ) );		
			n = d1;
			}
		/* "too many initializers" */
		if( d1!=0 && d1!=n ) UERROR( MESSAGE( 108 ));
		/* "empty array declaration" */
		if( n==0 ) WERROR( MESSAGE( 35 ));
		dimtab[d] = n;
	/*** This is a bug, there's no entry of array size to be fixed ***
		 if (gdebug) {
		  st_fix_array_size(&stab[pstk->in_id]);
		 }
	*/
		}

	else if( ISEQUAL(t, STRTY) || ISEQUAL(t, UNIONTY) ){
		/* clearly not fields either */
		inforce(  tsize( t, d, s ) );			
		}
	/* "bad scalar initialization" */
	else if( n > 1 ) UERROR( MESSAGE( 17 ));
	/* this will never be called with a field element... */
	else inforce(  tsize(t,d,s) );				

	paramno = 0;
	vfdalign( AL_INIT );
	ucw_write(Usdef, indblkno, inoff);				/*!02*/
	inoff = 0;
	iclass = SNULL;

	}

doinit( p ) register NODE *p; {

	/* take care of generating a value for the initializer p */
	/* inoff has the current offset (last bit written)
		in the current word being generated */

	register sz, d, s;
	register TWORD t;
	extern void eval_const_expr();


	/* note: size of an individual initializer is assumed to fit into an int */

	if( iclass < 0 ) goto leave;
	if( iclass == EXTERN || iclass == UNAME ){
		/* "cannot initialize extern or union" */
		UERROR( MESSAGE( 19 ) );
		iclass = -1;
		goto leave;
		}

	if( iclass == AUTO || iclass == REGISTER ){
		/* do the initialization and get out, without regard 
		    for filing out the variable with zeros, etc. */
		bccode();
		idname = pstk->in_id;
		p = buildtree( ASSIGN, buildtree( NAME, NIL, NIL ), p );
		ecomp(p);
		return;
		}

	if( p == NIL ) return;  /* for throwing away strings that have been turned into lists */

	if( ibseen ){
		/* "} expected" */
		UERROR( MESSAGE( 122 ));
		goto leave;
		}

# ifndef BUG1
	if( idebug > 1 ) printf( "doinit(%o)\n", p );
# endif

	t = pstk->in_t;  /* type required */
	d = pstk->in_d;
	s = pstk->in_s;
	if( pstk->in_sz < 0 ){  /* bit field */
		sz = -pstk->in_sz;
		}
	else {
		sz = tsize( t, d, s );
		}

	inforce( pstk->in_off );

	p = buildtree( ASSIGN, block( NAME, NIL,NIL, t, d, s ), p );
	p->in.left->in.op = FREE;
	p->in.left = p->in.right;
	p->in.right = NIL;
	p->in.left = optim( p->in.left );
	if( p->in.left->in.op == UNARY AND ){
		p->in.left->in.op = FREE;
		p->in.left = p->in.left->in.left;
		}
	p->in.op = INIT;

	if( sz < SZINT ){ /* special case: bit fields, etc. */
		/* "illegal initialization"  */
      		walkf(p->in.left, eval_const_expr);
		if( p->in.left->in.op != ICON ) UERROR( MESSAGE( 61 ) );
		else incode( p->in.left, sz );
		}
	else if( p->in.left->in.op == FCON ){
		fincode( p->in.left->fpn.dval, sz );
		}
	else {
		cinit( optim(p), sz );
		}

	gotscal();

	leave:
	tfree(p);
	}

gotscal(){
	register t, ix;
	register n, id;
	struct symtab *p;
	OFFSZ temp;

	for( ; pstk > instack; ) {

		if( pstk->in_fl ) ++ibseen;

		--pstk;
		
		t = pstk->in_t;

		if( ISEQUAL(t, STRTY) ){
			ix = ++pstk->in_x;
			if( (id=dimtab[ix]) < 0 ) continue;

			/* otherwise, put next element on the stack */

			p = &stab[id];
			instk( id, p->stype, p->dimoff, p->sizoff, p->offset+pstk->in_off );
			return;
			}
		else if( ISARY(t) ){
			n = ++pstk->in_n;
			if( n >= dimtab[pstk->in_d] && pstk > instack ) continue;

			/* put the new element onto the stack */

			temp = pstk->in_sz;
			instk( pstk->in_id, (TWORD)DECREF(pstk->in_t), pstk->in_d+1, pstk->in_s,
				pstk->in_off+n*temp );
			return;
			}

		}

	}

ilbrace(){ /* process an initializer's left brace */
	register t;
	INSTK *temp;

	temp = pstk;

	for( ; pstk > instack; --pstk ){

		t = pstk->in_t;
		if( !ISEQUAL(t , STRTY) && !ISARY(t) ) continue; /* not an aggregate */
		if( pstk->in_fl ){ /* already associated with a { */
			/* "illegal {" */
			if( pstk->in_n ) UERROR( MESSAGE( 74 ));
			continue;
			}

		/* we have one ... */
		pstk->in_fl = 1;
		break;
		}

	/* cannot find one */
	/* ignore such right braces */

	pstk = temp;
	}

irbrace(){
	/* called when a '}' is seen */

# ifndef BUG1
	if( idebug ) printf( "irbrace(): paramno = %d on entry\n", paramno );
# endif

	if( ibseen ) {
		--ibseen;
		return;
		}

	for( ; pstk > instack; --pstk ){
		if( !pstk->in_fl ) continue;

		/* we have one now */

		pstk->in_fl = 0;  /* cancel { */
		gotscal();  /* take it away... */
		return;
		}

	/* these right braces match ignored left braces: throw out */

	}

static long
upoff( size, alignment, poff ) register alignment; register long *poff; {
	/* update the offset pointed to by poff; return the
	/* offset of a value of size `size', alignment `alignment',
	/* given that off is increasing */

	register long off;

	off = *poff;
	SETOFF( off, alignment );
	if ( ( ( (ulong) offsz ) - ( (ulong) off ) ) < (ulong) size ){
		if( instruct!=INSTRUCT )cerror("too many local variables");
		else cerror("Structure too large");
		}
	*poff = off+size;
	return( off );
	}

oalloc( p, poff ) register struct symtab *p; register *poff; {
	/* allocate p with offset *poff, and update *poff */
	register al, tsz;
	register long off;
	long noff, upoff();

	al = talign( p->stype, p->sizoff );
	noff = off = *poff;
	tsz = tsize( p->stype, p->dimoff, p->sizoff );

	if(p->sclass == AUTO || (p->sclass == REGISTER && blevel != 1) ){/*!05*/
		if ( ( ( (ulong) offsz ) - ( (ulong) off ) ) < (ulong) tsz )
			cerror("too many local variables");
		if( tsz < SZINT && p->sclass == REGISTER) {
		  /* align REGISTER to ALINT */
		  off = upoff(SZINT, ALINT, &noff);
		  noff = off + ALINT;
		  off  = -noff;
		} else {
		  noff = off + tsz;
		  SETOFF( noff, al < ALINT ? ALINT : ALINT);
		  off = -noff;
		}
	}
	else

			/* align char/short PARAM and REGISTER to ALINT */
		if((p->sclass == PARAM || (p->sclass == REGISTER && blevel==1) )
		   && ( tsz < SZINT ) && REGISTER_TYPE(p->stype)){
			off = upoff( SZINT, ALINT, &noff );
			if( endian == BIGENDIAN )			/*!15*/
			  off = noff - tsz;
			}
		else
		{
		off = upoff( tsz, al, &noff );
		}

	if( !( p->sclass == REGISTER && blevel == 1) ){ 
	 /* in case we are allocating stack space for register arguments  !05*/
		if( p->offset == NOOFFSET ) p->offset = off;
		else if( off != p->offset ) return(1);
	}							 

	if(  (p->sclass == REGISTER && blevel != 1) &&
	  ( REGISTER_TYPE( p->stype ) && !ISVOL(p->stype) && !ISVP(p->stype)))
	  ucw_write(Uvreg, ucu_ucodety( p->stype ), Mmt, cblkno, off,   /*!10*/
	  tsize( p->stype, p->dimoff, p->sizoff ), 0);	        /*!09, !14   */
	*poff = noff;
	return(0);
	}

falloc( p, w, new, pty )  register struct symtab *p; NODE *pty; {	/*!13*/
/* allocate a field of width w */
	/* new is 0 if new entry, 1 if redefinition, -1 if alignment */

	register al,sz,type;

	type = (new<0)? pty->in.type : p->stype;

	/* this must be fixed to use the current type in alignments */
	switch( new<0? NORM(pty->in.type) : NORM(p->stype) ){				
	case ENUMTY:
		{
			int s;
			s = new<0 ? pty->fn.csiz : p->sizoff;
			al = dimtab[s+2];
			sz = dimtab[s];
			break;
			}

	case CHAR:
	case UCHAR:
		al = ALCHAR;
		sz = SZCHAR;
		break;

	case SHORT:
	case USHORT:
		al = ALSHORT;
		sz = SZSHORT;
		break;

	case INT:
	case UNSIGNED:
		al = ALINT;
		sz = SZINT;
		break;

	case LONG:
	case ULONG:
		al = ALLONG;
		sz = SZLONG;
		break;

	default:
		if( new < 0 ) {
			/* "illegal field type" */
			UERROR( MESSAGE( 57 ) );
			al = ALINT;
			}
		else {
			al = fldal( p->stype );
			sz =SZINT;
		     }
	
	}

	if( w > sz ) {
		/* "field too big" */
		UERROR( MESSAGE( 39 ));
		w = sz;
		}

	if( w == 0 ){ /* align only */
		SETOFF( strucoff, al );
		/* "zero size field" */
		if( new >= 0 ) UERROR( MESSAGE( 120 ));
		return(0);
		}

	if( strucoff%al + w > sz ) SETOFF( strucoff, al );
	if( new < 0 ) {
		if( (offsz-strucoff) < w )
			cerror("structure too large");
		strucoff += w;  /* we know it will fit */
		return(0);
		}

	/* establish the field */

	if( new == 1 ) { /* previous definition */
		if( p->offset != strucoff || p->sclass != (FIELD|w) ) return(1);
		}
	p->offset = strucoff;
	if( (offsz-strucoff) < w ) cerror("structure too large");
	strucoff += w;
	p->stype = type;
	fldty( p );
	return(0);
	}

nidcl( p ) NODE *p; { /* handle unitialized declarations */
	/* assumed to be not functions */
	register class;
	register commflag;  /* flag for labelled common declarations */

	commflag = 0;

	/* compute class */
	if( (class=curclass) == SNULL ){
		if( blevel > 1 ) class = AUTO;
		else if( blevel != 0 || instruct ) cerror( "nidcl error" );
		else { /* blevel = 0 */
			class = noinit();
			if( class == EXTERN ) {
				commflag = 1;
				st_commset ();
			    }
			}
		}

	defid( p, class );
	st_communset();

	if( class==EXTDEF || class==STATIC){
		/* simulate initialization by 0 */
		defnam(&stab[p->tn.rval]);
	        ucw_write(Usdef, stab[p->tn.rval].offset, 
                          tsize(p->tn.type, p->fn.cdim, p->fn.csiz));	
	}
	if( class==EXTERN && !commflag) {
	  	register struct symtab *sp = &stab[p->tn.rval];
	        register int sz = 0;   
		if( dimtab[sp->sizoff] ) /* do we know its size ? */
	          sz = tsize( sp->stype, sp->dimoff, sp->sizoff);
 		ucw_write(Uesym, sp->offset, 0, sz/Bytesize );	
	}
	if( commflag  ) commdec( p->tn.rval );
	}

TWORD
types( t1, t2, t3, t4) TWORD t1, t2, t3, t4; {				/*!06*/
	/* return a basic type from basic types t1, t2, and t3 */

	TWORD t[4], noun, adj, sign, tm;				/*!12*/
	register i;
	extern int volt;

	t[0] = t1;
	t[1] = t2;
	t[2] = t3;
	t[3] = t4;

	if( (t1 == STRTY) || (t2 == STRTY) || (t1 == UNIONTY) ||
	    (t2 == UNIONTY) || (t1 == ENUMTY) || (t2 == ENUMTY) ) {
	    /* a struct, union, or enum typename made volatile */
		if( (t1 != VOL) && (t2 != VOL) ) goto bad;	    
		return t1 + t2;
	} else
	if( ISPTR(t1) || ISPTR(t2) ) {
	    /* a pointer typename made volatile */		
		if( (t1 != VOL) && (t2 != VOL) ) goto bad;	    
		if( t1 == VOL ) return INCREFVP( t2 );
		else return INCREFVP(t1);
	}

	sign = INT;  /* INT, UNSIGNED, or SIGNED */
	noun = UNDEF;  /* INT, CHAR, or FLOAT    */
	adj = INT;  /* INT, LONG, or SHORT */
	tm = (volt) ? VOL : UNDEF; /* make all globals volatile if volt set*/


	for( i=0; i<4; ++i ){
		switch( t[i] ){

		default:
		bad:
			/* "illegal type combination" */
			UERROR( MESSAGE( 70 ) );
			return( INT );

		case CONST:
		        /* not implemented, but reserved for future use */
	        	WERROR( MESSAGE( 133 ) );
			continue;

		case UNDEF:
			continue;

		case VOL:
		case VOL|INT:		/* volatile i  == volatile int i */
			if (tm & !(VOLATILIZE)) goto bad;
			tm = VOL;
			continue;

		case VOL|UNSIGNED:
		case UNSIGNED:
			if( sign != INT ) goto bad;
			sign = UNSIGNED;
			continue;

		case VOL|SIGNED:
		case SIGNED:
			if( sign != INT ) goto bad;
			sign = SIGNED;
			continue;


		case LONG:
		case VOL|SHORT:
		case SHORT:
			if( adj != INT ) goto bad;
			adj = NORM(t[i]);
			continue;


		case INT:
		case VOL|UCHAR:
		case UCHAR:
		case VOL|CHAR:
		case CHAR:
	        case VOL|USHORT:
	        case USHORT:
		case VOL|FLOAT:
		case FLOAT:
		case VOL|DOUBLE:
		case DOUBLE:
			if( noun != UNDEF ) goto bad;
			noun = NORM(t[i]);
			continue;
			}
		}

	/* now, construct final type */
	if( noun == UNDEF ) noun = INT;
	else if( noun == FLOAT ){
		if( sign != INT || adj == SHORT ) goto bad;
		return( tm + (adj==LONG ? DOUBLE : FLOAT) );
		}
	else if( (noun == UCHAR || noun == CHAR) && adj != INT ) goto bad;


	if( adj != INT ) noun = adj;
	if( sign == UNSIGNED && noun != UCHAR)
	  return( tm + noun + (UNSIGNED-INT) );
	else if( sign == SIGNED  && noun == UCHAR) return( tm + CHAR );
	else return( tm + noun );
	}

NODE *
tymerge( typ, idp ) NODE *typ, *idp; {
	/* merge type typ with identifier idp  */

	register unsigned t;
	register i;
	register function_prototype_list head, p, q;
	extern int eprint();
	NODE tmp;
	
	if( typ->in.op != TYPE ) cerror( "tymerge: arg 1" );
	if(idp == NIL ) return( NIL );

# ifndef BUG1
	if( ddebug > 2 ) fwalk( idp, eprint, 0 );
# endif

	idp->in.type = typ->in.type;
	idp->fn.cdim = curdim;
	tyreduce( idp );
	idp->fn.csiz = typ->fn.csiz;

	for( t=typ->in.type, i=typ->fn.cdim; NORM(t)&TMASK; t = DECREF(t) ){
		if( ISARY(t) ) dstash( dimtab[i++] );
		}

	/* now idp is a single node: fix up type */
	idp->in.type = ctype( idp->in.type );
        if( !is_prototype_stack_empty() )
          idp->in.prototype = pop_function_prototype();
        else idp->in.prototype = NULL;
	if( !is_prototype_stack_empty() ) {
	/* multiple function declarators in declaration requires indirection*/
	  tmp.tn.rval = 0; tmp.tn.type = UNDEF;
          head = make_prototype( tmp, MULTIPLE_PROTOTYPES );
          p = head;
          p->pred = idp->in.prototype;
          while ( !is_prototype_stack_empty() ) {
	    p = make_prototype( tmp, MULTIPLE_PROTOTYPES );
            p->pred = pop_function_prototype();
	    q = tail_of_prototype_list(head);
	    q->succ = p;
          }
          idp->in.prototype = head;
	}
	if((t = BTYPE(idp->in.type)) != STRTY && t != UNIONTY && t != ENUMTY ){
		idp->fn.csiz = t;  /* in case ctype has rewritten things */
		}

	return( idp );
	}

tyreduce( p ) register NODE *p; {

/* build a type, and stash away dimensions, from a parse tree of the    */
/* declaration the type is build top down, the dimensions and prototype */
/* bottom up.								*/
	register o, temp;
	register unsigned t;

	o = p->in.op;
	p->in.op = FREE;

	if( o == NAME ) return;

	if( o == UNARY_VOLATILE_MUL ) {
	  if(  (((~BTMASK&p->in.type)<<(TSHIFT*2))>>(TSHIFT*2)) ==
              (~BTMASK&p->in.type)) /* too many type qualifiers ? */
	    t = INCREFVP( p->in.type );
          else{
            WERROR( MESSAGE ( 131 ) );
	    t = INCREF( p->in.type );
          }
        }
	else  t = INCREF( p->in.type );
	if( o == UNARY CALL )   t += (FTN-PTR);
	else if( o == LB ){
		t += (ARY-PTR);
		temp = p->in.right->tn.lval;
		p->in.right->in.op = FREE;
		if( ( temp == 0 ) & ( p->in.left->tn.op == LB ) )
			/* "null dimension" */
			UERROR( MESSAGE( 85 ) );
		}

	p->in.left->in.type = t;
	tyreduce( p->in.left );

	if( o == LB ) dstash( temp );
        if( (o == UNARY CALL) && (p->in.prototype != NULL) ) {
	  push_function_prototype( p->in.prototype );
          p->in.prototype = NULL;
        }
	p->tn.rval = p->in.left->tn.rval;
	p->in.type = p->in.left->in.type;
	}

fixtype( p, class ) register NODE *p; {
	register unsigned t, type;
	register mod1, mod2;
	
	/* fix up the types, and check for legality */

	if( (type = p->in.type) == UNDEF ) return;
	if( mod2 = (type&TMASK) ){
		t = DECREF(type);
		while( mod1=mod2, mod2 = (t&TMASK) ){
			if( ISEQUAL(mod1,ARY) && ISEQUAL(mod2,FTN) ){
				/* "array of functions is illegal" */
				UERROR( MESSAGE( 14 ) );
				type = 0;
				}
			else if( ISEQUAL(mod1, FTN) && ( ISEQUAL(mod2 ,ARY) ||
			         ISEQUAL(mod2, FTN) ) ){
				/* "function returns illegal type" */
				UERROR( MESSAGE( 47 ) );
				type = 0;
				}
			t = DECREF(t);
			}
		}

	/* detect function arguments, watching out for structure declarations */
	/* for example, beware of f(x) struct [ int a[10]; } *x; { ... } */
	/* the danger is that "a" will be converted to a pointer */

	if( class==SNULL && blevel==1 && !(instruct&(INSTRUCT|INUNION)) ) class = PARAM;
	if( class == PARAM || ( class==REGISTER && blevel==1 ) ){
		if( ISEQUAL(type, FLOAT) ) {
		  /* If no prototype, then promote to double, */
		  /* otherwise keep as float.		      */
	          if( stab[curftn].sprototype  == NULL) type = DOUBLE;
		} else if( ISARY(type) ){
			++p->fn.cdim;
			type += (PTR-ARY);
			}
		else if( ISFTN(type) ){
			/* "a function is declared as an argument" */
			WERROR( MESSAGE( 11 ) );
			type = INCREF(type);
			}

		}

	if( instruct && ISFTN(type) ){
		/* "function illegal in structure or union"  */
		UERROR( MESSAGE( 46 ) );
		type = INCREF(type);
		}
	p->in.type = type;
	}

uclass( class ) register class; {
	/* give undefined version of class */
	if( class == SNULL ) return( EXTERN );
	else if( class == STATIC ) return( USTATIC );
	else if( class == FORTRAN ) return( UFORTRAN );
	else return( class );
	}

fixclass( class, type ) TWORD type; {
	register int cir;

	/* first, fix null class */

	if( class == SNULL ){
		if( instruct&INSTRUCT ) class = MOS;
		else if( instruct&INUNION ) class = MOU;
		else if( blevel == 0 ) class = EXTDEF;
		else if( blevel == 1 ) class = PARAM;
		else class = AUTO;

		}

	/* now, do general checking */

	if( ISFTN( type ) ){
		switch( class ) {
		default:
			/* "function has illegal storage class" */
			UERROR( MESSAGE( 45 ) );
		case AUTO:
			class = EXTERN;
		case EXTERN:
		case EXTDEF:
		case FORTRAN:
		case TYPEDEF:
		case STATIC:
		case UFORTRAN:
		case USTATIC:
			;
			}
		}

	if( class&FIELD ){
		/* "illegal use of field" */
		if( !(instruct&INSTRUCT) ) UERROR( MESSAGE( 72 ) );
		return( class );
		}

	switch( class ){

	case MOU:
		/* "illegal class" */
		if( !(instruct&INUNION) ) UERROR( MESSAGE( 52 ) );
		return( class );

	case MOS:
		/* "illegal class" */
		if( !(instruct&INSTRUCT) ) UERROR( MESSAGE( 52 ) );
		return( class );

	case MOE:
		/* "illegal class" */
		if( instruct & (INSTRUCT|INUNION) ) UERROR( MESSAGE( 52 ) );
		return( class );

	case REGISTER:
		/* "illegal register declaration" */
		if( blevel == 0 ) UERROR( MESSAGE( 68 ) );
		return (class);					  

	case AUTO:
	case LABEL:
	case ULABEL:
		/* "illegal class" */
		if( blevel < 2 ) UERROR( MESSAGE( 52 ) );
		return( class );

	case PARAM:
		/* "illegal class" */
		if( blevel != 1 ) UERROR( MESSAGE( 52 ) );
		return( class );

	case UFORTRAN:
	case FORTRAN:
# ifdef NOFORTRAN
			NOFORTRAN;    /* a condition which can regulate the FORTRAN usage */
# endif
		/* "fortran declaration must apply to function" */
		if( !ISFTN(type) ) UERROR( MESSAGE( 40 ) );
		else {
			type = DECREF(type);
			if( ISFTN(type) || ISARY(type) || ISPTR(type) ) {
				/* "fortran function has wrong type" */
				UERROR( MESSAGE( 41 ) );
				}
			}
	case STNAME:
	case UNAME:
	case ENAME:
	case EXTERN:
	case STATIC:
	case EXTDEF:
	case TYPEDEF:
	case USTATIC:
		return( class );

	default:
		cerror( "illegal class: %d", class );
		/* NOTREACHED */

		}
	}

struct symtab *
mknonuniq(idindex) int *idindex; {/* locate a symbol table entry for */
	/* an occurrence of a nonunique structure member name */
	/* or field */
	register i;
	register struct symtab * sp;
	char *p,*q;

	sp = & stab[ i= *idindex ]; /* position search at old entry */
	while( !ISEQUAL(sp->stype, TNULL) ){ /* locate unused entry */
		if( ++i >= stabmax ){/* wrap around symbol table */
			i = 0;
			sp = stab;
			}
		else ++sp;
		if( i == *idindex ) cerror("Symbol table full");
		}
	sp->sflags = SNONUNIQ | SMOS;
	sp->sname = stab[*idindex].sname;
	*idindex = i;
#ifndef BUG1
	if ( ddebug )
	{
		printf( "\tnonunique entry for %s from %d to %d\n",
			sp->sname, *idindex, i );
	}
#endif
	return ( sp );
	}

lookup( name, s ) /* look up name: must agree with s w.r.t. STAG, SMOS and SHIDDEN */
#ifdef FLEXNAMES
	register
#endif
	char *name;
{ 

	register
#ifndef FLEXNAMES
		char *p, *q;
#endif
	int i, j, ii;
	register struct symtab *sp;

	/* compute initial hash index */
# ifndef BUG1
	if( ddebug > 2 ){
#ifdef FLEXNAMES
		printf( "lookup( %s, %d ), stwart=%d, instruct=%d\n",
			name, s, stwart, instruct );
#else
		printf( "lookup( %.8s, %d ), stwart=%d, instruct=%d\n",
			name, s, stwart, instruct );
#endif
		}
# endif

#ifdef FLEXNAMES
	i = (int) name;
        /* so code can be ported to machines where sign bit is set,e.g DG */
	if( i < 0 ) i = -i;
#else
	i = 0;
	for( p=name, j=0; *p != '\0'; ++p ){
		i = (i<<1)+ *p;
		if( ++j >= NCHNAM ) break;
		}
#endif
       
        i = i%stabmax;
	sp = &stab[ii=i];

	for(;;){ /* look for name */

		if( ISEQUAL(sp->stype, TNULL) ){ /* empty slot */
			sp->sflags = s;  /* set STAG, SMOS if needed, turn off all others */
#ifdef FLEXNAMES
			sp->sname = name;
#else
			p = sp->sname;
			for( j=0; j<NCHNAM; ++j ) if( *p++ = *name ) ++name;
#endif
			sp->stype = UNDEF;
			sp->sclass = SNULL;
			return( i );
			}
		if( (sp->sflags & (STAG|SMOS|SHIDDEN)) != s ) goto next;
#ifdef FLEXNAMES
		if ( sp->sname == name )
			return ( i );
#else
		p = sp->sname;
		q = name;
		for( j=0; j<NCHNAM;++j ){
			if( *p++ != *q ) goto next;
			if( !*q++ ) break;
			}
		return( i );
#endif
	next:
		if( ++i >= stabmax ){
			i = 0;
			sp = stab;
			}
		else ++sp;
		if( i == ii ) cerror( "symbol table full" );
		}
	}

#ifndef checkst
/* if not debugging, make checkst a macro */
checkst(lev){
	register int s, i, j;
	register struct symtab *p, *q;

	for( i=0, p=stab; i< stabmax; ++i, ++p ){
		if( ISEQUAL(p->stype, TNULL) ) continue;
		j = lookup( p->sname, p->sflags&(SMOS|STAG) );
		if( j != i ){
			q = &stab[j];
			if( ISEQUAL(q->stype, UNDEF) ||
			    q->slevel <= p->slevel ){
#ifdef FLEXNAMES
				cerror( "check error: %s", q->sname );
#else
				cerror( "check error: %.8s", q->sname );
#endif
				}
			}
#ifdef FLEXNAMES
		else if( p->slevel > lev )
			cerror( "%s check at level %d", p->sname, lev );
#else
		else if( p->slevel > lev )
			cerror( "%.8s check at level %d", p->sname, lev );
#endif
		}
	}
#endif


/* For bug #2301 */
check_statics()
{
  register struct symtab *p;

  for( p=stab; p <= &stab[stabmax]; ++p ){
    if (ISEQUAL(p->stype,TNULL)) /* empty symtab entry */
      continue;			
    if( !ISEQUAL(p->sclass, USTATIC) ) /* not static? */
      continue;
    if (p->suse < 0)		/* referenced? */
      uerror("static function declared and referenced, but not defined: %s\n",
	     p->sname);
  }
}


struct symtab *
relook(p) register struct symtab *p; {  /* look up p again, and see where it lies */

	register struct symtab *q;

	/* I'm not sure that this handles towers of several hidden definitions in all cases */
	q = &stab[lookup( p->sname, p->sflags&(STAG|SMOS|SHIDDEN) )];
	/* make relook always point to either p or an empty cell */
	if( ISEQUAL(q->stype, UNDEF) ) {
		  q->stype = TNULL; 
		  return(q);
	}
	while( q != p ){
		if( ISEQUAL(q->stype,TNULL) ) break;
		if( ++q >= &stab[stabmax] ) q=stab;
		}
	return(q);
	}


remove_symbol( idn )
/* Removes  prototype identifiers from the symbol table. Their entered */
/* in the first place because at the point of encountering them it's   */
/* unknown if we are in a function declaration context or function     */
/* definition context. If it's a function definition context they are  */
/* delted with like identifiers in argument lists, i.e. stashed away by*/
/* ftnarg then retrieved by dclargs.				       */

        register int idn; {
	register struct symtab *p;

	p = &stab[idn];
	if( p->sflags & SHIDES ) unhide(p);
	free_prototype_list(p->sprototype);
        p->sprototype = NULL;
        p->slevel = 0; 
	p->stype = TNULL;
} /* clear_symbol_entry */


static  void
substitute_schain_entry( x, y, index )
struct symtab *x, *y;
int index;  {
/* if x is in schain[index], substitute y for it. it's assumed that */
/* x->snext == y->snext.					    */

struct symtab *q, *p; /* q follows p */

  for ( p = q = schain[index]; ; q = p, p = p->snext )
     if( p == NULL ) return; /* not in list */
     else if( p == x ) {
            if( q == p ) schain[index] = y;
            else q->snext = y;
	    return;
     }
} /* substitute_schain_entry */  

clearst( lev ) register int lev; {
	register struct symtab *p, *q;
	register int temp;
	struct symtab *clist = 0;

	temp = lineno;

	/* step 1: remove entries */
	while( chaintop-1 > lev ){
		register int type;

		p = schain[--chaintop];
		schain[chaintop] = 0;
		for( ; p; p = q ){
			q = p->snext;
			type = p->stype;
			if( p->stype == TNULL || p->slevel <= lev )
				cerror( "schain botch" );
			lineno = p->suse < 0 ? -p->suse : p->suse;
			if( p->stype==UNDEF||( p->sclass==ULABEL && lev<2 ) ){
				lineno = temp;
				uerror( "%s undefined", p->sname );
			}
# ifndef BUG1
			if( ddebug ){
				printf( "removing %s", p->sname );
				printf( " from stab[%d], flags %o level %d\n",
					p-stab, p->sflags, p->slevel);
				}
# endif
			if( p->sflags & SHIDES )unhide( p );
			p->stype = TNULL;
			free_prototype_list(p->sprototype);
			p->sprototype = NULL;
			p->snext = clist;
			/* Bugfix #1702,#2314 */
			p->sizoff = -1;
			if (clist) {
			  clist->offset = (int) p; /* backpointer */
			}
			clist = p;
			}
		}

	/* step 2: fix any mishashed entries */
	p = clist;
	
	while( p ){
		register struct symtab *r,  *next;

		q = p;
		next = p->snext;
		p->sizoff = 0;	/* remove clist membership flag */
		for(;;){
			if( ++q >= &stab[stabmax] )q = stab;
			if( q == p || q->stype == TNULL )break;
			if( (r = relook(q)) != q ) {
			  /* rehash due to hidden variable or collison */
# ifndef BUG1
			  if( ddebug ){
			    printf( "moving %s", q->sname );
			    printf( " from stab[%d], flags %o level",
				   q-stab, q->sflags, q->slevel);
			    printf( " to stab[%d]\n", r-stab);
			  }
# endif
			  /* Bug fix #1702,#2314 */
			  if (r->sizoff == -1) {
			    struct symtab t; /* swap 'em */
			    t = *q;
			    *q = *r;
			    *r = t;
			    /* Fix up clist */
			    if (r == next) { 
			      next = q;
			    }
			    else {
			      ((struct symtab *) (q->offset)) -> snext = q;
			    }
			  }
			  else {
			    *r = *q;
			    q->stype = TNULL;
			  }
			  if( (r->sclass == LABEL) || (r->sclass==ULABEL) ) {
			    /*scope of label is entire function (blevel = 2),*/
		            /*remap schain entry to point to r.              */
			      
			      substitute_schain_entry( q, r, 2 );
			  }
			 }
			}
		p = next;
		}

	lineno = temp;
}


hide( p ) register struct symtab *p; {
	register struct symtab *q;
	for( q=p+1; ; ++q ){
		if( q >= &stab[stabmax] ) q = stab;
		if( q == p ) cerror( "symbol table full; increase default option: -Wf,-XNd%d", stabmax );
		if( ISEQUAL(q->stype, TNULL) ) break;
		}
	*q = *p;
	p->sflags |= SHIDDEN;
        q->sprototype = NULL;
	q->sflags = (p->sflags&(SMOS|STAG)) | SHIDES;
	/* "%.8s redefinition hides earlier one" */
	/* "%s redefinition hides earlier one" */
	if( hflag ) WERROR( MESSAGE( 2 ), p->sname );
# ifndef BUG1
	if( ddebug ) printf( "\t%d hidden in %d\n", p-stab, q-stab );
# endif
	return( idname = q-stab );
	}

unhide( p ) register struct symtab *p; {
	register struct symtab *q;
	register s, j;

	s = p->sflags & (SMOS|STAG);
	q = p;

	for(;;){

		if( q == stab ) q = &stab[stabmax-1];
		else --q;

		if( q == p ) break;

		if( (q->sflags&(SMOS|STAG)) == s ){
#ifdef FLEXNAMES
			if ( p->sname == q->sname ){
#else
			for( j =0; j<NCHNAM; ++j ) if( p->sname[j] != q->sname[j] ) break;
			if( j == NCHNAM ){ /* found the name */
#endif
				q->sflags &= ~SHIDDEN;
# ifndef BUG1
				if( ddebug ) printf( "unhide uncovered %d from %d\n", q-stab,p-stab);
# endif
				return;
				}
			}

		}
	cerror( "unhide fails" );
	}


