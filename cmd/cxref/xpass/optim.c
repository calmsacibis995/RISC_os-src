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
#ident	"$Header: optim.c,v 1.5.2.3 90/05/09 15:37:43 wje Exp $"
/* $Log:	optim.c,v $
 * Revision 1.5.2.3  90/05/09  15:37:43  wje
 * add restricted rights legend
 * 
 * Revision 1.5.2.2  89/12/10  22:26:23  wje
 * add RID 1.6 to branch RISCOS_4_50_FIXES
 * 
 * Revision 1.6  89/12/10  18:42:29  jay
 * replaced with files from 2.0 C compiler's lint, modified to support cxref.
 * closes bugs 1373
 * 
 * Revision 2000.7  89/03/26  14:48:18  bettina
 * 2.0G
 * 
 * Revision 1.3  87/12/16  14:32:21  gb
 * fix bug #1956, spurious err on cast in a constant expression init.
 * 
 * Revision 1.2  87/12/09  11:41:57  gb
 * added $Log keyword
 *  */

# include "mfile1"
# include "messages.h"
/* ref.		date		description				*/
/* !01		25mar85		tools didn't support shift ucode operations */
/* !02		12jul85		no way to create indirect stores to absolute
				addresses in ucode, so keep indirection.    */
/* !03		18decf85	most optimizations didn't apply to mips, some*/
/*				are still done to satisfy assumptions made in*/
/*				other parts of compiler.		     */

# define SWAP(p,q) {sp=p; p=q; q=sp;}
# define RCON(p) (p->in.right->in.op==ICON)
# define RO(p) p->in.right->in.op
# define RV(p) p->in.right->tn.lval
# define LCON(p) (p->in.left->in.op==ICON)
# define LO(p) p->in.left->in.op
# define LV(p) p->in.left->tn.lval
# define LFCON(p) (p->in.left->in.op==FCON)

int oflag = 0;

NODE *
fortarg( p ) NODE *p; {
	/* fortran function arguments */

	if( p->in.op == CM ){
		p->in.left = fortarg( p->in.left );
		p->in.right = fortarg( p->in.right );
		return(p);
		}

	while( ISPTR(p->in.type) ){
		p = buildtree( UNARY MUL, p, NIL );
		}
	return( optim(p) );
	}

	/* mapping relationals when the sides are reversed */
short revrel[] ={ EQ, NE, GE, GT, LE, LT, UGE, UGT, ULE, ULT };
NODE *
optim(p) register NODE *p; {
	/* local optimizations, most of which are probably machine independent */

	register o, ty;
	NODE *sp;
	int i;
	TWORD t;

	if( (t=BTYPE(p->in.type))==ENUMTY || t==MOETY ) econvert(p);
	if( oflag ) return(p);
	ty = optype( o=p->in.op);
	if( ty == LTYPE ) return(p);

	if( ty == BITYPE ) p->in.right = optim(p->in.right);
	p->in.left = optim(p->in.left);

	/* collect constants */

	switch(o){

	case SCONV:
	case PCONV:
		return( clocal(p) );

	case FORTCALL:
		p->in.right = fortarg( p->in.right );
		break;

	case UNARY AND:
		if( LO(p) != NAME ) cerror( "& error" );

		if( !andable(p->in.left) ) return(p);

		LO(p) = ICON;

		setuleft:
		/* paint over the type of the left hand side with the type of the top */
		p->in.left->in.type = p->in.type;
		p->in.left->fn.cdim = p->fn.cdim;
		p->in.left->fn.csiz = p->fn.csiz;
		p->in.op = FREE;
		return( p->in.left );

	case UNARY MUL:
		if( LO(p) != ICON ||
		  ( LO(p) == ICON &&p->in.left->tn.rval == NONAME))break;/*!02*/
		LO(p) = NAME;
		goto setuleft;						 

	case MINUS:
		if( !nncon(p->in.right) ) break;
		RV(p) = -RV(p);
		o = p->in.op = PLUS;

	case MUL:
	case PLUS:
	case AND:
	case OR:
	case ER:
		/* commutative ops; for now, just collect constants */
		/* someday, do it right */
		if( nncon(p->in.left) || ( (LCON(p) || LFCON(p)) && !RCON(p) ) ) SWAP( p->in.left, p->in.right );
		/* make ops tower to the left, not the right */
		/*
		* mixture of pointer and non-pointer types at this
		* juncture causes ``pointer arithmetic'' scaling where
		* it is not appropriate. Also floating point arithmetic
		* doesn't always obey algebraic identities of mathematics.
		*/
		if( RO(p) == o && ( ISPTR( p->in.left->in.type ) ==
			ISPTR( p->in.right->in.left->in.type ) ) &&
		   	!ISEQUAL(p->in.right->in.type,FLOAT) && 
		        !ISEQUAL(p->in.right->in.type,DOUBLE) )
		{
			register op;
			op = p->in.right->in.op;
			sp = buildtree( op, p->in.left,
				p->in.right->in.left );
			p->in.right->in.op = FREE;
			op = p->in.op;
			p->in.op = FREE;
			p = optim( buildtree( op, sp, p->in.right->in.right ) );
			}
		if(o == PLUS && LO(p) == MINUS && RCON(p) && RCON(p->in.left) &&
		  conval(p->in.right, MINUS, p->in.left->in.right)){
			zapleft:
			RO(p->in.left) = FREE;
			LO(p) = FREE;
			p->in.left = p->in.left->in.left;
		}
		if( RCON(p) && LO(p)==o && RCON(p->in.left) && conval( p->in.right, o, p->in.left->in.right ) ){
			goto zapleft;
			}
		else if( LCON(p) && RCON(p) && conval( p->in.left, o, p->in.right ) ){
			zapright:
			RO(p) = FREE;
			p->in.left = makety( p->in.left, p->in.type, p->fn.cdim, p->fn.csiz );
			p->in.op = FREE;
			return( clocal( p->in.left ) );
			}

		/* change muls to shifts */

/*		if( o==MUL && nncon(p->in.right) && (i=ispow2(RV(p)))>=0){
			if( i == 0 ){  multiplication by 1 
				goto zapright;
				}
			o = p->in.op = LS;
			p->in.right->in.type = p->in.right->fn.csiz = INT;
			RV(p) = i;
			}						  !01*/

		/* change +'s of negative consts back to - */
		if( o==PLUS && nncon(p->in.right) && RV(p)<0 ){
			RV(p) = -RV(p);
			o = p->in.op = MINUS;
			}
		break;

	case DIV:
		if( nncon( p->in.right ) && p->in.right->tn.lval == 1 ) goto zapright;
		break;

	case EQ:
	case NE:
	case LT:
	case LE:
	case GT:
	case GE:
	case ULT:
	case ULE:
	case UGT:
	case UGE:
		break; /* doesn't apply to mips !03*/

		}

	return(p);
	}

ispow2( c ) CONSZ c; {
	register i;
	if( c <= 0 || (c&(c-1)) ) return(-1);
	for( i=0; c>1; ++i) c >>= 1;
	return(i);
	}

nncon( p ) NODE *p; {
	/* is p a constant without a name */
	return( p->in.op == ICON && p->tn.rval == NONAME );
	}



extern void
eval_const_expr(p)
NODE *p;
{
  NODE t, *l, *r;
  extern NODE  *clocal(); 

  l =  p;
  if( l->in.op == SCONV ) {
    l = clocal(p ) ;			/* convert the constant */
    if( (l->in.op == SCONV) &&
        (l->in.left->in.op == ICON) &&
        ISPTR(l->in.left->in.type)) {
        l->in.op = FREE;
	l = l->in.left;
        eval_const_expr(l);
    } else 
      if( (l->in.op == SCONV) &&
	  (l->in.left->in.op == FCON) &&
	  !((ISEQUAL(l->in.type,FLOAT) || ISEQUAL(l->in.type,DOUBLE)))  &&
	   (ISEQUAL(l->in.left->in.type,FLOAT) || 
           (ISEQUAL(l->in.left->in.type,DOUBLE)))) {
		r = block( ICON, (NODE *)NULL, (NODE *)NULL, INT, 0, INT );
		r->tn.lval = (int) l->in.left->fpn.dval;
		r->tn.rval = NONAME;
		l->in.left->in.op = FREE;
		l->in.op = FREE;
		l = r;
    }
  }
  if( (optype(l->in.op) == BITYPE) && LCON(p) && RCON(p) &&
      conval(l->in.left, l->in.op, l->in.right) ) { /* evaluate  expression */
     t = *l->in.left;    
     tfree(p);
     *p = t;
     return;
  } 
  if( (optype(l->in.op) == UTYPE) && LCON(p) &&
      conval(l->in.left, l->in.op, l->in.right) ) { /* evaluate  expression */
     t = *l->in.left;    
     tfree(p);
     *p = t;
     return;
  } 
#ifndef CXREF
/* in cxref, the cpp does not do the substitions for defined constants,
 * rather, it passes them through so one can see the defined/refed use
 * of them.  This means lot's of spurious error msgs if this check is enabled.
 */

  if( (l->in.op != ICON)  && (l->in.op != FCON)){
      /* "constant expected" */
      UERROR( MESSAGE( 23 ));
  }
#endif
  if( p != l ) {
    *p = *l;
    l->in.op = FREE;
  }
}

