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
#ident	"$Header: prototype.c,v 1.1.1.2 90/05/09 15:38:33 wje Exp $"

/* $Log:	prototype.c,v $
 * Revision 1.1.1.2  90/05/09  15:38:33  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:19:17  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:42:49  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:46  bettina
 * 2.0G
 * 
 * Revision 1.3  88/01/12  11:03:54  gb
 * fix #2126, restoring -float option after unprototyped function calls.
 * 
 * Revision 1.2  87/12/09  11:42:03  gb
 * added $Log keyword
 *  */


#include "mfile1"
#include "messages.h"

/*
This file contains the representations and the operations for managing
prototype lists. The lists are built during parsing in cgram.y by 
calling make_prototype and add_prototype when reducing to a formal
parameter of a prototype. The list of formal parameter types are then
attached to the function name in the symbol table.



Picture of representations for a typical (function f) and an intricate
(function fpfi) prototype:


int f(int, float, char);
int (*fpfi(int (*)(long), int))(int, ...);



symbol table		prototype list for function f
_________	  ________	_______         ________
|f       |------->|      |      |      |        |      |
|________|	  |      |<---->|      |<-----> |      |
|        |	  |int   |      |float |.	| char |
|________|        |______|      |______|	|______|
|	 |
|________|		protototype list for fpfi
|
|fpfi    |------>________ 	________	_________
|________|	 |      |	|      |	|       |
|        |	 |	|	|      |	|	|
|________|	 |	<------->int   |<------->varargs|
		 |______|	|______|	|_______|
		  |
		  |		________	________
		  |		|      |<------>|       |
		  |		|______|	|long___|
		  |                ^
		  |		   |
		  | 		   |
	         _V______	__________	 ________
		 |	|	|	 |	 |	 |
		 |	|	|	 |	 |	 |
		 |      <------->        <-------->	 |
		 |______|	|int (*)__|	 |__int__|

*/




/* Stack used stash away prototypes of a declaration as the prototype is */
/* built bottom up from a parse tree. The rightmost prototype will be on */
/* top of the stack. The stack is also used to save the prototype of a   */
/* function argument in a nested function call.				 */
static struct prototype_stack {              
  function_prototype_list funct_proto;
  struct prototype_stack *pred;
  int precision; /* floating point precision for evaluating arguments */
} *prototype_sp = NULL;  

static int plevel = 0;     /* function prototype scope level, 0 = outermost */
static long saved_curclass;/* class of outermost prototype */

function_prototype_list
next_parameter_prototype( list ) 
  register function_prototype_list list; {
/* Returns the next formal parameter type for a prototype */  
  if( list == NULL ) return list;
  else return list->succ;
} /* next_parameter_prototype */


function_prototype_list
add_prototype( prototype_list, prototype )
  register function_prototype_list prototype_list, prototype; {
  register function_prototype_list fp;
 /* Adds a formal parameter type to the end of a  prototype list */  
  if( prototype_list == prototype ) return prototype_list;
  if( !prototype || !prototype_list )
    cerror("prototype list corrupted");
  fp = tail_of_prototype_list(prototype_list);
  fp->succ = prototype;
  prototype->pred = fp;
  return prototype_list;
} /* add_prototype */


function_prototype_list
make_prototype( type, declflag ) 
  register int declflag; 		
  /*register*/ NODE  type; {
  register function_prototype_list fp;
  register NODE *p;
/* Makes a formal parameter type  */
  
  if( !(fp = (function_prototype_list )
                 malloc( sizeof(struct function_prototype))) )
    cerror("ran out of memory while constructing prototype list");
  fp->succ = fp->pred = NULL;
  fp->prototype = type;
  fp->prototype.in.op = FREE;
  fp->flags = declflag ;
  if( type.tn.rval && (declflag & PROTOTYPE_DECLARATOR) ) {
    /* save identifier in case it's a function definition, where*/
    /* rval is symbol table index for identifier. Basically     */
    /* mimicking method used to declare a parameter that is     */
    /* mentioned as a  formal parameter, but not declared in the*/
    /* declaration list.					*/
    ftnarg( type.tn.rval ); 
    stab[paramstk[ paramno-1 ]].slevel = 1; /* treat like a parameter */
  }else  /* else if there is an id, ignore it */
  if( declflag & PROTOTYPE_DECLARATOR )
    fp->flags = declflag & ~PROTOTYPE_DECLARATOR  ;
  return fp;
} /* make_prototype */

void
free_prototype_list ( list ) 
  register function_prototype_list list; {
  register function_prototype_list l = list;
/* Frees prototype list */
  if( list == NULL ) return;
  do {
    list = next_parameter_prototype(list);
    free( (char * ) l);
    l = list;
  } while( list != NULL );
} /* free_prototype_list */



function_prototype_list
head_of_prototype_list ( list )
  register function_prototype_list list; {

  if( list == NULL ) return list;
  for( ; list->pred != NULL; list = list->pred) 
     if( ISEQUAL(list->prototype.tn.type, UNDEF))
       UERROR( MESSAGE( 143 ) );
  return list;
} /* head_of_prototype_list */

function_prototype_list
tail_of_prototype_list ( list )
  register function_prototype_list list; {

  if( list == NULL ) return list;
  for( ; list->succ != NULL; list = list->succ)  ;
  return list;
} /* tail_of_prototype_list */

boolean
prototype_lists_equal( x, y )
  register function_prototype_list x, y; {
/* Returns true if both lists match in type and number. Applied when */
/* two or more declarations for same functions is encountered.       */

  if( ( x != NULL) && ( y != NULL) ) 
    return (ARE_TYPES_EQUAL(x, y) && 
          prototype_lists_equal(next_parameter_prototype(x), 
 	  next_parameter_prototype(y)));
  else
  if( x == y ) return 1; /* x and y == NULL */
  else
    return 0;
} /* prototype_lists_equal */


static function_prototype_list
dereference_prototype( prototype, type )
  register function_prototype_list prototype;
  register TWORD type; {
  register int i = 0;
/* Returns the prototype for the current expression. If the prototype has */
/* multiple function declarators associated with it, the prototype        */
/* belonging to the specific indirection is returned. Example:            */
/* For the  declaration							  */
/*   int (*f( int (*) (long), int)) (int)				  */
/* the prototype for f would point to a list of two elements, where the   */
/* first would point to the prototype list containing (int) and the next  */
/* would point to the prototype list containing (*) (long) and int.	  */

  if( prototype == NULL ) return prototype;
  if( prototype->flags & MULTIPLE_PROTOTYPES ) {
    for( type = type & ~BTMASK; type ; type = DECREF(type))
    if( ISFTN( type )) i++;
    prototype = (formal_type(prototype, i))->pred;
  }
  return  prototype;
} /* dereference_prototype */

function_prototype_list 
signature( proc_name, type ) 
  register TWORD type;
  register NODE *proc_name;{
/* Returns the prototype list for function 'proc_name' in the context of */
/* the expression with type 'type'.		 		         */
 
  if( (proc_name->in.op == NAME) ) {
    if( proc_name->tn.prototype != NULL ) {
    	return proc_name->tn.prototype;
    } else 
    if( proc_name->tn.rval >= 0) {
      return dereference_prototype(stab[proc_name->tn.rval].sprototype, type);
    } else {
      return NULL;
    }
  } else {
  if( (proc_name->in.op == ICON) ) {
    if( proc_name->tn.rval != NONAME) {
      return dereference_prototype(stab[proc_name->tn.rval].sprototype, type);
    }else {
      return NULL;
    }
  } else 
  if( (proc_name->in.op == OREG)   || (proc_name->in.op == REG) ) {
    return dereference_prototype(proc_name->in.prototype, type);
  } else 
  if ( proc_name->in.left != NULL ) {
    return( signature( proc_name->in.left, type ) );
  } else {
    return NULL; 
  } 
}
} /* signature */

long
num_of_formals ( list )
  register function_prototype_list list; {
  register int n = 0;
/* Returns the number of formals parameters in a prototype list */
  
  for( ; list != NULL; list = next_parameter_prototype(list)) 
    if( (list->prototype.tn.type != VARARG)  && 
        (list->prototype.tn.type != UNDEF)) n++;
  return n;
} /* num_of_formals */


function_prototype_list
formal_type( list, parameter_number )
  register int parameter_number;
  register function_prototype_list list; {
  register int j;
/* Returns the formal type in the prototype list for parameter position */
/* parameter_number.							*/
  for( j=1; parameter_number > j; j++ ) {
    if( list == NULL ) return list;
    else
    if (list->prototype.tn.type == VARARG ) return list;
    else list = next_parameter_prototype(list) ;
  }
  return list;
} /* formal_type */

static NODE *
typecheck_scalar_argument( formal, actual )
  register function_prototype_list formal;
  register NODE *actual;{
  register TWORD t;
  register int d,s,size;
  register NODE *p;

  t = formal->prototype.tn.type;
  d = formal->prototype.fn.cdim;
  s = formal->prototype.fn.csiz;
  size = tsize( t, d, s);
  if( size < INT ) {
  /* prevent generating code for argument assignment conversions  */
  if(ISUNSIGNED(t)) MODTYPE(t, UNSIGNED);
  else MODTYPE(t, INT);
  s = SZINT;   
  }
  p = block( NAME, NIL, NIL, t, d, s);
  p = buildtree( PASSIGN, p, actual); /* type match and conversions */
  actual = p->in.right;
  p->in.left->in.op = p->in.op = FREE;
  p->in.right = NULL;
  tfree( p );
  return actual;
} /* typecheck_scalar_argument */


int static
bounds_check(formal, actual ) 
  NODE formal, actual; {
  register int formal_dim, actual_dim;
  register TWORD formal_type, actual_type;
/* Checks agreement on the number of dimensions and on the bounds for each */
/* dimension except the first.						   */

  /* arrays converted to pointers to first element */
  formal_type = DECREF( formal.in.type );/* remove first dimension*/
  formal_dim  = formal.fn.cdim + 1;
  if( !ISPTR(actual.tn.type ) ) {
   UERROR( MESSAGE( 140 ) );
   return 1;
  }
  actual_type= DECREF(actual.tn.type); /* normalize by removing pointer */
  actual_dim = actual.fn.cdim;  

  if( ISARY( actual_type ) && ISARY( formal_type ))
    /* check agreement on the number of dimensions */
    do {
      if ( !ISARY( formal_type ) || !ISARY( actual_type )) {
        UERROR( MESSAGE( 140 ) );
        return 1;
      }
      if( dimtab[formal_dim] != dimtab[actual_dim] ) {
        UERROR( MESSAGE( 141 ) );
        return 1 ;
      }
      formal_dim = ++formal_dim;
      actual_dim = ++actual_dim;
      formal_type = DECREF( formal_type );
      actual_type = DECREF( actual_type );  
    } while ( ISARY( formal_type ) || ISARY( actual_type ) );
    return 0;
 } /* bounds_check */


static NODE *
typecheck_array_argument( formal, actual ) 
  register function_prototype_list formal;
  register NODE *actual; {
  register int formal_dim, actual_dim;
  register TWORD formal_type, actual_type;
  NODE t;
/* Checks array arguments for agreement on the number of dimensions, */
/* the bounds for each dimension except the first, and the type of   */
/* the array.							     */
  if ( bounds_check(formal->prototype, *actual ) )
    return actual;
  /* check agreement on the type pointed to */
  t =  formal->prototype;
  t.tn.type = INCREF( DECREF( t.tn.type ) );
  formal = make_prototype(t, 0 );
  actual = typecheck_scalar_argument( formal, actual );
  free_prototype_list( formal );
  return actual;
} /* typecheck_array_argument */

static NODE *
typecheck_structure_argument( formal, actual ) 
  register function_prototype_list formal;
  register NODE *actual; {
/* Checks to see if structures are assignment compatible */
 
  
    if( !ISEQUAL(formal->prototype.fn.type, actual->fn.type )  ||
        (!ISEQUAL(formal->prototype.fn.type, STRTY) &&
        !ISEQUAL(formal->prototype.fn.type, UNIONTY)) ||
        formal->prototype.fn.csiz != actual->fn.csiz ) 
		UERROR( MESSAGE( 138 ) );
    return actual;
} /* typecheck_structure_argument */


static NODE *
typecheck_function_argument( formal, actual )
  register function_prototype_list formal;
  register NODE *actual; {
  NODE t;
/* Checks that pointer to functions arguments match. At some point,    */
/* checking should be done if the pointer to function has a prototype  */
/* against the actual argument.					       */

  t =  formal->prototype;
  t.tn.type = INCREF( t.tn.type  );  /* make it a pointer to function */
  formal = make_prototype(t, 0);
  actual = typecheck_scalar_argument( formal, actual );
  free_prototype_list( formal );
  return actual;
} /* typecheck_function_argument */


NODE*
typecheck_parameters( prototype_list, arg_list, num_of_actuals )
  register function_prototype_list prototype_list;
  int *num_of_actuals;
  NODE *arg_list;{
  register NODE *p;
  register function_prototype_list f_type;
  extern int fdebug, eprint();
/* Traverses the signature of the function and actuals, and verifies   */
/* that types match. Returns the actual argument list with conversions */  
/* inserted where required.					       */

  if( arg_list == NULL ) 
    return arg_list;
  if( fdebug ) fwalk( arg_list, eprint, 0 );	
  if ( arg_list->in.op == CM ) {
    arg_list->in.left = typecheck_parameters(prototype_list ,arg_list->in.left,
					     num_of_actuals);
    arg_list->in.right =typecheck_parameters(prototype_list,arg_list->in.right,
					     num_of_actuals);
    return arg_list;
  }
  ++*num_of_actuals;
  f_type = formal_type( prototype_list, *num_of_actuals );
  if( f_type == NULL ) {
    UERROR( MESSAGE( 139 ) ); /* too many arguments and no ellipsis */
    return arg_list;
  } 
  if( f_type->prototype.tn.type  == VARARG )
    return arg_list;  /* quit checking */
  if( arg_list->in.op == STARG )
    return typecheck_structure_argument( f_type, arg_list );
  else
  if( ISARY( f_type->prototype.tn.type) ) 
    return typecheck_array_argument ( f_type, arg_list );
  else 
  if( ISFTN(f_type->prototype.tn.type) ) 
    return typecheck_function_argument ( f_type, arg_list );
  else
    return typecheck_scalar_argument( f_type, arg_list );
} /* typecheck_parameters */


void
valid_argument_expressions( proc_name, call_node )
  register NODE *proc_name, *call_node; {
  register function_prototype_list formals;
  int formal_parameters = 0; /* number of formals */
  int actual_parameters = 0; /* number of actuals */
/* Checks that the argument list is compatible with the prototype */
/* for the function. Checks aren't performed if a prototype isn't */
/* in scope for the function.					  */
  formals = signature( proc_name, proc_name->tn.type );
  if( formals == NULL ) return; /* no function prototype declarator in scope */
  formal_parameters = num_of_formals( formals );
  call_node->in.right = typecheck_parameters( formals, call_node->in.right,
                                            &actual_parameters );
  if( !ISEQUAL(formals->prototype.tn.type,UNDEF) &&
     formal_parameters > actual_parameters)
    UERROR( MESSAGE ( 139 ) ); /* too few actuals parameters */
} /* valid_argument_expressions */


void
valid_argument_declaration( formal_type, actual_type )
  NODE  formal_type, actual_type; {
/* Checks consistency between a prototype that is in scope for a function */
/* and the argument declarations for its definition without a prototype.  */
  if( ISARY( formal_type.tn.type)  ) {
    if( bounds_check(formal_type, actual_type ) ) {
      UERROR(MESSAGE( 142 ) );
      return;
    }
    formal_type.tn.type = DECREF( formal_type.tn.type );
    /* array already converted, so normalize it */
    actual_type.tn.type = DECREF( actual_type.in.type );
    if( actual_type.tn.type != formal_type.tn.type ) 
        UERROR( MESSAGE( 142 ) );
  } else
  if( ISFTN( formal_type.in.type) ) {
    formal_type.in.type = INCREF ( formal_type.in.type );
    if( (formal_type.in.type != actual_type.in.type ) ||
        !prototype_lists_equal( actual_type.tn.prototype,
			        formal_type.tn.prototype) )
         UERROR( MESSAGE( 142 ) );
  } else
  if( formal_type.in.type != actual_type.in.type ||
    formal_type.fn.csiz != actual_type.fn.csiz )
    UERROR( MESSAGE( 142 ) );
} /* valid_argument_declaration */

void
push_function_prototype( list )
  function_prototype_list list; {
  register struct prototype_stack *sp;
  extern int singflag;
/* Pushes the prototype list onto the stack */

  if( !(sp = (struct prototype_stack *)malloc(sizeof(struct prototype_stack))))
    cerror("out of memory stacking prototype of function call");
  sp->funct_proto = list;
  sp->precision = singflag;
  sp->pred = prototype_sp;
  prototype_sp = sp;
} /* push_function_prototype */

function_prototype_list
pop_function_prototype() {
  register struct prototype_stack *sp;
  register function_prototype_list  fp;
  extern int floatflag, singflag;
/* Returns the prototype list on top of the stack */

  if( prototype_sp == NULL ) {
    singflag = floatflag; /* restore -float option after unproto'd func call */
    return NULL; /*  no prototype in scope */
  }

  sp = prototype_sp;
  fp = prototype_sp->funct_proto;
  singflag = prototype_sp->precision;
  prototype_sp = sp->pred;
  free( (char *) sp);
  return fp;
}  /* pop_function_prototype */
  

boolean
is_prototype_stack_empty() {
/* Returns true if prototype stack is empty */
  return (prototype_sp == NULL);
} /* is_prototype_stack_empty */


void
enter_prototype_scope() {
/* Entering a prototype scope */
  if( plevel == 0 ) {
  	saved_curclass = curclass;
  }  	
  plevel++;
} /* enter_prototype_scope */

void
exit_prototype_scope(fp) 
  function_prototype_list fp; {
/* Exiting a prototype scope */
  plevel--;
  if( plevel == 0 ) {
  	curclass = saved_curclass;
  }  	
} /* exit_prototype_scope */

boolean
is_prototype_id() {
/* Returns true if identifier just built is a prototype identifier. */
/* Prototype identifiers can be ignored, i.e. not entered into the  */
/* symbol table, if the prototype declaration occurs within the     */
/* following contexts: 					            */
/*   1) nested prototype declarations 				    */
/*   2) parameter list					            */
/*   3) function body						    */
  return 
    (plevel > 1)  ||		    /* id in nested prototype declaration */
    ((plevel == 1) && (blevel >= 1)); /* if blevel == 1, parameter list     */
				    /* context, otherwise function body   */
} /* is_prototype_id */

boolean
in_prototype_scope() {
/* Returns true if inside a prototype scope, false otherwise */
  return ( plevel > 0 );
}/* in_prototype_scope */

void
ignore_prototype_ids( list ) 
  register function_prototype_list list;  {
  register int i;
/* Identifiers in prototype scopes for a function declaration can be    */
/* ignored. Their put into the symbol table to begin with because at    */
/* the point where their encountered it's not known if it's declaration */
/* context or a definition context. For a definition context their put  */
/* into the symbol table so they can be handled like regular parameters.*/

  for( ; list != NULL; list = next_parameter_prototype(list)) 
 	 if( list->flags & PROTOTYPE_DECLARATOR && paramno) {
         	remove_symbol( paramstk[ --paramno ] );
	 }
  
} /* ignore_prototype_ids */


extern boolean
has_prototype_ids(function_prototype_list p)
{
	for( ; p != NULL; p = next_parameter_prototype(p)) {
 		if( p->flags & PROTOTYPE_DECLARATOR) {
			return TRUE;  
		}
	}
	return FALSE;
}

extern void
attach_prototype( NODE *t, function_prototype_list p)
{
	register opty =  optype(t->in.op);

	if( opty == LTYPE ) {
		t->in.prototype = p;  
		return;
	}
	attach_prototype(t->in.left, p);
}






