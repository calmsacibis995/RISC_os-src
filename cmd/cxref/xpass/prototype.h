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
/* $Header: prototype.h,v 1.1.1.2 90/05/09 15:38:40 wje Exp $ */
/* $Log:	prototype.h,v $
 * Revision 1.1.1.2  90/05/09  15:38:40  wje
 * add restricted rights legend
 * 
 * Revision 1.1.1.1  89/12/10  22:19:38  wje
 * Branch RISCOS_4_50_FIXES off of trunk.
 * 
 * Revision 1.1  89/12/10  18:42:57  jay
 * Initial revision
 * 
 * Revision 2000.7  89/03/26  14:48:50  bettina
 * 2.0G
 * 
 * Revision 1.2  87/12/09  11:42:06  gb
 * added $Log keyword
 *  */

/* flags for function_prototype structure */
  /* indicates symbol is a prototype declarator */
# define PROTOTYPE_DECLARATOR 0x1
  /* multiple function declarators */
# define MULTIPLE_PROTOTYPES  0x2


struct function_prototype {
        NODE prototype;  /* parameter type */
	unsigned long flags;      /* various flags */
        function_prototype_list succ, pred;
};


extern function_prototype_list
  add_prototype(),
  make_prototype(),
  head_of_prototype_list(),
  next_parameter_prototype(),
  tail_of_prototype_list(),
  pop_function_prototype(),
  formal_type(),
  signature(),
  curprototype;
   

extern void
  free_prototype_list(),
  push_function_prototype(),
  enter_prototype_scope(),
  exit_prototype_scope(),
  valid_argument_expresssions(),
  valid_argument_declaration();

extern long
  num_of_formals();

extern boolean
  prototype_lists_equal(),
  is_prototype_id(),
  is_prototype_stack_empty(),
  in_prototype_scope();

extern NODE*
  typecheck_parameters();

# define ARE_TYPES_EQUAL(x,y) (x->prototype.tn.type == y->prototype.tn.type)


