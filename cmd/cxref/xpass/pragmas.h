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
/* $Header: pragmas.h,v 1.1.1.2 90/05/09 15:38:01 wje Exp $ */


/* pragma types */
#define NEEDS_FRAMEPOINTER_PRAGMA	1	/* supports alloca'isms */

typedef long pragma_type ;
typedef struct token token;
typedef union pragma_node *pragma_node_ref;
typedef struct name_node *name_ref;
typedef struct pragma_node_seq *pragma_node_seq_ref;

struct token_node {
	pragma_type sym;
	long 	val;
};


struct name_node {
	char  *name;
	name_ref next;
};

struct needs_framepointer_node {
  	short type;
	name_ref nl;
};


union pragma_node {
	struct needs_framepointer_node needs_framepointer_node;
};



struct pragma_node_seq {
  	short type;
  	pragma_node_seq_ref prev, succ;
	pragma_node_ref pn_ref;
};


