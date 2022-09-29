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
/* $Header: var.h,v 1.1.2.2 90/05/07 19:57:56 wje Exp $ */

struct var {
	struct var *r_left;
	struct var *r_right;
	char *r_name;
	struct value r_val;
};

struct var *var_set1();
struct var *var_setstr1();
struct var *var_setnum1();
struct var **var_lookup1();

#define var_set(n, v)		var_set1(&var_head, n, v)
#define var_setstr(n, s)	var_setstr1(&var_head, n, s)
#define var_setnum(n, i)	var_setnum1(&var_head, n, i)
#define var_unset(n)		var_unset1(&var_head, n)
#define var_lookup(n)		(*var_lookup1(&var_head, n))
#define var_walk(f, a)		var_walk1(var_head, f, a)

struct var *var_head;		/* secret, shhh */
