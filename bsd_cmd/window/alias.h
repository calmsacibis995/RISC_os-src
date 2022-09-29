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
/* $Header: alias.h,v 1.1.2.2 90/05/07 19:51:23 wje Exp $ */

#define alias var
#define a_name r_name
#define a_buf r_val.v_str
#define a_flags r_val.v_type

	/* a_flags bits, must not interfere with v_type values */
#define A_INUSE		0x010	/* already inuse */

#define alias_set(n, s)		var_setstr1(&alias_head, n, s)
#define alias_walk(f, a)	var_walk1(alias_head, f, a)
#define alias_unset(n)		var_unset1(&alias_head, n)
#define alias_lookup(n)		(*var_lookup1(&alias_head, n))

struct var *alias_head;
