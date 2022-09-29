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
#ident	"$Header: token.c,v 1.3.2.2 90/05/09 16:50:11 wje Exp $"

#include "lex.h"
#include "tokens.h"

#define MAXTOKENS 20
token_t token[MAXTOKENS] = {
	/* types */
	"int",		INT|TYPE,
	"short",	SHORT|TYPE,
	"char",		CHAR|TYPE,
	"byte",		BYTE|TYPE,
	"float",	FLOAT|TYPE,
	"double",	LFLOAT|TYPE,
	"pointer",	PTR|TYPE,
	"struct",	SU,
	"union",	SU,
	/* modifiers */
	"static",	STATIC|CLASS,
	"long",		LONG|CLASS,
	"unsigned",	CLASS|UNSIGNED,
	"register",	CLASS,
	"signed",	CLASS|SIGNED,
	/* end marker */
	"",0
	};



