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
/* $Header: tokens.h,v 1.3.2.2 90/05/09 16:50:17 wje Exp $ */


typedef struct {
		char *tokenstr;
		int  tokenid; } token_t;

/* name is the default token for unrecognized strings of characters */
#define YYLVAL(t) (t & ~0xff)
#define PARSEVAL(t) (t & 0xff)

/* classes - type modifiers */
/* any type modifier that doesnt matter (short, unsigned) is just CLASS */
#define STATIC  0x800
#define LONG	0x400	 /* this is assigned to yylval */
#define SIGNED  0x200
#define UNSIGNED 0x100
#define SIGN_SPECIFIED(c) ( (c) & (SIGNED|UNSIGNED))


/* types */
#define INT	 0
#define CHAR	0x1000
#define SHORT	0x2000
#define FLOAT	0x4000
#define BYTE	0x8000
#define LFLOAT  (FLOAT|LONG)
/* arbitrary pointer - only restriction is NOT to CHAR */
#define PTR	0x10000
#define ARRAY	0x20000
#define PTRPARM (PTR|ARRAY)

#define STRING	(PTR|CHAR)
#define CHARARRAY (ARRAY|CHAR)

#ifndef NAME
#define INDIRECT  2
#define LP	 3
#define RP	4
#define LBRACKET	5
#define RBRACKET	6
#define LBRACE	7
#define RBRACE	(8)
#define CM (0xa)
#define SM (0xb)
#define NUMBER (0xc)
#define NAME 0x10
#define CLASS (NAME | 8)
#define TYPE (NAME | 4)
#define SU (0xd)
#define ERROR (0xe)
#endif
