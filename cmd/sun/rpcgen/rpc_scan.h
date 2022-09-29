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
/* $Header: rpc_scan.h,v 1.1.1.2 90/05/09 19:25:45 wje Exp $ */

/*	@(#)rpc_scan.h	1.2 88/05/08 4.0NFSSRC SMI	*/

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.4
 */

/*
 * rpc_scan.h, Definitions for the RPCL scanner 
 */

/*
 * kinds of tokens 
 */
enum tok_kind {
	TOK_IDENT,
	TOK_STRCONST,
	TOK_LPAREN,
	TOK_RPAREN,
	TOK_LBRACE,
	TOK_RBRACE,
	TOK_LBRACKET,
	TOK_RBRACKET,
	TOK_LANGLE,
	TOK_RANGLE,
	TOK_STAR,
	TOK_COMMA,
	TOK_EQUAL,
	TOK_COLON,
	TOK_SEMICOLON,
	TOK_CONST,
	TOK_STRUCT,
	TOK_UNION,
	TOK_SWITCH,
	TOK_CASE,
	TOK_DEFAULT,
	TOK_ENUM,
	TOK_TYPEDEF,
	TOK_INT,
	TOK_SHORT,
	TOK_LONG,
	TOK_UNSIGNED,
	TOK_FLOAT,
	TOK_DOUBLE,
	TOK_OPAQUE,
	TOK_CHAR,
	TOK_STRING,
	TOK_BOOL,
	TOK_VOID,
	TOK_PROGRAM,
	TOK_VERSION,
	TOK_EOF
};
typedef enum tok_kind tok_kind;

/*
 * a token 
 */
struct token {
	tok_kind kind;
	char *str;
};
typedef struct token token;


/*
 * routine interface 
 */
void scanprint();
void scan();
void scan2();
void scan3();
void scan_num();
void peek();
int peekscan();
void get_token();
