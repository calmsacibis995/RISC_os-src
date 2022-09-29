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
/* $Header: context.h,v 1.1.2.2 90/05/07 19:52:56 wje Exp $ */

#include <stdio.h>

struct context {
	struct context *x_link;		/* nested contexts */
	char x_type;			/* tag for union */
	union {
		struct {	/* input is a file */
			char *X_filename;	/* input file name */
			FILE *X_fp;		/* input stream */
			short X_lineno;		/* current line number */
			char X_bol;		/* at beginning of line */
			char X_noerr;		/* don't report errors */
			struct ww *X_errwin;	/* error window */
		} x_f;
		struct {	/* input is a buffer */
			char *X_buf;		/* input buffer */
			char *X_bufp;		/* current position in buf */
			struct value *X_arg;	/* argument for alias */
			int X_narg;		/* number of arguments */
		} x_b;
	} x_un;
		/* holding place for current token */
	int x_token;			/* the token */
	struct value x_val;		/* values associated with token */
		/* parser error flags */
	unsigned x_erred :1;		/* had an error */
	unsigned x_synerred :1;		/* had syntax error */
	unsigned x_abort :1;		/* fatal error */
};
#define x_buf		x_un.x_b.X_buf
#define x_bufp		x_un.x_b.X_bufp
#define x_arg		x_un.x_b.X_arg
#define x_narg		x_un.x_b.X_narg
#define x_filename	x_un.x_f.X_filename
#define x_fp		x_un.x_f.X_fp
#define x_lineno	x_un.x_f.X_lineno
#define x_bol		x_un.x_f.X_bol
#define x_errwin	x_un.x_f.X_errwin
#define x_noerr		x_un.x_f.X_noerr

	/* x_type values, 0 is reserved */
#define X_FILE		1		/* input is a file */
#define X_BUF		2		/* input is a buffer */

struct context cx;			/* the current context */
