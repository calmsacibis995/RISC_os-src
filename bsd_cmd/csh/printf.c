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
#ident	"$Header: printf.c,v 1.2.1.2 90/05/07 18:13:25 wje Exp $"

#include <varargs.h>

/*
 * Hacked "printf" which prints through cshputchar.
 * DONT USE WITH STDIO!
 */
printf(fmt, va_alist)
char *fmt;
va_dcl
{
	va_list ap;

	va_start(ap);
	_doprnt(fmt, ap);
	va_end(ap);
}

_strout(count, string, adjust, foo, fillch)
register char *string;
register int count;
int adjust;
register struct { int a[6]; } *foo;
{

	if (foo != 0)
		abort();
	while (adjust < 0) {
		if (*string=='-' && fillch=='0') {
			cshputchar(*string++);
			count--;
		}
		cshputchar(fillch);
		adjust++;
	}
	while (--count>=0)
		cshputchar(*string++);
	while (adjust) {
		cshputchar(fillch);
		adjust--;
	}
}
