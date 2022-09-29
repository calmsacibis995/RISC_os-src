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
#ident	"$Header: ctype_funcs.c,v 1.1.1.2 90/05/10 04:12:52 wje Exp $"

#include <ctype.h>

int
isalnum(c)
{
	return((_ctype + 1)[c] & (_U | _L | _N));
}

int
isalpha(c)
{
	return((_ctype + 1)[c] & (_U | _L));
}

int
iscntrl(c)
{
	return((_ctype + 1)[c] & _C);
}

int
isdigit(c)
{
	return((_ctype + 1)[c] & _N);
}

int
isgraph(c)
{
	return((_ctype + 1)[c] & (_P | _U | _L | _N));
}

int
islower(c)
{
	return((_ctype + 1)[c] & _L);
}

int
isprint(c)
{
	return((_ctype + 1)[c] & (_P | _U | _L | _N | _B));
}

int
ispunct(c)
{
	return((_ctype + 1)[c] & _P);
}

int
isspace(c)
{
	return((_ctype + 1)[c] & _S);
}

int
isupper(c)
{
	return((_ctype + 1)[c] & _U);
}

int
isxdigit(c)
{
	return((_ctype + 1)[c] & _X);
}

