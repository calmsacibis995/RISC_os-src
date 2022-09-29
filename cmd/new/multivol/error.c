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
#ident	"$Header: error.c,v 1.2.2.2 90/05/09 18:07:45 wje Exp $"
#include <stdio.h>
	extern	int	errno;
	extern	char	*O_name;
/*VARARGS1*/
fatal(fmt, arg1, arg2, arg3)
	char	*fmt;
	int	arg1, arg2, arg3;
{
	warning(fmt, arg1, arg2, arg3);
#ifdef	DEBUG
	abort();
#else
	exit(1);
#endif
}

sfatal(str)
	char	*str;
{
	swarning(str);
#ifdef	DEBUG
	abort();
#else
	exit(1);
#endif
}

/*VARARGS1*/
warning(fmt, arg1, arg2, arg3)
	char	*fmt;
	int	arg1, arg2, arg3;
{
	fprintf(stderr, "%s: ", O_name);
	fprintf(stderr, fmt, arg1, arg2, arg3);
	fprintf(stderr, "\n");
}

swarning(str)
	char	*str;
{
#ifdef	DEBUG
	fprintf(stderr, "(%d)  ", errno);
#endif
	fprintf(stderr, "%s: ", O_name);
	perror(str);
}
