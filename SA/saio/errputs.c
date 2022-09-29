#ident "$Header: errputs.c,v 1.2 90/05/23 15:02:26 huang Exp $"
/* $Copyright$ */

/*
 * errputs.c -- error output routines in case of failures
 */

/*
 * Primitive output routines in case all else fails
 */

#include "machine/cpu_board.h"

_errputs(cp)
register char *cp;
{
	register c;

	while (c = *cp++) {
		if (c == '\n')
			_errputc('\r');
		_errputc(c);
	}
}

_errputc(c)
char c;
{
#ifdef MIPS
#ifdef PROM
#if	(R3030 || RB3125)
	_scc_errputc(c);
#else
	_s2681_errputc(c);
#endif (R3030 || RB3125)
#else
	if (IS_R3030 || IS_RB3125)
	    _scc_errputc(c);
	else
	    _s2681_errputc(c);
#endif PROM
#endif MIPS

#ifdef SABLE
	_sable_errputc(c);
#endif SABLE
}
