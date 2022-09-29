#ident "$Header: tePutChar.c,v 1.1 90/03/22 19:10:52 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ANSI Terminal Emulator - backward compatibility functions.
 *   External (OS) access to console terminal emulator.
 */

#include "sys/types.h"

/* LINTLIBRARY */

#define	OK	0
#define	FALSE	0

extern	void	teReset(), tePutChar();

/*
 * Initialize the terminal emulator.
 */
int
g_pinit(clr)
	int	clr;
{
	teReset(clr);
	return OK;
}

/*
 * Initialize the color map.
 */
int
initcmap()
{
	teReset(0);
	return OK;
}

/*
 * Interpret and possibly display a character.
 */
int
g_putchar(c)
	int	c;
{
	tePutChar((u_char)c);
	return OK;
}
