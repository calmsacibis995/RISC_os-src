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
#ident	"$Header: tePutChar.c,v 1.1.1.2 90/05/10 05:35:05 wje Exp $"
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
