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
#ident	"$Header: ttyin.c,v 1.4.2.2 90/05/09 17:58:52 wje Exp $"

/*
 * Routines dealing with getting input from the keyboard (i.e. from the user).
 */

#include "less.h"

/*
 * The boolean "reading" is set true or false according to whether
 * we are currently reading from the keyboard.
 * This information is used by the signal handling stuff in signal.c.
 * {{ There are probably some race conditions here
 *    involving the variable "reading". }}
 */
public int reading;

static int tty;

/*
 * Open keyboard for input.
 * (Just use file descriptor 2.)
 */
	public void
open_getchr()
{
	tty = 2;
}

/*
 * Get a character from the keyboard.
 */
	public int
getchr()
{
	char c;
	int result;

	reading = 1;
	do
	{
		flush();
		result = read(tty, &c, 1);
	} while (result != 1);
	reading = 0;
	return (c & 0177);
}
