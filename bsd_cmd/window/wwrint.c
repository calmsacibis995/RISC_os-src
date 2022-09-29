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
#ident	"$Header: wwrint.c,v 1.1.2.2 90/05/07 20:03:10 wje Exp $"

#include "ww.h"
#include <fcntl.h>
#include <sys/signal.h>

/*
 * Tty input interrupt handler.
 * (1) Read input into buffer (wwib*).
 * (2) Set the interrupt flag if anything is read.
 * Currently, the last is used to get out of the blocking
 * select() in wwiomux().
 * To avoid race conditions, we only modify wwibq in here, except
 * when the buffer is empty; and everywhere else, we only change wwibp.
 * It should be completely safe.
 */
wwrint()
{
	register n;

	if (wwibp == wwibq)
		wwibp = wwibq = wwib;
	wwnread++;
	(void) fcntl(0, F_SETFL, FNDELAY|wwnewtty.ww_fflags);
	n = read(0, wwibq, wwibe - wwibq);
	(void) fcntl(0, F_SETFL, wwnewtty.ww_fflags);
	if (n > 0) {
		wwibq += n;
		wwnreadc += n;
		wwsetintr();
	} else if (n == 0)
		wwnreadz++;
	else
		wwnreade++;
}
