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
#ident	"$Header: signal.c,v 1.5.2.2 90/05/09 17:58:46 wje Exp $"

/*
 * Routines dealing with signals.
 *
 * A signal usually merely causes a bit to be set in the "signals" word.
 * At some convenient time, the mainline code checks to see if any
 * signals need processing by calling psignal().
 * An exception is made if we are reading from the keyboard when the
 * signal is received.  Some operating systems will simply call the
 * signal handler and NOT return from the read (with EINTR).
 * To handle this case, we service the interrupt directly from
 * the handler if we are reading from the keyboard.
 */

#include "less.h"
#include <signal.h>
#include <setjmp.h>

/*
 * The type of signal handler functions.
 * Usually int, although it should be void.
 */
typedef	int		HANDLER;

/*
 * "sigs" contains bits indicating signals which need to be processed.
 */
public int sigs;
#define	S_INTERRUPT	01
#ifdef SIGTSTP
#define	S_STOP		02
#endif
#if defined(SIGWINCH) || defined(SIGWIND)
#define S_WINCH		04
#endif

extern int reading;
extern int sc_width, sc_height;
extern char *first_cmd;
extern jmp_buf main_loop;

/*
 * Interrupt signal handler.
 */
	static HANDLER
interrupt()
{
	SIGNAL(SIGINT, interrupt);
	sigs |= S_INTERRUPT;
	if (reading)
		psignals();
}

#ifdef SIGTSTP
/*
 * "Stop" (^Z) signal handler.
 */
	static HANDLER
stop()
{
	SIGNAL(SIGTSTP, stop);
	sigs |= S_STOP;
	if (reading)
		psignals();
}
#endif

#ifdef SIGWINCH
/*
 * "Window" change handler
 */
winch()
{
	SIGNAL(SIGWINCH, winch);
	sigs |= S_WINCH;
	if (reading)
		psignals();
}
#else
#ifdef SIGWIND
/*
 * "Window" change handler
 */
winch()
{
	SIGNAL(SIGWIND, winch);
	sigs |= S_WINCH;
	if (reading)
		psignals();
}
#endif
#endif

/*
 * Set up the signal handlers.
 */
	public void
init_signals()
{
	(void) SIGNAL(SIGINT, interrupt);
#ifdef SIGTSTP
	(void) SIGNAL(SIGTSTP, stop);
#endif
#ifdef SIGWINCH
	(void) SIGNAL(SIGWINCH, winch);
#else
#ifdef SIGWIND
	(void) SIGNAL(SIGWIND, winch);
#endif
#endif
}

/*
 * Process any signals we have recieved.
 * A received signal cause a bit to be set in "sigs".
 */
	public void 
psignals()
{
	register int tsignals;

	tsignals = sigs;
	sigs = 0;
	if (tsignals == 0)
		return;

	dropout();		/* Discard any buffered output */

#ifdef S_WINCH
	if (tsignals & S_WINCH)
	{
		int old_width, old_height;
		/*
		 * Re-execute get_term() to read the new window size.
		 */
		old_width = sc_width;
		old_height = sc_height;
		get_term();
		if (sc_width != old_width || sc_height != old_height)
			first_cmd = "r";
		longjmp(main_loop, 1);
	}
#endif
#ifdef SIGTSTP
	if (tsignals & S_STOP)
	{
		/*
		 * Clean up the terminal.
		 */
#ifdef SIGTTOU
		SIGNAL(SIGTTOU, SIG_IGN);
#endif
		lower_left();
		clear_eol();
		flush();
		raw_mode(0);
#ifdef SIGTTOU
		SIGNAL(SIGTTOU, SIG_DFL);
#endif
		SIGNAL(SIGTSTP, SIG_DFL);
#if BSD4_3
		/*
		 * This system will not allow us to send a 
		 * stop signal (SIGTSTP) to ourself
		 * while we are in the signal handler, like maybe now.
		 * (This can be the case if we are reading; see comment above.)
		 * So we ask the silly system for permission to do so.
		 */
		sigsetmask(0);
#endif
		kill(getpid(), SIGTSTP);
		/*
		 * ... Bye bye. ...
		 * Hopefully we'll be back later and resume here...
		 * Reset the terminal and arrange to repaint the
		 * screen when we get back to the main command loop.
		 */
		SIGNAL(SIGTSTP, stop);
		raw_mode(1);
		first_cmd = "r";
		longjmp(main_loop, 1);
	}
#endif
	if (tsignals & S_INTERRUPT)
	{
		bell();
		/*
		 * {{ You may wish to replace the bell() with 
		 *    error("Interrupt"); }}
		 */
	}

	longjmp(main_loop, 1);
}
