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
#ident	"$Header: screen.c,v 1.4.1.2 90/05/10 03:41:45 wje Exp $"

/*
 *  Top - a top users display for Berkeley Unix
 *
 *  This file contains the routines that interface to termcap and stty/gtty.
 *
 *  Paul Vixie, February 1987: converted to use ioctl() instead of stty/gtty.
 */

#include <stdio.h>
#ifdef RISCOS
#include <sys/types.h>
#include <sys/termio.h>
#else RISCOS
#include <sgtty.h>
#endif RISCOS
#include "screen.h"
#include "boolean.h"

extern char *myname;

int putstdout();

int  scrolls;
int  hardcopy;
int  screen_length;
int  screen_width;
char ch_erase;
char ch_kill;
char smart_terminal;
char PC;
char *tgetstr();
char *tgoto();
char termcap_buf[1024];
char init_buf[1024];
char string_buffer[1024];
char home[15];
char lower_left[15];
char *clear_line;
char *clear_screen;
char *cursor_motion;
char *start_standout;
char *end_standout;
char *terminal_init;
char *terminal_end;
short ospeed;

#ifdef RISCOS
static struct termio old_settings;
static struct termio new_settings;
#else RISCOS
static struct sgttyb old_settings;
static struct sgttyb new_settings;
#endif RISCOS
static char is_a_terminal = No;

#define	STDIN	0
#define	STDOUT	1
#define	STDERR	2

init_termcap()

{
    char *bufptr;
    char *PCptr;
    char *term_name;
    char *temp_ptr;
    char *getenv();
    int status;

    /* assume we have a smart terminal until proven otherwise */
    smart_terminal = Yes;

    /* now get terminal name and termcap entry */
    term_name = getenv("TERM");
    if ((status = tgetent(termcap_buf, term_name)) != 1)
    {
	if (status == -1)
	{
	    fprintf(stderr, "%s: can't open termcap file\n", myname);
	}
	else
	{
	    fprintf(stderr, "%s: no termcap entry for a `%s' terminal\n",
		    myname, getenv("TERM"));
	}

	/* pretend it's dumb and proceed */
	smart_terminal = No;
	return;
    }

    /* these immediately indicate a very stupid terminal */
    if (tgetflag("hc") || tgetflag("os"))
    {
	smart_terminal = No;
	return;
    }

    /* set up common terminal capabilities */
    if ((screen_length = tgetnum("li")) <= 0)
    {
	screen_length = smart_terminal = 0;
	return;
    }

    /* screen_width is a little different */
    if ((screen_width = tgetnum("co")) == -1)
    {
	screen_width = 79;
    }
    else
    {
	screen_width -= 1;
    }

    /* initialize the pointer into the termcap string buffer */
    bufptr = string_buffer;

    /* get necessary capabilities */
    if ((clear_line    = tgetstr("ce", &bufptr)) == NULL ||
	(clear_screen  = tgetstr("cl", &bufptr)) == NULL ||
	(cursor_motion = tgetstr("cm", &bufptr)) == NULL)
    {
	smart_terminal = No;
	return;
    }

    /* get some more sophisticated stuff -- these are optional */
    terminal_init  = tgetstr("ti", &bufptr);
    terminal_end   = tgetstr("te", &bufptr);
    start_standout = tgetstr("so", &bufptr);
    end_standout   = tgetstr("se", &bufptr);

    /* pad character */
    PC = (PCptr = tgetstr("pc", &bufptr)) ? *PCptr : 0;

    /* set convenience strings */
    strcpy(home, tgoto(cursor_motion, 0, 0));
    strcpy(lower_left, tgoto(cursor_motion, 0, screen_length - 1));

    /* if stdout is not a terminal, pretend we are a dumb terminal */
#ifdef RISCOS
    if (-1 == ioctl(STDOUT, TCGETA, &old_settings))
#else RISCOS
    if (-1 == ioctl(STDOUT, TIOCGETP, &old_settings))
#endif RISCOS
    {
	smart_terminal = No;
    }
}

init_screen()

{
    /* get the old settings for safe keeping */
#ifdef RISCOS
    if (0 == ioctl(STDOUT, TCGETA, &old_settings))
#else RISCOS
    if (0 == ioctl(STDOUT, TIOCGETP, &old_settings))
#endif RISCOS
    {
	/* copy the settings so we can modify them */
	new_settings = old_settings;

#ifdef RISCOS
	/* turn on CBREAK and turn off character echo and tab expansion */
	new_settings.c_lflag |= ISIG;
	new_settings.c_lflag &= ~(ICANON | ECHO);
	new_settings.c_cc[VMIN] = 1;
	new_settings.c_cc[VTIME] = 1;
	new_settings.c_lflag &= ~TABDLY;
	ioctl(STDOUT, TCSETAW, &new_settings);

	/* remember the erase and kill characters */
	ch_erase = old_settings.c_cc[VERASE];
	ch_kill  = old_settings.c_cc[VKILL];
#else RISCOS
	/* turn on CBREAK and turn off character echo and tab expansion */
	new_settings.sg_flags |= CBREAK;
	new_settings.sg_flags &= ~(ECHO|XTABS);
	ioctl(STDOUT, TIOCSETP, &new_settings);

	/* remember the erase and kill characters */
	ch_erase = old_settings.sg_erase;
	ch_kill  = old_settings.sg_kill;
#endif RISCOS

	/* remember that it really is a terminal */
	is_a_terminal = Yes;

	/* send the termcap initialization string */
	putcap(terminal_init);
    }
    else
    {
	/* not a terminal at all---consider it dumb */
	smart_terminal = No;
    }
}

end_screen()

{
    /* move to the lower left, clear the line and send "te" */
    if (smart_terminal)
    {
	putcap(lower_left);
	putcap(clear_line);
	putcap(terminal_end);
    }

    /* if we have settings to reset, then do so */
    if (is_a_terminal)
    {
#ifdef RISCOS
	ioctl(STDOUT, TCSETAW, &old_settings);
#else RISCOS
	ioctl(STDOUT, TIOCSETP, &old_settings);
#endif RISCOS
    }
}

reinit_screen()

{
    /* install our settings if it is a terminal */
    if (is_a_terminal)
    {
#ifdef RISCOS
	ioctl(STDOUT, TCSETAW, &new_settings);
#else RISCOS
	ioctl(STDOUT, TIOCSETP, &new_settings);
#endif RISCOS
    }

    /* send init string */
    if (smart_terminal)
    {
	putcap(terminal_init);
    }
}

standout(fmt, a1, a2, a3)

char *fmt;
int a1, a2, a3;

{
    if (smart_terminal)
    {
	putcap(start_standout);
	printf(fmt, a1, a2, a3);
	putcap(end_standout);
    }
    else
    {
	printf(fmt, a1, a2, a3);
    }
}

clear()

{
    if (smart_terminal)
    {
	putcap(clear_screen);
    }
}

/* This has to be defined as a subroutine for tputs (instead of a macro) */

putstdout(ch)

char ch;

{
    putchar(ch);
}

