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
#ident	"$Header: tsh.c,v 1.3.2.4 90/05/07 18:18:22 wje Exp $"

#ifdef TSH
#include	"sh.h"
#include	"sh.local.h"
#include	"tsh.h"
#ifdef TERMIO
#ifdef RISCOS
#include <sysv/sys/termio.h>
#else RISCOS
#include <sysv/termio.h>
#endif RISCOS
#else TERMIO
#include	<sgtty.h>
#endif TERMIO



/*
 * tsh.c:
 * tsh routines dealing with interfacing between the tty driver, the rest
 * of the C-shell (csh), and the rest of tsh (the line editor).
 */

extern  int		tsh_in, tsh_out;	/* streams for I/O */

extern  int		tsh_num;		/* multiplier num */
extern  bool		tsh_num_reset;		/* reset multiplier num? */

extern  int		tsh_alt_num;		/* alt mode number */

extern  int		tsh_line_min;		/* min line length for ^N/^P */

extern  int		tsh_max_line;		/* max line length for redisp */

extern  char		tsh_buf[];		/* where current line is */
extern  char		*tsh_end;		/* last place in current line */

extern  bool		prev_del;		/* previous command was delete */
extern  bool		prev_del_reset;		/* reset prev delete flag? */

extern  int		(*tsh_map)();		/* pointer to character map () */
extern  bool		(*(*tsh_func))();	/* pointer to list of ()'s */

extern  short		*_cshctype_;		/* for character hacking */

#ifndef TERMIO
/*
 * line editor's version of special chcaracters and local special characters
 * leave both undefined
 */

static  struct  tchars		rtty_chars	= { NOCHAR, NOCHAR, NOCHAR,
							NOCHAR, NOCHAR, NOCHAR };
static  struct  ltchars		rtty_lchars	= { NOCHAR, NOCHAR, NOCHAR,
							NOCHAR, NOCHAR, NOCHAR };
#endif TERMIO

static  jmp_buf		jumpbuf;	/* for aborting */


/*
 * tsh_init:
 * Do some one-time initializations for tsh_read()
 * This must happen before .cshrc or .login is read.
 */

tsh_init()
{
	tsh_line_edit = FALSE;
}



/*
 * tsh_read:
 * Like a standard read call with the addition of the tsh_out stream indicator.
 * First, we reset the terminal.  The we read, calling a function on each char
 * read, until the function returns FALSE.  Then we clean up the terminal and
 * return.  There is also some stuff around the outside to insure that we don't
 * return more than the amount requested of characters.
 */

int  tsh_set_tty_state(tsh_mode)
	int	tsh_mode;
{
	static	int	in_tsh_mode = 0;
#ifdef TERMIO
	static struct	termio		tty_state;
	struct termio	rtty_state;
	int	cci;
#else TERMIO
	static	struct  sgttyb		tty_state;
	struct	sgttyb	rtty_state;
	static	struct  tchars		tty_chars;
	static	struct  ltchars		tty_lchars;
	int ldisc;	/* Current line discipline */
#endif TERMIO;

	if (tsh_mode) {
		if (in_tsh_mode)
			return;
#ifdef TERMIO
		ioctl( tsh_in, TCGETA, &tty_state );
#else TERMIO
		ioctl( tsh_in, TIOCGETP, &tty_state );
		ioctl( tsh_in, TIOCGETC, &tty_chars );
		ioctl( tsh_in, TIOCGLTC, &tty_lchars );
#endif TERMIO
		in_tsh_mode = 1;

	/*
	 * By setting the line discipline to the current line discipline,
	 * we avoid character dropping due to changing to CBREAK mode. This
	 * is because all output from the running process will have to be
	 * printed before the ioctl returns.
	 */

		rtty_state = tty_state;
#ifdef TERMIO
		rtty_state.c_iflag &= ~BRKINT;
		rtty_state.c_lflag &= ~(ICANON | ECHO | ISIG);
		rtty_state.c_oflag |= OPOST;
		for (cci = 0; cci < NCC; cci++)
			rtty_state.c_cc[cci] = CDEL;
					/* disable all special characters */
		rtty_state.c_cc[VMIN] = 1;
		rtty_state.c_cc[VTIME] = 1;
		(void) ioctl(tsh_in, TCSETAW, &rtty_state);
#else TERMIO
		if (ioctl(tsh_in, TIOCGETD, &ldisc) >= 0) {
			ioctl(tsh_in, TIOCSETD, &ldisc);
		} /* if it fails, we don't care */

		rtty_state.sg_flags |= CBREAK;
		rtty_state.sg_flags &= ~( ECHO | RAW );
		ioctl( tsh_in, TIOCSETN, &rtty_state );
		ioctl( tsh_in, TIOCSETC, &rtty_chars );
		ioctl( tsh_in, TIOCSLTC, &rtty_lchars );
#endif TERMIO
	} else {
		if (! in_tsh_mode) 
			return;
#ifdef TERMIO
		(void) ioctl( tsh_in, TCSETAW, &tty_state);
#else TERMIO
		ioctl( tsh_in, TIOCSETN, &tty_state );
		ioctl( tsh_in, TIOCSETC, &tty_chars );
		ioctl( tsh_in, TIOCSLTC, &tty_lchars );
#endif TERMIO
		in_tsh_mode = 0;
	}
}


int  tsh_read( in, out, buf, size )

    int			in, out;
    char		*buf;
    register  int	size;
{
	register  int		c			= 0;
	register  char		*bend, *p;
	static  char		*tot			= NOSTR;
	static  char		*pos			= NOSTR;

	if(  pos  <  tot  )  goto  just_out;
	tsh_in = in;
	tsh_out = out;

	tsh_set_tty_state(1); /* enter tsh mode */

	tsh_line_init();
    /*
     * Note that in this loop, (*tsh_map)() may change tsh_func!  Given the
     * current order of evaluation, this works properly.
     */
	if(  setjmp( jumpbuf )  )  tsh_reset( ABORT_CHAR );
	do  {
	    c = tsh_next_char();
	    if(  tsh_alt_num >= 0  )  {
		while(  isdigit( c )  )  {
		    tsh_alt_num = 10*tsh_alt_num + ( c - '0' );
		    c = tsh_next_char();
		}
		tsh_num_reset = FALSE;
	    }
	    if(  tsh_num_reset  )  {
		tsh_num = 1;
	    }
	    else  {
		if(  tsh_alt_num  >=  0  )  {
		    tsh_num = tsh_alt_num*tsh_num;
		    tsh_alt_num = -1;
		}
		tsh_num_reset = TRUE;
	    }
	    if(  prev_del_reset  )  prev_del = FALSE;
	    else  prev_del_reset = TRUE;
	}  while(  ( *tsh_func[ (*tsh_map)(c) ] )( c )  );
	pos = tsh_buf;
	tot = tsh_end;

	tsh_set_tty_state(0); /* leave tsh mode */

 just_out:
	bend = &buf[size];
	for( p = buf; p < bend  &&  pos < tot; *p++ = *pos++ );
	return( p - buf );
}



/*
 * tsh_next_char:
 * Return the next character.  If there are none left in our internal buffer,
 * see how many are out there and either get more or invoke redisplay and
 * wait for more.
 * Return EOF (-1) when read returns 0.
 */

int  tsh_next_char()
{
	register  int		i;
	static  char		buf[BUFSIZ];
	static  char		*buf_pos		= buf;
	static  char		*buf_end		= buf;
	int			count;

	if(  buf_pos  >=  buf_end  )  {
	    ioctl( tsh_in, FIONREAD, &count );
	    if(  count  ==  0  )  {
		tsh_redisplay( FALSE, FALSE );
	    }
	    if(  ( buf_end = &buf[ read(tsh_in, buf, BUFSIZ) ] )  <=  buf  )  return( -1 );
	    buf_pos = buf;
	}
	if(  ( i = *buf_pos++ )  ==  ABORT_CHAR  )  longjmp( jumpbuf, 1 );
	else  return( i );
}



/*
 * tsh_set_min:
 * Set the minimum line size for the history mechanism to consider.
 */

tsh_set_min( min )

    register  int		min;
{
	tsh_line_min = ( min >= 0 ? min : 0 );
}
#endif
