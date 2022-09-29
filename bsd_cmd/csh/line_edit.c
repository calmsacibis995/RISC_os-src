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
#ident	"$Header: line_edit.c,v 1.2.1.2 90/05/07 18:13:07 wje Exp $"

#ifdef TSH
#include	"sh.h"
#include	"sh.local.h"
#include	"tsh.h"



/*
 * line_edit.c:
 * routines for dealing with the line editor
 *
 * modified 18 sep 84 by bruce to add completion on ^Z
 * modified 2 oct 84 by bruce to fix ^K and word ops, also complete on esc-esc
 */


/* in-line vesion of ctype.h modified so that '_' is part of isalnum */
/* also modified isaspace() to be true only for space and tab! */

short	_cshctype[129] = { 0000,
    0040,0040,0040,0040,0040,0040,0040,0040,0040,0010,0000,0000,0000,0000,0040,0040,
    0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,0040,
    0030,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,0020,
    0004,0004,0004,0004,0004,0004,0004,0004,0004,0004,0020,0020,0020,0020,0020,0020,
    0020,0101,0101,0101,0101,0101,0101,0001,0001,0001,0001,0001,0001,0001,0001,0001,
    0001,0001,0001,0001,0001,0001,0001,0001,0001,0001,0001,0020,0020,0020,0020,0220,
    0020,0102,0102,0102,0102,0102,0102,0002,0002,0002,0002,0002,0002,0002,0002,0002,
    0002,0002,0002,0002,0002,0002,0002,0002,0002,0002,0002,0020,0020,0020,0020,0040};

short	*_cshctype_		= &(_cshctype[1]);

	
/*
 * stuff related to character buffers and redisplay
 * Cursors act like a real cursor and point to character after one just typed;
 * thus a character input at cursor gets inserted before the cursor.
 */

int		tsh_in, tsh_out;		/* I/O descriptors */

int		tsh_num			= 1;	/* multiplier num */
bool		tsh_num_reset		= TRUE;	/* reset multiplier num? */

int		tsh_alt_num		= -1;	/* alt mode number */

int		tsh_line_min		= 1;	/* min line length for ^N/^P */

int		tsh_max_line	= MAX_LINE_LEN;	/* max line length for redisp */

static  int	last_search		= 0;	/* last char searched for */

char		tsh_buf[NCARGS];		/* buffer for current line */
char		*tsh_sbuf		= 0;	/* character save for undo */
char		*tsh_ybuf		= 0;	/* character save for yank */

char		*tsh_end;			/* one after last char */
char		*tsh_cur;			/* virtual cursor on line */
char		*old_tsh_cur;			/* prev. virtual cursor on line */
int		tsh_cursor;			/* real cursor as position */
char		*tsh_left;			/* leftmost change on line */
int		tsh_right;			/* rightmost change as position */

bool		prev_del;			/* previous command was delete */
bool		prev_del_reset;			/* reset prev delete flag? */

static  struct  Hist	*hp;			/* history pointer */
static  struct  Hist	*mark_hp;		/* marked history pointer */



/*
 * stuff related to tsh_func tables
 * Note that tsh_set_edit_chars only gets to change things up to DELETE_FUNC
 */

#define		NFUNCS		0x25		/* enough for control + a few */

#define		DELETE_FUNC	0x20		/* for ^? */
#define		EOF_FUNC	0x21		/* for EOF */
#define		SELF_INSERT	0x22		/* self-inserting chars func # */
#define		SPACE_FUNC	0x23		/* for ^feature^ */
#define		BAD_FUNC	0x24		/* bad chars func # */


/*
 * definitions of functions correspoding to single chars typed
 * default_func is a copy of norm_func (or vice versa) for use with
 * set_edit_chars
 */

static  bool	mark(), bol(), back(), intr(), dela(), eol(), frwd();
static  bool	delb(), exc(), dsrch(), rdsp(), exc(), next(), prev();
static  bool	quote(), rsrch(), srch(), twdl(), numb(), kill(), yank();
static  bool	prfx(), insrt(), space(), eof(), bad();
static	bool	comp(), eeol();

bool		tsh_reset();

bool		(*norm_func[NFUNCS])()	= {
    			mark, bol, back, intr, dela, eol, frwd, tsh_reset,
			delb, space, exc, dsrch, rdsp, exc, next, bad,
			prev, quote, rsrch, srch, twdl, numb, bad, kill,
			bad, yank, comp, prfx, bad, bad, bad, bad,
			delb, eof, insrt, space, bad };

bool		(*default_func[NFUNCS])()	= {
    			mark, bol, back, intr, dela, eol, frwd, tsh_reset,
			delb, space, exc, dsrch, rdsp, exc, next, bad,
			prev, quote, rsrch, srch, twdl, numb, bad, kill,
			bad, yank, comp, prfx, bad, bad, bad, bad,
			delb, eof, insrt, space, bad };



/*
 * definitions of functions correspoding to single chars typed after prefix char.
 * Must include EOF_FUNC list!
 */

#define		PFUNCS		0x22

static  bool	boh(), eoh(), wdback(), wdfrwd(), wddelp(), wddeln();
static  bool	undo(), hynk(), nop();

bool		(*prfx_func[PFUNCS])()	= {
    			nop, boh, wdback, bad, wddeln, eoh, wdfrwd, bad,
			wddelp, bad, bad, bad, bad, bad, bad, bad,
			bad, bad, bad, bad, bad, undo, bad, bad,
			bad, hynk, bad, comp, bad, bad, bad, wddelp,
			bad, eof };



int		norm_map();

int		(*tsh_map)()		= norm_map;	/* ptr to char map () */

bool		(*(*tsh_func))()	= norm_func;	/* ptr to list of ()'s */
static  bool	(*(*ntsh_func))()	= norm_func;	/* next version of " */



/*
 * norm_map:
 * Map the normal characters into the proper classes for the desired
 * actions.
 */

int  norm_map( c )

    register  int		c;
{
	tsh_func = ntsh_func;
	if(  c  >=  0x7f  )  {
	    if(  c  ==  0x7f  )  c = DELETE_FUNC;
	    else  c = BAD_FUNC;
	}
	else  {
		  if(  c  >  ' '  )  c = SELF_INSERT;
	    else  if(  isaspace( c )  )  c = SPACE_FUNC;
	    else  if(  c < 0x00  ||  ( c == EOF_CHAR && tsh_end == tsh_buf )  )  c = EOF_FUNC;
	}
	return( c );
}



/*
 
 * prfx_map:
 * Map the characters into a sbuset for action after the prefix character
 * is typed.
 */

int  prfx_map( c )

    register  int		c;
{
	tsh_func = ntsh_func;
	if(  c  <  0x00  )  {
	    c = EOF_FUNC;
	}
	else  {
	    if(  isdigit( c )  )  {
		tsh_alt_num = c - '0';
		tsh_num_reset = FALSE;
		c = 0;		/* force alt-mode NOP function to ignore number */
	    }
	    else  {
		c = CNTL( c );
	    }
	}
	tsh_map = norm_map;
	ntsh_func = norm_func;
	return( c );
}



/*
 * tsh_line_init:
 * Called to clear lines initially and do various setup.
 */

tsh_line_init()
{
	char			*rindex();
	register  char		*p, *q;

	old_tsh_cur = tsh_cur = tsh_end = tsh_left = tsh_buf;
	tsh_cursor = tsh_right = 0;
	if(  tsh_sbuf  )  free( tsh_sbuf );
	tsh_sbuf = 0;
	hp = &Histlist;			/* initial history ptr for next/prev */
	tsh_num_reset = TRUE;
	tsh_alt_num = -1;
	prev_del_reset = TRUE;
	p = value( "prompt" );
	q = rindex( p, '\n' );		/* look for newlines */
	tsh_max_line = MAX_LINE_LEN - strlen( q != (char *)0 ? q : p );
}



/*
 * tsh_reset:
 * Reset things to their normal state after an abort.
 */

bool  tsh_reset( c )

    register  int		c;
{
	if(  c  ==  ABORT_CHAR  )  {
	    BELL();
	}
	tsh_num_reset = TRUE;
	tsh_alt_num = -1;
	prev_del_reset = TRUE;
	ntsh_func = tsh_func = norm_func;
	tsh_map = norm_map;
	return( TRUE );
}



/*
 * bol:
 * Go to the beginning of the line.
 * Ignores the repeat count.
 */

static  bool  bol( c )

    register  int		c;
{
	tsh_cur = tsh_buf;
	return( TRUE );
}



/*
 * eol:
 * Go to the end of the line.
 * Ignores the repeat count.
 */

static  bool  eol( c )

    register  int		c;
{
	tsh_cur = tsh_end;
	return( TRUE );
}



/*
 * back:
 * Go backward repeat characters.
 * It is an error to single back up past the beginning, but on a run
 * just end there.
 */

static  bool  back( c )

    register  int		c;
{

	if(  tsh_num  >  1  )  {
	    if(  ( tsh_cur -= tsh_num )  <  tsh_buf  )  tsh_cur = tsh_buf;
	}
	else  {
	    if(  --tsh_cur  <  tsh_buf  )  {
		BELL();
		tsh_cur = tsh_buf;
	    }
	}
	return( TRUE );
}



/*
 * frwd:
 * Go forward repeat characters.
 * It is an error to single forward past the end of the line, but on a run
 * just end there.
 */

static  bool  frwd( c )

    register  int		c;
{
	if(  tsh_num  >  1  )  {
	    if(  ( tsh_cur += tsh_num )  >  tsh_end  )  tsh_cur = tsh_end;
	}
	else  {
	    if(  ++tsh_cur  >  tsh_end  )  {
		BELL();
		tsh_cur = tsh_end;
	    }
	}
	return( TRUE );
}



/*
 * dela:
 * Delete repeat characters above the cursor.
 * It is an error to delete a single character past the end of the line,
 * but on a run just delete till there.
 */

static  bool  dela( c )

    register  int		c;
{
	register  char		*p;

	if(  tsh_cur  <  tsh_end  )  {
	    if(  ( p = tsh_cur + tsh_num )  >  tsh_end  )  p = tsh_end;
	    tsh_del_to( p );
	}
	else  {
	    BELL();
	}
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * delb:
 * Delete repeat characters behind the cursor.
 * It is an error to delete a single character before the beginning of the
 * line, but on a run just erase till there.
 */

static  bool  delb( c )

    register  int		c;
{
	register  char		*p;

	if(  tsh_cur  >  tsh_buf  )  {
	    if(  ( p = tsh_cur - tsh_num )  <  tsh_buf  )  p = tsh_buf;
	    tsh_del_from( p );
	}
	else  {
	    BELL();
	}
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * wdfrwd:
 * Go forward repeat count words.  One word forward means to go to the next
 * character following the next white space.
 */

static  bool  wdfrwd( c )

    register  int		c;
{
	register  char		*p;
	register  int		i;

	for( i = tsh_num; i > 0  &&  tsh_cur < tsh_end; --i )  {
	    for( p = tsh_cur; p < tsh_end && !isalnum(*p); p++ );
	    for( ; p < tsh_end && isalnum(*p); p++ );
	    tsh_cur = p;
	}
	return( TRUE );
}



/*
 * wdback:
 * Go backward repeat count words.  One word backward means to go the last
 * character preceding the previous whitespace (but before the second previous
 * white space).
 */

static  bool  wdback( c )

    register  int		c;
{
	register  char		*p;
	register  int		i;

	for( i = tsh_num; i > 0  &&  tsh_cur > tsh_buf; --i )  {
	    for( p = tsh_cur - 1; p > tsh_buf && !isalnum(*p); --p );
	    for( ; p > tsh_buf && isalnum(*p); --p );
	    tsh_cur = ( p > tsh_buf ? p + 1 : tsh_buf );
	}
	return( TRUE );
}



/*
 * wddelp:
 * Delete the previous repeat count words.
 */

static  bool  wddelp( c )

    register  int		c;
{
	register  char		*p;

	p = tsh_cur;
	wdback( c );
	tsh_del_to( p );
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * wddeln:
 * Delete the next repeat count words.
 */

static  bool  wddeln( c )

    register  int		c;
{
	register  char		*p;

	p = tsh_cur;
	wdfrwd( c );
	tsh_del_from( p );
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * twdl:
 * Twiddle (reverse) the two characters before the cursor.  It is an error
 * to twiddle if there aren't two characters before the cursor.
 * Ignores the repeat count.
 */

static  bool  twdl( c )

    register  int		c;
{
	register  int		i;

	if(  tsh_cur  >  ( tsh_buf + 1 )  )  {
	    i = tsh_cur[-2];
	    tsh_cur[-2] = tsh_cur[-1];
	    tsh_cur[-1] = i;
	    if(  ( tsh_cur - 2 )  <  tsh_left  )  tsh_left = tsh_cur - 2;
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * mark:
 * Mark the current line in the history list as the place to be.
 */

static  bool  mark( c )

    register  int		c;
{
	mark_hp = hp;
	return( TRUE );
}



/*
 * hynk:
 * If the marked line is still in the history list, insert it into
 * the current line.
 */

static  bool  hynk( c )

    register  int		c;
{
	static  struct  Hist		*h;

	for( h = Histlist.Hnext; h; h = h->Hnext )  {
	    if(  mark_hp  ==  h  )  break;
	}
	if(  h  )  {
	    tsh_rem_line();
	    tsh_ins_lex( tsh_cur, &mark_hp->Hlex );
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * nop:
 * Don't do anyting.
 * This is used by the alt-mode numeric feature to keep numbers from doing
 * anything when typed after an alt-mode.
 */

static  bool  nop( c )

    register  int		c;
{
	return( TRUE );
}



/*
 * next:
 * Go to the next (more recent) line (if any) in the history list which is longer
 * than the minimum size.
 */

static  bool  next( c )

    register  int		c;
{
	register  struct  Hist	*h, *nh;
	register  int		i;

	for( nh = hp; --tsh_num >= 0; )  {
	    do  {
		for( h = &Histlist; h && h->Hnext != nh; h = h->Hnext );
		nh = h;
	    }  while(  h && h != &Histlist && tsh_len_lex( &h->Hlex ) < tsh_line_min  );
	    if(  h  &&  h != &Histlist  )  {
		hp = h;
	    }
	    else  {
		hp = &Histlist;
		break;
	    }
	}
	if(  hp  !=  &Histlist  )  tsh_rpl_lex( &hp->Hlex );
	else  kill();
	return( TRUE );
}



/*
 * prev:
 * Go to the previous (earlier) line (if any) in the history list which is longer
 * than the minimum size.
 */

static  bool  prev( c )

    register  int		c;
{
	register  struct  Hist	*h;
	register  struct  Hist	*nhp		= (struct Hist *)0;
	register  int		i;

	for( h = hp; h && --tsh_num >= 0; )  {
	    for( h = h->Hnext; h && tsh_len_lex( &h->Hlex ) < tsh_line_min; h = h->Hnext );
	    if(  h  )  nhp = h;
	}
	if(  nhp  )  {
	    hp = nhp;
	    tsh_rpl_lex( &hp->Hlex );
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * boh:
 * go to the beginning (earliest) of the history list (the most recent line
 * which is longer than the minimum size).
 */

static  bool  boh( c )

    register  int		c;
{
	register  int		i;

	hp = &Histlist;
	prev( c );
	return( TRUE );
}



/*
 * eoh:
 * go to the end (oldest) of the history list (the oldest line which is longer
 * than the minimum size).
 */

static  bool  eoh( c )

    register  int		c;
{
	register  int		i;

	while(  hp->Hnext  !=  0  )  hp = hp->Hnext;
	if(  tsh_len_lex( &hp->Hlex )  <  tsh_line_min  )  next( c );
	else  tsh_rpl_lex( &hp->Hlex );
	return( TRUE );
}



/*
 * kill:
 * Get rid of the whole line.
 * Ignores the repeat count.
 */

static  bool  kill( c )

    register  int		c;
{
	register  int		i;

	tsh_rem_line();
	if(  tsh_right  <  ( i = tsh_pos(tsh_end - 1) )  )  tsh_right = i;
	tsh_cur = tsh_end = tsh_left = tsh_buf;
	if(  i  >=  tsh_max_line  )  tsh_redisplay( TRUE, FALSE );
	return( TRUE );
}



/*
 * srch:
 * Search forward until the cursor is under the character specified by
 * the next input character.  Searching for `\n' will find either the
 * next `\n' or the EOL.
 */

static  bool  srch( c )

    register  int		c;
{
	register  int		ch;
	register  char		*p;

	ch = tsh_next_char();
	if(  ch  ==  c  )  {
	    ch = last_search;
	}
	else  {
	    if(  ch  ==  QUOTE_CHAR  )  ch = tsh_next_char();
	    last_search = ch;
	}
	if(  tsh_cur  <  tsh_end  )  {
	    for( p = tsh_cur + 1; p < tsh_end; p++ )  {
		if(  *p  ==  ch  )  {
		    if(  --tsh_num  <=  0  )  break;
		}
	    }
	    if(  *p == ch  ||  ch == '\n'  )  {
		tsh_cur = p;
	    }
	    else  {
		BELL();
	    }
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * rsrch:
 * Like srch(), but goes backwards.
 */

static  bool  rsrch( c )

    register  int		c;
{
	register  int		ch;
	register  char		*p;

	ch = tsh_next_char();
	if(  ch  ==  c  )  {
	    ch = last_search;
	}
	else  {
	    if(  ch  ==  QUOTE_CHAR  )  ch = tsh_next_char();
	    last_search = ch;
	}
	if(  tsh_cur  >  tsh_buf  )  {
	    for( p = tsh_cur - 1; p > tsh_buf; --p )  {
		if(  *p  ==  ch  )  {
		    if(  --tsh_num  <=  0  )  break;
		}
	    }
	    if(  *p  ==  ch  )  {
		tsh_cur = p;
	    }
	    else  {
		BELL();
	    }
	}
	else  {
	    BELL();
	}
	return( TRUE );
}




/*
 * dsrch:
 * Uses srch() to search, but deletes all skipped over characters.
 */

static  bool  dsrch( c )

    register  int		c;
{
	register  char		*p;

	tsh_rem_line();
	p = tsh_cur;
	srch( c );
	tsh_del_from( p );
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}

/*
 * eeol:
 * Erase to end of line.
 */

static	bool	eeol()
{
	tsh_del_to(tsh_end);
	prev_del = TRUE;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * yank:
 * Insert the text in the yank buffer at the current position in the buffer.
 */

static  bool  yank( c )

    register  int		c;
{
	if(  tsh_ybuf  )  {
	    tsh_ins_string( tsh_cur, tsh_ybuf );
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * rdsp:
 * Redisplay the whole line on a clean line.
 * Ignores the repeat count.
 */

static  bool  rdsp( c )

    register  int		c;
{
	tsh_redisplay( TRUE, FALSE );
	return( TRUE );
}



/*
 * numb:
 * Set up the repeat count -- just multiply previous count by four.
 * It is the repeat count.
 */

static  bool  numb( c )

    register  int		c;
{
	tsh_num *= 4;
	tsh_num_reset = FALSE;
	return( TRUE );
}



/*
 * prfx:
 * The prefix character for extended commands.
 * In addition to changing the tsh_func, we must also insure that we
 * don't lose the repeat count, if any.
 */

static  bool  prfx( c )

    register  int		c;
{
	tsh_map = prfx_map;
	ntsh_func = prfx_func;
	tsh_num_reset = FALSE;
	tsh_alt_num = -1;
	prev_del_reset = FALSE;
	return( TRUE );
}



/*
 * insrt:
 * Insert the character at the current position.
 * There are no illegal characters to insert, since they all get filtered
 * before we get here.
 * Not too efficient if tsh_num > 0, but this is rare...
 */

static  bool  insrt( c )

    register  int		c;
{
	register  char		*p, *q;
	register  int		i;
	int                     t_num;


	if ((t_num= &tsh_buf[sizeof(tsh_buf)] - tsh_end) < (unsigned)tsh_num) {
	    BELL();
	}
	else
            t_num = tsh_num;

	for( ; t_num-- > 0; )  {
	    for( p = tsh_end, q = tsh_end - 1; q >= tsh_cur; *p-- = *q-- );
	    if(  tsh_left  >  tsh_cur  )  tsh_left = tsh_cur;
	    *tsh_cur++ = c;
	    *++tsh_end = ' ';
	}
	return( TRUE );
}



/*
 * space:
 * for space characters that cause expansion
 */

static  bool  space( c )

    register  int		c;
{
	register  char			*p;

	for( p = tsh_cur - 1; p > tsh_buf && !isaspace(*p) && *p != HIST; --p );
	if(  *p  ==  HIST  )  tsh_excl( p );
	insrt( c );
	return( TRUE );
}



/*
 * undo:
 * Restore the text from the save buffer to the line, copying the text in
 * the line to the save buffer at the same time..
 */

static  bool  undo( c )

    register  int		c;
{
	char			*malloc();
	register  char		*p, *q;
	register  int		i;
	char			buf[NCARGS];

	if(  tsh_sbuf  )  {
	    for( q = buf, p = tsh_buf; p < tsh_end; *q++ = *p++ );
	    *q = 0;
	    for( q = tsh_buf, p = tsh_sbuf; *p; *q++ = *p++ );
	    free( tsh_sbuf );
	    tsh_sbuf = malloc( tsh_end - tsh_buf + 1 );
	    if(  tsh_right  <  ( i = tsh_pos(tsh_end - 1) )  )  tsh_right = i;
	    tsh_end = q;
	    p = tsh_cur;
	    tsh_cur = old_tsh_cur;
	    old_tsh_cur = p;
	    tsh_left = tsh_buf;
	    for( q = tsh_sbuf, p = buf; *q++ = *p++; );
	    if(  i  >= tsh_max_line  )  tsh_redisplay( TRUE, FALSE );
	}
	else  {
	    BELL();
	}
	return( TRUE );
}



/*
 * quote:
 * Quote the next character.
 * Ignores the repeat count.
 */

static  bool  quote( c )

    register  int		c;
{
	insrt(  tsh_next_char()  );
	return( TRUE );
}



/*
 * exc:
 * Activate the line!
 * We do this by just returning FALSE which ends things.  Of course,
 * we also have to put a line-feed on for the shell, and redisplay if
 * necessary...
 */

static  bool  exc( c )

    register  int		c;
{
	tsh_redisplay( FALSE, TRUE );
	*tsh_end++ = '\n';
	return( FALSE );
}



/*
 * eof:
 * Redisplay the line and terminate...
 */

static  bool  eof( c )

    register  int		c;
{
	tsh_redisplay( FALSE, TRUE );
	return( FALSE );
}



/*
 * intr:
 * Erase the line and return a new-line to the shell to get a new prompt.
 * In a foreach/while kind of thing, we must "do the right thing" -- calling
 * pintr1 seems to be enough to do it!
 * Ignores the repeat count.
 */

static  bool  intr( c )

    register  int		c;
{
	kill();
	tsh_redisplay( FALSE, TRUE );
	*tsh_end++ = '\n';
	if(  whyles  )  pintr1( 0 );
	return( FALSE );
}



/*
 * bad:
 * For illegal characters.
 */

static  bool  bad( c )

    register  int		c;
{
	BELL();
	return( TRUE );
}

/* Character sequences which, when sent to the CRT, move the cursor down,
  back, and to left margin; and which ring the bell; and which move the cursor
  forward, printing a space. */
#define SIZEOF_CRT_STR 32
#define DEFAULT_CRT_STR "/^j/^h/^m/^g/ /"
static char crt_str[SIZEOF_CRT_STR];

#define SIZEOF_CRT_CHARS 5
char *crt_chars[SIZEOF_CRT_CHARS];
#define CONTROL_MASK 037

/*
 * Given the user's string of CRT cursor-manipulating sequences, store them
 * in our own data structure crt_str so we can access them conveniently.
 * Null string not allowed. String which is too short or empty modifies only
 * some (or none) of the default sequences.
 */
static void
do_set_crt_chars(str)
  char *str;
  {
  int i = 0;
  char delimiter = *str, *dest = crt_str;

  if (strlen(str) > SIZEOF_CRT_STR)
    return;
  while ((i < SIZEOF_CRT_CHARS) && (*str))
    {
    str++; /* pass the delimiter */
    if (*str == '\0')
      {
      if (i < (SIZEOF_CRT_CHARS - 1))
	bferr("lineedit variable's string was incomplete");
      break;
      }
    crt_chars[i] = dest;
    while ((*str) && (*str != delimiter))
      if ((*dest++ = *str++) == '^')
	*(dest - 1) = *str++ & CONTROL_MASK;
	
    *dest++ = '\0'; /* null terminate */
    i++;
    }
  }

/*
 * tsh_set_crt_chars:
 * Use the string to set the characters we'll send to the CRT to move the
 * cursor around (or, if the argument is null or empty, use the default).
 */
tsh_set_crt_chars( s )
    register char *s;
    {
    if ((s) && (*s)) do_set_crt_chars(s);
    else do_set_crt_chars(DEFAULT_CRT_STR);
    }


/*
 * tsh_set_edit_chars:
 * Use the default_chars set of default commands to modify the functions bound
 * to each control key.
 * We only allow things up to DELETE_FUNC to be changed
 * If s is null, set things back to the default state.
 */

tsh_set_edit_chars( s )

    register  char		*s;
{
	bool			(*funcs[NFUNCS])(), (*f)();
	register  int		j;

	for( j = 0; j < NFUNCS; j++ )  norm_func[j] = default_func[j];
	if (!s)
	  return;
	for( j = 0; j <= DELETE_FUNC  &&  *s; s++, j++  )  {
	    if(  *s  ==  CONTROL  )  {
		if(  *++s  ==  0  )  break;
		if(  *s  ==  DEL_CHAR  )  f = default_func[ norm_map( 0x7f ) ];
		else  f = default_func[ norm_map( *s & 0x1f ) ];
	    }
	    else  {
		if(  *s == PREFIX_CHAR  ||  *s == PREFIX_PRINT  )  {
		    if(  *++s  ==  0  )  break;
		    f = prfx_func[ prfx_map( *s ) ];
		}
		else  f = default_func[ norm_map( *s ) ];
	    }
	    norm_func[j] = f;
	}
}



/*
 * completion
 */

extern complete();

static bool comp( c )
{
	register  int		leftlength	= tsh_cur - tsh_buf;
	register  int		matches;
	register  char		*cp;
	char			buf[ sizeof tsh_buf ];

	strncpy( buf, tsh_buf, leftlength );		/* copy left of cursor */
	*( cp = buf + leftlength ) = NULL;		/* terminate it */
	matches = complete( buf, sizeof buf, leftlength );
	if( matches == 0 )  {
	    BELL();				/* no match */
	}
	else {					/* at least one match */
	    tsh_rem_line();
	    tsh_left = tsh_cur;			/* can't modify to left */
	    while( *cp )  {
		tsh_num = 1;
		insrt( *cp++ );			/* insert completion */
	    }
	    if( matches > 1 )  rdsp();		/* need new line after list */
	}
	return( TRUE );
}
#endif
