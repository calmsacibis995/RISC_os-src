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
#ident	"$Header: line_support.c,v 1.2.1.3 90/05/07 18:13:16 wje Exp $"

#ifdef TSH
#include	"sh.h"
#include	"sh.local.h"
#include	"tsh.h"



/*
 * line_support.c:
 * support routines for the line editor
 *
 * modified 18 sep 84 by bruce to have redisplay with the clear flag
 *	redisplay prompt, too.
 */



extern  int	tsh_out;		/* output stream */

extern  char	tsh_buf[];		/* character holding buffer */
extern  char	*tsh_sbuf;		/* character save for undo */
extern  char	*tsh_ybuf;		/* character save for yank */
extern  char	*tsh_end;		/* one after last char */
extern  char	*tsh_cur;		/* virtual cursor on line */
extern  char	*old_tsh_cur;		/* prev. virtual cursor on line */
extern  int	tsh_cursor;		/* real cursor as position */
extern  char	*tsh_left;		/* leftmost change on line */
extern  int	tsh_right;		/* rightmost change as position */
extern  int	tsh_max_line;		/* max line length for redisp */

extern  bool	prev_del;		/* previous command was delete */

extern  short	*_cshctype_;		/* char typing stuff */

/* Copy null-terminated string from src to dest[index], advancing index as
  we do so */
static void
crt_cpy(dest, index, src)
  char *dest, *src;
  int *index;
  {
  while (*src)
    dest[(*index)++] = *src++;
  }


/*
 * tsh_del_to:
 * delete characters to point marked by q from tsh_cur
 * deleted characters get pre-pended on kill buffer
 */

tsh_del_to( q )

    register  char		*q;
{
	char			*malloc();
	register  char		*p, *y;
	register  int		i;
	char			buf[NCARGS];

	if(  q  >  tsh_cur  )  {
	    for( p = buf, y = tsh_cur; y < q; *p++ = *y++ );
	    if(  tsh_ybuf  )  {
		if(  prev_del  )  {
		    for( y = tsh_ybuf; *y; *p++ = *y++ );
		}
		free( tsh_ybuf );
	    }
	    *p++ = 0;
	    tsh_ybuf = malloc( p - buf );
	    for( y = tsh_ybuf, p = buf; *y++ = *p++; );
	    if(  tsh_right  <  ( i = tsh_pos(tsh_end - 1) )  )  tsh_right = i;
	    i = q - tsh_cur;
	    if(  tsh_left  >  tsh_cur  )  tsh_left = tsh_cur;
	    for( y = tsh_cur; q < tsh_end; *y++ = *q++ );
	    tsh_end -= i;
	}
}



/*
 * tsh_del_from:
 * delete characters from point marked by q to tsh_cur
 * deleted characters get appended on kill buffer
 */

tsh_del_from( p )

    register  char		*p;
{
	char			*malloc();
	register  char		*q, *y;
	register  int		i;
	char			buf[NCARGS];

	if(  p  <  tsh_cur  )  {
	    q = buf;
	    if(  tsh_ybuf  )  {
		if(  prev_del  )  {
		    for( y = tsh_ybuf; *y; *q++ = *y++ );
		}
		free( tsh_ybuf );
	    }
	    for( y = p; y < tsh_cur; *q++ = *y++ );
	    *q++ = 0;
	    tsh_ybuf = malloc( q - buf );
	    for( y = tsh_ybuf, q = buf; *y++ = *q++; );
	    if(  tsh_right  <  ( i = tsh_pos(tsh_end - 1) )  )  tsh_right = i;
	    i = tsh_cur - p;
	    if(  tsh_left  >  p  )  tsh_left = p;
	    q = tsh_cur;
	    tsh_cur = p;
	    for( ; q < tsh_end; *p++ = *q++ );
	    tsh_end -= i;
	}
}



/*
 * tsh_redisplay:
 * Output the stuff necessary to update the display to correspond to
 * the current version of the line.
 */

tsh_redisplay( clean, crlf )

    bool		clean, crlf;
{
	register  char		*p;
	register int		i, k, ek;
	int			j;
	char			obuf[NCARGS];

	j = 0;
	ek = tsh_pos( tsh_end - 1 );
	if(  clean  )  {
	    cshputchar( '\n' );
	    printprompt();
	    tsh_cursor = 0;
	    tsh_left = tsh_buf;			/* force redisplay */
	}
	if(  tsh_left < tsh_end  ||  tsh_right > ek  )  {
	    move_pb( tsh_cursor, tsh_left, obuf, &j );
	    move_bb( tsh_left, tsh_end, obuf, &j );
	    if( !clean )  {		/* no need to do this if clean line */
		k = tsh_right - ek;
		for( i = k; i > 0; --i )  crt_cpy(obuf, &j, BLANK);
		for( i = k; i > 0; --i )  crt_cpy(obuf, &j, BACKWARD);
	    }
	    move_bb( tsh_end, tsh_cur, obuf, &j );
	}
	else  {
	    move_pb( tsh_cursor, tsh_cur, obuf, &j );
	}
	tsh_cursor = tsh_pos( tsh_cur - 1 );
	tsh_left = tsh_end;
	tsh_right = ek;
	if(  crlf  )  {
	    crt_cpy(obuf, &j, LEFT_MARGIN);
	    crt_cpy(obuf, &j, DOWN);
	    tsh_cursor = 0;
	}
	write( tsh_out, obuf, j );
}



/*
 * move_pb:
 * move from integer position "from" on the line to position "to" storing
 * the characters necessary to move in buf starting at position pointed to
 * by indx
 */

static  move_pb( from, to, buf, indx )

    register  int		from;
    register  char		*to, *buf;
    int				*indx;
{
	char		*tsh_ptr();
    	int	j		= *indx;
	register  int		top;
	register  char		*fromp;

	top = tsh_pos( to - 1 );
	if(  from  >=  top  )  {
	    for( ; from > top; --from )  crt_cpy(buf, &j, BACKWARD);
	}
	else  {
	    fromp = tsh_ptr( from );
	    for( ; fromp < to; fromp++ )  {
		if(  *fromp < 0x20  ||  *fromp >= 0x7f  )  {
		    buf[j++] = CONTROL;
		    buf[j++] = ( *fromp < 0x20 ? *fromp + CONTROL_ADD : DEL_CHAR );
		}
		else  {
		    buf[j++] = *fromp;
		}
	    }
	}
	*indx = j;
}



/*
 * move_bb:
 * move from position "from" on the line to position "to" storing the
 * characters necessary to move in buf starting at position pointed to
 * by indx
 */

static  move_bb( from, to, buf, indx )

    register  char		*from, *to, *buf;
    register  int		*indx;
{
    	int		j		= *indx;

	if(  from  >  to  )  {
	    for( --from; from >= to; --from )  {
		crt_cpy(buf, &j, BACKWARD);
		if(  *from < 0x20  ||  *from >= 0x7f  )
		  crt_cpy(buf, &j, BACKWARD);
	    }
	}
	else  {
	    for( ; from < to; from++ )  {
		if(  *from < 0x20  ||  *from >= 0x7f  )  {
		    buf[j++] = CONTROL;
		    buf[j++] = ( *from < 0x20 ? *from + CONTROL_ADD : DEL_CHAR );
		}
		else  {
		    buf[j++] = *from;
		}
	    }
	}
	*indx = j;
}



/*
 * pos:
 * return the position on the line of a char in the buffer
 * More precisely, return the number of spaces that have to
 * be typed from the beginning of the line to erase the character
 * at that position.  (Thus returns one more for a control character
 * than for a norm one).
 */

int  tsh_pos( p )

    register  char		*p;
{
	register  int		i;
	register  char		*s;

	for( i = 0, s = tsh_buf; s <= p; i++, s++ )  {
	    if(  *s < 0x20  ||  *s >= 0x7f  )  i++;
	}
	return( i );
}



/*
 * tsh_ptr:
 * Inverse of pos -- given an integer position, return the buffer index
 * coresponding to it.
 */

static  char  *tsh_ptr( i )

    register  int		i;
{
	register  char		*p;
	register  int		j;

	for( j = 0, p = tsh_buf; j < i; j++, p++ )  {
	    if(  *p < 0x20  ||  *p >= 0x7f  )  j++;
	}
	return( p );
}


/*
 * expand_lex:
 * Take the given lex and put an expanded version of it in the string buf.
 * First guy in lex list is ignored; last guy is ^J which we ignore
 * Only take lex'es from position from to position to inclusive
 * Note: csh sometimes sets bit 8 in characters which causes all kinds of
 * problems if we don't mask it here.
 * Note: excl's in lexes have been un-back-slashed and must be re-back-slashed
 */

static  char  *expand_lex( buf, lex, from, to )

    register  char			*buf;
    register  struct  wordent		*lex;
    int					from, to;
{
	register  struct  wordent	*lp;
	register  char			*p;
        register  char                  ch              = '\0';
	char				*obuf		= buf;

	if(  !lex  )  return( buf );	/* null lex */
	if(  ( lp = lex->next )  ==  lex  )  return( buf );	/* nada */
	if(  lp  == ( lex = lex->prev )  )  return( buf );	/* nada */
	to -= from;
	do  {
	    if(  --from  <  0  )  {
		if(  to--  >=  0  )  {
		    for( p = lp->word; *p; )  {
                        if( ! (ch=='>' || ch=='&') )   /* avoids >!  >&! */
                            if(  ( *p&0x7f )  ==  HIST  )  *buf++ = BACKSLASH;
                        *buf++ = ch = *p++&0x7f;
		    }
		    *buf++ = ' ';
		}
	    }
	}  while(  ( lp = lp->next )  !=  lex  );
	return(  ( --buf < obuf ? obuf : buf )  );	/* flush trailing space */
}



/*
 * tsh_len_lex:
 * Return the length of lex when expanded.
 */

int  tsh_len_lex( lex )

    register  struct  wordent		*lex;
{
	char				*expand_lex();
	register  char			*p;
	char				buf[NCARGS];

	p = expand_lex( buf, lex, 0, NCARGS );
	return( p - buf );
}



/*
 * tsh_ins_string:
 * insert the string *ins in the buffer at the position indicated by *p
 */

tsh_ins_string( p, ins )

    register  char		*p, *ins;
{
	register  char			*q, *s;
	char				buf[NCARGS];

	if(  !ins  ||  !*ins  )  return;
	if(  tsh_left  >  p  )  tsh_left = p;
	for( q = p, s = buf; q < tsh_end; *s++ = *q++ );
	for( ; *ins; *p++ = *ins++ );
	tsh_cur = tsh_end = p;
	for( q = buf; q < s; *tsh_end++ = *q++ );
}



/*
 * tsh_ins_lex:
 * insert the lex *lex in the buffer at the position indicated by *p
 */

tsh_ins_lex( p, lex )

    register  char			*p;
    register  struct  wordent		*lex;
{
	char				*expand_lex();
	char				buf[NCARGS];

	*expand_lex( buf, lex, 0, NCARGS ) = 0;
	tsh_ins_string( p, buf );
}



/*
 * tsh_rpl_lex:
 * Replace the whole line buffer with the given lex
 */

tsh_rpl_lex( lex )

    register  struct  wordent		*lex;
{
	char				*expand_lex();
	register  int		i;

	if(  tsh_right  <  ( i = tsh_pos(tsh_end - 1) )  )  tsh_right = i;
	tsh_end = expand_lex( tsh_buf, lex, 0, NCARGS );
	tsh_cur = tsh_end;
	tsh_left = tsh_buf;
}



/*
 * tsh_rem_line:
 */

tsh_rem_line()
{
	char			*malloc();
	register  char		*p, *q;

	if(  tsh_sbuf  )  free( tsh_sbuf );
	tsh_sbuf = malloc( tsh_end - tsh_buf + 1 );
	for( p = tsh_sbuf, q = tsh_buf; q < tsh_end; *p++ = *q++ );
	*p = 0;
	old_tsh_cur = tsh_cur;
}



/*
 * tsh_excl:
 * An excl has been found at point p -- back up and find some white space
 * (or the beginning of the buffer) and properly expand all the excl's from
 * there up to the current cursor position.
 * We also avoid (trying to) expanding '>!'
 */

tsh_excl( p )

    register  char			*p;
{
	char				*excl_expand();
	register  int			i;
	register  char			*q;

	if(  isaspace( p[1] )  &&  ( isaspace(p[-1]) ||  p[-1] == INDIRECT )  )  {
	    for( q = p - 1; q > tsh_buf && isaspace( *q ); --q );
	    if(  *q  ==  INDIRECT  )  ++p;
	}
	else  {
	    while(  !isaspace( *p )  &&  p > tsh_buf  )  --p;
	}
	for( ; ; )  {
	    while(  *p != HIST  &&  p < tsh_cur  )  ++p;
	    for( i = 1; (p - i) >= tsh_buf && p[-i] == BACKSLASH; i++ );
	    if(  i%2  ==  0  )  ++p;
	    if(  p  >=  tsh_cur  )  return;
	    if(  i%2  ==  1  )  p = excl_expand( p );
	}
}



/*
 * excl_expand:
 * There is an excl to be expanded to p -- do the right thing with it and
 * return a version of p advanced over the expanded stuff.  Also, update
 * tsh_cur and related things as appropriate...
 */

static  char  *excl_expand( p )

    register  char		*p;
{
	char				*expand_lex(), *number();
	register  char			*q;
	register  struct  Hist		*h		= Histlist.Hnext;
	register  struct  wordent	*l;
	int				i, from, to, dval;
	bool				all_dig;
	bool				been_once	= FALSE;
	char				*foo;
	char				*op		= p;
	char				buf[NCARGS];
	char				*bend		= buf;

	if(  !h  )  goto  excl_err;
 excl_sw:
	switch(  *( q = p + 1 )  )  {

	    case  '^':
		bend = expand_lex( buf, &h->Hlex, 1, 1 );
		break;

	    case  '$':
		if(  ( l = h->Hlex.prev )  )  {
		    bend = expand_lex( buf, l->prev->prev->prev, 1, 1 );
		}
		break;

	    case  '*':
		bend = expand_lex( buf, &h->Hlex, 1, NCARGS );
		break;

	    default:
		if(  been_once  )  {			/* too complicated */
		    op = tsh_cur;			/* skip whole thing */
		    goto  excl_ok;
		}
		been_once = TRUE;
		if(  *q  !=  HIST  )  {
		    for( i = 0, all_dig = TRUE; !isaspace(*q) && *q != COLON_CHAR && q < tsh_end; q++ )  {
			if(  !isdigit( *q )  )  all_dig = FALSE;
			else  i = 10*i + *q - '0';
		    }
		    --q;
		    if(  all_dig  )  {
			for( ; h; h = h->Hnext )  {
			    if(  h->Hnum  ==  i  )  break;
			}
		    }
		    else  {
			for( i = q - p; h; h = h->Hnext )  {
			    if(  ( l = &h->Hlex )  )  {
				if(  !strncmp( p + 1, l->next->word, i )  )  break;
			    }
			}
		    }
		}
		if(  !h  )  goto  excl_err;
		for( i = 0; i < 3; i++ )  tsh_end[i] = 0;	/* cheap way to avoid going off end badly */
		if(  q[1]  ==  COLON_CHAR  )  {			/* get some args */
		    p = ++q;
		    if(  !isdigit( q[1] )  &&  q[1] != '-'  &&  q[1] != '$'  )  goto  excl_sw;
		    if(  q[1] == '$'  &&  ( q[2] != '-' || !isdigit(q[3]) )  )  goto  excl_sw;
		    dval = 0;
		    if(  ( l = h->Hlex.prev )  )  {
			for( l = l->prev; l != h->Hlex.next; l = l->prev, dval++ );
		    }
		    if(  !dval  )  goto  excl_err;
		    if(  q[1]  == '-'  )  {
			q = number( q + 1, &to, dval );
			from = ( to <= 0 ? 0 : 1 );
		    }
		    else  {
			q = number( q, &from, dval );
			if(  q[1]  ==  '-'  )  {
			    ++q;
			    if(  !isdigit( q[1] )  &&  q[1] != '$'  )  to = NCARGS;
			    else  q = number( q, &to, dval );
			}
			else  {
			    to = from;
			}
		    }
		    if(  from < 0  ||  to < from  )  goto  excl_err;
		    bend = expand_lex( buf, &h->Hlex, from, to );
		}
		else  {						/* get whole cmd */
		    bend = expand_lex( buf, &h->Hlex, 0, NCARGS );
		}
		break;
	}
    /*
     * Now replace the text from op to q inclusive with the text from buf to bend.
     */
	if(  bend  <=  buf  )  goto  excl_err;
	*bend = 0;
	i = bend - buf;
	foo = tsh_cur;
	tsh_cur = ( q < tsh_end ? q + 1 : q );
	foo -= ( tsh_cur - op );
	tsh_rem_line();
	tsh_del_from( op );
	tsh_ins_string( tsh_cur, buf );
	tsh_cur = foo + i;
	return( op + i );
 excl_ok:
 	return( op + 1 );
 excl_err:
	BELL();
 	return( op + 1 );
}



/*
 * number:
 * Ignore character p points to, return number appearing after that.
 * A '$' by itself means a big number; "$-" is for negative
 * Return p pointing to last char used.
 */

static  char  *number( p, num, dval )

    register  char		*p;
    register  int		*num;
    register  int		dval;
{
	register  int		i;
	register  int		sign		= 1;

	if(  *++p  == '$'  )  {
	    if(  *++p  !=  '-'  )  {
		*num = NCARGS;
		return( --p );
	    }
	    sign = -1;
	    ++p;
	}
	for( i = 0; isdigit( *p ); i = 10*i + *p++ - '0' );
	*num = ( sign < 0 ? dval - i : i );
	return( --p );
}
#endif
