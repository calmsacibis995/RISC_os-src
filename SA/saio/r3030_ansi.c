#ident "$Header: r3030_ansi.c,v 1.1 90/02/28 21:57:19 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

#ifdef STANDALONE
#include <sys/types.h>
#include <machine/cpu.h>
#include <machine/cpu_board.h>
#include <machine/bitmap.h>
#else
#include <sys/types.h>
#include <sys/ipc.h>
#include <sys/shm.h>
#include <sys/sbd.h>
#include <sys/cpu_board.h>
#include <sys/bitmap.h>
#endif

#define	ROWS	24		/* height of ansi screen */
#define	COLUMNS	80		/* width of ansi screen */
#define	CHARX	12		/* width of char (pixels) */
#define	CHARY	24		/* hight of char (pixels) */
#define	M_YMAX	900		/* monochrome Y height */
#define	M_XMAX	1152		/* monochrome X width */
#define	M_LIMIT	(M_XMAX*M_YMAX/8) /* # of bytes in monochrome screen */
#define	NUM_ARG	10		/* number of ANSI arguments allowed */

/*
 * state machine states
 *
 */

#define	S_NORMAL	0
#define	S_ESC		1
#define	S_BRKT		2
#define	ESC		'\033'

/*
 * possible character options
 */
#define	BOLD	1	/* bold font */
#define	INV	4	/* inverse */
#define	BLANK	8	/* blank */
#define	UL	2	/* under lined */

extern	unsigned long monofont[];

Bitmap	monoscreen;
Bitmap	monoft;

long		num_args;
long		arg[NUM_ARG];
long		state;
char		last_char;
char		tabs[M_XMAX/CHARX];
int		cursor_level;
Rectangle	mono_display;
Point		cursor;
int		options;

Point
chtodisp(p)		/* monochrome char to display */
Point	p;
{
	if (p.x > (COLUMNS + 1))
		p.x = COLUMNS + 1;
	if (p.y > (ROWS + 1))
		p.y = ROWS + 1;
	p.x = (p.x - 1) * CHARX;
	p.y = (p.y - 1) * CHARY;
	return poffset(p, mono_display.origin);
}

xor_cursor()
{
	if (cursor_level)
		return;
	rectfill(&monoscreen, mover(Rect(0,0,CHARX,CHARY), chtodisp(cursor)), NOT(SRC));
}

init_ansi(bp)
register Bits	*bp;
{
	int	counter;

	monoscreen.base = bp; 
	monoscreen.width = (M_XMAX / (sizeof(Bits) * 8));
	monoft.base = monofont;
	monoft.width = 36;
	num_args = 0;
	state = S_NORMAL;
	last_char = 0;
	cursor_level = 0;
	options = 0;

	for(counter = M_LIMIT / (sizeof(Bits)); counter; counter--)
		*bp++ = 0;
	mono_display.origin.x = (M_XMAX - (COLUMNS * CHARX) + 16) / 2;
	mono_display.corner.x = mono_display.origin.x + (COLUMNS * CHARX);
	mono_display.corner.y = M_YMAX - 16;
	mono_display.origin.y = mono_display.corner.y - ROWS * CHARY;
	rectfill(&monoscreen, border(mono_display, 16), FILL);
	rectfill(&monoscreen, border(mono_display, 8), CLEAR);
	cursor.x = 1;
	cursor.y = 1;
	for(counter = 1; counter <= COLUMNS; counter++)
		if ((counter & 7) == 1)
			tabs[counter] = 1;
	xor_cursor();
}

ansi_putchar(ch)
char	ch;
{
	int	tmp;

	if (monoscreen.base == 0)
		return;
	ch &= 0x7f;
#if 0
	{
		static	int	x = 0;
		char		c;
		int		code = SOURCE;

		if ((c = ch) < ' ') {
			c += '@';
			code = NOT(SRC);
		}
		bitop(&monoft, mover(Rect(0,0,CHARX,CHARY),Pt((c - ' ')*CHARX,0)), &monoscreen,Pt(x * CHARX, CHARY), code);
		if (++x >= (M_XMAX/CHARX))
			x = 0;
	}
#endif
	switch(state) {
	case S_NORMAL:
		if (ch == ESC) {
			state = S_ESC;
			break;
		}
		ansi_display(ch);
		break;

	case S_ESC:
		for(num_args = NUM_ARG - 1; num_args >= 0; num_args--)
			arg[num_args] = 0;
		if (ch == '[') {
			state = S_BRKT;
			num_args = 0;
			break;
		}
		if (ch == 'H') {
			tabs[cursor.x] = 1;	/* set_tab */
			state = S_NORMAL;
			break;
		}
		ansi_display(ch);
		state = S_NORMAL;
		break;

	case S_BRKT:
		if ((ch >= '0') && (ch <= '9')) {
			if (num_args == 0)
				num_args = 1;
			arg[num_args] = arg[num_args] * 10 - '0' + ch;
			break;
		}
		if (ch == ';') {
			if (++num_args == NUM_ARG)
				--num_args;
			break;
		}
		xor_cursor();
		state = S_NORMAL;
		switch(ch) {
		case ESC:
			state = S_ESC;
			break;

/*
 * cursor movement functions
 */
		case 'A':	/* up */
			if ((cursor.y -= getarg(1)) < 1)
				cursor.y = 1;
			break;
		case 'B':	/* down */
			if ((cursor.y += getarg(1)) > ROWS)
				cursor.y = ROWS;
			break;
		case 'C':	/* right */
			if ((cursor.x += getarg(1)) > COLUMNS)
				cursor.x = COLUMNS;
			break;
		case 'D':	/* left */
			if ((cursor.x -= getarg(1)) < 1)
				cursor.x = 1;
			break;
		case 'G':	/* abs X */
			if ((cursor.x = getarg(1)) > COLUMNS)
				cursor.x = COLUMNS;
			if (cursor.x < 1)
				cursor.x = 1;
			break;
		case 'd':	/* abs Y */
			if ((cursor.y = getarg(1)) > ROWS)
				cursor.y = ROWS;
			if (cursor.y < 1)
				cursor.y = 1;
			break;
		case 'H':	/* abs Y & X */
			if ((cursor.x = getarg(2)) > COLUMNS)
				cursor.x = COLUMNS;
			if (cursor.x < 1)
				cursor.x = 1;
			if ((cursor.y = getarg(1)) > ROWS)
				cursor.y = ROWS;
			if (cursor.y < 1)
				cursor.y = 1;
			break;

/*
 * line/char inset and delete functions
 */
		case 'L':			/* insert lines */
			if ((tmp = getarg(1)) > (ROWS - cursor.y))
				tmp = ROWS - cursor.y;
			bitop(&monoscreen, Rpt(chtodisp(Pt(1, cursor.y)),
				chtodisp(Pt(COLUMNS + 1, ROWS + 1 - tmp))),
				&monoscreen, chtodisp(Pt(1, cursor.y + tmp)),
				SOURCE);
			rectfill(&monoscreen, Rpt(chtodisp(Pt(1, cursor.y)),
				chtodisp(Pt(COLUMNS + 1, cursor.y + tmp))),
				CLEAR);
			break;
		case 'M':			/* delete lines */
			if ((tmp = getarg(1)) > (ROWS - cursor.y))
				tmp = ROWS - cursor.y;
			bitop(&monoscreen, Rpt(chtodisp(Pt(1, cursor.y + tmp)),
				mono_display.corner), &monoscreen, chtodisp(
				Pt(1, cursor.y)), SOURCE);
			rectfill(&monoscreen, Rpt(chtodisp(Pt(1, ROWS + 1 - tmp)),
				mono_display.corner), CLEAR);
			break;
		case '@':			/* insert chars */
			if ((tmp = getarg(1)) > (COLUMNS - cursor.x))
				tmp = COLUMNS - cursor.x;
			bitop(&monoscreen, Rpt(chtodisp(cursor), chtodisp(Pt(
				COLUMNS + 1 - tmp, cursor.y + 1))), &monoscreen,
				chtodisp(Pt(cursor.x + tmp, cursor.y)),
				SOURCE);
			rectfill(&monoscreen, Rpt(chtodisp(cursor), chtodisp(Pt(
				cursor.x + tmp, cursor.y + 1))) , CLEAR);
			break;
		case 'P':			/* delete chars */
			if ((tmp = getarg(1)) > (COLUMNS - cursor.x))
				tmp = COLUMNS - cursor.x;
			bitop(&monoscreen, Rpt(chtodisp(Pt(cursor.x + tmp,
				cursor.y)), chtodisp(Pt(COLUMNS + 1, cursor.y +
				1))), &monoscreen, chtodisp(cursor), SOURCE);
			rectfill(&monoscreen, Rpt(chtodisp(Pt(COLUMNS + 1 - tmp,
				cursor.y)), chtodisp(Pt(COLUMNS + 1, cursor.y +
				1))), CLEAR);
			break;
		case 'J':			/* clear to end of page */
			rectfill(&monoscreen, Rpt(chtodisp(Pt(1, cursor.y + 1)),
			 	mono_display.corner), CLEAR);
			/* fall through, and clear this line */
		case 'K':			/* clear to end of line */
			rectfill(&monoscreen, Rpt(chtodisp(cursor), 
				Pt(mono_display.corner.x,
				mono_display.origin.y + cursor.y * CHARY)), CLEAR);
			break;

/*
 * tab functions (except set tab, it is \EH in state S_ESC
 */
		case 'g':			/* clear tabs */
			for(tmp = 1; tmp < COLUMNS; tmp++)
				tabs[tmp] = 0;
			break;

		case 'Z':			/* back tab */
			for(tmp = cursor.x - 1; tmp; tmp--)
				if (tabs[tmp])
					break;
			if (tmp < 1)
				tmp = 1;
			cursor.x = tmp;
			break;

/*
 * misc functions - set options/clear options. repeat last char
 */
		case 'm':			/* set options */
			if (num_args < 1) {
				options = 0;
				break;
			}
			for(tmp = 1; tmp <= num_args; tmp++)
				switch(arg[tmp]) {
				case 0:
					options = 0;
					break;
				case 1:
					options |= BOLD;
					break;
				case 4:
					options |= UL;
					break;
				case 7:
					options |= INV;
					break;
				case 8:
					options |= BLANK;
					break;
				}
			break;
		case 'p':			/* repeat last character */
			xor_cursor();
			for(tmp = getarg(1); tmp; tmp--)
				ansi_display(last_char);
			xor_cursor();
			break;
		}
		xor_cursor();
	}
}

ansi_display(ch)
char	ch;
{
	last_char = ch;
	if (ch >= ' ') {
		int	mode = SOURCE;
		int	mode2 = SRC | DST;
		Rectangle r;

		r = mover(Rect(0,0,CHARX,CHARY),Pt((ch - ' ')*CHARX,0));
		if (options & INV) {
			mode = (NOT(SRC));
			mode2 = NOT(SRC) & DST;
		}
		if (options & BLANK) {
			mode = DST;
			mode2 = DST;
		}
		bitop(&monoft, r, &monoscreen, chtodisp(cursor), mode);
		if (options & BOLD) {
			r.corner.x--;
			bitop(&monoft, r, &monoscreen,
				poffset(chtodisp(cursor), Pt(1,0)), mode2);
		}
		if (options & UL) {
			rectfill(&monoscreen, mover(Rect(0,21,12,23),
				chtodisp(cursor)), NOT(DST));
		}
		if (++cursor.x > COLUMNS) {
			cursor.x = 1;
			goto new_line;
		}
		xor_cursor();
		return;
	}
	xor_cursor();
	switch(ch) {
	case '\n':
new_line:
		if (++cursor.y > ROWS) {
			cursor.y--;
			bitop(&monoscreen, Rpt(chtodisp(1,2), mono_display.corner),
				&monoscreen, mono_display.origin, SOURCE);
			rectfill(&monoscreen, Rect(mono_display.origin.x,
				mono_display.corner.y - CHARY,
				mono_display.corner.x, mono_display.corner.y),
				CLEAR);
		}
		break;

	case '\r':
		cursor.x = 1;
		break;

	case '\b':
		if (--cursor.x < 1)
			cursor.x = 1;
		break;

	case '\007':
		break;		/* no bell */

	case '\t':
		{
			int	x;

			x = cursor.x + 1;
			cursor.x = COLUMNS;
			for(; x <= COLUMNS; x++) {
				if (tabs[x]) {
					cursor.x = x;
					break;
				}
			}
		}
	}
	xor_cursor();
}

getarg(num)
int	num;
{
	if ((num > num_args) || (arg[num] == 1))
		return 1;
	return arg[num];
}

ansi_debug_s(s)
char	*s;
{
	while (*s)
		ansi_debug(*s++);
}

ansi_debug(ch)
char	ch;
{
	static	x = 0;
	static	y = 0;
	extern	char	*frm_addr;

	if (frm_addr == 0)
		return 0;
	if (ch >= ' ') {
		Rectangle r;

		r = mover(Rect(0,0,CHARX,CHARY),Pt((ch - ' ')*CHARX,0));
		bitop(&monoft, r, &monoscreen, Pt(x * CHARX, y * CHARY),SOURCE);
		if (++x >= (M_XMAX / CHARX)) {
			x = 0;
			goto debug_new_line;
		}
		return;
	}
	switch(ch) {
	case '\n':
debug_new_line:
		if (++y >= 12) {
			y--;
			bitop(&monoscreen, Rect(0, CHARY, M_XMAX, CHARY * 12),
				&monoscreen, Pt(0, 0), SOURCE);
			rectfill(&monoscreen, Rect(0, CHARY * 11, M_XMAX,
				CHARY * 12), CLEAR);
		}
		break;

	case '\r':
		x = 0;
		break;

	case '\b':
		if (--x < 1)
			x = 0;
		break;

	case '\007':
		break;		/* no bell */
	}
}
