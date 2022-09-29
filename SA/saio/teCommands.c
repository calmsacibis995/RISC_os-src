#ident "$Header: teCommands.c,v 1.1 90/03/22 19:08:21 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ANSI Terminal Emulator - action functions.
 */
#include "sys/types.h"
#ifdef STANDALONE
#include "machine/teState.h"
#else
#include "sys/teState.h"
#endif STANDALONE

/*
 * Constants
 */
#define	ON		1

extern	int	nrows;
extern	int	ncols;
#define	NROWS	nrows
#define	NCOLS	ncols

#define	BVLEN		TABBYTES
#define	BVSetBit(bv,n)	(bv[(n - 1) >> 3] |= 0x80 >> ((n - 1) & 7))
#define	BVSetAll(bv)	{u_char *b; for (b = bv + BVLEN; b != bv; *--b = 0xff);}
#define	BVClrBit(bv,n)	(bv[(n - 1) >> 3] &= ~(0x80 >> ((n - 1) & 7)))
#define	BVClrAll(bv)	{u_char *b; for (b = bv + BVLEN; b != bv; *--b = 0);}

#define	Min(a,b)	((a) < (b) ? (a) : (b))

/*
 * Terminal emulator state.
 */
TeState	teState;

/*
 *--------------------- Support Functions -------------------------------------
 *
 * Return the next set bit in a bit vector starting search at bit n.
 *	(Warning: may find unused/pad bits in the last byte if set.)
 *	Bits are numbered 1 to N, 0 is returned if no set bit is found.
 *   Assumes 1 <= n <= BVLEN * 8.
 */
static	int
bvNextSet(bv, n)
	u_char	*bv;
	int	n;
{
	u_char	*b, *be, m;

	b = &bv[n >> 3];
	m = 0x80 >> (n &= 7);
	for (be = bv + BVLEN; b != be; b++) {
		if (*b)
			for ( ; m; m >>= 1, n++)
				if (*b & m)
					return (b - bv << 3) + n + 1;
		m = 0x80, n = 0;
	}
	return 0;
}

#ifndef STANDALONE /* { */
/*
 * Return the previous set bit in a bit vector starting search at bit n.
 *	Bits are numbered 1 to N, 0 is returned if no set bit is found.
 *   Assumes n <= BVLEN * 8.
 */
static	int
bvPrevSet(bv, n)
	u_char	*bv;
	int	n;
{
	u_char	*b, *be, m;

	if (n <= 2)
		return 0;
	b = &bv[(n -= 2) >> 3];
	m = 0x80 >> (n &= 7);
	for ( ; b >= bv; b--) {
		if (*b)
			for ( ; m != 0x100; m <<= 1, n--)
				if (*b & m)
					return (b - bv << 3) + n + 1;
		m = 1, n = 7;
	}
	return 0;
}
#endif /* } ! STANDALONE */

/*
 * Move the active position down one row.
 */
static	void
teDown(n, ps)
	int	n;
	TeState	*ps;			/* terminal emulator state */
{
	int	r;

	if ((r = ps->row + n) <= NROWS) {
		ps->row = r;
	} else if ((r -= NROWS) < NROWS) {
		teCopy(r + 1, 1, NROWS - r, NCOLS, 1, 1);
		teClear(NROWS - r + 1, 1, r, NCOLS);
		ps->row = NROWS;
	} else {
		teClear(1, 1, NROWS, NCOLS);
		ps->row = NROWS;
	}
	return;
}

/*
 * Move the active position up one row.
 */
static	void
teUp(n, ps)
	int	n;
	TeState	*ps;			/* terminal emulator state */
{
	int	r;

	if ((r = ps->row - n) >= 1) {
		ps->row = r;
	} else if ((r = 1 - r) < NROWS) {
		teCopy(1, 1, NROWS - r, NCOLS, r + 1, 1);
		teClear(1, 1, r, NCOLS);
		ps->row = 1;
	} else {
		teClear(1, 1, NROWS, NCOLS);
		ps->row = 1;
	}
	return;
}


/*
 * Move the active position forward one column.
 */
static	void
teForward(n, ps)
	int	n;
	TeState	*ps;			/* terminal emulator state */
{
	int	c;

	if ((c = ps->col + n) <= NCOLS) {
		ps->col = c;
	} else {
		ps->col = (c - 1) % NCOLS + 1;
		teDown((c - 1) / NCOLS, ps);
	}
	return;
}

/*
 * Move the active position back one column.
 */
static	void
teBack(n, ps)
	int	n;
	TeState	*ps;			/* terminal emulator state */
{
	int	c;

	if ((c = ps->col - n) >= 1) {
		ps->col = c;
	} else {
		ps->col = NCOLS + (c % NCOLS);
		teUp(-c / NCOLS + 1, ps);
	}
	return;
}

/*
 *--------------------- External Entry Point ----------------------------------
 *
 * Reset/initialize the terminal emulator window.
 */
void
teReset(clr)
	int	clr;			/* clear screen flag */
{
	int	n;

	teDevReset(nrows, ncols, clr);

#ifndef STANDALONE /* { */
	/*
	 * Restart the parser.
	 * Reset the parm/intermediate buf.
	 */
	teState.state = TES_RAW;
	teState.pp = teState.seq;
#endif /* } ! STANDALONE */

	/*
	 * Initialize the terminal emulator state.
	 * Initialize tab settings.
	 * Initialize the device.
	 * Turn on the cursor.
	 */
	if (clr) {
		teState.row = teState.col = 1;
	}
#ifndef STANDALONE /* { */
	teState.prev = 0;
	teState.sgr = SGR_NORMAL;
#endif /* } ! STANDALONE */
	for (n = 1; n <= NCOLS; n += DEFTABINTERV)
		BVSetBit(teState.tabs, n);
	if (clr)
		teCursor(teState.row, teState.col, ON);
	return;
}

/*
 *--------------------- Parse Table Entries -----------------------------------
 *
 * Print a character.
 */
void
tePut(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
#ifdef STANDALONE /* { */
	teDrawChar(c, ps->row, ps->col);
#else /* } STANDALONE { */
	teDrawChar(c, ps->row, ps->col, ps->sgr);
#endif /* } ! STANDALONE */
	teForward(1, ps);
	return;
}

/*
 * Bell
 */
void
teBEL(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	teBell();
	return;
}

/*
 * Back Space (fe)
 */
void
teBS(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	teBack(1, ps);
	return;
}

/*
 * Carriage Return (fe)
 */
void
teCR(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	ps->col = 1;
	return;
}

/*
 * Horizontal Tabulation (fe)
 */
void
teHT(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n;

	n = bvNextSet(teState.tabs, (int)teState.col);
	ps->col = n && n < NCOLS ? n : NCOLS;
	return;
}

/*
 * Line Feed (fe)
 */
void
teLF(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	teDown(1, ps);
	return;
}

#ifndef STANDALONE /* { */

/*
 * Cursor Forward (ed 1)
 */
void
teCUF(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	teForward(l[0] && l[1] ? l[1] : 1, ps);
	return;
}

/*
 * Cursor Back (ed 1)
 */
void
teCUB(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	teBack(l[0] && l[1] ? l[1] : 1, ps);
	return;
}

/*
 * Cursor Down (ed 1)
 */
void
teCUD(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	teDown(l[0] && l[1] ? l[1] : 1, ps);
	return;
}

/*
 * Cursor Up (ed 1)
 */
void
teCUU(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	teUp(l[0] && l[1] ? l[1] : 1, ps);
	return;
}

/*
 * Cursor Position (ed 2)
 */
void
teCUP(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(2, ps);

	ps->row = l[0] && l[1] ? Min(l[1], NROWS) : 1;
	ps->col = l[0] > 1 && l[2] ? Min(l[2], NCOLS) : 1;
	return;
}

/*
 * Cursor Horizontal Absolute (ed 1)
 */
void
teHPA(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	ps->col = l[0] && l[1] ? Min(l[1], NCOLS) : 1;
	return;
}

/*
 * Vertical Position Absolute (fe 1)
 */
void
teVPA(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	*l = teParms(1, ps);

	ps->row = l[0] && l[1] ? Min(l[1], NROWS) : 1;
	return;
}

/*
 * Cursor Back Tabulation (ed 1)
 */
void
teCBT(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	i, n, *l = teParms(1, ps);

	i = l[0] && l[1] ? l[1] : 1;
	n = teState.col;
	while (i--)
		if ((n = bvPrevSet(teState.tabs, n)) <= 1)
			break;
	ps->col = (n ? n : 1);
	return;
}

/*
 * Horizontal Tabulation Set (fe)
 */
void
teHTS(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	BVSetBit(teState.tabs, teState.col);
	return;
}

/*
 * Tabulation Clear (fe vs)
 */
void
teTBC(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(TEMAXPARMS, ps), *le;

	if (n = *l++)
		for (le = &l[n]; l < le; l++) {
			if (*l == 0)
				BVClrBit(teState.tabs, teState.col);
			else if (*l == 2 || *l == 3)
				BVClrAll(teState.tabs);
		}
	else
		BVClrBit(teState.tabs, teState.col);
	return;
}

/*
 * Erase in Display (ed vs)
 */
void
teED(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(TEMAXPARMS, ps), *le;
	int	ulr, nr;

	if (n = *l++) {
		for (le = &l[n]; l < le; l++) {
			if (*l == E_ACTtoEND) {
				ulr = ps->row;
				nr = NROWS - ps->row + 1;
			} else if (*l == E_STARTtoACT) {
				ulr = 1;
				nr = ps->row;
			} else if (*l == E_STARTtoEND) {
				ulr = 1;
				nr = NROWS;
			} else
				continue;
			teClear(ulr, 1, nr, NCOLS);
		}
	} else {
		ulr = ps->row;
		nr = NROWS - ps->row + 1;
		teClear(ulr, 1, nr, NCOLS);
	}
	return;
}

/*
 * Erase in Line (ed vs)
 */
void
teEL(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(TEMAXPARMS, ps), *le;
	int	ulc, nc;

	if (n = *l++) {
		for (le = &l[n]; l < le; l++) {
			if (*l == E_ACTtoEND) {
				ulc = ps->col;
				nc = NCOLS - ps->col + 1;
			} else if (*l == E_STARTtoACT) {
				ulc = 1;
				nc = ps->col;
			} else if (*l == E_STARTtoEND) {
				ulc = 1;
				nc = NCOLS;
			} else
				continue;
			teClear((int)ps->row, ulc, 1, nc);
		}
	} else {
		ulc = ps->col;
		nc = NCOLS - ps->col + 1;
		teClear((int)ps->row, ulc, 1, nc);
	}
	return;
}

/*
 * Delete Line (ed 1)
 */
void
teDL(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(1, ps), r;

	n = l[0] && l[1] ? l[1] : 1;
	if ((r = ps->row + n) < NROWS) {
		teCopy(r, 1, NROWS - r + 1, NCOLS, (int)ps->row, 1);
		teClear(NROWS - n + 1, 1, n, NCOLS);
	} else
		teClear((int)ps->row, 1, NROWS - (int)ps->row + 1, NCOLS);
	return;
}

/*
 * Delete Character (ed 1)
 */
void
teDCH(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(1, ps), r;

	n = l[0] && l[1] ? l[1] : 1;
	if ((r = ps->col + n) < NCOLS) {
		teCopy((int)ps->row, r, 1, NCOLS - r + 1,
		  (int)ps->row, (int)ps->col);
		teClear((int)ps->row, NCOLS - n + 1, 1, n);
	} else
		teClear((int)ps->row, (int)ps->col,
		  1, NCOLS - (int)ps->col + 1);
	return;
}

/*
 * Insert Line (ed 1)
 */
void
teIL(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(1, ps), r;

	n = l[0] && l[1] ? l[1] : 1;
	if ((r = ps->row + n) < NROWS) {
		teCopy((int)ps->row, 1, NROWS - r + 1, NCOLS, r, 1);
		teClear((int)ps->row, 1, n, NCOLS);
	} else
		teClear((int)ps->row, 1, NROWS - (int)ps->row + 1, NCOLS);
	return;
}

/*
 * Insert Character (ed 1)
 */
void
teICH(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(1, ps), r;

	n = (l[0] && l[1] ? l[1] : 1);
	if ((r = ps->col + n) < NCOLS) {
		teCopy((int)ps->row, (int)ps->col, 1, NCOLS - r + 1,
		  (int)ps->row, r);
		teClear((int)ps->row, (int)ps->col, 1, n);
	} else
		teClear((int)ps->row, (int)ps->col,
		  1, NCOLS - (int)ps->col + 1);
	return;
}

/*
 * Repeat
 */
void
teREP(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l;
	void	(*f)() = (void (*)())0;

	/*
	 * Only repeat single control characters or printable characters.
	 * Control and Escape sequences are too much trouble.
	 * Besides, most control and escape sequences have count
	 * parameters if it makes sense to repeat them.
	 * (IL, DL, CUU, etc. have counts; SGR, HTS, CUP, etc. don't.)
	 */
	if (ps->prev == ASCII_ESC)
		return;
	else if (ps->prev < ' ')
		f = teCtl;
	else if (ps->prev < ASCII_DEL)
		f = tePut;
	else
		return;
	l = teParms(1, ps);
	n = (l[0] && l[1] ? l[1] : 1);
	while (n--)
		(*f)(ps->prev, ps);
	return;
}

/*
 * Select Graphic Rendition (fe vs)
 */
void
teSGR(c, ps)
	u_char	c;			/* current character */
	TeState	*ps;			/* terminal emulator state */
{
	int	n, *l = teParms(TEMAXPARMS, ps), *le;

	if (n = *l++) {
		for (le = &l[n]; l < le; l++) {
			if (*l == 0)
				ps->sgr = SGR_NORMAL;
			else if (*l <= SGR_MAX)
				ps->sgr |= SgrMask(*l);
		}
	} else
		ps->sgr = SGR_NORMAL;
	return;
}

#endif /* } ! STANDALONE */
