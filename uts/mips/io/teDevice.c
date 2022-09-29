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
#ident	"$Header: teDevice.c,v 1.1.1.3 90/05/10 05:34:37 wje Exp $"

/*
 * ANSI Terminal Emulator - emulator/device interface functions.
 */
#include "sys/types.h"
#include "sys/teDevice.h"
#include "sys/teFont.h"
#include "sys/teState.h"

/*
 * Constants
 */
#define	BORDER		8
#define	ColX(c)		(teDev->window.ul.x + ((c) - 1) * TECHARWD)
#define	RowY(r)		(teDev->window.ul.y + ((r) - 1) * TECHARHT)
#define	NUM_DEVICES	5	/* maximum number of devices supported */

/*
 * Device description structures.
 */
GDevice	*teDev;				/* available to device layer */
static	GDevice	*teDevList[NUM_DEVICES] = {
	(GDevice *)0
};
static	device_ptr = 0;

add_device(dev)
GDevice	*dev;
{
	int	x;

	for(x = 0; x < device_ptr; x++)
		if (teDevList[x] == dev)
			return;			/* already registered */
	if (device_ptr == (NUM_DEVICES - 1)) {
		return -1;
	}
	teDevList[device_ptr++] = dev;
	return 0;
}

/*
 * Reset the terminal emulator window.
 */
void
teDevReset(nrows, ncols, clr)
	u_char	nrows, ncols;		/* size of te window in rows/cols */
	int	clr;			/* clear screen flag */
{
	GRect	r;
	GPoint	p;
	GDevice **dev = teDevList;

	while (teDev = *dev++) {
		/*
		 * Initialize the device.
		 */
		(*teDev->reset)(clr);

		/*
		 * Set the terminal emulator window
		 *   position and size in SC.
		 */
		teDev->window.sz.x = TECHARWD * ncols;
		teDev->window.sz.y = TECHARHT * nrows;
		teDev->window.ul.x = (teDev->size.x - teDev->window.sz.x) >> 1;
		teDev->window.ul.y = teDev->size.y - teDev->window.sz.y - 2 * BORDER;

		/*
		 * Draw the window frame with foreground color.
		 * Clear the window with background color.
		 */
		r.ul.x = teDev->window.ul.x - 2 * BORDER;
		r.ul.y = teDev->window.ul.y - 2 * BORDER;
		r.sz.x = 4 * BORDER + teDev->window.sz.x;
		r.sz.y = 4 * BORDER + teDev->window.sz.y;
		if (clr)
			(*teDev->fillRegion)(&r, teDev->bor);
		r.ul.x += BORDER;
		r.ul.y += BORDER;
		r.sz.x -= 2 * BORDER;
		r.sz.y -= 2 * BORDER;
		if (clr)
			(*teDev->fillRegion)(&r, teDev->bg);
	}
	return;
}

/*
 * Clear a rectangular region of the TE window.
 *   Assumes:
 *	the region to be erased is completely within the window.
 */
void
teClear(ulrow, ulcol, nrows, ncols)
	int	ulrow, ulcol;		/* row/col of upper left corner */
	int	nrows, ncols;		/* number of rows/cols to erase */
{
	GRect	r;
	GDevice **dev = teDevList;

	r.sz.x = ncols * TECHARWD;
	r.sz.y = nrows * TECHARHT;

	while (teDev = *dev++) {
		r.ul.x = ColX(ulcol);
		r.ul.y = RowY(ulrow);
		(*teDev->fillRegion)(&r, teDev->bg);
	}
	return;
}

/*
 * Copy a rectangular region of the TE window.
 *   Assumes:
 *	1) all moves are either horizontal or vertical, not diagonal.
 *	2) all moves (destination and source) are completely within the window.
 */
void
teCopy(sulrow, sulcol, nrows, ncols, dulrow, dulcol)
	int	sulrow, sulcol;		/* row/col of src upper left corner */
	int	nrows, ncols;		/* number of cols/rows to move */
	int	dulrow, dulcol;		/* row/col of dst upper left corner */
{
	GRect	sr;
	GPoint	d;
	GDevice **dev = teDevList;

	if (sulcol == dulcol && sulrow == dulrow)
		return;
	sr.sz.x = ncols * TECHARWD;
	sr.sz.y = nrows * TECHARHT;
	while (teDev = *dev++) {
		sr.ul.x = ColX(sulcol);
		sr.ul.y = RowY(sulrow);
		d.x = ColX(dulcol);
		d.y = RowY(dulrow);
		(*teDev->copyRegion)(&sr, &d);
	}
	return;
}

/*
 * Draw the cursor (invert a character cell).
 *	(onoff is ignored because the cursor is XOR'd)
 */
void
teCursor(row, col, onoff)
	u_char	row, col;		/* position in row/col */
	int	onoff;			/* on/off flag */
{
	GPoint	ul;
	GDevice **dev = teDevList;

	while (teDev = *dev++) {
		ul.x = ColX(col);
		ul.y = RowY(row);
		(*teDev->drawCursor)(&ul, teDev->cur, teDev->bg, onoff);
	}
	return;
}

/*
 * Draw a character at the given position, using sgr.
 *	Returns the character drawn for storage in teState.last (used by REP).
 */
void
#ifdef STANDALONE /* { */
  teDrawChar(c, row, col)
	u_char	c;			/* character index */
	u_char	row, col;		/* position in row/col */
#else /* } STANDALONE { */
  teDrawChar(c, row, col, sgr)
	u_char	c;			/* character index */
	u_char	row, col;		/* position in row/col */
	u_char	sgr;			/* selected graphic rendition */
#endif /* } ! STANDALONE */
{
	GDevice **dev = teDevList;

	u_char	t;
	GPoint	p;
#ifndef STANDALONE /* { */
	GGrRend	gr = GCHARNORMAL;
	int	reverse;

	if (sgr & SgrMask(SGR_UNDERLINE)) {
		if (sgr & SgrMask(SGR_BOLD))
			gr = GCHARLINEBOLD;
		else
			gr = GCHARLINE;
	} else if (sgr & SgrMask(SGR_BOLD))
		gr = GCHARBOLD;
	reverse = sgr & SgrMask(SGR_REVERSE);
#endif /* } ! STANDALONE */

	c = c < TECHAR0 || TECHAR0 + TENCHARS <= c ? TEDEFCHAR : c;
	while (teDev = *dev++) {
		p.x = ColX(col);
		p.y = RowY(row);
#		ifdef STANDALONE /* { */
			(*teDev->drawChar)((GChInd)c, &p, teDev->fg, teDev->bg);
#		else /* } STANDALONE { */
			if (reverse)
				(*teDev->drawChar)((GChInd)c, &p,
				  teDev->bg, teDev->fg, gr);
			else
				(*teDev->drawChar)((GChInd)c, &p,
				  teDev->fg, teDev->bg, gr);
#		endif /* } ! STANDALONE */
	}
	return;
}

/*
 * Bell
 */
void
teBell()
{
	GDevice **dev = teDevList;

	while (teDev = *dev++)
		(*teDev->bell)();
	return;
}
