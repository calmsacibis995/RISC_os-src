#ident "$Header: teDevColor8.c,v 1.1 90/03/22 19:08:40 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright$ */

/*
 * ANSI Terminal Emulator - device dependent graphics functions.
 *	for 8-plane color device (RS2030/3030 color display)
 */
#ifdef STANDALONE /* { */
typedef unsigned char u_char;
typedef unsigned char unchar;
#include "sysv/types.h"
#include "machine/teDevice.h"
#include "machine/teFont.h"
#else /* } STANDALONE { */
#include "sys/types.h"
#include "sys/teDevice.h"
#include "sys/teFont.h"
#endif /* } ! STANDALONE */

/*
 * Constants
 */
#define XSIZE		(teDev->size.x)		/* screen width (pixel=bytes) */
#define YSIZE		(teDev->size.y)		/* screen height (scanlines) */
#define	FB		(teDev->fb)		/* frame buffer address */
#define	FBWIDTH		(teDev->fbwidth)	/* frame buffer width (bytes) */
#define	XPAD		(FBWIDTH - XSIZE)	/* scanline pad (pixel=bytes) */
#define	FBBYTES		(FBWIDTH * YSIZE)	/* total FB size (bytes) */
#define	NEXTROW		(TECHARHT * FBWIDTH)	/* row of chars (bytes) */
#define	SCtoFB(p)	(FB + (p)->y * FBWIDTH + (p)->x)
#define	ByteColor(c)	((c) & 0xff)		/* one byte color */


/*
 * Clear the entire screen with color.
 */
void
c8FillScreen(c)
	GColor	c;
{
	uint	*p, *pe, *le;		/* region, region end, line end */

	for (pe = (p = (uint *)FB) + FBBYTES / 4; p != pe; p += XPAD / 4) {
		for (le = p + XSIZE / 4; p != le; p += 32) {
			p[ 0] = c; p[ 1] = c; p[ 2] = c; p[ 3] = c;
			p[ 4] = c; p[ 5] = c; p[ 6] = c; p[ 7] = c;
			p[ 8] = c; p[ 9] = c; p[10] = c; p[11] = c;
			p[12] = c; p[13] = c; p[14] = c; p[15] = c;
			p[16] = c; p[17] = c; p[18] = c; p[19] = c;
			p[20] = c; p[21] = c; p[22] = c; p[23] = c;
			p[24] = c; p[25] = c; p[26] = c; p[27] = c;
			p[28] = c; p[29] = c; p[30] = c; p[31] = c;
		}
	}
	return;
}

/*
 * Copy a region up.
 *	Assumes source and destination are completely on screen.
 */
static	void
c8CopyUp(sr, dist)
	GRect	*sr;		/* source rectangular region in SC */
	GCoord	dist;		/* vertical translation in SC (hor = 0) */
{
	unchar	*p = SCtoFB(&sr->ul);		/* source ul */
	unchar	*pe = p + sr->sz.y * FBWIDTH;	/* source ll */
	unchar	*le = p + sr->sz.x;		/* source ur */
	unchar	*d = p - dist * FBWIDTH;	/* destination ul */
	uint	r0, r1, r2, r3, r4, r5, r6, r7;
	unchar	*leb;			/* line end - byte region */
	uint	*leo, *lew;		/* line end - oct & fullword regions */
	uint	*pw, *dw;		/* region, dest. pointers in words */
	int	li = FBWIDTH - (le - p);/* line incr - to start of next line */

	for ( ; p != pe; p += li, d += li, le += FBWIDTH) {
		for (leb = (unchar *)((uint)p + 3 & ~3); p != leb; )
			*d++ = *p++;
		pw = (uint *)p; dw = (uint *)d;
		for (leo = (uint *)(p + (le - p & ~31)); pw != leo;
		  pw += 8, dw += 8) {
			r0 = pw[0]; r1 = pw[1]; r2 = pw[2]; r3 = pw[3];
			r4 = pw[4]; r5 = pw[5]; r6 = pw[6]; r7 = pw[7];
			dw[0] = r0; dw[1] = r1; dw[2] = r2; dw[3] = r3;
			dw[4] = r4; dw[5] = r5; dw[6] = r6; dw[7] = r7;
		}
		for (lew = (uint *)((uint)le & ~3); pw != lew; )
			*dw++ = *pw++;
		p = (unchar *)pw; d = (unchar *)dw;
		while (p != le)
			*d++ = *p++;
	}
	return;
}

/*
 * Copy a region down.
 *	Assumes source and destination are completely on screen.
 */
static	void
c8CopyDown(sr, dist)
	GRect	*sr;		/* source rectangular region in SC */
	GCoord	dist;		/* vertical translation in SC (hor = 0) */
{
	unchar	*p = SCtoFB(&sr->ul);		/* source ul */
	unchar	*pe = p + sr->sz.y * FBWIDTH;	/* source ll */
	unchar	*le = p + sr->sz.x;		/* source ur */
	unchar	*d = p + dist * FBWIDTH;	/* destination ul */
	uint	r0, r1, r2, r3, r4, r5, r6, r7;
	unchar	*leb;			/* line end - byte region */
	uint	*leo, *lew;		/* line end - oct & fullword regions */
	uint	*pw, *dw;		/* region, dest. pointers in words */
	int	li = FBWIDTH + (le - p);/* line incr - to start of next line */
					/*   result is greater than FBWIDTH */

	d = d + (pe - p) - FBWIDTH;	/* beginning of last dest. line */
	le = le + (pe - p) - FBWIDTH;	/* end of last source line */
	r0 = (uint)p;
	p = pe - FBWIDTH;		/* beginning of last source line */
	pe = (unchar *)r0 - FBWIDTH;	/* beginning of pre-first source line */
	for ( ; p != pe; p -= li, d -= li, le -= FBWIDTH) {
		for (leb = (unchar *)((uint)p + 3 & ~3); p != leb; )
			*d++ = *p++;
		pw = (uint *)p; dw = (uint *)d;
		for (leo = (uint *)(p + (le - p & ~31)); pw != leo;
		  pw += 8, dw += 8) {
			r0 = pw[0]; r1 = pw[1]; r2 = pw[2]; r3 = pw[3];
			r4 = pw[4]; r5 = pw[5]; r6 = pw[6]; r7 = pw[7];
			dw[0] = r0; dw[1] = r1; dw[2] = r2; dw[3] = r3;
			dw[4] = r4; dw[5] = r5; dw[6] = r6; dw[7] = r7;
		}
		for (lew = (uint *)((uint)le & ~3); pw != lew; )
			*dw++ = *pw++;
		p = (unchar *)pw; d = (unchar *)dw;
		while (p != le)
			*d++ = *p++;
	}
	return;
}

/*
 * Copy a region left.
 *	Assumes source and destination are completely on screen.
 */
static	void
c8CopyLeft(sr, dist)
	GRect	*sr;		/* source rectangular region in SC */
	GCoord	dist;		/* horizontal translation in SC (vert = 0) */
{
	unchar	*p = SCtoFB(&sr->ul);		/* source ul */
	unchar	*pe = p + sr->sz.y * FBWIDTH;	/* source ll */
	unchar	*le = p + sr->sz.x;		/* source ur */
	unchar	*d = p - dist;			/* destination ul */
	uint	li = FBWIDTH - (le - p);	/* line increment */

	for ( ; p != pe; p += li, d += li, le += FBWIDTH)
		while (p != le)
			*d++ = *p++;
	return;
}

/*
 * Copy a region right.
 *	Assumes source and destination are completely on screen.
 */
static	void
c8CopyRight(sr, dist)
	GRect	*sr;		/* source rectangular region in SC */
	GCoord	dist;		/* horizontal translation in SC (vert = 0) */
{
	unchar	*p = SCtoFB(&sr->ul);		/* source ul */
	unchar	*pe = p + sr->sz.y * FBWIDTH;	/* source ll */
	unchar	*le = p + sr->sz.x;		/* source ur */
	unchar	*d = p + dist;			/* destination ul */
	uint	li = le - p;			/* line increment */

	p += li; pe += li; le -= li; d += li;
	for (li += FBWIDTH; p != pe; p += li, d += li, le += FBWIDTH)
		while (p != le)
			*--d = *--p;
	return;
}

#ifndef STANDALONE /* { */

/*
 * Draw an underline in the character cell at location ul,
 *   using fg as the foreground (fg = bg for reverse video).
 */
static	void
c8DrawUnderline(ul, fg)
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg;		/* foreground colors */
{
	unchar	*p = SCtoFB(ul);	/* region */
	unchar	*le;			/* line end */
	unchar	c = ByteColor(fg);

	p += NEXTROW - FBWIDTH * 2;	/* second to last scanline of char */
#	if (TECHARWD > 7) /* { */
#		if (TECHARWD > 15) /* { */
			for (le = p + (TECHARWD & ~7); p != le; )
#		endif /* } */
			{
				p[0] = c; p[1] = c; p[2] = c; p[3] = c;
				p[4] = c; p[5] = c; p[6] = c; p[7] = c;
				p += 8;
			}
#	endif /* } */
#	if (TECHARWD % 8 > 0) /* { */
		p[0] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 1) /* { */
		p[1] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 2) /* { */
		p[2] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 3) /* { */
		p[3] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 4) /* { */
		p[4] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 5) /* { */
		p[5] = c;
#	endif /* } */
#	if (TECHARWD % 8 > 6) /* { */
		p[6] = c;
#	endif /* } */
	return;
}

/*
 * Draw the character indexed by ci with its upper left corner at point ul,
 *   using fg and bg as the foreground and background colors.
 *	Double-strike the character one pixel to the
 *	  left to give a bold graphic rendition.
 */
static	void
c8DrawCharBold(ci, ul, fg, bg)
	GChInd	ci;		/* character index */
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg, bg;		/* foreground, background colors */
{
	unchar	*c = TECharByte(ci);	/* character */
	unchar	*p = SCtoFB(ul);	/* region */
	unchar	*pe, *le, b;		/* char end, line end, char bits */
	unchar	fc = ByteColor(fg);	/* byte sized foreground color */
	unchar	bc = ByteColor(bg);	/* byte sized background color */

	for (pe = p + NEXTROW; p != pe; p += FBWIDTH - (TECHARWD & ~7)) {
#		if (TECHARWD > 7) /* { */
#			if (TECHARWD > 15) /* { */
				for (le = p + (TECHARWD & ~7); p != le; )
#			endif /* } */
				{
					b = *c++;
					b |= b << 1;
#					if (TECHARWD % 8 > 0) /* { */
						b |= *c >> 7;
#					endif /* } */
					p[0] = (b & 0x80 ? fc : bc);
					p[1] = (b & 0x40 ? fc : bc);
					p[2] = (b & 0x20 ? fc : bc);
					p[3] = (b & 0x10 ? fc : bc);
					p[4] = (b & 0x08 ? fc : bc);
					p[5] = (b & 0x04 ? fc : bc);
					p[6] = (b & 0x02 ? fc : bc);
					p[7] = (b & 0x01 ? fc : bc);
					p += 8;
				}
#		endif /* } */
#		if (TECHARWD % 8 > 0) /* { */
			b = *c++;
			b |= b << 1;
			p[0] = (b & 0x80 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 1) /* { */
			p[1] = (b & 0x40 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 2) /* { */
			p[2] = (b & 0x20 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 3) /* { */
			p[3] = (b & 0x10 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 4) /* { */
			p[4] = (b & 0x08 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 5) /* { */
			p[5] = (b & 0x04 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 6) /* { */
			p[6] = (b & 0x02 ? fc : bc);
#		endif /* } */
#		if (TEFONTPAD != 0) /* { */
			c += TEFONTPAD;
#		endif /* } */
	}
	return;
}

/*
 * Draw the character ci at location ul,
 *   using fg and bg as the foreground and background colors.
 *	Draw a horizontal line one line up from the character bottom
 *	  to give an underlined graphic rendition.
 */
static	void
c8DrawCharLine(ci, ul, fg, bg)
	GChInd	ci;		/* character index */
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg, bg;		/* foreground, background colors */
{
	void	c8DrawChar();

	c8DrawUnderline(ul, fg);
	c8DrawChar(ci, ul, fg, bg, GCHARNORMAL);
	return;
}

/*
 * Draw the character ci at location ul,
 *   using fg and bg as the foreground and background colors.
 *	Bold and underlined.
 */
static	void
c8DrawCharLineBold(ci, ul, fg, bg)
	GChInd	ci;		/* character index */
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg, bg;		/* foreground, background colors */
{
	c8DrawUnderline(ul, fg);
	c8DrawCharBold(ci, ul, fg, bg);
	return;
}

#endif /* } ! STANDALONE */

/*
 *--------------------- Device Table Entries ----------------------------------
 *
 * Clear a region with color.
 */
void
c8FillRegion(r, color)
	GRect	*r;		/* rectangular region in screen coordinates */
	GColor	color;		/* fill color */
{
	unchar	*p, *pe, *le;	/* region, region end, line end */
	unchar	*leb;		/* line end - byte region */
	uint	*leo, *lew;	/* line end - oct & fullword regions */
	uint	*pw;		/* region in words */
	int	li;		/* line incr - to start of next line */
	unchar	bc;		/* byte size color */

	if (r->ul.x == 0 && r->ul.y == 0
	  && r->sz.x == XSIZE && r->sz.y == YSIZE) {
		c8FillScreen(color);
		return;
	}

	p = SCtoFB(&r->ul);		/* region start (ul) */
	pe = p + r->sz.y * FBWIDTH;	/* region end (ll) */
	le = p + r->sz.x;		/* line end (ur) */

	/*
	 * For each scanline:
	 *	Do bytes up to fullword address.
	 *	Do maximum octwords.
	 *	Do maximum remaining fullwords.
	 *	Do remaining bytes.
	 */
	bc = ByteColor(color);
	li = FBWIDTH - (le - p);
	for ( ; p != pe; p += li, le += FBWIDTH) {
		for (leb = (unchar *)((uint)p + 3 & ~3); p != leb; )
			*p++ = bc;
		pw = (uint *)p;
		for (leo = (uint *)(p + (le - p & ~31)); pw != leo; pw += 8) {
			pw[0] = color; pw[1] = color;
			pw[2] = color; pw[3] = color;
			pw[4] = color; pw[5] = color;
			pw[6] = color; pw[7] = color;
		}
		for (lew = (uint *)((uint)le & ~3); pw != lew; )
			*pw++ = color;
		p = (unchar *)pw;
		while (p != le)
			*p++ = bc;
	}
	return;
}

/*
 * Copy a region.
 *	Assumes source and destination are completely on screen.
 *	Assumes source and destination share either x or y.
 *	  (unaligned copies are not required)
 */
void
c8CopyRegion(sr, dul)
	GRect	*sr;		/* source rectangular region in SC */
	GPoint	*dul;		/* destination upper-left corner in SC */
{
	GCoord	dx = dul->x - sr->ul.x;
	GCoord	dy = dul->y - sr->ul.y;

	if (dy == 0) {
		if (dx == 0)
			return;
		if (dx < 0)
			c8CopyLeft(sr, -dx);
		else
			c8CopyRight(sr, dx);
		return;
	}
	if (dx != 0)
		return;		/* can't do unaligned copies (not req'd) */
	if (dy < 0)
		c8CopyUp(sr, -dy);
	else
		c8CopyDown(sr, dy);
	return;
}

/*
 * Draw the character indexed by ci with its upper left corner at point ul,
 *   using fg and bg as the foreground and background colors.
 */
void
#ifdef STANDALONE /* { */
  c8DrawChar(ci, ul, fg, bg)
#else /* } STANDALONE { */
  c8DrawChar(ci, ul, fg, bg, gr)
#endif /* } ! STANDALONE */
	GChInd	ci;		/* character index */
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg, bg;		/* foreground, background colors */
#ifndef STANDALONE /* { */
	GGrRend	gr;		/* character graphic rendition */
#endif /* } ! STANDALONE */
{
	unchar	*c, *p;			/* character bitmap, region bitmap */
	unchar	*pe, *le, b;		/* char end, line end, char bits */
	unchar	fc = ByteColor(fg);	/* byte sized foreground color */
	unchar	bc = ByteColor(bg);	/* byte sized background color */
#if ! TECHARALIGN /* { */
	int	n;			/* first bit of char in char byte */
#endif /* } ! TECHARALIGN */

#ifndef STANDALONE /* { */
  static void	(*c8DrawCharGR[])() = {
	/* GCHARBOLD */		c8DrawCharBold,
	/* GCHARLINE */		c8DrawCharLine,
	/* GCHARLINEBOLD */	c8DrawCharLineBold,
  };
#endif /* } ! STANDALONE */

#ifndef STANDALONE /* { */
	if (gr > GCHARNORMAL && gr <= GCHARLINEBOLD) {
		(*c8DrawCharGR[gr - 1])(ci, ul, fg, bg);
		return;
	}
#endif /* } ! STANDALONE */

	c = TECharByte(ci);
	p = SCtoFB(ul);
#	if ! TECHARALIGN /* { */
		n = TECharBit(ci);
#	endif /* } ! TECHARALIGN */

	for (pe = p + NEXTROW; p != pe; p += FBWIDTH - (TECHARWD & ~7)) {
#		if (TECHARWD > 7) /* { */
#			if (TECHARWD > 15) /* { */
				for (le = p + (TECHARWD & ~7); p != le; )
#			endif /* } */
				{
#					if TECHARALIGN /* { */
						b = *c++;
#					else /* } { */
						if (n) {
							b = (*c++ << n);
							b |= *c >> 8 - n;
						} else
							b = *c++;
#					endif /* } TECHARALIGN */
					p[0] = (b & 0x80 ? fc : bc);
					p[1] = (b & 0x40 ? fc : bc);
					p[2] = (b & 0x20 ? fc : bc);
					p[3] = (b & 0x10 ? fc : bc);
					p[4] = (b & 0x08 ? fc : bc);
					p[5] = (b & 0x04 ? fc : bc);
					p[6] = (b & 0x02 ? fc : bc);
					p[7] = (b & 0x01 ? fc : bc);
					p += 8;
				}
#		endif /* } */
#		if (TECHARWD % 8 > 0) /* { */
			b = *c++;
			p[0] = (b & 0x80 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 1) /* { */
			p[1] = (b & 0x40 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 2) /* { */
			p[2] = (b & 0x20 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 3) /* { */
			p[3] = (b & 0x10 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 4) /* { */
			p[4] = (b & 0x08 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 5) /* { */
			p[5] = (b & 0x04 ? fc : bc);
#		endif /* } */
#		if (TECHARWD % 8 > 6) /* { */
			p[6] = (b & 0x02 ? fc : bc);
#		endif /* } */
#		if (TEFONTPAD != 0) /* { */
			c += TEFONTPAD;
#		endif /* } */
	}
	return;
}

/*
 * Draw an inverted character cell cursor with its upper left corner at ul,
 *	(onoff is ignored since the cursor is XOR).
 */
void
c8DrawCursor(ul, fg, bg, onoff)
	GPoint	*ul;		/* upper-left corner of cursor */
	GColor	fg, bg;		/* foreground, background */
	int	onoff;		/* draw(on)/erase(off) flag */
{
	unchar	*p = SCtoFB(ul);	/* source ul */
	unchar	r0, r1, r2, r3, r4, r5, r6, r7;
	unchar	cx = ByteColor(fg ^ bg);/* cx XORs fg into bg, bg into fg */
	unchar	*pe, *le;		/* region end, line end */

	for (pe = p + NEXTROW; p != pe; p += FBWIDTH - (TECHARWD & ~7)) {
#		if (TECHARWD > 7) /* { */
#			if (TECHARWD > 15) /* { */
				for (le = p + (TECHARWD & ~7); p != le; )
#			endif /* } */
				{
					r0 = p[0]; r1 = p[1];
					r2 = p[2]; r3 = p[3];
					r4 = p[4]; r5 = p[5];
					r6 = p[6]; r7 = p[7];
					p[0] = r0 ^ cx; p[1] = r1 ^ cx;
					p[2] = r2 ^ cx; p[3] = r3 ^ cx;
					p[4] = r4 ^ cx; p[5] = r5 ^ cx;
					p[6] = r6 ^ cx; p[7] = r7 ^ cx;
					p += 8;
				}
#		endif /* } */
#		if (TECHARWD % 8 > 0) /* { */
			p[0] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 1) /* { */
			p[1] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 2) /* { */
			p[2] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 3) /* { */
			p[3] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 4) /* { */
			p[4] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 5) /* { */
			p[5] ^= cx;
#		endif /* } */
#		if (TECHARWD % 8 > 6) /* { */
			p[6] ^= cx;
#		endif /* } */
	}
	return;
}
