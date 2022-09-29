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
#ident	"$Header: bitmap.c,v 1.4.1.3 90/05/10 05:11:07 wje Exp $"

#include <sys/bitmap.h>

#ifdef	STANDALONE
#define	NON_OVERLAPPING	NON_OVERLAPPING
#endif	STANDALONE

/*
 * this routine is a general bitmap operator. It does all functions on two
 * congruent rectangles in bitmaps. It handles overlapping operations on
 * rectangles within the same bitmap (sb == db). This routine does not do bounds
 * checking, so illegal rectangles do strange things (usually crash the kernel).
 *
 * If overlapping bit operations are not needed (this is really operations in which
 * the source origin word is (partly) within the destination rectangle) then define
 * NON_OVERLAPPING.
 *
 * The structure of a bitmap is a pointer to the bits (.base) and the number of
 * longs between vertical bits.
 *
 * we assume that all bitmaps start at '0,0' and are 32 bit aligned. We furthermore
 * assume that the screen is little endian (ie, bit 31 comes out first).
 */

#ifdef	COPY_BITOP
bitop(sb, r, db, p, f)
Bitmap	*sb;
Rectangle r;
Bitmap	*db;
Point	p;
int	f;
{
	/*
	 * to make the screen operations faster we copy the screen drawing
	 * operation into cached memory.
	 */
	extern	mem_bitop();
	extern	op();
	static int (*init)() = 0;

	if (!init) {
		init = allign_malloc((int)op - (int)mem_bitop);
		if (!init) {
			mem_bitop(sb, r, db, p, f);
			return;
		}
		bcopy((int)mem_bitop, init, (int)op - (int)mem_bitop));
	}
	(*init)(sb, r, db, p, f);
}

mem_bitop(sb, r, db, p, f)
#else	/* COPY_BITOP */
bitop(sb, r, db, p, f)
#endif	/* COPY_BITOP */
Bitmap	*sb;
Rectangle r;
Bitmap	*db;
Point	p;
int	f;
{
	Bits	*sptr;
	Bits	*dptr;
	Bits	sh;
	Bits	dmask;
	Bits	dmask1;
	Bits	t1, t2, t3, t4;
	Bits	sign;
	long	sadd;
	long	dadd;
	long	srotate;
	long	hcount;
	long	hcount_store;
	long	vcount;
	Rectangle t;

	vcount = r.corner.y - r.origin.y;
	hcount_store = r.corner.x - r.origin.x + (p.x & 31);
#ifndef	NON_OVERLAPPING
	if (sb == db) {
		t.origin = p;
		t.corner.x = p.x + r.corner.x - r.origin.x;
		t.corner.y = p.y + r.corner.y - r.origin.y;
		if (!(r.origin.x<t.corner.x && t.origin.x<r.corner.x &&
		      r.origin.y<t.corner.y && t.origin.y<r.corner.y))
			goto topleft;
		if ((r.origin.x < p.x) && (r.origin.y == t.origin.y))
			goto rightleft;
		if (r.origin.y >= t.origin.y)
			goto topleft;
		sptr = sb->base + (r.corner.y-1) * sb->width +
			(r.origin.x >> 5);
		dptr = db->base + (t.corner.y-1) * db->width +
			(p.x >> 5);
		sadd = -sb->width - (hcount_store >> 5) - 1;
		dadd = -db->width - (hcount_store >> 5);
	} else {
topleft:
#endif	NON_OVERLAPPING
		sptr = sb->base + r.origin.y * sb->width +
			(r.origin.x >> 5);
		dptr = db->base + p.y * db->width +
			(p.x >> 5);
		sadd = sb->width - (hcount_store >> 5) - 1;
		dadd = db->width - (hcount_store >> 5);
#ifndef	NON_OVERLAPPING
	}
#endif	NON_OVERLAPPING
	srotate = (p.x & 31) - (r.origin.x & 31);
	sign = 0;
	dmask = (unsigned)(~0) >> (p.x & 31);
	if (((dmask1 = ~((unsigned)(~0) >> (hcount_store & 31))) == 0) && hcount_store) {
		dmask1 = (Bits)-1;
		sadd++;
		dadd++;
	}
	if (srotate < 0) {
		sign = 1;
		srotate *= -1;
	}
	while (vcount--) {
		hcount = hcount_store;
		if (sign) {
			/*
			 * we rotate to the "screen" left
			 */
			t1 = (*sptr << srotate);
			sh = *++sptr;
			t1 |= (sh >> (32 - srotate));
		} else {
			/*
			 * we rotate the "screen" right
			 */
			t1 = ((sh = *sptr++) >> srotate);
		}
		hcount -= 32;
		t4 = *dptr;		/* t4 = masked destination */
		t3 = t4 & dmask;
		t2 = t4 ^ t3;
		t3 = op(f, t1, t3);
		t2 |= t3 & dmask;
		while(hcount > 0) {
			*dptr++ = t2;
			if (sign) {
				t1 = sh << srotate;
				sh = *++sptr;
				t1 |= (sh >> (32 - srotate));
			} else {
				if (srotate == 0) {
					t1 = *sptr++;
				} else {
					t1 = (sh << (32 - srotate));
					sh = *sptr++;
					t1 |= (sh >> srotate);
				}
			}
			hcount -= 32;
			t2 = op(f, t1, t4 = *dptr);
		}
		t3 = t4 & ~dmask1;
		t3 |= (t2 & dmask1);
		*dptr = t3;
		sptr += sadd;
		dptr += dadd;
	}
	return;
#ifndef	NON_OVERLAPPING
rightleft:
	/*
	 * this is a special case, we have to move the screen to the
	 * right only, so we must move bits starting at the left hand
	 * side.
	 */
	sptr = sb->base + r.origin.y * sb->width +
		(r.corner.x >> 5);
	dptr = db->base + p.y * db->width +
		(t.corner.x >> 5);
	sadd = sb->width + (hcount_store >> 5) + 1;
	dadd = db->width + (hcount_store >> 5);
	dmask = ((unsigned)(~0) >> (p.x & 31));
	srotate = (t.corner.x & 31) - (r.corner.x & 31);
	if (((dmask1 = ~((unsigned)(~0) >> (t.corner.x & 31))) == 0) &&
	     hcount_store) {
		hcount_store++;
	}
	sign = 0;
	if (srotate < 0) {
		sign = 1;
		srotate *= -1;
	}
	while (vcount--) {
		hcount = hcount_store;
		if (sign) {
			/*
			 * we rotate to the "screen" left
			 */
			t1 = ((sh = *sptr--) << srotate);
		} else {
			if (srotate == 0) {
				t1 = *sptr--;
			} else {
				/*
				 * we rotate the "screen" right
				 */
				t1 = (*sptr >> srotate);
				sh = *--sptr;
				t1 |= (sh << (32 - srotate));
			}
		}
		hcount -= 32;
		t4 = *dptr;		/* t4 = masked destination */
		t3 = t4 & dmask1;
		t2 = t4 ^ t3;
		t3 = op(f, t1, t3);
		t2 |= t3 & dmask1;
		while(hcount > 0) {
			*dptr-- = t2;
			if (sign) {
				t1 = (sh >> (32 - srotate));
				sh = *sptr--;
				t1 |= (sh << srotate);
			} else {
				if (srotate == 0) {
					t1 = *sptr--;
				} else {
					t1 = sh >> srotate;
					sh = *--sptr;
					t1 |= (sh << (32 - srotate));
				}
			}
			hcount -= 32;
			t2 = op(f, t1, t4 = *dptr);
		}
		t3 = t4 & ~dmask;
		t3 |= (t2 & dmask);
		*dptr = t3;
		sptr += sadd;
		dptr += dadd;
	}
#endif	NON_OVERLAPPING
}

/*
 * this is a software implementation of the standard hardware 'bitop' result:
 *
 *  INPUTS				function code
 *  S  D	0 1 2 3 4 5 6 7 8 9 A B C D E F
 *
 *  0  0	0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 1
 *  0  1	0 0 0 0 1 1 1 1 0 0 0 0 1 1 1 1
 *  1  0	0 0 1 1 0 0 1 1 0 0 1 1 0 0 1 1		<-- result table
 *  1  1	0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1
 */
op(code, s, d)
int	code;
Bits	s;	/* source */
Bits	d;	/* destination */
{
	switch(code) {
	case 0: return 0;
	case 1: return s & d;
	case 2: return ~((~s)|d);
	case 3: return s;
	case 4: return (~s)&d;
	case 5: return d;
	case 6: return s ^ d;
	case 7: return s | d;
	case 8: return ~(s|d);
	case 9: return (~s)^d;
	case 10: return ~d;
	case 11: return s&(~d);
	case 12: return ~s;
	case 13: return (~s)|d;
	case 14: return ~(s&d);
	case 15: return ~0;
	}
}

/*
 * interfaces to the bitop routine for other routines
 */

rectfill(sb, r, f)		/* fill rectangle with function f */
Bitmap	*sb;
Rectangle r;
int	f;
{
	bitop(sb, r, sb, r.origin, f);
}

/*
 * move rectangle r by (x,y) offset in o
 */
Rectangle
mover(r, o)
Rectangle r;
Point o;
{
	r.origin.x += o.x;
	r.origin.y += o.y;
	r.corner.x += o.x;
	r.corner.y += o.y;
	return r;
}

/*
 * add (x, y) offset in o to Point a
 */
Point
poffset(a, o)
Point a, o;
{
	a.x += o.x;
	a.y += o.y;
	return a;
}
