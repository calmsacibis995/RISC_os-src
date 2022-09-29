#ident "$Header: r3030_mono.c,v 1.2.6.2 90/12/20 11:03:08 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
 * |-----------------------------------------------------------|
 * | Copyright (c) 1990       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * |          Restrictive Rights Legend                        |
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 252.227-7013.  |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 *  $ */

#ifdef STANDALONE
typedef unsigned char unchar;
#include "sys/types.h"
#include "machine/cpu.h"
#include "machine/cpu_board.h"
#include "machine/rambo.h"
#include "machine/bitmap.h"
#include "machine/teDevice.h"
#else
#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"
#include "sys/bitmap.h"
#include "sys/teDevice.h"
#endif
#include "machine/grafreg.h"

#define	XMAX	1152
#define	YMAX	900
#define	WHITE	-1
#define	BLACK	0
#define	CHARX	12
#define	CHARY	24

#define	INVERSE	1
#define	BOLD	2
#define	UL	4

void	mono_r3030_reset();
void	mono_r3030_fill();
void	mono_r3030_copy();
void	mono_r3030_putc();
void	mono_r3030_cursor();
void	mono_r3030_blank();
void	mono_r3030_bell();

extern	unsigned long monofont[];

#ifdef STANDALONE
GDevice IVmono_r3030 = {
	{XMAX, YMAX},
	0,
	(XMAX / 32),
	{{0, 0}, {0, 0}},
	WHITE, BLACK,
	WHITE, WHITE,
	BLACK,
	mono_r3030_reset,
	mono_r3030_fill,
	mono_r3030_copy,
	mono_r3030_putc,
	mono_r3030_cursor,
	mono_r3030_blank,
	mono_r3030_bell
};

Bitmap	IVmonoscreen = {
	(Bits *)0,
	(XMAX / (sizeof(Bits) * 8))
};
Bitmap	IVmonoft = {
	(Bits *)monofont,
	36
};

GDevice mono_r3030; 
Bitmap	monoscreen;
Bitmap	monoft;
#else
GDevice mono_r3030 = {
	{XMAX, YMAX},
	0,
	(XMAX / 32),
	{{0, 0}, {0, 0}},
	WHITE, BLACK,
	WHITE, WHITE,
	BLACK,
	mono_r3030_reset,
	mono_r3030_fill,
	mono_r3030_copy,
	mono_r3030_putc,
	mono_r3030_cursor,
	mono_r3030_blank,
	mono_r3030_bell
};

Bitmap	monoscreen = {
	(Bits *)0,
	(XMAX / (sizeof(Bits) * 8))
};
Bitmap	monoft = {
	(Bits *)monofont,
	36
};
#endif


init_r3030_mono(addr, clr)
register Bits *addr;
{
#ifdef STANDALONE
	/* initialize variables */
	mono_r3030 = IVmono_r3030;
	monoscreen = IVmonoscreen;
	monoft = IVmonoft;
#endif
	monoscreen.base = addr;
	mono_r3030_reset(clr);
	add_device(&mono_r3030);
}

void
mono_r3030_reset(clr)
int	clr;
{
	unsigned long *ptr;

	if (clr && (ptr = (unsigned long *)monoscreen.base)) {
		int	count;

		for (count = 0; count < YMAX; count++) {
			ptr[0] = BLACK; ptr[1] = BLACK;
			ptr[2] = BLACK; ptr[3] = BLACK;
			ptr[4] = BLACK; ptr[5] = BLACK;
			ptr[6] = BLACK; ptr[7] = BLACK;
			ptr[8] = BLACK; ptr[9] = BLACK;
			ptr[10] = BLACK; ptr[11] = BLACK;
			ptr[12] = BLACK; ptr[13] = BLACK;
			ptr[14] = BLACK; ptr[15] = BLACK;
			ptr[16] = BLACK; ptr[17] = BLACK;
			ptr[18] = BLACK; ptr[19] = BLACK;
			ptr[20] = BLACK; ptr[21] = BLACK;
			ptr[22] = BLACK; ptr[23] = BLACK;
			ptr[24] = BLACK; ptr[25] = BLACK;
			ptr[26] = BLACK; ptr[27] = BLACK;
			ptr[28] = BLACK; ptr[29] = BLACK;
			ptr[30] = BLACK; ptr[31] = BLACK;
			ptr[32] = BLACK; ptr[33] = BLACK;
			ptr[34] = BLACK; ptr[35] = BLACK;
			ptr += 36;
		}
	}
}

mono_r3030_clear(addr)
register Bits *addr;
{
	monoscreen.base = addr;
	mono_r3030_reset(1);
}

void
mono_r3030_fill(r, c)
GRect	*r;
GColor	c;
{
	if (!monoscreen.base)
		return;
#if 0
	rectfill(&monoscreen, Rect(r->ul.x, r->ul.y, r->ul.x + r->sz.x + 1, r->ul.y + r->sz.y + 1), c & 15);
#else
	{
		unsigned long *fptr;
		unsigned long *fptr_o;
		unsigned long fx, lx;
		unsigned long fm, lm;
		int	x, y;
		int	count;

		fm = ((unsigned)(~0)) >> (r->ul.x & 31);
		lm = ~(((unsigned)(~0)) >> ((r->ul.x + r->sz.x) & 31));
		fx = fm & c;
		lx = lm & c;
		fm = ~fm;
		lm = ~lm;
		fptr_o = (unsigned long *)monoscreen.base + 36 * r->ul.y + (r->ul.x >> 5);
		count = ((r->ul.x + r->sz.x) >> 5) - (r->ul.x >> 5);
		for(y = 0; y < r->sz.y; y++) {
			fptr = fptr_o;
			*fptr = (*fptr & fm) | fx;
			fptr++;
			for(x = 1; x < count; x++) {
				*fptr++ = c;
			}
			*fptr = (*fptr & lm) | lx;
			fptr = (fptr_o += 36);
		}
	}
#endif
}

void
mono_r3030_copy(s, d)
GRect	*s;
GPoint	*d;
{
	if (!monoscreen.base)
		return;
	bitop(&monoscreen, Rect(s->ul.x, s->ul.y, s->ul.x + s->sz.x + 1, s->ul.y + s->sz.y + 1), &monoscreen, Pt(d->x, d->y), SRC);
}

void
mono_r3030_putc(c, p, f, b, gr)
GChInd	c;
GPoint	*p;
GColor	f, b;
#ifdef STANDALONE
int gr;
#else
GGrRend	gr;
#endif
{
	int	mode;
	int	mode2;
	Rectangle r;

	if (!monoscreen.base)
		return;
	r = mover(Rect(0,0,CHARX,CHARY),Pt((c - ' ')*CHARX,0));
	if (b != BLACK) {
		mode = NOT(SRC);
		mode2 = NOT(SRC) & DST;
	} else {
		mode = SRC;
		mode2 = SRC | DST;
	}
	bitop(&monoft, r, &monoscreen, Pt(p->x, p->y), mode);
#ifndef STANDALONE
	switch (gr) {
	case GCHARNORMAL:
		break;
	case GCHARBOLD:
		r.corner.x--;
		bitop(&monoft, r, &monoscreen, Pt(p->x + 1, p->y), mode2);
		break;
	case GCHARLINE:
		rectfill(&monoscreen, mover(Rect(0,21,12,23), Pt(p->x, p->y)), NOT(DST));
		break;
	case GCHARLINEBOLD:
		r.corner.x--;
		bitop(&monoft, r, &monoscreen, Pt(p->x + 1, p->y), mode2);
		rectfill(&monoscreen, mover(Rect(0,21,12,23), Pt(p->x, p->y)), NOT(DST));
		break;
	}
#endif
}

void
mono_r3030_cursor(p)
GPoint	*p;
{
	if (!monoscreen.base)
		return;
	rectfill(&monoscreen, mover(Rect(0,0,CHARX,CHARY),Pt(p->x, p->y)), NOT(SRC));
}

mono_blank(on)
int	on;
{
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

	if (on) {
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
	} else {
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
		ram_ch_p->dma_laddr = K1_TO_PHYS(monoscreen.base);
		ram_ch_p->dma_block_s = (MONO_FRAME_SIZE >>BLOCK_SHIFT);
		ram_ch_p->dma_mode = CHANNEL_EN | AUTO_RELOAD;
	}
}

void
mono_r3030_blank()
{
	if (!monoscreen.base)
		return;
}

void
mono_r3030_bell()
{
	if (!monoscreen.base)
		return;
}
