/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: r3030_mono.c,v 1.1.1.6 90/05/09 15:03:39 wje Exp $"

#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"
#include "sys/bitmap.h"
#include "sys/teDevice.h"
#include "sys/ipc.h"		/* needed for stream.h and shm.h */
#include "sys/msg.h"		/* needed for stream.h */
#include "sys/stream.h"		/* defines queue_t in grafreg.h */
#include "sys/grafreg.h"
#include "sys/buzzer.h"

#define	XMAX	1152
#define	YMAX	900
#define	WHITE	-1
#define	BLACK	0
#define	CHARX	12
#define	CHARY	24

#define	INVERSE	1
#define	BOLD	2
#define	UL	4

#define	DEFBELLTIME	20			/* milliseconds */
#define	DEFBELLPITCH	1024			/* Hz */

void	mono_r3030_reset();
void	mono_r3030_fill();
void	mono_r3030_copy();
void	mono_r3030_putc();
void	mono_r3030_cursor();
void	mono_r3030_blank();
void	mono_r3030_bell();

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

extern	unsigned long monofont[];

Bitmap	monoscreen = {
	(Bits *)0,
	(XMAX / (sizeof(Bits) * 8))
};
Bitmap	monoft = {
	(Bits *)monofont,
	36
};

init_r3030_mono(addr, clr)
register Bits *addr;
{
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
	if (r->sz.x < 32)
		rectfill(&monoscreen, Rect(r->ul.x, r->ul.y, r->ul.x + r->sz.x + 1, r->ul.y + r->sz.y + 1), c & 15);
	else {
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
GGrRend	gr;
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
}

void
mono_r3030_cursor(p)
GPoint	*p;
{
	if (!monoscreen.base)
		return;
	rectfill(&monoscreen, mover(Rect(0,0,CHARX,CHARY),Pt(p->x, p->y)), NOT(SRC));
}

mono_blank(blank)
int	blank;
{
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

	if (blank) {
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
	} else {
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR;
		ram_ch_p->dma_laddr = K1_TO_PHYS(monoscreen.base);
		ram_ch_p->dma_block_s = (MONO_FRAME_SIZE >>BLOCK_SHIFT);
		ram_ch_p->dma_mode = CHANNEL_EN | AUTO_RELOAD;
	}
	return 1;
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
	pkbd_buzzer(DEFBELLTIME, HZ_TO_LOAD(DEFBELLPITCH));
}

mono_reset_rambo_addr(addr)
unsigned long addr;
{
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);
	register int s;

	s = splall();
	while(ram_ch_p->dma_block_s != 3)
		;
	while(ram_ch_p->dma_block_s != 2)
		;
	ram_ch_p->dma_mode = CHANNEL_EN;
	while(ram_ch_p->dma_block_s != 0)
		;
	ram_ch_p->dma_laddr = addr;
	ram_ch_p->dma_block_s = (MONO_FRAME_SIZE >>BLOCK_SHIFT);
	ram_ch_p->dma_mode = CHANNEL_EN | AUTO_RELOAD;
	splx(s);
}
