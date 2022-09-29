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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: teDevc8_r3030.c,v 1.1.1.5.1.1.1.2 90/11/02 17:57:03 beacker Exp $"
/*
 * ANSI Terminal Emulator - device dependent graphics functions.
 *	for RS3030 8-plane color display
 */
#ifdef STANDALONE /* { */
#include "sysv/types.h"
#include "sysv/param.h"
#include "mips/cpu.h"
#include "mips/grafreg.h"
#include "mips/buzzer.h"
#else /* } STANDALONE { */
#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/stream.h"
#include "sys/grafreg.h"
#include "sys/buzzer.h"
#include "sys/utsname.h"
#endif /* } ! STANDALONE */

#include "sys/teDevice.h"

/*
 * Constants
 */
#define XSIZE		1280			/* in pixels=bytes*/
#define YSIZE		1024			/* scanlines */
#define BGCOLOR		0x00000000
#define FGCOLOR		0x01010101
#define	B_COLOUR	0x05050505
#define	C_COLOUR	0x03030303
#define	S_COLOUR	0x04040404
#define	DEFBELLTIME	20			/* milliseconds */
#define	DEFBELLPITCH	1024			/* Hz */

#define FBWIDTH		(32 * 1024)		/* in pixels=bytes */
#define REGBASE		(PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR))
#define FB		(PHYS_TO_K1(R3030_GRAPHICS_FRAME_ADDR))

#define	XREG		(R3030_GRAPHICS_REG_ADDR + R3030_XSERVER_OFFSET)
#define	KREG		(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET)
#define	MREG		(R3030_GRAPHICS_REG_ADDR + R3030_WRITE_OFFSET)

#define	RAM_ADDR_L	(PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_RAMDAC_ADLO_OFFSET))
#define	RAM_ADDR_H	(PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_RAMDAC_ADHI_OFFSET))
#define	RAM_IODATA	(PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_COL_REG_OFFSET))
#define	RAM_COLOUR	(PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_COL_DATA_OFFSET))

int	values[] = {
	0x40,
	0x00,
	0xc0,
	0xff,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00,
	0x00
};

int	colours[] = {
	0, 0, 0,	/* back ground */
	255, 255, 255,	/* foreground */
	0, 0, 0,	/* cursor ^ fg */
	0, 0, 255,	/* cursor, cursor ^ bg */
	0, 0, 0,	/* screen */
	255, 255, 0	/* border */
};

#define	VLEN	(sizeof(values)/sizeof(values[0]))
#define	CLEN	(sizeof(colours) / (3 * sizeof(colours[0])))

/*
 * Device description structure.
 */
extern	void	c8FillRegion(), c8CopyRegion(), c8DrawChar(), c8DrawCursor();
extern	void	pkbd_buzzer();
static	void	reset3030c8(), bell3030c8(), r3030_fill_region();
void blank3030c8();
extern	GDevice	*teDev;

GDevice	teDev3030c8 = {		/* RS3030 8-Plane Color Device */
	/* screen size */	{XSIZE, YSIZE},
	/* fb */		(unsigned char *)FB,
	/* fbwidth */		FBWIDTH,
	/* window */		{{0, 0}, {0, 0}},	/* set in teDevice.c */
	/* colors */		FGCOLOR, BGCOLOR,
				B_COLOUR, C_COLOUR,	/* border & cursor colours */
				S_COLOUR,
	/* reset */		reset3030c8,
#if 1
	/* fillRegion */	c8FillRegion,
#else
	/* fillRegion */	r3030_fill_region,
#endif
	/* copyRegion */	c8CopyRegion,
	/* drawChar */		c8DrawChar,
	/* drawCursor */	c8DrawCursor,
	/* blank */		blank3030c8,
	/* bell */		bell3030c8
};

int region_debug = 0;
int colour_retry = 0;
int colour_done = 0;
int colour_errs = 0;

static void
r3030_fill_region(r, c)
GRect	*r;			/* rectangular region in screen coordinates */
GColor	c;
{
	volatile unsigned char *frame;
	volatile unsigned char *line_ptr;
	register int x;
	register int y;
	int	s;

#if 1
	if (region_debug)
		return;
	s = splall();
	line_ptr = (volatile unsigned char *)FB;
	line_ptr += (r->ul.y * 32768);
	line_ptr += r->ul.x;
	for(y = r->sz.y; y; y--) {
		frame = line_ptr;
		for(x = r->sz.x; x; x--)
			*frame++ = c;
		line_ptr += 32768;
	}
	splx(s);
#endif
}

static	void
initCmap3030c8() 
{
	int	x;
	int	s;
	int	*ip;

	s = splall();
	*((volatile unsigned long *)PHYS_TO_K1(XREG)) = 0;
	for(x = 0; x < VLEN; x++)
		putreg(x + 1, 2, values[x]);
	ip = &colours[0];
	for(x = 0; x < VLEN; x++, ip += 3)
		putcolour(x, 0, ip[0], ip[1], ip[2]);
	*((volatile unsigned long *)PHYS_TO_K1(XREG)) = 1;
	*((volatile unsigned long *)PHYS_TO_K1(KREG)) = 0;
	*((volatile          long *)PHYS_TO_K1(MREG)) = -1;
	if (*((volatile unsigned long *)PHYS_TO_K1(KREG)) & 1) {
		panic("Bad K register\n");
	}
	splx(s);
}

putreg(f, s, v)
int	f, s, v;
{
	int	count = 0;

	v &= 0xff;
	*((volatile unsigned long *)RAM_ADDR_L) = f;
	*((volatile unsigned long *)RAM_ADDR_H) = s;
	*((volatile unsigned long *)RAM_IODATA) = v;

	*((volatile unsigned long *)RAM_ADDR_L) = f;
	*((volatile unsigned long *)RAM_ADDR_H) = s;
	while (((0xff & *((volatile unsigned long *)RAM_IODATA)) != v) && (count < 100)) {
		*((volatile unsigned long *)RAM_ADDR_L) = f;
		*((volatile unsigned long *)RAM_ADDR_H) = s;
		*((volatile unsigned long *)RAM_IODATA) = v;

		*((volatile unsigned long *)RAM_ADDR_L) = f;
		*((volatile unsigned long *)RAM_ADDR_H) = s;
		count++;
		colour_retry++;
	}
	if (count)
		colour_errs++;
	colour_done++;
}

putcolour(f, s, r, g, b)
int	f, s, r, g, b;
{
	int	count = 0;
	int	r1, r2, r3;

	r &= 0xff;
	g &= 0xff;
	b &= 0xff;
	*((volatile unsigned long *)RAM_ADDR_L) = f;
	*((volatile unsigned long *)RAM_ADDR_H) = s;
	*((volatile unsigned long *)RAM_COLOUR) = r;
	*((volatile unsigned long *)RAM_COLOUR) = g;
	*((volatile unsigned long *)RAM_COLOUR) = b;

	*((volatile unsigned long *)RAM_ADDR_L) = f;
	*((volatile unsigned long *)RAM_ADDR_H) = s;
	r1 = 0xff & *((volatile unsigned long *)RAM_COLOUR);
	r2 = 0xff & *((volatile unsigned long *)RAM_COLOUR);
	r3 = 0xff & *((volatile unsigned long *)RAM_COLOUR);

	while ((r1 != r || r2 != g || r3 != b) && (count < 100)){
		*((volatile unsigned long *)RAM_ADDR_L) = f;
		*((volatile unsigned long *)RAM_ADDR_H) = s;
		*((volatile unsigned long *)RAM_COLOUR) = r;
		*((volatile unsigned long *)RAM_COLOUR) = g;
		*((volatile unsigned long *)RAM_COLOUR) = b;

		*((volatile unsigned long *)RAM_ADDR_L) = f;
		*((volatile unsigned long *)RAM_ADDR_H) = s;
		r1 = 0xff & *((volatile unsigned long *)RAM_COLOUR);
		r2 = 0xff & *((volatile unsigned long *)RAM_COLOUR);
		r3 = 0xff & *((volatile unsigned long *)RAM_COLOUR);
		count++;
		colour_retry++;
	}
	if (count)
		colour_errs++;
	colour_done++;
}

/*
 * assumes a 1280 * YSIZE screen which is word aligned, and has
 * a fill mode - Rx3030 specific - use fill for a more general
 * interface if needed. - This is built for SPEED!!
 */
clear_colour_screen()
{
	register volatile unsigned long *frame;
	register volatile unsigned long *line_ptr;
		 register int y;
		 register int x;
		 register unsigned long colour = S_COLOUR;

	*((volatile unsigned long *)PHYS_TO_K1(XREG)) = 4;
	line_ptr = (volatile unsigned long *)FB;
	for(y = 0; y < YSIZE; y++) {
		frame = line_ptr;
		for(x = 0; x < 8; x++) {
			frame[0] = colour; frame[2] = colour;
			frame[4] = colour; frame[6] = colour;
			frame[8] = colour; frame[10] = colour;
			frame[12] = colour; frame[14] = colour;
			frame[16] = colour; frame[18] = colour;
			frame[20] = colour; frame[22] = colour;
			frame[24] = colour; frame[26] = colour;
			frame[28] = colour; frame[30] = colour;
			frame[32] = colour; frame[34] = colour;
			frame[36] = colour; frame[38] = colour;
			frame += 40;
		}
		line_ptr += 8192;
	}
	*((volatile unsigned long *)PHYS_TO_K1(XREG)) = 1;
}
		
init_r3030_c8(clr)
int	clr;
{
	add_device(&teDev3030c8);
	reset3030c8(clr);
	strcpy(utsname.m_type, MT_R3030_S);	/* we are now an RS3230 */
}

/*
 * Reset the device.
 */
static	void
reset3030c8(clr)
	int	clr;
{
	void	c8FillScreen();

	initCmap3030c8();
	if (clr)
		clear_colour_screen();
}

/*
 * Blank the video.
 */
void
blank3030c8(onoff)
	int	onoff;		/* ON=BLANK, OFF=UNBLANK */
{
	if (onoff)
	    *((volatile unsigned long *)PHYS_TO_K1(XREG)) = 0;
	else
	    *((volatile unsigned long *)PHYS_TO_K1(XREG)) = 1;
}

/*
 * Ring the bell.
 */
static	void
bell3030c8()
{
#ifndef TEST /* { */
	pkbd_buzzer(DEFBELLTIME, HZ_TO_LOAD(DEFBELLPITCH));
#endif /* } TODO */
}
