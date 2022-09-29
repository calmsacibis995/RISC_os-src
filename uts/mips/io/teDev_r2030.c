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
#ident	"$Header: teDev_r2030.c,v 1.1.1.4 90/05/09 15:04:03 wje Exp $"
/*
 * ANSI Terminal Emulator - device dependent graphics functions.
 *	for RS2030 8-plane color display
 */
#ifdef STANDALONE /* { */
#  include "sysv/types.h"
#  include "sysv/param.h"
#  include "mips/cpu.h"
#  include "mips/grafreg.h"
#  include "mips/buzzer.h"
#else /* } STANDALONE { */
#include "sys/types.h"
#include "sys/sbd.h"
#include "sys/stream.h"
#include "sys/grafreg.h"
#include "sys/buzzer.h"
#endif /* } ! STANDALONE */

#include "sys/teState.h"
#include "sys/teDevice.h"

int	nrows = 24;				/* rows and cols of ansi */
int	ncols = 80;				/* screen (setable in rx3030) */
/*
 * Constants
 */
#define XSIZE		1280			/* in pixels=bytes*/
#define YSIZE		1024			/* scanlines */
#define FBWIDTH		2048			/* in pixels=bytes */
#define BGCOLOR		0x00000000
#define FGCOLOR		0x01010101
#define	B_COLOR		0x05050505
#define	C_COLOR		0x03030303
#define	S_COLOR		0x04040404
#define	DEFBELLTIME	20			/* milliseconds */
#define	DEFBELLPITCH	1024			/* Hz */

#ifdef TEST /* { */
   static unchar	frameBuffer[FBWIDTH * YSIZE];
#  define FB		frameBuffer
   static unchar	videoRegs[0x10000];
#  define REGBASE	videoRegs
#else	/* } TEST { */
#  define FB		(PHYS_TO_K1(GRAPHICS_FRAME_ADDR))
#  define REGBASE	(PHYS_TO_K1(GRAPHICS_REG_ADDR))
#endif	/* } TEST */

/*
 * Device interface addresses
 */
#define	FBAbyte(a)	((volatile unchar *)(REGBASE + (a)))
#define	FBAhalf(a)	((volatile ushort *)(REGBASE + (a)))
#define	FBAword(a)	((volatile uint   *)(REGBASE + (a)))
#define	VVRICLR		FBAhalf(0x0000)		/* clear vert. retrace intr */
#define	VBLANK		FBAword(0x0080)		/* blank video */
#define	VUNBLANK	FBAword(0x1000)		/* unblank video */
#define	VWMASK		FBAbyte(0xfe03)		/* write mask */
#define	VCG1		FBAbyte(0xfe83)		/* cursor gen reg 1 */
#define	VCG2		FBAbyte(0xfe87)		/* cursor gen reg 1 */
#define	VCGB		FBAbyte(0xfe8f)		/* cursor gen reg 1 */
#define	VCG4		FBAbyte(0xfe83)		/* cursor gen reg 1 */
#define	VRDADDR		FBAbyte(0xff03)		/* RAMDAC address register */
#define	VRDRRGB		FBAbyte(0xff07)		/* RAMDAC palette register */
#define	VRDCTRL		FBAbyte(0xff0b)		/* RAMDAC control register */
#define	VRDORBG		FBAbyte(0xff0f)		/* RAMDAC overlay rgb reg. */
#define	VIDPROM		FBAbyte(0xff83)		/* ID PROM */

/*
 * Device description structure.
 */
extern	TeState	teState;
extern	void	c8FillRegion(), c8CopyRegion(), c8DrawChar(), c8DrawCursor();
static	void	reset2030c8(), blank2030c8(), bell2030c8();

GDevice	teDev2030c8 = {		/* RS2030 8-Plane Color Device */
	/* screen size */	{XSIZE, YSIZE},
	/* fb */		(unchar *)FB,
	/* fbwidth */		FBWIDTH,
	/* window */		{{0, 0}, {0, 0}},	/* set in teDevice.c */
	/* colors */		FGCOLOR, BGCOLOR,
				B_COLOR, C_COLOR,	/* border & cursor colours */
				S_COLOR,
	/* reset */		reset2030c8,
	/* fillRegion */	c8FillRegion,
	/* copyRegion */	c8CopyRegion,
	/* drawChar */		c8DrawChar,
	/* drawCursor */	c8DrawCursor,
	/* blank */		blank2030c8,
	/* bell */		bell2030c8
};

#ifdef TEST /* { */
int	wbflush() {}
#endif /* } TEST */

/*
 * csu.mips.s needs the following
 */
#ifdef STANDALONE /* { */
  ushort	*vblank = (ushort *)(VBLANK);
  ushort	*vunblank = (ushort *)(VUNBLANK);
  unchar	*vrdaddr = (unchar *)(VRDADDR);
  unchar	*vrdrrgb = (unchar *)(VRDRRGB);
  unchar	*vrdctrl = (unchar *)(VRDCTRL);
#endif /* } STANDALONE */

init_2030_colour()
{
	add_device(&teDev2030c8);
	teState.row = 1;
	teState.col = 1;
	teReset(1);
}

/*
 */
static	void
initCmap2030c8() 
{
	volatile unchar	*a;

	*VRDADDR = 0x04; wbflush();
	*VRDCTRL = 0xff; wbflush();
	*VRDADDR = 0x05; wbflush();
	*VRDCTRL = 0x00; wbflush();
	*VRDADDR = 0x06; wbflush();
	*VRDCTRL = 0xc0; wbflush();
	*VRDADDR = 0x00; wbflush();

	/*
	 * TODO: Should turn off interrupts while writing color map.
	 */
	a = VRDRRGB;
	*a = 0x00; wbflush();		/* color entry 0 = black */
	*a = 0x00; wbflush();
	*a = 0x00; wbflush();

	*a = 0xff; wbflush();		/* color entry 1 = white */
	*a = 0xff; wbflush();
	*a = 0xff; wbflush();

	*a = 0x00; wbflush();		/* color entry 2 = inv cursor */
	*a = 0x00; wbflush();
	*a = 0x00; wbflush();

	*a = 0x00; wbflush();		/* color entry 3 = cursor */
	*a = 0x00; wbflush();
	*a = 0xff; wbflush();

	*a = 0x00; wbflush();		/* color entry 4 = screen */
	*a = 0x00; wbflush();
	*a = 0x00; wbflush();

	*a = 0xff; wbflush();		/* color entry 5 = background */
	*a = 0xff; wbflush();
	*a = 0x00; wbflush();
	blank2030c8(OFF);
	return;
}

/*
 * Reset the device.
 */
static	void
reset2030c8(clr)
	int	clr;
{
	void	c8FillScreen();

	if (clr) {
		initCmap2030c8();
		c8FillScreen(BGCOLOR);
	}
	return;
}

/*
 * Blank the video.
 */
static	void
blank2030c8(onoff)
	int	onoff;		/* ON=BLANK, OFF=UNBLANK */
{
	if (onoff)
		*VBLANK = (uint)VBLANK;
	else
		*VUNBLANK = (uint)VUNBLANK;
	wbflush();
	return;
}

/*
 * Ring the bell.
 */
static	void
bell2030c8()
{
#ifndef TEST /* { */
	kbd_buzzer(DEFBELLTIME, HZ_TO_LOAD(DEFBELLPITCH));
#endif /* } TEST */
	return;
}
