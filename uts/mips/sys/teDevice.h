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
/* $Header: teDevice.h,v 1.1.1.3 90/05/10 06:40:42 wje Exp $ */
/*
 * ANSI Terminal Emulator - device dependent graphics interface.
 */
#ifndef _TEDEVICE_H_ /* { */
#define _TEDEVICE_H_

#define OFF	0
#define ON	1

/*
 * Types
 *
 *	All operations are expressed in Screen Coordinates (SC)
 *	  which are defined as follows:
 *		 ________________
 *		|0 --> X --> MaxX|
 *		||		 |
 *		|v		 |
 *		|Y		 |
 *		||		 |
 *		|v		 |
 *		|MaxY____________|
 *
 *	- (0,0) references the top, left pixel on the screen
 *	- X increases horizontally towards the right side of the screen
 *	- Y increases vertically towards the bottom of the screen
 *	- MaxX and MaxY correspond to the screen width and height in pixels
 */
typedef	unsigned int	GColor;		/* Color index */
typedef	unsigned int	GChInd;		/* Character index */
typedef	int		GCoord;		/* Screen Coordinate value */
#ifndef STANDALONE /* { */
  typedef enum {
	GCHARNORMAL,
	GCHARBOLD,
	GCHARLINE,
	GCHARLINEBOLD,
  } GGrRend;				/* character Graphic Rendition */
#endif /* } ! STANDALONE */

typedef	struct	_GPoint	{		/* Screen Coordinate point */
	GCoord	x, y;
} GPoint;

typedef	struct	_GRect {		/* Screen Coordinate rectangle */
	GPoint	ul, sz;			/*  upper-left corner, size */
} GRect;

/*
 * Device description structure
 */
typedef	struct	_GDevice {
	GPoint	size;			/* display size in pixels */
	unchar	*fb;			/* frame buffer memory address */
	int	fbwidth;		/* frame buffer memory width */
	GRect	window;			/* terminal emulator window in SC */
	GColor	fg, bg;			/* foreground and background colors */
	GColor	bor, cur;		/* border and cursor colours */
	GColor	screen;			/* screen outside frame colour */
	void	(*reset)();		/* reset the device */
	void	(*fillRegion)();	/* fill a rectangle with color */
	void	(*copyRegion)();	/* copy a rectangle to a new place */
	void	(*drawChar)();		/* draw a character */
	void	(*drawCursor)();	/* draw a character cursor */
	void	(*blank)();		/* blank the video */
	void	(*bell)();		/* ring the bell */
} GDevice;

extern	GDevice	*teDev;			/* in teDevice.c */

/*
 * Device dependent drawing functions - sample declarations
 *	(`my' should be replaced with a suitable device abbreviation)
 */
#ifdef SAMPLE_FUNCTION_DECLARATIONS /* { (actually a comment) */

void	myReset()	    /* reset the device */
	int	clr;		/* clear the screen if TRUE */

void	myFillRegion()	    /* fill a rectangle with color */
	GRect	*r;		/* rectangular region in screen coordinates */
	GColor	color;		/* fill color */

void	myCopyRegion()	    /* copy a rectangle to a new place */
	GRect	*sr;		/* source rectangular region in SC */
	GPoint	*dul;		/* destination upper-left corner in SC */

void	myDrawChar()	    /* draw a character */
	GChInd	ci;		/* character index */
	GPoint	*ul;		/* upper-left corner of character position */
	GColor	fg, bg;		/* foreground, background colors */
#ifndef STANDALONE /* { */
	GGrRend	gr;		/* character graphic rendition */
#endif /* } STANDALONE */

void	myDrawCursor()	    /* draw/erase a character cursor */
	GPoint	*ul;		/* upper-left corner of cursor */
	GColor	fg, bg;		/* foreground, background */
	int	onoff;		/* draw(ON)/erase(OFF) flag */

void	myBlank(onoff)	    /* blank/unblank the video */
	int	onoff;		/* blank(ON)/unblank(OFF) flag */

void	myBell()	    /* ring the bell */

#endif /* } SAMPLE_FUNCTION_DECLARATIONS */

#endif /* } ! _TEDEVICE_H_ */
