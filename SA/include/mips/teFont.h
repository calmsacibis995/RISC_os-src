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
#ident	"$Header: teFont.h,v 1.1 90/03/22 18:30:51 chungc Exp $"
/*
 * ANSI Terminal Emulator - bitmap character font interface.
 *
 *  Terminal Emulator Character Font
 *	Font:	Simple ASCII with serifs (constant width).
 *	Size:	~14 point (@ 100x100 dpi)
 *
 *  Font data definition is in teFont.c.
 */
#ifndef _TEFONT_H_ /* { */
#define _TEFONT_H_

#define TECHARWD	12	/* character width (bits) */
#define TECHARHT	24	/* character height (bits) */
#define TECHAR0		32	/* first character in font */
#define TENCHARS	96	/* number of characters in font */
#define TEDEFCHAR	127	/* default character for invalid indices */

/*
 * TEFONTWD	 font bitmap width in bytes
 * TEFONTPAD	 increment (in bytes) to the next scanline of a character
 * TECHARALIGN	 true if all character bitmaps are aligned on byte boundaries
 * TECharByte(i) the address of the first byte of the first scanline of char i
 * TECharBit(i)	 the first bit (0-7) of the character in TECharByte(i)
 *	(TECharBit is only useful [and defined] if TECHARALIGN is false)
 */

#define TEFONT_VERTICAL

#ifdef TEFONT_VERTICAL /* { */
    /*
     * font bitmap is a		A
     * vertical array of	B
     * character bitmaps	C
     */
#  define TEFONTWD	(TECHARWD + 7 >> 3)
#  define TEFONTPAD	0
#  define TECHARALIGN	1
#  define TECharByte(i)	(teFont + ((i) - TECHAR0) * TEFONTWD * TECHARHT)

#else /* } TEFONT_VERTICAL { */
    /*
     * font bitmap is a
     * horizontal array of	ABC
     * character bitmaps
     */
#  define TEFONTWD	(TECHARWD * TENCHARS + 7 >> 3)
#  define TEFONTPAD	(TEFONTWD - (TECHARWD + 7 >> 3))
#  define TECHARALIGN	0
#  define TECharByte(i)	(teFont + (((i) - TECHAR0) * TECHARWD >> 3))
#  define TECharBit(i)	(((i) - TECHAR0) * TECHARWD & 7)

#endif /* } ! TEFONT_VERTICAL */

extern	unsigned char	teFont[];

#endif /* } ! _TEFONT_H_ */
