#ident "$Header: keymap_r3030.c,v 1.1 90/05/22 15:10:21 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright 
 * |-----------------------------------------------------------|
 * | Copyright (c) 1987, 1990 MIPS Computer Systems, Inc.      |
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

#include "sys/param.h"
#ifdef KERNEL
#include "sys/keymap.h"
#else
#include "machine/keymap.h"
#endif

#define	UNIX_KEYBOARD	0
#define	AT_KEYBOARD	1

/***************************** AT Keymap ***************************/
/*				type	, base,	 shift, control    */
struct keydef at_keymap[] = {
/* 01,1  f9 	*/		FUNCTION,   9,     -1,   -1,
/* 02,2   	*/		     -1,   -1,     -1,   -1,
/* 03,3  f5 	*/		FUNCTION,   5,     -1,   -1,
/* 04,4  f3 	*/		FUNCTION,   3,     -1,   -1,
/* 05,5  f1	*/		FUNCTION,   1,     -1,   -1,
/* 06,6  f2	*/		FUNCTION,   2,     -1,   -1,
/* 07,7  f12	*/		FUNCTION,  12,     -1,   -1,
/* 08,8  	*/		     -1,   -1,     -1,   -1,
/* 09,9  f10	*/		FUNCTION,  10,     -1,   -1,
/* 0A,10 f8	*/		FUNCTION,   8,     -1,   -1,
/* 0B,11 f6	*/		FUNCTION,   6,     -1,   -1,
/* 0C,12 f4	*/		FUNCTION,   4,     -1,   -1,
/* 0D,13 tab	*/		ASCII,      9,      0,   -1,
/* 0E,14 `	*/		ASCII,   0x60,    '~',   -1,
/* 0F,15  	*/		     -1,   -1,     -1,   -1,
/* 10,16 	*/		     -1,   -1,     -1,   -1,
/* 11,17 alt(L)	*/		SHIFT,    ALT,     -1,   -1,
/* 12,18 shift(L)*/		SHIFT, LSHIFT,     -1,   -1,
/* 13,19 	*/		     -1,   -1,     -1,   -1,
/* 14,20 ctrl   */		SHIFT,   CTRL,     -1,   -1,
/* 15,21 q	*/		ALPHA,    'q',    'Q',   17,
/* 16,22 1	*/		NUMERIC,   '1',   '!',   -1,
/* 17,23 	*/		     -1,   -1,     -1,   -1,
/* 18,24 	*/		     -1,   -1,     -1,   -1,
/* 19,25 	*/		     -1,   -1,     -1,   -1,
/* 1A,26 z	*/		ALPHA,    'z',    'Z',   26,
/* 1B,27 s	*/		ALPHA,    's',    'S',   19,
/* 1C,28 a	*/		ALPHA,    'a',    'A',    1,
/* 1D,29 w 	*/		ALPHA,    'w',    'W',   23,
/* 1E,30 2	*/		NUMERIC,  '2',    '@',    0,
/* 1F,31 	*/		     -1,   -1,     -1,   -1,
/* 20,32 	*/		     -1,   -1,     -1,   -1,
/* 21,33 c	*/		ALPHA,    'c',    'C',    3,
/* 22,34 x	*/		ALPHA,    'x',    'X',   24,
/* 23,35 d	*/		ALPHA,    'd',    'D',    4,
/* 24,36 e	*/		ALPHA,    'e',    'E',    5,
/* 25,37 4	*/		NUMERIC,  '4',    '$',   -1,
/* 26,38 3	*/		NUMERIC,  '3',    '#',   -1,
/* 27,39 	*/		     -1,   -1,     -1,   -1,
/* 28,40 	*/		     -1,   -1,     -1,   -1,
/* 29,41 sp	*/		ALPHA,    ' ',    ' ',  ' ',	
/* 2A,42 v	*/		ALPHA,    'v',    'V',   22,
/* 2B,43 f	*/		ALPHA,    'f',    'F',    6,
/* 2C,44 t	*/		ALPHA,    't',    'T',   20,
/* 2D,45 r	*/		ALPHA,    'r',    'R',   18,
/* 2E,46 5	*/		NUMERIC,  '5',    '%',   -1,
/* 2F,47 	*/		     -1,   -1,     -1,   -1,
/* 30,48 	*/		     -1,   -1,     -1,   -1,	
/* 31,49 n	*/		ALPHA,    'n',    'N',   14,	
/* 32,50 b	*/		ALPHA,    'b',    'B',    2,	
/* 33,51 h	*/		ALPHA,    'h',    'H',    8,	
/* 34,52 g	*/		ALPHA,    'g',    'G',    7,
/* 35,53 y	*/		ALPHA,    'y',    'Y',   25,
/* 36,54 6	*/	        NUMERIC,  '6',    '^',   30,
/* 37,55 	*/		     -1,   -1,     -1,   -1,
/* 38,56  	*/		     -1,   -1,     -1,   -1,
/* 39,57 	*/		     -1,   -1,     -1,   -1,
/* 3A,58 m	*/		ALPHA,    'm',    'M', 0x0d,
/* 3B,59 j	*/		ALPHA,    'j',    'J', 0x0a,
/* 3C,60 u	*/		ALPHA,    'u',    'U',   21,
/* 3D,61 7	*/		NUMERIC,  '7',    '&',   -1,
/* 3E,62 8	*/		NUMERIC,  '8',    '*',   -1,
/* 3F,63 	*/		     -1,   -1,     -1,   -1,
/* 40,64 	*/		     -1,   -1,     -1,   -1,
/* 41,65 ,	*/		ASCII,    ',',    '<',   -1,
/* 42,66 k	*/		ALPHA,    'k',    'K',   11,
/* 43,67 i	*/		ALPHA,    'i',    'I',    9,
/* 44,68 o	*/		ALPHA,    'o',    'O',   15,
/* 45,69 0	*/		NUMERIC,  '0',    ')',   -1,
/* 46,70 9	*/		NUMERIC,  '9',    '(',   -1,
/* 47,71 	*/		     -1,   -1,     -1,   -1,
/* 48,72 	*/		     -1,   -1,     -1,   -1,
/* 49,73 .	*/		ASCII,    '.',    '>',   -1,
/* 4A,74 /	*/		ASCII,    '/',    '?',   -1,
/* 4B,75 l	*/		ALPHA,    'l',    'L',   12,
/* 4C,76 ;	*/		ASCII,    ';',    ':',   -1,
/* 4D,77 p	*/		ALPHA,    'p',    'P',   16,
/* 4E,78 -	*/		ASCII,    '-',    '_',   31,
/* 4F,79 	*/		     -1,   -1,     -1,   -1,
/* 50,80 	*/		     -1,   -1,     -1,   -1,
/* 51,81 	*/		     -1,   -1,     -1,   -1,
/* 52,82 '	*/		ASCII,   0x27,   0x22,   -1,
/* 53,83 	*/		     -1,   -1,     -1,   -1,
/* 54,84 [	*/		ASCII,	   '[',   '{',   27,
/* 55,85 =	*/		ASCII,     '=',   '+',   -1,
/* 56,86 	*/		     -1,    -1,    -1,   -1,
/* 57,87 	*/		     -1,    -1,    -1,   -1,
/* 58,88 CAPS LK*/		TOGGLE, CAPSLOCK,    -1,   -1,
/* 59,89 right SHIFT	*/	SHIFT,  LSHIFT,    -1,   -1,
/* 5A,90 CR	*/		ASCII,    0x0d,  0x0d,   10,
/* 5B,91 ]	*/		ASCII,     ']',   '}',   29,
/* 5C,92 	*/		     -1,    -1,    -1,   -1,
/* 5D,93 \ 	*/		ASCII,    '\\',   '|',   28,
/* 5E,94 	*/		     -1,    -1,    -1,   -1,
/* 5F,95 	*/		     -1,    -1,    -1,   -1,
/* 60,96 	*/		     -1,    -1,    -1,   -1,
/* 61,97 	*/		     -1,    -1,    -1,   -1,
/* 62,98 	*/		     -1,    -1,    -1,   -1,
/* 63,99 	*/		     -1,    -1,    -1,   -1,
/* 64,100 	*/		     -1,    -1,    -1,   -1,
/* 65,101 	*/		     -1,    -1,    -1,   -1,
/* 66,102 bksp	*/		ASCII,       8,     8,  127,
/* 67,103 	*/		     -1,    -1,    -1,   -1,
/* 68,104 	*/		     -1,    -1,    -1,   -1,
/* 69,105 end	*/		KEYPAD,      1,   '1',  117,
/* 6A,106 	*/		     -1,    -1,    -1,   -1,
/* 6B,107 lft arrow 101	*/	KEYPAD,      1,     4,  115,
/* 6C,108 home	*/		KEYPAD,      1,   '7',  119,
/* 6D,109 	*/		     -1,    -1,    -1,   -1,
/* 6E,110 	*/		     -1,    -1,    -1,   -1,
/* 6F,111 	*/		     -1,    -1,    -1,   -1,
/* 70,112 ins 101 */		KEYPAD,     -1,   '0',   -1,
/* 71,113 del 101 */		KEYPAD,     -1,    -1,   -1,
/* 72,114 dn arrow 101 */	KEYPAD,      1,   '2',  145,
/* 73,115 5 101	*/		KEYPAD,      1,   '5',  143,
/* 74,116 rt arrow */		KEYPAD,      1,   '6',  116,
/* 75,117 up arrow 101 */	KEYPAD,      1,   '8',  141,
/* 76,118 esc	*/		ASCII,    0x1b,  0x1b, 0x1b,
/* 77,119 	*/		     -1,    -1,    -1,   -1,
/* 78,120 f11	*/		FUNCTION,   11,    -1,   -1,
/* 79,121 + 101	*/		ASCII,     '+',   '+',   -1,
/* 7A,122 pg dn	*/		KEYPAD,      1,   '3',  118,
/* 7B,123 - 101	*/		ASCII,     '-',   '-',   -1,
/* 7C,124 * 101	*/		     -1,    -1,    -1,   -1,
/* 7D,125 pg up	*/		KEYPAD,      1,   '9',  132,
/* 7E,126 scrl lk */		TOGGLE, SCRLLOCK,  -1,   -1,
/* 7F,127 	*/		     -1,    -1,    -1,   -1,
/* 80,128 	*/		     -1,    -1,    -1,   -1,
/* 81,129 	*/		     -1,    -1,    -1,   -1,
/* 82,130 	*/		     -1,    -1,    -1,   -1,
/* 83,131 f7	*/		FUNCTION,    7,    -1,   -1,
};

/***************************** UNIX Keymap ***************************/

struct keydef ux_keymap[] = {
/* 01,1   	*/		     -1,   -1,     -1,   -1,
/* 02,2   	*/		     -1,   -1,     -1,   -1,
/* 03,3   	*/		     -1,   -1,     -1,   -1,
/* 04,4   	*/		     -1,   -1,     -1,   -1,
/* 05,5  	*/		     -1,   -1,     -1,   -1,
/* 06,6  	*/		     -1,   -1,     -1,   -1,
/* 07,7  f1	*/		FUNCTION,   1,     -1,   -1,
/* 08,8  esc	*/		ASCII,   0x1b,   0x1b, 0x1b,
/* 09,9  	*/		     -1,   -1,     -1,   -1,
/* 0A,10 	*/		     -1,   -1,     -1,   -1,
/* 0B,11 	*/		     -1,   -1,     -1,   -1,
/* 0C,12 	*/		     -1,   -1,     -1,   -1,
/* 0D,13 tab	*/		ASCII,      9,      0,   -1,
/* 0E,14 `	*/		ASCII,   0x60,    '~',   -1,
/* 0F,15 f2	*/		FUNCTION,   2,     -1,   -1,
/* 10,16 	*/		     -1,   -1,     -1,   -1,
/* 11,17 ctrl	*/		SHIFT,   CTRL,     -1,   -1,
/* 12,18 shift	*/		SHIFT, LSHIFT,     -1,   -1,
/* 13,19 	*/		     -1,   -1,     -1,   -1,
/* 14,20 caps lk	*/	TOGGLE, CAPSLOCK,  -1,   -1,
/* 15,21 q	*/		ALPHA,    'q',    'Q',   17,
/* 16,22 1	*/		NUMERIC,   '1',   '!',   -1,
/* 17,23 f3	*/	        FUNCTION,   3,     -1,   -1,
/* 18,24 	*/		     -1,   -1,     -1,   -1,
/* 19,25 alt	*/		SHIFT,    ALT,     -1,   -1,
/* 1A,26 z	*/		ALPHA,    'z',    'Z',   26,
/* 1B,27 s	*/		ALPHA,    's',    'S',   19,
/* 1C,28 a	*/		ALPHA,    'a',    'A',    1,
/* 1D,29 w 	*/		ALPHA,    'w',    'W',   23,
/* 1E,30 2	*/		NUMERIC,  '2',    '@',    0,
/* 1F,31 f4	*/		FUNCTION,   4,     -1,   -1,
/* 20,32 	*/		     -1,   -1,     -1,   -1,
/* 21,33 c	*/		ALPHA,    'c',    'C',    3,
/* 22,34 x	*/		ALPHA,    'x',    'X',   24,
/* 23,35 d	*/		ALPHA,    'd',    'D',    4,
/* 24,36 e	*/		ALPHA,    'e',    'E',    5,
/* 25,37 4	*/		NUMERIC,  '4',    '$',   -1,
/* 26,38 3	*/		NUMERIC,  '3',    '#',   -1,
/* 27,39 f5	*/		FUNCTION,   5,     -1,   -1,
/* 28,40 	*/		     -1,   -1,     -1,   -1,
/* 29,41 sp	*/		ALPHA,    ' ',    ' ',  ' ',	
/* 2A,42 v	*/		ALPHA,    'v',    'V',   22,
/* 2B,43 f	*/		ALPHA,    'f',    'F',    6,
/* 2C,44 t	*/		ALPHA,    't',    'T',   20,
/* 2D,45 r	*/		ALPHA,    'r',    'R',   18,
/* 2E,46 5	*/		NUMERIC,  '5',    '%',   -1,
/* 2F,47 f6	*/		FUNCTION,   6,     -1,   -1,
/* 30,48 	*/		     -1,   -1,     -1,   -1,	
/* 31,49 n	*/		ALPHA,    'n',    'N',   14,	
/* 32,50 b	*/		ALPHA,    'b',    'B',    2,	
/* 33,51 h	*/		ALPHA,    'h',    'H',    8,	
/* 34,52 g	*/		ALPHA,    'g',    'G',    7,
/* 35,53 y	*/		ALPHA,    'y',    'Y',   25,
/* 36,54 6	*/	        NUMERIC,  '6',    '^',   30,
/* 37,55 f7	*/		FUNCTION,   7,     -1,   -1,
/* 38,56  	*/		     -1,   -1,     -1,   -1,
/* 39,57 alt	*/		SHIFT,    ALT,     -1,   -1,
/* 3A,58 m	*/		ALPHA,    'm',    'M', 0x0d,
/* 3B,59 j	*/		ALPHA,    'j',    'J', 0x0a,
/* 3C,60 u	*/		ALPHA,    'u',    'U',   21,
/* 3D,61 7	*/		NUMERIC,  '7',    '&',   -1,
/* 3E,62 8	*/		NUMERIC,  '8',    '*',   -1,
/* 3F,63 f8	*/		FUNCTION,   8,     -1,   -1,
/* 40,64 	*/		     -1,   -1,     -1,   -1,
/* 41,65 ,	*/		ASCII,    ',',    '<',   -1,
/* 42,66 k	*/		ALPHA,    'k',    'K',   11,
/* 43,67 i	*/		ALPHA,    'i',    'I',    9,
/* 44,68 o	*/		ALPHA,    'o',    'O',   15,
/* 45,69 0	*/		NUMERIC,  '0',    ')',   -1,
/* 46,70 9	*/		NUMERIC,  '9',    '(',   -1,
/* 47,71 f9	*/		FUNCTION,   9,     -1,   -1,
/* 48,72 	*/		     -1,   -1,     -1,   -1,
/* 49,73 .	*/		ASCII,    '.',    '>',   -1,
/* 4A,74 /	*/		ASCII,    '/',    '?',   -1,
/* 4B,75 l	*/		ALPHA,    'l',    'L',   12,
/* 4C,76 ;	*/		ASCII,    ';',    ':',   -1,
/* 4D,77 p	*/		ALPHA,    'p',    'P',   16,
/* 4E,78 -	*/		ASCII,    '-',    '_',   31,
/* 4F,79 f10	*/		FUNCTION,  10,     -1,   -1,
/* 50,80 	*/		     -1,   -1,     -1,   -1,
/* 51,81 	*/		     -1,   -1,     -1,   -1,
/* 52,82 '	*/		ASCII,   0x27,   0x22,   -1,
/* 53,83 	*/		     -1,   -1,     -1,   -1,
/* 54,84 [	*/		ASCII,	   '[',   '{',   27,
/* 55,85 =	*/		ASCII,     '=',   '+',   -1,
/* 56,86 f11	*/		FUNCTION,   11,    -1,   -1,
/* 57,87 lf	*/		ASCII,      10,    10,   10,
/* 58,88 opt	*/		     -1,    -1,    -1,   -1,
/* 59,89 right SHIFT	*/	SHIFT,  LSHIFT,    -1,   -1,
/* 5A,90 CR	*/		ASCII,    0x0d,  0x0d,   10,
/* 5B,91 ]	*/		ASCII,     ']',   '}',   29,
/* 5C,92 \	*/		ASCII,    '\\',   '|',   28,
/* 5D,93 	*/		     -1,    -1,    -1,   -1,
/* 5E,94 f12	*/		FUNCTION,   12,    -1,   -1,
/* 5F,95 	*/		     -1,    -1,    -1,   -1,
/* 60,96 dwn arrow	*/	KEYPAD,      1,    -1,   -1,
/* 61,97 lft arrow	*/	KEYPAD,      1,    -1,   -1,
/* 62,98 	*/		     -1,    -1,    -1,   -1,
/* 63,99 up arrow	*/	KEYPAD,      1,    -1,   -1,
/* 64,100 	*/		     -1,    -1,    -1,   -1,
/* 65,101 	*/		     -1,    -1,    -1,   -1,
/* 66,102 bksp	*/		ASCII,       8,     8,  127,
/* 67,103 	*/		     -1,    -1,    -1,   -1,
/* 68,104 	*/		     -1,    -1,    -1,   -1,
/* 69,105 end	*/		KEYPAD,      1,   '1',  117,
/* 6A,106 rt arrow	*/	KEYPAD,      1,    -1,   -1,
/* 6B,107 lft arrow 101	*/	KEYPAD,      1,     4,  115,
/* 6C,108 home	*/		KEYPAD,      1,   '7',  119,
/* 6D,109 	*/		     -1,    -1,    -1,   -1,
/* 6E,110 	*/		     -1,    -1,    -1,   -1,
/* 6F,111 	*/		     -1,    -1,    -1,   -1,
/* 70,112 ins 101 */		KEYPAD,     -1,   '0',   -1,
/* 71,113 del 101 */		KEYPAD,     -1,    -1,   -1,
/* 72,114 dn arrow 101 */	KEYPAD,      1,   '2',  145,
/* 73,115 5 101	*/		KEYPAD,      1,   '5',  143,
/* 74,116 rt arrow */		KEYPAD,      1,   '6',  116,
/* 75,117 up arrow 101 */	KEYPAD,      1,   '8',  141,
/* 76,118 	*/		     -1,    -1,    -1,   -1,
/* 77,119 	*/		     -1,    -1,    -1,   -1,
/* 78,120 	*/		     -1,    -1,    -1,   -1,
/* 79,121 + 101	*/		ASCII,     '+',   '+',   -1,
/* 7A,122 pg dn	*/		KEYPAD,      1,   '3',  118,
/* 7B,123 - 101	*/		ASCII,     '-',   '-',   -1,
/* 7C,124 * 101	*/		     -1,    -1,    -1,   -1,
/* 7D,125 pg up	*/		KEYPAD,      1,   '9',  132,
/* 7E,126 scrl lk */		TOGGLE, SCRLLOCK,  -1,   -1,
/* 7F,127 	*/		     -1,    -1,    -1,   -1,
/* 80,128 	*/		     -1,    -1,    -1,   -1,
/* 81,129 	*/		     -1,    -1,    -1,   -1,
/* 82,130 	*/		     -1,    -1,    -1,   -1,
/* 83,131 	*/		     -1,    -1,    -1,   -1,
/* 84,132 	*/		     -1,    -1,    -1,   -1,
};

struct keydef *keymap;
int keymap_size;
int key_scan_set;

extern int Keyswitch;

static int last_key;
static int prev_key;

struct keydef *
lookup (key)
{
	key &= 0xFF;

	prev_key = last_key;
	last_key = key;

	if (key > keymap_size) {
                interkey (key); 
		return NULL; /* Should we just forget this key */
	}
	else
	        return (&keymap[key - 1]);
}

kybreak(c)
int c;
{
	return (prev_key == 0xF0) ? 1 : 0;
}

kymake(c)
int c;
{
	return (prev_key == 0xF0) ? 0 : 1;
}

keyboard_map(which)
int	which;
{
	key_scan_set = 0;
	switch (which) {
	    case AT_KEYBOARD:
		keymap = at_keymap;
		keymap_size = sizeof(at_keymap) / sizeof(struct keydef);
		break;

	    default:
	    case UNIX_KEYBOARD:
		Keyswitch = 0; 
		keymap = ux_keymap;
		keymap_size = sizeof(ux_keymap) / sizeof(struct keydef);
		break;
	}
}


extern int lastsetlights;

#define CAP_LIGHT       (1<<2)

ky_set_capsled( on )
int on;
{
        if (!on) {
                pkbd_set_lights(lastsetlights | CAP_LIGHT);
        } else {
                pkbd_set_lights(lastsetlights & ~CAP_LIGHT);
        }
}

