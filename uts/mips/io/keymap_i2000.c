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
#ident	"$Header: keymap_i2000.c,v 1.2.1.5 90/05/30 23:50:39 wje Exp $"

#include "sys/param.h"
#include "sys/keymap.h"

/***************************** xt Keymap ***************************/

/*				type	  base	  shift  ctrl		*/

struct keydef xt_keymap[] = {
/* 01,1  esc 	*/		ASCII,	  0x1b,   0x1b,  0x1b,	
/* 02,2  1 	*/		NUMERIC,  '1',    '!',   -1,	
/* 03,3  2 	*/		NUMERIC,  '2',    '@',    0,	
/* 04,4  3 	*/		NUMERIC,  '3',    '#',   -1,	
/* 05,5  4 	*/		NUMERIC,  '4',    '$',   -1,	
/* 06,6  5	*/		NUMERIC,  '5',    '%',   -1,	
/* 07,7  6	*/		NUMERIC,  '6',    '^',   30,	
/* 08,8  7	*/		NUMERIC,  '7',    '&',   -1,	
/* 09,9  8	*/		NUMERIC,  '8',    '*',   -1,	
/* 0A,10 9	*/		NUMERIC,  '9',    '(',   -1,	
/* 0B,11 0	*/		NUMERIC,  '0',    ')',   -1,	
/* 0C,12 -	*/		NUMERIC,  '-',    '_',   31,	
/* 0D,13 =	*/		NUMERIC,  '=',    '+',   -1,	
/* 0E,14 bksp	*/		ASCII,      8,      8,  127,	
/* 0F,15 tab 	*/		ASCII,      9,      0,   -1,	
/* 10,16 q	*/		ALPHA,    'q',    'Q',   17,	
/* 11,17 w	*/		ALPHA,    'w',    'W',   23,	
/* 12,18 e	*/		ALPHA,    'e',    'E',    5,	
/* 13,19 r	*/		ALPHA,    'r',    'R',   18,	
/* 14,20 t	*/		ALPHA,    't',    'T',   20,	
/* 15,21 y	*/		ALPHA,    'y',    'Y',   25,	
/* 16,22 u	*/		ALPHA,    'u',    'U',   21,	
/* 17,23 i	*/		ALPHA,    'i',    'I',    9,	
/* 18,24 o	*/		ALPHA,    'o',    'O',   15,	
/* 19,25 p	*/		ALPHA,    'p',    'P',   16,	
/* 1A,26 [	*/		ASCII,    '[',    '{',   27,	
/* 1B,27 ]	*/		ASCII,    ']',    '}',   29,	
/* 1C,28 CR	*/		ASCII,   0x0d,   0x0d,   10,	
/* 1D,29 ctrl 	*/		SHIFT, CTRL_KEY,   -1,   -1,	
/* 1E,30 a	*/		ALPHA,    'a',    'A',    1,	
/* 1F,31 s	*/		ALPHA,    's',    'S',   19,	
/* 20,32 d	*/		ALPHA,    'd',    'D',    4,	
/* 21,33 f	*/		ALPHA,    'f',    'F',    6,	
/* 22,34 g	*/		ALPHA,    'g',    'G',    7,	
/* 23,35 h	*/		ALPHA,    'h',    'H',    8,	
/* 24,36 j	*/		ALPHA,    'j',    'J', 0x0a,	
/* 25,37 k	*/		ALPHA,    'k',    'K',   11,	
/* 26,38 l	*/		ALPHA,    'l',    'L',   12,	
/* 27,39 COMMA	*/		ASCII,    ';',    ':',   -1,	
/* 28,40 '	*/		ASCII,   0x27,   0x22,   -1,	
/* 29,41 `	*/		ASCII,   0x60,    '~',   -1,	
/* 2A,42 left SHIFT*/		SHIFT,LSHIFT,     -1,   -1,	
/* 2B,43 \	*/		ASCII,    '\\',    '|',   28,	
/* 2C,44 z	*/		ALPHA,    'z',    'Z',   26,	
/* 2D,45 x	*/		ALPHA,    'x',    'X',   24,	
/* 2E,46 c	*/		ALPHA,    'c',    'C',    3,	
/* 2F,47 v	*/		ALPHA,    'v',    'V',   22,	
/* 30,48 b	*/		ALPHA,    'b',    'B',    2,	
/* 31,49 n	*/		ALPHA,    'n',    'N',   14,	
/* 32,50 m	*/		ALPHA,    'm',    'M', 0x0d,	
/* 33,51	*/		ASCII,    ',',    '<',   -1,	
/* 34,52 .	*/		ASCII,    '.',    '>',   -1,
/* 35,53 / 101	*/		ASCII,    '/',    '?',   -1,
/* 36,54 right SHIFT*/	        SHIFT, RSHIFT,     -1,   -1,	
/* 37,55 prnt scrn*/		TOGGLE,     0,     -1,   -1,	
/* 38,56 alt 	*/		SHIFT,   ALT,     -1,   -1,	
/* 39,57 sp	*/		ALPHA,    ' ',    ' ',  ' ',	
/* 3A,58 caps lk*/		TOGGLE, CAPSLOCK,   -1,   -1,	
/* 3B,59 f1	*/		FUNCTION,  -1,     -1,   -1,	
/* 3C,60 f2	*/		FUNCTION,  -1,     -1,   -1,	
/* 3D,61 f3	*/		FUNCTION,  -1,     -1,   -1,	
/* 3E,62 f4	*/		FUNCTION,  -1,     -1,   -1,	
/* 3F,63 f5	*/		FUNCTION,  -1,     -1,   -1,	
/* 40,64 f6	*/		FUNCTION,  -1,     -1,   -1,	
/* 41,65 f7	*/		FUNCTION,  -1,     -1,   -1,	
/* 42,66 f8	*/		FUNCTION,  -1,     -1,   -1,	
/* 43,67 f9	*/		FUNCTION,  -1,     -1,   -1,	
/* 44,68 f10	*/		FUNCTION,  -1,     -1,   -1,	
/* 45,69 num lk*/		TOGGLE,  NUMLOCK, -1,   -1,	
/* 46,70 scrl lk*/		TOGGLE, SCRLLOCK, -1,   -1,	
/* 47,71 home	*/		KEYPAD,     1,    '7',  119,	
/* 48,72 up arrow 101	*/	KEYPAD,     1,    '8',  141,	
/* 49,73 pg up     */		KEYPAD,     1,    '9',  132,	
/* 4A,74 - 101     */		ASCII,    '-',    '-',   -1,	
/* 4B,75 lft arrow */		KEYPAD,     1,    '4',  115,	
/* 4C,76 5 101     */		KEYPAD,     1,    '5',  143,	
/* 4D,77 rt arrow  */		KEYPAD,     1,    '6',  116,	
/* 4E,78 + 101     */		ASCII,    '+',    '+',   -1,	
/* 4F,79 end	*/		KEYPAD,     1,    '1',  117,	
/* 50,80 dn arrow 101	*/	KEYPAD,     1,    '2',  145,	
/* 51,81 pg dn   */		KEYPAD,     1,    '3',  118,	
/* 52,82 ins 101 */		KEYPAD,    -1,    '0',   -1,
/* 53,83 del 101 */		KEYPAD,    -1,    -1,    -1,	
/* 54,84 SYS KEY */		KEYPAD,	   -1,    -1,    -1,	
/* 55,85 E0 	*/		E0_E1,     -1,    -1,    -1,	
/* 56,86 E1 	*/		E0_E1,     -1,    -1,    -1,	
/* 57,87 f11	*/		FUNCTION,  -1,    -1,    -1,	
/* 58,88 f12	*/		FUNCTION,  -1,    -1,    -1,	
};

struct keydef *keymap = xt_keymap;
int keymap_size = sizeof(xt_keymap) / sizeof(struct keydef);
int key_scan_set = 1;

struct keydef *
lookup (key)
{
	key &= 0x7F;
	
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
	return (c & 0x80) ? 1 : 0;
}

kymake(c)
int c;
{
	return (c & 0x80) ? 0 : 1;
}

ky_set_capsled( on )
int on;
{
}
