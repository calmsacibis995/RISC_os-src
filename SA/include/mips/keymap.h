/* --------------------------------------------------- */
/* | Copyright (c) 1986 MIPS Computer Systems, Inc.  | */
/* | All Rights Reserved.                            | */
/* --------------------------------------------------- */
/* $Header: keymap.h,v 1.0 90/04/03 13:04:56 beacker Exp $ */

struct keydef {
    unsigned char type;
    unsigned char base;
    unsigned char shift;
    unsigned char ctrl;
};

/* key type */

#define	ASCII	0
#define	NUMERIC	1
#define	ALPHA	2
#define	SHIFT	3
#define	TOGGLE	4
#define	FUNCTION 5
#define	KEYPAD	6

/* base values */

#define	LSHIFT		0xaa
#define	CTRL		0x81
#define	ALT		0x82
#define	RSHIFT		0xb6
#define	CAPSLOCK	0x86
#define	NUMLOCK  	0x87
#define	SCRLLOCK 	0x88
#define	E0_E1		0x56

/* modifier bits */

#define	_LALT		0x01
#define	_RALT		0x02
#define	_RSHIFT		0x04
#define	_LSHIFT		0x08
#define	_CAPSLOCK	0x10
#define	_RCTRL		0x20
#define	_LCTRL		0x40

#define	AT_BREAK	0xf0
#define	AT_EXT		0xE0

/* LED lights */
# define	C_LOCK_ON	0x7
# define	C_LOCK_OFF	0x0

# define	ALTED(m)	(((m)&_RALT)||((m)&_LALT))
# define	SHIFTED(m)	(((m)&_RSHIFT)||((m)&_LSHIFT))
# define	CTRLED(m)	(((m)&_RCTRL)||((m)&_LCTRL))

# define	BREAK(c) ((c)&0x80)
# define	MAKE(c)	(!BREAK(c))
