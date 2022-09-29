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
#ident	"$Header: ggetchar.c,v 1.4.2.6 90/05/30 23:49:53 wje Exp $"
/*
 * $Header: ggetchar.c,v 1.4.2.6 90/05/30 23:49:53 wje Exp $
 */
#include "sys/param.h"
#ifdef KERNEL
# include "sys/types.h"
# include "sys/sbd.h"
# include "sys/kbd.h"
# include "sys/keymap.h"
#else
# include "saio/saio.h"
# include "machine/cpu.h"
# include "machine/kbd.h"
#endif /* KERNEL */

int	Keyswitch = 1;		/* keyswtch nvram variable */
short mod;			/* state of modifiers */

extern struct keydef *keymap;
extern int keymap_size;
extern int key_scan_set;

 /* keyboard is dead */
int kbddead = 1;

/* keyboard commands */

#define	KBCLED	0xed
#define	KBCECHO	0xee
#define	KBCSCAN	0xf0
#define	KBCRDID	0xf2
#define	KBCSRPT	0xf3
#define	KBCEN	0xf4
#define	KBCDIS	0xf5
#define	KBCRST	0xff

/* things from the keyboard */

#define	KBNULL	0x00
#define	KBACK	0xfa
#define	KBOVFL	0xff		/* keyboard overflow */
#define	KBECHO	0xee		/* echo */
#define	KBSTOK	0xaa		/* self test ok */
#define	KBSTNOK	0xfc		/* self test fail */
#define	KBRSEND	0xfe		/* resend */

#define 	ACK_ATTACK 6

g_ginit()
{
	char c;
	extern char *keyswtch;
	int ack;

	/* initialize the keyboard stuff */
	kbd_ginit();

	/* disable the keyboard */
	kbd_cmd(KBCDIS);

	/* clean out the buffer */
	while (kbd_nqueued () != 0)
		kbd_qread (&c, 1);

	/*
	 *initialize the keyboard
	 */
	send_to_keyboard (KBCRST);

	/*
	 * 1. disable the keyboard
	 */
	send_to_keyboard (KBCDIS);

	/*
	 * 2. set keyboard to scan set 1
	 */
	send_to_keyboard (KBCSCAN);
	send_to_keyboard (key_scan_set);

	/*
	 * 3. enable keyboard
	 */
	send_to_keyboard (KBCEN);

	/*
	 * 4. eat any characters left over
	 */
	while (kbd_nqueued() > 0)
		 kbd_qread (&c, 1);
    
	kbddead = 0;

	/*
	 * 6. get the state of the keyswitch variable 
	 */
	Keyswitch = atoi( keyswtch );
	if ( Keyswitch != 1 && Keyswitch != 0 )
		Keyswitch = 1;

	return 0;
}


/* interpret key value */

interkey (c)
    int c;
{
    switch (c) {
      case KBSTNOK:		/* self test fail */
	kbddead = 1;
	return (-1);
      case KBRSEND:		/* resend */
	return (1);
      case KBNULL:		/* keyboard overflow */
      case KBOVFL:		/* keyboard overflow */
      case KBACK:		/* acknowledge */
      case KBECHO:		/* echo */
      case KBSTOK:		/* self test ok */
      default:
	return (0);
    }
}

/*
 * g_getchar return 0 if no characters are available or the character or'd
 * with 0x100.
 */
g_getchar (mode)
    int mode;
{
	unsigned char c, c1;
	struct keydef *kp, *lookup ();
	short prefix;
	

	if (kbddead || (kbd_nqueued() < 2))
		return 0;

	while (1) {
		if (kbd_qread (&c, 1) == 0)
			return 0;

		prefix = 0;

		/* E0 */
		if (c == 0xe0) {
			while (kbd_qread (&c1, 1) != 1)
				;		/* scandevs */
			switch (c1 & 0x7f) {
			    case 0x1c:	/* right enter,alt,ctrl  */
			    case 0x38:	/* handled by code below */
			    case 0x1d:
				prefix++;
				break;
			    default:		/* handled by code below */
				break;
	    		}
		}
		else
			/* E1 */
			if (c == 0xe1) {
				/* read and throw away 1d 45 - 9d c5 */
				while (kbd_qread (&c1, 1) != 1)
					;		/* scandevs */
				while (kbd_qread (&c1, 1) != 1)
					;		/* scandevs */
				c = E0_E1;
			}

		/* map keycode */

		if ((kp = lookup (c)) != NULL)
		switch (kp->base) {
		    case CAPSLOCK:	/* CAPSLOCK is a toggle */
			/* CAPSLOCK is really caps lock */
			if ( Keyswitch == 0 ) {
				if ((mod & _CAPSLOCK) && kymake(c)) {
					mod &= ~_CAPSLOCK;
					/* turn on cap lock LED here! */
					ky_set_capsled( 1 );
				}
				else if ((mod & _CAPSLOCK) == 0 && kymake(c)) {
					mod |= _CAPSLOCK;
					/* turn off cap lock LED here! */
					ky_set_capsled( 0 );
				}
			}
			/* CAPSLOCK is really control */
			else {
				if (kybreak(c))
					mod &= (prefix) ? ~_RCTRL : ~_LCTRL;
				else
					mod |= (prefix) ? _RCTRL : _LCTRL;
			}
			/* throw away the break */
			break;
		    case LSHIFT:	/* LEFT SHIFT */
			if (kybreak(c))
				mod &= ~_LSHIFT;
			else
				mod |= _LSHIFT;
			break;
		    case RSHIFT:	/* RIGHT SHIFT */
			if (kybreak(c))
				mod &= ~_RSHIFT;
			else
				mod |= _RSHIFT;
			break;
		    case CTRL_KEY:	/* CTRL_KEY */
			/* CTRL_KEY is really control */
			if ( Keyswitch == 0 ) {
				if (kybreak(c))
					mod &= (prefix) ? ~_RCTRL : ~_LCTRL;
				else
					mod |= (prefix) ? _RCTRL : _LCTRL;
			}
			/* if CTRL_KEY is really caps lock */
			else {
				if ((mod & _CAPSLOCK) && kymake(c)) {
					mod &= ~_CAPSLOCK;
					/* turn on cap lock LED here! */
					ky_set_capsled( 1 );
				}
				else if ((mod & _CAPSLOCK) == 0 && kymake(c)) {
					mod |= _CAPSLOCK;
					/* turn off cap lock LED here! */
					ky_set_capsled( 0 );
				}
			}
			break;
		    case ALT:	/* ALT	 */
			if (kybreak(c))
				mod &= (prefix) ? ~_RALT : ~_LALT;
			else
				mod |= (prefix) ? _RALT : _LALT;
			break;
		    default:		/* another key */
			if (kymake(c)) {
				if (SHIFTED (mod))
					return (kp->shift | 0x100);
				if (mod & _CAPSLOCK && kp->type == ALPHA)
					return (kp->shift | 0x100);
				if (CTRLED (mod))
					return (kp->ctrl | 0x100);
				return (kp->base | 0x100);
			}		/* throw away break codes */
			else if (mode)
				return (0 | 0x100);
			break;
		}
	}
}

#ifndef KERNEL
g_getc (argc, argv)
    int argc;
    char **argv;
{
    int count, c;
    
    if (argc != 2)
      count = 5;
    else
      atob (*(++argv), &count);

    printf ("g_getc: looking for %d chars: ", count);
    while (count--) {
        while ((c = g_getchar (0)) == 0)
	  _scandevs();
	putchar (c & 0xff);
    }
    printf ("\n");

    return 0;
}
#endif /* !KERNEL */
