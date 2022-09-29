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
/* $Header: keyboard.h,v 1.6.2.2.1.1.1.2 90/10/05 09:56:12 beacker Exp $ */
/*
 * $Header: keyboard.h,v 1.6.2.2.1.1.1.2 90/10/05 09:56:12 beacker Exp $
 */
/*
 * Project: Jupiter
 * Date: 20-June-1988
 * Keyboard driver definitions
 */

typedef struct {
    struct _kbdiopb *kbd_iocb;	/* K1 space pointer to keyboard structure */
    enum { KeyboardOpen, KeyboardClosed, KeyboardDead, KeyboardAlive } kbd_state;
    int kbd_flag;		/* control flags */
    int kbd_oflag[2];		/* open flags for each minor device */
    queue_t *kbd_rqueue;	/* Input queue */
    queue_t *kbd_wqueue;	/* Output queue */
    mblk_t *kbd_wbp;		/* Current write buffer */
    mblk_t *kbd_rbp;		/* Current read buffer */
    mblk_t *kbd_rhead;		/* Head of read chain */
    mblk_t *kbd_rtail;		/* Tail of read chain */
    struct termio kbd_termio;	/* Termio struct for keyboard */
} KeyboardControl;

/*
 * Defines for kbd_flag
 */
#define KBD_STOPPED	(1<<0)	/* output to keyboard has been stopped */
#define KBD_XMITTING	(1<<1)	/* driver has issued a xmit cmd and is
				   waiting for an interrupt */

/*
 * Defines for kbd_oflag
 */
#define KBD_CLOSE	0	/* indicates device is closed */
#define KBD_OPEN	1	/* indicates device is open */
#define KBD_TIMESTAMP	2	/* indicates timestamp on input */

/*
 * General defines
 */
#define KBD_MSG_SIZE	16	/* This should be enough for input.
				   Should do some measurement of the max
				   input at one time.  The keyboard can
				   dump up to 4 characters for one 
				   keystroke */
