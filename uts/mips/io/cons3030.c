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
#ident	"$Header: cons3030.c,v 1.1.1.10.1.1.1.4 91/01/18 15:07:10 beacker Exp $"


#include "sys/types.h"
#include "sys/stream.h"
#include "sys/cmn_err.h"
#include "sys/errno.h"
#include "sys/cpu_board.h"
#include "sys/bitmap.h"
#include "sys/teState.h"
#include "sys/ipc.h"		/* needed for stream.h and shm.h */
#include "sys/msg.h"		/* needed for stream.h */
#include "sys/stream.h"		/* defines queue_t in grafreg.h */
#include "sys/grafreg.h"
#include "sys/sbd.h"
#include "sys/rambo.h"

/*
 * on the r3030 we have a choice of places where the console can be,
 * it can be on either of the serial ports, on the colour, or the mono screen,
 * or any combination....
 *
 * The decision is made in console_init by looking at the 'console' prom
 * environment variable. We decide based upon the first character in the
 * console string...
 *
 *	0:	tty0
 *	1:	tty1
 *	r:	tty0 (tty1 also in proms)
 *	m:	if keyboard then mono else tty1
 *	c:	if colour & keyboard then colour else tty1
 *	a:	tty0, tty1, mono, colour
 *	v:	if keyboard then (mono ALSO if colour then colour) else tty1
 *	g,l:	if colour & keyboard then colour else if keyboard then mono else tty1
 *	<anything else> 'l'
 */

#define	DEFAULT	do_1		/* default console for failures */

static	char	console_port = 'a';
static	queue_t	*console_stream = 0;
static	int	console_unit;
extern	char	*console;
extern	int	mono;
extern	int	color;
extern	Bitmap	monoscreen;
extern	TeState	teState;
extern	int	mono_blank();
int		do_mono;
int		do_colour;
int		do_0;
int		do_1;
int		nrows = 24;
int		ncols = 80;
unsigned long	*prom_rambo_buffer;

console_init()
{
	register is_keybd;
	register is_colour;
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

	stopclocks();
	scc_init();
	is_keybd = pkbd_check();
	is_colour = color_check();
	if (*console == 'g' || *console == 'l') {
		console_port = 'l';
	} else
		console_port = *console;
	do_mono = do_colour = do_0 = do_1 = 0;
	switch(console_port) {
	case 'm':
		if (is_keybd)
			do_mono = 1;
		else
			DEFAULT = 1;
		break;
	case 'c':
		if (is_colour && is_keybd)
			do_colour = 1;
		else
			DEFAULT = 1;
		break;
	case 'a':
		do_1 = 1;
		do_0 = 1;
	case 'v':
		if (is_keybd) {
			do_mono = 1;
			if (is_colour)
				do_colour = 1;
		} else
			DEFAULT = 1;
		break;
	case 'r':		/* only 0 for console messages */
		do_0 = 1;
		if (is_keybd) {
			do_mono = 1;
			if (is_colour)
				do_colour = 1;
		}
		break;
	default:
		console_port = 'l';	/* 'l' */
	case 'l':
		if (is_keybd) {
			if (is_colour)
				do_colour = 1;
			else
				do_mono = 1;
		} else
			DEFAULT = 1;
		break;
	case '0':
		do_0 = 1;
		break;
	case '1':
		do_1 = 1;
		break;
	}
	if (do_mono || do_colour) {
		register r, c, x, y;

		prom_rambo_buffer = (unsigned long *)(*(unsigned long *)0x80000510);
		if (prom_rambo_buffer == 0) {
			prom_rambo_buffer = (unsigned long *)(0x800000 - (132 * 1024));
		}
		if (do_mono) {
			init_r3030_mono(PHYS_TO_K1(PROM_RAMBO_BUFFER), 0);
		}
		if (do_colour) {
			init_r3030_c8(0);
		}
		r = (*(int *)0x80000500);
		c = (*(int *)0x80000504);
		y = (*(int *)0x80000508);
		x = (*(int *)0x8000050c);
		if (	r > 0 && c > 0 &&
			r < 37 && c < 81 &&
			x > 0 && x < c &&
			y > 0 && y < c
		) {
			nrows = r;
			ncols = c;
			teState.row = y;
			teState.col = x;
			teReset(0);		/* just initialise - don't clear */
		} else {
			/*
			 * we could not detect any form of communication
			 * with the proms.
			 * - default
			 */
			teState.row = 1;
			teState.col = 1;
			teReset(1);
		}
	}
}

console_exit()
{
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

	if (do_mono && monoscreen.base) {
		(*(volatile int *)0xa0000500) = nrows;
		(*(volatile int *)0xa0000504) = ncols;
		(*(volatile int *)0xa0000508) = teState.row;
		(*(volatile int *)0xa000050c) = teState.col;
		bcopy(monoscreen.base, PHYS_TO_K1(PROM_RAMBO_BUFFER), MONO_FRAME_SIZE);
		mono_reset_rambo_addr((u_long)PROM_RAMBO_BUFFER);
	}
	if (do_colour) {
		(*(volatile int *)0xa0000500) = nrows;
		(*(volatile int *)0xa0000504) = ncols;
		(*(volatile int *)0xa0000508) = teState.row;
		(*(volatile int *)0xa000050c) = teState.col;
	}
}

/*
 * Make sure that console is not blanked
 */
clean_console()
{
	if (color)
		blank3030c8(0);
	if (mono)
		mono_blank(0);
}

video_console_init()
{
	register volatile struct rambo_chan *ram_ch_p = 
		&(((struct rambo *)PHYS_TO_K1(RAMBO_BASE))->rambo_ch[1]);

	if (do_mono) {
		mono = 1;	/* ensure that the mono screen is enabled */
	} else {
		ram_ch_p->dma_mode = FLUSH_RAMBO_FIFO | CLR_DMA_ERR; /* disable */
	}
	if (do_colour) {
		color = 1;
	}
}

chk_console(kdead)
int kdead;
{
	if (do_mono || do_colour) {
		if (kdead) {
			/*
			 * the keyboard cannot be found (or is dead)
			 * Tell the user that we are reverting to
			 * tty1 so that the user can connect a
			 * new keyboard and reboot, or plug in a console.
			 */
			cmn_err(CE_WARN, "Cannot Find Keyboard - console on tty1\n");
			console_port = '1';
			do_mono = 0;
			do_colour = 0;
			DEFAULT = 1;
		} else if (console_port != 'a') {
			/*
			 * only out to the screen now.
			 */
			DEFAULT = 0;
		}
	}
}

console_getc()
{
	return grafgetc();
}

console_putc(c)
{
	grafputc(c, 1);
}

grafputc(c, f)
char	c;
int	f;			/* CR/LF mode or not kludge */
{
	int s;
	extern	int	panic_level;	/* are we panic'ing ? */

	if (!panic_level && console_stream) {	/* output to stream */
		mblk_t	*bp;

		if ((bp = allocb(1, BPRI_HI)) == (mblk_t *)0){
			return;			/* drop character */
		}
		*bp->b_wptr++ = c;

		/* buffer up messages and schedule them to be sent
		   downstream later on.  This lets drivers running
		   at spl higher than tty call cmn_err without
		   breaking streams spl locking */
		strcatx(bp);
		return;
	}

	if (do_0)
		sccputc(0, c);

	if (do_1)
		sccputc(1, c);

	if (do_mono || do_colour) {
		tePutChar(c);
		if (f && (c == '\n'))
			tePutChar('\r');
	}
}

mblk_t *cons_head = 0;
mblk_t *cons_tail = 0;
int cmn_sched = 0;
int cmn_lock = 0;

cmn_wakeup()
{
	int s;
	mblk_t *bp;
	s = splhi();
	cmn_sched = 0;
	bp = cons_head;
	cons_head = 0;
	if (console_stream && !panic_level){
		if (bp)
			pts_wput(console_stream,bp);
	}else
		freemsg(bp);

	splx(s);
}


/* concatenate messages and schedule a timeout to
   send the messages downstream  */

strcatx(bp)
mblk_t	*bp;
{
	int s;

	s = splhi();
	if(cons_head) {
		cons_tail->b_cont = bp;
		cons_tail = bp;
	} else {
		cons_head = cons_tail = bp;
	}
	bp->b_cont = 0;

	if(cmn_sched == 0) {
		timeout(cmn_wakeup,0,0);
		cmn_sched = 1;
	}
	splx(s);
}

grafgetc()
{
	register char c;

	if (!panic_level && console_stream) {
		cmn_err(CE_WARN, "grafgetc called with stream active\n");
		return 0;
	}
	switch(console_port) {
	default:
		while ((c = g_getchar(0)) == 0)
			;			/* endless loop here */
		break;
	case '0':
	case 'r':
		c = sccgetc( 0 );
		break;
	case '1':
		c = sccgetc( 1 );
		break;
	}
	return c & 0x7f;
}
	
/*
 * Make sure that kernel output is redirected to frame buffer
 * instead of non-existant stream.
 */
graphics_shutdown()
{
	extern int cpu_config;

	if (cpu_config & P_COLOUR) {
		initcmap();
		g_pinit(0);
		console_stream = 0;
	}
	if (color)
		blank3030c8(0);
	if (mono)
		mono_blank(0);
}


/*
 * When minor 0 of the pseudo tty is opened this becomes the output channel
 * for kernel printfs.
 */
graphics_set_queue(unit, wq)
register int unit;
register queue_t *wq;
{
	if (console_stream != 0)
		return(EBUSY);
	console_stream = wq;
	console_unit = unit;
	return 0;
}

/*
 * If the control side of the console is closed the kernel should resort
 * to printing directly on the frame buffer.
 */
graphics_remove_queue(unit)
register int unit;
{
	if (unit != console_unit)
		return (EINVAL);
	console_stream = 0;
	return 0;
}

is_mono()
{
	return do_mono;
}

mono_base()
{
	return PHYS_TO_K1(prom_rambo_buffer);
}
