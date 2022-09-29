#ident "$Header: pkbd.c,v 1.2.6.2 90/12/20 11:02:45 chungc Exp $"
/*	%Q%	%I%	%M%	*/
/* $Copyright: |
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
#include "machine/cpu.h"
#include "machine/kbd.h"
#include "machine/mk48t02.h"
#include "machine/cpu_board.h"

#define	NOINTER		1

union ky_w_b {
	unsigned long	ky_word;
	struct ky_by {
		unsigned char	pad[3];
		unsigned char	ky_byte;
	} ky_by;
};

struct kyboard {
	union ky_w_b	ky_d;
	union ky_w_b	ky_c;
};

#define	ky_cmd_b	ky_c.ky_by.ky_byte
#define	ky_cmd_w	ky_c.ky_word
#define	ky_data_b	ky_d.ky_by.ky_byte
#define	ky_data_w	ky_d.ky_word

#define	STEST_OK	0x55
#define	ITEST_OK	0x00

#define	MAX_KEY_TIME	90000		/* maximum time for a key response */

#define	KBNULL	0x00
#define	KBACK	0xfa
#define	KBOVFL	0xff		/* keyboard overflow */
#define	KBECHO	0xee		/* echo */
#define	KBSTOK	0xaa		/* self test ok */
#define	KBSTNOK	0xfc		/* self test fail */
#define	KBRSEND	0xfe		/* resend */

#define	KBRESET		0xff	/* reset the keyboard */
#define	KBSCAN		0xf0
#define	SCAN_TYPE	0x00

#define	KBCEN		0xf4	/* enable keyboard */
#define	KBCDIS		0xf5
#define	KBSDEFAULT	0xF6	/* set the defaults */

#ifdef NOINTER
#define	KBD_CONFIG	(CB_SYSTEM | CB_INH_OVERRIDE)
#else
#define	KBD_CONFIG	(CB_SYSTEM | CB_INH_OVERRIDE | CB_ENINTR)
#endif

#define KBD_DELAY	8
#define	KBD_DATA_DELAY	1000
#define	KBD_ERROR	(-1)

/* things from the keyboard */
#define 	ACK_ATTACK 6

typedef unsigned char QTYPE;

struct kbd_buffer {
	unsigned char	*ibuf;
	int	in;
	int	out;
	int	bufsz;
};

#define	NQUEUED(x) (((x)->in==(x)->out)? 0 :		\
			    (((x)->in<(x)->out)? 		\
			     (x)->bufsz-(x)->out+(x)->in :	\
			     (x)->in-(x)->out))

#define	QEMPTY(q) ((q)->in == (q)->out)

struct kbd_buffer	kbd_bstruct;
unsigned char	kbd_buf[KBDBUFSZ];

volatile struct kyboard *ky_ptr = (struct kyboard *)PHYS_TO_K1(KEYBOARD_BASE);
int kbd_wait;
unsigned lastsetlights;

static int kbd_dbg = 0;

extern int kbddead;
extern int color_cons, mono_cons;

int ky_in();

/*
 * Initialize keyboard driver.
 */
pkbd_init()
{
	char *cp;
        extern char *getenv();
	int status, count, kbtype;
	register volatile struct kyboard *ky = ky_ptr;

        cp = getenv("keyboard");
	kbtype = (cp && !strcmp(cp,"AT")) ? 1 : 0;
	keyboard_map(kbtype);

	lastsetlights = 0;
	kbddead = 1;

#if 0
	/* configure the keyboard */
	ky_clean(ky);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);

	ky_clean(ky);
	ky_cmd(ky, KYC_OUTPUT);
	ky_cmd_accept(ky);
	status = ky_in(ky);

	ky_clean(ky);
	ky_cmd(ky, KYC_WOUT);
	ky_cmd_accept(ky);
	ky_out(ky, status | 0xc1);

	DELAY(100000);

	ky_clean(ky);
	ky_cmd(ky, KYC_WOUT);
	ky_cmd_accept(ky);
	ky_out(ky, (status & ~0xc0) | 1);
#endif
	ky_clean(ky);
	ky_cmd(ky, KYC_WCB);
	ky_cmd_accept(ky);
	ky_out(ky, KBD_CONFIG);

	ky_clean(ky);
	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);

	DELAY(600000);
#if 0
	printf("pkbd before init 0x%x\n", ky->ky_cmd_b);
#endif
	status = send_to_keyboard(KBRESET);
	if ((status == KBD_ERROR) || (status == KBRSEND)) {
#if 0
		printf("keyboard interface test failure 0x%x\n", status);
#endif
		ky_clean(ky);
		ky_cmd(ky, KYC_DISABLE);
		ky_cmd_accept(ky);
	} else {
#if 0
		printf("pkbd status after init 0x%x\n", ky->ky_cmd_b);
		DELAY(400000);
#endif

		status = ky_in_lim(ky, MAX_KEY_TIME);
#if 0
		printf("kbd rst status 0x%x\n", status);
#endif

		ky_accept_data(ky);

		ky_clean(ky);
		ky_cmd(ky, KYC_WCB);
		ky_cmd_accept(ky);
		ky_out(ky, KBD_CONFIG);

		g_ginit();

		send_to_keyboard(0xed);
		send_to_keyboard(1);

		send_to_keyboard(KBCEN);

		kbddead = 0;
	}
}


pkbd_check() {
        int status;
        register volatile struct kyboard *ky = ky_ptr;

        kbddead = 1;
        kbd_ginit();

        /* configure the keyboard */
        ky_clean(ky);
        ky_cmd(ky, KYC_DISABLE);
        ky_cmd_accept(ky);

        ky_clean(ky);
        ky_cmd(ky, KYC_WCB);
        ky_cmd_accept(ky);
        ky_out(ky, KBD_CONFIG);

        ky_clean(ky);
        ky_cmd(ky, KYC_ENABLE);
        ky_cmd_accept(ky);
        DELAY(600000);

        ky_clean(ky);
        ky_out(ky,KBRESET);
	if (status = waitforack()) goto done;
        if ((status=ky_in_lim(ky, MAX_KEY_TIME)) != 0xAA) goto done;
        kbddead = 0;
	return(1);
done: 
        ky_clean(ky);
        ky_cmd(ky, KYC_DISABLE);
        ky_cmd_accept(ky);
        return(0);
}


pkbd_set_lights(which)
int which;
{
        lastsetlights = which & KTCLEDMASK;

        send_to_keyboard(KTCLED);
        send_to_keyboard(lastsetlights);
}


static unsigned long new_time;
unsigned long blank_time;
int screen_blanked;

/*
 * pkbdintr()
 * Receives characters from the keyboard and places them on the
 * streams input queues.
 */
pkbdintr ()
{
	register int c;
	register volatile struct todc_clock *tod = (struct todc_clock *)PHYS_TO_K1(TODC_CLOCK_ADDR_R3030);

	while (pkbd_avail()) {
		if (screen_blanked) {
			if (mono_cons) mono_blank(0);
			if (color_cons) blank3030c8(0);
			screen_blanked = 0;
		}
		blank_time = 0;
		c = (char)pkbd_getc();
		kydeposit(&kbd_bstruct, c);
		DELAY(KBD_DELAY);
	}
	if (!screen_blanked) {
		new_time = bcd_to_dec((tod->todc_mins & 0x7f));
		if (!blank_time) blank_time = (new_time+30)%60;
		if (new_time == blank_time) {
			if (mono_cons) mono_blank(1);
			if (color_cons) blank3030c8(1);
			screen_blanked = 1;
		}
	}
}

/*
 * pkbd_avail return 1 if at least one character is available.
 * The keyboard ring buffer doesn't have to be high speed.
 */
pkbd_avail ()
{
	return (ky_ptr->ky_cmd_b & KYS_OBF) ? (ky_ptr->ky_cmd_b) : 0;
}

/*
 * Get next character of off input ring buffer.  Does not check to
 * see if characters are available.  kcirc_avail should be called first.
 */
pkbd_getc ()
{
	register int c;
	register int stat;
	register volatile struct kyboard *ky = ky_ptr;

	DELAY(KBD_DELAY);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);

	DELAY(KBD_DELAY);
	stat = ky->ky_cmd_b;

	DELAY(KBD_DELAY);
	c = ky->ky_data_b;

	DELAY(KBD_DELAY);
	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);

	if (kbd_dbg)
		printf("0x%x:0x%x,", c, stat);

	return c;
}

ky_cmd(ky, x)
volatile struct kyboard *ky;
int	x;
{
	DELAY(KBD_DELAY);
	while (ky->ky_cmd_b & KYS_IBF)
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	ky->ky_cmd_w = (x);
}

ky_cmd_accept(ky)
volatile struct kyboard *ky;
{
	register int counter = 255;

	DELAY(KBD_DELAY);
	while (ky->ky_cmd_b & KYS_IBF && counter-- > 0)
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	return (counter > 0 ? 1 : 0);
}

ky_out(ky, x)
volatile struct kyboard *ky;
int	x;
{
	DELAY(KBD_DELAY);
	while (ky->ky_cmd_b & KYS_IBF)
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	ky->ky_data_w = (x);
}

ky_clean(ky)
volatile struct kyboard *ky;
{
	register int	count;
	register int	sum;

	while (ky_in_ready(ky)) {
		sum += ky_in(ky);
		count++;
	}
	return count;
}

ky_in(ky)
volatile struct kyboard *ky;
{
	DELAY(KBD_DELAY);
	while ( !(ky->ky_cmd_b & KYS_OBF) )
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	return (int)ky->ky_data_b;
}

ky_in_ready(ky)
volatile struct kyboard *ky;
{
	register int status;

	DELAY(KBD_DELAY);
	return ((status = (int)ky->ky_cmd_b) & KYS_OBF) ? status : 0;
}

ky_in_lim(ky, time_lim)
volatile struct kyboard *ky;
int time_lim;
{
	DELAY(KBD_DELAY);
	while ( !(ky->ky_cmd_b & KYS_OBF) && (time_lim-- > 0))
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	return (time_lim > 0) ? (int)ky->ky_data_b : KBD_ERROR;
}

ky_accept_data(ky)
volatile struct kyboard *ky;
{
	DELAY(KBD_DELAY);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);

	DELAY(KBD_DATA_DELAY);

	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);
}

kbd_ginit() {
	register struct kbd_buffer *ky = &kbd_bstruct;

	ky->ibuf = kbd_buf;
	ky->in = ky->out = 0;
	ky->bufsz = KBDBUFSZ;
}

kbd_cmd(cmd)
int cmd;
{
	ky_cmd(ky_ptr, cmd);
}

kbd_nqueued()
{
	register struct kbd_buffer *qstruct = &kbd_bstruct;

	return (NQUEUED(qstruct));
}

kbd_qread (qdata, qsize)
QTYPE *qdata;
int qsize;
{
	struct kbd_buffer *qstruct = &kbd_bstruct;
	extern int stdio_init;
	QTYPE qq;
	int qs = qsize;

	if (qs < 0 || qdata == NULL)
		return (-1);
	while (!QEMPTY (qstruct)) {
		*qdata++ = qstruct->ibuf[qstruct->out++];
		if (qstruct->out == qstruct->bufsz)
			qstruct->out = 0;
		if (--qs == 0)
			break;
	}
	return (qsize - qs);
}

send_to_keyboard (command)
	int command;
{
	register int ack;
	register int status;
	register volatile struct kyboard *ky = ky_ptr;

	ack = 0;

	do {
		DELAY(KBD_DELAY);
		while (ky->ky_cmd_b & KYS_IBF) {
			DELAY(KBD_DELAY);
			kbd_wait++;
		}
		DELAY(KBD_DELAY);
		while (ky->ky_cmd_b & KYS_OBF) {
			DELAY(KBD_DELAY);
			status = ky->ky_data_b;
			if ( ! kbddead ) 
			    	kydeposit(&kbd_bstruct, status);
			DELAY(KBD_DELAY);
		}
		ky_out(ky, command);
		status = waitforack();

		if (status == KBD_ERROR || status==0xff) {
			if (kbd_dbg)
				printf("send ret 0x%x\n", status);
		} else {
			ky_accept_data(ky);
		}
	} while (status && ack++ < ACK_ATTACK);

	return status;
}

kydeposit(kbdp, ch)
register struct kbd_buffer *kbdp;
unsigned char ch;
{
	kbdp->ibuf[kbdp->in++] = ch;
	if (kbdp->in == kbdp->bufsz)
		kbdp->in = 0;
}
	
waitforack ()
{
	register int c;
	register volatile struct kyboard *ky = ky_ptr;

	while (1) {
		if ((c = ky_in_lim(ky, MAX_KEY_TIME)) == KBD_ERROR)
			return KBD_ERROR;
		if (c == KBACK)
			break;
		else
			return (interkey (c));
	}
	return (0);
}
