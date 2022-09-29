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
#ident	"$Header: pkbd.c,v 1.2.1.6.1.1.1.3 90/11/02 17:56:01 beacker Exp $"
/*
 * $Header: pkbd.c,v 1.2.1.6.1.1.1.3 90/11/02 17:56:01 beacker Exp $
 */
/*
 * Keyboard driver for RS3230
 * Notes:
 *   This driver is a copy of the uart driver modified for the keyboard.
 */

#define	NOINTER			/* no interrupts for now */
#define	TRYKBD			/* use the kybd code */

#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/termio.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "sys/sbd.h"
#include "sys/file.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/user.h"
#include "sys/ipc.h"
#include "sys/msg.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/edt.h"
#include "sys/debug.h"

#include "sys/iop.h"
#include "sys/buzzer.h"
#include "sys/kbd.h"
#include "sys/kbd_ioctl.h"
#include "sys/keyboard.h"
#include "sys/grafreg.h"
#include "sys/ss.h"
#include "sys/cpu_board.h"
#include "sys/rambo.h"
#include "sys/keymap.h"
#include "sys/shm.h"

extern int dbg_kbd;
extern char *keyboard_str;

extern struct stty_ld def_stty_ld;

extern time_t lbolt;
extern int mono;
extern int color;
extern int blank;

long blank_time; 
int screen_blanked;
struct shmid_ds *display_shm1, *display_shm2, *display_shm3, *display_shm4;

int pkbd_act(), pkbd_zap(), pkbd_outc(), pkbd_setline(), pkbd_modem_control();
int pkbd_driver_control();

struct ss_devdep_info pkbd_ss_devdep =
        { "KBD", SS_TTY, 1, 2, 32,
           pkbd_act, pkbd_zap, pkbd_outc, pkbd_setline, pkbd_modem_control, 
	   pkbd_driver_control
        };

static struct module_info dum_info = {
    STRID_KEYBOARD,			/* module ID */
    "keyboard",				/* module name */
    0,					/* minimum packet size */
    1024,				/* maximum packet size */
    128,				/* high water mark */
    16,					/* low water mark */
};

int pkbdopen();
int pkbdclose();
static struct qinit pkbd_rinit = {
    NULL, ss_rsrv, pkbdopen, pkbdclose, NULL, &dum_info, NULL
};

static struct qinit pkbd_winit = {
    ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab pkbdinfo = {&pkbd_rinit, &pkbd_winit, NULL, NULL};

struct ss_struct pkbd_ss_struct;
struct ss_line pkbd_ss_line[2];

extern int showconfig;
extern int kbddead;
extern caddr_t frm_addr;
extern int key_scan_set;
extern int keymap_size;

static KeyboardControl PKeyBoard;

unsigned pkbd_txto[2] = 0;		/* indicates tx timeout called */
unsigned pkbd_buzzerout = 0;

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

#define	KBCEN		0xf4	/* enable keyboard */
#define	KBCDIS		0xf5
#define	KBSDEFAULT	0xF6	/* set the defaults */

#define	KB_RD_PORT	0xd0
#define	KB_WT_PORT	0xd1

#ifdef NOINTER
#define	KBD_CONFIG	(CB_SYSTEM | CB_DISABLE | CB_INH_OVERRIDE)
#else
#define	KBD_CONFIG	(CB_SYSTEM | CB_DISABLE | CB_INH_OVERRIDE | CB_ENINTR)
#endif

#define KBD_DELAY	4
#define	KBD_DATA_DELAY	500
#define	KBD_ERROR	(-1)

#define	KYS_ERRS	(KYS_TTO | KYS_RTO | KYS_PARITY)

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
unsigned lastsetlights = 0;

int ky_in();

/*
 * Initialize keyboard driver.
 */
pkbdedtinit(edt)
    struct edt *edt;
{
	int status;
	int keyboard_type;

	PKeyBoard.kbd_state = KeyboardClosed;
	PKeyBoard.kbd_oflag[0] = KBD_CLOSE;
	PKeyBoard.kbd_oflag[1] = KBD_CLOSE;
	edt->e_base = (paddr_t)&PKeyBoard;

	kbd_ginit();

	if ( kbddead ) {
		ky_clean(ky_ptr);
		ky_cmd(ky_ptr, KYC_DISABLE);
		ky_cmd_accept(ky_ptr);

		if (showconfig) {
		    cmn_err(CE_WARN, "keyboard not communicating\n");
		}
	} else {
		PKeyBoard.kbd_state = KeyboardAlive;
		ky_clean(ky_ptr);
		ky_cmd(ky_ptr, KYC_WCB);
		ky_cmd_accept(ky_ptr);
		ky_out(ky_ptr, KBD_CONFIG);

		pkbd_set_kbd();

		for (status = 0; status < 8 ; status++) {
			pkbd_set_lights(status);

			send_to_keyboard(KBCEN);
			DELAY(50000);
		}
		
		pkbd_set_lights(0);

		send_to_keyboard(KBCEN);

		keyboard_type = UNIX_KEYBOARD;
		if ( keyboard_str && (strcmp(keyboard_str, "AT") == 0) )
			keyboard_type = AT_KEYBOARD;

		keyboard_map(keyboard_type);

		kbddead = 0;
	}

	chk_console(kbddead);

	pkbd_ss_struct.ss_nlines = 2;
	pkbd_ss_struct.ss_devdep = &pkbd_ss_devdep;
	pkbd_ss_line[0].pss = &pkbd_ss_struct;
	pkbd_ss_line[0].ss_lenable = 1;
	ss_cont_i(&pkbd_ss_line[0], IFLAGS, &def_stty_ld.st_termio);
	pkbd_ss_line[1].pss = &pkbd_ss_struct;
	pkbd_ss_line[1].ss_lenable = 1;
	ss_cont_i(&pkbd_ss_line[1], IFLAGS, &def_stty_ld.st_termio);

#ifdef NOINTER
	if ( !kbddead )
		pkbd_poll();
#endif
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

	if ( send_to_keyboard(KBRESET) == KBD_ERROR ) {
		ky_clean(ky);
		ky_cmd(ky, KYC_DISABLE);
		ky_cmd_accept(ky);

		if (showconfig) {
		    cmn_err(CE_WARN, "keyboard not communicating\n");
		}
	} else {
		DELAY(400000);
		if ((status=ky_in_lim(ky, MAX_KEY_TIME)) != 0xAA) {
		    if (showconfig) {
			cmn_err(CE_WARN,"keyboard rst status 0x%x\n", status);
		    }
		}
		ky_accept_data(ky);

		kbddead = 0;
	}

	return (kbddead ? 0 : 1);
}

pkbd_set_kbd() {

	send_to_keyboard(KBCDIS);

	send_to_keyboard(KBSCAN);
	send_to_keyboard(key_scan_set);

	send_to_keyboard(KBCEN);
}

pkbd_set_lights(which)
int which;
{
	lastsetlights = which & KTCLEDMASK;
	
	send_to_keyboard(KTCLED);
	send_to_keyboard(lastsetlights);
}

#define	X_INUSE	((display_shm1 && display_shm1->shm_reg && \
			display_shm1->shm_reg->r_refcnt > 0) || \
		 (display_shm2 && display_shm2->shm_reg && \
			display_shm2->shm_reg->r_refcnt > 0) || \
		 (display_shm3 && display_shm3->shm_reg && \
			display_shm3->shm_reg->r_refcnt > 0) || \
		 (display_shm4 && display_shm4->shm_reg && \
			display_shm4->shm_reg->r_refcnt > 0))


#ifdef NOINTER
pkbd_poll() {
	if (ky_in_ready(ky_ptr)) {
		pkbdintr();
		if (screen_blanked) {
		    if ( mono )
			mono_blank(0);
		    if ( color )
			blank3030c8(0);
		    screen_blanked = 0;
		}
		if (blank_time)
		    blank_time = 0;
	}
	else {
	    if ( !X_INUSE && !screen_blanked ) {
		if ( !blank_time ) {
		    blank_time = lbolt;
		}
		else
		if ( (lbolt - blank_time) >= (blank * HZ) ) {
		    if ( mono )
			mono_blank(1);
		    if ( color )
			blank3030c8(1);
		    screen_blanked = 1;
		}
	   }
	}

	if (kbd_wait) {
		kbd_wait = 0;
		wakeup(ky_ptr);
	}
	timeout(pkbd_poll, 0, 1);
}
#endif

pkbd_txintr(pline)
register struct ss_line *pline;
{
	register int dev = pline - pkbd_ss_line;

	pkbd_txto[dev] = 0;
	if ( PKeyBoard.kbd_state == KeyboardOpen ) {
		ss_start(pline);
	}
}

/*
 * pkbdopen()
 */
pkbdopen (rq, dev, flag, sflag)
    queue_t *rq;
    dev_t dev;
    int flag;
    int sflag;
{
    register int minordev = minor(dev);
    int s;
    queue_t *wq;
    
    if ( kbddead ) {
	u.u_error = EIO;
	return OPENFAIL;
    }
    
    /*
     * Only one keyboard
     */
    if ((minordev != 0) && (minordev != 1) )
      return OPENFAIL;
    
    if (minordev == 1) {
    }
    PKeyBoard.kbd_state = KeyboardOpen;
    PKeyBoard.kbd_oflag[minordev] = KBD_OPEN;
    return(ss_open (&pkbd_ss_line[minordev], rq, dev, flag, sflag, IFLAGS));
}

/*
 * pkbdintr()
 * Receives characters from the keyboard and places them on the
 * streams input queues.
 */
pkbdintr ()
{
	register int c;

	while (ky_in_ready(ky_ptr)) {
		c = (char)pkbd_getc();
		if ( !kbddead )
			kydeposit(&kbd_bstruct, c);
		DELAY(KBD_DELAY);
	}
	pkbd_receive(&PKeyBoard);
}

pkbd_receive(k)
register KeyboardControl *k;
{
	register int c, gflag;
	char ch;
	extern time_t lbolt;

	if (k->kbd_state == KeyboardAlive) 
		return;

	if (k->kbd_state != KeyboardOpen) {
		/* clean out the buffer */
		while (kbd_nqueued() != 0)
			kbd_qread(&ch, 1);
		return;
	}

	while (1) {
		gflag = k->kbd_oflag[0];	/* graf device open status */
		if ( gflag == KBD_CLOSE ) {
			if ( (c = g_getchar(0)) == 0 ) {
				break;
			} else {
				c &= 0xff;
			}
		} else {
			if ( kbd_nqueued() ) {
				kbd_qread (&ch, 1);
				c = ch;
			} else {
				break;
			}
		}
		/*
		 * Start or stop output (if permitted)
		 */
		if ( (gflag == KBD_CLOSE)  && 
				!ss_startstop(&pkbd_ss_line[1], (c & 0xFF)))
			continue;
		pkbd_ss_inc(&pkbd_ss_line[gflag ? 0 : 1], gflag, c);
	}
}

ky_in_ready(ky)
volatile struct kyboard *ky;
{
	register int status;

	DELAY(KBD_DELAY);
	return ((status = (int)ky->ky_cmd_b) & KYS_OBF) ? status : 0;
}

/* make sure that the keyboard does not have any chars lying about */
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

/*
 * pkbd_avail return 1 if at least one character is available.
 * The keyboard ring buffer doesn't have to be high speed.
 */
pkbd_avail ()
{
        return (ky_in_ready(ky_ptr));
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

	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);

	stat = ky->ky_cmd_b;

	c = ky->ky_data_b;

	DELAY(KBD_DELAY);
	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);

	if (dbg_kbd)
		cmn_err (CE_CONT, "0x%x 0x%x, ", c, stat);
#if 0	/* XXX */
	if (c > keymap_size && c != 0xF0)
		pkbd_set_kbd();
#endif
	return c;
}


pkbd_act(pline)
register struct ss_line *pline;
{
    return 1;
}

pkbd_zap(pline)
register struct ss_line *pline;
{
    return 0;
}

pkbdclose(rq)
queue_t *rq;
{
	register struct ss_line *pline = (struct ss_line *)rq->q_ptr;
	register int minordev = (pline - &pkbd_ss_line[0]);
    
	if ( !pline ) {
		return;
	}
	ss_close(rq);
	if(minordev == 0) {
		teReset(1);
	}
	PKeyBoard.kbd_oflag[minordev] = KBD_CLOSE;
	if ( (PKeyBoard.kbd_oflag[0] == KBD_CLOSE) && 
			(PKeyBoard.kbd_oflag[1] == KBD_CLOSE) ) {
		PKeyBoard.kbd_state = KeyboardClosed;
	}
}

pkbd_outc(pline, cs, len)
register struct ss_line *pline;
    register char *cs;
    register int   len;
{
	register int dev = pline - pkbd_ss_line;
	register svsr;

	while(len--) {
	    grafputc(*cs++,0);
	    pline->ss_wbp->b_rptr++;
	}
	svsr = splclock();
	if ( !pkbd_txto[dev] ) {
		pkbd_txto[dev]++;
		timeout(pkbd_txintr,pline,4);
	}
	splx(svsr);
}

pkbd_setline(pline, cflag)
register struct ss_line *pline;
    int cflag;
{
    return 0;
}

pkbd_modem_control(pline)
register struct ss_line *pline;
{
    return 0;
}


pkbd_driver_control(pline,sscmd,value)
register struct ss_line *pline;
int sscmd, value;
{
    register struct iocblk *ioss;
    register int minordev = (pline - &pkbd_ss_line[0]);
    register int status;
	
    switch ( sscmd ) {

    case SS_DC_M_IOCTL:
	ioss = (struct iocblk*)((mblk_t *)value)->b_rptr;
	switch ( ioss->ioc_cmd ) {
	case KTCRINGBELL:
		status = pkbd_ringbell(ioss,value);
#ifdef ATOM_BUZZ
		sleep(&pkbd_buzzerout, PUSER | PCATCH);
#endif
		return status;

	case KTCPRGMBUZZER:
		status = pkbd_prgmbuzzer(ioss,value);
#ifdef ATOM_BUZZ
		sleep(&pkbd_buzzerout, PUSER | PCATCH);
#endif
		return status;

	case KTCSETLIGHTS:
		return(pkbd_setlights(ioss,value));

	case KTCGETLIGHTS:
		return(pkbd_getlights(ioss,value));

	case KTCSETTIMESTAMP:
    		PKeyBoard.kbd_oflag[minordev] = KBD_TIMESTAMP;
		ioss->ioc_error = 0;
		ioss->ioc_count = 0;
		((mblk_t *)value)->b_datap->db_type = M_IOCACK;
		return(1);

	case KTCCLRTIMESTAMP:
    		PKeyBoard.kbd_oflag[minordev] = KBD_OPEN;
		ioss->ioc_error = 0;
		ioss->ioc_count = 0;
		((mblk_t *)value)->b_datap->db_type = M_IOCACK;
		return(1);

	case KTCWRTCOLOR:
		return(pkbd_colormap(ioss,value));

	case KTMBLANK:
		if ((status=mono_blank(*(int *)((mblk_t *)value)->b_cont->b_rptr))) {
			ioss->ioc_error = 0;
			((mblk_t *)value)->b_datap->db_type = M_IOCACK;
		} else {
			ioss->ioc_error = EINVAL;
			((mblk_t *)value)->b_datap->db_type = M_IOCNAK;
		}
		ioss->ioc_count = 0;
		return(status);

	default:
		break;
	}
	break;

    default:
	break;

    }
    return 0;
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
	register int	counter = 255;

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

ky_in(ky)
volatile struct kyboard *ky;
{
	DELAY(KBD_DELAY);
	while ( !(ky->ky_cmd_b & KYS_OBF) )
		DELAY(KBD_DELAY);
	DELAY(KBD_DELAY);
	return (int)ky->ky_data_b;
}

ky_in_kbd(ky)
volatile struct kyboard *ky;
{
	int	key;

	DELAY(KBD_DELAY);
	while ( !(ky->ky_cmd_b & KYS_OBF) )
		DELAY(KBD_DELAY);

	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);
	key = (int)ky->ky_data_b;

	DELAY(KBD_DATA_DELAY);
	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);

	return key;
}

ky_accept_data(ky)
volatile struct kyboard *ky;
{
	int	key;

	DELAY(KBD_DELAY);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);

	DELAY(KBD_DATA_DELAY);

	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);
}

ky_in_lim(ky, time_lim)
volatile struct kyboard *ky;
int time_lim;
{
	int status;

	DELAY(KBD_DELAY);
	while ( !((status = ky->ky_cmd_b) & (KYS_OBF | KYS_ERRS)) && (time_lim-- > 0))
		DELAY(KBD_DELAY);

	if (status & KYS_ERRS)
		return KBD_ERROR;

	DELAY(KBD_DELAY);
	return (time_lim > 0) ? (int)ky->ky_data_b : KBD_ERROR;
}

pkbd_stopbuzz() {
	register volatile struct rambo *rambo = ((struct rambo *)PHYS_TO_K1(RAMBO_BASE));

	rambo->rambo_creg = rambo->rambo_creg | BUZZON;
	pkbd_buzzerout = 0;
#ifdef ATOM_BUZZ
	wakeup(&pkbd_buzzerout);
#endif
}

pkbd_buzzer(bztime,bzload)
register short bztime, bzload;
{
	register struct buzzer *bz;
	register volatile struct rambo *rambo = ((struct rambo *)PHYS_TO_K1(RAMBO_BASE));
	int r_creg, bzval;

	if ( pkbd_buzzerout ) {
		return(0);
	}

	r_creg = rambo->rambo_creg & ~BUZZMASK;

	bzval = HZ_TO_LOAD(bzload);

	/* buzzer values are 1524, 762 381, 190 */
	if ( bzval > (1524 + 762)/2) {
		r_creg |= BUZZ0;
	} else if ( bzval > (762 + 381)/2) {
		r_creg |= BUZZ1;
	} else if ( bzval > (381 + 190)/2) {
		r_creg |= BUZZ2;
	} else {
		r_creg |= BUZZ3;
	}

	rambo->rambo_creg = r_creg & ~BUZZON;
	pkbd_buzzerout++;

	timeout(pkbd_stopbuzz, 0, bztime);

	return(1);
}

#define KBDBELLTIME  15		/* 10ms * 15 = .15 sec */

pkbd_ringbell(ioss, bp)
register struct iocblk *ioss;
register mblk_t *bp;
{

	if ( !pkbd_buzzer(KBDBELLTIME,TCT2_LOAD) ) {
		ioss->ioc_error = EBUSY;
		bp->b_datap->db_type = M_IOCNAK;
	} else {
		ioss->ioc_error = 0;
		bp->b_datap->db_type = M_IOCACK;
	}
	ioss->ioc_count = 0;
	return(1);
}

pkbd_prgmbuzzer(ioss, bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	register struct buzzer *buz = (struct buzzer *)bp->b_cont->b_rptr;

	if (! (ioss->ioc_count == sizeof(struct buzzer))) {
		ioss->ioc_error = EINVAL;
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
		return(1);
	};
	if ( !pkbd_buzzer(buz->buzzer_time,buz->buzzer_load) ) {
		ioss->ioc_error = EBUSY;
		bp->b_datap->db_type = M_IOCNAK;
	} else {
		ioss->ioc_error = 0;
		bp->b_datap->db_type = M_IOCACK;
	}
	bp->b_datap->db_type = M_IOCACK;
	return(1);
}

pkbd_setlights(ioss, bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	register int s;
	int stat;

	s = spltty();

	lastsetlights = (*(int *)(bp->b_cont->b_rptr)) & KTCLEDMASK;
	pkbd_set_lights(lastsetlights);

	bp->b_datap->db_type = M_IOCACK;
	ioss->ioc_error = 0;
	ioss->ioc_count = 0;

	splx(s);
	return(1);
}

pkbd_getlights(ioss, bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	if (! (ioss->ioc_count == sizeof(int))) {
		ioss->ioc_error = EINVAL;
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
		return(1);
	};
	*(int *)(bp->b_cont->b_rptr) = lastsetlights;
	ioss->ioc_error = 0;
	ioss->ioc_count = sizeof(int);
	bp->b_datap->db_type = M_IOCACK;
	return(1);
}

	
kbd_ginit() {
	register struct kbd_buffer *kp = &kbd_bstruct;

	kp->ibuf = kbd_buf;
	kp->in = kp->out = 0;
	kp->bufsz = KBDBUFSZ;
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
	int ack = 0;
	char c;
	int status;
	register volatile struct kyboard *ky = ky_ptr;

	do {
		DELAY(KBD_DELAY);
		while (ky->ky_cmd_b & KYS_IBF) {
			DELAY(KBD_DELAY);
			kbd_wait++;
		}
		DELAY(KBD_DELAY);
		while (ky->ky_cmd_b & KYS_OBF) {
			DELAY(KBD_DELAY);
			c =  ky->ky_data_b;
			if ( ! kbddead )
				kydeposit(&kbd_bstruct, c);
			DELAY(KBD_DELAY);
		}
		ky_out(ky, command);
		status = waitforack();
		
		if (status == KBD_ERROR || status == 0xff) {
			if (dbg_kbd)
				cmn_err(CE_WARN,"send ret 0x%x\n", status);
			if (ack >= ACK_ATTACK)
				return KBD_ERROR;
		} else {
			ky_accept_data(ky);
		}
	} while (status && ack++ < ACK_ATTACK);

	return 0;
}

waitforack ()
{
	register int c;
	register volatile struct kbd_buffer *kbdp = &kbd_bstruct;

	while (1) {
		c = ky_in_lim(ky_ptr, MAX_KEY_TIME);
		if (c == KBD_ERROR || c == 0xff) {
			return KBD_ERROR;
		} else {
			if (c == KBACK)
				break;
			else
				return (interkey (c));
		}
	}
	return (0);
}

kydeposit(kbdp, ch)
register struct kbd_buffer *kbdp;
unsigned char ch;
{
	kbdp->ibuf[kbdp->in++] = ch;
	if (kbdp->in == kbdp->bufsz)
		kbdp->in = 0;
}
	
struct colorm pkbd_cm;
struct colorm pkbd_cm_sv;
unsigned int irq5to = 0;
unsigned int donotirq5 = 0;

pkbd_irq5on()
{
	irq5on();
}
pkbd_irq5missing()
{
	donotirq5++;
	irq5to = 0;
	cmn_err(CE_WARN,"No IRQ5 Interrupt Available");
	write_color();
}

pkbd_colormap(ioss, bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	register int *intrclr = (int *)PHYS_TO_K1(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET);
	register char *cp;
	register int i;
	register int svsr;
	register error = 0;

	pkbd_cm = *(struct colorm *)bp->b_cont->b_rptr;
	if ( pkbd_cm.cmstart >= GRAPHICS_MAX_COLOR_REG ) {
		error++;
	} else if ( pkbd_cm.cmcount > (GRAPHICS_MAX_COLOR_REG-pkbd_cm.cmstart) ) {
		error++;
	}
	if ( error ) {
		ioss->ioc_error = EINVAL;
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
		pkbd_cm.cmcount = 0;
		return(1);
	}
	svsr = irq5off();
	splhi();		/* all other off too */
	for ( cp = &pkbd_cm_sv.cmap[pkbd_cm.cmstart*3], i = 0; 
					i < (pkbd_cm.cmcount*3); i++ ) {
		*cp++ = pkbd_cm.cmap[i];
	}
	if ( !irq5to ) {
		pkbd_cm_sv.cmstart = pkbd_cm.cmstart;
		pkbd_cm_sv.cmcount = pkbd_cm.cmcount;
	} else {
		if ( pkbd_cm.cmstart < pkbd_cm_sv.cmstart ) {
			pkbd_cm_sv.cmcount += (pkbd_cm_sv.cmstart-pkbd_cm.cmstart);
			pkbd_cm_sv.cmstart = pkbd_cm.cmstart;
		} else if ( (pkbd_cm_sv.cmstart+pkbd_cm_sv.cmcount) <
			(pkbd_cm.cmstart+pkbd_cm.cmcount) ) {
			pkbd_cm_sv.cmcount = 
			   (pkbd_cm.cmstart+pkbd_cm.cmcount) - pkbd_cm_sv.cmstart;
		}
	}
	if ( donotirq5 ) {
		write_color();
		irq5x(svsr);
	} else {
		if ( !irq5to ) {
			irq5to++;
			timeout(pkbd_irq5on,0,4);
			timeout(pkbd_irq5missing,0,HZ*5);
			irq5x(svsr);
			*intrclr = 0;
			irq5on();
		} else {
			irq5x(svsr);
		}
	}
	ioss->ioc_error = 0;
	ioss->ioc_count = 0;
	bp->b_datap->db_type = M_IOCACK;
	return(1);
	
}

write_color()
{
	register unsigned char *raddr = 
			(unsigned char *)PHYS_TO_K1(GRAPHICS_COLOR_REG);
	register unsigned char *daddr = 
			(unsigned char *)PHYS_TO_K1(GRAPHICS_COLOR_DATA);
	register cmstart = pkbd_cm_sv.cmstart;
	register cmcount = pkbd_cm_sv.cmcount;
	register cmindx = cmstart*3;

	while ( cmcount-- ) {
		*raddr = cmstart++; wbflush();
		*daddr = pkbd_cm_sv.cmap[cmindx++]; wbflush();
		*daddr = pkbd_cm_sv.cmap[cmindx++]; wbflush();
		*daddr = pkbd_cm_sv.cmap[cmindx++]; wbflush();
	}

}

pkbd_ss_inc(pline,gflag,c)
register struct ss_line *pline;
register int c, gflag;
{
    	register time_t tstamp;

	if ( gflag != KBD_TIMESTAMP ) {
		ss_inc(pline, c);
	} else {
		ss_inc(pline, KBDSYNCCHAR);
		ss_inc(pline, KBDSYNCCHAR);
		ss_inc(pline, KBDSYNCCHAR);
		ss_inc(pline, c);
		tstamp = lbolt;
		ss_inc(pline, ((tstamp>>24)&0xff));
		ss_inc(pline, ((tstamp>>16)&0xff));
		ss_inc(pline, ((tstamp>>8)&0xff));
		ss_inc(pline, (tstamp&0xff));
	}
}

pkbdset4()
{
	/*
	 * set up for 4MB simms in I8042
	 */

	register volatile struct kyboard *ky = ky_ptr;
	unsigned long	t1;
	long	count = 0;

	ky_clean(ky);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);
	ky_clean(ky);
	ky_cmd(ky, KYC_STEST);
	ky_cmd_accept(ky);
	if ((ky_in(ky) & 0xff) != 0x55) {
		/*
		 * we failed self test, maybe there was data banked up
		 */
		for (count = 0; count < 1000; count++)
			if (ky_in_ready(ky))
				break;
		if (!ky_in_ready(ky) || ((ky_in(ky) & 0xff) != 0x55))
			return 0;
	}
	for(t1 = 0; ((t1 & 4) == 0) && (count < 10); count++) {
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		ky_cmd_accept(ky);
		t1 = ky_in(ky);
		ky_cmd(ky, KB_WT_PORT);
		ky_cmd_accept(ky);
		ky_out(ky, t1 | 0x05);
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		t1 = ky_in(ky);
		if ((t1 & 4) == 0) {
			cmn_err(CE_WARN, "Cannot set keyboard controller to 4Mb, retrying...\n");
		}
	}
	return ((t1 & 4) != 0);
}

pkbdset1()
{
	/*
	 * set up for 1MB simms in I8042
	 */

	register volatile struct kyboard *ky = ky_ptr;
	unsigned long	t1;
	long	count = 0;

	ky_clean(ky);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);
	ky_clean(ky);

	for(t1 = 4; ((t1 & 4) == 4) && (count < 10); count++) {
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		ky_cmd_accept(ky);
		t1 = ky_in(ky);
		ky_cmd(ky, KB_WT_PORT);
		ky_out(ky, (t1 & ~0x04) | 1);
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		t1 = ky_in(ky);
		if ((t1 & 4) == 4) {
			cmn_err(CE_WARN, "Cannot set keyboard controller to 1Mb, retrying...\n");
		}
	}
	return ((t1 & 4) != 4);
}

#define	SLOT_INT_CLR	(1<<5)

pkbd_int_clr() {
	register volatile struct kyboard *ky = ky_ptr;
	unsigned long	t1;
	long	count = 0;

	ky_clean(ky);
	ky_cmd(ky, KYC_DISABLE);
	ky_cmd_accept(ky);
	ky_clean(ky);

	for(t1 = 0; ((t1 & SLOT_INT_CLR) == 0) && (count < 10); count++) {
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		ky_cmd_accept(ky);
		t1 = ky_in(ky);
		ky_cmd(ky, KB_WT_PORT);
		ky_out(ky, (t1 | SLOT_INT_CLR) | 1);
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		t1 = ky_in(ky);
		if ((t1 & SLOT_INT_CLR) == 0) {
			cmn_err(CE_WARN, "Cannot set slot int retrying...\n");
		}
	}
	for(t1 = SLOT_INT_CLR; ((t1 & SLOT_INT_CLR) == SLOT_INT_CLR) && (count < 10); count++) {
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		ky_cmd_accept(ky);
		t1 = ky_in(ky);
		ky_cmd(ky, KB_WT_PORT);
		ky_out(ky, (t1 & ~SLOT_INT_CLR) | 1);
		ky_clean(ky);
		ky_cmd(ky, KB_RD_PORT);
		t1 = ky_in(ky);
		if ((t1 & SLOT_INT_CLR) == SLOT_INT_CLR) {
			cmn_err(CE_WARN, "Cannot clear slot int retrying...\n");
		}
	}

	ky_clean(ky);
	ky_cmd(ky, KYC_ENABLE);
	ky_cmd_accept(ky);
	ky_clean(ky);

	return ((t1 & SLOT_INT_CLR) != SLOT_INT_CLR);
}

irq5on() {
	return ail5();
}

irq5off() {
	return iil5();
}

irq5x() {
	return;
}

kbd_buzzer() {
}

pkbd_report()
{
	register volatile struct kyboard *ky = ky_ptr;
	int	stat;

	ky_clean(ky);
	ky_cmd(ky, KYC_RCB);

	ky_cmd_accept(ky);
	if ((stat = ky_in_lim(ky, MAX_KEY_TIME)) == KBD_ERROR) {
		printf("keyboard not responding\n");
	} else {
		printf("keyboard command byte 0x%x\n", stat);
	}
}
