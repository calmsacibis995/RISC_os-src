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
#ident	"$Header: kbd.c,v 1.10.1.5 90/05/30 23:50:30 wje Exp $"
/*
 * Keyboard driver for IOP
 * Project: Jupiter Workstation
 * Date: 20-June-1988
 * Notes:
 *   This driver is a copy of the uart driver modified for the keyboard.
 */

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

extern int dbg_kbd;

extern struct stty_ld def_stty_ld;

int kbd_act(), kbd_zap(), kbd_outc(), kbd_setline(), kbd_modem_control();
int kbd_driver_control();

struct ss_devdep_info kbd_ss_devdep =
        { "KBD", SS_TTY, 1, 2, 32,
           kbd_act, kbd_zap, kbd_outc, kbd_setline, kbd_modem_control, 
	   kbd_driver_control
        };

static struct module_info dum_info = {
    STRID_IOP_KBD,			/* module ID */
    "keyboard",				/* module name */
    0,					/* minimum packet size */
    1024,				/* maximum packet size */
    128,				/* high water mark */
    16,					/* low water mark */
};

int kbdopen();
int kbdclose();
static struct qinit kbd_rinit = {
    NULL, ss_rsrv, kbdopen, kbdclose, NULL, &dum_info, NULL
};

static struct qinit kbd_winit = {
    ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab kbdinfo = {&kbd_rinit, &kbd_winit, NULL, NULL};

struct ss_struct kbd_ss_struct;
struct ss_line kbd_ss_line[2];

static KeyboardControl KeyBoard;

unsigned kbd_txto[2] = 0;		/* indicates tx timeout called */
unsigned kbd_buzzerout = 0;
/*
 * Initialize keyboard driver.
 */
kbdedtinit(edt)
    struct edt *edt;
{
    struct _kbdiopb *k;
    int status;
    extern int showconfig;
    
    if ((k = (struct _kbdiopb *)iop_alloc (KYBDIOCB,
					   sizeof (struct _kbdiopb))) == NULL)
      cmn_err (CE_PANIC, "kbdedtinit: iop_alloc failed");

    KeyBoard.kbd_iocb = k;
    KeyBoard.kbd_state = KeyboardClosed;
    KeyBoard.kbd_oflag[0] = KBD_CLOSE;
    KeyBoard.kbd_oflag[1] = KBD_CLOSE;
    edt->e_base = (paddr_t)&KeyBoard;
    kbd_ss_struct.ss_nlines = 2;
    kbd_ss_struct.ss_devdep = &kbd_ss_devdep;
    kbd_ss_line[0].pss = &kbd_ss_struct;
    kbd_ss_line[0].ss_lenable = 1;
    ss_cont_i(&kbd_ss_line[0], IFLAGS, &def_stty_ld.st_termio);
    kbd_ss_line[1].pss = &kbd_ss_struct;
    kbd_ss_line[1].ss_lenable = 1;
    ss_cont_i(&kbd_ss_line[1], IFLAGS, &def_stty_ld.st_termio);
}

kbd_txintr(pline)
register struct ss_line *pline;
{
	register int dev = pline - kbd_ss_line;

	kbd_txto[dev] = 0;
	if ( KeyBoard.kbd_state == KeyboardOpen ) {
		ss_start(pline);
	}
}

/*
 * kbdopen()
 */
kbdopen (rq, dev, flag, sflag)
    queue_t *rq;
    dev_t dev;
    int flag;
    int sflag;
{
    register int minordev = minor(dev);
    int s;
    queue_t *wq;
    
    /*
     * Only one keyboard
     */
    if ((minordev != 0) && (minordev != 1) )
      return OPENFAIL;
    
    if (KeyBoard.kbd_state == KeyboardDead) {
	u.u_error = EIO;
	return OPENFAIL;
    }
    
    if (minordev == 1) {
	teReset(0);
    }
    KeyBoard.kbd_state = KeyboardOpen;
    KeyBoard.kbd_oflag[minordev] = KBD_OPEN;
    return(ss_open (&kbd_ss_line[minordev], rq, dev, flag, sflag, IFLAGS));
}


/*
 * kbdintr()
 * Interrupt dispatcher.
 */
kbdintr (k)
    register KeyboardControl *k;
{
    int stat ;

    if (dbg_kbd)
      cmn_err (CE_CONT, "[");
    iop_clear (KYBDIOCB);
    
    if (k == 0)
      return;
	
    /*
     * Check to see if characters are available.  
     */
    kbd_receive (k);
    if (dbg_kbd)
      cmn_err (CE_CONT, "]\n");
}

/*
 * kbd_receive()
 * Receives characters from the IOP and places them on the
 * streams input queues.
 */
kbd_receive (k)
    register KeyboardControl *k;
{
    register struct _kbdiopb *Kiocb;
    register int c, gflag;
    extern time_t lbolt;
    if (k->kbd_state != KeyboardOpen)
      return;

    Kiocb = k->kbd_iocb;
    if (Kiocb == NULL)
      return;

    if (((int)Kiocb & K1BASE) != K1BASE)
      printf ("kbd_receive: iocb is not K1!!!!\n");

    while (1) {
	gflag = KeyBoard.kbd_oflag[0];	/* graphics device open status */
	if ( gflag == KBD_CLOSE ) {
		if ( (c = g_getchar(0)) == 0 ) {
			break;
		} else {
			c &= 0xff;
		}
	} else {
		if ( kcirc_avail(Kiocb) ) {
			c = kcirc_getc(Kiocb);
		} else {
			break;
		}
	}
	/*
	 * Start or stop output (if permitted)
	 */
	if ( (gflag == KBD_CLOSE)  && 
		!ss_startstop(&kbd_ss_line[1], (c & 0xFF)))
		continue;
	kbd_ss_inc(&kbd_ss_line[gflag ? 0 : 1], gflag, c);
    }

}
/*
 * kcirc_avail return 1 if at least one character is available.
 * The keyboard ring buffer doesn't have to be high speed.
 */
kcirc_avail (K)
    register struct _kbdiopb *K;
{
    return K->in != K->out ? 1 : 0;
}

/*
 * Get next character of off input ring buffer.  Does not check to
 * see if characters are available.  kcirc_avail should be called first.
 */
kcirc_getc (K)
    register struct _kbdiopb *K;
{
    register int c;

    c = K->ibuf[K->out++];
    if (K->out >= KBDBUFSZ)
      K->out = 0;

    if (dbg_kbd)
      cmn_err (CE_CONT, "0x%x ", c & 0xff);
    return c;
}


kbd_act(pline)
register struct ss_line *pline;
{
    return 1;
}

kbd_zap(pline)
register struct ss_line *pline;
{
    return 0;
}

kbdclose(rq)
queue_t *rq;
{
    register struct ss_line *pline = (struct ss_line *)rq->q_ptr;
    register int minordev = (pline - &kbd_ss_line[0]);
    
    if ( !pline ) {
	return;
    }
    ss_close(rq);
    if(minordev == 0) {
	teReset(1);
    }
    KeyBoard.kbd_oflag[minordev] = KBD_CLOSE;
    if ( (KeyBoard.kbd_oflag[0] == KBD_CLOSE) && 
		(KeyBoard.kbd_oflag[1] == KBD_CLOSE) ) {
	KeyBoard.kbd_state = KeyboardClosed;
    }
}

kbd_outc(pline, cs, len)
register struct ss_line *pline;
    register char *cs;
    register int   len;
{
	register int dev = pline - kbd_ss_line;
	register svsr;

	while(len--) {
	    grafputc(*cs++,0);
	    pline->ss_wbp->b_rptr++;
	}
	svsr = splclock();
	if ( !kbd_txto[dev] ) {
		kbd_txto[dev]++;
		timeout(kbd_txintr,pline,4);
	}
	splx(svsr);
}

kbd_setline(pline, cflag)
register struct ss_line *pline;
    int cflag;
{
    return 0;
}

kbd_modem_control(pline)
register struct ss_line *pline;
{
    return 0;
}


kbd_driver_control(pline,sscmd,value)
register struct ss_line *pline;
int sscmd, value;
{
    register struct iocblk *ioss;
    register int minordev = (pline - &kbd_ss_line[0]);
	
    switch ( sscmd ) {

    case SS_DC_M_IOCTL:
	ioss = (struct iocblk*)((mblk_t *)value)->b_rptr;
	switch ( ioss->ioc_cmd ) {
	case KTCRINGBELL:
		return(kbd_ringbell(ioss,value));

	case KTCPRGMBUZZER:
		return(kbd_prgmbuzzer(ioss,value));

	case KTCSETLIGHTS:
		return(kbd_setlights(ioss,value));

	case KTCGETLIGHTS:
		return(kbd_getlights(ioss,value));

	case KTCSETTIMESTAMP:
    		KeyBoard.kbd_oflag[minordev] = KBD_TIMESTAMP;
		ioss->ioc_error = 0;
		ioss->ioc_count = 0;
		((mblk_t *)value)->b_datap->db_type = M_IOCACK;
		return(1);

	case KTCCLRTIMESTAMP:
    		KeyBoard.kbd_oflag[minordev] = KBD_OPEN;
		ioss->ioc_error = 0;
		ioss->ioc_count = 0;
		((mblk_t *)value)->b_datap->db_type = M_IOCACK;
		return(1);

	case KTCWRTCOLOR:
		return(kbd_colormap(ioss,value));

	default:
		break;
	}
	break;

    default:
	break;

    }
    return 0;
}


kbd_buzzer(bztime,bzload)
register short bztime, bzload;
{
	register struct buzzer *bz;
	int stat;

	if ( kbd_buzzerout ) {
		return(0);
	}
	if ((bz = (struct buzzer *)iop_alloc(BUZZERIOCB, 
					sizeof(struct buzzer))) == 0) {
		cmn_err (CE_PANIC, "kbd_kbd_buzzer: iop_alloc failed\n");
	}
	while (iop_cmdsem (BUZZERIOCB) == SEMAPHORE_SET)
	  ;
	bz->buzzer_time = bztime;
	bz->buzzer_load = bzload;
	bz->buzzer_flags = BUZZER_IE;
	kbd_buzzerout++;
	if (iop_poke (BUZZERIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(bz),
		  			NO_SLEEP_ADDR) == -1)
	  cmn_err (CE_PANIC, "kbd_buzzer");
	
	return(1);
}

#define KBDBELLTIME  15		/* 10ms * 15 = .15 sec */

kbd_ringbell(ioss,bp)
register struct iocblk *ioss;
register mblk_t *bp;
{

	if ( !kbd_buzzer(KBDBELLTIME,TCT2_LOAD) ) {
		ioss->ioc_error = EBUSY;
		bp->b_datap->db_type = M_IOCNAK;
	} else {
		ioss->ioc_error = 0;
		bp->b_datap->db_type = M_IOCACK;
	}
	ioss->ioc_count = 0;
	return(1);
}

kbd_prgmbuzzer(ioss,bp)
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
	if ( !kbd_buzzer(buz->buzzer_time,buz->buzzer_load) ) {
		ioss->ioc_error = EBUSY;
		bp->b_datap->db_type = M_IOCNAK;
	} else {
		ioss->ioc_error = 0;
		bp->b_datap->db_type = M_IOCACK;
	}
	ioss->ioc_count = 0;
	return(1);
}

unsigned lastsetlights = 0;

kbd_setlights(ioss,bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	register struct _kbdiopb *Kiocb;
	register int s;
	int stat;

	/*
	 *  For newer proms, there is a function KBDWRITELEDS, that provides for
	 *  atomic writes to the leds.  We try that first.  If it fails, we
	 *  do it with two KBDWRITE's instead, all proms support that.
	 *  (The extra code can be deleted when all old proms are gone.)
	 */
	s = spltty();
	if ((Kiocb = (struct _kbdiopb *)iop_alloc (KYBDIOCB,
					   sizeof (struct _kbdiopb))) == NULL) {
		cmn_err (CE_PANIC, "kbdsetlights: iop_alloc failed");
	}
	Kiocb->obuf = (*(int *)(bp->b_cont->b_rptr)) & KTCLEDMASK;
	lastsetlights = Kiocb->obuf;
	Kiocb->command = KBDWRITELEDS|KBDINTR;
	while (iop_cmdsem (KYBDIOCB) == SEMAPHORE_SET)
	  ;
	if (iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(KeyBoard.kbd_iocb),
		      NO_SLEEP_ADDR) == -1)
	  cmn_err (CE_PANIC, "kbd_setlights:");
	iop_wait (KYBDIOCB, IOPB_SPIN, &stat, 0);
	iop_clear (KYBDIOCB);
	/* old prom will return ILS_BADCMD, try the old way in that case. */
	if ( Kiocb->status & ILS_BADCMD ) {
		if ((Kiocb = (struct _kbdiopb *)iop_alloc (KYBDIOCB,
					   sizeof (struct _kbdiopb))) == NULL) {
			cmn_err (CE_PANIC, "kbdsetlights: iop_alloc failed");
		}
		Kiocb->obuf = KTCLED;
		Kiocb->command = KBDWRITE|KBDINTR;
		while (iop_cmdsem (KYBDIOCB) == SEMAPHORE_SET)
	  	;
		if (iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(KeyBoard.kbd_iocb),
		      	NO_SLEEP_ADDR) == -1)
	  	cmn_err (CE_PANIC, "kbd_setlights:");
		iop_wait (KYBDIOCB, IOPB_SPIN, &stat, 0);
		iop_clear (KYBDIOCB);
		Kiocb->obuf = (*(int *)(bp->b_cont->b_rptr)) & KTCLEDMASK;
		Kiocb->command = KBDWRITE|KBDINTR;
		while (iop_cmdsem (KYBDIOCB) == SEMAPHORE_SET)
	  	;
		if (iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(KeyBoard.kbd_iocb),
		      	NO_SLEEP_ADDR) == -1)
	  	cmn_err (CE_PANIC, "kbd_setlights:");
		iop_wait (KYBDIOCB, IOPB_SPIN, &stat, 0);
		iop_clear (KYBDIOCB);
	}
	ioss->ioc_error = 0;
	ioss->ioc_count = 0;
	bp->b_datap->db_type = M_IOCACK;
	splx(s);
	return(1);
}
kbd_getlights(ioss,bp)
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

typedef struct _kbdiopb QSTRUCT;

#define	NQUEUED(x) (((x)->in==(x)->out)? 0 :		\
			    (((x)->in<(x)->out)? 		\
			     (x)->bufsz-(x)->out+(x)->in :	\
			     (x)->in-(x)->out))

#define	QEMPTY(q) ((q)->in == (q)->out)

typedef unsigned char QTYPE;

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

struct _kbdiopb *kbdiopb;
extern int kbddead;

kbd_ginit() {
    if ((kbdiopb = (struct _kbdiopb *)iop_alloc (KYBDIOCB, sizeof (struct iocb))) == 0) {
	kbddead = 1;
	return (-1);
    }

    /* initialize kbd private structure */

    kbdiopb->command = KBDNULL | KBDINTR;
    kbdiopb->status = 0;
    kbdiopb->obuf = 0;
    kbdiopb->bufsz = KBDBUFSZ;
    kbdiopb->in = 0;
    kbdiopb->out = 0;

    /* init IOP driver */

    iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(kbdiopb), 0);
    interstat ();

    /*
     * force debugging to a known state
     */
    kbdiopb->command = KBDDEBUGOFF | KBDINTR;
    iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET (kbdiopb), 0);
    interstat ();

}

kbd_cmd(cmd)
int cmd;
{
    kbdiopb->obuf = cmd;
    kbdiopb->command = KBDWRITE | KBDINTR;
    iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET (kbdiopb), 0);
    interstat ();
}

kbd_nqueued()
{
	return (NQUEUED(kbdiopb));
}

kbd_qread (qdata, qsize)
    QTYPE *qdata;
    int qsize;
{
    QSTRUCT *qstruct = kbdiopb;
    extern int stdio_init;
    QTYPE qq;
    int qs = qsize;

    if (qs < 0 || qdata == NULL || qstruct == NULL)
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

send_to_keyboard (Command)
    int Command;
{
    int ack = 0;
    do {
	kbdiopb->obuf = Command;
	kbdiopb->command = KBDWRITE | KBDINTR;
	iop_poke (KYBDIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET (kbdiopb), 0);
	interstat ();
    } while (waitforack () && ack++ < ACK_ATTACK);

    return 0;
}

waitforack ()
{
    unsigned char c;
    register volatile struct _kbdiopb *kbdp = kbdiopb;

    while (1) {
	if (NQUEUED (kbdp) > 0) {
	    kbd_qread (&c, 1);
	    if (c == KBACK)
		break;
	    else
		return (interkey (c));
	}
    }
    return (0);
}

/* interpret keyboard status */

interstat ()
{
    int status;

    iop_wait (KYBDIOCB, IOPB_SPIN, &status);
    iop_clear (KYBDIOCB);
}

struct colorm kbd_cm;
struct colorm kbd_cm_sv;
unsigned int irq5to = 0;
unsigned int donotirq5 = 0;

kbd_irq5on()
{
	irq5on();
}
kbd_irq5missing()
{
	donotirq5++;
	irq5to = 0;
	write_color();
}

kbd_colormap(ioss,bp)
register struct iocblk *ioss;
register mblk_t *bp;
{
	register int *intrclr = (int *)PHYS_TO_K1(IOP_VRTCLEAR);
	register char *cp;
	register int i;
	register int svsr;
	register error = 0;

	kbd_cm = *(struct colorm *)bp->b_cont->b_rptr;
	if (!(ioss->ioc_count == sizeof(struct colorm))) {
		error++;
	} else if ( kbd_cm.cmstart >= GRAPHICS_MAX_COLOR_REG ) {
		error++;
	} else if ( kbd_cm.cmcount > (GRAPHICS_MAX_COLOR_REG-kbd_cm.cmstart) ) {
		error++;
	}
	if ( error ) {
		ioss->ioc_error = EINVAL;
		ioss->ioc_count = 0;
		bp->b_datap->db_type = M_IOCNAK;
		kbd_cm.cmcount = 0;
		return(1);
	}
	svsr = irq5off();
	splhi();		/* all other off too */
	for ( cp = &kbd_cm_sv.cmap[kbd_cm.cmstart*3], i = 0; 
					i < (kbd_cm.cmcount*3); i++ ) {
		*cp++ = kbd_cm.cmap[i];
	}
	if ( !irq5to ) {
		kbd_cm_sv.cmstart = kbd_cm.cmstart;
		kbd_cm_sv.cmcount = kbd_cm.cmcount;
	} else {
		if ( kbd_cm.cmstart < kbd_cm_sv.cmstart ) {
			kbd_cm_sv.cmcount += (kbd_cm_sv.cmstart-kbd_cm.cmstart);
			kbd_cm_sv.cmstart = kbd_cm.cmstart;
		} else if ( (kbd_cm_sv.cmstart+kbd_cm_sv.cmcount) <
			(kbd_cm.cmstart+kbd_cm.cmcount) ) {
			kbd_cm_sv.cmcount = 
			   (kbd_cm.cmstart+kbd_cm.cmcount) - kbd_cm_sv.cmstart;
		}
	}
	if ( donotirq5 ) {
		write_color();
		irq5x(svsr);
	} else {
		if ( !irq5to ) {
			irq5to++;
			timeout(kbd_irq5on,0,4);
			timeout(kbd_irq5missing,0,HZ*5);
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
	register cmstart = kbd_cm_sv.cmstart;
	register cmcount = kbd_cm_sv.cmcount;
	register cmindx = cmstart*3;

	while ( cmcount-- ) {
		*raddr = cmstart++; wbflush();
		*daddr = kbd_cm_sv.cmap[cmindx++]; wbflush();
		*daddr = kbd_cm_sv.cmap[cmindx++]; wbflush();
		*daddr = kbd_cm_sv.cmap[cmindx++]; wbflush();
	}

}

kbd_ss_inc(pline,gflag,c)
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
{}
pkbdset1()
{}

pkbd_report()
{}
