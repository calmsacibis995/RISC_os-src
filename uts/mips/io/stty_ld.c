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
#ident	"$Header: stty_ld.c,v 1.40.1.5.1.3.1.2 90/11/12 17:51:42 beacker Exp $"

/* streams 'line discipline'
 */

#include "sys/sbd.h"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/systm.h"
#include "sys/termio.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/proc.h"
#include "sys/fs/s5dir.h"
#include "sys/user.h"
#include "sys/debug.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "bsd43/sys/ioctl.h"
#include "bsd43/sys/signal.h"
#include "sys/sysinfo.h"
#include "sys/kmem.h"

#define UNTIMEOUT_ID untimeout


/* Map user level memory allocation interface to the one supported
 * by the kernel.
 */

#define free(addr) 		kmemfree(addr, M_MISC, M_WAITOK)
#define malloc(sz)		kmemalloc(sz, M_MISC, M_WAITOK)
#define calloc(nelem,elsz) 	kmemzalloc((nelem)*(elsz), M_MISC, M_WAITOK)


extern mblk_t *str_allocb();
extern int _posix_vdisable;

/* default/initial streams 'line discipline' settings */
struct stty_ld def_stty_ld = {
	{				/* st_termio			*/
		(ICRNL|IXON|IXANY	/* st_iflag: input modes	*/
		 |BRKINT|IGNPAR|ISTRIP),
		OPOST|ONLCR|TAB3,	/* st_oflag: output modes	*/
		0,			/* st_cflag: control modes	*/
		ISIG|ICANON|ECHO|ECHOK,	/* st_lflag: line discipline modes */
		LDISC0,			/* st_line:  line discipline	*/
		  			/* st_cc: special characters	*/
		{  CINTR,		/* 0: VINTR 			*/
		   CQUIT,		/* 1: VQUIT			*/
		   CERASE,		/* 2: VERASE			*/
		   CKILL,		/* 3: VKILL			*/
		   CEOF,		/* 4: VEOF (ICANON),		*/
					/*    VMIN (~ICANON)		*/
		   CNUL,		/* 5: VEOL (ICANON),		*/
					/*    VTIME (~ICANON)		*/
		   CNUL,		/* 6: VEOL2			*/
		   CDEL,		/* 7: VSWTCH			*/
		   CSTART,		/* 8: V_START			*/
		   CSTOP,		/* 9: V_STOP			*/
		   CDEL,		/* 10: V_SUSP			*/
		   CDEL,		/* 11: V_DSUSP			*/
		   CRPRNT,		/* 12: V_RPRNT			*/
		   CFLUSH,		/* 13: V_FLUSH			*/
		   CWERASE,		/* 14: V_WERAS			*/
		   CESC,		/* 15: V_LNEXT			*/
		   CDEL,		/* 16: V_STATUS			*/
		   CDEL,		/* 17: V_SAVED_EOF		*/
		   CDEL,		/* 18: V_SAVED_EOL		*/
		   CDEL,		/* 19: (unused)			*/
		   CDEL,		/* 20: (unused)			*/
		   CDEL,		/* 21: (unused)			*/
		   CDEL},		/* 22: (unused)			*/
		0,			/* st_termio.c_saved_flags 	*/
		0,			/* st_termio.c_filler		*/
	},
};

#define SUSPEND_CHAR(c, stp) \
	(stp->st_line != LDISC0 && \
	 (c == stp->st_cc[V_SUSP] || \
	  ((c == stp->st_cc[V_DSUSP]) && !(stp->st_lflag&POSIX_NOIEXTEN))))

#define EOL_CHAR(c, stp) \
	(c == '\n' || c == stp->st_cc[VEOL] || \
	  ((c == stp->st_cc[VEOL2]) && !(stp->st_lflag&POSIX_NOIEXTEN)))

#define	INTR_CHAR(c, stp) (c == stp->st_cc[VINTR]) 
#define	QUIT_CHAR(c, stp) (c == stp->st_cc[VQUIT]) 

static struct module_info stm_info = {
	STRID_STTY_LD,			/* module ID */
	"STTY_LD",			/* module name */
	0,				/* minimum packet size */
	INFPSZ,				/* infinite maximum packet size	*/
	1,				/* hi-water mark */
	0,				/* lo-water mark */
};


static int st_open();
static st_rput(), st_rsrv(), st_close();

static struct qinit st_rinit = {
	st_rput, st_rsrv, st_open, st_close, NULL, &stm_info, NULL
};

static st_wput(), st_wsrv();
static struct qinit st_winit = {
	st_wput, st_wsrv, NULL, NULL, NULL, &stm_info, NULL
};

struct streamtab stty_ldinfo = {&st_rinit, &st_winit, NULL, NULL};



#define ECHO_EOF "^D\b\b"		/* echo this for EOF */



/* types of characters
 */
#define NP	1			/* non-printing, so do not space   */
#define SP	2			/* simple, spacing character	   */
#define LC	3			/* lower case character		   */
#define CT	4			/* print as '^' followed by letter */
#define BS	5			/* backspace			   */
#define LF	6			/* line feed/newline		   */
#define CR	7			/* carriage return		   */
#define HT	8			/* tab				   */
#define VT	9			/* vertical tab			   */
#define FF	10			/* form feed			   */

#define SH_DELTA 16
#define SH(c)	(SH_DELTA+(c))		/* print as \ followed by letter   */


/* the common case when we do not care about case
 */
static u_char st_types[256] = {
	CT, CT, CT, CT,			/* NUL, SOH, STX, ETX */
	CT, CT, CT, CT,			/* EOT, ENQ, ACK, BEL */
	BS, HT, LF, VT,			/* BS, HT, LF, VT */
	FF, CR, CT, CT,			/* FF, CR, SO, SI */
	CT, CT, CT, CT,			/* DLE, DC1, DC2, DC3 */
	CT, CT, CT, CT,			/* DC4, NAK, SYN, ETB */
	CT, CT, CT, CT,			/* CAN, EM, SUB, ESC */
	CT, CT, CT, CT,			/* FS, GS, RS, US */

	SP, SP, SP, SP, SP, SP, SP, SP,	/* space thru ? */
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,

	SP, SP, SP, SP, SP, SP, SP, SP,	/* @ thru _ */
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,

	SP, SP, SP, SP, SP, SP, SP, SP,	/* ` thru  DEL*/
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, CT,


	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
};

/* case matters
 */
static u_char st_types_c[256] = {
	CT, CT, CT, CT,			/* NUL, SOH, STX, ETX */
	CT, CT, CT, CT,			/* EOT, ENQ, ACK, BEL */
	BS, HT, LF, VT,			/* BS, HT, LF, VT */
	FF, CR, CT, CT,			/* FF, CR, SO, SI */
	CT, CT, CT, CT,			/* DLE, DC1, DC2, DC3 */
	CT, CT, CT, CT,			/* DC4, NAK, SYN, ETB */
	CT, CT, CT, CT,			/* CAN, EM, SUB, ESC */
	CT, CT, CT, CT,			/* FS, GS, RS, US */

	SP, SP, SP, SP, SP, SP, SP, SP,	/* space thru ? */
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,
	SP, SP, SP, SP, SP, SP, SP, SP,

	SP,      SH('A'), SH('B'), SH('C'),	/* @ABC */
	SH('D'), SH('E'), SH('F'), SH('G'),	/* DEFG */
	SH('H'), SH('I'), SH('J'), SH('K'),	/* HIJK */
	SH('L'), SH('M'), SH('N'), SH('O'),	/* LMNO */
	SH('P'), SH('Q'), SH('R'), SH('S'),	/* PQRS */
	SH('T'), SH('U'), SH('V'), SH('W'),	/* TUVW */
	SH('X'), SH('Y'), SH('Z'),   SP,	/* XYZ[ */
	SH(CESC),  SP,      SP,      SP,	/*  ]^_ */

	SH(0x27), LC, LC, LC,		/* `abc */
	LC, LC, LC, LC,			/* defg */
	LC, LC, LC, LC,			/* hijk */
	LC, LC, LC, LC,			/* lmno */
	LC, LC, LC, LC,			/* pqrs */
	LC, LC, LC, LC,			/* tuvw */
	LC, LC, LC, SH('('),		/* xyz{ */
	SH('!'), SH(')'), SH('^'), CT,	/* |}~  */


	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,

	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
	NP, NP, NP, NP, NP, NP, NP, NP,
};



/* pass a data message to the next queue
 */
static int				/* 1=success, 0=cannot send it now */
st_pass(q, bp)
register queue_t *q;
register mblk_t *bp;
{
	ASSERT(M_DATA == bp->b_datap->db_type);

	if (!canput(q->q_next)) {
		noenable(q);
		return 0;
	}

	putnext(q, bp);
	return 1;
}



/* process a M_FLUSH message
 *	We must flush from the service routines because:
 *	  (1) the put() function of a queue can be called from an interrupt
 *		routine
 *	  (2) we need to do more than flush the queues themselves.  We must
 *		change our private data structures
 *	  (3) therefore we must either have all the service functions run
 *		at splstr() or we must queue flush requests.  We might spend a
 *		long time working on a single, big data block.
 *	  (4) therefore, we should queue flush requests.
 */
static
st_rflush(stp,flag,rq)
register struct stty_ld *stp;
register u_char flag;
register queue_t *rq;
{
	if (flag & FLUSHR) {
		flushq(rq, FLUSHDATA);

		freemsg(stp->st_imsg);
		stp->st_imsg = NULL;

		freemsg(stp->st_lmsg);
		stp->st_lmsg = NULL;
		stp->st_llen = 0;

		st_untime(stp);
		qenable(rq);
	}
}


static
st_wflush(stp,flag,wq)
register struct stty_ld *stp;
register u_char flag;
register queue_t *wq;
{
	if (flag & FLUSHW) {
		flushq(wq, FLUSHDATA);

		if (stp->st_line != LDISC_NEW) {
			freemsg(stp->st_emsg);
			stp->st_emsg = NULL;

			if (stp->st_state & ST_TABWAIT) {
				stp->st_state &= ~ST_TABWAIT;
				qenable(stp->st_rq);
			}
		};

		qenable(wq);
	}
}



/* open a new stream
 */
/* ARGSUSED */
static int
st_open(rq, dev, flag, sflag)
register queue_t *rq;			/* our read queue */
dev_t dev;
int flag;
int sflag;
{
	register struct stty_ld *stp;
	register struct proc *pp;

	if (MODOPEN != sflag)
		return OPENFAIL;

	if (!(stp = (struct stty_ld*)rq->q_ptr)) {
		register queue_t *wq;	/* initialize on the 1st open() */

		if (!(stp = (struct stty_ld*)malloc(sizeof(*stp))))
			return OPENFAIL;

		bcopy((char*)&def_stty_ld,	/* choose an initial state */
		      (char*)stp,
		      sizeof(*stp));
		wq = WR(rq);
		rq->q_ptr = (caddr_t)stp;
		wq->q_ptr = (caddr_t)stp;
		stp->st_rq = rq;
		stp->st_wq = wq;

		qenable(rq);		/* must tell stream head about tty */
	}

	return 0;
}



/* close a stream
 *	This is called when the stream is being dismantled or when this
 *	module is being popped.
 */
static
st_close(rq)
register queue_t *rq;
{
	register struct stty_ld *stp = (struct stty_ld*)rq->q_ptr;

	ASSERT(stp->st_rq == rq);

	set_str_control(rq,(SO_ISTTY | SO_JOBCTRL | SO_PENDIN | SO_TOSTOP),0);

	st_untime(stp);			/* forget cannonical timer */

	freemsg(stp->st_imsg);		/* release all of our buffers */
	freemsg(stp->st_lmsg);
	freemsg(stp->st_emsg);

	str_unbcall(rq);		/* stop waiting for buffers */

	free((char*)stp);		/* all finished */
}



/* accept a new output message
 */
static
st_wput(wq, bp)
register queue_t *wq;
register mblk_t *bp;
{
	register struct stty_ld *stp = (struct stty_ld*)wq->q_ptr;

	ASSERT(stp->st_wq == wq);

	switch (bp->b_datap->db_type) {
	case M_DATA:
		if (bp->b_rptr >= bp->b_wptr	/* discard empty messages */
		    && !msgdsize(bp)) {
			freemsg(bp);
			break;
		}
		if (stp->st_line == LDISC_NEW &&
		    stp->st_lflag & LNEW_FLUSHO) {
			freemsg(bp);
			break;
		}
		if ((OPOST & stp->st_oflag)	/* if nothing to do, */
		    || 0 != wq->q_first	
		    || !st_pass(wq, bp))	/* pass it along */
			putq(wq,bp);	/* unless we must look at it */
		break;

	case M_FLUSH:
		enableok(wq);
		putq(wq, bp);
		break;

	case M_RETYPE:
		putq(wq,bp);
		break;
		
		
	case M_IOCTL:
		switch (((struct iocblk*)bp->b_rptr)->ioc_cmd) {
		case TCSETAW:		/* must let output drain before	*/
		case TCSETAF:		/* some IOCTLs */
			st_update_pendin(stp,bp);
		case TCSBRK:
		case TIOCSTI:
			putq(wq,bp);
			break;

		case BSD43_TIOCOUTQ:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (! (((struct iocblk*)bp->b_rptr)->ioc_count == sizeof(int))) {
				bp->b_datap->db_type = M_IOCNAK;
				qreply(wq,bp);
				break;
			};
			argi += qmsgdsize(wq);
			putnext(wq,bp);	/* now pass on down */
			break;
#undef argi

		case FIOASYNC:
#define argi  (*(int*)(bp->b_cont->b_rptr))
			if (argi)
				stp->st_state |= ST_SIGIO;
			else
				stp->st_state &= ~ST_SIGIO;
			putnext(wq,bp);	/* now pass on down */
			break;
#undef argi

		case TCSETA:
			st_update_pendin(stp,bp);
		default:		/* send other msgs down */
			putnext(wq,bp);
			break;
		}
		break;

	default:
		putnext(wq,bp);
		break;
	}
}


/*
 * copy LNEW_PENDIN setting to our state and delete from M_IOCTL block
 * on its way down.	
 */
static int
st_update_pendin(stp,bp)
	struct	stty_ld *stp;
	mblk_t	*bp;
{
	stp->st_lflag &= ~LNEW_PENDIN;
	stp->st_lflag |= STERMIO(bp)->c_lflag & LNEW_PENDIN;
	STERMIO(bp)->c_lflag &= ~LNEW_PENDIN;
}



/*
 * copy LNEW_PENDIN from out state to the M_IOCTL block
 */
static int
st_copy_pendin(stp,bp)
	struct	stty_ld *stp;
	mblk_t	*bp;
{
	STERMIO(bp)->c_lflag &= ~LNEW_PENDIN;
	STERMIO(bp)->c_lflag |= stp->st_lflag & LNEW_PENDIN;
}



/* send whatever output we have
 */
static int				/* 0=failed */
st_wsend(bp,cp,wq)
register mblk_t *bp;			/* what we have done so far */
u_char *cp;				/* bytes up to, but including, this */
queue_t *wq;				/* send it on this queue */
{
	register mblk_t *bp1;

	if (cp <= bp->b_rptr)		/* quit if we have done nothing */
		return 1;

	if (!canput(wq->q_next)){	/* if we can't send below, then disable ourselves */
		noenable(wq);		/* and stop queueing packets for below */
		return 0;
	}

	if (!(bp1 = dupb(bp))) {
		(void)bufcall(0, BPRI_HI, qenable, wq);
		return 0;
	}

	bp1->b_wptr = cp;
	bp->b_rptr = cp;
	putnext(wq, bp1);

	if (!canput(wq->q_next))
		noenable(wq);
	return 1;
}



/* try to allocate a block
 *	on failure, send what we have
 */
static mblk_t*				/* 0=failed */
st_ckblk(sz,bp,cp,wq)
uint sz;				/* this size block */
mblk_t *bp;				/* what we have so far */
u_char *cp;				/* up to this byte */
queue_t *wq;				/* our write queue */
{
	register mblk_t *bp1;

	ASSERT(sz <= MAXBSIZE);
	bp1 = str_allocb(sz,wq,BPRI_LO);
	if (!bp1)			/* if allocation fails */
		(void)st_wsend(bp,cp,wq);	/* get what we can */
	return bp1;
}



/* send what we have, and allocate a new output block
 */
static mblk_t*				/* 0=failed */
st_wnew(sz,bp,cp,wq)
uint sz;				/* this size block */
mblk_t *bp;				/* what we have so far */
u_char *cp;				/* up to this byte */
queue_t *wq;				/* our write queue */
{
	if (!st_wsend(bp,cp,wq))	/* send what we have so far */
		return NULL;

	if (!canput(wq->q_next)) {	/* quit if downstream congested */
		noenable(wq);
		return NULL;
	}

	return st_ckblk(sz,bp,cp,wq);
}



/* send a delay
 */
static int				/* 0=failed */
st_wdelay(stp,bp,cp,del,pre)
register struct stty_ld *stp;
register mblk_t *bp;			/* what we have done so far */
u_char *cp;				/* send thru this byte */
int del;				/* and then delay this many ticks */
char pre;				/* '\0' or preamble to delay */
{
	register mblk_t *bp1, *bp0;

	if (!pre) bp0 = NULL;
	else {
		bp0 = st_ckblk(1,bp,cp,stp->st_wq);
		if (!bp0) return 0;
		*bp0->b_wptr++ = pre;
	}

	if (!del) bp1 = NULL;
	else {				/* get msg for delay */
		bp1 = st_ckblk(sizeof(int),
			       bp,cp,stp->st_wq);
		if (!bp1) {
			if (bp0) freeb(bp0);
			return 0;
		}

		if (del < 32		/* use fill characters if asked */
		    && stp->st_oflag & OFILL) {
			register char c;
			c = ((stp->st_oflag & OFDEL) ? 0177 : 0);
			*bp1->b_wptr++ = c;
			if (del > 3)
				*bp1->b_wptr++ = c;

		} else {
			bp1->b_datap->db_type = M_DELAY;
			*(int*)(bp1->b_wptr) = del;
			bp1->b_wptr += sizeof(int);
		}
	}

	if (!st_wsend(bp,cp+1,stp->st_wq)) {	/* send up thru target */
		if (bp0) freeb(bp0);
		if (bp1) freeb(bp1);
		return 0;
	}

	if (bp0) putnext(stp->st_wq, bp0);
	if (bp1) putnext(stp->st_wq, bp1);

	return 1;
}



/* do CR/LF delays
 */
static int				/* 0=failed */
st_cr(stp,bp,cp,pre)
register struct stty_ld *stp;
register mblk_t *bp;			/* what we have done so far */
u_char *cp;
char pre;				/* send this character first */
{
	register int del;

	switch (stp->st_oflag & CRDLY) {
	case CR0:
		del = 0;
		break;
	case CR1:
		del = (stp->st_ocol >> 4) + 3;
		if (del < 6) del = 6;
		break;
	case CR2:
		del = 5;
		break;
	case CR3:
		del = 9;
		break;
	}

	if (!st_wdelay(stp,bp,cp,del,pre))
		return 0;

	stp->st_ocol = 0;
	return 1;
}



/* (try to) send a message toward the output
 */
static mblk_t*				/* return unfinished part */
st_mout(stp,wbp)
register struct stty_ld *stp;
register mblk_t *wbp;			/* write this message */
{
	register u_char *cp, *lim;
	register u_char *ctype;
	register mblk_t *bp1;
	register u_char c, type;
	register int n;


	if (wbp->b_rptr >= wbp->b_wptr	/* discard empty messages */
	    && !msgdsize(wbp)) {
		freemsg(wbp);
		return 0;
	}

	if (!(OPOST & stp->st_oflag)) {	/* quick in raw mode */
		if (!st_pass(stp->st_wq, wbp))
			return wbp;
		return NULL;
	}

	if (!(stp->st_lflag & XCASE)	/* in usual situation */
	    && !(stp->st_oflag & OLCUC))	/* upper case is ok */
		ctype = &st_types[0];
	else
		ctype = &st_types_c[0];

	do {
		cp = wbp->b_rptr;
		lim = wbp->b_wptr;
		while (cp < lim) {
			switch (type = ctype[c = *cp]) {

			case NP:
				break;

			case SP:
				stp->st_ocol++;
				break;

			case CT:
				break;

			case BS:
				if ((stp->st_oflag & BSDLY)
				    && !st_wdelay(stp,wbp,cp,2,'\0'))
					return wbp;
				if (stp->st_ocol)
					stp->st_ocol--;
				break;

			case LF:
				if (stp->st_oflag & ONLRET) {
					if (!st_cr(stp,wbp,cp,'\0'))
						return wbp;
				} else if (stp->st_oflag & ONLCR) {
					if (!st_cr(stp,wbp,cp,
						   ((stp->st_oflag & ONOCR)
						    && 0 == stp->st_ocol)
						   ? '\0' : '\r'))
						return wbp;

				} else if ((stp->st_oflag & NLDLY)
					   && !st_wdelay(stp,wbp,cp,5,'\0')) {
					return wbp;
				}
				break;

			case CR:
				if (stp->st_oflag & OCRNL) {
					if ((stp->st_oflag & NLDLY)
					    && !st_wdelay(stp,wbp,cp,5,0))
						return wbp;
				} else {
					if (!st_cr(stp,wbp,cp,'\0'))
						return wbp;
				}
				break;

			case HT:
				n = 8 - (stp->st_ocol & 7);
				switch (stp->st_oflag & TABDLY) {
				case TAB0:
					stp->st_ocol +=n;
					break;
				case TAB1:
					if (n >= 5
					    && !st_wdelay(stp,wbp,cp,n,0))
						return wbp;
					stp->st_ocol +=n;
					break;
				case TAB2:
					if (!st_wdelay(stp,wbp,cp,2,'\0'))
						return wbp;
					stp->st_ocol +=n;
					break;
				case TAB3:
					bp1 = st_wnew(n,wbp,cp,stp->st_wq);
					if (!bp1)
						return wbp;
					stp->st_ocol +=n;
					do {
						*bp1->b_wptr++ = ' ';
					} while (--n);
					putnext(stp->st_wq,bp1);
					wbp->b_rptr++;	/* do not send ^I */
					break;
				}
				break;

			case VT:
				if ((stp->st_oflag & VTDLY)
				    && !st_wdelay(stp,wbp,cp,0177,'\0'))
					return wbp;
				break;

			case FF:
				if ((stp->st_oflag & FFDLY)
				    && !st_wdelay(stp,wbp,cp,0177,'\0'))
					return wbp;
				break;

			case LC:	/* send burst of lower case */
				if (!(stp->st_oflag & OLCUC)) {
					stp->st_ocol++;
					break;
				}
				n = (lim  - cp)<<1;
				if (n > 64) n = 64;
				bp1 = st_wnew(n,wbp,cp,stp->st_wq);
				if (!bp1)
					return wbp;
				n = (bp1->b_datap->db_lim
				     - bp1->b_datap->db_base);
				do {
					c -= ('a'-'A');
					*bp1->b_wptr++ = c;
					stp->st_ocol++;
				} while (++cp < lim
					 && --n > 0
					 && LC == (ctype[c = *cp]));
				putnext(stp->st_wq,bp1);
				wbp->b_rptr = cp;
				cp--;
				break;

			default:	/* send burst of upper case */
				if (!(stp->st_lflag & XCASE)) {
					stp->st_ocol++;
					break;
				}
				n = (lim  - cp)<<1;
				if (n > 64) n = 64;
				bp1 = st_wnew(n,wbp,cp,stp->st_wq);
				if (!bp1)
					return wbp;
				n = (bp1->b_datap->db_lim
				     - bp1->b_datap->db_base);
				do {
					*bp1->b_wptr++ = CESC;
					*bp1->b_wptr++ = type-SH_DELTA;
					stp->st_ocol += 2;
				} while (++cp < lim
					 && (n -= 2) > 1
					 && SH_DELTA <= (type = ctype[*cp]));
				putnext(stp->st_wq,bp1);
				wbp->b_rptr = cp;
				cp--;
				break;
			}

			++cp;
		}

		if (!st_wsend(wbp,cp,stp->st_wq))	/* send block on */
			return wbp;	/* quit if we cannot */

		bp1 = rmvb(wbp,wbp);	/* trim block from input */
		freeb(wbp);
		wbp = bp1;
	} while (0 != wbp		/* quit at end of message */
		 && canenable(stp->st_wq));	/* or downstream congestion */

	return wbp;
}



/* work on the output queue
 *	This code is similar to the old System V line disciplines.  However,
 *	it is faster both in the worst case and on average.  For example,
 *	it never scans the output more than once.  You should judge for
 *	yourself whether it is shorter or simpler.  It is supposed to have
 *	the same behavior, when view from the terminal or the application
 *	program.
 */
static
st_wsrv(wq)
register queue_t *wq;			/* our write queue */
{
	register struct stty_ld *stp;
	register mblk_t *wbp;
	register struct iocblk *iocp;

	stp = (struct stty_ld*)wq->q_ptr;
	ASSERT(stp->st_wq == wq);

	enableok(wq);
	for (;;) {
		wbp = getq(wq);		/* get another message */
		if (!wbp)
			break;

		switch (wbp->b_datap->db_type) {
		case M_DATA:		/* send data on */
			if (!canenable(wq)	/* unless already congested */
			    || (wbp = st_mout(stp,wbp),
				0 != wbp)) {
				putbq(wq, wbp);
				return;
			}
			stp->st_xcol = stp->st_ocol;
			break;

		case M_IOCTL:
			iocp = (struct iocblk*)wbp->b_rptr;
			if (iocp->ioc_cmd == TIOCSTI) {
				if (! (iocp->ioc_count == sizeof(char))) {
					wbp->b_datap->db_type = M_IOCNAK;
					qreply(wq,wbp);
					break;
				};
				putq(RD(wq), unlinkb(wbp));
				iocp->ioc_count = 0;
				wbp->b_datap->db_type = M_IOCACK;
				qreply(wq, wbp);
			} else {
				putnext(wq, wbp);
			}
			break;

		case M_FLUSH:
			st_wflush(stp,*wbp->b_rptr,wq);
			putnext(wq, wbp);
			break;

		case M_RETYPE:
			if (! st_retype_line(stp,0,-1)) {
				putbq(wq,wbp);
				return;
			};
			stp->st_termio.c_lflag &= ~LNEW_PENDIN;
			wbp->b_datap->db_type = M_IOCACK;
			qreply(wq,wbp);
			break;

		default:
			putnext(wq, wbp);
			break;
		}
	}

	if (wbp = stp->st_emsg) {	/* do echos */
		if (stp->st_state & ST_BCOL_BAD) {
			stp->st_bcol = stp->st_ocol;
			stp->st_state &= ~ST_BCOL_BAD;
		}
		if (stp->st_emsg = st_mout(stp,wbp))
			return;		/* quit if not finished */
	}

	if (stp->st_state & ST_TABWAIT) {
		stp->st_state &= ~ST_TABWAIT;
		qenable(stp->st_rq);
	}
	if (stp->st_xcol > stp->st_ocol)
		stp->st_xcol = stp->st_ocol;
	if (stp->st_bcol > stp->st_ocol)
		stp->st_bcol = stp->st_ocol;
}



/* make room in the current, canonical line buffer
 *	come here when we run out of room in the buffer.
 */
static int				/* 0=failed */
st_iroom(stp)
register struct stty_ld *stp;
{
	register mblk_t *ibp = stp->st_imsg;

	ASSERT(ibp && ibp == stp->st_ibp);

	if (LDISC0 == stp->st_line) {	/* for old stuff, flush the line */
		ibp->b_wptr = ibp->b_rptr;
		freemsg(stp->st_lmsg);
		stp->st_lmsg = 0;
		stp->st_llen = 0;
		return 1;

	} else {			/* kill last character in new mode */
		return st_bs(stp,0, ECHOE);
	}
}



/* send typed-ahead lines to the stream head
 */
static
st_sendold(stp)
register struct stty_ld *stp;
{
	register mblk_t *bp;
	register queue_t *rq = stp->st_rq;

	while (NULL != (bp = stp->st_lmsg)
	       && canput(rq->q_next)) {
		stp->st_lmsg = bp->b_cont;
		bp->b_cont = 0;
		stp->st_llen -= (bp->b_wptr - bp->b_rptr);
		putnext(rq,bp);
	}
}



/* send whatever input we have up the read queue, and get another buffer.
 */
static mblk_t*				/* 0=failed to allocate buffer */
st_rsend(stp,ac)
register struct stty_ld *stp;
int ac;					/* 1=allocate a new buffer */
{
	register mblk_t *imp;
	register ushort sz;

	ASSERT(stp->st_lflag & ICANON);
	
	imp = stp->st_imsg;
	if (imp) {			/* if we have a buffer */
		sz = imp->b_wptr - imp->b_rptr;	/* & it has data */
		if (sz) {
			str_conmsg(&stp->st_lmsg, &stp->st_lbp, imp);
			stp->st_llen +=sz;
			st_sendold(stp);
		} else {
			freemsg(imp);
		}
		imp = 0;
	}

	if (ac) {			/* get new canonical line buffer */
		if (NULL != stp->st_lmsg	/* use typed-ahead line */
		    && (imp = stp->st_lbp,
			imp->b_wptr + ST_MAX_LINE <= imp->b_datap->db_lim)) {
			imp = dupb(imp);
			if (0 != imp)
				imp->b_rptr = imp->b_wptr;

		} else {		/* or create a new buffer */
			imp = str_allocb((!stp->st_lmsg
					  ? ST_MAX_LINE	/* modest at 1st */
					  : ((ST_MAX_LINE*3)/2)),
					 stp->st_rq,BPRI_LO);
		}
	}

	stp->st_state |= ST_BCOL_BAD;
	stp->st_imsg = imp;
	stp->st_ibp = imp;
	return imp;
}



/* kill/forget non-canonical timer
 */
static
st_untime(stp)
register struct stty_ld *stp;
{
	if (ST_TIMING & stp->st_state)
		UNTIMEOUT_ID(stp->st_tid);

	stp->st_state &= ~(ST_TIMING|ST_TIMED);
}


/* do something when non-canonical timer expires
 */
static
st_tock(stp)
register struct stty_ld *stp;
{
	stp->st_state &= ~ST_TIMING;
	stp->st_state |= ST_TIMED;
	qenable(stp->st_rq);
}



/* stuff a non-canonical input byte into a buffer
 *      In non-ICANON mode, if excessive input is discovered we stop
 *      reading data allowing lower routines to enforce flow control.
 */
static int				/* 0=failed */
st_nonstuff(stp,c,rmp)
register struct stty_ld *stp;
u_char c;				/* stuff this byte */
register mblk_t *rmp;			/* from this buffer */
{
	register mblk_t *ibp;

	ibp = stp->st_imsg;
	if (0 != ibp) {			/* if already have a buffer */
		if (ibp->b_wptr		/* & have room, then go fast */
		    < ibp->b_datap->db_lim) {
			if (c == *ibp->b_wptr) {
				++ibp->b_wptr;	/* our buffer */
				return 1;	/* already has right char */
			}

			if (1 == ibp->b_datap->db_ref) {
				*ibp->b_wptr++ = c;	/* if buf private */
				return 1;	/* then we can change it */
			}
		}

		if (msgdsize(ibp) >= ST_MAX_LINE	/* if we have lots */
		    && !canput(stp->st_rq->q_next))	/* of text already */
			return 0;	/* forget this character */
	}


	if (c == *rmp->b_rptr		/* if copying char, */
	    && rmp->b_rptr < rmp->b_datap->db_lim) {	/* clone input */
		ibp = dupb(rmp);
		if (!ibp) {
			(void)bufcall(0, BPRI_HI, qenable, stp->st_rq);
			return 0;
		}
		ibp->b_wptr = rmp->b_rptr+1;

	} else {			/* else, make a new buffer */
		ibp = str_allocb(rmp->b_wptr - rmp->b_rptr,
				 stp->st_rq,BPRI_LO);
		if (!ibp)
			return 0;
		*ibp->b_wptr++ = c;
	}

	if (!stp->st_imsg) stp->st_imsg = ibp;
	else stp->st_ibp->b_cont = ibp;
	stp->st_ibp = ibp;
	return 1;
}



/* stuff an echo into a suitable buffer
 *	On successful exit, there is guaranteed to be a current echo message.
 */
static int				/* 0=failed */
st_estuff(stp,c,rmp)
register struct stty_ld *stp;
register u_char c;
mblk_t *rmp;				/* 0 or read queue msg */
{
	register mblk_t *ebp;

	if (stp->st_emsg		/* if already have a buffer */
	    && (ebp = stp->st_ebp,
		ebp->b_wptr < ebp->b_datap->db_lim)) {	/* & have room */
		if (c == *ebp->b_wptr) {	/* go fast */
			++ebp->b_wptr;
			return 1;
		}

		if (1 == ebp->b_datap->db_ref) {	/* if new buffer */
			*ebp->b_wptr++ = c;	/* it is ok to change it */
			return 1;
		}
	}

	if (rmp
	    && c == *rmp->b_rptr	/* if copying char, */
	    && rmp->b_rptr < rmp->b_datap->db_lim) {	/* clone input */
		ebp = dupb(rmp);
		if (!ebp) {
			(void)bufcall(0, BPRI_HI, qenable, stp->st_rq);
			return 0;
		}
		ebp->b_wptr = rmp->b_rptr+1;

	} else {			/* else, make a new buffer */
		ebp = str_allocb(rmp ? (rmp->b_wptr - rmp->b_rptr) : 1,
				 stp->st_rq,BPRI_LO);
		if (!ebp)
			return 0;
		*ebp->b_wptr++ = c;
	}

	if (!stp->st_emsg) {
		stp->st_emsg = ebp;
		if (canenable(stp->st_wq))
			qenable(stp->st_wq);
	} else {
		stp->st_ebp->b_cont = ebp;
	}
	stp->st_ebp = ebp;
	return 1;
}



/* see that there is room for some echos
 */
static mblk_t*				/* 0=failed */
st_echeck(stp,n)
register struct stty_ld *stp;
register int n;
{
	register mblk_t *ebp;

	ebp = stp->st_ebp;
	if (!stp->st_emsg
	    || 1 != ebp->b_datap->db_ref
	    || ebp->b_wptr >= (ebp->b_datap->db_lim - n)) {
		ASSERT(n <= MAXBSIZE);
		ebp = str_allocb(n, stp->st_rq,BPRI_LO);
		if (ebp) {		/* fail if cannot get another block */
			if (!stp->st_emsg) {
				stp->st_emsg = ebp;
				if (canenable(stp->st_wq))
					qenable(stp->st_wq);
			} else {
				stp->st_ebp->b_cont = ebp;
			}
			stp->st_ebp = ebp;
		}
	}

	return ebp;
}



/* echo a byte
 *	Check for control characters
 */
static int				/* 0=failed */
st_echo(stp,c,rmp)
register struct stty_ld *stp;
register u_char c;			/* echo this */
mblk_t *rmp;				/* 0 or read queue msg */
{
	if (stp->st_line != LDISC0 &&
	    ! (stp->st_line == LDISC_NEW &&
	       (stp->st_lflag & LNEW_CTLECH) == 0)) {
		switch (st_types[c]) {
		case CT:		/* convert control characters */
		case BS:
		case LF:
		case CR:
		case VT:
		case FF:
			if (!st_echeck(stp,2))
				return 0;
			(void)st_estuff(stp, '^',rmp);
			c ^= '@';
			break;
		}
	}

	if (c == CEOF)
		return(1);
	return st_estuff(stp,c,rmp);
}



/* echo backspacing over an echoed byte
 *	Check for control characters
 */
static int				/* 0=failed */
st_echo_bs(stp,c)
register struct stty_ld *stp;
register u_char c;			/* echo this */
{

	if (stp->st_line == LDISC0 ||
	    (stp->st_line == LDISC_NEW &&
	     ((stp->st_lflag & LNEW_CRTBS) == 0) &&
	     (stp->st_state & ST_LIT) == 0)) /* Real BSD doesn't consider
						CRTBS when doing literals,
						so we wont either */
		return(1);

	if (stp->st_line != LDISC0 &&
	    ! (stp->st_line == LDISC_NEW &&
	       (stp->st_lflag & LNEW_CTLECH) == 0)) {
		switch (st_types[c]) {
		case CT:		/* convert control characters */
		case BS:
		case LF:
		case CR:
		case VT:
		case FF:
			if (!st_echeck(stp,2))
				return 0;
			(void)st_estuff(stp, '\b',NULL);
			c |= '@';
			break;
		}
	}

	if (c == CEOF)
		return(1);
	return st_estuff(stp,'\b',NULL);
}



/* send a string of 'echos'
 */
static int				/* 0=failed */
st_estr(stp,n,str)
register struct stty_ld *stp;
register int n;				/* length of the string */
register char *str;
{
	register mblk_t *ebp;

	if (!(ebp = st_echeck(stp,n)))
		return 0;

	while (--n >= 0)
		*ebp->b_wptr++ = *str++;
	return 1;
}



/*
 * set up wait for output complete, if tab echoing needed
 */
st_bs_wait_needed(stp)
	struct stty_ld *stp;
{
	if (stp->st_line == LDISC0 ||
	    (stp->st_lflag & ECHO) == 0 ||
	    (stp->st_line == LDISC_NEW &&
	     (stp->st_lflag & ECHOE) == 0 &&
	     (stp->st_lflag & LNEW_CRTBS) == 0) ||
	    (stp->st_imsg->b_rptr >= stp->st_imsg->b_wptr))
		return(1);
	return(stp->st_wq->q_first ||
	       stp->st_emsg);
}

/* delete the last character input
 */
static int				/* 0=failed */
st_bs(stp,mode,eflag)
register struct stty_ld *stp;
int	mode;	/* 0 = start, 1 = begin, 2 = middle, 3 = end */
int  eflag;  /* echo flag = ECHOE or ECHOK */
{
	static char bs[] = "\b \b\b \b\b \b\b \b\b \b\b \b\b \b\b \b";
	static char bs2[] = "\b\b\b\b\b\b\b\b";
	register mblk_t *ibp = stp->st_imsg;
	register short new;
	register u_char *cp, *lim;
	register u_char *ctype;

	ASSERT(ibp && ibp == stp->st_ibp);

 	if ((lim = ibp->b_wptr-1) < ibp->b_rptr ||
 	     lim < ibp->b_datap->db_base) {
 		/* we are either at the beginning of our input or
 		/* we are at the beginning of the buffer, so there is 
 		 * nothing to backspace over
 		 */
 		return 1;
 	};
	if (stp->st_line == LDISC0) {
		switch (stp->st_lflag & (ECHO|ECHOE)) {
		case (ECHO|ECHOE):
			    if (!st_estr(stp,3, &bs[0]))
				    return 0;
			    break;
		case (ECHO):
			    if (!st_estr(stp,1, &bs[0]))
				    return 0;
			    break;
		case (ECHOE):
			    if (!st_estr(stp,2, &bs[1]))
				    return 0;
			    break;
		}
		if (ibp->b_rptr >= ibp->b_wptr)	/* quit if nothing to do */
			return 1;

	} else if (stp->st_lflag & ECHO) {
		if (stp->st_line == LDISC_NEW &&
		   (stp->st_lflag & LNEW_CRTBS) == 0 &&
		   (stp->st_lflag & eflag) == 0) {
			if (stp->st_lflag & LNEW_PRTERA) {
				if (ibp->b_rptr >= ibp->b_wptr)
						/* quit if nothing to do */
					return 1;

				if (! st_echeck(stp,4))
					return(0);
				if (mode == 0 ||
				    mode == 1)
					st_estuff(stp,'\\',NULL);
				st_echo(stp,*lim,NULL);
				if (mode == 0 ||
				    mode == 3)
					st_estuff(stp,'/',NULL);
			} else {
				if (! st_echo(stp,stp->st_cc[VERASE],NULL))
				  	return(0);
			};
			goto update_and_return;
		};

		if (ibp->b_rptr >= ibp->b_wptr)	/* quit if nothing to do */
			return 1;

		if (stp->st_wq->q_first	/* wait until is output quiet */
		    || (stp->st_emsg &&
			stp->st_emsg->b_rptr < stp->st_emsg->b_wptr)) {
			stp->st_state |= ST_TABWAIT;
			noenable(stp->st_rq);
			return 0;
		}

		new = stp->st_ocol;
	       if (OPOST & stp->st_oflag) {
		ctype = ((stp->st_lflag & XCASE)
			 ? &st_types_c[0]
			 : &st_types[0]);
		switch (ctype[*lim]) {
		case NP:
			break;
		case SP:
		case LC:
			new--;
			break;
		case CT:
			if (*lim == CEOF)
				break;
		case BS:
		case LF:
		case CR:
		case VT:
		case FF:
			new -= 2;
			break;

		case HT:
			new = stp->st_bcol;
			for (cp = ibp->b_rptr; cp < lim; cp++) {
				switch (ctype[*cp]) {
				case NP:
					break;
				case SP:
				case LC:
					new++;
					break;
				case CT:
				case BS:
				case LF:
				case CR:
				case VT:
				case FF:
					new += 2;
					break;
				case HT:
					new += 8;
					new &= ~7;
					break;
				default:
					new += 2;
					break;
				}
			}
			break;

		default:
			new -= 2;
			break;
		}
	       }
	       else
			new -= 1;

		if (new < stp->st_ocol) {	
			if ((new < stp->st_xcol) && (OPOST & stp->st_oflag)) {
				if (!st_retype_line(stp,1,0))
					return 0;
			} else if (stp->st_line == LDISC_NEW &&
				   (stp->st_lflag & eflag) == 0) {
				new = (stp->st_ocol - new);
				if (new > sizeof(bs2))
 					new = sizeof(bs2);
				if (!st_estr(stp,new,bs2))
					return(0);
			} else {
				new = 3*(stp->st_ocol - new);
				if (new > sizeof(bs))
					new = sizeof(bs);
				if (!st_estr(stp, new, bs))
					return 0;
			}
		}
	} else {
		if (ibp->b_rptr >= ibp->b_wptr)	/* quit if nothing to do */
			return 1;
	};		

update_and_return:
	ibp->b_wptr = lim;
	return 1;
}



#define st_bs_mode(count,ibp) ((count == 1) ? 0 : \
				(((ibp->b_wptr - ibp->b_rptr) == count) ? 1 : \
				  ((ibp->b_wptr - ibp->b_rptr) == 1) ? 3 : 2))
	
/* empty the line buffer
 */
static int				/* 0=failed */
st_del_line(stp,c)
register struct stty_ld *stp;
u_char c;				/* character to echo */
{
	register mblk_t *ibp = stp->st_imsg;

	ASSERT(ibp && ibp == stp->st_ibp);
	if (stp->st_lflag & ECHOK) {
		if (stp->st_line == LDISC0) {
			register mblk_t *ebp;

			if (!(ebp = st_echeck(stp,2)))
				return 0;
			if (stp->st_lflag & ECHO)
				*ebp->b_wptr++ = c;
			*ebp->b_wptr++ = '\n';

		} else {
			int	count;

			if (! st_echeck(stp,
				((ibp->b_wptr - ibp->b_rptr) * 2) + 1))
				return(0);
			count = ibp->b_wptr - ibp->b_rptr;
			while (ibp->b_rptr < ibp->b_wptr) {
				if (!st_bs(stp,st_bs_mode(count,ibp), ECHOK))
					return 0;
			};
		}
	} else if (stp->st_line == LDISC_NEW &&
		   (stp->st_lflag & ECHO) != 0) {
		if (! st_echeck(stp,3))
			return(0);
		st_echo(stp,c,NULL);
		st_estuff(stp,'\n',NULL);		
	}

	ibp->b_wptr = ibp->b_rptr;
	return 1;
}



/* delete the previous word
 */
static int				/* 0=failed */
st_del_word(stp)
register struct stty_ld *stp;
{
	register u_char c;
	register mblk_t *ibp = stp->st_imsg;
	int	count;

	ASSERT(ibp && ibp == stp->st_ibp);

	count = ibp->b_wptr - ibp->b_rptr;
	while (ibp->b_rptr < ibp->b_wptr) {
		c = *(ibp->b_wptr - 1);
		if (' ' != c && '\t' != c)
			break;
		if (!st_bs(stp,st_bs_mode(count,ibp), ECHOE))
			return 0;
	}

	while (ibp->b_rptr < ibp->b_wptr) {
		c = *(ibp->b_wptr - 1);
		if (' ' == c || '\t' == c)
			break;
		if (!st_bs(stp,st_bs_mode(count,ibp), ECHOE))
			return 0;
	}

	return 1;
}



/* retype what we have so far
 */
static int				/* 0=failed */
st_retype_line(stp,delta,doecho)
register struct stty_ld *stp;
int delta;				/* exclude these bytes from echo */
int	doecho;
{
	register mblk_t *bp;
	register u_char *cp;
	register int sz;

	if (stp->st_wq->q_first		/* when things are quiet, */
	    || !st_echeck(stp,ST_MAX_LINE*5)) {	/* get room to send line(s) */
		stp->st_state |= ST_TABWAIT;
		noenable(stp->st_rq);
		return 0;
	}

	if (doecho > 0)
		(void)st_echo(stp, stp->st_retype, (mblk_t*)0);
	if (doecho >= 0)
		(void)st_estuff(stp, '\n', (mblk_t*)0);
	stp->st_bcol = 0;
	stp->st_xcol = 0;


	for (bp = stp->st_lmsg; bp; bp = bp->b_cont) {	/* echo other lines */
		cp = bp->b_rptr;
		sz = bp->b_wptr - cp;
		if (sz <= 0) {		/* 0-length is EOF */
			if (stp->st_line != LDISC_NEW)
				(void)st_estr(stp,2,ECHO_EOF);
			else {
				(void)st_echo(stp,stp->st_cc[VEOF],NULL);
				(void)st_echo_bs(stp,stp->st_cc[VEOF]);
			};
		} else {
			while (--sz)
				(void)st_echo(stp,*cp++,bp);
			if (*cp == '\n')
				(void)st_estuff(stp,'\n',bp);
			else
				(void)st_echo(stp,*cp,bp);
		}
	}

	bp = stp->st_imsg;
	if (! (bp && bp == stp->st_ibp))
		return(1);

	cp = bp->b_rptr;		/* now echo current line */
	sz = (bp->b_wptr-cp) - delta;	/* decide how many bytes */
	if (sz < 0) sz = 0;		/* must be echoed */

	while (sz--)
		st_echo(stp,*cp++,bp);

	return 1;
}


/* print process status
 */
static int				/* 0=failed */
st_process_status(stp)
register struct stty_ld *stp;
{
	char	sbuf[80];
	char	*cp;

	if (get_process_display_status(&sbuf[1],sizeof(sbuf) - 2,stp->st_wq))
		return(1);

	if (stp->st_wq->q_first		/* when things are quiet, */
	    || !st_echeck(stp,ST_MAX_LINE*5)) {	/* get room to send line(s) */
		stp->st_state |= ST_TABWAIT;
		noenable(stp->st_rq);
		return 0;
	}

	sbuf[0] = '\n';
	for (cp = &sbuf[1]; *cp ; cp++) ;
	*cp++ = '\n';

	return(st_estr(stp,(cp - sbuf),sbuf));
}


/* accept a new input message
 */
static
st_rput(rq, bp)
register queue_t *rq;
mblk_t *bp;
{
	register struct stty_ld *stp;

	stp = (struct stty_ld*)rq->q_ptr;
	ASSERT(stp->st_rq == rq);

	switch (bp->b_datap->db_type) {
	case M_IOCACK:			/* let service fnc change state	*/
	case M_FLUSH:
		putq(rq,bp);
		break;

	case M_DATA:
		if (0 != stp->st_imsg
		    || !(stp->st_state & ST_INPASS)
		    || 0 != stp->st_rrunning	/* if caught up, */
		    || 0 != rq->q_first
		    || !st_pass(rq,bp)) {	/* try to pass it on */
			putq(rq,bp);	/* else, save if for later */
		}
		break;

	default:
		putnext(rq,bp);
		break;
	}
}



/* process character input
 *	This function tries to emulate the system 5 tty line discipline,
 *	including its quirks and bugs.  Think carefully before you fix it.
 */
static mblk_t*				/* NULL or unused data */
st_cin(stp,rmp)
register struct stty_ld *stp;
register mblk_t *rmp;
{
	register u_char c;
	int echo;
	register ushort lflag = stp->st_lflag;
	register ushort iflag = stp->st_iflag;
	register u_char *cp;
	register mblk_t *ibp;
	register long lim;

#define CFULL() (stp->st_llen >= ST_MAX_LINE)

#define NEWLIM() (ST_MAX_LINE - (ibp->b_wptr - ibp->b_rptr))

#define CISTUFF(c) (*ibp->b_wptr++ = (c), lim--)
#define CKSTUFF(min) {while (lim < (min)) { if (!st_iroom(stp)) return rmp; \
	lim = NEWLIM();} }

#define CSEND() {ibp = st_rsend(stp,1); if (!ibp) return rmp; lim = NEWLIM();}

#define DO_ECHO() {if (echo != -1 && !st_echo(stp,echo,rmp)) return rmp;}

	ibp = stp->st_imsg;
	if (lflag & ICANON) {		/* create line buffer */
		if (!ibp
		    || ibp != stp->st_ibp
		    || (lim = NEWLIM()) < 0)
			CSEND()
		else if (ibp->b_wptr == ibp->b_rptr) /* Make sure to realign columns after */
			stp->st_state |= ST_BCOL_BAD;/* finishing processing the echoed input */

	} else {
		st_untime(stp);		/* reset canonical timer */
		lim = MAXBSIZE;
	}


	for (cp = rmp->b_rptr; ; rmp->b_rptr = cp) {

		while (cp >= rmp->b_wptr) {	/* next block at block end */
			register mblk_t *nbp = rmp->b_cont;
			freeb(rmp);
			rmp = nbp;
			if (!nbp)
				return 0;	/* quit at end of message */
			cp = rmp->b_rptr;
		}

		c = *cp++;		/* get the next character */
		echo = ((lflag & ECHO) ? c : -1);

		if (stp->st_line == LDISC_NEW && /* the next char turns flush off */
		    stp->st_lflag & LNEW_FLUSHO)
		        stp->st_lflag &= ~LNEW_FLUSHO;
			
		/* after literal character, for fancy line discipline,
		 *	just take the character
		 */
		if ((stp->st_state & ST_LIT)
		    && LDISC0 != stp->st_line) {
			CKSTUFF(1);
			DO_ECHO();
			CISTUFF(c);
			stp->st_state &= ~(ST_ESC|ST_LIT);
			continue;
		}

		if (c == '\n' && (iflag & INLCR))
			c = '\r';
		else if (c == '\r') {
			if (iflag & IGNCR)
				continue;
			if (iflag & ICRNL) {
				c = '\n';
				if ((lflag & ECHO) &&
				    ! (stp->st_line == LDISC_NEW &&
				       stp->st_lflag & LNEW_FLUSHO))
					echo = c;
			}
		} else if ((iflag & IUCLC)
			   && 'A' <= c && c <= 'Z') {
			c += ('a'-'A');
		}

		if ((lflag & ISIG) && 
		    (INTR_CHAR(c, stp) || QUIT_CHAR(c, stp) || 
		     SUSPEND_CHAR(c, stp)) && 
		    (_posix_vdisable != 0 || c != CDEL)) {
			stp->st_state &= ~(ST_ESC|ST_LIT);
			if (!(lflag & NOFLSH)) {
				st_rflush(stp,FLUSHRW,stp->st_rq);
				st_wflush(stp,FLUSHRW,stp->st_wq);
				if (!putctl1(stp->st_rq, M_FLUSH,FLUSHW)
				    || !putctl1(stp->st_wq, M_FLUSH,FLUSHR)) {
					(void)bufcall(1,BPRI_HI,
						      qenable,stp->st_rq);
					return rmp;
				      }
			} else if ((lflag & ICANON) && !CFULL()) {
				CSEND();
			}
			if (stp->st_line == LDISC_NEW) {
				if (! (lflag & NOFLSH)) {
					freemsg(stp->st_emsg);
					stp->st_emsg = NULL;

					if (stp->st_state & ST_TABWAIT) {
						stp->st_state &= ~ST_TABWAIT;
						qenable(stp->st_rq);
					}
				};

				if (echo != -1 && ! st_echeck(stp,
					(st_types[c] == CT ? 2 : 1)))
					return(rmp);
			};
			if (!putctl1(stp->st_rq->q_next,
			     (c == stp->st_cc[V_DSUSP] ? M_SIG : M_PCSIG),
			     (c == stp->st_cc[VINTR] ? SIGINT : 
			      (c == stp->st_cc[VQUIT] ? SIGQUIT : SIGTSTP)))) {
				(void)bufcall(1,BPRI_HI, qenable,stp->st_rq);
				return rmp;
			}
			if (stp->st_line == LDISC_NEW && echo != -1) {
				DO_ECHO();
			};
			if (!(lflag & NOFLSH)) {
				freemsg(rmp);
				return 0;
			}
			continue;
		}

		if (lflag & ICANON) {
			sysinfo.canch++;
			if (EOL_CHAR(c, stp) &&
			    (_posix_vdisable != 0 || c != CDEL)) {
				if (echo != -1 ||
				    ((lflag & ECHONL) && c == '\n')) 
					if (! st_echeck(stp,
						(st_types[c] == CT ? 2 : 1)))
						return(rmp);
				if (stp->st_state & ST_LIT) {
					CKSTUFF(2);
					CISTUFF(CESC);
				} else {
					CKSTUFF(1);
				}
				stp->st_state &= ~(ST_ESC|ST_LIT);
				if (CFULL())
					return(rmp);
				if (echo != -1
				    || ((lflag & ECHONL) && c == '\n')) {
					if (st_types[c] == CT) {
						DO_ECHO();
					} else if (!st_estuff(stp,c,rmp))
						return rmp;
				}
				CISTUFF(c);
				{
				    ibp = st_rsend(stp,1);
				    if (!ibp) {
					rmp->b_rptr++;
					return rmp;
				    }
				    lim = NEWLIM();
				}
				continue;
			    }

			if (!(stp->st_state & (ST_ESC|ST_LIT))) {
				if (c == CESC && (lflag & XCASE)) {
					DO_ECHO();
					stp->st_state |= ST_ESC;
					continue;
				}

				if ((c == stp->st_lit) &&
					!(lflag & POSIX_NOIEXTEN) &&
				    (_posix_vdisable != 0 || c != CDEL)) {
					if (LDISC0 == stp->st_line) {
						DO_ECHO();
						stp->st_state |= ST_LIT;
					} else {
						if (echo != -1 &&
						    ! st_echeck(stp,4))
						  	return(rmp);
	/* echo only the ^ of the ^V combo */	c = '^';
						st_echo(stp,c,NULL);
						stp->st_state |= ST_LIT;
						st_echo_bs(stp,c);
					}
					continue;
				}

				if ((c == stp->st_cc[VERASE]) &&
				    (_posix_vdisable != 0 || c != CDEL)) {
					if (!st_bs(stp,0, ECHOE))
						return rmp;
					continue;
				}

				if ((c == stp->st_cc[VKILL]) &&
				    (_posix_vdisable != 0 || c != CDEL)) {
					if (!st_del_line(stp,c))
						return rmp;
					continue;
				}

				if ((c == stp->st_cc[VEOF]) &&
				    (_posix_vdisable != 0 || c != CDEL)) {
					register mblk_t *bp;
					if (CFULL())
						return(rmp);
					if (stp->st_line != LDISC0 &&
					    echo != -1 &&
					    ! st_echeck(stp,4))
						return(rmp);
					if (stp->st_line == LDISC_NEW &&
					    ibp->b_wptr > ibp->b_rptr) {
						ibp = st_rsend(stp,1);
						lim = (ibp ? NEWLIM() : 0);
						/* in new mode, EOF signalled
						 * by a line not properly
						 * terminated
						 */
					} else {
					    CSEND();
					    bp = str_allocb(1,stp->st_rq,
							    BPRI_LO);
					    if (!bp)
						return rmp;
					    str_conmsg(&stp->st_lmsg,
						   	&stp->st_lbp,
						   	bp);
					};
					if (echo != -1)
					    switch(stp->st_line) {
					    case LDISC0:
						break;
					    case LDISC_NEW:
						(void)st_echo(stp,c,NULL);
						(void)st_echo_bs(stp,c);
						break;
					    default:
						(void)st_estr(stp,4,ECHO_EOF);
						break;
					}
					if (!ibp) {
						rmp->b_rptr++;
						return(rmp);
					};
					continue;
				}

				if (LDISC0 != stp->st_line) {
					if ((c == stp->st_werase) &&
					    !(lflag & POSIX_NOIEXTEN) &&
					    (_posix_vdisable != 0 ||
					     c != CDEL)) {
						if (!st_del_word(stp))
							return rmp;
						continue;
					}

					if ((c == stp->st_retype) &&
					    !(lflag & POSIX_NOIEXTEN) &&
					    (_posix_vdisable != 0 ||
					     c != CDEL)) {
						if (!st_retype_line(stp,0,1))
							return rmp;
						continue;
					}

					if ((c == stp->st_flushc) &&
					    !(lflag & POSIX_NOIEXTEN) &&
					    (_posix_vdisable != 0 ||
					     c != CDEL)) {
						if (stp->st_lflag & 
							LNEW_FLUSHO) {
						     stp->st_lflag &= 
							~LNEW_FLUSHO;
						     continue;
						};
						if (stp->st_line == LDISC_NEW &&
						    echo != -1 &&
						    ! st_echeck(stp,2))
							return(rmp);
						if (!putctl1(stp->st_rq,
							     M_FLUSH,
							     FLUSHW)) {
							(void)bufcall(1,
							      BPRI_HI,
							      qenable,
							      stp->st_rq);
							return rmp;
						}
						if (stp->st_line == LDISC_NEW) {
							DO_ECHO();
						        stp->st_lflag |=
								LNEW_FLUSHO;
						};
						continue;
					}

					if (stp->st_line == LDISC_NEW &&
					    !(lflag & POSIX_NOIEXTEN) &&
					    (c == stp->st_cc[V_STATUS]) &&
					    (_posix_vdisable != 0 ||
					     c != CDEL)) {
						if (!st_process_status(stp))
							return rmp;
						continue;
					}
				}
				CKSTUFF(1);
				DO_ECHO();


			} else {	/* previous was 'literal' character */
				register u_char bslash = CESC;

				CKSTUFF(2);
				if (! (stp->st_line == LDISC0 &&
				       c == stp->st_cc[VEOF] &&
				       (_posix_vdisable != 0 ||
					stp->st_cc[VEOF] != CDEL))) {
					DO_ECHO();	/* do not echo ^D */
							/* as itself */
				}
				if ((c == stp->st_cc[VERASE]
				    || c == stp->st_cc[VKILL]
				    || c == stp->st_cc[VEOF]) &&
				      (_posix_vdisable != 0 ||
				       c != CDEL)) {
					bslash = 0;

				} else if ((lflag & XCASE)
					   && (stp->st_state & ST_ESC)) {
					bslash = 0;
					switch (c) {
					case CESC: break;
					case '\'': c = '`'; break;
					case '(': c = '{'; break;
					case '!': c = '|'; break;
					case ')': c = '}'; break;
					case '^': c = '~'; break;
					default:
						if ('a' <= c && c <= 'z')
							c +=('A'-'a');
					}
				}

				stp->st_state &= ~(ST_ESC|ST_LIT);

				if (LDISC0 == stp->st_line
				    && bslash == CESC) {
					if (CESC == c)
						stp->st_state |= ST_LIT;
					else
						CISTUFF(bslash);
				}
			}
			CISTUFF(c);


		} else {		/* non-canonical input */
			if (echo != -1
			    && !st_estuff(stp,echo,rmp))
				return rmp;
			if (!st_nonstuff(stp,c,rmp)) {	/* pass it on */
				if (echo != -1)	/* unecho if failed */
					--stp->st_ebp->b_wptr;
				return rmp;
			}
		}
	}
}



/* work on the input queue
 */
static
st_rsrv(rq)
register queue_t *rq;			/* our read queue */
{
	register struct stty_ld *stp;
	register mblk_t *rmp;
	register struct iocblk *iocp;

	stp = (struct stty_ld*)rq->q_ptr;
	ASSERT(stp->st_rq == rq);

	st_sendold(stp);		/* send previous canonical lines */

	enableok(rq);
	for (;;) {
		if (!(stp->st_state & ST_ISTTY)) {	/* tell head */
			register mblk_t *bp;
			register struct stroptions *sop;
			int	toggleflags;

			if (stp->st_iflag & IBLKMD) {
				stp->st_iflag = IBLKMD;
				stp->st_lflag = 0;
				stp->st_cc[VMIN] = 1;
				stp->st_cc[VTIME] = 1;
				stp->st_state &= ~(ST_ESC | ST_LIT);
			}

			/* notice how raw & fast input now is */
			stp->st_state &= ~(ST_INRAW|ST_INPASS);
			if (!(stp->st_iflag & (INLCR|IGNCR|ICRNL|IUCLC))
			    && !(stp->st_lflag & (ICANON|ISIG|ECHO))) {
				stp->st_state |= ST_INRAW;

				if (stp->st_cc[VMIN] <= 1)
					stp->st_state |= ST_INPASS;
			}

			bp = str_allocb(sizeof(struct stroptions),
					rq,BPRI_LO);
			if (!bp)
				break;
			bp->b_datap->db_type = M_SETOPTS;
			sop = (struct stroptions*)bp->b_rptr;
			bp->b_wptr += sizeof(struct stroptions);
			sop->so_flags = (SO_READOPT|SO_HIWAT|SO_LOWAT
					 |SO_ISTTY|SO_VTIME);
			sop->so_hiwat = 1;
			sop->so_lowat = 0;
			if (stp->st_lflag & ICANON) {	/* in cooked mode, */
				sop->so_readopt = RMSGN;	/* keep EOL */
				sop->so_vtime = 0;
			} else {
				sop->so_readopt = RNORM;
				if (0 != stp->st_cc[VMIN]) {
					sop->so_vtime = 0;
				} else {
					sop->so_vtime = (stp->st_cc[VTIME]==0
							 ? -1
							 : stp->st_cc[VTIME]);
				}
			}
			toggleflags = 0;
			if (stp->st_line == LDISC1 ||
			    stp->st_line == LDISC_NEW)
				toggleflags |= SO_JOBCTRL;
			if ((stp->st_line == LDISC1 ||
			     stp->st_line == LDISC_NEW) &&
			    (stp->st_termio.c_lflag & TOSTOP))
				toggleflags |= SO_TOSTOP;
			if (stp->st_line == LDISC_NEW &&
			    (stp->st_termio.c_lflag & LNEW_PENDIN))
				toggleflags |= SO_PENDIN;
			sop->so_flags |= toggleflags;
			putnext(rq, bp);

			toggleflags ^= (SO_JOBCTRL | SO_TOSTOP | SO_PENDIN);
			if (toggleflags)
				if (! set_str_control(rq,toggleflags,0))
					break;

			stp->st_state |= ST_ISTTY;

			if (!(stp->st_lflag & ICANON)) {
				rmp = stp->st_imsg;
				if (rmp != NULL
				    && rmp->b_rptr >= rmp->b_wptr) {
					freemsg(rmp);
					stp->st_imsg = 0;
				}
				if (stp->st_lmsg != NULL) {
					str_conmsg(&stp->st_imsg,
						   &stp->st_ibp,
						   stp->st_lmsg);
					stp->st_lmsg = 0;
					stp->st_llen = 0;
				}
			}
		}


		stp->st_rrunning = 1;
		rmp = getq(rq);		/* get another message */
		if (!rmp)
			break;

		switch (rmp->b_datap->db_type) {
		case M_IOCACK:
			iocp = (struct iocblk*)rmp->b_rptr;
			switch (iocp->ioc_cmd) {
			case TCSETA:
			case TCSETAW:
			case TCSETAF:
			  	{
				    int old_line = stp->st_line;
				    int old_lit = stp->st_lit;

				    st_copy_pendin(stp,rmp);
				    stp->st_termio = *STERMIO(rmp);
				    if (old_line == LDISC0 &&
				        old_lit == CESC &&
					(stp->st_line == LDISC1 ||
					 stp->st_line == LDISC_NEW) &&
					stp->st_lit == CESC)
					stp->st_lit = CLNEXT;
				    else if ((old_line == LDISC1 ||
					      old_line == LDISC_NEW) &&
					     old_lit == CLNEXT &&
					     stp->st_line == LDISC0 &&
					     stp->st_lit == CLNEXT)
					stp->st_lit = CESC;
				    stp->st_state &= ~ST_ISTTY;
				    if (! (stp->st_lflag & ICANON))
				  	stp->st_state &= ~(ST_ESC | ST_LIT);
				};
				break;

			case TCGETA:
				if (iocp->ioc_count >= sizeof(struct termio)) {
					st_copy_pendin(stp,rmp);
					STERMIO(rmp)->c_cc[V_LNEXT] =
						stp->st_lit;
				};
				break;

			case FIONREAD:
				{
#define rmp_cnt (*(int*)rmp->b_cont->b_rptr)
					queue_t *nq;

					if (stp->st_lflag & ICANON) {
						rmp_cnt = 0;
					} else {
						fion(rq,rmp,rmp_cnt);
					};
					for (nq = rq;
					     (nq = nq->q_next) != NULL; )
						fion(nq,rmp,rmp_cnt);
#undef rmp_cnt
				};
				break;

			case TCBLKMD:
				stp->st_iflag = IBLKMD;
				stp->st_state &= ~ST_ISTTY;
				break;
			}
			putnext(rq,rmp);
			break;


		case M_FLUSH:
			st_rflush(stp,*rmp->b_rptr,rq);
			putnext(rq,rmp);
			break;

		case M_DATA:		/* process data messages */
			if (stp->st_state & ST_INRAW) {	/* in raw mode, */
				str_conmsg(&stp->st_imsg,	/* add new */
					   &stp->st_ibp, rmp); /* to input */
			} else {
				rmp = st_cin(stp, rmp);
				if (rmp != NULL) {
					putbq(rq, rmp);
					goto for_exit;
				}
			}
			break;

		case M_PCSIG:
			if (*rmp->b_rptr == SIGIO &&
			    stp->st_state & ST_SIGIO) {
				/* discard message since we are cooking */
				freemsg(rmp);
				break;
			}
			/* fall through to pass on the message */
		default:
			putnext(rq,rmp);
			break;
		}
	}
for_exit:;
	stp->st_rrunning = 0;

	if (stp->st_lmsg) {
		st_sendold(stp);

	} else if (!(stp->st_lflag & ICANON)
		   && 0 != (rmp = stp->st_imsg)) {
/* Here, we have a non-empty buffer/msg waiting to go upstream.
 * So, ship it up stream if it satisfyies the minimum size requirement,
 *	or if it has been waiting long enough.
 * If the stream is constipated, then we may as well go to sleep.  However,
 * we cannot disable ourself if we are not in a very dumb mode, since we
 * must watch for interrupt characters.
 */
		register ushort n = stp->st_cc[VMIN];
		if (n <= 1
		    || (stp->st_state & ST_TIMED)
		    || msgdsize(rmp) >= n) {
			if (!canput(rq->q_next)) {
				if (stp->st_state & ST_INPASS)
					noenable(rq);
			} else {
				putnext(rq,rmp);
				stp->st_imsg = 0;
				st_untime(stp);
			}

		} else {
			n = stp->st_cc[VTIME];
			if (0 != n
			    && !(stp->st_state & ST_TIMING)) {
				stp->st_tid = timeout(st_tock,
						      (caddr_t)stp,
						      (n * (short)(HZ/10)));
				stp->st_state |= ST_TIMING;
			}
		}
	}
}


set_str_control(rq,opt,is_set)
	register queue_t *rq;
	int	opt;
	int	is_set;
{
	register mblk_t *bp;
	register struct stroptions *sop;

	if (! is_set)
		opt |= SO_NOT;

	bp = str_allocb(sizeof(struct stroptions),
				rq,BPRI_LO);
	if (!bp)
		return(0);

	bp->b_datap->db_type = M_SETOPTS;
	sop = (struct stroptions*)bp->b_rptr;
	bp->b_wptr += sizeof(struct stroptions);
	sop->so_flags = opt;

	putnext(rq, bp);
	return(1);
}

