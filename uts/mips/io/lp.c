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
#ident	"$Header: lp.c,v 1.3.1.4 90/05/22 18:32:00 wje Exp $"
#include "sys/types.h"
#include "sys/param.h"
#include "sys/immu.h"
#include "sys/sbd.h"
#include "sys/pcb.h"
#include "sys/signal.h"
#include "sys/user.h"
#include "sys/systm.h"
#include "sys/buf.h"
#include "sys/iobuf.h"
#include "sys/errno.h"
#include "sys/cmn_err.h"
#include "sys/debug.h"
#include "sys/edt.h"

#include "sys/bsd_glue.h"
#include "sys/iop.h"
#include "sys/lpreg.h"

char *ascii_lp_stat[] = {
	"not responding",
	"not ready",
	"not cabled correctly", 
	"not selected (offline)",
	"out of paper",
	"bad command",
	"device busy: command rejected",
	"cannot initialize"
};

extern struct lp_unitinfo lpunitinfo[];
struct lp_unitinfo *lpalive[NLP];

int	lpstrategy();

#define LP_TIMEOUT	(2*60)		/* 2 min timeout, 4 before message */

/* Check that port exists and is in working order */
lpedtinit(e)
register struct edt *e;
{
	register int			unit = e->e_intr_info->v_unit;
	register struct lp_device	*lpaddr;
	register struct lp_unitinfo	*ui;
	int				lpintr();
	long 				l = 0, status;
	extern int showconfig;
    
	if (showconfig)
		cmn_err (CE_CONT, "    IOP: lineprinter              ");

	if ((lpaddr = (struct lp_device *)iop_alloc(PPIOCB,
	    sizeof(*lpaddr))) == NULL)
		goto noconfig;

	/* give the init cmd , this will generate an interrupt */
	lpaddr->lp_cmd = LP_INIT
#ifdef notdef
	| LP_IE
#endif
	;
	wbflush();
	iop_poke(PPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(lpaddr), 0);
	while (l++ < LP_TIMEOUT * 100) {
		if (iop_wait(PPIOCB, IOPB_NOWAIT, &status, 0) == 0) {
			if (lpaddr->lp_status)
				goto noconfig;
			iop_clear(PPIOCB);
			ui = lpalive[unit] = &lpunitinfo[unit];
			ui->ui_device = lpaddr;
			e->e_intr_info->v_vintr = lpintr;
			if (showconfig)
	  			cmn_err (CE_CONT, "available\n");
			return ((int)lpaddr);
		}
		DELAY(400);
	} 
/*cmn_err (CE_CONT, "timedout\n");	/**/
noconfig:
/*cmn_err (CE_CONT, "stat=%x\n", lpaddr->lp_status);	/**/
	iop_clear(PPIOCB);
	if (showconfig)
	  	cmn_err (CE_CONT, "\n");
	return (0);
}

lpopen(dev)
dev_t dev;
{
	register int			lp = LPUNIT(dev);
	register struct lp_unitinfo 	*ui;
	register struct lp_device 	*lpaddr;

/*cmn_err(CE_CONT, "open\n");	/**/
	if (lp >= NLP || (ui = lpalive[lp]) == 0 || ui->ui_state & LP_OPEN)
		return (u.u_error = ENXIO);
/*cmn_err(CE_CONT, "ok\n");	/**/
	ui->ui_state |= LP_OPEN;
	if ((ui->ui_canon = LPCANON(dev)) != LP_CANON_RAW) {
/*cmn_err(CE_CONT, "notraw\n");	/**/
		if ((ui->ui_memp[0] = (char *)
		    sptalloc (1, PG_N|PG_G|PG_M|PG_VR|PG_LOCK, 0, 0)) == 0) {
			cmn_err(CE_CONT, "lp%d: cannot alloc mem\n", lp);
			u.u_error = ENXIO;
			return;
		}
		ui->ui_memp[1] = ui->ui_memp[0] + (NBPC / 2);
		ui->ui_memn = 0;
		ui->ui_outp = ui->ui_memp[0];
		ui->ui_outn = 0;
		ui->ui_logcol = ui->ui_physcol = 0;
		ui->ui_physline = 0;
		ui->ui_maxcol = LPMAXCOL(dev) ? LPMAXCOL(dev) : LP_MAXCOL;
	}
/*cmn_err(CE_CONT, "opend\n");	/**/
	return (0);
}

lpclose(dev)
dev_t dev;
{
	register int lp = LPUNIT(dev);
	register struct lp_unitinfo *ui = lpalive[lp];

/*cmn_err(CE_CONT, "close\n");	/**/
	if (ui->ui_memp[0]) {
		sptfree(ui->ui_memp[0], 1, 0);
		ui->ui_memp[0] = 0;
	}
	ui->ui_state &= ~LP_OPEN;
	return(0);
}

lpwrite(dev)
register dev_t dev;
{
/*cmn_err(CE_CONT, "write\n");	/**/
	physio(lpstrategy, 0, dev, B_WRITE);
}

lpstrategy(bp)
register struct buf	*bp;
{
	register struct lp_unitinfo 	*ui = lpalive[LPUNIT(bp->b_dev)];
	register char 			*p;
	register u_short 		n  = bp->b_bcount;
	register u_short		part;
	int				s;

/*cmn_err(CE_CONT, "strat\n");	/**/
	if (n == 0)
		return;
	iomap(bp);
	p  = bp->b_dmaaddr;
	switch (ui->ui_canon) {
	    case LP_CANON_RAW:
/*cmn_err(CE_CONT, "raw\n");	/**/
		while (n) {
			part = (n > LPMAXIO) ? LPMAXIO : n;
/*cmn_err(CE_CONT, "n=%x, p=%x, part=%x\n", n, p, part);	/**/
			lpoutput(ui, (u_long)p, part);
			if (u.u_error)
				break;
			n -= part;
			p += part;
/*cmn_err(CE_CONT, "n=%x, p=%x, part=%x\n", n, p, part);	/**/
		}
		break;
	    case LP_CANON_CAP:
/*cmn_err(CE_CONT, "caps\n");	/**/
		do {
			lp_cap(ui, *p++);
			if (u.u_error)
				break;
		} while (--n);
		break;
	    default:
/*cmn_err(CE_CONT, "default\n");	/**/
		do {
			lpcanon(ui, *p++);
			if (u.u_error)
				break;
		} while (--n);
		break;
	}
	if (ui->ui_outn) {
		lpoutput(ui, (u_long)ui->ui_memp[ui->ui_memn], ui->ui_outn);
		ui->ui_outp = ui->ui_memp[ui->ui_memn];
		ui->ui_outn = 0;
	}
	s = splbio();
	while (ui->ui_state & LP_ACTIVE) {
/*cmn_err(CE_CONT, "iowait\n");	/**/
		if (sleep((caddr_t )ui, PRIBIO|PCATCH))
			lpabort(ui);
	}
	splx(s);
/*cmn_err(CE_CONT, "seterr\n");	/**/
	if (ui->ui_state & LP_CMD_FAILED)
		u.u_error = EIO;
/*cmn_err(CE_CONT, "unmap\n");	/**/
	iounmap(bp);
/*cmn_err(CE_CONT, "done\n");	/**/
	iodone(bp);
/*cmn_err(CE_CONT, "bye\n");	/**/
	return (0);
}

lp_cap(ui, c)
register struct lp_unitinfo 	*ui;
register char 			c;
{
	register doback = 0;

/*cmn_err(CE_CONT, "cap\n");	/**/
	switch (c) {
		case '{': c = '(';  doback = 1; break;
		case '}': c = ')';  doback = 1; break;
		case '`': c = '\''; doback = 1; break;
		case '|': c = '!';  doback = 1; break;
		case '~': c = '^';  doback = 1; break;
		default: if (c>='a' && c<='z') c += 'A'-'a'; 
			break;
	}
	if (doback) {
		lpcanon(ui, '-');
		lpcanon(ui, '\b');
	}
	lpcanon(ui, c);
}

lpcanon(ui, c)
register struct lp_unitinfo 	*ui;
register char 			c;
{
	register int logcol, physcol;

/*cmn_err(CE_CONT, "canon(%x)\n", c);	/**/
	logcol = ui->ui_logcol;
	physcol = ui->ui_physcol;

	switch(c) {
	    case ' ':
		logcol++;
		break;

	    case '\t':
		logcol = (logcol+8) & ~7;
		break;

	    case '\f':
		if (ui->ui_physline == 0 && physcol == 0)
			break;			/* fall into ... */

	    case '\n':
		lpputbuf(ui, c);
		if (c == '\f') {
			ui->ui_physline = 0;
			physcol = 0;
		} else
			ui->ui_physline++;
		/* fall into ... */

	    case '\r':
		logcol = 0;
		break;

	    case '\b':
		if (logcol > 0)
			logcol--;
		break;

	    default:
		if (logcol < physcol) {
			lpputbuf(ui, '\r');
			physcol = 0;
		}
		if (logcol < ui->ui_maxcol) {
			while (logcol > physcol) {
				lpputbuf(ui, ' ');
				physcol++;
			}
			lpputbuf(ui, c);
			physcol++;
		}
		logcol++;
	}
	if (logcol > 1000)			/* ignore long lines  */
		logcol = 1000;
	ui->ui_logcol = logcol;
	ui->ui_physcol = physcol;
}

lpputbuf(ui, c)
register struct lp_unitinfo 	*ui;
register char 			c;
{
/*cmn_err(CE_CONT, "putbuf\n");	/**/
/*cmn_err(CE_CONT, "outp=%x\n", ui->ui_outp);	/**/
	*ui->ui_outp++ = c;
/*cmn_err(CE_CONT, "outn=%x\n", ui->ui_outn);	/**/
	ui->ui_outn++;
	if (ui->ui_outn >= LPMAXIO) { /* actually should never be >  */
/*cmn_err(CE_CONT, "outing\n");	/**/
		lpoutput(ui, (u_long)ui->ui_memp[ui->ui_memn], ui->ui_outn);
		ui->ui_memn = 1 - ui->ui_memn;
		ui->ui_outp = ui->ui_memp[ui->ui_memn];
		ui->ui_outn = 0;
	}
/*cmn_err(CE_CONT, "put\n");	/**/
}

lpoutput(ui, p, n)
register struct lp_unitinfo 	*ui;
register u_long			p;
register u_short		n;
{
	register struct lp_device	*lpaddr = ui->ui_device;
	int				s;

	s = splbio();
	while (ui->ui_state & LP_ACTIVE) {
/*cmn_err(CE_CONT, "output\n");	/**/
		if (sleep((caddr_t )ui, PRIBIO|PCATCH)) {
			lpabort(ui);
			return;
		}
	}
	splx(s);
	ui->ui_state |= LP_ACTIVE;
	lpaddr->lp_cmd = LP_SEND | LP_IE;
	lpaddr->lp_count = n;
	iop_ptes_short(lpaddr->lp_page, p, n);
	lpaddr->lp_offset = (u_short )p & PGOFSET;
	lpaddr->lp_timeout = LP_TIMEOUT;
	wbflush();
/*cmn_err(CE_CONT, "cmd=%x cnt=%x pg0=%x pg1=%x off=%x tmo=%x stat=%x\n", lpaddr->lp_cmd, lpaddr->lp_count, lpaddr->lp_page[0], lpaddr->lp_page[1], lpaddr->lp_offset, lpaddr->lp_timeout, lpaddr->lp_status);	/**/
	iop_poke(PPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(lpaddr), 0);
	return;
}

lpabort(ui)
register struct lp_unitinfo 	*ui;
{
	register struct lp_device	*lpaddr = ui->ui_device;

	u.u_error = EINTR;
	ui->ui_state |= LP_ABORTED;
	ui->ui_state &= ~LP_ACTIVE;
	lpaddr->lp_cmd = LP_ABORT | LP_IE;
	lpaddr->lp_timeout = LP_TIMEOUT;
	wbflush();
	iop_poke(PPIOCB, IOPB_SPIN, K1_TO_IOPB_OFFSET(lpaddr), 0);
}

lpintr(lp)
int lp;
{
	register struct lp_unitinfo	*ui;
	register u_char			busy, errs;
	register u_char			shift = 0;

	if (lp >= NLP) {
		cmn_err(CE_CONT,"lp%d: interrupt from nonexistent device\n",lp);
		return;
	}
	ui = lpalive[lp];
	errs = ui->ui_device->lp_status & 0xFF;
	busy = ui->ui_device->lp_status >> 8;
/*cmn_err(CE_CONT, "intr: errs=%x\n", errs);	/**/
/*cmn_err(CE_CONT, "intr: busy=%x\n", busy);	/**/
	iop_clear(PPIOCB);
/*cmn_err(CE_CONT, "cleardone\n");	/**/
	if (ui->ui_state & LP_ABORTED) {
		ui->ui_state &= ~LP_ABORTED;
		return;
	}
	if ((ui->ui_state & LP_ACTIVE) == 0) {
		cmn_err(CE_CONT,"lp%d: stray interrupt\n",lp);
		return;
	}
	if (errs & (LP_STAT_BADCMD_ERR | LP_STAT_BUSY_ERR))
		ui->ui_state |= LP_CMD_FAILED;
	while (errs) {
/*cmn_err(CE_CONT, "errs=%x\n", errs);	/**/
		if (errs & 0x1)
			cmn_err(CE_CONT,"lp%d: %s\n", lp, ascii_lp_stat[shift]);
		errs >>= 1;
		shift++;
	}
/*cmn_err(CE_CONT, "busy=%x\n", busy);	/**/
	if (!busy) {
/*cmn_err(CE_CONT, "wake\n");	/**/
		
		ui->ui_state &= ~LP_ACTIVE;
		wakeup((caddr_t)ui);
	}
	return;
}
