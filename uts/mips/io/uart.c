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
#ident	"$Header: uart.c,v 1.17.1.4.1.1.1.2 90/11/12 17:52:29 beacker Exp $"
/*
 * $Header: uart.c,v 1.17.1.4.1.1.1.2 90/11/12 17:52:29 beacker Exp $
 */
/*
 * Uart driver for IOP
 * Project: Jupiter Workstation
 * Date: 09-Feb-1988
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

#include "sys/cpu_board.h"
#include "sys/systm.h"
#include "sys/fs/s5dir.h"
#include "sys/sysinfo.h"
#include "sys/debug.h"
#include "sys/ss.h"

#ifndef	3_0	/* 4_0 */
#include "bsd43/sys/ioctl.h"
#endif

#include "sys/iop.h"
#include "sys/async.h"		/* definitions for uart's iocb */
#include "sys/uartreg.h"
#include "sys/uart_ioctl.h"

/*
 * bits 31-2 indicate debug level. bit 0 indicates port. bit 1 turns
 * basic debugging on.
 */
#ifdef	3_0
int	dbg_uart;
#else	/* 4_0 */
extern int dbg_uart;
#endif
extern int showconfig;
#define SET_CONSOLE_LINE if (console_line == -1 ) set_console_line();
int console_line = -1;

#define DebugUart(x, lvl)  \
  ((dbg_uart & 2) && ((dbg_uart & 1) == (x)) && ((dbg_uart >> 2) & lvl))

/*
 * 02-Sept-1988 
 * Currently we've been having problems with silo mode.  At this point
 * I believe that the problem is on my side of the fence.
 */
/* #define DISABLE_SILO /* */
#ifdef	3_0
int uart_silo = 1;
#else	/* 4_0 */
extern int uart_silo;
#endif

extern int _posix_vdisable;

Convert_Table_t baud_rate_table[] = {
    { ASYNC_B50,	B50 },
    { ASYNC_B75,	B75 },
    { ASYNC_B110,	B110 },
    { ASYNC_B134,	B134 },
    { ASYNC_B150,	B150 },
    { ASYNC_B200,	B200 },
    { ASYNC_B300,	B300 },
    { ASYNC_B600,	B600 },
    { ASYNC_B1200,	B1200 },
    { ASYNC_B1800,	B1800 },
    { ASYNC_B2400,	B2400 },
    { ASYNC_B4800,	B4800 },
    { ASYNC_B9600,	B9600 },
    { ASYNC_B19200,	B19200 },
    { ASYNC_B38400,	B38400 },
    { -1, -1 },		/* End-of-table */
};

Convert_Table_t data_size_table[] = {
    { ASYNC_DATA5,	CS5 },
    { ASYNC_DATA6,	CS6 },
    { ASYNC_DATA8,	CS7 },
    { ASYNC_DATA8,	CS8 },
    { -1, -1 },
};

static struct module_info dum_info = {
    STRID_IOP_UART,			/* module ID */
    "uart",				/* module name */
    0,					/* minimum packet size */
    1024,				/* maximum packet size */
    128,				/* high water mark */
    16,					/* low water mark */
};

#define	UART_BMAX 	1
#define UART_LMAX	NUART
#define	UART_OUTCMAX	(UART_BUF_SIZE/sizeof(struct tty_atom))

#define OTHER(up) 	(((up)->uart_dpl->ss_line & 1) ? ((up)-1) : ((up)+1))
#define SECONDPORT(up)	(up->uart_dpl->ss_line & 1)

#define	DPUNIT(m)	(((m)&0x7E)>>1)
#define	DPLINE(m)	((m)&0x01)

int 	uart_act(), uart_zap(), uart_outc(), uart_setline();
int	uart_modem_control(), uart_driver_control();
static int	uartopen(), uart_lost_xmit();

#ifdef	3_0
struct ss_devdep_info dudd =
	{ "UART", SS_TTY, UART_BMAX, UART_LMAX, UART_OUTCMAX,
	  uart_act, uart_zap, uart_outc, uart_setline, ss_null, ss_null,
	};
#else	/* 4_0 */
struct ss_devdep_info dudd =
	{ "UART", SS_TTY, UART_BMAX, UART_LMAX, UART_OUTCMAX,
	  uart_act, uart_zap, uart_outc, uart_setline, uart_modem_control,
	  uart_driver_control
	};
#endif

#ifdef	3_0
struct ss_struct duboard[UART_BMAX];
#else	/* 4_0 */
struct ss_struct duboard[UART_BMAX];
struct ss_line   dulines[UART_BMAX][UART_LMAX];
#endif


static struct qinit uart_rinit = {
    NULL, ss_rsrv, uartopen, ss_close, NULL, &dum_info, NULL
};

static struct qinit uart_winit = {
    ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab uartinfo = {&uart_rinit, &uart_winit, NULL, NULL};

Uart_t uart_control_blocks[NUART];

#define USE_STATIC_MEMORY
#ifdef USE_STATIC_MEMORY
char uart_silo_buf[NBPP * 3];
#endif
unsigned uartto[2] = { 0,0 };


/*
 * Initialize uart driver.
 */
extern int uart_dbx;
uartedtinit(edt)
    struct edt *edt;
{
    register Uart_t *up;
    register int line;
    register struct async *a;
    struct async *iop_alloc();
    int stat;
    
    line = edt->e_intr_info->v_unit;
    uart_silo = 1;
    if ( uart_dbx ) {
	if ( line == 1 ) {
		printf("NOT USING UNIT 1 ********************\n");
		return;
	}
    }

    if (showconfig)
	cmn_err(CE_CONT, "uartedtinit <line = %d>: available\n",line);

    up = &uart_control_blocks[line];
    up->uart_line = line;
#ifdef	3_0
    up->uart_dpl = &(duboard[DPUNIT(line)].ss_line[DPLINE(line)]);
#else	/* 4_0 */
    up->uart_dpl = &dulines[DPUNIT(line)][DPLINE(line)];
#endif
    up->uart_dpl->ss_line = line; 
    up->uart_dpl->pss = duboard + DPUNIT(line); 
    up->uart_dpl->ss_llocal = (char *)up;
    up->uart_dpl->pss->ss_nlines = UART_LMAX;
    up->uart_dpl->pss->ss_devdep = &dudd;
    up->uart_dpl->ss_lenable = 1;		/* set enable flag */

    if ((a = iop_alloc (UARTIOCB + line, sizeof (struct async))) == 0)
      cmn_err (CE_PANIC, "uartinit: iop_alloc failed\n");
    
#ifdef USE_STATIC_MEMORY
    up->xmit_buffer = (caddr_t)K0_TO_K1(ctob(btoc(&uart_silo_buf[0] + (NBPP * line))));
#else
    /*
     * Each uart has one page of memory.  Half is used by the
     * receive side and the transmit takes the other half.
     * Set the following pte bit:
     *   NoCache := The uart never reads the same data twice.
     *   Modify  := We know the driver will write to the xmit buffer.
     *              The keyboard driver might find it useful to leave
     *              this bit off for debugging.
     *   Valid   := Obvious
     *   Global  := Everyone can access it.  jimp would like to have
     *              a shared memory interface for the keyboard and mouse
     *              in which the user could directly access the data.
     *   Lock    := Don't swap these pages out.
     */
    if ((up->xmit_buffer = (caddr_t)
	 sptalloc (1, PG_N|PG_M|PG_VR|PG_G|PG_LOCK, 0, 0)) == 0)
      return;
#endif
    
    /*
     * Divide the buffer in half.  Do not use UART_BUF_SIZE.  Currently
     * UART_BUF_SIZE is ((NBPP / 2) - sizeof(long)).  A magic cookie is
     * placed at the end of the buffer to see if someone is trashing
     * main memory.
     */
    up->rcv_buffer = up->xmit_buffer + (NBPP / 2);
    *((long *)(up->xmit_buffer + UART_BUF_SIZE)) = UART_MAGIC;
    *((long *)(up->rcv_buffer + UART_BUF_SIZE)) = UART_MAGIC;
    
    /*
     * Once the value is stored here it can only be changed if the driver
     * is relocating the map.  Sable assumes that async_maps[0] will always
     * contain a page frame number and that there's only one page.
     * If this changes you must also change sable.
     */
#ifdef USE_STATIC_MEMORY
    a->async_maps[0] = btoc(up->xmit_buffer);
#else
    a->async_maps[0] = (u_long)(kvtokptbl(up->xmit_buffer)->pgm.pg_pfn);
#endif /* USE_STATIC_MEMORY */

    a->async_nmaps = 1;
    a->async_receive.rcv_reladdr = (int)up->rcv_buffer & POFFMASK;
    a->async_receive.rcv_out = 0;
    a->async_receive.rcv_in = 0;
    a->async_receive.rcv_len = UART_BUF_SIZE;
    a->async_transmit.xmt_reladdr = 0;
    a->async_transmit.xmt_len = 0;
    up->com_blk = a;
    
    up->driver_state = UART_IS_AVAIL;
    edt->e_base = (paddr_t)up;
}


/*
 * uartopen()
 */
static int
uartopen (rq, dev, flag, sflag)
    queue_t *rq;
    dev_t dev;
    int flag;
    int sflag;
{
    register Uart_t *up;
    register int requested_state;
    register int line, s, n;
    queue_t *wq;
    ushort cflag;

    if (sflag)
	return OPENFAIL;

    if (DPUNIT(minor(dev)) >= UART_BMAX)
	return OPENFAIL;

    if((line = UART_LINE(dev)) >= NUART)
      return OPENFAIL;

    up = &uart_control_blocks[line];
    if ((up->driver_state & UART_IS_AVAIL) == 0) {
	u.u_error = EIO;
	return OPENFAIL;
    }
    
    /*
     * If we are already in the process or have complete the open
     * return an error if the requesting stream is not the same
     * one that opened us the first time.
     */
    if ( up->uart_dpl->ss_state & (SS_ISOPEN|SS_WOPEN) ){
	if( up->uart_dpl->ss_rq != rq ) {
            u.u_error = ENOSR;
            return OPENFAIL;
	}
    }
    else {
	s = spltty();
	up->uart_line = line;
#ifndef	3_0	/* 4_0 */
	cflag = IFLAGS;
#endif
	if (UART_MODEM_LINE(dev)) {
	    cflag &= ~CLOCAL;
	}
	if (!(up->uart_dpl->ss_state & (SS_ISOPEN|SS_WOPEN))) {
		up->com_blk->async_flow = 0;
	}
#ifdef	3_0
	n = ss_open(up->uart_dpl, rq, dev, flag, sflag);
#else	/* 4_0 */
	n = ss_open(up->uart_dpl, rq, dev, flag, sflag, cflag);
#endif
	splx(s);
	return(n);
    }
}

/*
 * Activate a port
 * 	This should be called only when safe from interrupt
 */
static int	uart_act_flags();

static int 
uart_act(dpl)
    struct ss_line	*dpl;
{
    return( uart_act_flags(dpl, dpl->ss_cflag));
}

static int 
uart_act_flags(dpl,cflag)
    struct ss_line	*dpl;
    ushort		cflag;
{
    register Uart_t *up;

    up = (Uart_t *)dpl->ss_llocal;

#ifndef	3_0	/* 4_0 */
    up->com_blk->async_modem_out |= ASYNC_DTR | ASYNC_RTS;
    uart_run_cmd (up, ASYNC_PARAM, IOPB_SPIN);

    dpl->ss_modem_state |= ( BSD43_TIOCM_LE  | BSD43_TIOCM_DTR 
		 	   | BSD43_TIOCM_RTS | BSD43_TIOCM_CTS);
#endif

    /* check carrier is on */
    if (up->com_blk->async_modem_in & ASYNC_DCD)
	return(1);
    else
	return(0);
}
    
/*
 * Shutdown port
 *	this should be called only when safe from interrupt
 */
uart_zap(dpl)
    struct ss_line	*dpl;
{
    register Uart_t *up, *o_up;
    register struct ss_line *o_dpl;
    register s;

    up = (Uart_t *)dpl->ss_llocal;

#ifndef	3_0	/* 4_0 */
    s = spltty();
    uart_stop ( dpl );
    splx(s);
    up->driver_state &= UART_IS_AVAIL;
    up->com_blk->async_modem_out &= ~(ASYNC_DTR | ASYNC_RTS);
    uart_run_cmd (up, ASYNC_PARAM, IOPB_SPIN);
    dpl->ss_modem_state &= ( BSD43_TIOCM_LE  | BSD43_TIOCM_DTR
			   | BSD43_TIOCM_RTS | BSD43_TIOCM_ST
			   | BSD43_TIOCM_CTS
			   | BSD43_TIOCM_SR  | BSD43_TIOCM_CD
			   | BSD43_TIOCM_RI  | BSD43_TIOCM_DSR );
#endif
}

/*
 * stop transmit
 *	this should be called only safe from interrupt
 */
uart_stop(dpl)
    register struct ss_line *dpl;
{
    register Uart_t *up;
    int stat;

    up = (Uart_t *)dpl->ss_llocal;
    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
      ;
    up->com_blk->async_command = ASYNC_STOP | up->intr_mask;
    if (iop_poke (UARTIOCB + up->uart_line, IOPB_SPIN,
		  K1_TO_IOPB_OFFSET(up->com_blk),
		  NO_SLEEP_ADDR) == -1)
      cmn_err (CE_PANIC, "uart_stop");
    
    iop_wait (UARTIOCB + up->uart_line, IOPB_SPIN, &stat, 0);

    iop_clear (UARTIOCB + up->uart_line);
    dpl->ss_state &= ~SS_BUSY;
}

/*
 * This procedure outputs "len" chars pointed to by "cp" on line "cp1
 * This procedure gets called from ss_tx 
 */
uart_outc(dpl,cs,len)
    register struct ss_line	*dpl;
    register char		*cs;
    register int		len;
{
    register Uart_t *up;
    register struct async *a;
    register struct tty_atom *atom;
    register int chars_to_xmit;
    register int s;
    register int tspl;
    int i;
    
    up = (Uart_t *)dpl->ss_llocal;
    a = (struct async *)up->com_blk;

    s = spltty(); 
    if ( up->uart_dpl->ss_state & SS_BUSY ) {  /* if no uart_tint yet */
	splx(s);
	return;
    }
    if (len) {
	if (uart_silo) {
	    chars_to_xmit = MIN(UART_BUF_SIZE/sizeof(struct tty_atom),len);
	    atom = (struct tty_atom *)up->xmit_buffer;
	}
	else {
	    chars_to_xmit = 1;
	    atom = &a->async_transmit.xmt_char;
	}
	
	for (i = 0; i < chars_to_xmit; i++, cs++, atom++) {
	    atom->tty_text = *cs;
	    atom->tty_info = 0;
	}	
	
	a->async_transmit.xmt_reladdr = 0;
	a->async_transmit.xmt_len = chars_to_xmit * sizeof (*atom);
	if (dpl->ss_state & SS_XBUSY) {
		dpl->ss_state &= ~SS_XBUSY;
	} else if (dpl->ss_wbp) {
		dpl->ss_wbp->b_rptr += chars_to_xmit;
	}				
	
	/*
	 * Save the count so that during the transmitter interrupt
	 * we can find out how much was transfered.  Normally xmt_len
	 * will be set to zero by the IOP.  During the interrupt
	 * we need to update the bp pointer to reflect the amount
	 * transferred and call this routinue again.
	 */
	up->xmit_request_size = chars_to_xmit;
	
	while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
	  ;
	up->uart_dpl->ss_state |= SS_BUSY;
	a->async_command = ASYNC_XMIT | up->intr_mask;
	if (iop_poke (UARTIOCB + up->uart_line,
		      IOPB_SPIN,
		      K1_TO_IOPB_OFFSET(up->com_blk),
		      NO_SLEEP_ADDR) == -1)
	  cmn_err (CE_PANIC, "uart_start_output:");
	tspl = splclock();
	if ( uartto[up->uart_line] == 0 ) {
		uartto[up->uart_line]++;
		timeout (uart_lost_xmit, up, (HZ * 3));
	}
	splx(tspl);
	
	/*
	 * Either the transmitter has been started or we started
	 * a timer.  There's nothing more that can be done now so
	 * break out of the while loop and return.
	 */
	sysinfo.outch++;
    }
    splx (s); 
}

/*
 * This procedure sets the baud, parity, ... line parameters contaied in
 * "cflag" of the line "dpl"
 */
uart_setline (dpl, cflag, tp)
    register struct ss_line	*dpl;
    register ushort		cflag;
    register struct termio	*tp;
{
    register Uart_t *up;
    register int baud;

    up = (Uart_t *)dpl->ss_llocal;

    /* set line parameters */
    uart_param (dpl, cflag, tp);

    if( up->com_blk->async_modem_in & ASYNC_DCD ){
#ifndef	3_0	/* 4_0 */
	dpl->ss_modem_state |= BSD43_TIOCM_CD;
#endif
    } else {
#ifndef	3_0	/* 4_0 */
	dpl->ss_modem_state &= ~BSD43_TIOCM_CD;
#endif
    }
    return(0);
}

/* 
 * modem control procedure
 */
#ifndef	3_0	/* 4_0 */
uart_modem_control(dpl,op,value)
    struct ss_line	*dpl;
    int		 	op;
    int 		value;
{
    register Uart_t *up;

    up = (Uart_t *)dpl->ss_llocal;
    
    switch (op) {
	case SS_MC_STARTBREAK:
		up->com_blk->async_modem_out |= ASYNC_SBRK;
		break;
	case SS_MC_STOPBREAK:
		up->com_blk->async_modem_out &= ~ASYNC_SBRK;
		break;
	case SS_MC_ENABLEFLOW:
		up->com_blk->async_modem_out |= ASYNC_RTS;
		break;
	case SS_MC_DISABLEFLOW:
		up->com_blk->async_modem_out &= ~ASYNC_RTS;
		break;
	case SS_MC_ENABLEDTR:
		up->com_blk->async_modem_out |= (ASYNC_DTR | ASYNC_RTS);
		break;
	case SS_MC_DISABLEDTR:
		up->com_blk->async_modem_out &= ~(ASYNC_DTR | ASYNC_RTS);
    		break;
	default:
		break;
    }
    uart_run_cmd( up, ASYNC_PARAM, 0 );

    return(0);
}
#endif	/* 4_0 */

/*
 * driver control
 */
#ifndef	3_0	/* 4_0 */
uart_driver_control(dpl,op,value)
    struct ss_line	*dpl;
    int 		op;
    int			value;
{
    Uart_t *up;
    register struct iocblk *ioss;

    up = (Uart_t *)dpl->ss_llocal;

    switch (op) {
 	case SS_DC_TIOCOUTQ:
		return((dpl->ss_state & SS_BUSY) ? 1 : 0 );
		break;
	case SS_DC_STOP:
		uart_stop_output(up); 
		break;

	case SS_DC_FLOWCNTL:
		uart_flowcntl(dpl,value);
		break;

	case SS_DC_M_IOCTL:
		ioss = (struct iocblk*)((mblk_t *)value)->b_rptr;
		switch ( ioss->ioc_cmd ) {
		case UTCSETTIMESTAMP:
			up->driver_state |= UART_TIMESTAMP;
			ioss->ioc_error = 0;
			ioss->ioc_count = 0;
			((mblk_t *)value)->b_datap->db_type = M_IOCACK;
			return(1);

		case UTCCLRTIMESTAMP:
			up->driver_state &= ~UART_TIMESTAMP;
			ioss->ioc_error = 0;
			ioss->ioc_count = 0;
			((mblk_t *)value)->b_datap->db_type = M_IOCACK;
			return(1);

		default:
			break;
		}
	default:
		break;
    }
    return(0);
}
#endif	/* 4_0 */

/*
 * This routine is bogus.  Some how I'm losing an transmit interrupt or
 * the IOP is not delivering one.  In either case this is needed for now.
 */
unsigned uartlostcnt = 0;
unsigned uartlostrep = 0;
static int
uart_lost_xmit (up)
register Uart_t *up;
{
	register struct ss_line *dpl;

	if (iop_cmdsem (UARTIOCB + up->uart_line)) {
		uartlostrep++;
		timeout (uart_lost_xmit, up, (HZ*3));
	} else {
#ifdef IOPSTAT
		IopStat[IOPSTAT_UART_LOSS].s_stat++;
#endif
		dpl = up->uart_dpl;
		if ( (dpl->ss_state & SS_TXSTOP) ||
			(up->com_blk->async_status & ASYNC_STAT_OBS) ) {
			timeout (uart_lost_xmit, up, (HZ*3));
		} else {
			uartto[up->uart_line] = 0;
			uartlostcnt++;
			cmn_err(CE_WARN,"!UART_LOST_XMIT %x\n", 
							uartlostcnt);
			uartintr (up);
		}
	}
}


/*
 * uartintr()
 * Interrupt dispatcher.
 */
uartintr (up)
    register Uart_t *up;
{
    register struct ss_line *dpl;
    register volatile struct async *cb = up->com_blk;
    int stat;
    
    /* if status semephore is set, clear it. */

    if ( iop_wait(UARTIOCB + up->uart_line,IOPB_NOWAIT,&stat) == 0 ) {
    	iop_clear (UARTIOCB + up->uart_line);
    }
    
    /*
     * This stuff is order dependent.
     * First check for errors and command completions first.  These
     * two can be in either order.
     */
    if (cb->async_status & ASYNC_STAT_ERROR)
      uart_error_handle (up);
    if (up->driver_state & UART_IS_SLEEP)
      uart_command_complete (up);
    
    /* 
     * Finally see if there's characters to receive.  The IOP will 
     * update this on the fly and by checking for this condition last
     * we can hope to pick up as many characters as possible.
     */
    if (cb->async_status & ASYNC_STAT_RXRDY)
	uart_receive (up);

    if (cb->async_status & ASYNC_FLOW){
	if(cb->async_modem_in & ASYNC_DCD)
	    ss_con(dpl);
	else
	    ss_coff(dpl);
    }

    if (iop_cmdsem (UARTIOCB + up->uart_line))
	return cb->async_status;
    /*
     * Now check for transmitter ready and restart if possible.
     */
    if ((up->uart_dpl->ss_state & SS_BUSY) &&
		(cb->async_status & (ASYNC_STAT_TXRDY|ASYNC_STAT_TXEMP)) &&
		(cb->async_transmit.xmt_reladdr)) 
	uart_tint( (struct ss_line *)up->uart_dpl );

    

    /*
     * Sanity checking.  With silo mode enabled I seen disk damage that
     * can't be explain other than someone is stepping on a internal buffer 
     * somewhere.  If my guess is correct either the V50 or this driver is 
     * stepping past the receive buffers.
     */
    if (*((long *)(up->xmit_buffer + UART_BUF_SIZE)) != UART_MAGIC) {
	cmn_err (CE_CONT, "uart%d: xmit buffer got stepped on.\n",
		 up->uart_line);
	cmn_err (CE_CONT, "uart%d: found 0x%x\n",
		 *((long *)(up->xmit_buffer + UART_BUF_SIZE)));
	*((long *)(up->xmit_buffer + UART_BUF_SIZE)) = UART_MAGIC;
    }
	
    if (*((long *)(up->rcv_buffer + UART_BUF_SIZE)) != UART_MAGIC) {
	cmn_err (CE_CONT, "uart%d: rcv buffer got stepped on.\n",
		 up->uart_line);
	cmn_err (CE_CONT, "uart%d: found 0x%x\n",
		 *((long *)(up->rcv_buffer + UART_BUF_SIZE)));
	*((long *)(up->rcv_buffer + UART_BUF_SIZE)) = UART_MAGIC;
    }
    
    return cb->async_status;
}

uart_error_handle (up)
    register Uart_t *up;
{
    cmn_err (CE_CONT, "uart%d: status 0x%x\r\n", up->uart_line, 
    	     up->com_blk->async_status);
    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
      ;
    up->com_blk->async_command = ASYNC_RESET_ERROR | up->intr_mask;
    if (iop_poke (UARTIOCB + up->uart_line,
		  IOPB_NOWAIT,
		  K1_TO_IOPB_OFFSET(up->com_blk),
		  0)) {
	up->uart_dpl->ss_state |= SS_TXSTOP;
	cmn_err (CE_CONT, "uart: line %d shutting down\n");
	return;
    }
}

uart_command_complete (up)
    register Uart_t *up;
{
	if ( iop_cmdsem(UARTIOCB + up->uart_line) == SEMAPHORE_CLEAR ) {
		iop_wakeup (UARTIOCB + up->uart_line, 0);
	}
}

/* 
 * uart_tint
 * 	Transmit Intrrupt
 */
uart_tint (dpl)
    register struct ss_line *dpl;
{
    register Uart_t *up = (Uart_t *)dpl->ss_llocal;

    sysinfo.xmtint++;
    dpl->ss_state &= ~SS_BUSY;

    if ( uartto[up->uart_line] ) {
    	uartto[up->uart_line] = 0;
    	untimeout_func( uart_lost_xmit, up );
    }

    ss_start(dpl);
}


/*
 * uart_receive()
 * Receives characters from the IOP and places them on the
 * streams input queues.
 */
uart_receive (up)
    register Uart_t *up;
{
    register volatile struct rcv *rp;
    register volatile struct tty_atom *atom;
    register mblk_t *bp;
    register struct ss_line *dpl;		
    register int chars_avail;

    sysinfo.rcvint++;
    dpl = (struct ss_line *)up->uart_dpl;
    rp = &up->com_blk->async_receive;
    if(!((SS_ISOPEN|SS_WOPEN) & dpl->ss_state)) {
	rp->rcv_out = rp->rcv_in;  /* dump the characters on the floor */
	return;
    }

    if (((int)rp & K1BASE) != K1BASE)
      cmn_err (CE_CONT, "uart_receive: rcv is not K1!!!!\n");
    
    while (1) {
	if (uart_silo) {
	    /*
	     * We're using a ring buffer scheme.  If
	     * the R2000 pointer is less than the IOP
	     * we pick up that many characters.  If pointers
	     * are equal then the buffer is empty.
	     */
	    if (rp->rcv_out <= rp->rcv_in)
	      chars_avail = (rp->rcv_in - rp->rcv_out)
		/ sizeof (struct tty_atom);
	    
	    /*
	     * See how many characters are available until
	     * the end of the buffer is reached.  The IOP
	     * pointer has wrapped around at this point.
	     * Our pointer should never be equal to the end-of-buffer.
	     * Take the case where our pointer equal n-1.  One
	     * character will be received and at the end of the
	     * receive loop our pointer will be reset to the beginning
	     * of the buffer.
	     */
	    else if (rp->rcv_out <= UART_BUF_SIZE) {
		chars_avail = (UART_BUF_SIZE - rp->rcv_out) /
		  sizeof (struct tty_atom);
		
		/* sanity check */
		if (chars_avail == 0)
		  cmn_err (CE_CONT,
			   "uart_receive: rcv_out == UART_BUF_SIZE\n");
	    }
	    
	    atom = (struct tty_atom *)(up->rcv_buffer + rp->rcv_out);
	}
	else {
	    chars_avail = 1;
	    atom = &rp->rcv_char;
	}
	
	if (chars_avail == 0)
	  break;

	while (chars_avail > 0) {
	    if( ! ss_startstop( dpl, (atom->tty_text & 0xff))) {
	        rp->rcv_out += sizeof(*atom);
		chars_avail--;
		atom++; 
		continue;
	    }
	    if ( atom->tty_info & ASYNC_SOFT_OV ) {
		    cmn_err(CE_WARN,"ASYNC_SOFT_OV (%x)",atom->tty_info);
		    atom->tty_info &= ~ASYNC_SOFT_OV;
	    }
	    
	    if(atom->tty_info != 0){	

		/* overrun error */
		if(atom->tty_info & (ASYNC_UART_OV|ASYNC_SOFT_OV)) {
		    dpl->ss_overflow++;
		}

		/* when break recieved */
		if(atom->tty_info & ASYNC_BREAK){
		    if(dpl->ss_iflag & IGNBRK){
	        	rp->rcv_out += sizeof(*atom);
			chars_avail--;
			atom++; 
			continue;
		    }
		    if(dpl->ss_iflag & BRKINT){
			ss_mk_sig(dpl,SIGINT);
		    }
		    else {
			if(dpl->ss_iflag & PARMRK){
				uart_ss_inc(dpl,0377);
				uart_ss_inc(dpl,0);
			}
			uart_ss_inc(dpl,0);
		    }
		}

		/* ignore parity */
		else if(dpl->ss_iflag & IGNPAR) {
		    if (DebugUart (up->uart_line, 1))
			uartputc ('C');
		}

		else if(atom->tty_info & (ASYNC_PARITY_ERR|ASYNC_FRAME_ERR)){
		    if(atom->tty_info & ASYNC_FRAME_ERR) {
			/*
			 * if there is an entry for frameing error count
			 * in the local structure "uart", increment that 
			 * count. But it isn't used in kernel anywhere 
			 */
		    }
		    if(dpl->ss_iflag & PARMRK){
			if (DebugUart (up->uart_line, 1))
			  uartputc ('4');
			uart_ss_inc(dpl,0377);
			uart_ss_inc(dpl,0);
			uart_ss_inc(dpl,atom->tty_text);
		    }
		    else{
			uart_ss_inc(dpl,0);
			if (DebugUart (up->uart_line, 1))
			  uartputc ('c');
		    }
		}
	    }
	    else if ((dpl->ss_iflag & ISTRIP) == 0 &&
			(atom->tty_text & 0xff) == 0377 &&
			dpl->ss_iflag & PARMRK) {
		uart_ss_inc(dpl,0377);
		uart_ss_inc(dpl,0377);
	    }
	    else {
		uart_ss_inc(dpl,atom->tty_text);
		if (DebugUart (up->uart_line, 1))
		    uartputc (atom->tty_text);
	    }

	    rp->rcv_out += sizeof(*atom);
	    atom++; 
	    chars_avail--;
	}

	if (uart_silo) {
	    if (rp->rcv_out >= UART_BUF_SIZE)
	      rp->rcv_out = 0;
	}
	else
	  break;		/* one char is all you get is BS mode */
    }

    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
      ;
    up->com_blk->async_command = ASYNC_RCV_ACK | up->intr_mask;
    if (iop_poke (UARTIOCB + up->uart_line, IOPB_SPIN,
		  K1_TO_IOPB_OFFSET(up->com_blk),
		  NO_SLEEP_ADDR) == -1)
        cmn_err (CE_PANIC, "uart_receive: can't reset error!\n");
    
    /* Queue flow contol logic from putq */
    if ((dpl->ss_state & SS_ISOPEN)  && dpl->ss_rq && 
	 	canenable(dpl->ss_rq) && (dpl->ss_rq->q_flag & QWANTR)) {
	qenable (dpl->ss_rq);
    } else {
	/* ss_inc did not qenable */
	/* cleared in ss_rsrv */
	dpl->ss_ldebug |= SS_DBG_QENBSLEEP;
    }
}


/*
 * uart_param()
 * Sets the parameters for the uart.  It tries to be intelligent about
 * setting the parameters.  It only interrupts the data flow if absolutely
 * necessary.
 */
uart_param(dpl, cflag, tp)
    register struct ss_line *dpl;
    register ushort cflag;
    register struct termio *tp;
{
    register struct async *a;
    register Uart_t *up = (Uart_t *)dpl->ss_llocal;
    int uart_baud;
    int uart_data_size;
    int stat;
    unsigned char *p;
    
    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
      ;
    a = up->com_blk;
    a->async_modem_out = ASYNC_DTR | ASYNC_RTS;
    
    uart_baud = uart_convert (baud_rate_table, cflag & CBAUD);
    if ((a->async_RS232C & ASYNC_BAUD_M) != uart_baud) {
	a->async_RS232C &= ~ASYNC_BAUD_M; /* clear the old value */
	a->async_RS232C |= uart_baud;
    }
    
    /*
     * One stop bit.
     */
    if ((cflag & CSTOPB) == 0) {
	if ((a->async_RS232C & ASYNC_STOP1) == 0) {
	    a->async_RS232C &= ~ASYNC_STOP_M;
	    a->async_RS232C |= ASYNC_STOP1;
	}
    }
    /*
     * Two stop bits.
     */
    else if ((a->async_RS232C & ASYNC_STOP2) == 0) {
	a->async_RS232C &= ~ASYNC_STOP_M;
	a->async_RS232C |= ASYNC_STOP2;
    }
    
    if ((tp->c_iflag & ISTRIP) && ((cflag & CSIZE) == CS8)) {
	up->uart_dpl->ss_cflag &= ~CSIZE;
	up->uart_dpl->ss_cflag |= CS7;
    }
    
    uart_data_size = uart_convert (data_size_table, cflag & CSIZE);
    if ((a->async_RS232C & ASYNC_DATA_M) != uart_data_size) {
	a->async_RS232C &= ~ASYNC_DATA_M;
	a->async_RS232C |= uart_data_size;
    }
    
    if (uart_silo)
      a->async_RS232C |= ASYNC_SILO;
    else
      a->async_RS232C &= ~ASYNC_SILO;
    
    if (cflag & PARENB) {
	if ((a->async_RS232C & ASYNC_PAR_M) == ASYNC_NO_PAR) {
	    a->async_RS232C &= ~ASYNC_PAR_M;
	    if (cflag & PARODD)
	      a->async_RS232C |= ASYNC_ODD_PAR;
	    else
	      a->async_RS232C |= ASYNC_EVEN_PAR;
	}
    }
    else if ((a->async_RS232C & ASYNC_PAR_M) != ASYNC_NO_PAR) {
	a->async_RS232C &= ~ASYNC_PAR_M;
	a->async_RS232C |= ASYNC_NO_PAR;
    }
    
    a->async_receive.rcv_reladdr = (int)up->rcv_buffer & POFFMASK;
    /*
     * This flushes anything that's already in the buffer.
     */
    a->async_receive.rcv_out = a->async_receive.rcv_in;
    a->async_receive.rcv_len = UART_BUF_SIZE;
    a->async_transmit.xmt_reladdr = 0;
    a->async_transmit.xmt_len = 0;
    
    up->intr_mask = ASYNC_IXMIT_READY | ASYNC_IXMIT_EMPTY |
      ASYNC_IRECVERR | ASYNC_IRECV | ASYNC_IMODEM | ASYNC_ICMDRDY;
    a->async_command = ASYNC_INIT | up->intr_mask;
    
    /*
     * The uart driver calls iop_param in two places.  Once
     * during uart open and the other is during wput.  We
     * can only call sleep during open.  iop_poke is called with
     * either IOPB_SLEEP or IOPB_NOWAIT.  If IOPB_NOWAIT is used
     * to poke the iop then spin waiting for the command to complete.
     * If sleeping setup a timer and call iop_wait.
     * (Great comment, too bad code no reflect it.)
     */
    if (iop_poke (UARTIOCB + up->uart_line, IOPB_SPIN, 
			K1_TO_IOPB_OFFSET(a), NO_SLEEP_ADDR) == -1) {
      cmn_err (CE_PANIC, "uart_param: iop not available\n");
    }
    iop_wait (UARTIOCB + up->uart_line, IOPB_SPIN, &stat, 0);
    iop_clear (UARTIOCB + up->uart_line);

    return 0;
}

uart_flowcntl(dpl,tp)
register struct ss_line *dpl;
register struct termio *tp;
{
	register struct async *a;
	register Uart_t *up = (Uart_t *)dpl->ss_llocal;
	register uint diff;
	int stat;

	while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
	  ;
	a = up->com_blk;
	diff = dpl->ss_iflag ^ tp->c_iflag;
	
	/*
	 * If canocical processing is turned on then set the uart's
	 * vmin and vtim to some internal default.  Else use the
	 * values found in the terminfo structure.
	 * 
	 */
	if ( diff & ICANON ) {
		if (!(tp->c_lflag & ICANON)) { 
			a->async_vtim = tp->c_cc[VTIME];
			a->async_vmin = tp->c_cc[VMIN];
		}
	}

	if ( diff & IXON ) {
		if ((tp->c_iflag & IXON) &&
			((_posix_vdisable != 0) ||
			((tp->c_cc[VSTART] != CDEL) &&
			(tp->c_cc[VSTOP] != CDEL)))){
			a->async_flow |=
	  			ASYNC_SW_OFLOW | ASYNC_EAT_FLOW;
			a->async_x_on = tp->c_cc[VSTART];
			a->async_x_off = tp->c_cc[VSTOP];
		} else {
	    		a->async_flow &= ~ASYNC_SW_OFLOW;
		}
	}
	if ( diff & IXOFF ) {
		if ((tp->c_iflag & IXOFF) &&
			((_posix_vdisable != 0) ||
			((tp->c_cc[VSTART] != CDEL) &&
			(tp->c_cc[VSTOP] != CDEL)))) {
			a->async_flow |= ASYNC_SW_IFLOW | ASYNC_EAT_FLOW;
			a->async_x_on = tp->c_cc[VSTART];
			a->async_x_off = tp->c_cc[VSTOP];
		} else {
	    		a->async_flow &= ~ASYNC_SW_IFLOW;
		}
	}
	if ( diff & (IXON|IXOFF) ) {
		if ( (a->async_flow & (ASYNC_SW_OFLOW|ASYNC_SW_IFLOW)) == 0 ) {
			a->async_flow &= ~ASYNC_EAT_FLOW;
		}
	}
	if ( diff & IXANY ) {
		if (tp->c_iflag & IXANY) {
			a->async_flow |= ASYNC_SW_XANY;
		} else {
			a->async_flow &= ~ASYNC_SW_XANY;
		}
	}
	a->async_command = ASYNC_FLOW | up->intr_mask;
	if (iop_poke (UARTIOCB + up->uart_line, IOPB_SPIN, 
			K1_TO_IOPB_OFFSET(a), NO_SLEEP_ADDR) == -1) {
	  cmn_err (CE_PANIC, "uart_flowcntl: iop not available\n");
	}
	iop_wait (UARTIOCB + up->uart_line, IOPB_SPIN, &stat, 0);
	iop_clear (UARTIOCB + up->uart_line);
}



/*
 * uart_convert
 * Giving a termio value return its IOP equivalent.  There are two tables
 * in use at this time. One is for baud rates and the other is for data
 * sizes.
 */
uart_convert (table, val)
    Convert_Table_t *table;
    register int val;
{
    register Convert_Table_t *p;
    
    for (p = table; p->iop_value != -1 && p->termio_value != -1; p++)
      if (p->termio_value == val)
	break;
    return p->iop_value;
}


/*
 * uart_stop_output()
 * The stop bit in driver_state will be state by the calling routine.
 */
uart_stop_output (up)
    register Uart_t *up;
{
    int stat;

    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
      ;
    up->com_blk->async_command = ASYNC_STOP | up->intr_mask;
    if (iop_poke (UARTIOCB + up->uart_line, IOPB_SPIN,
		  K1_TO_IOPB_OFFSET(up->com_blk),
		  NO_SLEEP_ADDR) == -1)
      cmn_err (CE_PANIC, "uart_stop_output");
    iop_wait (UARTIOCB + up->uart_line, IOPB_SPIN, &stat, 0);

    iop_clear (UARTIOCB + up->uart_line);
}

/*
 * run IOP command
 */
uart_run_cmd (up, cmd, flag)
    register Uart_t *up;
    register short cmd;
    register int flag;
{
    register int offset;
    int stat;
    
    offset = K1_TO_IOPB_OFFSET(up->com_blk);
    up->com_blk->async_command = cmd | up->intr_mask;
    
    while (iop_cmdsem (UARTIOCB + up->uart_line) == SEMAPHORE_SET)
	  ;
    if (iop_poke (UARTIOCB + up->uart_line, IOPB_NOWAIT, 
					offset, NO_SLEEP_ADDR) == -1)
        cmn_err (CE_PANIC, "uart_run_cmd: uart still busy");

    if( flag == IOPB_SPIN ){
        iop_wait (UARTIOCB + up->uart_line, IOPB_SPIN, &stat, 0);
        iop_clear (UARTIOCB + up->uart_line);
    }
    else if( flag == IOPB_SLEEP ) {
	up->driver_state |= UART_IS_SLEEP;
	iop_wait (UARTIOCB + up->uart_line, IOPB_SLEEP, &stat, NO_SLEEP_ADDR);
	up->driver_state &= ~UART_IS_SLEEP;
        iop_clear (UARTIOCB + up->uart_line);
    }
}

#define LINE console_line
#define CONSOLE_UART (UARTIOCB + LINE)
/*
 * Simple output to uart devices.  Polls for input and and busy waits
 * for output.  No interrupts are used.  This should only be called during
 * the early stages of the kernel's life, i.e. autoconfig
 */
early_uartinit()
{
    register struct async *a;
    struct async *iop_alloc();
    int status;
    
    SET_CONSOLE_LINE;
    if ((a = iop_alloc (CONSOLE_UART, sizeof (*a))) == NULL)
      return;		/* not much that we can do */
    
    /*
     * Set up the uart for polled BS mode at 9600
     * baud.
     * TODO:  Should make an inquiry to the nvram and see
     * what baud rate to use.
     */
    a->async_command = ASYNC_INIT;
    a->async_RS232C = ASYNC_B9600 | ASYNC_STOP1 | ASYNC_DATA8 | ASYNC_BSMODE | ASYNC_NO_PAR;
    a->async_modem_out = ASYNC_DTR | ASYNC_RTS;
    a->async_vtim = 1;
    a->async_vmin = 1;
    a->async_flow = 0;
    a->async_x_on = 0;
    a->async_x_off = 0;
    a->async_lit_c = '\\';
    
    /*
     * If this command fails we can only hope the there's a
     * graphics board available to output some diagnostics.
     */
    if (iop_poke (CONSOLE_UART, IOPB_SPIN, K1_TO_IOPB_OFFSET(a), 0) == 0) {
	iop_wait (CONSOLE_UART, IOPB_SPIN, &status, 0);
    }
    iop_clear (CONSOLE_UART);
    a->async_command = ASYNC_FLOW;
    if (iop_poke (CONSOLE_UART, IOPB_SPIN, K1_TO_IOPB_OFFSET(a), 0) == 0) {
	iop_wait (CONSOLE_UART, IOPB_SPIN, &status, 0);
    }
    iop_clear (CONSOLE_UART);
    return;
}

/*
 * We not looking for performance here.  Just get in and get the job
 * done.
 */
uartgetc()
{
    register volatile struct async *a;
    struct async *iop_alloc();
    register int c;
    int status;
    
    SET_CONSOLE_LINE;
    if ((a = iop_alloc (CONSOLE_UART, sizeof (*a))) == NULL)
      return 0; 
    
    while ((a->async_status & ASYNC_STAT_RXRDY) == 0)
      ;
    
    c = a->async_receive.rcv_char.tty_text;
    a->async_command = ASYNC_RCV_ACK;
    if (iop_poke (CONSOLE_UART, IOPB_SPIN, K1_TO_IOPB_OFFSET(a), 0) == 0)
      iop_wait (CONSOLE_UART, IOPB_SPIN, &status, 0);
    iop_clear (CONSOLE_UART);
    return c;
}

uartputc (c)
    register int c;
{
    register volatile struct async *a;
    register struct tty_atom *atom;
    register Uart_t *up;
    struct async *iop_alloc();
    int status;
    
    SET_CONSOLE_LINE;
    up = &uart_control_blocks[LINE];
/*  iop_clear (CONSOLE_UART); */
    if (c == '\n')
      uartputc ('\r');
    
    if ((a = iop_alloc (CONSOLE_UART, sizeof (*a))) == NULL)
      return;
    
    while ((a->async_status & ASYNC_STAT_TXRDY) == 0)
      ;
    
    if ((a->async_RS232C & ASYNC_MODE_M) == ASYNC_SILO) {
	atom = (struct tty_atom *)uart_control_blocks[LINE].xmit_buffer;
	atom->tty_text = c;
	a->async_transmit.xmt_len = sizeof (*atom);
	a->async_transmit.xmt_reladdr = 0;
    }
    else
      a->async_transmit.xmt_char.tty_text = c;
    
    /*
     * The uart task uses the interrupt bits in the last async_command
     * to interrupt the R2000.  If the driver is open and we don't
     * reset the receive interrupt then the kernel will not get a
     * receive interrupt.  We don't have to worry about transmit interrupts
     * because they're set every time a xmit command is sent.
     */
    if (uart_control_blocks[LINE].uart_dpl == NULL)
#ifdef	3_0
	uart_control_blocks[LINE].uart_dpl = 
			&(duboard[DPUNIT(LINE)].ss_line[DPLINE(LINE)]);
#else	/* 4_0 */
        uart_control_blocks[LINE].uart_dpl = &dulines[DPUNIT(LINE)][DPLINE(LINE)];
#endif
    if (uart_control_blocks[LINE].uart_dpl->ss_state & SS_ISOPEN)
      a->async_command = ASYNC_XMIT | ASYNC_IRECV;
    else
      a->async_command = ASYNC_XMIT;
    
    /*
     * Testing for a failure at this point is useless.  If you can't
     * print how else are you going to indicate that there's are problem.
     */
    iop_poke (CONSOLE_UART, IOPB_SPIN, K1_TO_IOPB_OFFSET(a), 0);
    iop_wait (CONSOLE_UART, IOPB_SPIN, &status, 0);
    iop_clear (CONSOLE_UART);

    /*
     * If the kernel driver was transmitting when this routine was called
     * uartintr will never be called with a xmitintr.  This routine clears the
     * status semaphore and therefore iop_intr will not call uartintr.
     */
    if (up->uart_dpl->ss_state & SS_BUSY)
      uartintr (up);

    return;
}

extern char *console;
unsigned s_c_l_cnt = 0;
set_console_line()
{
	if ( (console != NULL) && ((*console == '1') || (*console == 't') || 
		(*console == 'T')) ) {
		console_line = 1;
	} else {
		console_line = 0;
	}
}

uart_ss_inc(dpl,c)
register struct ss_line *dpl;
register char c;
{
	register Uart_t *up = (Uart_t *)dpl->ss_llocal;
    	register time_t tstamp;

	if ( !(up->driver_state & UART_TIMESTAMP) ) {
		ss_inc(dpl, c);
	} else {
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, UARTSYNCCHAR);
		ss_inc(dpl, c);
		tstamp = lbolt;
		ss_inc(dpl, ((tstamp>>24)&0xff));
		ss_inc(dpl, ((tstamp>>16)&0xff));
		ss_inc(dpl, ((tstamp>>8)&0xff));
		ss_inc(dpl, (tstamp&0xff));
	}
}


/*
  Local Variables:
  c-indent-level: 4
  c-argdecl-indent: 4
  End:
  */
