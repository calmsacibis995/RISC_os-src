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
#ident	"$Header: mouse.c,v 1.2.2.2 90/05/10 05:23:54 wje Exp $"
/*
 * $Header: mouse.c,v 1.2.2.2 90/05/10 05:23:54 wje Exp $
 */
/*
 * Mouse driver for V50 serial port.
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

#include "sys/conf.h"
#include "sys/ss.h"
#include "sys/iop.h"
#include "sys/mouse.h"
#include "sys/mousereg.h"

/* #define MOUSE_DEBUG 1 */

#define BAUD_TABLE(val) \
{ B/**/val, BAUD_TO_LOAD(val) }

struct MouseTable {
    int tval;
    int mval;
} MouseBauds[] = {
    BAUD_TABLE(1200),
    BAUD_TABLE(2400),
    BAUD_TABLE(4800),
    BAUD_TABLE(9600),
};
int nMouseBauds = sizeof (MouseBauds) / sizeof (struct MouseTable);

static struct module_info dum_info = {
    STRID_IOP_MOUSE,		/* module ID */
    "mouse",			/* module name */
    0,				/* minimum packet size */
    1024,			/* maximum packet size */
    128,			/* high water mark */
    16,				/* low water mark */
};

#define	MOUSE_BMAX	1
#define	MOUSE_LMAX	1
#define MOUSE_OUTCMAX	1
#define	MICKIES		1
#define	MOUSEUNIT(x)	((x) / MICKIES)
#define	MOUSELINE(x)	((x) - MICKIES * MOUSEUNIT(x))

int	Mouse_act(), Mouse_zap(), Mouse_outc(), Mouse_setline();
int 	Mouseopen();  

struct ss_devdep_info mousedd = 
	{ "IOP_MOUSE", SS_TTY, MOUSE_BMAX, MOUSE_LMAX, MOUSE_OUTCMAX,
	  Mouse_act, Mouse_zap, Mouse_outc, Mouse_setline, 
	  ss_null, ss_null,
	};

#ifdef	R3_0 
struct ss_struct Mouse_board[MOUSE_BMAX];
#else	/* R4_0 */
struct ss_struct Mouse_board[MOUSE_BMAX];
struct ss_line 	 Mouse_lines[MOUSE_BMAX][MOUSE_LMAX];
#endif

static struct qinit Mouse_rinit = {
    NULL, ss_rsrv, Mouseopen, ss_close, NULL, &dum_info, NULL
};

static struct qinit Mouse_winit = {
    ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};
    
struct streamtab Mouseinfo = {
    &Mouse_rinit, &Mouse_winit, NULL, NULL
};

/* Local structure for IOP MOUSE */
MouseBody MickyMouse[MICKIES];

/*-------------------------------------------------------
 * Initialize IOP Mouse Driver (V50 serial port)
 *------------------------------------------------------*/
Mouseedtinit (edt)
    struct edt *edt;
{
    register MouseBody	*Micky;
    MouseReg *IOReg;
    int line;

    line = edt->e_intr_info->v_unit;
    Micky = &MickyMouse[line];
#ifdef MOUSE_DEBUG
    cmn_err(CE_CONT,"mouse init : <%d> :",line);
#endif

#ifdef	R3_0
    Micky->Mouse_dpl = &(Mouse_board[MOUSEUNIT(line)].ss_line[MOUSELINE(line)]);
#else	/* R4_0 */
    Micky->Mouse_dpl = &Mouse_lines[MOUSEUNIT(line)][MOUSELINE(line)];
#endif
    Micky->Mouse_dpl->ss_line = line;
    Micky->Mouse_dpl->pss = Mouse_board + MOUSEUNIT(line);
    Micky->Mouse_dpl->pss->ss_nlines = MOUSE_LMAX;
    Micky->Mouse_dpl->pss->ss_devdep = &mousedd;
    Micky->Mouse_dpl->ss_llocal = (char *)Micky;
    Micky->Mouse_dpl->ss_lenable = 1;

    if (IOReg = (MouseReg *)iop_alloc (MOUSEIOCB, sizeof (MouseReg))) {
	Micky->MouseEyes = IOReg;
	Micky->Health = MOUSE_AVAIL;
	edt->e_base = (paddr_t)Micky;

#ifdef notdef
	IOReg->m_debug_level = 10;
	IOReg->m_command = M_DEBUG_COMMAND;
	if ((iop_poke (MOUSEIOCB, IOPB_NOWAIT, K1_TO_IOPB_OFFSET(IOReg), 
		NO_SLEEP_ADDR) == -1) || MouseSpin (2000))
            Micky->Health = MOUSE_DEAD;
#endif /* notdef */
    }
    else
	Micky->Health = MOUSE_DEAD;
#ifdef	MOUSE_DEBUG
    cmn_err(CE_CONT," available\n");
#endif
}

/*----------------------------------------------------
 * IOP Mouse Open 
 *---------------------------------------------------*/
Mouseopen (rq, dev, flag, sflag)
    queue_t *rq;
    dev_t dev;
    int flag, sflag;
{
    register MouseBody	*Micky;
    register queue_t *wq, *head_rq;
    register struct stdata *stp;
    register int line, s, i;
    int cflag;
  
#ifdef	MOUSE_DEBUG
    cmn_err(CE_CONT,"mouseopen : start\n");
#endif
    /*
     * There's only one mouse port so everything else is invalid.
     */
    if (sflag)
    	return OPENFAIL;

    if (MOUSEUNIT(minor(dev)) >= MOUSE_BMAX) {
	u.u_error = EINVAL;
    	return OPENFAIL;
    }

    if ((line = MOUSELINE(minor(dev))) >= MICKIES) {
	u.u_error = EINVAL;
    	return OPENFAIL;
    }

    Micky = &MickyMouse[line];
    if (Micky->Health == MOUSE_DEAD) {
    	u.u_error = EIO;
    	return OPENFAIL;
    }

    if (Micky->Mouse_dpl->ss_state & (SS_ISOPEN|SS_WOPEN)) {
	if (Micky->Mouse_dpl->ss_rq != rq) {
      	    u.u_error = ENOSR;
      	    return OPENFAIL;
        }
    }
    else {
#ifdef	MOUSE_DEBUG
	cmn_err(CE_CONT,"mouseopen : ss_open\n");
#endif
#ifdef	R3_0
    	if( ss_open (Micky->Mouse_dpl, rq, dev, flag, sflag) == OPENFAIL) 
#else	/* R4_0 */
    	cflag = CS8 | HUPCL | CLOCAL | B1200;
    	if( ss_open (Micky->Mouse_dpl, rq, dev, flag, sflag, cflag) == OPENFAIL)
#endif
	    return OPENFAIL;

	/* push data converter module upon stty_ld */
#ifdef	MOUSE_DEBUG
	cmn_err(CE_CONT,"mouseopen: push mdc modules\n");
#endif
    	s = spltty();
    	if (( i = findmod ("mouse_dc")) < 0) {
	    u.u_error = ENXIO;
	    splx(s);
  	    return OPENFAIL;
    	}

    	head_rq = rq->q_next->q_next;  	   	/* queue of stream head */
    	stp = (struct stdata *)head_rq->q_ptr;  
    	if ( !qattach ( fmodsw[i].f_str, head_rq ,dev, 0)) {
	    if (u.u_error) u.u_error = ENXIO;
	    splx(s);
	    return OPENFAIL;
    	}
        else 
	    stp->sd_pushcnt++;
    	splx (s);
    }
#ifdef	MOUSE_DEBUG
    cmn_err(CE_CONT,"mouseopen: complete\n");
#endif
    return minor(dev);
}
      
/*---------------------------------------------------------------
 * Activate IOP Mouse
 *	This should be called only when safe from interrupt
 *--------------------------------------------------------------*/
Mouse_act( dpl )
    register struct ss_line	*dpl;
{
#ifdef	MOUSE_DEBUG
    cmn_err(CE_CONT,"Mouse_act\n");
#endif
    return 0;
}

/*---------------------------------------------------------------
 * Shutdown IOP Mouse
 *	This should be called only when safe from interrupt
 *--------------------------------------------------------------*/
Mouse_zap( dpl )
    register struct ss_line	*dpl;
{
#ifdef	MOUSE_DEBUG
    cmn_err(CE_CONT,"Mouse_zap\n");
#endif
    return 0;
}

/*-------------------------------------------------------------- 
 * This procedure output "len" characters pointed to by "cs"
 *  	One character maked one interrupt to IOP
 *-------------------------------------------------------------*/
Mouse_outc( dpl, cs, len )
    register struct ss_line	*dpl;
    register char		*cs;
    register int		len;
{
    register MouseBody *Micky;
    MouseReg *IOReg;
    int i,s;

    Micky = (MouseBody *)dpl->ss_llocal;
    IOReg = (MouseReg *)Micky->MouseEyes;    

    s = spltty();
    for(i = 0 ; i < len ; i++, cs++) {
	IOReg->m_xmit_char = *cs;
	IOReg->m_command = M_TRANSMIT;
      
	Micky->Mouse_dpl->ss_state |= SS_BUSY;
	if (iop_poke (MOUSEIOCB, IOPB_SPIN,
		      K1_TO_IOPB_OFFSET(IOReg), NO_SLEEP_ADDR) == -1)
	  cmn_err (CE_NOTE, "Mouse: Can't poke V50 for transmit\n");
    }
    splx (s);
}

/*-------------------------------------------------------------------
 * This procedure to set the baud rate, parity,..., line decipline.
 *------------------------------------------------------------------*/
Mouse_setline ( dpl, cflag, tp )
    register struct ss_line	*dpl;
    int				cflag;
    register struct termio	*tp;
{
    register MouseBody *Micky = (MouseBody *)dpl->ss_llocal;
    register struct MouseTable *mp;
    register MouseReg *IOReg;
    int stat;

    IOReg = Micky->MouseEyes;
    for (mp = &MouseBauds[0]; mp < &MouseBauds[nMouseBauds]; mp++)
      if (mp->tval == ( cflag & CBAUD ))
          IOReg->m_baud = mp->mval;

    IOReg->m_code_reg = 0;
    if ( cflag & CSTOPB )
        IOReg->m_code_reg |= M_STOP2;
    else
        IOReg->m_code_reg |= M_STOP1;

    if (( cflag & CSIZE ) == CS8)
        IOReg->m_code_reg |= M_CS8;
    else
        IOReg->m_code_reg |= M_CS7;

    if ( cflag & PARENB ) {
        if ( cflag & PARODD )
      	    IOReg->m_code_reg |= M_PARITY_ODD;
        else
            IOReg->m_code_reg |= M_PARITY_EVEN;
    }
    else
        IOReg->m_code_reg |= M_PARITY_NONE;

    IOReg->m_command = M_PARAMS_SET | M_INTR;
    if (iop_poke (MOUSEIOCB, IOPB_NOWAIT, 
			K1_TO_IOPB_OFFSET(IOReg), NO_SLEEP_ADDR) == -1)
        cmn_err (CE_NOTE, "Mouse: Failed to poke mouse\n");
    else if ( MouseSpin (1000) )
        cmn_err (CE_NOTE, "Mouse: Failed to set mouse params.\n");
    else
        iop_clear (MOUSEIOCB);
    return 0;
}

/*---------------------------------------------
 * Wait for semaphore will be cleard by IOP
 *--------------------------------------------*/
MouseSpin (CountDown)
    int CountDown;
{
    int stat;
  
    do {
        if (iop_wait (MOUSEIOCB, IOPB_NOWAIT, &stat, 0) == 0)
            return 0;
    	DELAY(1000);
    } while (CountDown--);
    return 1;
}

/*-------------------------------------
 * Mouse Interupt Routine 
 *------------------------------------*/
Mouseintr (Micky)
    register MouseBody *Micky;
{
    register MouseReg *IOReg;
    register int MouseStatus, MouseError, ValidIntr;

    ValidIntr = 0;
    MouseStatus = Micky->MouseEyes->m_status_reg;
    MouseError = MouseStatus &
      (M_PARITY_ERROR | M_OVERRUN_ERROR | M_FRAME_ERROR | M_BREAK_DETECTED);
    iop_clear (MOUSEIOCB);

    /* Transmit Start */
    if ((Micky->Mouse_dpl->ss_state & SS_BUSY) && (MouseStatus & M_XMIT_RDY)) {
	Micky->Mouse_dpl->ss_state &= ~SS_BUSY;
	ss_start( Micky->Mouse_dpl );
    	ValidIntr = 1;
    }

    /* Receive Start */
    if (MouseStatus & M_RCV_RDY) {
        MouseReceive (Micky, MouseError);
        ValidIntr = 1;
    }
    return 0;
}

/*------------------------------------------------------- 
 * Receive Character from IOP 
 *	One character is coming up in one interrupt
 *------------------------------------------------------*/
MouseReceive (Micky, MouseError)
    register MouseBody *Micky;
{
    register mblk_t *bp;
    register MouseReg *IOReg;
    register struct ss_line *dpl = (struct ss_line *)Micky->Mouse_dpl;

    IOReg = Micky->MouseEyes;
  
    if ( !(Micky->Mouse_dpl->ss_state & (SS_ISOPEN|SS_WOPEN))) 
	return;

    if ( !ss_startstop(dpl, ( IOReg->m_rcv_char & 0xff ))) 
	return;

    /* Get Character into Buffer */
    if ( !MouseError ) {
	if((( dpl->ss_termio.c_iflag & IGNPAR ) == 0 ) &&
	   (( dpl->ss_termio.c_iflag & ISTRIP ) == 0 ) &&
 	    ( dpl->ss_termio.c_iflag & PARMRK ) && (IOReg->m_rcv_char == 0377))
	    ss_slowr( dpl, 0377 );
	ss_slowr( dpl, IOReg->m_rcv_char );
    }
    else if (( MouseError & M_PARITY_ERROR ) &&
		(( dpl->ss_termio.c_iflag & IGNPAR ) == 0)) {
	if ( dpl->ss_termio.c_iflag & PARMRK ) {
	    ss_slowr( dpl, 0377 );
	    ss_slowr( dpl, 0 );
	    ss_slowr( dpl, IOReg->m_rcv_char );
	}
	else 
	    ss_slowr( dpl, 0 );
    }

    /* Respond to IOP */
    IOReg->m_command = M_RCV_ACK;
    if (iop_poke (MOUSEIOCB, IOPB_SPIN, 
			K1_TO_IOPB_OFFSET(IOReg), NO_SLEEP_ADDR) == -1)
        cmn_err (CE_NOTE, "MouseReceive: Can't send RCV_ACK\n");

    /* Set Queue Enable */
    if (( dpl->ss_state & SS_ISOPEN ) && dpl->ss_rq &&
	    canenable( dpl->ss_rq ) && ( dpl->ss_rq->q_flag & QWANTR )) {
	qenable (dpl->ss_rq );
    } else
	dpl->ss_ldebug |= SS_DBG_QENBSLEEP;
}

/*--------------------------------
 * Dump IOP Mouse information
 *-------------------------------*/
MouseDump (Micky,String)
    register MouseBody *Micky;
    char *String;
{
    register MouseReg *IOReg;

    IOReg = Micky->MouseEyes;
    cmn_err (CE_CONT, "MouseIO at 0x%x\n  <%s>\n", IOReg, String);
    cmn_err (CE_CONT, "  m_baud:        0x%x\n", IOReg->m_baud);
    cmn_err (CE_CONT, "  m_code_reg:    0x%x\n", IOReg->m_code_reg);
    cmn_err (CE_CONT, "  m_status_reg:  0x%x\n", IOReg->m_status_reg);
    cmn_err (CE_CONT, "  m_xmit_char:   0x%x\n", IOReg->m_xmit_char);
    cmn_err (CE_CONT, "  m_rcv_char:    0x%x\n", IOReg->m_rcv_char);
    cmn_err (CE_CONT, "  m_command:     0x%x\n", IOReg->m_command);
    cmn_err (CE_CONT, "  m_debug_level: 0x%x\n", IOReg->m_debug_level);
}
  
      

/*
  Local Variables:
  c-indent-level: 2
  c-argdecl-indent: 2
  End:
  */
