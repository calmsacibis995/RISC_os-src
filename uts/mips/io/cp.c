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
#ident	"$Header: cp.c,v 1.23.1.8.1.3.1.6 91/01/09 15:50:18 beacker Exp $"

/*
 * Integrated Solutions {Intelligent} Communications Processor driver
 */
#include "sys/sbd.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/sysmacros.h"
#include "sys/systm.h"
#include "sys/signal.h"
#include "sys/pcb.h"
#include "sys/immu.h"
#include "sys/region.h"
#include "bsd/sys/time.h"
#include "sys/user.h"
#include "sys/errno.h"
#include "sys/termio.h"
#include "sys/file.h"
#include "sys/stream.h"
#include "sys/stropts.h"
#include "sys/strids.h"
#include "sys/stty_ld.h"
#include "sys/sysinfo.h"
#include "sys/debug.h"
#include "sys/cmn_err.h"
#include "sys/cpreg.h"
#include "sys/buf.h"
#include "sys/ss.h"
#include "sys/edt.h"
#include "sys/elog.h"
#include "sys/vmereg.h"
#include "bsd43/sys/ioctl.h"
#include "sys/bsd_glue.h"
#include "sys/kmem.h"

#define CP_BMAX		8
#define CP_LMAX		16
#define CP_OUTCMAX	NBPP
#define LP_BMAX 	CP_BMAX
#define LP_LMAX		1
#define LP_OUTCMAX	1024
#define CP_REAL_TINT	CP_LMAX + 1	/* Some impossible port number */

#ifndef MIN
#define MIN(a, b)	(((a) < (b)) ? (a) : (b));
#endif


/*
 * This alows for selected print statements to be turned on and off
 * while the kernel is runing.
 */
/* cp debug points */
#define CPDBG_PT1	0x0001		/* interrupt status */
#define CPDBG_PT2	0x0002		/* transmitt flow */
#define CPDBG_PT3	0x0004		/* reciever flow */
#define CPDBG_PT4	0x0008		/* ioctl flow */
#define CPDBG_PT5	0x0010		/* DCD interrupts */
#define CPDBG_PT6	0x0020		/* LP interrupts */
int cp_print;

#ifdef R6000
#define CP_SILO_DMA
#endif R6000

/*
 *	Common Streams Terminal Driver Interface
 */


int cp_enable_line(), cp_disable_line(), cp_outc(), cp_setline(), cp_modem_control();
int cp_driver_control();

struct ss_devdep_info cpdd =
        { "CP", SS_TTY, CP_BMAX, CP_LMAX, CP_OUTCMAX,
           NULL, NULL, cp_outc, cp_setline, cp_modem_control, 
	   cp_driver_control
        };

struct ss_struct cpboard[CP_BMAX];

struct ss_line cplines[CP_BMAX][CP_LMAX];

int	cpconf[CP_BMAX];

#ifdef CP_SILO_DMA
struct cp_board_info_type {
  ushort *silo_buffer;		/* Points to buffer for silo (input) data */
  int silo_bufsize;		/* Size of silo data buffer */
  int silo_sph;			/* I/O mapping handle for buffer */
  ioaddr_t silo_ioaddr;		/* I/O address of silo buffer (on VME bus) */
  int silo_mode;		/* silo dma enabled (=1) or disabled (=0) */

  /* The following fields are used to maintain copies of some controller
   * registers.  This is done in order to avoid an extra read of the
   * register before setting/clearing bits for individual lines.
   */
  ushort ler_copy;		/* current value in controller's ler */
  ushort acr_copy;		/* current value in controller's acr */
  ushort brk_copy;		/* current value in controller's brk */
} cp_board_info[CP_BMAX];
#else
struct cp_board_info_type {
  ushort ler_copy;		/* current value in controller's ler */
  ushort acr_copy;		/* current value in controller's acr */
  ushort brk_copy;		/* current value in controller's brk */
} cp_board_info[CP_BMAX];
#endif CP_SILO_DMA

/* Minor device number decoding */
#define    LPUNIT(m)    (CPUNIT(m))
#define    LPLINE(m)    (0)


int lp_enable_line(), lp_disable_line(), lp_outc(), lp_setline(), lp_modem_control();
int lp_driver_control();

struct ss_devdep_info lpdd =
        { "LP", SS_TTY, LP_BMAX, LP_LMAX, LP_OUTCMAX,
           NULL, NULL, lp_outc, lp_setline, lp_modem_control, 
	   lp_driver_control
        };

struct ss_struct lpboard[LP_BMAX];
struct ss_line lplines[LP_BMAX][LP_LMAX];

/*
 * Line local information connected to ss structure via ss_llocal
 */

struct cpp_s {
	struct	ss_struct *pcp;	/* pointer to controller struct */
	struct	ss_line *pcl; /* pointer to ss_line struct */
	struct cp_board_info_type *board_info;
	int 	tx_ints;	/* For debugging */
	int 	tx_timeouts;	/* For debugging */
	unsigned char   v_vec;
	char	cpp_timer_check;
	int	parity_errors;
	int	framing_errors;
	int	overruns;
	int 	bad_bc;
	ioaddr_t stradr;
	};

/* allocate storage for line info */
struct cpp_s cp_linfo[CP_BMAX][CP_LMAX];

struct	cp_lp {
	struct	ss_struct *pcp;	/* pointer to controller struct */
	struct	ss_line *pcl; /* pointer to ss_line struct */

	int		lp_tid;
	short		lp_state;
#define	LP_EXIST	0x01
#define	LP_CENTRON	0x02
#define	LP_TOUT		0x10
#define	LP_NOCHANGE	0x20
#define LP_WAIT		0x80
	short		lp_canon;
	short		lp_physcol;
	short		lp_logcol;
	short		lp_physline;
	short		lp_maxcol;
	struct buf	*lp_inbuf;
	struct buf	*lp_outbuf;
	char		*lp_outp;
	short		lp_outn;
	char		lp_lastch;
	};

/* allocate storage for line info */
struct cp_lp lp_linfo[LP_BMAX][LP_LMAX];

/*
 *	Lineprinter driver parameters
 */
#define	LP_PRI		(PZERO+8)
#define	LP_BUFSIZE	(1024)
#define LP_TIME_CYCLE	20
#define LP_TIMEOUT	(2*60) / LP_TIME_CYCLE		/* 2 min timeout, 4 before message */
#define HIWORD(x)	(((unsigned)(x))>>16)
#define LOWORD(x)	(((unsigned)(x))&0xffff)

mblk_t	*cp_getbp();

#define CP_BR	 16			/* number of possible baud rates */
/*
 * baud rate conversion	:                           1  1  2  4  9  1  3
 *			          1  1  1  2  3  6  2  8  4  8  6  9  8
 *			    5  7  1  3  5  0  0  0  0  0  0  0  0  .  .
 *			 0  0  5  0  4  0  0  0  0  0  0  0  0  0  2  4 */
int cp_bconv0[CP_BR]={	 0, 0, 0, 1, 2, 3, 4, 4, 5, 6,10, 8, 9,11,12,15, };/**/
int cp_bconv1[CP_BR]={	 0, 0, 0, 1, 2, 3, 3, 4, 5, 6, 6, 8, 9,11,11,12, };/**/

extern int ss_bticks[16];
/*
 * streams stuff
 */
static struct module_info dum_info = {
	STRID_MUX,			/* module ID */
	"MUX",				/* module name */
	0,				/* minimum packet size */
	1024,				/* maximum packet size */
	128,				/* high water mark */
	16,				/* low water mark */
};

static int cp_open();
static struct qinit cp_rinit = {
	NULL, ss_rsrv, cp_open, ss_close, NULL, &dum_info, NULL
};

static struct qinit cp_winit = {
	ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab cpinfo = {&cp_rinit, &cp_winit, NULL, NULL};

static int lp_open();
static struct qinit lp_rinit = {
	NULL, ss_rsrv, lp_open, ss_close, NULL, &dum_info, NULL
};

static struct qinit lp_winit = {
	ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab lpinfo = {&lp_rinit, &lp_winit, NULL, NULL};

#ifdef DEBUG
#define CPDEBUG(x)	x
#else
#define CPDEBUG(x)
#endif DEBUG

static int cp_brk_tmr();
static cp_start_timer();
static lpinit();
int spltty();

static int linesfound = 0;
int cp_timeout = 0;

#ifdef R6000
/* This is a kludge to get around a problem with multiple I/O bus systems.
 * Each register access (read or write) to the serial controller takes on the
 * order of 12 to 16 microseconds (actually 20 to 24 microseonds if the
 * controller is currently VME bus master),
 * Writes may get queued up in the GBA and
 * when a read finally comes along, we must wait for all of the pending writes
 * to complete.  This time may be longer than 64 microseconds which causes
 * VME BERR on other VME busses on the system.  Delaying 32 microseconds
 * allows the buffer to be empty when the read is attempted.
 */

#define wbflush wait_for_sio
wait_for_sio()
{
  DELAY(32);
}
#endif R6000

cp_ctlr_ioinit(pl, dmaaddr, dmasize, stradr)
struct ss_line *pl;
ulong dmaaddr;
ulong dmasize;
ioaddr_t *stradr;
{

	struct ss_struct *pb = (struct ss_struct *)pl->pss;

	ASSERT(pb->ss_csh);

	/* 
	 * check if this line already has a map descriptor. If yes, then
	 * unmap it first and get it a new descriptor. When a vme_ioremap()
	 * routine is available , it can be called instead of unmap and map.
	 */
	if(pl->ss_sph)
		vme_iounmap(pl->ss_sph);

        if (!vme_iomap(pb->ss_csh, dmaaddr, dmasize,
	   GBA_CONTIG_ADDR+GBA_NOPART_MAP, &pl->ss_sph, stradr))
	cmn_err(CE_PANIC, "Can't map the data into GBA!\n");

 /*
  * Make sure that any data in the CPU cache has been written back to
  * physical memory.
  */
	 writeback_virtual_data(dmaaddr, dmasize );

}


/*
 * Probe the controller.  If it responds, initialize it.
 */
int
cpedtinit(e)
struct edt *e;
{
    register struct ss_struct *cpb;
    register struct ss_line   *cpl;
    register volatile struct cpdevice *cpaddr;
    register int    ctlr;
    register int    line;
    register int    numlines;    /* diagnostic output control */
    register int conf;
    struct cpp_s *cpp;

    /* 
     * Perform struct init. 
     */

    ctlr = e->e_intr_info->v_unit;
    if (ctlr >= CP_BMAX) {
	cmn_err(CE_WARN,"cp%d ignored: driver supports %d controllers, max",
		ctlr,CP_BMAX);
	return;
    }

    cpb = &cpboard[ctlr];
#ifndef R6000    
    cpb->ss_addr = (caddr_t)e->e_base;
#else
    /* Allow flexible VME bus config on 6000 */
    if (!(cpb->ss_addr =
	  (caddr_t)find_r6000_controller(e, 0, sizeof(cpaddr->cp_sel))))
      cpb->ss_addr = (caddr_t)e->e_base;  /* Not found -- restore addr */
#endif R6000
    cpb->ss_bconv  = cp_bconv0;
    cpb->ss_devdep = &cpdd;
    cpb->ss_lines = &cplines[ctlr][0];


    for (cpl = cpb->ss_lines, line=0;  line < CP_LMAX; cpl++, line++) {
	cpl->ss_line    = line;
	cpl->pss        = cpb;
	cpl->ss_lenable = 0;        /* disable */

	/* make local pointer point to associated storage area */
	cpp = &cp_linfo[ctlr][line];
	cpl->ss_llocal = (char *) cpp;

	/* remember line number address */
	cpp->pcp = cpb;
	cpp->pcl = cpl;
	cpp->tx_ints = 0;
	cpp->tx_timeouts = 0;
	cpp->v_vec = e->e_intr_info->v_vec;
	cpp->board_info = &cp_board_info[ctlr];
    }

    /* Now talk to the hardware */

    if (showconfig)
	cmn_err(CE_CONT,"cp%d: ",ctlr);
    numlines = 0;

    cpaddr = (struct cpdevice *) cpb->ss_addr;

    cpconf[ctlr] = 0;
#ifdef CP_SILO_DMA
    cp_board_info[ctlr].silo_buffer = 0;
    cp_board_info[ctlr].silo_bufsize = 0;
    cp_board_info[ctlr].silo_sph = 0;
    cp_board_info[ctlr].silo_ioaddr = 0;
    cp_board_info[ctlr].silo_mode = 0;
#endif CP_SILO_DMA
    cp_board_info[ctlr].ler_copy = 0;
    cp_board_info[ctlr].acr_copy = 0;
    cp_board_info[ctlr].brk_copy = 0;
    if (IOBADADDR (cpaddr, sizeof (cpaddr->cp_sel))) {
	cpb->ss_nlines = 0;
  	if (showconfig) {
	    cmn_err(CE_CONT,"not present at 0x%x\n", cpb->ss_addr);
	}
	return;
    } else {
  	if (showconfig) {
	    cmn_err(CE_CONT,"present at 0x%x ", cpb->ss_addr);
	}
    }

    /* 
     * reserve a cache section for the card. Store the handle in the
     * cpb struct. 
     */
    if(!vme_reserve_iomap(ctlr, cpaddr, 32, &cpb->ss_csh,
			  DMA_A24_AM | EBA_CS_AUTOEXP))
       cmn_err(CE_PANIC,"Couldn't allocate cache section in GBA !\n");


    cpaddr->cp_sel = SEL_MC;		wbflush();
    while (cpaddr->cp_sel & SEL_MC)
	DELAY(128);
    conf = cpaddr->cp_sel;
    cpconf[ctlr] = conf;
    cpb->ss_nlines = conf&SEL_CONF_NLINES;
    cpaddr->cp_sel = SEL_SILO;		wbflush();
	
    /*
     *	High 8 bits == silo high water mark, silo will then interupt
     *	Low 8 bits == silo aging time, ie interupt after this many 256ths of a second
     */
    cpaddr->cp_pr = (40 << 8) | 11;	wbflush();
    		/* silo fill level | silo age time */

#ifdef CP_SILO_DMA
    if (conf & 0x0800) 
      cp_board_info[ctlr].silo_mode = 1;
    if (showconfig)
      if (cp_board_info[ctlr].silo_mode)
	cmn_err(CE_CONT, " silo DMA enabled, ");
      else
	cmn_err(CE_CONT, " silo DMA disabled, ");
    if (cp_board_info[ctlr].silo_mode & 0x01)
      cp_silo_dma_setup(cpb);
#endif CP_SILO_DMA	
    cpaddr->cp_isr = (ISR_IE << 8) | (e->e_intr_info->v_vec);	wbflush();
    if (showconfig) {
	cmn_err(CE_CONT,"vector set to 0x%x\n",e->e_intr_info->v_vec);
    }
    if (conf & SEL_CONF_BR) {
        cpb->ss_bconv  = cp_bconv1;
    } else
	cpb->ss_bconv = cp_bconv0;
	
    for (cpl = cpb->ss_lines;
	 cpl < (cpb->ss_lines + cpb->ss_nlines);
         cpl++) {
		if (numlines == 0) {
                	if (showconfig)
		    		cmn_err(CE_CONT,"lines:");
                	numlines++;
                	linesfound++;
            	}

	    	if (showconfig)
                	cmn_err(CE_CONT," %d",(cpl - cpb->ss_lines));

            	cpl->ss_lenable = 1;
    }


    /* finish up line availability diagnostic output */
    if (showconfig) {
	if (numlines == 0)
	    cmn_err(CE_CONT,"NO LINES PRESENT \n");
	else
	    cmn_err(CE_CONT," PRESENT \n");
    }

    if (linesfound)
	cp_start_timer();

    lpinit(ctlr);
}

static int
cp_open (rq, dev, flag, sflag)
    queue_t    *rq;
    dev_t    dev;
    int    flag;
    int    sflag;
{
	register struct ss_struct *cpb;
	register struct ss_line      *cpl;    /* line of interest */
	register int line;
	int	status;
	int	was_open;

	if (ISLP(dev)) {
		return(lp_open(rq,dev,flag,sflag));
	}

	if (CPUNIT(minor(dev)) >= CP_BMAX)
		return(OPENFAIL);
	cpb = cpboard + CPUNIT(minor(dev));
	line = CPLINE(minor(dev));
	if (line >= cpb->ss_nlines)
		return(OPENFAIL);
	cpl = cpb->ss_lines + line;

	cp_linfo[cpb-cpboard][line].tx_ints =0;
	cp_linfo[cpb-cpboard][line].tx_timeouts=0;

    	return(ss_open (cpl, rq, dev, flag, sflag,IFLAGS));
}

/*
 * Activate the specified line
 * Return: true if Carrier is up
 */
cp_enable_line (cpl)
    register struct ss_line    *cpl;
{
    	register volatile struct cpdevice *cpaddr;
    	register ushort	ln;
	struct cpp_s *cpp;
	struct cp_board_info_type *binfo;

	cpp = (struct cpp_s *) cpl->ss_llocal;
	binfo = cpp->board_info;

	ln = (1 << cpl->ss_line);
    	cpaddr = (struct cpdevice *) (cpl->pss->ss_addr);

	/* Use & maintain copy of LER & ACR to avoid extra controller reads */
	cpaddr->cp_ler = (binfo->ler_copy |= ln);	
	wbflush();
	cpaddr->cp_acr = (binfo->acr_copy |= ln);
	wbflush();
	cpl->ss_modem_state |= (BSD43_TIOCM_LE |
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS);
#ifdef R6000
	/* This is a real kludge to handle multiple VME buses.  Empirical
	 * data shows the controller taking up to 30 microseconds per register
	 * access if the access is occuring while the controller is trying to
	 * use DMA for a data block.  To avoid hanging the read(s), wait for
	 * all writes to finish.
	 */
	DELAY( 32 * 4 );
#endif R6000	
	if (cpaddr->cp_drr & ln)
		cpl->ss_modem_state |= BSD43_TIOCM_RI;
	/*
	 * return DCD
	 */
	return(cpaddr->cp_dcr & ln);
}

/* 
 * Turn off the specified line 
 */
cp_disable_line (cpl)
    register struct ss_line *cpl;
{
    	register volatile struct cpdevice *cpaddr;
    	register ushort	ln;
	struct cpp_s *cpp;
	struct cp_board_info_type *binfo;

	cpp = (struct cpp_s *) cpl->ss_llocal;
	binfo = cpp->board_info;

	ln = (1 << cpl->ss_line);
    	cpaddr = (struct cpdevice *) (cpl->pss->ss_addr);

	cpaddr->cp_brk = (binfo->brk_copy &= ~ln);
	wbflush();
	cpaddr->cp_ler = (binfo->ler_copy &= ~ln);
	wbflush();
	cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
	wbflush();
	cpaddr->cp_pr = 0;
	wbflush();
	cpl->ss_cflag = 0;
	cpl->ss_modem_state &= ~(BSD43_TIOCM_LE | 
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS |
				BSD43_TIOCM_ST |
				BSD43_TIOCM_SR |
				BSD43_TIOCM_CD |
				BSD43_TIOCM_RI |
				BSD43_TIOCM_DSR);
}


/* 
 * This procedure outputs "len" chars pointed to by "cp" on line "cpl".
 * This procedure gets called from ss_tx.
 */
cp_outc(cpl, cs, len)
    register struct ss_line    *cpl;
    register char *cs;
    register int   len;
{
	register volatile struct cpdevice *cpaddr;
        struct cpp_s *cpp;
	register struct ss_struct *cpb;
	ioaddr_t stradr;
	int s;
	
    	if (len) {
        	sysinfo.outch += len;
        	cpl->ss_state |= SS_BUSY;
		cpb = (struct ss_struct *)cpl->pss;
		cpaddr = (struct cpdevice *)cpb->ss_addr;
		s= spltty();
		cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
		wbflush();

		/*
		 * map the data for this line and get an address that the
		 * controller can use to get data from.
		 */
		cp_ctlr_ioinit(cpl, cs, len ,&stradr);
                ASSERT( (((int)cs & POFFMASK) + len - 1) < NBPP ); 

		/* Use this for debugging in the tx intr routine */
		cpp= (struct cpp_s *)cpb->ss_lines->ss_llocal;
		cpp->stradr = stradr;

		cpaddr->cp_bah = HIWORD(stradr);
		wbflush();
		cpaddr->cp_bal = LOWORD(stradr);
		wbflush();
		cpaddr->cp_bc = 
		cpl->ss_lcc = len;
		wbflush();

		cpaddr->cp_tcr = 1 << cpl->ss_line;
		wbflush();
		splx(s);
	 }
}

#ifdef CP_SILO_DMA
/* 
 * This procedure sets up silo DMA.
 */
cp_silo_dma_setup(cpb)
     struct ss_struct *cpb;
{
     register volatile struct cpdevice *cpaddr;
     struct cpp_s *cpp;
     struct cp_board_info_type *binfo;
	
     cpaddr = (struct cpdevice *) cpb->ss_addr;
     cpp= (struct cpp_s *)cpb->ss_lines->ss_llocal;
     binfo = cpp->board_info;

     cpaddr->cp_sel = SEL_SILO;
     wbflush();

     /* 
      * check if this line already has a map descriptor. If not,
      * then map it once & for all making certain that any data in CPU
      * cache has been written back to physical memory.  Since we will
      * only read this data (i.e. controller will only write) we only need
      * a cpu cache invalidate before reading.
      */
     if(!binfo->silo_buffer) {
       binfo->silo_buffer =
	 (ushort *)kmemzalloc(4100, M_DEVBUF, M_NOWAIT|M_CONTIGUOUS);
       binfo->silo_bufsize = 4096;	/* Max number of input chars */
       /* The last 4 bytes of the buffer remain zero in order to handle the
	* case where valid input data completely fills the buffer.
	*/
       if (!vme_iomap(cpb->ss_csh, binfo->silo_buffer, binfo->silo_bufsize,
		      GBA_CONTIG_ADDR+GBA_NOPART_MAP, &binfo->silo_sph,
		      &binfo->silo_ioaddr))
	 cmn_err(CE_PANIC, "Can't map the silo area into GBA!\n");
       
       /*
	* Make sure that any data in the CPU cache has been written back to
	* physical memory.
	*/
       writeback_virtual_data(binfo->silo_buffer, binfo->silo_bufsize );
     }

     cpaddr->cp_bah = HIWORD(binfo->silo_ioaddr);
     wbflush();
     cpaddr->cp_bal = LOWORD(binfo->silo_ioaddr);
     wbflush();
     cpaddr->cp_bc = binfo->silo_bufsize;
     wbflush();

     cpaddr->cp_sr = 0x8000;	/* set Transfer Silo  (TS), clear TSD */
     wbflush();
     
}
#endif CP_SILO_DMA


/*
 * This procedure sets the baud, parity, ... line parameters contained in
 * "cflag" of the line "cpl".
 */
cp_setline (cpl, cflag)
    register struct ss_line    *cpl;
    int cflag;
{
	register volatile struct cpdevice *cpaddr;
	register uint diff;
	register ushort lpr;

	cpaddr = (struct cpdevice *)cpl->pss->ss_addr;

	lpr = (cpl->pss->ss_bconv[cflag & CBAUD] << 4) | 
		cpl->pss->ss_bconv[cflag & CBAUD];
	lpr |= (cflag & CSTOPB) ? PR_TWOSB : PR_ONESB;
	lpr |= (cflag & CSIZE) << 4;
	if (cflag & (PARENB)) {
		lpr |= PR_PENABLE;
		if (cflag & PARODD) {
			lpr |= PR_OPAR;
		}
	}

	/*
	 * always do XOFF quickly
	 */
	lpr |= PR_XOFF;
	cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
	wbflush();
	cpaddr->cp_pr = lpr;
	wbflush();
	return(0);
}


cp_modem_control(cpl,op,value)
	struct	ss_line	*cpl;
	int	op;
	int	value;
{
	register volatile struct cpdevice *cpaddr;
    	register ushort	ln;
	struct cpp_s *cpp;
	struct cp_board_info_type *binfo;

	cpp = (struct cpp_s *) cpl->ss_llocal;
	binfo = cpp->board_info;

	cpaddr = (struct cpdevice *)cpl->pss->ss_addr;
	ln = (1 << cpl->ss_line);

	switch (op) {
	case SS_MC_STARTBREAK:
		cpaddr->cp_brk = (binfo->brk_copy |= ln); 
		wbflush();
		break;

	case SS_MC_STOPBREAK:
		cpaddr->cp_brk = (binfo->brk_copy &= ~ln); 
		wbflush();
		break;

	case SS_MC_ENABLEFLOW:
		/* not available */
		break;

	case SS_MC_DISABLEFLOW:
		/* not available */
		break;

	case SS_MC_ENABLEDTR:
		cpaddr->cp_acr = (binfo->acr_copy |= ln);
		wbflush();
		break;

	case SS_MC_DISABLEDTR:
		cpaddr->cp_acr = (binfo->acr_copy &= ~ln);
		wbflush();
		break;

	default:
		break;
	}
	return(0);
}


cp_driver_control(cpl,op,value)
	struct	ss_line	*cpl;
	int	op;
	int	value;
{
	register volatile struct cpdevice *cpaddr;
    	register ushort	ln;
	int	s;
	int	count;
	register mblk_t *wbp;

	cpaddr = (struct cpdevice *)cpl->pss->ss_addr;
	ln = (1 << cpl->ss_line);

	switch (op) {
	case SS_DC_TIOCOUTQ:
		s = spltty();
		if (cpl->ss_state & SS_BUSY) {
			cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
			wbflush();
			count = (cpl->ss_lcc - cpaddr->cp_bc);
		} else
			count = 0;
		splx(s);
		return(count);

	case SS_DC_DISABLELINE:
		return(cp_disable_line(cpl));
		break;

	case SS_DC_ENABLELINE:
		return(cp_enable_line(cpl));
		break;

	case SS_DC_SDRVFLUSH:
		s = spltty();
		if (cpl->ss_state & SS_BUSY) {
			cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
			wbflush();
			cpaddr->cp_bc = 0;
			wbflush();
			cpl->ss_state &= ~SS_BUSY;
			if (cpl->ss_state & SS_XBUSY) {
				cpl->ss_state &= ~SS_XBUSY;
			} else if (wbp = cpl->ss_wbp) {
				wbp->b_rptr += cpl->ss_lcc;
			};				
			cpl->ss_lcc = 0;
		};
		splx(s);
		break;

	case SS_DC_DRAINOUTPUT:
		ss_wait_for_not_busy(cpl);
		delay2(ss_bticks[cpl->ss_cflag & CBAUD] + 2,STOPRI|PCATCH);
		break;

	case SS_DC_STOP:
	default:
		break;
	}
	return(0);
}


/* 
 * Hardware level interrupt procedures. 
 * Gather valid input and place in the silo. 
 * This data will be processed by cp_dispatch.
 */
cpintr(cp)
	int	cp; 
{
	struct	ss_struct *cpb;	
	register volatile struct cpdevice *cpaddr;
	ushort	isr;
	
	if (cp >= CP_BMAX) {
		cmn_err(CE_CONT, "cpintr:  unexpected interrupt (0x%x)\n",cp);
		return;
	};
	cpb = cpboard + cp;
	cpaddr = (struct cpdevice *)cpb->ss_addr;

	/* 
	 *  bug in cpu chip - causes load to dropped on interrupt 
	 *  so disable all interrupts
	 */
	isr = cpaddr->cp_isr;


	if (cp_print & CPDBG_PT1) {
               cmn_err(CE_CONT,"cpintr: isr = 0x%x\n",isr);
        }

	switch (isr & 0xFF) {
		case ISR_SI:
			cp_sint(cpb);
			break;
		
		case ISR_TI:
			cp_tint(cpb,CP_REAL_TINT);
			break;
		
		case ISR_CI:
			cp_cint(cpb);
			break;
		
		case ISR_PI:
			lpint(lpboard + cp);
			break;
		
		case ISR_RI:
			break;

		case ISR_NI:
			cp_bad_mem(cpb);
			break;
	}
	if (cp_print & CPDBG_PT1) {
		cmn_err(CE_CONT,"cpintr: end of interrupt\n");
	}


}

cp_bad_mem(cpb)
	register struct ss_struct *cpb;
{

#ifdef TODO
	ushort bal, bah;
	unsigned int addr;
	register volatile struct cpdevice *cpaddr;
	cpaddr = (struct cpdevice *)cpb->ss_addr;
	bah = cpaddr->cp_bah;
	bal = cpaddr->cp_bal;
	addr = (bah << 8 | bal) & 0xffff;
	cmn_err(CE_CONT,"cp.c: VME Bus Error or Controller Timeout, addr = 0x%x",addr);
#endif
	cp_timeout++;
	cmn_err(CE_CONT,"!cp.c: VME Bus Error or Controller Timeout, board %d\n", cpb - cpboard);
}


/*
 * silo service interrupt
 */
cp_sint (cpb)
	register struct ss_struct *cpb;
{
	register ushort c;
	register ushort silo_intr;
	register mblk_t	*bp;
	register volatile struct cpdevice *cpaddr;
	register struct ss_line *cpl;
	register struct cpp_s *cpp;
	register int	line_mask;
	register int	ll = -1;	/* last line */
	register uint	breaks = 0,	/* breaks occured this intrpt */
			any_brks = 0;	/* breaks to timeout for */
#ifdef CP_SILO_DMA
	ushort *silo_char;

#endif CP_SILO_DMA	

	sysinfo.rcvint++;
	silo_intr = 0;
	cpaddr = (struct cpdevice *) cpb->ss_addr;

	/*
	 * Loop fetching characters from the silo for this cp.
	 */
#ifdef CP_SILO_DMA
	/* Use any line in order to find the board_info.  If silo_mode
	 * indicates we're using silo dma then we need to flush the
	 * I/O cache & invalidate the CPU cache before reading the data.
	 */
        cpp= (struct cpp_s *)cpb->ss_lines->ss_llocal;
	if (cpp->board_info->silo_mode & 0x01) {
	  vme_ioflush( cpp->board_info->silo_sph,
		      cpp->board_info->silo_buffer, 
		      cpp->board_info->silo_bufsize );
	  invalidate_virtual_data(
		      cpp->board_info->silo_buffer, 
		      cpp->board_info->silo_bufsize );
	  silo_char = (ushort *)cpp->board_info->silo_buffer;
	} else
	  silo_char = (ushort *)&cpaddr->cp_swr;

	while ((c = *silo_char) & SWR_VDP) {
	  	if (cpp->board_info->silo_mode & 0x01) silo_char++;
#else
	while ((c = cpaddr->cp_swr) & SWR_VDP) {
#endif CP_SILO_DMA	  
		
		/*
		 * This will save a few instructions when we get multiple
		 * characters from one silo.
		 */
		if ((c & SWR_LN_MASK) != ll) {
		  	ll = c & SWR_LN_MASK;
			if ((ll >> SWR_LN_SHIFT) >= cpb->ss_nlines) {
				cmn_err(CE_CONT,
				    "cp%d:  silo input from unknown line %d",
				    (cpb - cpboard),
				    (ll >> SWR_LN_SHIFT));
				continue;
			};
			cpl = cpb->ss_lines + (ll >> SWR_LN_SHIFT);
			cpp = (struct cpp_s *) cpl->ss_llocal;
			line_mask = (1 << cpl->ss_line);
		}

		/*
		 * Remember that this line cause an interrupt so that
		 * we can enable the queue for this line.
		 */
		silo_intr |= line_mask;
		
		if (!(cpl->ss_state & SS_ISOPEN) || !(cpl->ss_cflag & CREAD)) {
			continue;
		}

		/*
		 * If this character is NOT a FE, then turn off the break
		 * bit for that port
		 */
		if (!(c & SWR_FE))
			breaks &= ~(line_mask);
	
		/*
		 * Start or stop output (if permitted)
		 */
		if (! ss_startstop(cpl,(c & 0xFF))) 
			continue;

		/*
		 * Data Overrun error
		 */
		if (c & SWR_DO) {
#ifdef MIPS_LOCAL
			cmn_err(CE_CONT,"!cp%d: line %d overflow\n", 
				    (cpb - cpboard), cpl->ss_line);
#endif
			cpp->overruns++;
		}

		/*
		 * Parity Error
		 */
		if (c & SWR_PE) {
#ifdef MIPS_LOCAL
			cmn_err(CE_CONT,"!cp%d: line %d parity error\n", 
				    (cpb - cpboard), cpl->ss_line);
#endif
			cpp->parity_errors++;
			if (IGNPAR & cpl->ss_iflag) {
				continue; 
			} else if (!(INPCK & cpl->ss_iflag)) {
				/* ignore input parity errors */
			} else if (cpl->ss_iflag & PARMRK) {
				ss_slowr(cpl, 0377);
				ss_slowr(cpl, 0);
			} else
				c = '\0';

		}
		
		if (c & SWR_FE) {
#ifdef MIPS_LOCAL
			cmn_err(CE_CONT,"!cp%d: line %d framing error\n", 
				    (cpb - cpboard), cpl->ss_line);
#endif
			cpp->framing_errors++;

			if (breaks & line_mask)
				continue;
			else
				breaks |= line_mask;

			/*
			 * Above gets rid of back to back breaks in this
			 * interrupt.  Below keeps track of non back to
			 * back breaks happening on other ports so to 
			 * set a timeout so that we receive no other breaks
			 * on these ports for break time + 1/2 quiet time
			 * seconds.
			 */

			if (!(cpb->ss_breaks & line_mask)) {
				cpb->ss_breaks |= line_mask;
				any_brks |= line_mask;
			} else
				continue;
			if (cpl->ss_iflag & IGNBRK)
                                continue;
                        if (cpl->ss_iflag & BRKINT) {
                                ss_mk_sig(cpl, SIGINT);
                                continue;
                        }
			if (cpl->ss_iflag & PARMRK) {
				ss_slowr (cpl, 0377);
				ss_slowr (cpl, 0);
			}
			c = '\0';

		} else if ((cpl->ss_iflag & ISTRIP) == 0 &&
			   (c & 0xFF) == 0377 &&
			   cpl->ss_iflag & PARMRK) {
			ss_slowr(cpl,0377);
		};			

		ss_inc(cpl,c);
		silo_intr &= ~line_mask; /* ss_inc also does qenable()'s */
      	}

	/*
	 * Do a timeout on any breaks that have happened
	 */

	if (any_brks) {
		timeout(cp_brk_tmr,(((cpb - cpboard) << 16) |
				    (any_brks & 0xFFFF)), HZ/4 + HZ/40);
	}

	if (silo_intr) {
		for (cpl = cpb->ss_lines;
		     cpl < (cpb->ss_lines + cpb->ss_nlines) && silo_intr;
		     cpl++) {
			line_mask = (1 << cpl->ss_line);
			if (silo_intr & line_mask) {
				silo_intr ^= line_mask;
				if ((cpl->ss_state & SS_ISOPEN) &&
				    cpl->ss_rq != 0 &&
				    canenable(cpl->ss_rq)) {
				    	qenable (cpl->ss_rq);
				}
			}
		}
	}
#ifdef CP_SILO_DMA
	if (cpp->board_info->silo_mode & 0x01)
	  cp_silo_dma_setup(cpb);
#endif CP_SILO_DMA	
}
	
/**** timers ****/
/*
 * Clear breaks on lines that are passed in info.  Breaks will now be
 * accepted on those ports.
 */

static int
cp_brk_tmr(info)
uint info;
{
	register uint port;

	port = (info >> 16) & 0xFFFF;
	cpboard[port].ss_breaks &= ~(info & 0xFFFF);

}



#define CPIDLE 0
#define CPALERT 1
static unsigned char cptimer_started = 0;

/* This will be called at interupt level spltty() */
static int
cptimer()
{
	struct ss_struct *cpb;
	struct ss_line *cpl;
	struct cpp_s *cpp;
	register s;
	register volatile struct cpdevice *cpaddr;

	cptimer_started = 0;
	for (cpb = cpboard; cpb < cpboard + CP_BMAX; cpb++) {
		for (cpl = cpb->ss_lines; 
		     cpl < (cpb->ss_lines + cpb->ss_nlines);
		     cpl++) {
			cpp = (struct cpp_s *) cpl->ss_llocal;
			cpaddr = (struct cpdevice *)cpb->ss_addr;
			if (cpl->ss_state & SS_BUSY) {
				if (cpp->cpp_timer_check == CPALERT) {
					cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
					wbflush();
					cpaddr->cp_bc = 0;
					wbflush();
					cp_tint(cpb,cpl->ss_line);
				        if (cp_print & CPDBG_PT1) {
					    cmn_err(CE_CONT,
			   "cptimer: dropped tx_intr on unit cp%d line %d\n",
				            (cpb - cpboard),cpl->ss_line);
					}
				    cp_reset_ints(cpb);
				} else {
					cpp->cpp_timer_check = CPALERT;
				}
			}
		}
	}
	cp_start_timer();
}
cp_reset_ints(cpb)
register struct ss_struct *cpb;
{
	struct ss_line *cpl;
	struct cpp_s *cpp;
	register volatile struct cpdevice *cpaddr = (struct cpdevice *)cpb->ss_addr;
	struct cp_board_info_type *binfo;

	cpl = cpb->ss_lines; 
	cpp = (struct cpp_s *) cpl->ss_llocal;
	binfo = cpp->board_info;

	cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
	wbflush();
    	cpaddr->cp_isr = (ISR_IE << 8) | cpp->v_vec;
	wbflush();

	cpaddr->cp_ler = (binfo->ler_copy |= (1 << cpl->ss_line));
	wbflush();
}

static
cp_start_timer()
{
	int s;

	s = splhi();
	if (! cptimer_started) {
		cptimer_started = 1;
		timeout_spl(cptimer, (caddr_t)0, 2.5 * HZ, spltty);	/* every 1 second */
	};
	splx(s);
}


/*
 * CP transmitter interrupt. Restart each line which used to be active but has
 * terminated transmission since the last interrupt.
 */
cp_tint(cpb,linenum)
	register struct ss_struct *cpb;
	register int linenum;
{
	register ushort line;
	register mblk_t *wbp;
	register volatile struct cpdevice *cpaddr;
	register ushort tcr;
	register struct ss_line *cpl;
	register int line_mask;
	register struct cpp_s *cpp;
	ioaddr_t tmp;
	int diff;
	int bc1, bc2;

	sysinfo.xmtint++;
	cpaddr = (struct cpdevice *)cpb->ss_addr;

	/*
	 * bug in cpu chip causes load to be dropped on interrupt 
	 * so disable interrupts.
	 */
	tcr = cpaddr->cp_tcr;

	if (linenum != CP_REAL_TINT)
		tcr |= (1 << linenum);

	for (cpl = cpb->ss_lines;
	     cpl < (cpb->ss_lines + cpb->ss_nlines) && tcr ;
	     cpl++) {
		line_mask = (1 << cpl->ss_line);
		cpp = (struct cpp_s *) cpl->ss_llocal;
		if (tcr & line_mask) {

			/* pacify the watchdog */
			cpp->cpp_timer_check = CPIDLE;
			if (cpl->ss_line != linenum)
				cpp->tx_ints++;
			else
				cpp->tx_timeouts++;

			tcr ^= line_mask;
			if (cpl->ss_state & SS_BUSY) {
				cpl->ss_state &= ~SS_BUSY;
			
				/*
				 * If the user process or line discipline
				 * sent ^S/^Q we won't have a write buffer
				 */
				if (cpl->ss_state & SS_XBUSY) {
				  	cpl->ss_state &= ~SS_XBUSY;

			/* If we were sending a data buffer then advance the rptr by
			   the number we wanted to send - the number that are still left
			   (for some reason).  It turns out, however, that the bc register
			   sometimes provides bogus info under certain condtions.  (This
			   site had very long cables > 100 ft). In this case , check the 
				 address registers to see if they are correct, otherwise just use
				 the lcc and ignore the bc.
			*/
				} else if (wbp = cpl->ss_wbp) {
					cpaddr->cp_sel = cpl->ss_line << SEL_SHIFT;
					wbflush();
					bc1 = (int)(cpaddr->cp_bc);
					if ((cpl->ss_lcc - bc1) < 0){
						/* bc register is hosed, lets see if the
						   address registers tell a better story */

						tmp = (cpaddr->cp_bah & 0xffff)<< 16;
						tmp |= (cpaddr->cp_bal & 0xffff);
						diff = tmp - cpp->stradr;
						if (diff == cpl->ss_lcc){ /* Address regs are OK */
#ifdef MIPS_LOCAL
							cmn_err(CE_CONT,"cp.c: bad bd, addrs regs are OK; recovered\n");
#endif
							wbp->b_rptr += cpl->ss_lcc - diff;
						}
						else{      /* Address regs are hosed also, nothing else to do */
#ifdef MIPS_LOCAL
							cmn_err(CE_CONT,"cp.c: lcc %d, addr_reg 0x%x, straddr 0x%x, bc %d\n",
							  cpl->ss_lcc, tmp, cpp->stradr, cpaddr->cp_bc); 
#endif
							wbp->b_rptr += cpl->ss_lcc; /* wing it and assume all characters went out*/
						}
						cpp->bad_bc++;
					}
					else{
						bc2 = (int)(cpaddr->cp_bc);
#ifdef MIPS_LOCAL
						if (bc2 != bc1)
							cmn_err(CE_CONT, "bc still moving bc1:%x bc2%x lcc:%x\n",
							   bc1, bc2, cpl->ss_lcc);
#endif MIPS_LOCAL

						wbp->b_rptr += cpl->ss_lcc - cpaddr->cp_bc;
					}
				}
				cpl->ss_lcc = 0;
			};

			/*
			 * Stop send output for now.  Wait for a ^Q.
			 */
			if (! (cpl->ss_state & SS_TXSTOP))
				ss_tx (cpl);
		}
	}
}

/*
 * carrier detect interrupt; deal with carrier transitions.
 */
cp_cint(cpb)
	register struct ss_struct *cpb;
{
	register ushort dcr;
	register struct ss_line *cpl;
	register ushort drr;

	sysinfo.mdmint++;
	dcr = ((volatile struct cpdevice *)
				cpb->ss_addr)->cp_dcr;
	drr = ((volatile struct cpdevice *)
				cpb->ss_addr)->cp_drr;
	for (cpl = cpb->ss_lines;
	     cpl < (cpb->ss_lines + cpb->ss_nlines); 
	     cpl++) {
		if (!(dcr & (1<<cpl->ss_line))) {
			if (cpl->ss_modem_state & BSD43_TIOCM_CD)
				ss_coff (cpl);
		} else {
			if (!(cpl->ss_modem_state & BSD43_TIOCM_CD))
				ss_con (cpl);
		}
		if (!(drr & (1<<cpl->ss_line))) {
			cpl->ss_modem_state &= ~BSD43_TIOCM_RI;
		} else {
			cpl->ss_modem_state |= BSD43_TIOCM_RI;
		};
	}
}



/*
 *	line printer driver
 */

static int
lp_open (rq, dev, flag, sflag)
    queue_t    *rq;
    dev_t    dev;
    int    flag;
    int    sflag;
{
	register struct ss_struct *lpb;
	register struct ss_line      *lpl;    /* line of interest */
	register int line, myminor;
	register struct cp_lp *lp;

	ASSERT((ISLP(dev)));

	myminor = minor(dev);
	if (LPUNIT(myminor) >= LP_BMAX) {
		u.u_error = ENXIO;
		return(OPENFAIL);
	};
	lpb = lpboard + LPUNIT(myminor);
	line = LPLINE(myminor);
	if (line >= lpb->ss_nlines) {
		u.u_error = ENXIO;
		return(OPENFAIL);
	};
	lpl = lpb->ss_lines + line;
	lp = (struct cp_lp *) lpl->ss_llocal;

	if (lpl->ss_state & SS_ISOPEN) {
		if (LPCANON(dev) != lp->lp_canon) {
			u.u_error = ENXIO;
			return(OPENFAIL);
		};
	} else {
		lp->lp_canon = LPCANON(dev);
	};
	lp->lp_lastch = -1;
	    
	return(ss_open (lpl, rq, dev, flag, sflag, IFLAGS));
}


static
lpinit(ctlr)
    register int ctlr;
{
    register struct ss_struct *lpb, *cpb;
    register struct ss_line   *lpl;
    register struct cpdevice *cpaddr;
    register int    line;
    register int    numlines;    /* diagnostic output control */
    struct cp_lp *lpp;

    /* 
     * Perform struct init. 
     */
    lpb = &lpboard[ctlr];
    cpb = &cpboard[ctlr];

    lpb->ss_addr = cpboard[ctlr].ss_addr;
    lpb->ss_bconv  = cp_bconv0;
    lpb->ss_devdep = &lpdd;
    lpb->ss_lines = &lplines[ctlr][0];
    lpb->ss_nlines = LP_LMAX;
    lpb->ss_csh = cpb->ss_csh;   /* let lp use the same cache section */


    for (line = 0; line < lpb->ss_nlines; line++) {	

	lpl = lpb->ss_lines + line;
        lpl->ss_line    = line;
        lpl->pss        = lpb;
        lpl->ss_lenable = 0;        /* disable */

        /* make make local pointer point to associated storage area */
        lpp = &lp_linfo[ctlr][line];
        lpl->ss_llocal = (char *) lpp;

        /* remember line number address */
        lpp->pcp = lpb;
        lpp->pcl = lpl;
    }

    if (showconfig)
            cmn_err(CE_CONT,"lpinit: Board %d: ",ctlr);
    numlines = 0;

    if ((cpconf[ctlr] & SEL_CONF_LP) == 0) {
    	lpb->ss_nlines = 0;
      	if (showconfig) {
	    printf("not present at 0x%x\n", lpb->ss_addr);
    	}
	return;
    }

    if (showconfig) {
    	printf("present at 0x%x\n", lpb->ss_addr);
    }
    lpb->ss_nlines = 1;

    for (lpl = lpb->ss_lines, line=0;
    	 lpl < (lpb->ss_lines + lpb->ss_nlines); lpl++, line++) {

       	lpp = (struct cp_lp *) lpl->ss_llocal;
    	lpp->lp_state |= LP_EXIST;
    	lpp->lp_maxcol = LP_MAXCOL;
    	
        if (numlines == 0) {
	    if (showconfig)
    	    	cmn_err(CE_CONT,"lines:");
            numlines++;
        }

        if (showconfig)
            cmn_err(CE_CONT," %d",line);

        lpl->ss_lenable = 1;
    }

    /* finish up line availability diagnostic output */
    if (showconfig) {
        if (numlines == 0)
            cmn_err(CE_CONT,"NO LINES PRESENT \n");
        else
            cmn_err(CE_CONT," PRESENT \n");
    }
}


/*
 * Activate the specified line
 * Return: true if Carrier is up
 */
lp_enable_line (lpl)
    register struct ss_line    *lpl;
{
	struct	cp_lp *lp;

	lp = (struct cp_lp *) lpl->ss_llocal;

	if (lp->lp_inbuf == NULL) {
		lp->lp_inbuf = geteblk(LP_BUFSIZE);
		lp->lp_outbuf = geteblk(LP_BUFSIZE);
		lp->lp_outp = lp->lp_outbuf->b_un.b_addr;
		lp->lp_outn = 0;
		lp->lp_logcol = lp->lp_physcol = 0;
		lp->lp_physline = 0;
#ifdef notdef
		lpcanon(lpl, '\f');
#endif
	};
	lpl->ss_modem_state |= (BSD43_TIOCM_LE |
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS |
				BSD43_TIOCM_CD |
				BSD43_TIOCM_DSR);
	return 1;
}

/* 
 * Turn off the specified line 
 */
static lp_disable_line_interface();

lp_disable_line (lpl)
    register struct ss_line *lpl;
{
	struct	cp_lp *lpp;

	lpp = (struct cp_lp *) lpl->ss_llocal;
	lp_disable_line_interface(lpl);

	if (lpp->lp_inbuf != NULL) {
		brelse(lpp->lp_inbuf);
		lpp->lp_inbuf = NULL;
	};
	if (lpp->lp_outbuf != NULL) {
		brelse(lpp->lp_outbuf);
		lpp->lp_outbuf = NULL;
	};
	lpl->ss_modem_state &= ~(BSD43_TIOCM_LE | 
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS |
				BSD43_TIOCM_CTS |
				BSD43_TIOCM_ST |
				BSD43_TIOCM_SR |
				BSD43_TIOCM_CD |
				BSD43_TIOCM_RI |
				BSD43_TIOCM_DSR);
}


static
lp_abort_output(lpl)
	struct 	ss_line *lpl;
{
	register int s;
	register volatile struct cpdevice *cpaddr = 
			(struct cpdevice *)(lpl->pss->ss_addr);
	register struct cp_lp *lp;

	s = spltty();
	lp = (struct cp_lp *) lpl->ss_llocal;
	if (lpl->ss_state & SS_BUSY) {
		cpaddr->cp_sel = SEL_LP;
		wbflush();
		cpaddr->cp_sr &= ~LSR_GO;
		wbflush();
		if ((lp->lp_state & LP_TOUT) == LP_TOUT) {
			untimeout (lp->lp_tid);
					/* forget stray timeout */
			lp->lp_state &= ~LP_TOUT;
		}
		if (lpl->ss_state & SS_BUSY) {
			wakeup((caddr_t) lpl);
			lpl->ss_state &= ~SS_BUSY;
			lpl->ss_state &= ~SS_WCLOSE;
		};
	}
	splx(s);
}


static
lp_disable_line_interface(lpl)
	struct	ss_line *lpl;
{
	lp_abort_output(lpl);
	lpl->ss_cflag = 0;
}


/* 
 * This procedure outputs "len" chars pointed to by "p" on line "lpl".
 * This procedure gets called from ss_tx.
 */
lp_outc(lpl, p, len)
    register struct ss_line    *lpl;
    register char *p;
    register int   len;
{
	register struct cp_lp *lp;
	register mblk_t *wbp;
	register u_char c;

	lp = (struct cp_lp *) lpl->ss_llocal;
	if (len > 0) {
		if ((signed char)lp->lp_lastch >= (signed char)0) {
			c = (u_char) lp->lp_lastch;
			lp->lp_lastch = -1;
			lpcanon(lpl,(unsigned int) c);
			if (lpl->ss_state & SS_BUSY)
				return;
		};
		for (; len > 0; len--, p++) {
			sysinfo.outch++;
			lpcanon(lpl,*p);
			if (lpl->ss_state & SS_BUSY) {
			        if (wbp = lpl->ss_wbp) {
			            /* account for character output */
					wbp->b_rptr = p;
			        }
				return;
			};
		};

	        /*
	         * If the user process or line discipline
	         * sent ^S/^Q we won't have a write buffer
	         */
	        if (wbp = lpl->ss_wbp) {
	            /* account for character output */
			wbp->b_rptr = p;
	        }

		if (lp->lp_outn > 0)
			lpflush(lpl,LP_TIMEOUT*HZ);
	};

}


lpcanon(lpl, c)
	register struct ss_line *lpl;
	register int c;
{
	register struct cp_lp *lp = (struct cp_lp *) lpl->ss_llocal;
	register int logcol, physcol;

	if (lp->lp_canon&LP_CANON_CAP) {
		if (c>='a' && c<='z')
			c += 'A'-'a'; 
		else
		    switch (c) {
			case '{':	c = '(';	break;

			case '}':	c = ')';	break;

			case '`':	c = '\'';	break;

			case '|':	c = '!';	break;

			case '~':	c = '^';
		    }
	}

	lpoutput(lpl, c);
	return;
}

lpoutput(lpl, c)
	struct ss_line *lpl;
	int c;
{
	register struct cp_lp *lp = (struct cp_lp *) lpl->ss_llocal;

	if (lpl->ss_state & SS_BUSY) {
		lp->lp_lastch = c;
	} else if (lp->lp_outn < LP_BUFSIZE) {
		lp->lp_outn++;
		*lp->lp_outp++ = c;
		return;
	} else {
		lp->lp_lastch = c;
		lpflush(lpl, LP_TIMEOUT*HZ);
	};
}

static lptout();

lpflush(lpl, time_val)
	struct ss_line *lpl;
	register int time_val;
{
	register int s;
	register struct cp_lp *lp = (struct cp_lp *) lpl->ss_llocal;
	register volatile struct cpdevice *cpaddr = 
			(struct cpdevice *)lpl->pss->ss_addr;
	ioaddr_t stradr;

	s = spltty();
	if (lp->lp_outn) {
		cpaddr->cp_sel = SEL_LP;
		wbflush();
		lp->lp_outp = lp->lp_outbuf->b_un.b_addr;
		
		/*
		 * map the data for this line and get an address that the 
		 * controller can use to get data from.
		 */
		cp_ctlr_ioinit(lpl, lp->lp_outp,lp->lp_outn ,&stradr);
                ASSERT( (((int)(lp->lp_outp) & POFFMASK) + lp->lp_outn - 1) < NBPP );
		cpaddr->cp_bah = HIWORD(stradr);
		wbflush();
		cpaddr->cp_bal = LOWORD(stradr);
		wbflush();
		cpaddr->cp_bc = lp->lp_outn;
		wbflush();
		lpl->ss_state |= SS_BUSY;
		if ((lp->lp_state & LP_TOUT) == 0) {
			lp->lp_state |= LP_TOUT;
			lp->lp_tid =
				timeout(lptout, (caddr_t)lpl, time_val);
		}

		cpaddr->cp_sr |= LSR_GO;
		wbflush();
	}
	splx(s);
}

lpint(lpb)
	struct ss_struct *lpb;
{
	register struct cp_lp *lp;
	register struct ss_line *lpl;

	lpl = lpb->ss_lines;
	lp = (struct cp_lp *)lpl->ss_llocal;

    	sysinfo.xmtint++;

	if ((lp->lp_state & LP_EXIST) == 0 &&
	    (lpl->ss_state & SS_BUSY) == 0) {
		cmn_err(CE_CONT,"lp%d: stray printer interrupt\n",
			(lpl->pss - lpboard));
		return;
	}
	lpl->ss_state &= ~SS_BUSY;
	lp->lp_state &= ~LP_NOCHANGE;
	if ((lp->lp_state & LP_TOUT) == LP_TOUT) {
		untimeout (lp->lp_tid);	/* forget stray timeout */
		lp->lp_state &= ~LP_TOUT;
	}
	if (lp->lp_state & LP_WAIT) {
		wakeup(lpl);
		lp->lp_state &= ~LP_WAIT;
	};
	lp->lp_outn = 0;
	if (! (lpl->ss_state & SS_TXSTOP))
		ss_tx(lpl);
}

/* The parrellel port will only report back a tx interrupt if there
   is a device on the other side.  Therefore whenever we send chars out
   this timeout routine starts cycling. It checks status and allows
   us to kill a process that would ordinarily wait for the interrupt
   before dieing.
*/

static lptout(lpl)
	struct ss_line *lpl;
{
	register struct cp_lp *lp = (struct cp_lp *) lpl->ss_llocal;
	register volatile struct cpdevice *cpaddr;
	register short status;
	static int counter = 0;

	cpaddr  = (struct cpdevice *)lpl->pss->ss_addr;
	if (cp_print & CPDBG_PT6)
		cmn_err(CE_CONT,"lptout lp%d ss_state %x lp_state %x ",
			(lpl->pss - lpboard), lpl->ss_state, lp->lp_state);

	if ((lpl->ss_state & SS_BUSY) == 0) {	/* no longer waiting */
		lp->lp_state &= ~LP_TOUT;
		counter = 0;
		return;
	}

	if ((lpl->ss_state & SS_WCLOSE)) {	/* We are busy & waiting to close*/
		freemsg(lpl->ss_wbp);	/* Make sure to free msg block before abrting */
		lpl->ss_wbp = NULL;
		lp_abort_output(lpl);
		counter = 0;
		return;
	}

	if (++counter == LP_TIME_CYCLE){
	    counter = 0;
	    if (lp->lp_state & LP_NOCHANGE) {
		cpaddr->cp_sel = SEL_LP;
		wbflush();
		status = cpaddr->cp_sr;
		cmn_err(CE_CONT,"lp%d: printer ", (lpl->pss - lpboard));

		if ((status&LSR_SEL) == 0)
			cmn_err(CE_CONT,"not online ");
		if (status&LSR_PAPE)
			cmn_err(CE_CONT,"out of paper ");
		if ((status&LSR_RDY) == 0)
			cmn_err(CE_CONT,"not ready ");

		cmn_err(CE_CONT,"\n");
		lp->lp_state &= ~LP_NOCHANGE;
		if (lp->lp_state & LP_WAIT) {
			lp->lp_state &= ~LP_TOUT;
			lp_abort_output(lpl);
			return;
		};
	    } else
		lp->lp_state |= LP_NOCHANGE;
	}
	timeout(lptout, (caddr_t)lpl, LP_TIMEOUT*HZ);
}


/*
 * This procedure sets the baud, parity, ... line parameters contained in
 * "cflag" of the line "lpl".
 */
lp_setline (lpl, cflag)
    register struct ss_line    *lpl;
    int cflag;
{
	return(0);
}


lp_modem_control(lpl,op,value)
	struct	ss_line	*lpl;
	int	op;
	int	value;
{
	return(0);
}


lp_driver_control(lpl,op,value)
	struct	ss_line	*lpl;
	int	op;
	int	value;
{
	struct cp_lp *lp = (struct cp_lp *) lpl->ss_llocal;
	int	s;
	int	count;
	struct cpdevice *cpaddr = (struct cpdevice *)lpl->pss->ss_addr;

	switch (op) {
	case SS_DC_TIOCOUTQ:
		s = spltty();
		if (lpl->ss_state & SS_BUSY) {
			cpaddr->cp_sel = SEL_LP;
			wbflush();
			count = (lp->lp_outn - cpaddr->cp_bc);
		} else
			count = lp->lp_outn;
		if ((signed char)lp->lp_lastch >= (signed char)0) 
			count++;
		splx(s);
		return(count);

	case SS_DC_DRAINOUTPUT: /* only callable from ss_close or ss_open */
		lpflush(lpl, HZ);	/* timeout quickly so we can close */
		ss_wait_for_not_busy(lpl);
		break;

	case SS_DC_DISABLELINE:
		return(lp_disable_line(lpl));
		break;

	case SS_DC_ENABLELINE:
		return(lp_enable_line(lpl));
		break;

	default:
		break;
	}
	return(0);
}


