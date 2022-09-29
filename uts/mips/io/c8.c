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
#ident	"$Header: c8.c,v 1.25.1.10.1.4.1.5 90/12/05 14:10:04 beacker Exp $"

/*
 *    c8 - terminal device driver for DSC COM-8 serial card.
 *
 *    Adapted from sample terminal driver in Microsoft XENIX 
 *    Software Development Guide, Appendix C.
 *
 *    D. E. Messinger, D. E. Germann, 1985-05-14.
 *    Copyright (c) 1985, DSC.
 */

static char c8_copyright[] = "Copyright (c) 1985, DSC.\n";


#include "sys/sbd.h"
#include "sys/cpu_board.h"
#include "sys/param.h"
#include "sys/types.h"
#include "sys/kmem.h"
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
#include "sys/edt.h"
#include "sys/ss.h"
#include "bsd43/sys/ioctl.h"
#include "sys/rambo.h"
#include "sys/grafreg.h"


extern void (*poll_funct)();
extern int c8_poll_loopcnt, c8_poll_mode;
static void c8_poll(), c8_dispatch(), c8_check_poll();
static volatile int c8_timeid;

int c8_rint_mode = 1;
int c8baseaddr;
int r3030_polling = 0;
unsigned long rambo_ticks23, rambo_ticks15;
unsigned long c8rambo_time, c8rambo_ticks, rambodelay();

#define DEBUGGING

/* registers */
#define RRDATA 0        /* received data */
#define RTDATA 0        /* transmitted data */
#define RSTATUS 5       /* status */
#define RMstat 6        /* modem status */
#define RMctrl 4        /* modem control */
#define RCtrl 3         /* line control */
#define RIENABL 1       /* interrupt enable */
#define RSPEED 0        /* data rate */
#define RIIR 2          /* interrupt identification */
#define RFIFO 2         /* FIFO enable */
#define RSCRATCH 7	/* Scratch register */

/* status register bits */
#define SRRDY    0x01        /* received data ready */
#define SOERR    0x02        /* received data overrun */
#define SPERR    0x04        /* received data parity error */
#define SFERR    0x08        /* received data framing error */
#define SBRKI    0x10        /* break interrupt */
#define STRDY    0x20        /* transmitter ready (tx holding reg empty) */
#define STEMT    0x40        /* transmitter empty (thr && tsr empty) */

/* modem status register bits */
#define MSDCTS   0x01        /* delta CTS */
#define MSDDSR   0x02        /* delta DSR */
#define MSTERI   0x04        /* trailing edge of ring indicator */
#define MSDDCD   0x08        /* delta DCD */
#define MSCTS    0x10        /* status of CTS */
#define MSDSR    0x20        /* status of DSR */
#define MSRI     0x40        /* status of RI */
#define MSDCD    0x80        /* status of carrier detect */

/* control register */
#define CBITS5    0x00        /* five bit characters */
#define CBITS6    0x01        /* six bit characters */
#define CBITS7    0x02        /* seven bit characters */
#define CBITS8    0x03        /* eight bit characters */
#define CDTR      0x01        /* data terminal ready */
#define CRTS      0x02        /* request to send */
#define CSTOP2    0x04        /* two stop bits */
#define CPARITY   0x08        /* parity on */
#define CEVEN     0x10        /* even parity */
#define CBREAK    0x40        /* send break (space) */
#define CDLAB     0x80        /* enable divisor latch access */

/* interrupt enable */
#define ERECV    0x01   /* receiver ready */
#define EXMIT    0x02   /* transmitter ready */
#define ERLSTAT  0x04   /* line status interrupt */
#define EMS      0x08   /* modem status change */
#define ALLINTS (EXMIT | EMS)
#define ENINTR   0x08   /* board interupt enable */

/* interrupt identification */
#define IMS     0x00   /* Modem interrupt active */
#define IACTIVE 0x01   /* when bit is 0=> interrupt is active */
#define IXMIT   0x02   /* Transmint interrupt active */
#define IRECV   0x04   /* Receive interrupt active */
#define IRLSTAT 0x06   /* recieve line status interrupt */
#define IMASK   0x07   /* Documentation lies. Upper bits are not alays zero */
#define IFIFOON 0x80   /* 1 => FIFO enabled; 0 => character mode or no fifo */

/* FIFO control register (write only) */
#define FENABLE	0x01	/* enable the fifo */
#define FRRESET	0x02	/* reset the receive fifo */
#define FXRESET	0x04	/* reset the transmit fifo */
#define FDMAMOD	0x08	/* DMA mode select */
#define FTRGRMASK 0xC0	/* mask for receiver trigger level */
#define FTRGR01	0x00	/* trigger level 1 byte */
#define FTRGR04	0x40	/* trigger level 4 bytes */
#define FTRGR08	0x80	/* trigger level 8 bytes */
#define FTRGR14 0xC0	/* trigger level 14 bytes */

/* status regigister info */
#define C8IDLE 0xff
#define c8devtoboard(x)    ((x>>3) & 3)
#define c8devtoline(x)  (x&7)


/* Minor device number decoding */
#define    C8UNIT(m)    (((m)>>3)&0x7)
#define    C8LINE(m)    ((m)&0x07)

#define C8_BMAX     	8    		/* Max number of boards */
#define C8_BMAX_3030	2  		/* Max for 3030 */
#define BDS_PER_INTR	C8_BMAX/2	/* We only support two interrupts */
#define C8_LMAX    	8    		/* Max lines per board  */
#define C8_OUTCMAX 	16    		/* Max number of chars which can be output by outc */

#define C8STATREG 	0x13e		/* interrupt status register */
#define C8STATREG2 	0x340		/* second status register */
#define C8ADDRINC 	0x40            /* inc to next board addr      */
#define C8_R2400_BASE	0x13000000
#define C8_R3030_BASE	0x10000000
#define RAT_CLR_INT	0x10400003	/* Clear the interrupt pending bit on the RAT
					   For 3030 only */

/* Hardcode these addresses for use when interrupts come in when the driver
isn't even initialized yet */
#define C8PORT1       0x140			/* current default until lboot */

#ifndef POLL_CONST
#define POLL_CONST 4
#endif

#define UPSHIFT		0
#define DOWNSHIFT	1
#define RAMBO_DEF_TIME	(unsigned long) 2000
#define RAMBO_DELTA	200
#define MIN_RAMBO_TIME	300
#define MAX_RAMBO_TIME	2400

int  min_rambo_time = MIN_RAMBO_TIME;

#define TICK0 0
#define TICK1 1		
#define TICK2 2	
#define TICK3 3
#define TICK4 4

/* For the 120 this had to be done in clock.c, but on the 3030 we can
take advantage of this handy microsecond callout table */
#define MAX_INTERVAL POLL_CONST		/* Poll MAX_INTERVAL times every POLL_CONST ticks */
#define MIN_INTERVAL 1			/* Only poll once every POLL_CONST ticks */
#define INVAL_INT	1
#define POLL_TBL_SIZE	5

struct entry {
	void 	(*func)();
};
extern struct entry poll_tbl[MAX_INTERVAL-MIN_INTERVAL + 2][POLL_TBL_SIZE]; 

int c8_enable_line(), c8_disable_line(), c8_outc(), c8_setline(), c8_modem_control();
int c8_driver_control();

struct ss_devdep_info c8dd =
        { "C8", SS_TTY, C8_BMAX, C8_LMAX, 1,
          NULL, NULL, c8_outc, c8_setline, c8_modem_control, 
	   c8_driver_control
        };

struct ss_devdep_info c8fdd =
        { "C8", SS_TTY, C8_BMAX, C8_LMAX, C8_OUTCMAX,
	    NULL, NULL, c8_outc, c8_setline, c8_modem_control, 
	   c8_driver_control
        };

struct ss_struct *c8board[C8_BMAX];	/* pointers to ss board structures */

int c8_bmax; 				/* Highest number board present */
int c8_speedmax;

int irqtab[2];			/* Store the IRQ levels of the boards */

/* Software managed silo */
#define SILOMAX 512

struct c8_silo {
	uchar	si_status;
	uchar	si_c;
};


/* 
 * The Digi Com 8 board can set a separate I/O address for each 
 * line. System configuration conventions will probably allocate
 * line addresses contiguously, however the driver will use an 
 * array to hold the address for each line.
 */

/*
 * Line local information connected to ss structure via ss_llocal
 */
 struct c8_linfo {
    int li_addr;			/* line address */
    int ctlr;				/* Board number */
#ifdef DEBUGGING
    int li_overflow;			/* count of silo overflows */
    int li_silomaxfull;			/* max silo count */
    int li_fifomaxfull;			/* max fifo count */
#endif
    struct c8_silo *li_rptr;		/* silo read pointer */
    struct c8_silo *li_wptr;		/* silo write pointer */
    struct c8_silo *li_silo_start;	/* start of silo */
    struct c8_silo *li_silo_limit;	/* limit of silo */
    struct c8_silo li_silo[SILOMAX];	/* actual silo */
};

struct c8_linfo  *c8_ddptr[C8_BMAX][C8_LMAX];	/* this gives a direct link to
						   c8_linfo, so we can debug it */

/* 
 * The digi board presents interrupt which are delivered to my procedure
 * Before the c8init procedure is called. Seems to me like a bug 
 * elsewhere in the system, but I shall protect myself anyway.
 *
 * c8_initdone == 0 => ignore interrupts. c8init has not completed.
 */
int c8_initdone;

int c8_poll_rate; 			/* Keep track of current polling rate */

#define C8_FIFO_SIZE	16
#define C8_H_WATER	C8_FIFO_SIZE  - 4
#define C8_L_WATER	1
int c8_fifomax;	/* keep track of how full the fifo is getting
			   on a current basis */
/*
 * streams stuff
 */
static struct module_info dum_info = {
    STRID_COM8,            /* module ID */
    "COM8",                /* module name */
    0,                /* minimum packet size */
    1024,                /* maximum packet size */
    128,                /* high water mark */
    16,                /* low water mark */
};



int c8_open();

static struct qinit c8_rinit = {
    NULL, ss_rsrv, c8_open, ss_close, NULL, &dum_info, NULL
};

static struct qinit c8_winit = {
    ss_wput, NULL, NULL, NULL, NULL, &dum_info, NULL
};

struct streamtab c8info = {&c8_rinit, &c8_winit, NULL, NULL};

/*
 * This alows for selected print statements to be turned on and off
 * while the kernel is runing.
 */
/* c8 debug points */
#define C8DBG_PT1    0x0001        /* interrupt status */
#define C8DBG_PT2    0x0002        /* transmitt flow */
#define C8DBG_PT3    0x0004        /* reciever flow */
#define C8DBG_PT4    0x0008        /* ioctl flow */
#define C8DBG_PT5    0x0010        /* DCD interrupts */
#define C8DBG_PT6    0x0020        /* if set, allow spurious intr prints */
#define C8DBG_PT7    0x0040        /* Polling stuff */
#define C8DBG_PT8    0x0080        /* To log errors or not */
int c8_print = (C8DBG_PT6|C8DBG_PT8);

/* baud rate conversion table */
#define C8_BR     16    /* number of possible baud rates */
int c8_bconv[C8_BR] = {
    /* 0    */    0,
    /* 50    */    2304,
    /* 75    */    1536,
    /* 110    */    1047,
    /* 134    */    857,
    /* 150    */    768,
    /* 200    */    576,
    /* 300    */    384,
    /* 600    */    192,
    /* 1200    */    96,
    /* 1800    */    64,
    /* 2400    */    48,
    /* 4800    */    24,
    /* 9600    */    12,
    /* EXTA    */    6,    /* 19200 */
    /* EXTB    */    3     /* 38.4 */ 
    };

/* How often to call the polling function.  Currently
   each polling interval is 2.5 ms.  This is the rate for
   38.4 ports.  9600 ports are polled at 10 ms intervals */

typedef struct conv_entry{
	short ticks;
	short rate;
} conv_entry;

typedef struct c8_conv_entry{
	struct conv_entry baud_conv[C8_BR];
} c8_conv_entry;

#define C8_NUM_CONV	2	/* Number of conversion tables */

c8_conv_entry	conv_tbl[C8_NUM_CONV] = {
/* Slowest */
/* 0     */  TICK0,0, 		/* 50    */  TICK4,4,
/* 75    */  TICK4,4, 		/* 110   */  TICK4,4,
/* 134   */  TICK4,4, 		/* 150   */  TICK4,4,
/* 200   */  TICK4,4, 		/* 300   */  TICK4,4,
/* 600   */  TICK4,4, 		/* 1200  */  TICK4,4,
/* 1800  */  TICK4,4, 		/* 2400  */  TICK4,4,
/* 4800  */  TICK4,4, 		/* 9600  */  TICK4,4,
/* 19.2  */  TICK2|TICK4, 2, 	/* 38.4  */  TICK1|TICK2|TICK3|TICK4 ,1,

/*fastest */
/* 0     */  TICK0,0, 		/* 50    */  TICK4,4,
/* 75    */  TICK4,4, 		/* 110   */  TICK4,4,
/* 134   */  TICK4,4, 		/* 150   */  TICK4,4,
/* 200   */  TICK4,4, 		/* 300   */  TICK4,4,
/* 600   */  TICK4,4, 		/* 1200  */  TICK4,4,
/* 1800  */  TICK4,4, 		/* 2400  */  TICK4,4,
/* 4800  */  TICK4,4, 		/* 9600  */  TICK2 |TICK4,2,
/* 19.2 */TICK1|TICK2|TICK3|TICK4 ,1,/*38.4 */ TICK1|TICK2|TICK3|TICK4 ,1
};

int			 myindex = C8_NUM_CONV-1;
struct conv_entry	*myconv = &(conv_tbl[C8_NUM_CONV-1].baud_conv[0]);

int c8_brd_poll[C8_BMAX];	/* Keeps max polling rate of any ports */

int c8_brktmr();

/* slow down. Don't hammer registers so fast */
#define c8_iodelay()    DELAY(50)

#define RAT_CONV(addr) ((addr & 0xffff) << 2) | 3	/* RAT bus conversion */


/*
 * Write I/O byte
 */
outb(addr, value)
    int addr;
    uchar value;
{
    register volatile uchar *ioaddr;

    if (IS_R3030)
	ioaddr = (volatile uchar *)PHYS_TO_K1((addr & 0xff000000) | RAT_CONV(addr));
    else
        ioaddr  = (volatile uchar *) PHYS_TO_K1(addr);

    *ioaddr = value;
    wbflush();
}
    

/*
 * Read I/O byte
 */
uchar
inb(addr, value)
    int addr;
    uchar value;
{
    register volatile uchar *ioaddr;


    if (IS_R3030)
	ioaddr = (volatile uchar *)PHYS_TO_K1((addr & 0xff000000) | RAT_CONV(addr));
    else
         ioaddr = (volatile uchar *) PHYS_TO_K1(addr);

    value  = *ioaddr;
    return (value & 0xff);
}


c8_open (rq, dev, flag, sflag)
    queue_t    *rq;
    dev_t    dev;
    int    flag;
    int    sflag;
{
    register struct ss_struct *c8b;
    register struct ss_line      *c8l;    /* line of interest */
    register struct c8_linfo  *c8li;
    register int line, myminor;
    register int     c8addr;
    int 	board;

    myminor = minor(dev);
    board = C8UNIT(myminor);
    c8b = c8board[board];
    line = C8LINE(myminor);

    if (board >= c8_bmax  ||  !c8b	||
	line >= c8b->ss_nlines ||
        (c8b->ss_lines + line)->ss_lenable == 0) 
            return(OPENFAIL);

    c8l = c8b->ss_lines + line;

    c8addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;
    if ((c8l->ss_state & SS_ISOPEN) == 0) { /* only do this on first open */
	if (c8_poll_mode)
	    outb(c8addr + RIENABL, ALLINTS);  /* allow ints for this port */
	else
	    outb(c8addr + RIENABL, ALLINTS|ERECV);  /* allow ints for this port */
    }

    line = ss_open (c8l, rq, dev, flag, sflag, IFLAGS);

    if (c8_poll_mode) {
        c8_reset_poll(c8l,c8l->ss_cflag & CBAUD);
    }


    return (line);
}


c8edtinit(e)
struct edt *e;
{
    register struct ss_struct *c8b;
    register struct ss_line   *c8l;
    register struct c8_linfo  *c8li;
    register int    c8addr;
    register int    line;
    register int    numlines;    /* diagnostic output control */
    register unsigned short newimr;
    register int    linesfound=0;
    int c8_fifo;
    caddr_t	    tmp_bd_addr;
    int		    tmp_li_addr;
    int 	    counter, max_bds;
    int ctlr;
    static int edt_already_called = 0;
    void c8_monitor();

    /* select proper memory mapping */
    if (IS_R3030){
	if (check_for_color()){
		if (showconfig) cmn_err(CE_CONT, "c8init: Video board present\n");
		return;
	}
	c8baseaddr = C8_R3030_BASE;
	max_bds = C8_BMAX_3030;
    } else {
	c8baseaddr = C8_R2400_BASE;
	max_bds = BDS_PER_INTR;
    }
    if (e->e_base == C8PORT1)   /* board 1 - 4 */
	ctlr = 0;
    else
	ctlr = BDS_PER_INTR;

    for (counter = 0; counter < max_bds ; counter++,ctlr++) {

	if (showconfig) cmn_err(CE_CONT,"c8init: Board %d: ",ctlr);
        numlines = 0;
	c8_fifo = 1;

	c8_brd_poll[ctlr]= 0;

	/* See how many boards we have */

	tmp_bd_addr = (caddr_t)((c8baseaddr + e->e_base) + (counter * C8ADDRINC));

	/* For some reason this method doesn't work on the 3030 */
        if (!IS_R3030){
    	    outb((int)tmp_bd_addr + RCtrl, 0x55);
	    outb((int)tmp_bd_addr + RMctrl, 0x0); 
	    if (inb((int)tmp_bd_addr + RCtrl) != 0x55) {
		if (showconfig) cmn_err(CE_CONT,"NOT PRESENT\n");
		c8board[ctlr] = 0;
		continue;
	    }
         }else{
	    outb((int)tmp_bd_addr + RSCRATCH, 0x55);
	    if (inb((int)tmp_bd_addr + RSCRATCH) != 0x55) {
		if (showconfig) cmn_err(CE_CONT,"NOT PRESENT\n");
		c8board[ctlr] = 0;
		continue;
	    }
         }
	/* edt may get called out of order (ie ctlrs 4-7 first, then 0-3) */
	if (c8_bmax < ctlr + 1)
		c8_bmax = ctlr + 1;
	c8board[ctlr] = (struct ss_struct *)kmemzalloc(sizeof(struct ss_struct),0,0);
	c8b = c8board[ctlr];
	if (c8b == NULL)
		cmn_err(CE_WARN,"Cannot malloc for c8board[%d]",ctlr);
	c8b->ss_lines = (struct ss_line *)kmemzalloc(sizeof(struct ss_line)*C8_LMAX,0,0);
	if (c8b->ss_lines == NULL)
		cmn_err(CE_WARN,"Cannot malloc for c8board[%d]",ctlr);
        c8b->ss_bconv  = c8_bconv;
        c8b->ss_devdep = &c8dd;
        c8b->ss_addr = tmp_bd_addr;
        c8b->ss_nlines = 0;
        c8b->ss_breaks = (uint) 0;
        c8b->ss_csh = (ulong) 0;

	for (c8l = c8b->ss_lines,line = 0; line <C8_LMAX; c8l++,line++){
            c8l->ss_line    = line;
            c8l->pss        = c8b;
            c8l->ss_lenable = 0;        /* disable */

            /* make local pointer point to associated storage area */
            c8l->ss_llocal = (char *)kmemzalloc(sizeof(struct c8_linfo),0,0); 
	    if (c8l->ss_llocal == NULL)
		cmn_err(CE_WARN,"Cannot malloc for c8board[%d]",ctlr);
            c8li = (struct c8_linfo *)c8l->ss_llocal;

	    c8_ddptr[ctlr][line] = (struct c8_linfo *)c8l->ss_llocal;

            /* initialize line local info */
            c8li->li_addr = (int)c8b->ss_addr + (line * 8);
            c8li->li_rptr = c8li->li_wptr = c8li->li_silo_start =
			&c8li->li_silo[0];
	    c8li->li_silo_limit = &c8li->li_silo[SILOMAX];
	    c8li->ctlr = ctlr;

            c8addr = c8li->li_addr;

	    if (line != 0){ 		/* we have already talked to 0 */
		    outb(c8addr + RCtrl, 0x55);
		    outb(c8addr + RMctrl, 0x0); 
		    if (inb(c8addr + RCtrl) != 0x55) {
			cmn_err(CE_CONT,"Nonexistant port %d",line);
			continue;
		    }
	    }
            if (numlines == 0) {
                if (showconfig) cmn_err(CE_CONT,"lines:");
                linesfound++;
            }
	    numlines++;
            c8l->ss_lenable = 1;

	    if (showconfig) cmn_err(CE_CONT," %d",line);

            /*
             * Disable all interrupts. Interupts are not enabled until
             *    open. The board likes to generate premature interrupts.
             * Enable "Board" interrupts for this line. 
             */
            outb(c8addr + RCtrl, 0);       /* Line control register */
            outb(c8addr + RIENABL, 0);     /* disable ints for this port */
	    c8_iodelay();
            outb(c8addr + RMctrl, ENINTR); /* allow board interrupts */
            outb(c8addr + RFIFO, FENABLE);       /* FIFO enable */
	    c8_iodelay();
	    if (inb(c8addr + RIIR) & IFIFOON) {
		if (showconfig)
		    cmn_err(CE_CONT,"F");
	    } else {
		c8_fifo = c8_poll_mode = 0;
	    }
	}
	if (c8_fifo)
		c8b->ss_devdep = &c8fdd;		

        /* finish up line availability diagnostic output */
	if (showconfig) {
            if (numlines == 0)
                cmn_err(CE_CONT,"NO LINES PRESENT \n");
            else
                cmn_err(CE_CONT," PRESENT \n");
	}
	c8b->ss_nlines = numlines;
	
    }
    /* allow interrupts at this point */
    if (linesfound) {
	if (e->e_base == C8PORT1)   /* board 1 - 4 */
		irqtab[0] = e->e_atbusintr_info->a_irq;
	else
		irqtab[1] = e->e_atbusintr_info->a_irq;

        irq_unmask(e->e_atbusintr_info->a_irq);
        newimr = *(volatile unsigned short *)PHYS_TO_K1(IMR);

	if (showconfig)
            cmn_err(CE_CONT,"c8init: Unmask IRQ %d, new IMR = %x\n", 
                e->e_atbusintr_info->a_irq, newimr);

         c8_initdone = 1;
    }
    if (IS_R3030){
	    /* Find out how many rambo ticks for 2.5 ms */
	    c8rambo_time = RAMBO_DEF_TIME;
	    c8rambo_ticks = rambodelay(c8rambo_time);
    }

    /* Get ready to poll, but don't until a port is opened */
    if (linesfound && c8_poll_mode) {
		c8_poll_rate = 0;
		poll_funct = c8_poll;
		if (!edt_already_called){
			c8_monitor();
			edt_already_called++;
		}
    }

}

/*
 * Activate the specified line
 * Return: true if Carrier is up
 */
c8_enable_line (c8l)
    register struct ss_line    *c8l;
{
    register int addr;
    register int stat;

    addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;

    outb(addr + RFIFO, FRRESET|FENABLE);

    c8_iodelay();
    outb(addr+RMctrl, inb(addr+RMctrl) | CDTR | CRTS);

    stat = inb(addr+RMstat);
    c8l->ss_modem_state |= (BSD43_TIOCM_LE |
				BSD43_TIOCM_DTR |
				BSD43_TIOCM_RTS);
    if (stat & MSCTS) 
	c8l->ss_modem_state |= BSD43_TIOCM_CTS;
    else
	c8l->ss_modem_state &= ~BSD43_TIOCM_CTS;
    
    if (stat & MSDCD) 
        return(1);
    else
        return(0);
}

/* 
 * Turn off the specified line 
 */
c8_disable_line (c8l)
register struct ss_line *c8l;
{
    register int addr;
    register struct ss_struct *c8b;
    register struct ss_line *temp;

    addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;

    outb(addr+RMctrl, inb(addr+RMctrl) & ~CRTS);
    c8l->ss_modem_state &= ~(BSD43_TIOCM_LE | 
			BSD43_TIOCM_RTS |
			BSD43_TIOCM_ST |
			BSD43_TIOCM_SR |
			BSD43_TIOCM_CD |
			BSD43_TIOCM_RI |
			BSD43_TIOCM_DSR);

    if (c8_poll_mode) {
	    c8_reset_poll(c8l,0);
    }
}


/* 
 * This procedure outputs "len" chars pointed to by "cs" on line "c8l".
 * This procedure gets called from ss_tx.
 */
c8_outc(c8l, cs, len)
    register struct ss_line    *c8l;
    register char *cs;
    register int   len;
{
    register int addr;

    /* the Digi C8 board can only do ssdd_maxoutc char at a time */
    ASSERT(len <= c8l->pss->ss_devdep->ssdd_maxoutc);
        
    if (len) {
        sysinfo.outch += len;
        c8l->ss_state |= SS_BUSY;

	c8l->ss_lcc = len;
        addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;
	wbflush();
	while (len--) {
	    outb(addr+RTDATA, *cs++);
	}
    }
}


/*
 * This procedure sets the baud, parity, ... line parameters contained in
 * "cflag" of the line "c8l".
 */
c8_setline (c8l, cflag)
    register struct ss_line    *c8l;
    int cflag;
{
    register int addr;
    register int temp;
    register struct c8_linfo   *c8li;

    addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;

    /* set up speed */
    outb(addr+RCtrl, inb(addr+RCtrl) | CDLAB);
    c8_iodelay();

    temp = c8l->pss->ss_bconv[cflag & CBAUD];

    outb(addr+RSPEED, temp & 0xff);
    c8_iodelay();
    outb(addr+RSPEED+1, temp >> 8);
    c8_iodelay();
    outb(addr+RCtrl, inb(addr+RCtrl) & ~CDLAB);
    c8_iodelay();

    if (c8_poll_mode) {
	c8_reset_poll(c8l,cflag & CBAUD);
    }

    /* set up line control */
    temp = (cflag & CSIZE) >> 4;    /* length */
    if (cflag & CSTOPB)
        temp |= CSTOP2;
    if (cflag & PARENB) {
        temp |= CPARITY;
        if ((cflag & PARODD) == 0)
            temp |= CEVEN;
    }
    outb(addr+RCtrl, temp);
    return(0);
}


c8_modem_control(c8l,op,value)
	struct	ss_line	*c8l;
	int	op;
	int	value;
{
    	register int addr;

    	addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;

	switch (op) {
	case SS_MC_STARTBREAK:
	    	outb(addr+RCtrl, inb(addr+RCtrl) | CBREAK);
		break;

	case SS_MC_STOPBREAK:
	    	outb(addr+RCtrl, inb(addr+RCtrl) & ~CBREAK);
		break;

	case SS_MC_ENABLEFLOW:
	    	outb(addr+RMctrl, inb(addr+RMctrl) | CRTS);
		break;

	case SS_MC_DISABLEFLOW:
	        outb(addr+RMctrl, inb(addr+RMctrl) & ~CRTS);
		break;

	case SS_MC_ENABLEDTR:
	    	outb(addr+RMctrl, inb(addr+RMctrl) | CDTR);
		break;

	case SS_MC_DISABLEDTR:
	        outb(addr+RMctrl, inb(addr+RMctrl) & ~CDTR);
		break;

	default:
		break;
	}
	return(0);
}


extern int ss_bticks[16];

c8_driver_control(c8l,op,value)
	struct	ss_line	*c8l;
	int	op;
	int	value;
{
    	register int addr;
	int	s;

    	addr = ((struct c8_linfo *) c8l->ss_llocal)->li_addr;

	switch (op) {
	case SS_DC_TIOCOUTQ:
		return((c8l->ss_state & SS_BUSY) ? 1 : 0);

	case SS_DC_DISABLELINE:
		return(c8_disable_line(c8l));
		break;

	case SS_DC_ENABLELINE:
		return(c8_enable_line(c8l));
		break;

	case SS_DC_DRAINOUTPUT:
		ss_wait_for_not_busy(c8l);
		while ((inb(addr+RSTATUS) & STEMT) == 0) {
			if (delay2(ss_bticks[c8l->ss_cflag & CBAUD] + 2,
				   STOPRI|PCATCH))
				break;
		}
		break;
		
	case SS_DC_SDRVFLUSH:
		s = spltty();
		if (c8l->ss_state & SS_BUSY) {
			c8l->ss_state &= ~SS_BUSY;
			if (c8l->ss_state & SS_XBUSY) {
				c8l->ss_state &= ~SS_XBUSY;
			} else if (c8l->ss_wbp) {
				c8l->ss_wbp->b_rptr += c8l->ss_lcc;
			};				
			c8l->ss_lcc = 0;

			/* Now if there is a fifo on this port, flush it */
			if (c8l->pss->ss_devdep->ssdd_maxoutc > 1) {
				outb(addr + RFIFO, FXRESET|FENABLE);
			}
			/* Wait for last character to drain, else we'll trash
			 * the first char of the next open
			 */
			if (value == SS_FLUSHCANSLEEP) {
			    while ((inb(addr+RSTATUS) & STEMT) == 0) {
				delay2(ss_bticks[c8l->ss_cflag & CBAUD] + 2,
				       PZERO);
			    }
			}
		};
		splx(s);
		break;

	default:
		break;
	}
	return(0);
}


/* 
 * Hardware level interrupt procedures. 
 *
 * Note: The 3030 RAT bus simulates edge triggered interrupts.  Therefor
 * the RAT will hold the interrupt line active until it is cleared by writing
 * to RAT_CLEAR_INT. However one must watch for the following scenerio which
 * will cause the interrupt handler to be called for no reason.  The interrupt comes in,
 * interrupt handler is called, it writes RAT_CLEAR_INT, and starts draining the onboard
 * fifo.  WHile this is happening another port interrupts, causing the interrupt
 * line to go active.  The digi c8intr will now service both ports (because it services
 * ports based on the status registers, not interrupts) and exits when done.  However the
 * interrupt line is still active, so the handler gets called even though nothing is pending
 * anymore.  This routine must guarantee that ioaddr is clear when it exits.
 */
c8intr(vec)
int vec;
{
    register int dev;
    register int validint=0;
    register uchar  iir;
    register uchar  status, c; 		/* line status and character */
    register struct ss_line    *c8l;
    register struct c8_linfo   *c8li;
    register board, line;
    register int addr;
    register int statreg;
    int k, s;
#ifdef DEBUGGING
    int count;
#endif
    int spltty();
    volatile unsigned char *ioaddr = (unsigned char *)(PHYS_TO_K1(RAT_CLR_INT)); 
    
    if (IS_R3030 || (vec == irqtab[0]))
	    statreg = C8STATREG;
    else
	    statreg = C8STATREG2;

    if (c8_initdone == 0) {
	c8_gobbleint(statreg);
	return;
    }

    for (dev = inb(c8baseaddr+statreg); dev != C8IDLE; dev = inb(c8baseaddr+statreg)) {
        validint = 1;
	if (IS_R3030 || (vec == irqtab[0]))
		board = c8devtoboard(dev);
	else
		board = c8devtoboard(dev) + BDS_PER_INTR;

        line  = c8devtoline(dev);
        c8l  = c8board[board]->ss_lines + line;
        c8li = (struct c8_linfo *) c8l->ss_llocal;
        addr = c8li->li_addr;

        while (((iir = (inb(addr+RIIR) & IMASK)) & IACTIVE) == 0) {
            switch (iir) {
                case IRECV:
                    status = inb(addr+RSTATUS);
                    c = inb(addr+RRDATA);
		    if (poll_funct) {
			cmn_err(CE_CONT,"\nc8[%d] line %d: unexpected ",
				board,line);
			cmn_err(CE_CONT,"receive interrupt\n");
		    } else {
			if (c8_rint_mode) {
			    k = 1;
			    c8_putsilo(c8li,status,c);
			    while ((status = inb(addr+RSTATUS)) & SRRDY) {
				++k;
				c = inb(addr+RRDATA);
				c8_putsilo(c8li,status,c);
			    }
#ifdef DEBUGGING
			    if (k > c8li->li_fifomaxfull )
				c8li->li_fifomaxfull = k;
			    if ((count = c8li->li_wptr - c8li->li_rptr) < 0)
				count += SILOMAX;
			    if (count > c8li->li_silomaxfull)
				c8li->li_silomaxfull = count;
#endif
			    if (!c8_timeid)
				c8_timeid = timeout_spl(c8_dispatch,0,HZ/30,spltty);
			} else {
			    s = spltty();
			    c8_poll();
			    splx(s);
			}
		    }
                    break;

                case IXMIT:
                    status = inb(addr+RSTATUS);
                    if (!c8l->ss_lenable) {
                        cmn_err(CE_CONT,"\nc8[%d] line%d: unexpected ", 
                                board, line);
                        cmn_err(CE_CONT,"transmit interrupt\n" );
                    } else {
                        c8_tint(c8l, status);
                    }
                    break;

                case IMS:
                    status = inb(addr+RMstat);
                    if (!c8l->ss_lenable)  {
                        cmn_err(CE_CONT,"\nc8[%d] line%d: unexpected ", 
                                board, line);
                        cmn_err(CE_CONT,"modem interrupt\n" );
                    } else {
                        c8_cint(c8l, status);
                    }
                    break;

                default:
                    cmn_err(CE_CONT,"\nc8[%d][%d]: ",board,line);
                    cmn_err(CE_CONT,"OTHER interrupt %x\n",iir);
                    break;
            }
        }
	if (IS_R3030)		
		*ioaddr = 1;
    }

    if (!validint) {
        if (c8_print & C8DBG_PT6)
            cmn_err(CE_CONT,"c8: spurious interrupt\n");
    }
}

/* 
 * This procedure is called from r2400_ins to process the input
 * which has been gathered by c8intr.
 */
static void
c8_dispatch()
{
    uchar  c;
    uchar  status;    /* line status */
    register struct ss_struct  *c8b;
    register struct ss_line    *c8l;
    register struct c8_linfo   *c8li;
    static int i = 0;
    register int x;

    c8_timeid = 0;

    for (x=0; x < c8_bmax ;++x) {
	i = ++i % c8_bmax;
	c8b = c8board[i];

	if (c8_brd_poll[i] == 0 && c8_poll_mode)	/* Nothing to poll */
		continue;

	for (c8l=c8b->ss_lines;c8l < (c8b->ss_lines + c8b->ss_nlines); ++c8l) {
	    c8li = (struct c8_linfo *)c8l->ss_llocal;
	    for (; c8li->li_rptr != c8li->li_wptr; ) {
		c8_getsilo(c8li, &status, &c);
                c8_rint(c8l, c, status);
	    }
	}
    }
    i = ++i % c8_bmax;  	/* Start at the next board next time around */
}

/*
 * Gobble up premature interrupts. Use hardcoded addresses since nothing
   is setup yet.
 */
c8_gobbleint(statreg)
int statreg;
{
    register int dev;
    uchar  iir;
    uchar  status;    /* line status */
    struct ss_line    *c8l;
    register board, line;
    register int addr;
    register int stat_addr;	/* interrupt status reg */
    register int c8baseaddr;

    if (IS_R3030){
	c8baseaddr = C8_R3030_BASE;
    } else {
	c8baseaddr = C8_R2400_BASE;
    }

    stat_addr = c8baseaddr + statreg;
    for (dev=inb(stat_addr); dev != C8IDLE ;dev=inb(stat_addr)) {
        board = c8devtoboard(dev);
        line  = c8devtoline(dev);

        cmn_err(CE_CONT,"\nc8[%d][%d]: ",board,line);

	addr = c8baseaddr + C8PORT1 + (board * C8ADDRINC) + line*8;

        while (((iir = (inb(addr+RIIR) & IMASK)) & IACTIVE) == 0) {
            switch (iir) {
                case IRLSTAT:
                case IRECV:
                    status = inb(addr+RSTATUS);
                    status = inb(addr+RRDATA);
	    cmn_err(CE_CONT,"Illegal Recieve interrupt: cycle system power to reset board\n");
                    break;
                case IXMIT:
                    status = inb(addr+RSTATUS);
	    cmn_err(CE_CONT,"Illegal Transmit interrupt: cycle system power to reset board\n");
                    break;
                case IMS:
                    status = inb(addr+RMstat);
	    cmn_err(CE_CONT,"Illegal Modem interrupt: cycle system power to reset board\n");
                    break;
                default:
                    break;
            }
        }
    }
}

/*
 * Place a single event on the silo 
 */
c8_putsilo (c8li, status, c)
    register struct c8_linfo *c8li;
    register uchar          status;
    register uchar          c;
{
    register struct c8_silo *siloptr;

    siloptr = c8li->li_wptr;
    siloptr->si_status = status;
    siloptr->si_c      = c;

    if (++c8li->li_wptr == c8li->li_silo_limit)
	c8li->li_wptr = c8li->li_silo_start;

    if (c8li->li_wptr == c8li->li_rptr) {
#ifdef DEBUGGING
	++c8li->li_overflow;
#endif
	c8li->li_wptr--;
	if (c8li->li_wptr < c8li->li_silo_start)
	    c8li->li_wptr = c8li->li_silo_start;
    }
}

/* 
 * Remove a single event from the silo
 */
c8_getsilo (c8li, status, c)
    register struct c8_linfo *c8li;
    uchar          *status;
    uchar          *c;
{
     register struct c8_silo *siloptr;


    siloptr = c8li->li_rptr;

    *status = siloptr->si_status;
    *c      = siloptr->si_c;

    if (++c8li->li_rptr == c8li->li_silo_limit)
	c8li->li_rptr = c8li->li_silo_start;
}


c8_rint(c8l, c, status)
    register struct ss_line    *c8l;
    register int c;
    register int status;
{
    sysinfo.rcvint++;

    if (!(c8l->ss_state & SS_ISOPEN) || !(c8l->ss_cflag & CREAD))
        return;


    /* process start stop */
	/* 
	 * Errors?
	 */
	if (status & (SOERR|SPERR|SFERR|SBRKI)) {
	    if (SOERR & status) {
		c8l->ss_overflow++;	/* count overrun */
		    if (c8_print & C8DBG_PT8)
		        cmn_err(CE_CONT,"c8[%d]: line %d overflow\n",
			((struct c8_linfo *) c8l->ss_llocal)->ctlr, c8l->ss_line);
	    }
	    if (SBRKI & status) {	/* if there was a BREAK	*/
		if (c8l->ss_iflag & IGNBRK)
			return;		/* ignore it if ok */
		if (c8l->ss_iflag & BRKINT) {
			ss_mk_sig(c8l,SIGINT);
			return;
		}
		if (c8l->ss_iflag & PARMRK) {
                       ss_slowr(c8l,0377);
                       ss_slowr(c8l,0);
		}
		c = '\0';

	    } else if (IGNPAR & c8l->ss_iflag) {
		return;

	    } else if (!(INPCK & c8l->ss_iflag)) {
		/* ignore input parity errors if asked */

	    } else if ((SPERR|SFERR) & status) {
		if (SFERR & status) {
		    c8l->ss_framerr++;
		    if (c8_print & C8DBG_PT8)
		        cmn_err(CE_CONT,"c8[%d]: line %d framing error\n",
			((struct c8_linfo *) c8l->ss_llocal)->ctlr, c8l->ss_line);
		}
		else if (c8_print & C8DBG_PT8)
		        cmn_err(CE_CONT,"c8[%d]: line %d parity error\n",
			((struct c8_linfo *) c8l->ss_llocal)->ctlr, c8l->ss_line);

		if (c8l->ss_iflag & PARMRK) {
		    ss_slowr(c8l,0377);
		    ss_slowr(c8l,0);
		} else {
		    c = '\0';
		}
	    }

	} else if ((c8l->ss_iflag & ISTRIP) == 0 &&
		   (c & 0xFF) == 0377 &&
		   c8l->ss_iflag & PARMRK) {
		ss_slowr(c8l,0377);
	}			

        /* put char on the input queue */
        ss_inc (c8l, c);
}


c8_tint(c8l, status)
    register struct ss_line    *c8l;
    register int status;
{
    register mblk_t *wbp;
    register char cs;

    sysinfo.xmtint++;

    if ((status & STRDY) && (c8l->ss_state & SS_BUSY))
    {
        c8l->ss_state &= ~SS_BUSY;

        /*
         * If the user process or line discipline
         * sent ^S/^Q we aren't using the write buffer
         */
	if (c8l->ss_state & SS_XBUSY) {
	    c8l->ss_state &= ~SS_XBUSY;
	} else if (wbp = c8l->ss_wbp) {
            /* account for character output */
            wbp->b_rptr+= c8l->ss_lcc;
        }
	c8l->ss_lcc = 0;
    };
    if ((status & STRDY) &&
        !(c8l->ss_state & SS_TXSTOP)) 
            ss_tx (c8l);
}

c8_cint(c8l, status)
    register struct ss_line *c8l;
    register int status;
{
    sysinfo.mdmint++;

    if (status & MSCTS) {
        if (!(c8l->ss_modem_state & BSD43_TIOCM_CTS)) 	/* Call ss to mark CTS as on */
		ss_cts_on(c8l);
    } else {
        if (c8l->ss_modem_state & BSD43_TIOCM_CTS) 	/* Call ss to mark CTS as off */
		ss_cts_off(c8l);
    }


    if (status & MSDCD) {
        if (!(c8l->ss_modem_state & BSD43_TIOCM_CD)) 	/* Call ss to mark Carrier as on */
            ss_con (c8l);
    } else {
        if (c8l->ss_modem_state & BSD43_TIOCM_CD) 	/* Call ss to mark Carrier as off */
            ss_coff (c8l);
    }
}

/* This routine switches the table that we use for baud rate to polling
ticks conversion.  This lets us easily speed up or slow down polling.
The 3030 has a millisecond timer that we can adjust after we
have gone above the fastest table or below the slowest table.
*/

swtch_tbl(dir)
int dir;
{
	if (dir == UPSHIFT){
		if (myindex != C8_NUM_CONV-1){
			myindex++;
			myconv = &(conv_tbl[myindex].baud_conv[0]);
			if (c8_print & C8DBG_PT7)
				cmn_err(CE_CONT, "Upshifted\n");
			return (1);
		}
		if (IS_R3030){
			if (c8rambo_time > min_rambo_time){
			    c8rambo_time -= RAMBO_DELTA;
			    c8rambo_ticks = rambodelay(c8rambo_time);
			    return(0);
			}
		}
	}else{
		if (myindex != 0){
			myindex--;
			myconv = &(conv_tbl[myindex].baud_conv[0]);
			if (c8_print & C8DBG_PT7)
				cmn_err(CE_CONT, "Downshifted\n");
			return(1);
		}
		if (IS_R3030){
			if (c8rambo_time < MAX_RAMBO_TIME){
			    c8rambo_time += RAMBO_DELTA;
			    c8rambo_ticks = rambodelay(c8rambo_time);
			}
		}
	}
	return(0);
}

/* Someone is changing the speed of a port.  So we must re-evaluate 
how often the polling routine should be called.
*/

c8_reset_poll(c8l,baud)
register struct ss_line    *c8l;
int baud;
{
	int bd_num;
	register struct ss_struct  *c8b;
	int max_speed;
	register int s, i;

	/* Update this board's polling rate by finding		*/
	/* the maximum poll rates of all ports on this board	*/

	bd_num = ((struct c8_linfo *) c8l->ss_llocal)->ctlr;
	c8b = c8board[bd_num];

	max_speed = 0;
	for(c8l=c8b->ss_lines; c8l < (c8b->ss_lines + c8b->ss_nlines);c8l++) {
	    if ((c8l->ss_cflag & CBAUD)
	    && (max_speed < c8l->ss_cflag & CBAUD)){
		    max_speed = c8l->ss_cflag & CBAUD;
	    }
	}
	/* set board to max rate */
	c8_brd_poll[bd_num] = max_speed;

	/* Now find max poll_rate for all boards 	*/
	/* to send to clock routine				*/
	max_speed = 0;
	for (bd_num=0;bd_num<c8_bmax;bd_num++){
	    if (max_speed < c8_brd_poll[bd_num])
		max_speed = c8_brd_poll[bd_num];
	}
	c8_speedmax = max_speed;

	/* Update port polling rate */
	if (max_speed == 0){
		c8_poll_rate = 0;
		r3030_polling = 0;	
	}
        else{
	/* Start at fastest poll rate for any opens */
		if (myindex != C8_NUM_CONV-1){
			s = spltty();
			myindex=C8_NUM_CONV-1;
			myconv = &(conv_tbl[myindex].baud_conv[0]);
			splx(s);
		}
		
	       if (c8_poll_rate != myconv[max_speed].rate){
		   s=spltty();
	           c8_poll_rate = myconv[max_speed].rate;
	           poll_unregister_me(c8_poll);
	           if (poll_register_me(c8_poll,c8_poll_rate))
cmn_err(CE_CONT,"c8_reset_poll: Could not poll at %d rate\n",c8_poll_rate);
                   splx(s);
	       }

	}
	/* Start up 3030 polling if necessary */
	if (IS_R3030 && c8_poll_rate && !r3030_polling){  
		r3030_polling++;	    		
		c8_check_poll();
	}
}

/* Suck characters out of the hardware fifos, and stash them in 
   software silos for now.  These chars will get processed later.
   Only look at boards and ports that are scheduled to be looked at.
*/
static void
c8_poll()
{
    register struct ss_struct *c8b;
    register struct ss_line *c8l;
    register struct c8_linfo *c8li;
    register int c8addr, k;
    register uchar status, c;
    int flag,s,counter;
    static int c8_polling_cnt = 0;
    static int i = 0;
#ifdef DEBUGGING
    int count;
    static int poll_debug = 0;
#endif
    int spltty();


    s = spltty();
    flag = 0x80;
    c8_polling_cnt += c8_poll_rate;
		
    for (counter=0; counter < c8_bmax;counter++) {
	i = ++i % c8_bmax;
	c8b = c8board[i];

	if ((!c8b)||!(myconv[c8_brd_poll[i]].ticks & c8_polling_cnt))
		/* No port on this board needs service */
	   continue;

	for (c8l=c8b->ss_lines; c8l < (c8b->ss_lines + c8b->ss_nlines); c8l++) {

	    /* Poll port if this is its tick, its open, and its enabled */
	    if ((myconv[c8l->ss_cflag & CBAUD].ticks & c8_polling_cnt) 
		  && (c8l->ss_state & SS_ISOPEN) && (c8l->ss_lenable)){
	        c8li = (struct c8_linfo *) c8l->ss_llocal;
		c8addr = c8li->li_addr;
		for (k = 0; k < c8_poll_loopcnt; k++) {
		    if ((status = inb(c8addr+RSTATUS)) & SRRDY) {
			c = inb(c8addr+RRDATA);
		        if (ss_startstop (c8l, c)) {
				c8_putsilo(c8li,status,c);
				flag = 0;
			}
		    } else
			break;
		}

		if (k > c8_fifomax)
		    c8_fifomax= k;
#ifdef DEBUGGING
		if (k > c8li->li_fifomaxfull)
		    c8li->li_fifomaxfull = k;
	        if ((count = c8li->li_wptr - c8li->li_rptr) < 0)
		    count += SILOMAX;
		if (count > c8li->li_silomaxfull)
		    c8li->li_silomaxfull = count;
#endif
	    }
#ifdef DEBUGGING
	    else
		   poll_debug++;
#endif
	}
    }
    i = ++i % c8_bmax;		/* Start at the next board, next time around */
    if (c8_polling_cnt >= POLL_CONST)
	c8_polling_cnt = TICK0;

    if (!flag && !c8_timeid)
	c8_timeid = timeout_spl(c8_dispatch,0,HZ/30,spltty);
    
    splx(s);
}

/* Current time on RAMBO */
#define	C8_CLOCK_COUNT	*(volatile ulong *)0xbc000c00 /* RAMBO count reg */

#ifdef DEBUGGING
unsigned long c8max_delay;
unsigned long c8min_delay = 1000000L;
#endif

static void
c8_check_poll()
{
	register int i;
	unsigned long delay;
	static last_time;
	static int poll_tick = 1;	/* which tick is this? */


#ifdef DEBUGGING
	delay = C8_CLOCK_COUNT;

	if (c8rambo_time == 1000){
		if (delay - last_time > c8max_delay)
			c8max_delay = delay - last_time;
		else
			if (delay - last_time < c8min_delay)
				c8min_delay = delay - last_time;
	}
#endif
		
	for (i=0; (i<POLL_TBL_SIZE) && (poll_tbl[poll_tick][i].func != NULL); i++){
			poll_tbl[poll_tick][i].func();
	}

	if (poll_tick++ > MAX_INTERVAL)
		poll_tick = 1;

	i = splall();
	if (c8_poll_rate) {
		delay = C8_CLOCK_COUNT + c8rambo_ticks;
		interrupt_at(delay, c8_check_poll,0);
	} else			/* stop polling, no more open ports */
		r3030_polling = 0;
	
#ifdef DEBUGGING
	last_time = C8_CLOCK_COUNT;
#endif
	splx(i);
}

/* 
    If we are on the 3030 make sure a video board is not
    present.
*/
#define	KREG		(R3030_GRAPHICS_REG_ADDR + R3030_KERNEL_OFFSET)
#define	RAMBO_COUNT	(RAMBO_BASE + RAMBO_TCOUNT)
check_for_color() {
	register volatile unsigned long *time;
	register volatile unsigned long *kreg;
	register	  unsigned long stime;
	register	  unsigned long	skreg;

	time = (volatile unsigned long *)PHYS_TO_K1(RAMBO_COUNT);
	kreg = (volatile unsigned long *)PHYS_TO_K1(KREG);
	stime = *time;
	skreg = *kreg;
	while((*time - stime) < 62500)
		if ((skreg ^ *kreg) & 0x30) {
			return  1;
		}
	return  0;
}


/* provide number of ports for the 3030.  It only has one slot */
is_digi()
{
	if(c8board[0] && c8board[1])
		return(16);
	if (c8board[0] || c8board[1])
		return(8);
	return(0);
}

/*
	Monitor how full the fifos are, and adjust polling rates
	accordingly.  The r2400 has granularity down to 2.5
	ms.  However the 3030 has microsecond granularity.
	We can increase the polling speed at 1/2 second intervals
	but decreas only at 15 second intervals.
*/

void
c8_monitor()
{
	int splhi();
	void c8_monitor();
	static int cntr = 0;
	int threshold = 100;
	int new_rate = c8_poll_rate;
	int changed;

    if (c8_poll_rate){
	if (c8_fifomax >= C8_H_WATER){ 
	  cntr = 0;

          if (c8_print & C8DBG_PT7)
	    cmn_err(CE_CONT,"Trying to go up\n");

	  changed = swtch_tbl(UPSHIFT);
	}
	else
	if (c8_fifomax <= C8_L_WATER){
	 if (++cntr == threshold){
	  cntr = 0;
          if (c8_print & C8DBG_PT7)
	    cmn_err(CE_CONT,"Trying to go down\n");
	  changed = swtch_tbl(DOWNSHIFT);
	 }
	}

	if (changed && (c8_poll_rate != myconv[c8_speedmax].rate)){
                if (c8_print & C8DBG_PT7)
	  	     cmn_err(CE_CONT,"Changing rate \n");
		poll_unregister_me(c8_poll);
		c8_poll_rate = myconv[c8_speedmax].rate;
		if (poll_register_me(c8_poll,c8_poll_rate))
		    cmn_err(CE_CONT,"c8_monitor: Could not reset polling\n");
	}
    }

	c8_fifomax = 0;
	timeout_spl(c8_monitor,0,HZ/2,splhi);
}

