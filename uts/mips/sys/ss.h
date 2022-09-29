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
/* $Header: ss.h,v 1.12.1.3.1.1.1.2 90/11/12 18:31:10 beacker Exp $ */

#ifndef	_SYS_SS_
#define	_SYS_SS_	1

/* ssdd_dev_type */
#define SS_TTY   0
#define SS_OTHER 1

/* used by drivers for null hardware procs */
int ss_null();  

/* ss stream entry points. Used by drivers in qinit structures */
int ss_open(), ss_rsrv(), ss_close(), ss_wput();

struct ss_devdep_info {
	char   *ssdd_devname;	/* Name of the device, for interest */
	int     ssdd_dev_type;	/* SS_TTY, SS_OTHER... */
	int     ssdd_maxboards;	/* MAX number of this type of board */
	int     ssdd_maxlines;	/* # of ports on per board */
	int     ssdd_maxoutc;	/* max chars that outc can take at one time */
	int   (*ssdd_act)();	/* Hardware, activate line */
	int   (*ssdd_zap)();	/* Hardware, deactivate line */
	int   (*ssdd_outc)();	/* Hardware, output characters */
	int   (*ssdd_setline)();/* Hardware, set line params */
	int   (*ssdd_modem_control)(); /* modem control operation */
	int   (*ssdd_driver_control)();    /* driver control operation */
};

/*
 *	Modem control operation codes 
 */

#define SS_MC_STARTBREAK	1
#define SS_MC_STOPBREAK 	2
#define SS_MC_ENABLEFLOW	3	/* Really RTS */
#define SS_MC_DISABLEFLOW	4	/* Really RTS */
#define SS_MC_ENABLEDTR		5
#define SS_MC_DISABLEDTR	6


/*
 *	Driver control operation codes
 */

#define SS_DC_SDRVFLUSH		1 
#define SS_DC_TIOCSWINSZ	2
#define SS_DC_TIOCOUTQ		3
#define SS_DC_DRAINOUTPUT	4
#define SS_DC_STOP		5
#define SS_DC_DISABLELINE	6
#define SS_DC_ENABLELINE	7
#define SS_DC_SUSPEND_OUTPUT	8
#define SS_DC_RESUME_OUTPUT	9
#define SS_DC_M_IOCTL		10
#define SS_DC_FLOWCNTL		11

#define SS_FLUSHNOSLEEP		0	/* arg for DRVFLUSH, above meaning...*/
#define SS_FLUSHCANSLEEP	1	/* ...driver can or can't sleep */

/*
 * These typedef is here because of System V's brain damaged type.h
 * which has ushort, uint, ulong and --> unchar <---
 */
typedef unsigned char	uchar;

struct ss_line {
	queue_t	*ss_rq,
		*ss_wq;
	mblk_t	*ss_rmsg,	/* current message */
		*ss_rmsge,
		*ss_rbp,
		*ss_wbp;
	char    *ss_llocal;	/* loophole line dependent stuff */
	int	ss_rmsg_len,
		ss_rbsize;
	int	ss_line;	/* line number */
	int	ss_lenable;	/* 1=> line is enabled */
	int	ss_tid;		/* service timer id */
	int	ss_state;	/* current state of port */
	int	ss_lcc;		/* last char count for xfer */
	int	ss_allocb_fail,
		ss_rsrv_cnt;	/* non zero => delay timer on */
	int 	ss_ldebug;	/* SS line debug flags */
	int	ss_overflow;	/* count of overflow errs */
	int	ss_framerr;	/* count of framming errors */
	int	ss_stopcnt;	/* count of stop characters */
	int	ss_startcnt;	/* count of start characters */
	struct termio ss_termio;
#define ss_iflag ss_termio.c_iflag
#define ss_cflag ss_termio.c_cflag
#define ss_ttyline ss_termio.c_line
#define ss_litc ss_termio.c_cc[V_LNEXT]
#define ss_stopc ss_termio.c_cc[V_STOP]
#define ss_startc ss_termio.c_cc[V_START]
	struct	ss_struct *pss;	/* pointer to controller struct */
	struct	winsize ss_winsize;
	unsigned long	ss_modem_state;
	ulong   ss_sph;			/* gba map area descriptor   */
};

struct ss_struct {
	int		ss_nlines;	/* # of ports on board */
	caddr_t		ss_addr;	/* board address */
	int		*ss_bconv;	/* baud rate conversion table */
	struct ss_devdep_info *ss_devdep; /* per board hardware dep info */
	struct ss_line  *ss_lines;	/* pointer to array of line structs */
	uint	ss_breaks;		/* Breaks that have happened */
	ulong	ss_csh;			/* Log. cache sect. desc     */
};

/* bits in ss_state */
#define SS_ISOPEN	0x0001		/* device is open		*/
#define SS_WOPEN	0x0002		/* waiting for carrier		*/
#define SS_DCD		0x0004		/* we have carrier		*/
#define SS_TIMEOUT	0x0008		/* delaying			*/
#define SS_BREAK	0x0010		/* breaking			*/
#define SS_BREAK_QUIET	0x0020		/* finishing break		*/
#define SS_TXSTOP	0x0040		/* output stopped by received XOFF */
#define SS_LIT		0x0080		/* have seen literal character	*/
#define SS_BLOCK	0x0100		/* XOFF sent because input full */
#define SS_TX_TXON	0x0200		/* need to send XON		*/
#define SS_TX_TXOFF	0x0400		/* need to send XOFF		*/
#define SS_BUSY		0x0800		/* xmit in progress		*/
#define SS_XCLUDE	0x1000		/* exclusive use		*/
#define SS_SIGIO	0x2000		/* send SIGIO when I/O possible */
#define SS_FLOW		0x4000		/* do hardware flow control	*/
#define SS_XBUSY	0x8000		/* xmit of XON/XOFF in progress */
#define SS_WCLOSE      0x10000		/* waiting in close for 	*/
					/* transmit complete		*/
#define SS_CTS         0x20000		/* We have CTS   	 	*/
#define SS_OPENWAIT    0x40000		/* set openwait in stream head	*/
#define SS_CLOSING     0x80000          /* wait for close to complete */
#define SS_OPENING    0x100000		/* currently opening		*/

#define	IFLAGS	(CS8|HUPCL|CLOCAL|SSPEED)


/* bits in ss_ldebug */
#define SS_DBG_RSRVSLEEP 0x0001		/* set => waiting to be awoken */
#define SS_DBG_QENBSLEEP 0x0002		/* set => ss_inc did not qenable */

#define SSIDLE 0
#define SSALERT 1

/*	Is this the modem port? 				*/
/*	Note:  The sense of this flag is inverted if 		*/
/*	kopt _riscos_ttys_default_clocal is disabled (set to -1). */

#define SSMODEM(dev) (dev & 0x80)

#endif !_SYS_SS_
