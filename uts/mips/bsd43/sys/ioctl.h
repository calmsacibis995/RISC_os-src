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
/* $Header: ioctl.h,v 1.10.1.2 90/05/10 04:52:00 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)ioctl.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/*
 * Ioctl definitions
 */
#ifndef	BSD43__IOCTL_
#define	BSD43__IOCTL_
#ifdef KERNEL
#include "ttychars.h"
#include "ttydev.h"
#else
#include <bsd43/sys/ttychars.h>
#include <bsd43/sys/ttydev.h>
#endif

struct bsd43_(tchars) {
	char	t_intrc;	/* interrupt */
	char	t_quitc;	/* quit */
	char	t_startc;	/* start output */
	char	t_stopc;	/* stop output */
	char	t_eofc;		/* end-of-file */
	char	t_brkc;		/* input delimiter (like nl) */
};
struct bsd43_(ltchars) {
	char	t_suspc;	/* stop process signal */
	char	t_dsuspc;	/* delayed stop process signal */
	char	t_rprntc;	/* reprint line */
	char	t_flushc;	/* flush output (toggles) */
	char	t_werasc;	/* word erase */
	char	t_lnextc;	/* literal next character */
};

/*
 * Structure for TIOCGETP and TIOCSETP ioctls.
 */

#ifndef BSD43__SGTTYB_
#define	BSD43__SGTTYB_
struct bsd43_(sgttyb) {
	char	sg_ispeed;		/* input speed */
	char	sg_ospeed;		/* output speed */
	char	sg_erase;		/* erase character */
	char	sg_kill;		/* kill character */
	short	sg_flags;		/* mode flags */
};
#endif

/*
 * Window/terminal size structure.
 * This information is stored by the kernel
 * in order to provide a consistent interface,
 * but is not used by the kernel.
 *
 * Type must be "unsigned short" so that types.h not required.
 */
struct bsd43_(winsize) {
	unsigned short	ws_row;			/* rows, in characters */
	unsigned short	ws_col;			/* columns, in characters */
	unsigned short	ws_xpixel;		/* horizontal size, pixels */
	unsigned short	ws_ypixel;		/* vertical size, pixels */
};

/*
 * Pun for SUN.
 */
struct bsd43_(ttysize) {
	unsigned short	ts_lines;
	unsigned short	ts_cols;
	unsigned short	ts_xxx;
	unsigned short	ts_yyy;
};
#define	BSD43_TIOCGSIZE	BSD43_TIOCGWINSZ
#define	BSD43_TIOCSSIZE	BSD43_TIOCSWINSZ

#ifndef BSD43__IO
/*
 * Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 128 bytes.
 */
#define	BSD43_IOCPARM_MASK	0x7f	/* parameters must be < 128 bytes */
#define	BSD43_IOC_VOID	0x20000000	/* no parameters */
#define	BSD43_IOC_OUT	0x40000000	/* copy out parameters */
#define	BSD43_IOC_IN	0x80000000	/* copy in parameters */
#define	BSD43_IOC_INOUT	(BSD43_IOC_IN|BSD43_IOC_OUT)

/* the 0x20000000 is so we can distinguish new ioctl's from old */
#define	BSD43__IO(x,y)	(BSD43_IOC_VOID|('x'<<8)|y)
#define	BSD43__IOR(x,y,t)	(BSD43_IOC_OUT|((sizeof(t)&BSD43_IOCPARM_MASK)<<16)|('x'<<8)|y)
#define	BSD43__IOW(x,y,t)	(BSD43_IOC_IN|((sizeof(t)&BSD43_IOCPARM_MASK)<<16)|('x'<<8)|y)

/* this should be _IORW, but stdio got there first */
#define	BSD43__IOWR(x,y,t)	(BSD43_IOC_INOUT|((sizeof(t)&BSD43_IOCPARM_MASK)<<16)|('x'<<8)|y)
#endif

/*
 * tty ioctl commands
 */
#define	BSD43_TIOCGETD	BSD43__IOR(t, 0, int)	/* get line discipline */
#define	BSD43_TIOCSETD	BSD43__IOW(t, 1, int)	/* set line discipline */
#define	BSD43_TIOCHPCL	BSD43__IO(t, 2)		/* hang up on last close */
#define	BSD43_TIOCMODG	BSD43__IOR(t, 3, int)	/* get modem control state */
#define	BSD43_TIOCMODS	BSD43__IOW(t, 4, int)	/* set modem control state */
#define		BSD43_TIOCM_LE	0001		/* line enable */
#define		BSD43_TIOCM_DTR	0002		/* data terminal ready */
#define		BSD43_TIOCM_RTS	0004		/* request to send */
#define		BSD43_TIOCM_ST	0010		/* secondary transmit */
#define		BSD43_TIOCM_SR	0020		/* secondary receive */
#define		BSD43_TIOCM_CTS	0040		/* clear to send */
#define		BSD43_TIOCM_CAR	0100		/* carrier detect */
#define		BSD43_TIOCM_CD	BSD43_TIOCM_CAR
#define		BSD43_TIOCM_RNG	0200		/* ring */
#define		BSD43_TIOCM_RI	BSD43_TIOCM_RNG
#define		BSD43_TIOCM_DSR	0400		/* data set ready */
#define	BSD43_TIOCGETP	BSD43__IOR(t, 8,struct bsd43_(sgttyb))/* get params -- gtty */
#define	BSD43_TIOCSETP	BSD43__IOW(t, 9,struct bsd43_(sgttyb))/* set params -- stty */
#define	BSD43_TIOCSETN	BSD43__IOW(t,10,struct bsd43_(sgttyb))/* as above, but no flushtty */
#define	BSD43_TIOCEXCL	BSD43__IO(t, 13)		/* set exclusive use of tty */
#define	BSD43_TIOCNXCL	BSD43__IO(t, 14)		/* reset exclusive use of tty */
#define	BSD43_TIOCFLUSH	BSD43__IOW(t, 16, int)	/* flush buffers */
#define	BSD43_TIOCSETC	BSD43__IOW(t,17,struct bsd43_(tchars))/* set special characters */
#define	BSD43_TIOCGETC	BSD43__IOR(t,18,struct bsd43_(tchars))/* get special characters */
#define		BSD43_TANDEM	0x00000001	/* send stopc on out q full */
#define		BSD43_CBREAK	0x00000002	/* half-cooked mode */
#define		BSD43_LCASE	0x00000004	/* simulate lower case */
#define		BSD43_ECHO	0x00000008	/* echo input */
#define		BSD43_CRMOD	0x00000010	/* map \r to \r\n on output */
#define		BSD43_RAW		0x00000020	/* no i/o processing */
#define		BSD43_ODDP	0x00000040	/* get/send odd parity */
#define		BSD43_EVENP	0x00000080	/* get/send even parity */
#define		BSD43_ANYP	0x000000c0	/* get any parity/send none */
#define		BSD43_NLDELAY	0x00000300	/* \n delay */
#define			BSD43_NL0	0x00000000
#define			BSD43_NL1	0x00000100	/* tty 37 */
#define			BSD43_NL2	0x00000200	/* vt05 */
#define			BSD43_NL3	0x00000300
#define		BSD43_TBDELAY	0x00000c00	/* horizontal tab delay */
#define			BSD43_TAB0	0x00000000
#define			BSD43_TAB1	0x00000400	/* tty 37 */
#define			BSD43_TAB2	0x00000800
#define		BSD43_XTABS		0x00000c00	/* expand tabs on output */
#define		BSD43_CRDELAY	0x00003000	/* \r delay */
#define			BSD43_CR0	0x00000000
#define			BSD43_CR1	0x00001000	/* tn 300 */
#define			BSD43_CR2	0x00002000	/* tty 37 */
#define			BSD43_CR3	0x00003000	/* concept 100 */
#define		BSD43_VTDELAY	0x00004000	/* vertical tab delay */
#define			BSD43_FF0	0x00000000
#define			BSD43_FF1	0x00004000	/* tty 37 */
#define		BSD43_BSDELAY	0x00008000	/* \b delay */
#define			BSD43_BS0	0x00000000
#define			BSD43_BS1	0x00008000
#define		BSD43_ALLDELAY	(BSD43_NLDELAY|BSD43_TBDELAY|BSD43_CRDELAY|BSD43_VTDELAY|BSD43_BSDELAY)
#define		BSD43_CRTBS	0x00010000	/* do backspacing for crt */
#define		BSD43_PRTERA	0x00020000	/* \ ... / erase */
#define		BSD43_CRTERA	0x00040000	/* " \b " to wipe out char */
#define		BSD43_TILDE	0x00080000	/* hazeltine tilde kludge */
#define		BSD43_MDMBUF	0x00100000	/* start/stop output on carrier intr */
#define		BSD43_LITOUT	0x00200000	/* literal output */
#define		BSD43_TOSTOP	0x00400000	/* SIGSTOP on background output */
#define		BSD43_FLUSHO	0x00800000	/* flush output to terminal */
#define		BSD43_NOHANG	0x01000000	/* no SIGHUP on carrier drop */
#define		BSD43_L001000	0x02000000
#define		BSD43_CRTKIL	0x04000000	/* kill line with " \b " */
#define		BSD43_PASS8	0x08000000
#define		BSD43_CTLECH	0x10000000	/* echo control chars as ^X */
#define		BSD43_PENDIN	0x20000000	/* tp->t_rawq needs reread */
#define		BSD43_DECCTQ	0x40000000	/* only ^Q starts after ^S */
#define		BSD43_NOFLSH	0x80000000	/* no output flush on signal */

/* locals, from 127 down */
#define	BSD43_TIOCLBIS	BSD43__IOW(t, 127, int)	/* bis local mode bits */
#define	BSD43_TIOCLBIC	BSD43__IOW(t, 126, int)	/* bic local mode bits */
#define	BSD43_TIOCLSET	BSD43__IOW(t, 125, int)	/* set entire local mode word */
#define	BSD43_TIOCLGET	BSD43__IOR(t, 124, int)	/* get local modes */
#define		BSD43_LCRTBS	(BSD43_CRTBS>>16)
#define		BSD43_LPRTERA	(BSD43_PRTERA>>16)
#define		BSD43_LCRTERA	(BSD43_CRTERA>>16)
#define		BSD43_LTILDE	(BSD43_TILDE>>16)
#define		BSD43_LMDMBUF	(BSD43_MDMBUF>>16)
#define		BSD43_LLITOUT	(BSD43_LITOUT>>16)
#define		BSD43_LTOSTOP	(BSD43_TOSTOP>>16)
#define		BSD43_LFLUSHO	(BSD43_FLUSHO>>16)
#define		BSD43_LNOHANG	(BSD43_NOHANG>>16)
#define		BSD43_LCRTKIL	(BSD43_CRTKIL>>16)
#define		BSD43_LPASS8	(BSD43_PASS8>>16)
#define		BSD43_LCTLECH	(BSD43_CTLECH>>16)
#define		BSD43_LPENDIN	(BSD43_PENDIN>>16)
#define		BSD43_LDECCTQ	(BSD43_DECCTQ>>16)
#define		BSD43_LNOFLSH	(BSD43_NOFLSH>>16)
#define	BSD43_TIOCSBRK	BSD43__IO(t, 123)		/* set break bit */
#define	BSD43_TIOCCBRK	BSD43__IO(t, 122)		/* clear break bit */
#define	BSD43_TIOCSDTR	BSD43__IO(t, 121)		/* set data terminal ready */
#define	BSD43_TIOCCDTR	BSD43__IO(t, 120)		/* clear data terminal ready */
#define	BSD43_TIOCGPGRP	BSD43__IOR(t, 119, int)	/* get pgrp of tty */
#define	BSD43_TIOCSPGRP	BSD43__IOW(t, 118, int)	/* set pgrp of tty */
#define	BSD43_TIOCSLTC	BSD43__IOW(t,117,struct bsd43_(ltchars))/* set local special chars */
#define	BSD43_TIOCGLTC	BSD43__IOR(t,116,struct bsd43_(ltchars))/* get local special chars */
#define	BSD43_TIOCOUTQ	BSD43__IOR(t, 115, int)	/* output queue size */
#define	BSD43_TIOCSTI	BSD43__IOW(t, 114, char)	/* simulate terminal input */
#define	BSD43_TIOCNOTTY	BSD43__IO(t, 113)		/* void tty association */
#define	BSD43_TIOCPKT	BSD43__IOW(t, 112, int)	/* pty: set/clear packet mode */
#define		BSD43_TIOCPKT_DATA	0x00	/* data packet */
#define		BSD43_TIOCPKT_FLUSHREAD	0x01	/* flush packet */
#define		BSD43_TIOCPKT_FLUSHWRITE	0x02	/* flush packet */
#define		BSD43_TIOCPKT_STOP	0x04	/* stop output */
#define		BSD43_TIOCPKT_START	0x08	/* start output */
#define		BSD43_TIOCPKT_NOSTOP	0x10	/* no more ^S, ^Q */
#define		BSD43_TIOCPKT_DOSTOP	0x20	/* now do ^S ^Q */
#define	BSD43_TIOCSTOP	BSD43__IO(t, 111)		/* stop output, like ^S */
#define	BSD43_TIOCSTART	BSD43__IO(t, 110)		/* start output, like ^Q */
#define	BSD43_TIOCMSET	BSD43__IOW(t, 109, int)	/* set all modem bits */
#define	BSD43_TIOCMBIS	BSD43__IOW(t, 108, int)	/* bis modem bits */
#define	BSD43_TIOCMBIC	BSD43__IOW(t, 107, int)	/* bic modem bits */
#define	BSD43_TIOCMGET	BSD43__IOR(t, 106, int)	/* get all modem bits */
#define	BSD43_TIOCREMOTE	BSD43__IOW(t, 105, int)	/* remote input editing */
#define	BSD43_TIOCGWINSZ	BSD43__IOR(t, 104, struct bsd43_(winsize))	/* get window size */
#define	BSD43_TIOCSWINSZ	BSD43__IOW(t, 103, struct bsd43_(winsize))	/* set window size */
#define	BSD43_TIOCUCNTL	BSD43__IOW(t, 102, int)	/* pty: set/clr usr cntl mode */
#define		BSD43_UIOCCMD(n)	BSD43__IO(u, n)		/* usr cntl op "n" */

#define	BSD43_OTTYDISC	0		/* old, v7 std tty driver */
#define	BSD43_NETLDISC	1		/* line discip for berk net */
#define	BSD43_NTTYDISC	2		/* new tty discipline */
#define	BSD43_TABLDISC	3		/* tablet discipline */
#define	BSD43_SLIPDISC	4		/* serial IP discipline */

#define	BSD43_FIOCLEX	BSD43__IO(f, 1)		/* set exclusive use on fd */
#define	BSD43_FIONCLEX	BSD43__IO(f, 2)		/* remove exclusive use */
/* another local */
#define	BSD43_FIONREAD	BSD43__IOR(f, 127, int)	/* get # bytes to read */
#define	BSD43_FIONBIO	BSD43__IOW(f, 126, int)	/* set/clear non-blocking i/o */
#define	BSD43_FIOASYNC	BSD43__IOW(f, 125, int)	/* set/clear async i/o */
#define	BSD43_FIOSETOWN	BSD43__IOW(f, 124, int)	/* set owner */
#define	BSD43_FIOGETOWN	BSD43__IOR(f, 123, int)	/* get owner */

/* socket i/o controls */
#define	BSD43_SIOCSHIWAT	BSD43__IOW(s,  0, int)		/* set high watermark */
#define	BSD43_SIOCGHIWAT	BSD43__IOR(s,  1, int)		/* get high watermark */
#define	BSD43_SIOCSLOWAT	BSD43__IOW(s,  2, int)		/* set low watermark */
#define	BSD43_SIOCGLOWAT	BSD43__IOR(s,  3, int)		/* get low watermark */
#define	BSD43_SIOCATMARK	BSD43__IOR(s,  7, int)		/* at oob mark? */
#define	BSD43_SIOCSPGRP	BSD43__IOW(s,  8, int)		/* set process group */
#define	BSD43_SIOCGPGRP	BSD43__IOR(s,  9, int)		/* get process group */

#define	BSD43_SIOCADDRT BSD43__IOW(r,10,struct rtentry) /* add route */
#define	BSD43_SIOCDELRT BSD43__IOW(r,11,struct rtentry) /* delete route */

#define	BSD43_SIOCSIFADDR	BSD43__IOW(i,12,struct ifreq) /* set ifnet addr */
#define	BSD43_SIOCGIFADDR	BSD43__IOWR(i,13, struct ifreq) /* get ifnet addr */
#define	BSD43_SIOCSIFDSTADDR BSD43__IOW(i, 14, struct ifreq) /* set p-p addr */
#define	BSD43_SIOCGIFDSTADDR BSD43__IOWR(i,15, struct ifreq) /* get p-p addr */
#define	BSD43_SIOCSIFFLAGS   BSD43__IOW(i, 16, struct ifreq) /* set ifnet flags */
#define	BSD43_SIOCGIFFLAGS   BSD43__IOWR(i,17, struct ifreq) /* get ifnet flags */
#define	BSD43_SIOCGIFBRDADDR BSD43__IOWR(i,18, struct ifreq) /* get broadcast addr */
#define	BSD43_SIOCSIFBRDADDR BSD43__IOW(i,19, struct ifreq)  /* set broadcast addr */
#define	BSD43_SIOCGIFCONF	   BSD43__IOWR(i,20, struct ifconf)	/* get ifnet list */
#define	BSD43_SIOCGIFNETMASK BSD43__IOWR(i,21, struct ifreq)	/* get net addr mask */
#define	BSD43_SIOCSIFNETMASK BSD43__IOW(i,22, struct ifreq)	/* set net addr mask */
#define	BSD43_SIOCGIFMETRIC  BSD43__IOWR(i,23, struct ifreq)	/* get IF metric */
#define	BSD43_SIOCSIFMETRIC  BSD43__IOW(i,24, struct ifreq)	/* set IF metric */
#define	BSD43_SIOCSIFMEM   BSD43__IOW(i, 25, struct ifreq) /* set interface mem */
#define	BSD43_SIOCGIFMEM	 BSD43__IOWR(i,26, struct ifreq) /* get interface mem */
#define BSD43_SIOCSIFMTU   BSD43__IOW(i, 27, struct ifreq) /* set if_mtu */
#define BSD43_SIOCGIFMTU   BSD43__IOWR(i,28, struct ifreq) /* get if_mtu */
#define	BSD43_SIOCGIFSTATS BSD43__IOWR(i, 29, struct ifreq)/* get enp stats */

#define	BSD43_SIOCSARP  BSD43__IOW(i, 30, struct arpreq) /* set arp entry */
#define	BSD43_SIOCGARP  BSD43__IOWR(i,31, struct arpreq) /* get arp entry */
#define	BSD43_SIOCDARP  BSD43__IOW(i,32, struct arpreq) /* delete arp entry */
#define BSD43_SIOCUPPER BSD43__IOW(i, 40, struct ifreq) /* attach upper layer */
#define BSD43_SIOCLOWER       BSD43__IOW(i, 41, struct ifreq)       /* attach lower layer */

/* protocol i/o controls */
#define BSD43_SIOCSNIT        BSD43__IOW(p,  0, struct bsd43_(nit_ioc))     /* set nit modes */
#define BSD43_SIOCGNIT        BSD43__IOWR(p, 1, struct bsd43_(nit_ioc))     /* get nit modes */

#endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define ALLDELAY BSD43_ALLDELAY
#   define ANYP BSD43_ANYP
#   define BS0 BSD43_BS0
#   define BS1 BSD43_BS1
#   define BSDELAY BSD43_BSDELAY
#   define CBREAK BSD43_CBREAK
#   define CR0 BSD43_CR0
#   define CR1 BSD43_CR1
#   define CR2 BSD43_CR2
#   define CR3 BSD43_CR3
#   define CRDELAY BSD43_CRDELAY
#   define CRMOD BSD43_CRMOD
#   define CRTBS BSD43_CRTBS
#   define CRTERA BSD43_CRTERA
#   define CRTKIL BSD43_CRTKIL
#   define CTLECH BSD43_CTLECH
#   define DECCTQ BSD43_DECCTQ
#   define ECHO BSD43_ECHO
#   define EVENP BSD43_EVENP
#   define FF0 BSD43_FF0
#   define FF1 BSD43_FF1
#   define FIOASYNC BSD43_FIOASYNC
#   define FIOCLEX BSD43_FIOCLEX
#   define FIOGETOWN BSD43_FIOGETOWN
#   define FIONBIO BSD43_FIONBIO
#   define FIONCLEX BSD43_FIONCLEX
#   define FIONREAD BSD43_FIONREAD
#   define FIOSETOWN BSD43_FIOSETOWN
#   define FLUSHO BSD43_FLUSHO
#   define IOCPARM_MASK BSD43_IOCPARM_MASK
#   define IOC_IN BSD43_IOC_IN
#   define IOC_INOUT BSD43_IOC_INOUT
#   define IOC_OUT BSD43_IOC_OUT
#   define IOC_VOID BSD43_IOC_VOID
#   define L001000 BSD43_L001000
#   define LCASE BSD43_LCASE
#   define LCRTBS BSD43_LCRTBS
#   define LCRTERA BSD43_LCRTERA
#   define LCRTKIL BSD43_LCRTKIL
#   define LCTLECH BSD43_LCTLECH
#   define LDECCTQ BSD43_LDECCTQ
#   define LFLUSHO BSD43_LFLUSHO
#   define LITOUT BSD43_LITOUT
#   define LLITOUT BSD43_LLITOUT
#   define LMDMBUF BSD43_LMDMBUF
#   define LNOFLSH BSD43_LNOFLSH
#   define LNOHANG BSD43_LNOHANG
#   define LPASS8 BSD43_LPASS8
#   define LPENDIN BSD43_LPENDIN
#   define LPRTERA BSD43_LPRTERA
#   define LTILDE BSD43_LTILDE
#   define LTOSTOP BSD43_LTOSTOP
#   define MDMBUF BSD43_MDMBUF
#   define NETLDISC BSD43_NETLDISC
#   define NL0 BSD43_NL0
#   define NL1 BSD43_NL1
#   define NL2 BSD43_NL2
#   define NL3 BSD43_NL3
#   define NLDELAY BSD43_NLDELAY
#   define NOFLSH BSD43_NOFLSH
#   define NOHANG BSD43_NOHANG
#   define NTTYDISC BSD43_NTTYDISC
#   define ODDP BSD43_ODDP
#   define OTTYDISC BSD43_OTTYDISC
#   define PASS8 BSD43_PASS8
#   define PENDIN BSD43_PENDIN
#   define PRTERA BSD43_PRTERA
#   define RAW BSD43_RAW
#   define SIOCADDRT BSD43_SIOCADDRT
#   define SIOCATMARK BSD43_SIOCATMARK
#   define SIOCDARP BSD43_SIOCDARP
#   define SIOCDELRT BSD43_SIOCDELRT
#   define SIOCGARP BSD43_SIOCGARP
#   define SIOCGHIWAT BSD43_SIOCGHIWAT
#   define SIOCGIFADDR BSD43_SIOCGIFADDR
#   define SIOCGIFBRDADDR BSD43_SIOCGIFBRDADDR
#   define SIOCGIFCONF BSD43_SIOCGIFCONF
#   define SIOCGIFDSTADDR BSD43_SIOCGIFDSTADDR
#   define SIOCGIFFLAGS BSD43_SIOCGIFFLAGS
#   define SIOCGIFMEM BSD43_SIOCGIFMEM
#   define SIOCGIFMETRIC BSD43_SIOCGIFMETRIC
#   define SIOCGIFMTU BSD43_SIOCGIFMTU
#   define SIOCGIFNETMASK BSD43_SIOCGIFNETMASK
#   define SIOCGIFSTATS BSD43_SIOCGIFSTATS
#   define SIOCGLOWAT BSD43_SIOCGLOWAT
#   define SIOCGNIT BSD43_SIOCGNIT
#   define SIOCGPGRP BSD43_SIOCGPGRP
#   define SIOCLOWER BSD43_SIOCLOWER
#   define SIOCSARP BSD43_SIOCSARP
#   define SIOCSHIWAT BSD43_SIOCSHIWAT
#   define SIOCSIFADDR BSD43_SIOCSIFADDR
#   define SIOCSIFBRDADDR BSD43_SIOCSIFBRDADDR
#   define SIOCSIFDSTADDR BSD43_SIOCSIFDSTADDR
#   define SIOCSIFFLAGS BSD43_SIOCSIFFLAGS
#   define SIOCSIFMEM BSD43_SIOCSIFMEM
#   define SIOCSIFMETRIC BSD43_SIOCSIFMETRIC
#   define SIOCSIFMTU BSD43_SIOCSIFMTU
#   define SIOCSIFNETMASK BSD43_SIOCSIFNETMASK
#   define SIOCSLOWAT BSD43_SIOCSLOWAT
#   define SIOCSNIT BSD43_SIOCSNIT
#   define SIOCSPGRP BSD43_SIOCSPGRP
#   define SIOCUPPER BSD43_SIOCUPPER
#   define SLIPDISC BSD43_SLIPDISC
#   define TAB0 BSD43_TAB0
#   define TAB1 BSD43_TAB1
#   define TAB2 BSD43_TAB2
#   define TABLDISC BSD43_TABLDISC
#   define TANDEM BSD43_TANDEM
#   define TBDELAY BSD43_TBDELAY
#   define TILDE BSD43_TILDE
#   define TIOCCBRK BSD43_TIOCCBRK
#   define TIOCCDTR BSD43_TIOCCDTR
#   define TIOCEXCL BSD43_TIOCEXCL
#   define TIOCFLUSH BSD43_TIOCFLUSH
#   define TIOCGETC BSD43_TIOCGETC
#   define TIOCGETD BSD43_TIOCGETD
#   define TIOCGETP BSD43_TIOCGETP
#   define TIOCGLTC BSD43_TIOCGLTC
#   define TIOCGPGRP BSD43_TIOCGPGRP
#   define TIOCGSIZE BSD43_TIOCGSIZE
#   define TIOCGWINSZ BSD43_TIOCGWINSZ
#   define TIOCHPCL BSD43_TIOCHPCL
#   define TIOCLBIC BSD43_TIOCLBIC
#   define TIOCLBIS BSD43_TIOCLBIS
#   define TIOCLGET BSD43_TIOCLGET
#   define TIOCLSET BSD43_TIOCLSET
#   define TIOCMBIC BSD43_TIOCMBIC
#   define TIOCMBIS BSD43_TIOCMBIS
#   define TIOCMGET BSD43_TIOCMGET
#   define TIOCMODG BSD43_TIOCMODG
#   define TIOCMODS BSD43_TIOCMODS
#   define TIOCMSET BSD43_TIOCMSET
#   define TIOCM_CAR BSD43_TIOCM_CAR
#   define TIOCM_CD BSD43_TIOCM_CD
#   define TIOCM_CTS BSD43_TIOCM_CTS
#   define TIOCM_DSR BSD43_TIOCM_DSR
#   define TIOCM_DTR BSD43_TIOCM_DTR
#   define TIOCM_LE BSD43_TIOCM_LE
#   define TIOCM_RI BSD43_TIOCM_RI
#   define TIOCM_RNG BSD43_TIOCM_RNG
#   define TIOCM_RTS BSD43_TIOCM_RTS
#   define TIOCM_SR BSD43_TIOCM_SR
#   define TIOCM_ST BSD43_TIOCM_ST
#   define TIOCNOTTY BSD43_TIOCNOTTY
#   define TIOCNXCL BSD43_TIOCNXCL
#   define TIOCOUTQ BSD43_TIOCOUTQ
#   define TIOCPKT BSD43_TIOCPKT
#   define TIOCPKT_DATA BSD43_TIOCPKT_DATA
#   define TIOCPKT_DOSTOP BSD43_TIOCPKT_DOSTOP
#   define TIOCPKT_FLUSHREAD BSD43_TIOCPKT_FLUSHREAD
#   define TIOCPKT_FLUSHWRITE BSD43_TIOCPKT_FLUSHWRITE
#   define TIOCPKT_NOSTOP BSD43_TIOCPKT_NOSTOP
#   define TIOCPKT_START BSD43_TIOCPKT_START
#   define TIOCPKT_STOP BSD43_TIOCPKT_STOP
#   define TIOCREMOTE BSD43_TIOCREMOTE
#   define TIOCSBRK BSD43_TIOCSBRK
#   define TIOCSDTR BSD43_TIOCSDTR
#   define TIOCSETC BSD43_TIOCSETC
#   define TIOCSETD BSD43_TIOCSETD
#   define TIOCSETN BSD43_TIOCSETN
#   define TIOCSETP BSD43_TIOCSETP
#   define TIOCSLTC BSD43_TIOCSLTC
#   define TIOCSPGRP BSD43_TIOCSPGRP
#   define TIOCSSIZE BSD43_TIOCSSIZE
#   define TIOCSTART BSD43_TIOCSTART
#   define TIOCSTI BSD43_TIOCSTI
#   define TIOCSTOP BSD43_TIOCSTOP
#   define TIOCSWINSZ BSD43_TIOCSWINSZ
#   define TIOCUCNTL BSD43_TIOCUCNTL
#   define TOSTOP BSD43_TOSTOP
#   define UIOCCMD BSD43_UIOCCMD
#   define VTDELAY BSD43_VTDELAY
#   define XTABS BSD43_XTABS
#   define _IO BSD43__IO
#   define _IOCTL_ BSD43__IOCTL_
#   define _IOR BSD43__IOR
#   define _IOW BSD43__IOW
#   define _IOWR BSD43__IOWR
#   define _SGTTYB_ BSD43__SGTTYB_
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


