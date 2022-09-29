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
/* $Header: termio.h,v 1.18.1.2 90/05/10 06:41:00 wje Exp $ */

#ifndef	_SYS_TERMIO_
#define	_SYS_TERMIO_

/* terminal I/O definitions
 *  Merged System V and 4.3 BSD
 */


#define oNCC	8
#define	NCC	23

/* control characters */
#define	VINTR	0
#define	VQUIT	1
#define	VERASE	2
#define	VKILL	3
#define	VEOF	4
#define	VEOL	5
#define	VEOL2	6
#define	VMIN	4
#define	VTIME	5
#ifdef mips
#define VSWTCH	7
#endif
#define V_START	8		/* start output */
#define VSTART V_START		/* (POSIX) start output */
#define V_STOP	9		/* stop output */
#define VSTOP V_STOP		/* (POSIX) stop output */
#define V_SUSP	10		/* suspend process signal */
#define VSUSP V_SUSP		/* (POSIX) suspend process signal */
#define	V_DSUSP	11		/* delayed suspend process signal */
#define V_RPRNT	12		/* reprint line */
#define V_FLUSH	13		/* flush output (toggles) */
#define V_WERAS	14		/* word erase */
#define V_LNEXT	15		/* literal next character */
#define V_STATUS 16		/* print process status */
#define V_SAVED_EOF 17		/* (bsd43) saved VEOF in RAW mode */
#define V_SAVED_MIN V_SAVED_EOF
#define V_SAVED_EOL 18		/* (bsd43) saved VEOL in RAW mode */
#define V_SAVED_TIME V_SAVED_EOL


#define	CNUL	0
#define	CDEL	0377

/* default control chars */
#define	CTRL(c)	('c'&037)
#define	CESC	'\\'
#define	CINTR	0177	/* DEL */
#define	CQUIT	034	/* FS, cntl | */
#ifdef mips
#define CERASE	'#'
#else
#define	CERASE	CTRL(H)
#endif
#define	CKILL	'@'
#define	CEOF	CTRL(d)
#define	CSTART	CTRL(q)
#define	CSTOP	CTRL(s)
#ifdef mips
#define	CSWTCH	CTRL(z)
#define CNSWTCH	0
#endif

#define	CSUSP	CTRL(z)			/* someday, */
#define	CDSUSP	CTRL(y)			/* we may have 4.2 signals */

#define	CRPRNT	CTRL(r)			/* we now have these control chars */
#define	CFLUSH	CTRL(o)
#define	CWERASE	CTRL(w)
#define	CLNEXT	CTRL(v)


/* input modes */
#define	IGNBRK	0000001
#define	BRKINT	0000002
#define	IGNPAR	0000004
#define	PARMRK	0000010
#define	INPCK	0000020
#define	ISTRIP	0000040
#define	INLCR	0000100
#define	IGNCR	0000200
#define	ICRNL	0000400
#define	IUCLC	0001000
#define	IXON	0002000
#define	IXANY	0004000
#define	IXOFF	0010000
#define	IBLKMD	0020000

/* output modes */
#define	OPOST	0000001
#define	OLCUC	0000002
#define	ONLCR	0000004
#define	OCRNL	0000010
#define	ONOCR	0000020
#define	ONLRET	0000040
#define	OFILL	0000100
#define	OFDEL	0000200
#define	NLDLY	0000400
#define	NL0	0
#define	NL1	0000400
#define	CRDLY	0003000
#define	CR0	0
#define	CR1	0001000
#define	CR2	0002000
#define	CR3	0003000
#define	TABDLY	0014000
#define	TAB0	0
#define	TAB1	0004000
#define	TAB2	0010000
#define	TAB3	0014000
#define	BSDLY	0020000
#define	BS0	0
#define	BS1	0020000
#define	VTDLY	0040000
#define	VT0	0
#define	VT1	0040000
#define	FFDLY	0100000
#define	FF0	0
#define	FF1	0100000

/* control modes */
#define	CBAUD	0000017
#define	B0	0
#define	B50	0000001		/* not supported */
#define	B75	0000002
#define	B110	0000003
#define	B134	0000004
#define	B150	0000005
#define	B200	0000006		/* not supported */
#define	B300	0000007
#define	B600	0000010
#define	B1200	0000011
#define	B1800	0000012		/* not supported */
#define	B2400	0000013
#define	B4800	0000014
#define	B9600	0000015
#define	B19200	0000016
#define	EXTA	0000016
#define	B38400	0000017
#define	EXTB	0000017
#define	CSIZE	0000060
#define	CS5	0
#define	CS6	0000020
#define	CS7	0000040
#define	CS8	0000060
#define	CSTOPB	0000100
#define	CREAD	0000200
#define	PARENB	0000400
#define	PARODD	0001000
#define	HUPCL	0002000
#define	CLOCAL	0004000
#define CNEW_RTSCTS	0020000
#ifdef mips
/* #define RCV1EN	0010000 */
/* #define XMT1EN	0020000 */
#define LOBLK	0040000
#endif
#define CNEW_MDMBUF	0100000


/* line discipline 0 modes in lflag */
#define	ISIG	0000001
#define	ICANON	0000002
#define	XCASE	0000004
#define	ECHO	0000010
#define	ECHOE	0000020
#define	ECHOK	0000040
#define	ECHONL	0000100
#define	NOFLSH	0000200

/* internal flag to indicate when the POSIX IEXTEN flag has been cleared 
 * indicating that special character functions not defined in the 
 * standard should not be processed.  This bit should NEVER be modified
 * be a SYSV or BSD program
 */
#define	POSIX_NOIEXTEN	0000400 	

#define LNEW_CTLECH	0x0400
#define LNEW_PRTERA	0x0800
#define LNEW_FLUSHO	0x1000
#define LNEW_CRTBS  	0x2000
#define LNEW_PENDIN 	0x4000
#define TOSTOP 		0x8000

/* (bsd43) saved flags for 4.3 BSD emulation (c_saved_flags) */

/*	-- flags saved from other flag words			*/
#define LSAVE_OPOST 	0x0001
#define LSAVE_ISIG  	0x0002
#define LSAVE_ICANON 	0x0004
#define LSAVE_IUCLC	0x0008
#define LSAVE_OLCUC	0x0010
#define LSAVE_ICRNL	0x0020
#define LSAVE_ONLCR	0x0040
#define LSAVE_XCASE	0x0080
#define LSAVE_IGNCR	0x0100
#define LSAVE_INLCR	0x0200
#define LSAVE_BRKINT	0x0400

/*	-- flags representing nominal 4.3 BSD modes		*/
#define LSAVE_RAW	0x1000
#define LSAVE_CBREAK	0x2000
#define	LSAVE_CRMOD	0x4000
#define LSAVE_LCASE	0x8000

#define	SSPEED	B9600


#ifndef _IO
/*
 * 4.3BSD Ioctl's have the command encoded in the lower word,
 * and the size of any in or out parameters in the upper
 * word.  The high 2 bits of the upper word are used
 * to encode the in/out status of the parameter; for now
 * we restrict parameters to at most 1024 bytes.
 */
#define	IOCPARM_MASK	0x3ff		/* parameters must be < 1024 bytes */
#define	IOC_VOID	0x20000000	/* no parameters */
#define	IOC_OUT		0x40000000	/* copy out parameters */
#define	IOC_IN		0x80000000	/* copy in parameters */
#define	IOC_INOUT	(IOC_IN|IOC_OUT)
/* the 0x20000000 is so we can distinguish new ioctl's from old */
#define	_IO(x,y)	(IOC_VOID|('x'<<8)|y)
#define	_IOR(x,y,t)	(IOC_OUT|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
#define	_IOW(x,y,t)	(IOC_IN|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
/* this should be _IORW, but stdio got there first */
#define	_IOWR(x,y,t)	(IOC_INOUT|((sizeof(t)&IOCPARM_MASK)<<16)|('x'<<8)|y)
#endif


/*
 * Ioctl control packet
 */
struct Otermio {
	unsigned short	c_iflag;	/* input modes */
	unsigned short	c_oflag;	/* output modes */
	unsigned short	c_cflag;	/* control modes */
	unsigned short	c_lflag;	/* line discipline modes */
	char	c_line;			/* line discipline */
	unsigned char	c_cc[oNCC];	/* control chars */
};
struct termio {
	unsigned short	c_iflag;	/* input modes */
	unsigned short	c_oflag;	/* output modes */
	unsigned short	c_cflag;	/* control modes */
	unsigned short	c_lflag;	/* line discipline modes */
	char	c_line;			/* line discipline */
	unsigned char	c_cc[NCC];	/* control chars */
	unsigned short	c_saved_flags;  /* (bsd43) saved flags */
			/*
			 * BSD43 lmode modes implemented using iflag, oflag, and
			 * cflag, and lflag values:
			 *
			 *	BSD43_LCRTBS	LNEW_CRTBS (c_lflag)
			 *				[ LDISC_NEW only ]
			 *	BSD43_LPRTERA	LNEW_PRTERA (c_lflag)
			 *				[ LDISC_NEW only ]
			 *	BSD43_LCRTERA	ECHOE	(c_lflag)
			 *	BSD43_LMDMBUF	CNEW_MDMBUF (c_cflag)
			 *	BSD43_LNOHANG	~HUPCL	(c_cflag)
			 *	BSD43_LCRTKIL	ECHOK (c_lflag)
			 *	BSD43_LFLUSHO	LNEW_FLUSHO (c_lflag)
			 *				[ LDISC_NEW only ]
			 *	BSD43_LTOSTOP	TOSTOP (c_lflag)
			 *	BSD43_LCTLECH	LNEW_CTLECH (c_lflag)
			 *				[ LDISC_NEW only ]
			 *	BSD43_LPENDIN	LNEW_PENDIN (c_lflag)
			 *				[ LDISC_NEW only ]
			 *	BSD43_LLITOUT	~OPOST	(c_oflag)
			 *	BSD43_LPASS8	~ISTRIP	(c_iflag)
			 *	BSD43_LDECCTQ	~IXANY	(c_iflag)
			 *	BSD43_LNOFLSH	NOFLSH	(c_lflag)
			 *
			 * BSD43 lmode modes not supported:
			 *	BSD43_LTILDE	
			 *	BSD43_LL001000	
			 */

			/*
			 * BSD43 sgtty flags implemented using iflag, oflag, and
			 * cflag values:
			 *
			 *	BSD43_CBREAK	ISIG	(c_lflag)
			 *			ICANON  (c_lflag)
			 *			LSAVE_ISIG (c_saved_flags)
			 *			LSAVE_ICANON (c_saved_flags)
			 *			V_SAVED_EOL (c_cc)
			 *			V_SAVED_EOF (c_cc)
			 *	BSD43_RAW	OPOST	(c_oflag)
			 *			ISIG	(c_lflag)
			 *			ICANON  (c_lflag)
			 *			LSAVE_OPOST (c_saved_flags)
			 *			LSAVE_ISIG (c_saved_flags)
			 *			LSAVE_ICANON (c_saved_flags)
			 *			V_SAVED_EOL (c_cc)
			 *			V_SAVED_EOF (c_cc)
			 *	BSD43_LCASE	IUCLC	(c_iflag)
			 *			OLCUC	(c_oflag)
			 *			XCASE	(c_saved_flags)
			 *			LSAVE_IUCLC (c_saved_flags)
			 *			LSAVE_OLCUC (c_saved_flags)
			 *			LSAVE_XCASE (c_saved_flags)
			 *	BSD43_CRMOD	ICRNL	(c_iflag)
			 *			ONLCR	(c_oflag)
			 *			LSAVE_ICRNL (c_saved_flags)
			 *			LSAVE_ONLCR (c_saved_flags)
			 *	BSD43_ANYP	PARENB	(c_cflag)
			 *			INPCK	(c_cflag)
			 *			PARODD  (c_cflag)
			 *	BSD43_ODDP	PARENB	(c_cflag)
			 *			INPCK	(c_cflag)
			 *			PARODD  (c_cflag)
			 *	BSD43_EVENP	PARENB  (c_cflag)
			 *			INPCK	(c_cflag)
			 *			PARODD  (c_cflag)
			 *	BSD43_TANDEM	IXOFF	(c_cflag)
			 *	BSD43_ECHO	ECHO	(c_lflag)
			 *	BSD43_TBDELAY	TABDLY	(c_oflag)
			 *		BSD43_TAB0	TAB0
			 *		BSD43_TAB1	TAB1
			 *		BSD43_TAB2	TAB2
			 *	BSD43_XTABS		TAB3
			 *	BSD43_CRDELAY	CRDLY	(c_oflag)
			 *		BSD43_CR0	CR0
			 *		BSD43_CR1	CR1
			 *		BSD43_CR2	CR2
			 *		BSD43_CR3	CR3
			 *	BSD43_BSDELAY	BSDLY	(c_oflag)
			 *		BSD43_BS0	BS0
			 *		BSD43_BS1	BS1
			 *	ispeed/ospeed	CBAUD (c_cflag)
			 *
			 *  BSD43 sgtty flags not yet supported:
			 *
			 *	BSD43_NLDELAY	
			 *		BSD43_NL0	
			 *		BSD43_NL1	
			 *		BSD43_NL2	
			 *		BSD43_NL3	
			 *	BSD43_VTDELAY	
			 *		BSD43_FF0	
			 *		BSD43_FF1	
			 *
			 * BSD43 sgtty flags not supported:
			 *
			 *	ispeed		(no split speed)
			 *	ospeed		(no split speed)
			 */

	unsigned short	c_filler;
};
#define	C_BSD43_LINE	0x80		/* set if BSD discipline */
#define	C_BSD43_OTTYDISC (C_BSD43_LINE | 0) /* old, v7 std tty driver */
#define	C_BSD43_NTTYDISC (C_BSD43_LINE | 2) /* new tty discipline */
#define	C_BSD43_TABLDISC (C_BSD43_LINE | 3) /* tablet discipline */
#define	C_BSD43_SLIPDISC (C_BSD43_LINE | 4) /* serial IP discipline */

#define	IOCTYPE	0xff00

#define	TIOC	('T'<<8)
#define	oTCGETA	(TIOC|1)
#define TCGETA	_IOR(T,1,struct termio) /* get terminal attributes */
#define	oTCSETA	(TIOC|2)
#define TCSETA	_IOW(T,2,struct termio) /* set terminal attributes */
#define	oTCSETAW (TIOC|3)
#define TCSETAW	_IOW(T,3,struct termio) /* set terminal attributes */
#define	oTCSETAF (TIOC|4)
#define TCSETAF	_IOW(T,4,struct termio) /* set terminal attributes */
#define	TCSBRK	(TIOC|5)
#define	TCXONC	(TIOC|6)
#define	TCFLSH	(TIOC|7)
#define	TCDSET	(TIOC|32)
#define	TCBLKMD	(TIOC|33)

#define	TIOCEXCL	_IO(t, 13)	/* set exclusive use of tty */
#define	TIOCNXCL	_IO(t, 14)	/* reset exclusive use of tty */

#define	TIOCGPGRP	_IOR(t, 119, int)	/* get pgrp of tty */
#define	TIOCSPGRP	_IOW(t, 118, int)	/* set pgrp of tty */

#define	TIOCOUTQ	_IOR(t, 115, int)	/* output queue size */
#define oTIOCSTI (TIOC|114)		/* simulate terminal input */
#define	TIOCSTI	_IOW(t, 114, char)	/* simulate terminal input */
#define oTIOCNOTTY (TIOC|113)		/* disconnect from tty & pgrp */
#define	TIOCNOTTY	_IO(t, 113)		/* void tty association */

#define	oTIOCPKT (TIOC|112)		/* pty: set/clear packet mode */
#define	TIOCPKT	_IOW(t, 112, int)	/* pty: set/clear packet mode */
#define		TIOCPKT_DATA		0x00	/* data packet */
#define		TIOCPKT_FLUSHREAD	0x01	/* flush packet */
#define		TIOCPKT_FLUSHWRITE	0x02	/* flush packet */
#define 	TIOCPKT_STOP		0x04	/* stop output */
#define 	TIOCPKT_START		0x08	/* start output */
#define		TIOCPKT_NOSTOP		0x10	/* no more ^S, ^Q */
#define		TIOCPKT_DOSTOP		0x20	/* now do ^S ^Q */
#define	TIOCREMOTE _IOW(t, 105, int)	/* remote input editing */

/*
 * Window size structure
 */
struct winsize {
	unsigned short	ws_row, ws_col;		/* character size of window */
	unsigned short	ws_xpixel, ws_ypixel;	/* pixel size of window */
};

#define TIOCGWINSZ _IOR(t, 104, struct winsize)	/* get window size */
#define TIOCSWINSZ _IOW(t, 103, struct winsize)	/* set window size */
#define	TIOCUCNTL  _IOW(t, 102, int)	/* pty: set/clr usr cntl mode */

#define	TFIOC	('F'<<8)

#define oFIOCLEX	(('f'<<8)|1)
#define oFIONCLEX	(('f'<<8)|2)
#define	oTIOCHPCL	(('t'<<8)|2)
#define	oTIOCGETP	(('t'<<8)|8)
#define	oTIOCSETP	(('t'<<8)|9)
#define	oTIOCEXCL	(('t'<<8)|13)
#define	oTIOCNXCL	(('t'<<8)|14)

#ifdef KERNEL
#define TIOC_GISTTY	_IOR(t, 77, int) /* test if device is a terminal */
					/* (internal to kernel only)	*/
#endif KERNEL

#ifndef FIOCLEX
#define	FIOCLEX	_IO(f, 1)		/* set close-on-exec for fd */
#define	FIONCLEX _IO(f, 2)		/* reset close-on-exec for fd  */
#endif FIOCLEX

#define oFIONREAD (TFIOC|127)		/* pre-3.5 value of FIONREAD */
/* these are also defined in soioctl.h--XXX should be cleaned up */
#ifndef FIONREAD
#define	FIONREAD _IOR(f, 127, int)	/* get # bytes to read */
#define	FIONBIO	 _IOW(f, 126, int)	/* set/clear non-blocking i/o */
#define	FIOASYNC _IOW(f, 125, int)	/* set/clear async i/o */
#endif
#define	FIOSETOWN _IOW(f, 124, int)	/* set owner */
#define	FIOGETOWN _IOR(f, 123, int)	/* get owner */


#define	LDIOC	('D'<<8)
#define	LDOPEN	(LDIOC|0)
#define	LDCLOSE	(LDIOC|1)
#define	LDCHG	(LDIOC|2)
#define	LDGETT	(LDIOC|8)
#define	LDSETT	(LDIOC|9)

/*
 * Terminal types
 */
#define	TERM_NONE	0	/* tty */
#define	TERM_TEC	1	/* TEC Scope */
#define	TERM_V61	2	/* DEC VT61 */
#define	TERM_V10	3	/* DEC VT100 */
#define	TERM_TEX	4	/* Tektronix 4023 */
#define	TERM_D40	5	/* TTY Mod 40/1 */
#define	TERM_H45	6	/* Hewlitt-Packard 45 */
#define	TERM_D42	7	/* TTY Mod 40/2B */

/*
 * Terminal flags
 */
#define TM_NONE		0000	/* use default flags */
#define TM_SNL		0001	/* special newline flag */
#define TM_ANL		0002	/* auto newline on column 80 */
#define TM_LCF		0004	/* last col of last row special */
#define TM_CECHO	0010	/* echo terminal cursor control */
#define TM_CINVIS	0020	/* do not send esc seq to user */
#define TM_SET		0200	/* must be on to set/res flags */

/*
 * 'Line Disciplines
 */
#define LDISC0	0			/* ancient, standard */
#define LDISC_OLD LDISC0
#define LDISC1	1			/* new, 4.2BSD-like in streams */
#define LDISC_NEW 2			/* new, 4.3 BSD-compatible */
#define LDISC_SLIP 4			/* SLIP protocol */


/*
 * structure of ioctl arg for LDGETT and LDSETT
 *	This structure is no longer used.
 */
struct	termcb	{
	char	st_flgs;	/* term flags */
	char	st_termt;	/* term type */
	char	st_crow;	/* gtty only - current row */
	char	st_ccol;	/* gtty only - current col */
	char	st_vrow;	/* variable row */
	char	st_lrow;	/* last row */
};

#endif	_SYS_TERMIO_
