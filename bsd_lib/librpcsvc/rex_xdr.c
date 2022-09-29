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
#ident	"$Header: rex_xdr.c,v 1.2.1.3 90/05/09 14:45:25 wje Exp $"
#ifndef lint
static char sccsid[] = 	"@(#)rex_xdr.c	1.2 88/05/08 4.0NFSSRC Copyr 1988 Sun Micro";
#endif

/* 
 * Copyright (c) 1988 by Sun Microsystems, Inc.
 * @(#) from SUN 1.4
 */

/*
 * rex_xdr - remote execution external data representations
 */

#include <stdio.h>
#include <rpc/rpc.h>
#include <sys/errno.h>
#ifdef SYSTYPE_BSD43
#include <sys/ioctl.h>
#endif
#ifdef SYSTYPE_SYSV
/* magic to allow us to xdr a bsd43 structure in sysv compilation*/
#define BSD43_(x) x
#define bsd43_(x) x
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
#include <bsd43/sys/ioctl.h>
#endif

#include "rex.h"

/*
 * xdr_rex_start - process the command start structure
 */
xdr_rex_start(xdrs, rst)
	XDR *xdrs;
	struct rex_start *rst;
{
	return
		xdr_argv(xdrs, &rst->rst_cmd) &&
		xdr_string(xdrs, &rst->rst_host, 1024) &&
		xdr_string(xdrs, &rst->rst_fsname, 1024) &&
		xdr_string(xdrs, &rst->rst_dirwithin, 1024) &&
		xdr_argv(xdrs, &rst->rst_env) &&
		xdr_u_short(xdrs, &rst->rst_port0) &&
		xdr_u_short(xdrs, &rst->rst_port1) &&
		xdr_u_short(xdrs, &rst->rst_port2) &&
		xdr_u_long(xdrs, &rst->rst_flags);
}

xdr_argv(xdrs, argvp)
	XDR *xdrs;
	char ***argvp;
{
	register char **argv = *argvp;
	register char **ap;
	int i, count;

	/*
	 * find the number of args to encode or free
	 */
	if ((xdrs->x_op) != XDR_DECODE)
		for (count = 0, ap = argv; *ap != 0; ap++)
			count++;
	/* XDR the count */
	if (!xdr_u_int(xdrs, (unsigned *) &count))
		return (FALSE);

	/*
	 * now deal with the strings
	 */
	if (xdrs->x_op == XDR_DECODE) {
		*argvp = argv = (char **)mem_alloc((unsigned)(count+1)*sizeof (char **));
		for (i = 0; i <= count; i++)	/* Note: <=, not < */
			argv[i] = 0;
	}

	for (i = 0, ap = argv; i < count; i++, ap++)
		if (!xdr_string(xdrs, ap, 10240))
			return (FALSE);

	if (xdrs->x_op == XDR_FREE && argv != NULL) {
		mem_free((char *) argv, (count+1)*sizeof (char **));
		*argvp = NULL;
	}
	return (TRUE);
}

/*
 * xdr_rex_result - process the result of a start or wait operation
 */
xdr_rex_result(xdrs, result)
	XDR *xdrs;
	struct rex_result *result;
{
	return
		xdr_int(xdrs, &result->rlt_stat) &&
		xdr_string(xdrs, &result->rlt_message, 1024);

}

/*
 * xdr_rex_ttymode - process the tty mode information
 */
xdr_rex_ttymode(xdrs, mode)
	XDR *xdrs;
	struct rex_ttymode *mode;
{
  int six = 6;
  int four = 4;
  char *speedp = NULL;
  char *morep = NULL;
  char *yetmorep = NULL;

  if (xdrs->x_op != XDR_FREE) {
  	speedp = &mode->basic.sg_ispeed;
	morep = (char *)&mode->more;
  	yetmorep = (char *)&mode->yetmore;
  }
  return
	xdr_bytes(xdrs, (char **) &speedp, &four, 4) &&
	xdr_short(xdrs, &mode->basic.sg_flags) &&
	xdr_bytes(xdrs, (char **) &morep, &six, 6) &&
	xdr_bytes(xdrs, (char **) &yetmorep, &six, 6) &&
	xdr_u_long(xdrs, &mode->andmore);
}


/*
 * xdr_rex_ttysize - process the tty size information
 */
xdr_rex_ttysize(xdrs, size)
	XDR *xdrs;
	struct rex_ttysize *size;
{
  return
	xdr_int(xdrs, &size->ts_lines) &&
	xdr_int(xdrs, &size->ts_cols);
}
