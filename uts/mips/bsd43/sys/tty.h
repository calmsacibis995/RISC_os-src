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
/* $Header: tty.h,v 1.10.1.2 90/05/10 04:57:23 wje Exp $ */
/*
 * Copyright (c) 1982, 1986 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)tty.h	7.1 (Berkeley) 6/4/86
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#ifdef KERNEL
#include "ttychars.h"
#include "ttydev.h"
#else
#include <bsd43/sys/ttychars.h>
#include <bsd43/sys/ttydev.h>
#endif

/*
 * A clist structure is the head of a linked list queue
 * of characters.  The characters are stored in blocks
 * containing a link and CBSIZE (param.h) characters. 
 * The routines in tty_subr.c manipulate these structures.
 */
struct bsd43_(clist) {
	int	c_cc;		/* character count */
	char	*c_cf;		/* pointer to first char */
	char	*c_cl;		/* pointer to last char */
};

/*
 * Per-tty structure.
 *
 * Should be split in two, into device and tty drivers.
 * Glue could be masks of what to echo and circular buffer
 * (low, high, timeout).
 */
struct bsd43_(tty) {
	union {
		struct {
			struct	bsd43_(clist) T_rawq;
			struct	bsd43_(clist) T_canq;
		} t_t;
#define	bsd43_t_rawq	t_nu.t_t.T_rawq	/* raw characters or partial line */
#define	bsd43_t_canq	t_nu.t_t.T_canq	/* raw characters or partial line */
		struct {
			struct	bsd43_(buf) *T_bufp;
			char	*T_cp;
			int	T_inbuf;
			int	T_rec;
		} t_n;
#define	bsd43_t_bufp	t_nu.t_n.T_bufp	/* buffer allocated to protocol */
#define	bsd43_t_cp	t_nu.t_n.T_cp	/* pointer into the ripped off buffer */
#define	bsd43_t_inbuf	t_nu.t_n.T_inbuf/* number chars in the buffer */
#define	bsd43_t_rec	t_nu.t_n.T_rec	/* have a complete record */
	} t_nu;
	struct	bsd43_(clist) t_outq;	/* device */
	int	(*t_oproc)();		/* device */
	struct	bsd43_(proc) *t_rsel;	/* tty */
	struct	bsd43_(proc) *t_wsel;
				caddr_t	T_LINEP;	/* ### */
	caddr_t	t_addr;		/* ??? */
	dev_t	t_dev;		/* device */
	int	t_flags;		/* some of both */
	int	t_state;		/* some of both */
	short	t_pgrp;			/* tty */
	char	t_delct;		/* tty */
	char	t_line;			/* glue */
	char	t_col;			/* tty */
	char	t_ispeed, t_ospeed;	/* device */
	char	t_rocount, t_rocol;	/* tty */
	struct	bsd43_(ttychars) t_chars;	/* tty */
	struct	bsd43_(winsize) t_winsize;	/* window size */
/* be careful of tchars & co. */
#define	bsd43_t_erase	t_chars.tc_erase
#define	bsd43_t_kill	t_chars.tc_kill
#define	bsd43_t_intrc	t_chars.tc_intrc
#define	bsd43_t_quitc	t_chars.tc_quitc
#define	bsd43_t_startc	t_chars.tc_startc
#define	bsd43_t_stopc	t_chars.tc_stopc
#define	bsd43_t_eofc	t_chars.tc_eofc
#define	bsd43_t_brkc	t_chars.tc_brkc
#define	bsd43_t_suspc	t_chars.tc_suspc
#define	bsd43_t_dsuspc	t_chars.tc_dsuspc
#define	bsd43_t_rprntc	t_chars.tc_rprntc
#define	bsd43_t_flushc	t_chars.tc_flushc
#define	bsd43_t_werasc	t_chars.tc_werasc
#define	bsd43_t_lnextc	t_chars.tc_lnextc
};

#define	BSD43_TTIPRI	28
#define	BSD43_TTOPRI	29

/* limits */
#define	BSD43_NSPEEDS	16
#define	BSD43_TTMASK	15
#define	BSD43_OBUFSIZ	100
#define	BSD43_TTYHOG	255
#ifdef KERNEL
short	bsd43_(tthiwat)[BSD43_NSPEEDS], bsd43_(ttlowat)[BSD43_NSPEEDS];
#define	BSD43_TTHIWAT(tp)	tthiwat[(tp)->t_ospeed&BSD43_TTMASK]
#define	BSD43_TTLOWAT(tp)	ttlowat[(tp)->t_ospeed&BSD43_TTMASK]
extern	struct bsd43_(ttychars) bsd43_(ttydefaults);
#endif

/* internal state bits */
#define	BSD43_TS_TIMEOUT	0x000001	/* delay timeout in progress */
#define	BSD43_TS_WOPEN	0x000002	/* waiting for open to complete */
#define	BSD43_TS_ISOPEN	0x000004	/* device is open */
#define	BSD43_TS_FLUSH	0x000008	/* outq has been flushed during DMA */
#define	BSD43_TS_CARR_ON	0x000010	/* software copy of carrier-present */
#define	BSD43_TS_BUSY	0x000020	/* output in progress */
#define	BSD43_TS_ASLEEP	0x000040	/* wakeup when output done */
#define	BSD43_TS_XCLUDE	0x000080	/* exclusive-use flag against open */
#define	BSD43_TS_TTSTOP	0x000100	/* output stopped by ctl-s */
#define	BSD43_TS_HUPCLS	0x000200	/* hang up upon last close */
#define	BSD43_TS_TBLOCK	0x000400	/* tandem queue blocked */
#define	BSD43_TS_RCOLL	0x000800	/* collision in read select */
#define	BSD43_TS_WCOLL	0x001000	/* collision in write select */
#define	BSD43_TS_NBIO	0x002000	/* tty in non-blocking mode */
#define	BSD43_TS_ASYNC	0x004000	/* tty in async i/o mode */
/* state for intra-line fancy editing work */
#define	BSD43_TS_BKSL	0x010000	/* state for lowercase \ work */
#define	BSD43_TS_QUOT	0x020000	/* last character input was \ */
#define	BSD43_TS_ERASE	0x040000	/* within a \.../ for PRTRUB */
#define	BSD43_TS_LNCH	0x080000	/* next character is literal */
#define	BSD43_TS_TYPEN	0x100000	/* retyping suspended input (PENDIN) */
#define	BSD43_TS_CNTTB	0x200000	/* counting tab width; leave FLUSHO alone */

#define	BSD43_TS_LOCAL	(BSD43_TS_BKSL|BSD43_TS_QUOT|BSD43_TS_ERASE|BSD43_TS_LNCH|BSD43_TS_TYPEN|BSD43_TS_CNTTB)

/* define partab character types */
#define	BSD43_ORDINARY	0
#define	BSD43_CONTROL	1
#define	BSD43_BACKSPACE	2
#define	BSD43_NEWLINE	3
#define	BSD43_TAB		4
#define	BSD43_VTAB	5
#define	BSD43_RETURN	6

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BACKSPACE BSD43_BACKSPACE
#   define CONTROL BSD43_CONTROL
#   define NEWLINE BSD43_NEWLINE
#   define NSPEEDS BSD43_NSPEEDS
#   define OBUFSIZ BSD43_OBUFSIZ
#   define ORDINARY BSD43_ORDINARY
#   define RETURN BSD43_RETURN
#   define TAB BSD43_TAB
#   define TS_ASLEEP BSD43_TS_ASLEEP
#   define TS_ASYNC BSD43_TS_ASYNC
#   define TS_BKSL BSD43_TS_BKSL
#   define TS_BUSY BSD43_TS_BUSY
#   define TS_CARR_ON BSD43_TS_CARR_ON
#   define TS_CNTTB BSD43_TS_CNTTB
#   define TS_ERASE BSD43_TS_ERASE
#   define TS_FLUSH BSD43_TS_FLUSH
#   define TS_HUPCLS BSD43_TS_HUPCLS
#   define TS_ISOPEN BSD43_TS_ISOPEN
#   define TS_LNCH BSD43_TS_LNCH
#   define TS_LOCAL BSD43_TS_LOCAL
#   define TS_NBIO BSD43_TS_NBIO
#   define TS_QUOT BSD43_TS_QUOT
#   define TS_RCOLL BSD43_TS_RCOLL
#   define TS_TBLOCK BSD43_TS_TBLOCK
#   define TS_TIMEOUT BSD43_TS_TIMEOUT
#   define TS_TTSTOP BSD43_TS_TTSTOP
#   define TS_TYPEN BSD43_TS_TYPEN
#   define TS_WCOLL BSD43_TS_WCOLL
#   define TS_WOPEN BSD43_TS_WOPEN
#   define TS_XCLUDE BSD43_TS_XCLUDE
#   define TTHIWAT BSD43_TTHIWAT
#   define TTIPRI BSD43_TTIPRI
#   define TTLOWAT BSD43_TTLOWAT
#   define TTMASK BSD43_TTMASK
#   define TTOPRI BSD43_TTOPRI
#   define TTYHOG BSD43_TTYHOG
#   define VTAB BSD43_VTAB
#   define t_brkc bsd43_t_brkc
#   define t_bufp bsd43_t_bufp
#   define t_canq bsd43_t_canq
#   define t_cp bsd43_t_cp
#   define t_dsuspc bsd43_t_dsuspc
#   define t_eofc bsd43_t_eofc
#   define t_erase bsd43_t_erase
#   define t_flushc bsd43_t_flushc
#   define t_inbuf bsd43_t_inbuf
#   define t_intrc bsd43_t_intrc
#   define t_kill bsd43_t_kill
#   define t_lnextc bsd43_t_lnextc
#   define t_quitc bsd43_t_quitc
#   define t_rawq bsd43_t_rawq
#   define t_rec bsd43_t_rec
#   define t_rprntc bsd43_t_rprntc
#   define t_startc bsd43_t_startc
#   define t_stopc bsd43_t_stopc
#   define t_suspc bsd43_t_suspc
#   define t_werasc bsd43_t_werasc
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


