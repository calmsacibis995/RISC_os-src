/*
 * |-----------------------------------------------------------|
 * | Copyright (c) 1989       MIPS Computer Systems, Inc.      |
 * | All Rights Reserved                                       |
 * |-----------------------------------------------------------|
 * | Use, duplication, or disclosure by the Government is      |
 * | subject to restrictions as set forth in                   |
 * | subparagraph (c)(1)(ii) of the Rights in Technical        |
 * | Data and Computer Software Clause of DFARS 52.227-7013.   |
 * |         MIPS Computer Systems, Inc.                       |
 * |         928 Arques Avenue                                 |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
#ident	"$Header: teState.h,v 1.1 90/03/22 18:31:01 chungc Exp $"
/*
 * ANSI Terminal Emulator - parser and window state interface.
 */
#ifndef _TESTATE_H_ /* { */
#define _TESTATE_H_

/*
 * Macros & Constants
 */
#define TABBYTES	((100 + 7) / 8)
#define TEMAXSEQ	128
#define TEMAXPARMS	 64
#define DEFTABINTERV	8			/* every 8 spaces */

#define ASCII_DEL	0x7f	/* Delete (Rubout) */
#define ASCII_ESC	0x1b	/* Escape */
#define ASCII_CSI	0x5b	/* Control Sequence Introducer (after ESC) */

#ifndef STANDALONE /* { */
#define E_ACTtoEND	0
#define E_STARTtoACT	1
#define E_STARTtoEND	2

#define SGR_NORMAL	0
#define SGR_BOLD	1
#define SGR_FAINT	2
#define SGR_ITALIC	3
#define SGR_UNDERLINE	4
#define SGR_SLOWBLINK	5
#define SGR_FASTBLINK	6
#define SGR_REVERSE	7
#define SGR_INVISIBLE	8
#define SGR_MAX		8
#define SgrMask(a)	(1 << (a) - 1)
#endif /* } ! STANDALONE */

/*
 * ANSI parsing states.
 */
#define TES_RAW		0	/* initial state */
#ifdef STANDALONE /* { */
#  define TESTATES	1
#else /* } STANDALONE { */
#  define TES_ES	1	/* escape sequence */
#  define TES_CS	2	/* control sequence */
#  define TES_CSP	3	/* control sequence at parameters */
#  define TESTATES	4
#endif /* } ! STANDALONE */

/*
 * Types
 */
typedef	struct TeState {
	unsigned char	row, col;	/* active position [1-ROWS]x[1-COLS] */
	unsigned char	tabs[TABBYTES];	/* current tab settings */
#ifndef STANDALONE /* { */
	unsigned char	state;		/* current parser state */
	unsigned char	prev;		/* previous character (if printable) */
	unsigned char	sgr;		/* selected graphics renditions */
	unsigned char	*pp;		/* end of parameter character list */
	unsigned char	seq[TEMAXSEQ];	/* parm and intermediate characters */
#endif /* } ! STANDALONE */
} TeState;

/*
 * External Functions
 */
extern	TeState	teState;
extern	void	teDevReset(), teClear(), teCopy(),
		teCursor(), teDrawChar(), teBell();
extern	void	teReset(), tePutChar(),
		teDmp(), teIgn(), tePut(), teCtl(),
		teBEL(), teBS(),  teCR(),  teHT(),  teLF();
#ifndef STANDALONE /* { */
  extern int	*teParms();
  extern void	teEsc(), teCSI(), teESF(), teCSP(), teCSF(),
		teCBT(), teCUB(), teCUD(), teCUF(), teCUP(), teCUU(),
		teDCH(), teDL(),  teED(),  teEL(),  teHPA(), teHTS(),
		teICH(), teIL(),  teREP(), teSGR(), teTBC(), teVPA();
#endif /* } ! STANDALONE */

#endif /* } ! _TESTATE_H_ */
