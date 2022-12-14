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
/* $Header: cribcur.h,v 1.1.2.2 90/05/10 03:04:36 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)cribcur.h	5.1 (Berkeley) 5/30/85
 */

# define	PLAY_Y		15	/* size of player's hand window */
# define	PLAY_X		12
# define	TABLE_Y		21	/* size of table window */
# define	TABLE_X		14
# define	COMP_Y		15	/* size of computer's hand window */
# define	COMP_X		12
# define	Y_SCORE_SZ	9	/* Y size of score board */
# define	X_SCORE_SZ	41	/* X size of score board */
# define	SCORE_Y		0	/* starting position of scoring board */
# define	SCORE_X	 	(PLAY_X + TABLE_X + COMP_X)
# define	CRIB_Y		17	/* position of crib (cut card) */
# define	CRIB_X		(PLAY_X + TABLE_X)
# define	MSG_Y		(LINES - (Y_SCORE_SZ + 1))
# define	MSG_X		(COLS - SCORE_X - 1)
# define	Y_MSG_START	(Y_SCORE_SZ + 1)

# define	PEG	'*'	/* what a peg looks like on the board */

extern	WINDOW		*Compwin;		/* computer's hand window */
extern	WINDOW		*Msgwin;		/* message window */
extern	WINDOW		*Playwin;		/* player's hand window */
extern	WINDOW		*Tablewin;		/* table window */
