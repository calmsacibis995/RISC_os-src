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
/* $Header: deck.h,v 1.1.2.2 90/05/10 03:04:42 wje Exp $ */

/*
 * Copyright (c) 1980 Regents of the University of California.
 * All rights reserved.  The Berkeley software License Agreement
 * specifies the terms and conditions for redistribution.
 *
 *	@(#)deck.h	5.1 (Berkeley) 5/30/85
 */

/*
 * define structure of a deck of cards and other related things
 */


#define		CARDS		52		/* number cards in deck */
#define		RANKS		13		/* number ranks in deck */
#define		SUITS		4		/* number suits in deck */

#define		CINHAND		4		/* # cards in cribbage hand */
#define		FULLHAND	6		/* # cards in dealt hand */

#define		LGAME		121		/* number points in a game */
#define		SGAME		61		/* # points in a short game */

#define		SPADES		0		/* value of each suit */
#define		HEARTS		1
#define		DIAMONDS	2
#define		CLUBS		3

#define		ACE		0		/* value of each rank */
#define		TWO		1
#define		THREE		2
#define		FOUR		3
#define		FIVE		4
#define		SIX		5
#define		SEVEN		6
#define		EIGHT		7
#define		NINE		8
#define		TEN		9
#define		JACK		10
#define		QUEEN		11
#define		KING		12
#define		EMPTY		13

#define		VAL(c)		( (c) < 9 ? (c)+1 : 10 )    /* val of rank */


#ifndef TRUE
#	define		TRUE		1
#	define		FALSE		0
#endif

typedef		struct  {
			int		rank;
			int		suit;
		}		CARD;

typedef		char		BOOLEAN;

