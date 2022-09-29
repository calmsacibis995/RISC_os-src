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
/* $Header: deck.h,v 1.1.2.2 90/05/10 03:16:19 wje Exp $ */

# define	bool	int

# define	CC_D	deck[0]
# define	CH_D	deck[1]

struct dk_st {			/* deck description structure		*/
	int	num_cards;		/* number of cards in deck	*/
	int	last_card;		/* number of last card picked	*/
	bool	gojf_used;		/* set if gojf card out of deck	*/
	long	*offsets;		/* offests for start of cards	*/
};

typedef struct dk_st	DECK;
