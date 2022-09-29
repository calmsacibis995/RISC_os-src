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
#ident	"$Header: extern.c,v 1.1.2.2 90/05/10 03:08:26 wje Exp $"

# include	"hangman.h"

bool	Guessed[26];

char	Word[BUFSIZ],
	Known[BUFSIZ],
	*Noose_pict[] = {
		"     ______",
		"     |    |",
		"     |",
		"     |",
		"     |",
		"     |",
		"   __|_____",
		"   |      |___",
		"   |_________|",
		NULL
	};

int	Errors,
	Wordnum = 0;

double	Average = 0.0;

ERR_POS	Err_pos[MAXERRS] = {
	{  2, 10, 'O' },
	{  3, 10, '|' },
	{  4, 10, '|' },
	{  5,  9, '/' },
	{  3,  9, '/' },
	{  3, 11, '\\' },
	{  5, 11, '\\' }
};

FILE	*Dict = NULL;

off_t	Dict_size;
