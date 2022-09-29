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
/* $Header: hash.h,v 1.6.2.2 90/05/09 18:57:45 wje Exp $ */


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */

#define		HASHZAP		0x03FF
#define		CDMARK		0x8000

#define		NOTFOUND		0x0000
#define		BUILTIN			0x0100
#define		FUNCTION		0x0200
#define		COMMAND			0x0400
#define		REL_COMMAND		0x0800
#define		PATH_COMMAND	0x1000
#define		DOT_COMMAND		0x8800		/* CDMARK | REL_COMMAND */

#define		hashtype(x)	(x & 0x1F00)
#define		hashdata(x)	(x & 0x00FF)


typedef struct entry
{
	char	*key;
	short	data;
	char	hits;
	char 	cost;
	struct entry	*next;
} ENTRY;

extern ENTRY	*hfind();
extern ENTRY	*henter();
extern int		hcreate();
