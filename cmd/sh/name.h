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
/* $Header: name.h,v 1.5.2.2 90/05/09 18:59:16 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 *	UNIX shell
 */


#define	N_ENVCHG 0020
#define N_RDONLY 0010
#define N_EXPORT 0004
#define N_ENVNAM 0002
#define N_FUNCTN 0001

#define N_DEFAULT 0

struct namnod
{
	struct namnod	*namlft;
	struct namnod	*namrgt;
	char	*namid;
	char	*namval;
	char	*namenv;
	int	namflg;
};

