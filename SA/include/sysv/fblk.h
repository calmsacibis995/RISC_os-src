#ident "$Header: fblk.h,v 1.3 90/01/23 13:37:14 huang Exp $"
/* $Copyright$ */

/*
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:14 $
 * $State: Exp $	$Author: huang $
 * $Log:	fblk.h,v $
 * Revision 1.3  90/01/23  13:37:14  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:10:59  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  15:59:52  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  11:01:19  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

	/*
	#ident "@(#)kern-3b2:sys/fblk.h	1.3"
	*/
struct	fblk
{
	int	df_nfree;
	daddr_t	df_free[NICFREE];
};
