#ident "$Header: types.h,v 1.3 90/01/23 13:37:32 huang Exp $"
/* $Copyright$ */

/*
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:32 $
 * $State: Exp $	$Author: huang $
 * $Log:	types.h,v $
 * Revision 1.3  90/01/23  13:37:32  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:11:26  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  16:00:00  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  11:11:20  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


typedef	struct { int r[1]; } *	physadr;
typedef	long		daddr_t;	/* <disk address> type */
typedef	char *		caddr_t;	/* ?<core address> type */
typedef	unsigned short	ushort;
typedef	ushort		ino_t;		/* <inode> type */
typedef short		cnt_t;		/* ?<count> type */
typedef	long		time_t;		/* <time> type */
typedef	int		label_t[15];
typedef	short		dev_t;		/* <device number> type */
typedef	long		off_t;		/* ?<offset> type */
typedef	unsigned long	paddr_t;	/* <physical address> type */
typedef	int		key_t;		/* IPC key type */
typedef unsigned int	uint;
typedef unsigned char	use_t;		/* use count for swap.  */
