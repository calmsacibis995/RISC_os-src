#ident "$Header: dir.h,v 1.3 90/01/23 13:37:23 huang Exp $"
/* $Copyright$ */

/*
 * $Revision: 1.3 $	$Date: 90/01/23 13:37:23 $
 * $State: Exp $	$Author: huang $
 * $Log:	dir.h,v $
 * Revision 1.3  90/01/23  13:37:23  huang
 * Added $Copyright$
 * 
 * Revision 1.2  89/10/26  08:09:43  hal
 * remove $Source line
 * 
 * Revision 1.1  87/08/18  15:59:50  mdove
 * Initial revision
 * 
 * Revision 1.1  86/03/13  10:59:26  opsys
 * Initial revision
 * 
 */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef	DIRSIZ
#define	DIRSIZ	14
#endif
struct	direct
{
	ino_t	d_ino;
	char	d_name[DIRSIZ];
};
