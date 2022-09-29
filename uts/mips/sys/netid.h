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
/* $Header: netid.h,v 1.6.4.2 90/05/10 06:30:01 wje Exp $ */

#ifndef	_SYS_NETID_
#define	_SYS_NETID_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#define NIDCPY(a,b)	(a[0]=b[0],\
			 a[1]=b[1],\
			 a[2]=b[2],\
			 a[3]=b[3],\
			 a[4]=b[4],\
			 a[5]=b[5])

#define NIDCLR(a)	(a[0]=0,\
			 a[1]=0,\
			 a[2]=0,\
			 a[3]=0,\
			 a[4]=0,\
			 a[5]=0)

#define NIDCMP(a,b)	(a[0]==b[0]&&\
			 a[1]==b[1]&&\
			 a[2]==b[2]&&\
			 a[3]==b[3]&&\
			 a[4]==b[4]&&\
			 a[5]==b[5])


#endif	_SYS_NETID_
