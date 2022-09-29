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
/* $Header: cmn_err.h,v 1.7.2.2 90/05/10 06:07:46 wje Exp $ */

#ifndef	_SYS_CMN_ERR_
#define	_SYS_CMN_ERR_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/* Common error handling severity levels */

#define CE_CONT  0	/* continuation				*/
#define CE_NOTE  1	/* notice				*/
#define CE_WARN	 2	/* warning				*/
#define CE_PANIC 3	/* panic				*/

/*	Codes for where output should go.
*/

#define	PRW_BUF		0x01	/* Output to putbuf.		*/
#define	PRW_CONS	0x02	/* Output to console.		*/
#define PRW_MSGBUF	0x04	/* Output to bsd43_pmsgbuf	*/

extern short	prt_where;

#endif	_SYS_CMN_ERR_
