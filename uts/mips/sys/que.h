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
/* $Header: que.h,v 1.6.4.2 90/05/10 06:32:52 wje Exp $ */

#ifndef	_SYS_QUE_
#define	_SYS_QUE_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*********************************************************************

	following structures declares the data structures used for
	queue manipulation.

*********************************************************************/

/*****

	Queues consist simply of one-way list.  Each queue is
	defined by a 'queue control element' made up of head
	and tail pointers.  Each item held on a queue contains
	a pointer word used to link successive item together.
	The head pointer contains the address of the first
	item in the queue and the tail pointer contains the
	address of the last item.

*****/



/*
 *	Queue Item
 */

#define	QITEM	struct qitem_st

struct	qitem_st
	{
	QITEM	*qi_next;	/* next queue item pointer */
	char	data[1];	/* data */
	};



/*
 *	Queue Control Element
 */

#define	QCTRL	struct qctrl_st

struct	qctrl_st
	{
	QITEM	*qc_head;		/* head pointer */
	QITEM	*qc_tail;		/* tail pointer */
	};






#endif	_SYS_QUE_
