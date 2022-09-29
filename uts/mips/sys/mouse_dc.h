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
/* $Header: mouse_dc.h,v 1.1.2.2 90/05/10 06:29:23 wje Exp $ */
/*
 *  Mouse Data Converter Module for Jupiter,M12,Ustation/S
 *					by tsuji
 */

struct  MouseConv {
	int 	data_cnt;		/* data count (byte)	 */
 	char save_data;			/* data conversion area	 */
	queue_t *rq;			/* read queue of module  */
	queue_t *wq;			/* write queue of module */
	mblk_t 	*wbp;			/* message block	 */
	mblk_t	*read_buffer;		/* message block 	 */
	mblk_t	*read_head;		/* message block head	 */
	mblk_t	*read_tail;		/* message block tail	 */
};
