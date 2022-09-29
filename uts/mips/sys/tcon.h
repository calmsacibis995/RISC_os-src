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
/* $Header: tcon.h,v 1.7.4.2 90/05/10 06:40:36 wje Exp $ */

#ifndef	_SYS_TCON_
#define	_SYS_TCON_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * TCON Kernel Driver for the Remote Login process
 *
 */


#ifndef TRUE
#define TRUE	1
#define FALSE	0
#endif TRUE

#define DEL	0177
#define ESC	0200

/*
 * Packets used for local-remote communication have the following
 * structure
 */

#define DATA		0	/* data				*/
#define CONNECT		1	/* connect to an available tty	*/
#define DISCONNECT	2	/* disconnect on a used tty	*/
#define IOCTL		3	/* ioctl termio settings	*/
#define STATUS		4	/* tell somebody something	*/
#define SIGNAL		5	/* various types of signals	*/
#define DACK		6	/* data ACK			*/
#define IOSYNC		7	/* local-remote ioctl sync	*/
#define DEBUG		10



#define PKHDRSZ		4
#define PKDATASZ	508
#define PKSIZE		512

typedef struct {
	char ptype,		/* packet type: data or control	*/
	     tconid[2],		/* numeric id of local machine  */
	     ttyid;		/* index of the tty data struct	*/
	char data[PKDATASZ];	/* data or control information	*/
	} PACKET;
/*eject*/
/*
 * TCON debug flags
 */

#define DOPEN	0x00000001
#define DCLOSE	0x00000002
#define DREAD	0x00000004
#define DWRITE	0x00000008
#define DIOCTL	0x00000010
#define DINTR	0x00000020
#define DPROC	0x00000040
#define DPARM	0x00000080
#define DOOS	0x00000100
#define DMCNTL	0x00000200
#define DTTXIN	0x00000400
#define DTTXOUT	0x00000800
#define DCANON	0x00001000
#define DTIMEO	0x00002000

#endif	_SYS_TCON_
