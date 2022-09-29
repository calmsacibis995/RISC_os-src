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
/* $Header: strids.h,v 1.11.2.3 90/05/10 10:53:02 wje Exp $ */

#ifndef	_SYS_STRIDS_
#define	_SYS_STRIDS_	1


/* Stream module ID and M_CTL registration
 *
 */



/* 
 */
#define STRID_DUART	9001		/* stream duart driver */
#define STRID_STTY_LD	9002		/* stream line discipline */
#define STRID_WIN	9003		/* stream windows */
#define STRID_PTC	9004		/* stream control pty */
#define STRID_PTS	9005		/* stream slave pty */
#define	STRID_CD3608	9006		/* stream central data serial driver */
#define STRID_MUX	9007		/* stream mux driver from ISI */
#define STRID_CONS	9008		/* stream sable console */
#define STRID_COM8	9009		/* stream driver for DSC COM-8 */
#define STRID_IOP_UART	9010		/* stream iop uart (Jupiter WS) */
#define STRID_IOP_KBD	9011		/* stream iop keyboard (Jupiter WS) */
#define STRID_IOP_MOUSE	9012		/* stream iop mouse (Jupiter WS) */
#define STRID_MOUSE_DC	9013		/* stream iop mouse (Jupiter WS) */
#define	STRID_KEYBOARD	9014		/* stream keyboard (RS3230) */

/* IDs for M_CTL messages
 *	One of these ints should be the first 4 bytes of an M_CTL message
 */
#define STRCTL__			/* none defined yet */

#endif	_SYS_STRIDS_
