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
/* $Header: vsio.h,v 1.7.1.2 90/05/10 05:00:50 wje Exp $ */
 /****************************************************************************
 *									    *
 *  Copyright (c) 1983, 1984 by						    *
 *  DIGITAL EQUIPMENT CORPORATION, Maynard, Massachusetts.		    *
 *  All rights reserved.						    *
 * 									    *
 *  This software is furnished on an as-is basis and may be used and copied *
 *  only with inclusion of the above copyright notice. This software or any *
 *  other copies thereof may be provided or otherwise made available to     *
 *  others only for non-commercial purposes.  No title to or ownership of   *
 *  the software is hereby transferred.					    *
 * 									    *
 *  The information in this software is  subject to change without notice   *
 *  and  should  not  be  construed as  a commitment by DIGITAL EQUIPMENT   *
 *  CORPORATION.							    *
 * 									    *
 *  DIGITAL assumes no responsibility for the use  or  reliability of its   *
 *  software on equipment which is not supplied by DIGITAL.		    *
 * 									    *
 *									    *
 ****************************************************************************/
/* 
 * vsio.h - VS100 I/O command definitions
 * 
 * Author:	Christopher A. Kent
 *		Digital Equipment Corporation
 *		Western Research Lab
 * Date:	Tue Jun 21 1983
 */
/*----- COPYRIGHT (END) ------------------------------------------------------*/

#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


/* 
 * Possible ioctl calls
 */

#define	BSD43_VSIOINIT	BSD43__IO(V, 0)	/* init the device */
#define	BSD43_VSIOSTART	BSD43__IOW(V, 1, int)	/* start microcode */
#define	BSD43_VSIOABORT	BSD43__IO(V, 2)	/* abort a command chain */
#define	BSD43_VSIOPWRUP	BSD43__IO(V, 3)	/* power-up reset */
#define	BSD43_VSIOGETVER	BSD43__IOR(V, 4, int)	/* get rom version */
#define	BSD43_VSIOSYNC	BSD43__IO(V, 6)	/* synch with device */
#define	BSD43_VSIOBBACTL	BSD43__IOW(V, 8, int)	/* control the BBA */
#define	BSD43_VSIOFIBCTL	BSD43__IOW(V, 9, int)	/* lamp on/off */
#define	BSD43_VSIOFIBRETRY	BSD43__IOW(V,10, int)	/* fiber retries */
#define	BSD43_VSIOGETSTATS	BSD43__IOR(V,11, bsd43_(vsStats))	/* get statistics */
#define	BSD43_VSIOGETIOA	BSD43__IOR(V,13, bsd43_(vsIoAddrAddr))/* get ioreg address */
#define	BSD43_VSIOUSERWAIT	BSD43__IO(V, 15)	/* wait for user I/O completion */
#define BSD43_VSIOWAITGO	BSD43__IOW(V, 16, caddr_t)	/* wait then go */


#define	BSD43_VSIO_OFF	0		/* option off */
#define	BSD43_VSIO_ON		1		/* option on */

#define	BSD43_VS_FIB_FINITE	1		/* finite retries */
#define	BSD43_VS_FIB_INFINITE	2		/* infinite retries */

/* 
 * Event queue entries
 */

typedef struct	bsd43_(_vs_event){
	u_short	vse_x;		/* x position */
	u_short	vse_y;		/* y position */
	u_short	vse_time;	/* 10 millisecond units (button only) */
	char	vse_type;	/* button or motion? */
	u_char	vse_key;	/* the key (button only) */
	char	vse_direction;	/* which direction (button only) */
	char	vse_device;	/* which device (button only) */
}bsd43_(vsEvent);

#define	BSD43_VSE_BUTTON	0		/* button moved */
#define	BSD43_VSE_MMOTION	1		/* mouse moved */
#define	BSD43_VSE_TMOTION	2		/* tablet moved */

#define	BSD43_VSE_KBTUP	0		/* up */
#define	BSD43_VSE_KBTDOWN	1		/* down */

#define	BSD43_VSE_MOUSE	1		/* mouse */
#define	BSD43_VSE_DKB		2		/* main keyboard */
#define	BSD43_VSE_TABLET	3		/* graphics tablet */
#define	BSD43_VSE_AUX		4		/* auxiliary */
#define	BSD43_VSE_CONSOLE	5		/* console */

typedef struct bsd43_(_vsStats){
	int	errors;			/* count errors */
	int	unsolIntr;		/* count unsolicited interrupts */
	int	overruns;		/* event queue overruns */
	int	flashes;		/* flashes on fiber link */
	int	ignites;		/* times turned on */
	int	douses;			/* times turned off */
	int	linkErrors;		/* link errors */
}bsd43_(vsStats);

typedef struct bsd43_(_vs_cursor){
	short x;
	short y;
}bsd43_(vsCursor);

typedef struct bsd43_(_vs_box) {
	short bottom;
	short right;
	short left;
	short top;
}bsd43_(vsBox);

typedef struct bsd43_(_vsIoAddr) {
	short	 *ioreg;
	short	 bsd43_(status);
	caddr_t  obuff;
	int	 obufflen;
	int	 bsd43_(reloc);
	bsd43_(vsEvent)  *ibuff;
	int	 iqsize;		/* may assume power of 2 */
	int	 ihead;			/* atomic write */
	int	 itail;			/* atomic read */
	bsd43_(vsCursor) mouse;			/* atomic read/write */
	bsd43_(vsBox)	 mbox;			/* atomic read/write */
} bsd43_(vsIoAddr);
typedef bsd43_(vsIoAddr) *bsd43_(vsIoAddrAddr);

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define VSE_AUX BSD43_VSE_AUX
#   define VSE_BUTTON BSD43_VSE_BUTTON
#   define VSE_CONSOLE BSD43_VSE_CONSOLE
#   define VSE_DKB BSD43_VSE_DKB
#   define VSE_KBTDOWN BSD43_VSE_KBTDOWN
#   define VSE_KBTUP BSD43_VSE_KBTUP
#   define VSE_MMOTION BSD43_VSE_MMOTION
#   define VSE_MOUSE BSD43_VSE_MOUSE
#   define VSE_TABLET BSD43_VSE_TABLET
#   define VSE_TMOTION BSD43_VSE_TMOTION
#   define VSIOABORT BSD43_VSIOABORT
#   define VSIOBBACTL BSD43_VSIOBBACTL
#   define VSIOFIBCTL BSD43_VSIOFIBCTL
#   define VSIOFIBRETRY BSD43_VSIOFIBRETRY
#   define VSIOGETIOA BSD43_VSIOGETIOA
#   define VSIOGETSTATS BSD43_VSIOGETSTATS
#   define VSIOGETVER BSD43_VSIOGETVER
#   define VSIOINIT BSD43_VSIOINIT
#   define VSIOPWRUP BSD43_VSIOPWRUP
#   define VSIOSTART BSD43_VSIOSTART
#   define VSIOSYNC BSD43_VSIOSYNC
#   define VSIOUSERWAIT BSD43_VSIOUSERWAIT
#   define VSIOWAITGO BSD43_VSIOWAITGO
#   define VSIO_OFF BSD43_VSIO_OFF
#   define VSIO_ON BSD43_VSIO_ON
#   define VS_FIB_FINITE BSD43_VS_FIB_FINITE
#   define VS_FIB_INFINITE BSD43_VS_FIB_INFINITE
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


