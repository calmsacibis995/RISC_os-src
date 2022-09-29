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
/* $Header: hdelog.h,v 1.6.4.2 90/05/10 06:21:22 wje Exp $ */

#ifndef	_SYS_HDELOG_
#define	_SYS_HDELOG_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* values for filling in readtype are: */
#define HDECRC		1	/* for CRC data checking */
#define HDEECC		2	/* for Error Correction Code data processing */

/* values for filling in severity are: */
#define HDEMARG		1	/* for marginal blocks */
#define HDEUNRD		2	/* for unreadable blocks */

struct hdedata {
	dev_t	diskdev;	/* the major/minor disk device number */
				/* (major number for character dev) */
	char	dskserno[12];	/* disk pack serial number */
				/* protection of removable media */
	short	pad;		/* to aid 3b2/80186 alignment */
	daddr_t	blkaddr;	/* physical block address
				 * in machine-independent form */
	char	readtype;	/* CRC or EC */
	char	severity;	/* marginal or unreadable */
	char	badrtcnt;	/* number of unreadable tries */
	char	bitwidth;	/* bitwidth of corrected error: 0 if CRC */
	time_t	timestmp;	/* time stamp helps pathological cases*/
};

/* structure for hdeeqdt[] table declared in space.h */
struct hdeedd {
	dev_t	hdeedev;
	short	hdetype;
	short	hdeflgs;
	short	hdepid;
	daddr_t	hdepdsno;
};

#define EQD_ID		0
#define EQD_IF		1
#define EQD_TAPE	2
#define EQD_EHDC	3
#define EQD_EFC		4

/* size of internal log report queue of the hdelog driver: */
#define HDEQSIZE	19

#endif	_SYS_HDELOG_
