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
/* $Header: tod.h,v 1.2.3.2 90/05/10 06:42:02 wje Exp $ */
/*
 * $Header: tod.h,v 1.2.3.2 90/05/10 06:42:02 wje Exp $
 */
/*
 * time of day interface between host and iop.
 */
#ifdef LANGUAGE_C
struct tod_cmblk {
	long time;				/* time ala UNIX */
	short command;			/* the command to the iop read/write */
};
#endif

/*
 * time of day commands.
 */
#define TODREAD		0x1				/* read the time of day */
#define TODWRITE	0x2				/* write the time of day */

/*
 * time of day status.
 */
#define TOD_CMD_OK		0L			/* read/write tod succeeded */
#define TOD_CMD_FAIL	1L			/* some hardware problem */
#define TOD_CMD_NOBATT	1L			/* battery backup failed */

/* __EOF__ */  
