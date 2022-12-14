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
/* $Header: strlog.h,v 1.6.4.2 90/05/10 06:39:10 wje Exp $ */

#ifndef	_SYS_STRLOG_
#define	_SYS_STRLOG_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Streams Log Driver Interface Definitions
 */

/*
 * structure of control portion of log message 
 */
struct log_ctl {
	short	mid;
	short	sid;
	char 	level;		/* level of message for tracing */
	short	flags;		/* message disposition */
	long	ltime;		/* time in machine ticks since boot */
	long	ttime;		/* time in seconds since 1970 */
	long	seq_no;		/* sequence number */
};
	
/* Flags for log messages */

#define SL_FATAL	01	/* indicates fatal error */
#define SL_NOTIFY	02	/* logger must notify administrator */
#define SL_ERROR	04	/* include on the error log */
#define SL_TRACE	010	/* include on the trace log */


/*
 * Structure defining ids and levels desired by the tracer (I_TRCLOG).
 */
struct trace_ids {
	short ti_mid;
	short ti_sid;
	char  ti_level;
};


/*
 * Log Driver I_STR ioctl commands
 */

#define I_TRCLOG	1	/* process is tracer */
#define I_ERRLOG	2	/* process is error logger */

/*
 * Parameter definitions for logger messages 
 */
#define LOGMSGSZ	128
#define NLOGARGS	3


#endif	_SYS_STRLOG_
