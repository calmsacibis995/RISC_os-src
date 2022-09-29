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
/* $Header: libgen.h,v 1.5.2.2 90/05/10 02:37:54 wje Exp $ */

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


/*	declarations of functions found in libgen
*/


extern char * basename();

extern unsigned char * bgets();

extern unsigned bufsplit();

extern char * copylist();

extern char * day();

extern char * dirname();

extern int eaccess();

extern int incount();

extern int julian();

extern int leap();

extern int mkdirp();

extern int num();

extern int numd();

extern p2open();

extern int p2close();

extern char * pathfind();

extern char * regerror();

extern int rmdirp();

extern int sesystem();

extern short sgets();

extern char * smemcpy();

extern void sputs();

extern char * stradd();

extern char * strccpy();

extern char * strecpy();

extern int strnlen();

extern char * sweekday();

extern long tconv();

extern long tconvs();

extern void to_date();

extern long to_day();

extern char * triml();

extern char * trimt();

extern int waitpid();

extern int weekday();
