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
#ident	"$Header: stdio_def.c,v 1.6.2.2 90/05/10 01:48:28 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*
 * Pointers to imported
 * symbols _sibuf, _sobuf, _smbuf
 * _iob and _bufendtab and _lastbuf
 */
#include <stdio.h>

 unsigned char (* _libc__sibuf)[] = 0, (* _libc__sobuf)[] = 0;

 unsigned char (* _libc__smbuf)[][_SBFSIZ] = 0;

 FILE (* _libc__iob)[] = 0;

 FILE * (* _libc__lastbuf) = 0;

 unsigned char * (* _libc__bufendtab)[] = 0; 
