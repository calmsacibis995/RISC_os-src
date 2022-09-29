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
#ident	"$Header: data.c,v 1.7.2.2 90/05/10 01:44:13 wje Exp $"

/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/*LINTLIBRARY*/
#include <stdio.h>

#if !u370

/* some slop is allowed at the end of the buffers in case an upset in
 * the synchronization of _cnt and _ptr (caused by an interrupt or other
 * signal) is not immediately detected.
 */
unsigned char _sibuf[BUFSIZ+8], _sobuf[BUFSIZ+8];
/*
 * Ptrs to start of preallocated buffers for stdin, stdout.
 */
#endif

unsigned char _smbuf[_NFILE+1][_SBFSIZ];

FILE _iob[_NFILE] = {
#if vax || u3b || M32 || mips
	{ 0, NULL, NULL, _IOREAD, 0},
	{ 0, NULL, NULL, _IOWRT, 1},
	{ 0, _smbuf[2], _smbuf[2], _IOWRT+_IONBF, 2},
#endif
#if pdp11
	{ NULL, 0, NULL, _IOREAD, 0},
	{ NULL, 0, NULL, _IOWRT, 1},
	{ _smbuf[2], 0, _smbuf[2], _IOWRT+_IONBF, 2},
#endif
#if u370
	{ NULL, 0, NULL, _IOREAD, 0},
	{ NULL, 0, NULL, _IOWRT, 1},
	{ NULL, 0, NULL, _IOWRT+_IONBF, 2},
#endif
};
/*
 * Ptr to end of io control blocks
 */
FILE *_lastbuf = { &_iob[_NFILE] };

/*
 * Ptrs to end of read/write buffers for each device
 * There is an extra bufend pointer which corresponds to the dummy
 * file number _NFILE, which is used by sscanf and sprintf.
 */
unsigned char *_bufendtab[_NFILE+1] = { NULL, NULL, _smbuf[2]+_SBFSIZ, };
