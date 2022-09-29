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
/* $Header: fifo.h,v 1.2.1.4 90/05/10 06:16:07 wje Exp $ */

/*	@(#)fifo.h	2.3 88/05/24 4.0NFSSRC SMI;	*/
/*	@(#)fifo.h 1.7 86/08/27 SMI;	*/

#ifndef	_SYS_FS_FIFO_
#define	_SYS_FS_FIFO_	1

#ifdef KERNEL

/*
 * Regardless of the size of buffers used to buffer FIFO data, we use a
 * singly linked list of FIFO buffers.
 */
 
struct fifo_bufhdr {
	struct fifo_bufhdr *fb_next;	/* ptr to next buffer */
	char fb_data[1];
};

#define FIFO_BUFHDR_SIZE (sizeof(struct fifo_bufhdr *))
#define FIFO_BUFFER_SIZE (fifoinfo.fifobsz + FIFO_BUFHDR_SIZE)

/*
 *	Fifo information structure.
 */
struct fifoinfo {
	int	fifobuf,	/* max # bytes stored in a fifo */
		fifomax,	/* largest size of a single write to a fifo */
		fifobsz,	/* # of data bytes in each fifo data buffer */
		fifomnb;	/* max # bytes reserved for all fifos */
};

#define FIFO_BUF (fifoinfo.fifobuf)
#define FIFO_MAX (fifoinfo.fifomax)
#define FIFO_BSZ (fifoinfo.fifobsz)
#define FIFO_MNB (fifoinfo.fifomnb)

int fifo_alloc;			/* total number of bytes reserved for fifos */
extern struct fifoinfo	fifoinfo;	/* fifo parameters */

#endif KERNEL

#endif	_SYS_FS_FIFO_
