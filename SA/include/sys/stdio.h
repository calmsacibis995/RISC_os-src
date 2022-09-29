/* $Header: stdio.h,v 1.2 90/01/23 13:47:16 huang Exp $ */
/* $Copyright$ */

#ifndef	_STDIO_
#define	_STDIO_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

#ifndef _NFILE
#define _NFILE	20

#if u370
#define BUFSIZ	4096
#endif
#if vax || u3b || M32 || u3b15 || u3b5 || u3b2 || mips
#define BUFSIZ	1024
#endif
#if pdp11
#define BUFSIZ	512
#endif

/* buffer size for multi-character output to unbuffered files */
#define _SBFSIZ 8

typedef struct {
#if vax || u3b || M32 || u3b15 || u3b5 || u3b2 || mips
	int	_cnt;
	unsigned char	*_ptr;
#else
	unsigned char	*_ptr;
	int	_cnt;
#endif
	unsigned char	*_base;
	char	_flag;
	char	_file;
} FILE;

/*
 * _IOLBF means that a file's output will be buffered line by line
 * In addition to being flags, _IONBF, _IOLBF and _IOFBF are possible
 * values for "type" in setvbuf.
 */
#define _IOFBF		0000
#define _IOREAD		0001
#define _IOWRT		0002
#define _IONBF		0004
#define _IOMYBUF	0010
#define _IOEOF		0020
#define _IOERR		0040
#define _IOLBF		0100
#define _IORW		0200

#ifndef NULL
#define NULL		0
#endif
#ifndef EOF
#define EOF		(-1)
#endif

#define clearerr(p)	((void) ((p)->_flag &= ~(_IOERR | _IOEOF)))
#define feof(p)		((p)->_flag & _IOEOF)
#define ferror(p)	((p)->_flag & _IOERR)
#define fileno(p)	(p)->_file

#define stdio_putc(x, p)	((int) (*(p)->_ptr++ = (unsigned char) (x)))

#endif _NFILE
#endif	_STDIO_
