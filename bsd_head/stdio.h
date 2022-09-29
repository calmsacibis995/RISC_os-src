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
/* $Header: stdio.h,v 1.2.2.3.1.1.1.2 90/10/26 16:04:45 beacker Exp $ */
#ifndef BSD43_
#    include <bsd43/bsd43_.h>
#endif BSD43_


#define	BSD43_BUFSIZ	4096
# ifndef BSD43_FILE
extern	struct	bsd43_(_iobuf) {
	int	_cnt;
#ifdef host_mips
	unsigned char	*_ptr;
	unsigned char	*_base;
#else
	char	*_ptr;		/* should be unsigned char */
	char	*_base;		/* ditto */
#endif
	int	_bufsiz;
	short	_flag;
	char	_file;		/* should be short */
#ifdef host_mips
	char	*_name;		/* For pascal */
#endif
} bsd43_(_iob)[];

#define	BSD43__IOREAD	01
#define	BSD43__IOWRT	02
#define	BSD43__IONBF	04
#define	BSD43__IOMYBUF	010
#define	BSD43__IOEOF	020
#define	BSD43__IOERR	040
#define	BSD43__IOSTRG	0100
#define	BSD43__IOLBF	0200
#define	BSD43__IORW	0400
#ifndef BSD43_NULL
#define	BSD43_NULL	0
#endif BSD43_NULL
#define	BSD43_FILE	struct bsd43_(_iobuf)
#define	BSD43_EOF	(-1)

#define	bsd43_stdin	(&bsd43_(_iob)[0])
#define	bsd43_stdout	(&bsd43_(_iob)[1])
#define	bsd43_stderr	(&bsd43_(_iob)[2])
#ifdef host_mips
#ifndef lint
#  define bsd43_getc(p)	(--(p)->_cnt>=0? *(p)->_ptr++:bsd43_(_filbuf)(p))
#endif not lint
#else
#ifndef lint
#define	bsd43_getc(p)		(--(p)->_cnt>=0? (int)(*(unsigned char *)(p)->_ptr++):bsd43_(_filbuf)(p))
#endif not lint
#endif
#define	bsd43_getchar()	bsd43_getc(bsd43_stdin)
#ifndef lint
#define bsd43_putc(x, p)	(--(p)->_cnt >= 0 ?\
	(int)(*(unsigned char *)(p)->_ptr++ = (x)) :\
	(((p)->_flag & BSD43__IOLBF) && -(p)->_cnt < (p)->_bufsiz ?\
		((*(p)->_ptr = (x)) != '\n' ?\
			(int)(*(unsigned char *)(p)->_ptr++) :\
			bsd43_(_flsbuf)(*(unsigned char *)(p)->_ptr, p)) :\
		bsd43_(_flsbuf)((unsigned char)(x), p)))
#endif not lint
#define	bsd43_putchar(x)	bsd43_putc(x,bsd43_stdout)
#define	bsd43_feof(p)	(((p)->_flag&BSD43__IOEOF)!=0)
#define	bsd43_ferror(p)	(((p)->_flag&BSD43__IOERR)!=0)
#define	bsd43_fileno(p)	((p)->_file)
#define	bsd43_clearerr(p)	((p)->_flag &= ~(BSD43__IOERR|BSD43__IOEOF))

BSD43_FILE	*bsd43_(fopen)();
BSD43_FILE	*bsd43_(fdopen)();
BSD43_FILE	*bsd43_(freopen)();
long		bsd43_(ftell)();
char		*bsd43_(fgets)();
#ifdef vax
char		*bsd43_(sprintf)();		/* too painful to do right */
#endif
BSD43_FILE	*bsd43_(popen)();
char		*bsd43_(gets)();
# endif

/*----- NUMIPS: STRIP MACRO PREFIXES (BEGIN) ---------------------------------*/
/* 
 * Strip off "BSD43_" and "bsd43_" for use with standard BSD code.
 * (GENERATED AUTOMATICALLY -- DON'T EDIT BY HAND)
 */
#ifdef SYSTYPE_BSD43
#   define BUFSIZ BSD43_BUFSIZ
#   define EOF BSD43_EOF
#   define FILE BSD43_FILE
#ifndef NULL
#   define NULL BSD43_NULL
#endif NULL
#   define _IOEOF BSD43__IOEOF
#   define _IOERR BSD43__IOERR
#   define _IOLBF BSD43__IOLBF
#   define _IOMYBUF BSD43__IOMYBUF
#   define _IONBF BSD43__IONBF
#   define _IOREAD BSD43__IOREAD
#   define _IORW BSD43__IORW
#   define _IOSTRG BSD43__IOSTRG
#   define _IOWRT BSD43__IOWRT
#   define clearerr bsd43_clearerr
#   define feof bsd43_feof
#   define ferror bsd43_ferror
#   define fileno bsd43_fileno
#   define getc bsd43_getc
#   define getchar bsd43_getchar
#   define putc bsd43_putc
#   define putchar bsd43_putchar
#   define stderr bsd43_stderr
#   define stdin bsd43_stdin
#   define stdout bsd43_stdout
#endif SYSTYPE_BSD43
/*----- NUMIPS: STRIP MACRO PREFIXES (END) -----------------------------------*/


