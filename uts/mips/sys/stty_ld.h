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
 * |         950 DeGuigne Drive                                |
 * |         Sunnyvale, CA 94086                               |
 * |-----------------------------------------------------------|
 */
/* $Header: stty_ld.h,v 1.12.3.2.1.1.1.2 90/11/12 18:31:32 beacker Exp $ */

#ifndef	_SYS_STTY_LD_
#define	_SYS_STTY_LD_	1


/* streams tty interface
 *	definitions for the streams 'line discipline'
 */


#ifdef mips
#define u_char unchar
#endif

/* view an IOCTL message as a termio structure */
#define STERMIO(bp) ((struct termio*)(bp)->b_cont->b_rptr)


/* one of these is created for each stream
 */
struct stty_ld {
	struct termio st_termio;	/* modes and such		*/
#define st_iflag st_termio.c_iflag	/* input modes			*/
#define st_oflag st_termio.c_oflag	/* output modes			*/
#define st_cflag st_termio.c_cflag	/* 'control' modes		*/
#define st_lflag st_termio.c_lflag	/* line discipline modes	*/
#define	st_line  st_termio.c_line	/* line discipline		*/
#define	st_cc	 st_termio.c_cc		/* control chars		*/
#define st_werase st_cc[V_WERAS]
#define st_retype st_cc[V_RPRNT]
#define st_flushc st_cc[V_FLUSH]
#define st_lit	st_cc[V_LNEXT]

	u_char	st_rrunning;		/* input busy			*/

	ushort	st_state;		/* current state		*/
	int	st_tid;			/* input timer ID		*/
	short	st_ocol;		/* current output 'column'	*/
	short	st_xcol;		/* current 'interfering' column	*/
	short	st_bcol;		/* 'base' of input line		*/

	ushort st_llen;			/* bytes of typed-ahead text	*/

	queue_t *st_rq;			/* our read queue		*/
	queue_t *st_wq;			/* our write queue		*/
	mblk_t	*st_imsg, *st_ibp;	/* input line			*/
	mblk_t	*st_lmsg, *st_lbp;	/* typed-ahead canonical lines	*/
	mblk_t	*st_emsg, *st_ebp;	/* current echos		*/
};

/* states */
#define ST_LIT		0x0001		/* have seen literal character	*/
#define ST_ESC		0x0002		/* have seen backslash		*/
#define ST_TIMING	0x0004		/* timer is running		*/
#define ST_TIMED	0x0008		/* timer has already run	*/
#define ST_TABWAIT	0x0010		/* waiting to echo HT delete	*/
#define ST_BCOL_BAD	0x0020		/* base column # is wrong	*/
#define ST_ECHOING	0x0040		/* current output is just echos	*/
#define ST_ISTTY	0x0080		/* told stream head about tty	*/
#define ST_INRAW	0x0100		/* input is very raw		*/
#define ST_INPASS	0x0200		/* pass input upstream fast	*/
#define ST_SIGIO	0x0400		/* send SIGIO when I/O is possible */

#define ST_MAX_LINE	256		/* maximum cooked/cannonical line */

#endif	_SYS_STTY_LD_
