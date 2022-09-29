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
/* $Header: pipe_inode.h,v 1.8.1.3 90/05/10 06:17:34 wje Exp $ */

#ifndef	_SYS_FS_PIPE_INODE_
#define	_SYS_FS_PIPE_INODE_	1


/*
 * Definitions for "pipe" filesystem.
 */
#include "sys/fs/com_inode.h"

  /*
   * Common inode information for named pipes under the efs.  The
   * implementation for pipes and fifos is shared by all the filesystems
   * that use the com filesystem.
   */
struct pipe_inode {
	struct	com_inode pi_com;
	short	pi_fflag;	/* state flag */
	short	pi_frptr;	/* read pointer */
	short	pi_frcnt;	/* number of readers count */
	short	pi_fwptr;	/* write pointer */
	short	pi_fwcnt;	/* number of writers count */
	struct	buf *pi_bp;	/* buffer holding data */
	struct  proc *pi_sel	/* process selecting on this pipe */
};

#define	pipe_fsptr(ip)	((struct pipe_inode *) (ip)->i_fsptr)

#define	PIPE_IFIR	01
#define	PIPE_IFIW	02
#define	PIPE_COLL	04		/* multiple select()s on this pipe */
#define	PIPE_SIZE	(5 * 1024)
#define	PIPE_MAGIC	0xF1F0F1F0

extern	void	pipe_openi();
extern	void	pipe_closei();
extern	void	pipe_readi();
extern	void	pipe_writei();

#endif	_SYS_FS_PIPE_INODE_
