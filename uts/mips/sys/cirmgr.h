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
/* $Header: cirmgr.h,v 1.7.4.2 90/05/10 06:07:27 wje Exp $ */

#ifndef	_SYS_CIRMGR_
#define	_SYS_CIRMGR_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/


#define MAXTOKLEN sizeof(struct token)		/* maximum token length in bytes */

struct gdpmisc {
	int hetero;
	int version;
};

struct token {
	int	t_id;	 /* token id for differentiating multiple ckts	*/
	char	t_uname[MAXDNAME]; /* full domain name of machine		*/
};


struct gdp {
	struct queue *queue;	/* pointer to associated stream head	*/
	struct file *file;	/* file pointer to stream head we stole */
	short mntcnt;		/* number of mounts on this stream	*/
	short sysid;
	short flag;		/* connection info */		
	int hetero;		/* need to canonicalize messages	*/
	int version;		/* DU version at the other end of circuit */
	long time;		/* time delta */
	struct token token;	/* circuit identification		*/
	char	*idmap[2];	/* 0=uid=UID_DEV, 1=gid=GID_DEV		*/
};

extern int maxgdp;
extern struct gdp gdp[];
#define get_sysid(x)       ((struct gdp *)(x)->sd_queue->q_ptr)->sysid
#define	GDP(x)		((struct gdp *)(x)->q_ptr)


/* GDP circuit state flags */
#define GDPRECOVER	0x004
#define GDPDISCONN	0x002
#define GDPCONNECT	0x001
#define GDPFREE		0x000

#endif	_SYS_CIRMGR_
