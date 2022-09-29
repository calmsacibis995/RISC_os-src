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
/* $Header: recover.h,v 1.6.4.2 90/05/10 06:34:20 wje Exp $ */

#ifndef	_SYS_RECOVER_
#define	_SYS_RECOVER_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

extern struct rd_user * cr_rduser();
extern void del_rduser();
extern void clean_GEN_rd();
extern void cleanup();


/* opcodes */
#define REC_DISCONN	1	/* circuit gone */
#define REC_KILL	2	/* exit */
#define REC_FUMOUNT	3	/* forced unmount */
#define REC_MSG		4 

/* rfdaemon msgflag flags */
#define  MORE_SERVE	0x001
#define  DISCONN	0x002
#define  RFSKILL	0x004
#define  FUMOUNT	0x008

/* active general and specific RDs */
#define ACTIVE_GRD(R) 	((R->rd_stat & RDUSED) && \
	    		(R->rd_qtype & GENERAL) && (R->rd_user_list))
#define ACTIVE_SRD(R)   ((R->rd_stat & RDUSED) && \
		         (R->rd_user_list != NULL) && \
		         (R->rd_qtype & SPECIFIC))

/* This is the structure that gets passed to the user daemon */
#define ULINESIZ 120	/* want a 128-char buffer */
struct u_d_msg {
	int opcode;
	int count;
	char res_name[ULINESIZ];
};


#endif	_SYS_RECOVER_
