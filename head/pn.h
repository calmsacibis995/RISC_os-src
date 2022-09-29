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
/* $Header: pn.h,v 1.7.3.2 90/05/10 01:02:44 wje Exp $ */

#ifndef	_PN_
#define	_PN_	1


/*	Copyright (c) 1984 AT&T	*/
/*	  All Rights Reserved  	*/

/*	THIS IS UNPUBLISHED PROPRIETARY SOURCE CODE OF AT&T	*/
/*	The copyright notice above does not evidence any   	*/
/*	actual or intended publication of such source code.	*/

/* switch table structure */

typedef struct {
	char *sw_opcode;	/* opcode */
	int sw_idx;		/* index */
} pntab_t;

#define RFS 105			/* for rfsdaemon */

#define NUMSWENT 3		/* the number of entries in sw_tab */

/* these are the indicies into sw_tab.
   note that the orders must match the opcodes */

#define RF_RF 0			/* remote file service */
#define RF_NS 1			/* name service */
#define RF_AK 2			/* acknowledgement */

#define NUMDUENT 1		/* the number of entries in du_tab */

/* these are the indicies into du_tab.
   note that the orders must match the opcodes */

#define MNT 0			/* mount case */

#define PASSWDLEN	20		/* length of password */
#define C_RETRY		2		/* command retry count */
#define DEVSTR		"/dev/%s"
#define LISTNMSG	"NLPS:000:001:%d"
#define CANONSTR	"c4ll"		/* canonical pntab */
#define CANON_CLEN	16		/* canonical length of pntab */
#define OPCODLEN	4		/* 3 chars + null */

/* negotiate data packect */

typedef struct {
	long n_hetero;		/* heterogeneity indication */
	char n_passwd[PASSWDLEN];	/* password */
	struct token n_token;	/* client's token */
	char n_netname[MAXDNAME];	/* netnodename */
} ndata_t;

typedef struct {
	char pn_op[OPCODLEN];
	long pn_lo;
	long pn_hi;
} pnhdr_t;

/* these version numbers specify compatibility between
	different versions of protocol negotiations */

#define LO_VER	1		/* lo version of this library */
#define HI_VER	1		/* hi version of this library */

/* these are the flags for the negotiate routine */

#define	SERVER	0
#define	CLIENT	1

#define TIMOD	"timod"

#endif	_PN_
