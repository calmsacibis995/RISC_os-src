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
/* $Header: if_laiocb.h,v 1.1.2.2 90/05/10 06:22:39 wje Exp $ */

/* iocb specific items */

#define LAIOCBMAX 16

#define LANOWAIT	1
#define LASPIN		2
#define LASLEEP		3
#define LASCAN		4

#define	DONT_INTERRUPT	0
#define	DO_INTERRUPT	1

/* 
 * command packets consist of the command and a null delimited argument list 
 *
 * status returns consist of the returned status (> 0: success, -1: failure) and
 * a null delimited status list 
 */

#define LA_ERROR	-1

#define LA_PROBE	1
/*
 * arg list: interrupt flag
 * status: LA_PROBE, 6 byte physical ethernet address in cmd block
 */

#define LA_INIT		2
/* 
 * arg list: ptr to initialization block
 * status: LA_INIT
 */
#define LA_STOP		3
/* 
 * arg list: no args
 * status: LA_STOP
 */
#define LA_STRT		4
/* 
 * arg list: no args
 * status: LA_STRT
 */
#define LA_RECV		5
/* 
 * status: LA_RECV
 */
#define LA_XMIT		6
/* 
 * status: LA_XMIT
 */
#define LA_XMIT_DONE	7
/* 
 * status: LA_XMIT_DONE
 */
#define LA_STAT		8
/* 
 * arg list: no args
 * status: LA_STAT, CSR_ERRTOT errors in cmd blk
 */
#define LA_INIT_DONE	9
/* 
 * status: LA_INIT_DONE
 */
#define LA_RESET	10
/* 
 * status: LA_RESET
 */

#define LA_DBG_ON	11
/* 
 * status: LA_DBG_ON
 */

#define LA_DBG_OFF	12
/* 
 * status: LA_DBG_OFF
 */

#define LA_MISS		13
/*
 * status: LA_MISS
 */

#define LA_LAST		LA_MISS		/* keep up to date */
#define	NUM_CMDS	LA_LAST+1

#define INT_LB		0x8	/*  Internal loopback mode           */
#define INT_CL		0x9	/*  Internal loopback collision mode */
#define INT_CE		0xA	/*  Internal loopback CRC error mode */
#define EXT_LB		0xC	/*  External loopback mode           */

	/*  UNIT  */
#define	UNIT_AF_INET		0x0	/*  UNIT for INET domain      */
#define	UNIT_AF_LANCETEST	0x1	/*  UNIT for LANCETEST demain */
