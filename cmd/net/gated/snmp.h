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
/* $Header: snmp.h,v 1.1.1.2 90/05/09 17:03:38 wje Exp $ */

#define ServerPipeName	"/dev/gated.server"
#define ClientPipeName	"/dev/gated.client"

#define SNMP_IPROUTEMETRIC1	1
#define SNMP_IPROUTEMETRIC2	2
#define SNMP_IPROUTEMETRIC3	3
#define SNMP_IPROUTEMETRIC4	4
#define SNMP_IPROUTENEXTHOP	5
#define SNMP_IPROUTETYPE	6
#define SNMP_IPROUTEPROTO	7
#define SNMP_IPROUTEAGE		8
#define SNMP_EGPINMSGS		9
#define SNMP_EGPINERRORS	10
#define SNMP_EGPOUTMSGS		11
#define SNMP_EGPOUTERRORS	12
#define SNMP_EGPNEIGHSTATE	13
#define SNMP_EGPNEIGHADDR	14
#define SNMP_ERROR		15

struct snmpmsg {
	int type;
	int index;
	int next;
	int value;
	};
